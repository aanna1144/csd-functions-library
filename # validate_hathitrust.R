# validate_hathitrust.R
#
# Validates enrich_hathitrust() against a GreenGlass ground-truth sample
# (testing_ht_api_file.xlsx: 1,000 rows with known-correct HathiTrust values
#
# For each row, uses whichever identifier is available (OCLC No. preferred,
# then ISSN, then ISBN), runs it through enrich_hathitrust(), and compares
# the result against the known GreenGlass columns already in the file.
#
# Outputs:
#   full_validation_results.csv - every tested row with match/mismatch flags
#   mismatches_for_review.csv   - just the mismatches, for manual review and reporting back to Akshay
 
library(readxl)
library(dplyr)
 
source("csd-functions-library/csd_function_library.R")  # adjust path if enrich_hathitrust() lives elsewhere
 
gg <- read_excel("testing_ht_api_file.xlsx")
print(colnames(gg))
 
# Sanity check, done once at the top rather than separately in each part:
# makes sure the CURRENT enrich_hathitrust() is loaded, not an older cached version.

# We check this by looking at the function's PARAMETERS (new_col_status),
# not its output column names. Part 2 below calls  enrich_hathitrust() with custom 
# column names (status_oclc, status_issn, etc.), so checking output columns there 
# would give a false outdated error even when the correct version is loaded. 
# Checking the parameter list instead works correctly no matter how the output columns get renamed.

# If this fails, it means an old copy of csd_function_library.R is still
# sourced - the fix is to re-source the current file, not to change anything in this script.
if (!"new_col_status" %in% names(formals(enrich_hathitrust))) {
  stop(
    "enrich_hathitrust() does not have a 'new_col_status' parameter.\n",
    "This means an OUTDATED version of csd_function_library.R is currently sourced\n",
    "(the old version used new_col_pd/new_col_ic instead of the current 4-column output).\n",
    "Parameters actually present: ", paste(names(formals(enrich_hathitrust)), collapse = ", "), "\n",
    "Fix: re-source the latest csd_function_library.R, then re-run this script."
  )
}
 
# ── Shared setup ─────────────────────────────────────────────────────────────
 
# ISSNs are 8 digits and can have leading zeros (e.g. "01477196"). If the sheet
# or readxl treated the column as numeric anywhere along the way, that leading
# zero would be silently dropped.
gg$ISSN <- ifelse(is.na(gg$ISSN), NA_character_,
                   formatC(as.character(gg$ISSN), width = 8, flag = "0"))
 
# OCLC No. and ISBN may come in as numeric OR character depending on how
# Excel stored the cells. Handle both: convert to character, then strip a
# trailing ".0" left over if the value was numeric.
as_id_string <- function(x) {
  chr <- trimws(as.character(x))
  chr <- sub("\\.0$", "", chr)
  ifelse(is.na(x) | chr == "" | chr == "NA", NA_character_, chr)
}
 
strip_scheme <- function(u) sub("^https?://", "", u)
 
# A title with multiple scanned items can return a semicolon-joined,
# multi-value status/code (e.g. "Full view; Limited (search-only)"). The
# order those get listed in isn't guaranteed stable between separate API
# calls, so comparing raw strings directly can flag two identical answers
# as a "disagreement" just because they're listed in a different order.
# enrich_hathitrust() itself sorts these before joining now, but this stays
# here too for older cached results.
normalize_multi <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    paste(sort(trimws(strsplit(v, ";")[[1]])), collapse = "; ")
  }, character(1), USE.NAMES = FALSE)
}
 
 
# ═══════════════════════════════════════════════════════════════════════════
# PART 1: Validate against GreenGlass's known-correct values
# ═══════════════════════════════════════════════════════════════════════════
 
gg_part1 <- gg |>
  rename(
    gg_public_domain = `HathiTrust Public Domain`,
    gg_in_copyright   = `HathiTrust In Copyright`,
    gg_url            = `HathiTrust URL`
  ) |>
  mutate(
    id_type_used = case_when(
      !is.na(`OCLC No.`) ~ "oclc",
      !is.na(ISSN)       ~ "issn",
      !is.na(ISBN)       ~ "isbn",
      TRUE               ~ NA_character_
    ),
    identifier_used = case_when(
      id_type_used == "oclc" ~ as_id_string(`OCLC No.`),
      id_type_used == "issn" ~ ISSN,
      id_type_used == "isbn" ~ as_id_string(ISBN),
      TRUE                   ~ NA_character_
    )
  )
 
n_skipped <- sum(is.na(gg_part1$id_type_used))
message(n_skipped, " of ", nrow(gg_part1), " rows have no OCLC/ISSN/ISBN to test -- skipped.")
 
# Run enrich_hathitrust() once per identifier type present, since a single
# call only handles one id_type at a time, then recombines.
results_list <- lapply(c("oclc", "issn", "isbn"), function(t) {
  subset_df <- gg_part1 |> filter(id_type_used == t)
  if (nrow(subset_df) == 0) return(NULL)
  message("Testing ", nrow(subset_df), " rows via ", toupper(t), "...")
  enrich_hathitrust(subset_df, id_col = "identifier_used", id_type = t)
})
 
tested <- bind_rows(results_list)
 
# enrich_hathitrust() returns the full rights picture (HathiTrust Rights
# Status / Code / Description), not a collapsed Yes/No, but for THIS comparison
# specifically we still need a binary Yes/No because that's what GreenGlass itself uses.
derive_pd <- function(status) {
  ifelse(is.na(status) | status %in% c("Not Found", "Error"), "No",
         ifelse(grepl("Full view", status, fixed = TRUE), "Yes", "No"))
}
derive_ic <- function(status) {
  ifelse(is.na(status) | status %in% c("Not Found", "Error"), "No",
         ifelse(grepl("Limited (search-only)", status, fixed = TRUE), "Yes", "No"))
}
 
tested <- tested |>
  mutate(
    pd_match = derive_pd(`HathiTrust Rights Status`) == gg_public_domain,
    ic_match = derive_ic(`HathiTrust Rights Status`)  == gg_in_copyright,
    url_match = (is.na(gg_url) & is.na(`HathiTrust URL`)) |
                (!is.na(gg_url) & !is.na(`HathiTrust URL`) &
                   strip_scheme(gg_url) == strip_scheme(`HathiTrust URL`))
  )
 
message("\n=== PART 1: VALIDATION SUMMARY ===")
message("Rows tested:  ", nrow(tested), " (", n_skipped, " skipped -- no identifier available)")
message("Public Domain match rate: ", round(mean(tested$pd_match, na.rm = TRUE) * 100, 1), "%")
message("In-Copyright match rate:  ", round(mean(tested$ic_match, na.rm = TRUE) * 100, 1), "%")
message("URL match rate:           ", round(mean(tested$url_match, na.rm = TRUE) * 100, 1), "%")
 
message("\n--- BY IDENTIFIER TYPE (row-level) ---")
by_type <- tested |>
  group_by(id_type_used) |>
  summarise(
    n = n(),
    pd_rate = round(mean(pd_match, na.rm = TRUE) * 100, 1),
    ic_rate = round(mean(ic_match, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )
print(by_type)
 
# Row-level rates can be badly skewed if the same identifier appears many
# times (a single wrong/right answer gets counted once per duplicate row).
# Unique-identifier-level rates gives the per-title accuracy.
message("\n--- BY IDENTIFIER TYPE (unique-identifier level, corrects duplication skew) ---")
by_type_unique <- tested |>
  distinct(identifier_used, id_type_used, .keep_all = TRUE) |>
  group_by(id_type_used) |>
  summarise(
    n_unique = n(),
    pd_rate = round(mean(pd_match, na.rm = TRUE) * 100, 1),
    ic_rate = round(mean(ic_match, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )
print(by_type_unique)
 
mismatches <- tested |> filter(!pd_match | !ic_match | !url_match)
message(nrow(mismatches), " row(s) had at least one mismatch -- see mismatches_for_review.csv")
 
write.csv(mismatches, "mismatches_for_review.csv", row.names = FALSE)
write.csv(tested, "full_validation_results.csv", row.names = FALSE)
 
message("Wrote full_validation_results.csv and mismatches_for_review.csv")
 
 
# ═══════════════════════════════════════════════════════════════════════════
# PART 2: API self-consistency test (raw output, OCLC vs. ISSN/ISBN)
# ═══════════════════════════════════════════════════════════════════════════
 
gg_part2 <- gg |>
  mutate(
    oclc_id = as_id_string(`OCLC No.`),
    isbn_id = as_id_string(ISBN)
  )
 
# Runs enrich_hathitrust() with a given id_col/id_type, tagging output
# columns with `label` so multiple runs on the same rows don't collide.
run_lookup <- function(df, id_col, id_type, label) {
  enrich_hathitrust(
    df, id_col = id_col, id_type = id_type,
    new_col_status = paste0("status_", label),
    new_col_code   = paste0("code_", label),
    new_col_desc   = paste0("desc_", label),
    new_col_url    = paste0("url_", label)
  )
}
 
# Compares HathiTrust's raw output when queried via OCLC vs. via an
# alternate identifier (ISSN or ISBN), on the exact same rows. Pulled into
# one function and called twice below (once per identifier type) rather
# than kept as two separate, nearly-identical blocks - so if the
# comparison logic ever needs a fix (like the multi-value ordering fix
# from earlier), it only has to be changed in one place.
compare_via_identifier <- function(alt_id_col, alt_id_type, out_csv) {
  subset_df <- gg_part2 |> filter(!is.na(oclc_id) & !is.na(.data[[alt_id_col]]))
  message("\nRows with BOTH an OCLC number and an ", toupper(alt_id_type), ": ", nrow(subset_df))
 
  res_via_oclc <- run_lookup(subset_df, "oclc_id", "oclc", "oclc")
  res_via_alt  <- run_lookup(subset_df, alt_id_col, alt_id_type, "alt")
 
  comparison <- subset_df |>
    mutate(
      status_via_oclc = res_via_oclc$status_oclc,
      code_via_oclc    = res_via_oclc$code_oclc,
      desc_via_oclc    = res_via_oclc$desc_oclc,
      url_via_oclc     = res_via_oclc$url_oclc,
      status_via_alt   = res_via_alt$status_alt,
      code_via_alt     = res_via_alt$code_alt,
      desc_via_alt     = res_via_alt$desc_alt,
      url_via_alt      = res_via_alt$url_alt,
      status_agree     = normalize_multi(status_via_oclc) == normalize_multi(status_via_alt),
      code_agree       = normalize_multi(code_via_oclc) == normalize_multi(code_via_alt),
      url_agree        = url_via_oclc == url_via_alt
    )
 
  message("Rights Status agreement: ", round(mean(comparison$status_agree, na.rm = TRUE) * 100, 1), "%")
  message("Rights Code agreement:   ", round(mean(comparison$code_agree, na.rm = TRUE) * 100, 1), "%")
  message("URL agreement:           ", round(mean(comparison$url_agree, na.rm = TRUE) * 100, 1), "%")
 
  write.csv(comparison, out_csv, row.names = FALSE)
  comparison
}
 
message("\n=== PART 2: API self-consistency test (OCLC vs. ISSN/ISBN, raw output) ===")
issn_comparison <- compare_via_identifier("ISSN",    "issn", "issn_vs_oclc_comparison.csv")
isbn_comparison <- compare_via_identifier("isbn_id", "isbn", "isbn_vs_oclc_comparison.csv")
 
message("\nWrote issn_vs_oclc_comparison.csv and isbn_vs_oclc_comparison.csv")
message("Each file has *_agree columns -- TRUE means OCLC and the other identifier",
        " returned the exact same raw HathiTrust output for that title.")
 
message("\n=== ALL DONE ===")