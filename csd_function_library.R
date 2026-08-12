# ╔════════════════════════════════════════════════════════════════════════════╗
# ║  UCSB Library Collection Strategies — R FUNCTION LIBRARY                   ║
# ║                                                                            ║
# ║  Source this file to get reusable functions for data work.                 ║
# ║                                                                            ║
# ║  Environment variables required (see Renviron.example):                    ║
# ║    OCLC_CLIENT_ID_WCMetaAPI                                                ║
# ║    OCLC_CLIENT_SECRET_WCMetaAPI                                            ║
# ║    OCLC_CLIENT_ID_WCSearchAPI                                              ║
# ║    OCLC_CLIENT_SECRET_WCSearchAPI                                          ║
# ║    GOOGLE_AUTH_EMAIL                                                       ║
# ║    SELECTOR_LOOKUP_SHEET_ID                                                ║
# ║                                                                            ║
# ║  Department: Collection Strategies, UC Santa Barbara Library               ║
# ║  Maintainers: Akshay Agrawal                                               ║
# ╚════════════════════════════════════════════════════════════════════════════╝

# ── Dependencies ─────────────────────────────────────────────────────────────

library(httr2)
library(dplyr)
library(purrr)
library(stringr)          #only needed if using enrich_vernacular_title()
library(googlesheets4)    #only needed if using enrich_selectors()


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║  FUNCTION 1: OCLC API — TOKEN MANAGEMENT                                   ║
# ║                                                                            ║
# ║  OCLC tokens expire after ~20 minutes. Each API (Metadata vs Search)       ║
# ║  uses a separate WSKey with its own client_id/secret, so tokens are        ║
# ║  stored independently.                                                     ║
# ║                                                                            ║
# ║  get_oclc_token() fetches a new token for a given API.                     ║
# ║  ensure_valid_token() is called internally by every API function           ║
# ║  and refreshes automatically before expiry.                                ║
# ║                                                                            ║
# ║  api = "metadata"  → scope WorldCatMetadataAPI                             ║
# ║                       uses OCLC_CLIENT_ID_WCMetaAPI /                      ║
# ║                            OCLC_CLIENT_SECRET_WCMetaAPI                    ║
# ║  api = "search"    → scope wcapi                                           ║
# ║                       uses OCLC_CLIENT_ID_WCSearchAPI /                    ║
# ║                            OCLC_CLIENT_SECRET_WCSearchAPI                  ║
# ╚════════════════════════════════════════════════════════════════════════════╝

oclc_token_env <- new.env(parent = emptyenv())

# Fetches a new OCLC access token for the specified API and stores it.
# api must be "metadata" or "search".
# client_id and client_secret are read from .Renviron if not provided.
get_oclc_token <- function(api = "metadata",
                           client_id = NULL,
                           client_secret = NULL) {

  if (api == "metadata") {
    client_id     <- client_id     %||% Sys.getenv("OCLC_CLIENT_ID_WCMetaAPI")
    client_secret <- client_secret %||% Sys.getenv("OCLC_CLIENT_SECRET_WCMetaAPI")
    scope         <- "WorldCatMetadataAPI"
    token_key     <- "metadata"
    expires_key   <- "metadata_expires_at"
  } else if (api == "search") {
    client_id     <- client_id     %||% Sys.getenv("OCLC_CLIENT_ID_WCSearchAPI")
    client_secret <- client_secret %||% Sys.getenv("OCLC_CLIENT_SECRET_WCSearchAPI")
    scope         <- "wcapi"
    token_key     <- "search"
    expires_key   <- "search_expires_at"
  } else {
    stop("api must be 'metadata' or 'search'.", call. = FALSE)
  }

  if (client_id == "" || client_secret == "") {
    stop("OCLC credentials not found for ", api, " API. Check your .Renviron.",
         call. = FALSE)
  }

  resp <- request("https://oauth.oclc.org/token") |>
    req_auth_basic(client_id, client_secret) |>
    req_body_form(
      grant_type = "client_credentials",
      scope      = scope
    ) |>
    req_perform()

  body <- resp_body_json(resp)

  oclc_token_env[[token_key]]   <- body$access_token
  oclc_token_env[[expires_key]] <- Sys.time() + body$expires_in - 60

  invisible(body$access_token)
}

# Checks if the current token for the specified API is still valid; refreshes if not.
# Called internally before every API request.
# api must be "metadata" or "search".
# client_id and client_secret are passed through to get_oclc_token if a refresh is needed.
ensure_valid_token <- function(api = "metadata",
                               client_id = NULL,
                               client_secret = NULL) {

  if (api == "metadata") {
    token_key   <- "metadata"
    expires_key <- "metadata_expires_at"
  } else {
    token_key   <- "search"
    expires_key <- "search_expires_at"
  }

  if (is.null(oclc_token_env[[token_key]]) || Sys.time() >= oclc_token_env[[expires_key]]) {
    message("\nRefreshing OCLC ", api, " API token...")
    get_oclc_token(api = api, client_id = client_id, client_secret = client_secret)
  }

  oclc_token_env[[token_key]]
}


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║  FUNCTION 2: OCLC API — LC CLASSIFICATION ENRICHMENT                       ║
# ║                                                                            ║
# ║  Uses the WorldCat Metadata API classification-bibs endpoint to get        ║
# ║  the most popular LC call number for each OCLC number in a data frame.     ║
# ║                                                                            ║
# ║  Takes a data frame and the name of the column containing OCLC numbers.    ║
# ║  Appends a new column (default: "LC_Recommendation") with the result.      ║
# ║  Deduplicates OCLC numbers for performance, shows a progress bar, and      ║
# ║  preserves the original row count exactly.                                 ║
# ║                                                                            ║
# ║  Credentials are read from .Renviron (OCLC_CLIENT_ID_WCMetaAPI,            ║
# ║  OCLC_CLIENT_SECRET_WCMetaAPI) unless passed explicitly.                   ║
# ║                                                                            ║
# ║  Usage:                                                                    ║
# ║    df <- df |> enrich_lc_classification(oclc_col = "OCLC Number")          ║
# ╚════════════════════════════════════════════════════════════════════════════╝

enrich_lc_classification <- function(data,
                                     oclc_col,
                                     new_col       = "LC_Recommendation",
                                     client_id     = NULL,
                                     client_secret = NULL) {

  client_id     <- client_id     %||% Sys.getenv("OCLC_CLIENT_ID_WCMetaAPI")
  client_secret <- client_secret %||% Sys.getenv("OCLC_CLIENT_SECRET_WCMetaAPI")

  if (client_id == "" || client_secret == "") {
    stop("OCLC Metadata API credentials not found. Set OCLC_CLIENT_ID_WCMetaAPI and OCLC_CLIENT_SECRET_WCMetaAPI in .Renviron.",
         call. = FALSE)
  }

  original_n <- nrow(data)

  # Work with unique OCLC numbers only
  oclc_values <- data[[oclc_col]]
  unique_oclc <- unique(na.omit(as.character(oclc_values)))
  n_unique    <- length(unique_oclc)

  message("Looking up LC classifications for ", n_unique, " unique OCLC numbers...")

  results <- character(n_unique)
  pb      <- txtProgressBar(min = 0, max = n_unique, style = 3)

  for (i in seq_along(unique_oclc)) {

    token <- ensure_valid_token(api = "metadata",
                                client_id = client_id,
                                client_secret = client_secret)

    results[i] <- tryCatch({
      resp <- request(paste0(
        "https://metadata.api.oclc.org/worldcat/search/classification-bibs/",
        unique_oclc[i]
      )) |>
        req_headers(
          Authorization = paste("Bearer", token),
          Accept        = "application/json"
        ) |>
        req_perform()

      data_resp <- resp_body_json(resp)
      lc        <- data_resp$lc$mostPopular[[1]]

      if (is.null(lc)) NA_character_ else lc

    }, error = function(e) {
      NA_character_
    })

    setTxtProgressBar(pb, i)
  }

  close(pb)

  # Build lookup and join back
  lookup <- tibble(
    .oclc_key   = unique_oclc,
    !!new_col := results
  )

  data$.oclc_key <- as.character(oclc_values)
  data <- left_join(data, lookup, by = ".oclc_key")
  data$.oclc_key <- NULL

  stopifnot(
    "Row count changed after LC enrichment — this should never happen." =
      nrow(data) == original_n
  )

  matched <- sum(!is.na(data[[new_col]]))
  message("Done. ", matched, " of ", original_n, " rows matched.")

  data
}

# ╔════════════════════════════════════════════════════════════════════════════╗
# ║  FUNCTION 3: OCLC API — UC HOLDINGS OVERLAP                                ║
# ║                                                                            ║
# ║  Uses the WorldCat Search API bibs-holdings endpoint filtered by UC        ║
# ║  institution symbols. For each OCLC number, returns a pipe-separated       ║
# ║  list of UC institution names that hold the item.                          ║
# ║                                                                            ║
# ║  Takes a data frame and the name of the column containing OCLC numbers.    ║
# ║  Appends a new column (default: "UC_Libraries") with the result.           ║
# ║                                                                            ║
# ║  Set rlf_only = TRUE to restrict the check to RLF symbols only:            ║
# ║    ZAS, ZAP, ZAPSP, HH0, ZASSP                                             ║
# ║  Set rlf_only = FALSE (default) to check all UC symbols:                   ║
# ║    ZAS, UCMER, BOL, UCILW, CUY, CUV, CUI, CLU, MERUC, ZAP, CRU,            ║
# ║    CUS, CUN, CUZ, UCDLL, ZAPSP, HH0, ZASSP                                 ║
# ║                                                                            ║
# ║  Deduplicates OCLC numbers for performance, shows a progress bar, and      ║
# ║  preserves the original row count exactly.                                 ║
# ║                                                                            ║
# ║  Usage:                                                                    ║
# ║    df <- df |> enrich_uc_overlap(oclc_col = "OCLC Number")                 ║
# ║    df <- df |> enrich_uc_overlap(oclc_col = "OCLC Number",                 ║
# ║                                  rlf_only = TRUE)                          ║
# ╚════════════════════════════════════════════════════════════════════════════╝

enrich_uc_overlap_parallel <- function(data,
                              oclc_col,
                              rlf_only      = FALSE,
                              new_col       = "UC_Libraries",
                              max_active    = 5,
                              client_id     = NULL,
                              client_secret = NULL) {

  client_id     <- client_id     %||% Sys.getenv("OCLC_CLIENT_ID_WCSearchAPI")
  client_secret <- client_secret %||% Sys.getenv("OCLC_CLIENT_SECRET_WCSearchAPI")

  if (client_id == "" || client_secret == "") {
    stop("OCLC Search API credentials not found. Set OCLC_CLIENT_ID_WCSearchAPI and OCLC_CLIENT_SECRET_WCSearchAPI in .Renviron.",
         call. = FALSE)
  }

  rlf_symbols    <- c("ZAS", "ZAP", "ZAPSP", "HH0", "ZASSP")
  all_uc_symbols <- c(
    "ZAS", "UCMER", "BOL", "UCILW", "CUY", "CUV", "CUI", "CLU",
    "MERUC", "ZAP", "CRU", "CUS", "CUN", "CUZ", "UCDLL",
    "ZAPSP", "HH0", "ZASSP"
  )

  symbols      <- if (rlf_only) rlf_symbols else all_uc_symbols
  symbol_param <- paste(symbols, collapse = ",")

  original_n  <- nrow(data)
  oclc_values <- data[[oclc_col]]
  unique_oclc <- unique(na.omit(as.character(oclc_values)))
  n_unique    <- length(unique_oclc)

  label <- if (rlf_only) "RLF" else "UC"
  message("Checking ", label, " holdings overlap for ", n_unique,
          " unique OCLC numbers (", length(symbols), " symbols, max_active=", max_active, ")...")

  # ── Token once ───────────────────────────────────────────────────────────────
  token <- ensure_valid_token(api = "search",
                              client_id = client_id,
                              client_secret = client_secret)

  # ── One request per OCLC number ───────────────────────────────────────────────
  requests <- lapply(unique_oclc, function(oclc) {
    request(paste0(
      "https://americas.discovery.api.oclc.org/worldcat/search/v2/bibs-holdings",
      "?oclcNumber=", oclc,
      "&heldBySymbol=", symbol_param, "&heldInCountry=", "US"
    )) |>
      req_headers(Authorization = paste("Bearer", token), Accept = "application/json") |>
      req_retry(max_tries = 3, backoff = ~2^.x)
  })

  # ── Fire in parallel ──────────────────────────────────────────────────────────
  responses <- req_perform_parallel(requests, max_active = max_active, on_error = "continue")

  # ── Parse: one result per response ───────────────────────────────────────────
  result_vec <- vapply(seq_along(responses), function(i) {
    tryCatch({
      data_resp      <- resp_body_json(responses[[i]])
      brief_recs     <- data_resp$briefRecords
      if (is.null(brief_recs) || length(brief_recs) == 0) return(NA_character_)

      brief_holdings <- brief_recs[[1]]$institutionHolding$briefHoldings
      if (is.null(brief_holdings) || length(brief_holdings) == 0) return(NA_character_)

      names_vec <- vapply(
        brief_holdings,
        function(h) if (!is.null(h$institutionName)) h$institutionName else NA_character_,
        character(1)
      )
      names_vec <- names_vec[!is.na(names_vec)]
      if (length(names_vec) == 0) NA_character_ else paste(names_vec, collapse = "|")

    }, error = function(e) NA_character_)
  }, character(1))

  names(result_vec) <- unique_oclc

  # ── Build lookup and join back ────────────────────────────────────────────────
  lookup <- tibble(
    .oclc_key  = names(result_vec),
    !!new_col := unname(result_vec)
  )

  data$.oclc_key <- as.character(oclc_values)
  data <- left_join(data, lookup, by = ".oclc_key")
  data$.oclc_key <- NULL

  stopifnot(
    "Row count changed after UC overlap enrichment — this should never happen." =
      nrow(data) == original_n
  )

  matched <- sum(!is.na(data[[new_col]]))
  message("Done. ", matched, " of ", original_n, " rows had ", label, " holdings.")

  data
}



# ╔════════════════════════════════════════════════════════════════════════════╗
# ║  FUNCTION 4: OCLC API — TOTAL HOLDING COUNT                                ║
# ║                                                                            ║
# ║  Uses the WorldCat Search API bibs-holdings endpoint (no symbol filter)    ║
# ║  to get the total US holding count for each OCLC number.                   ║
# ║                                                                            ║
# ║  Takes a data frame and the name of the column containing OCLC numbers.    ║
# ║  Appends a new column (default: "Total_Holding_Count") with the result.    ║
# ║  Deduplicates OCLC numbers for performance, shows a progress bar, and      ║
# ║  preserves the original row count exactly.                                 ║
# ║                                                                            ║
# ║  Usage:                                                                    ║
# ║    df <- df |> enrich_total_holdings(oclc_col = "OCLC Number")             ║
# ╚════════════════════════════════════════════════════════════════════════════╝

enrich_total_holdings_parallel <- function(data,
                                  oclc_col,
                                  new_col       = "Total_Holding_Count",
                                  max_active    = 5,
                                  client_id     = NULL,
                                  client_secret = NULL) {

  client_id     <- client_id     %||% Sys.getenv("OCLC_CLIENT_ID_WCSearchAPI")
  client_secret <- client_secret %||% Sys.getenv("OCLC_CLIENT_SECRET_WCSearchAPI")

  if (client_id == "" || client_secret == "") {
    stop("OCLC Search API credentials not found. Set OCLC_CLIENT_ID_WCSearchAPI and OCLC_CLIENT_SECRET_WCSearchAPI in .Renviron.",
         call. = FALSE)
  }

  original_n  <- nrow(data)
  oclc_values <- data[[oclc_col]]
  unique_oclc <- unique(na.omit(as.character(oclc_values)))
  n_unique    <- length(unique_oclc)

  message("Fetching total holding counts for ", n_unique,
          " unique OCLC numbers (max_active=", max_active, ")...")

  # ── Token once ───────────────────────────────────────────────────────────────
  token <- ensure_valid_token(api = "search",
                              client_id = client_id,
                              client_secret = client_secret)

  # ── One request per OCLC number ───────────────────────────────────────────────
  requests <- lapply(unique_oclc, function(oclc) {
    request(paste0(
      "https://americas.discovery.api.oclc.org/worldcat/search/v2/bibs-holdings",
      "?oclcNumber=", oclc, "&heldInCountry=", "US"
      
    )) |>
      req_headers(Authorization = paste("Bearer", token), Accept = "application/json") |>
      req_retry(max_tries = 3, backoff = ~2^.x)
  })

  # ── Fire in parallel ──────────────────────────────────────────────────────────
  responses <- req_perform_parallel(requests, max_active = max_active, on_error = "continue")

  # ── Parse: one result per response ───────────────────────────────────────────
  result_vec <- vapply(seq_along(responses), function(i) {
    tryCatch({
      data_resp  <- resp_body_json(responses[[i]])
      brief_recs <- data_resp$briefRecords
      if (is.null(brief_recs) || length(brief_recs) == 0) return(NA_integer_)

      count <- brief_recs[[1]]$institutionHolding$totalHoldingCount
      if (is.null(count)) NA_integer_ else as.integer(count)

    }, error = function(e) NA_integer_)
  }, integer(1))

  names(result_vec) <- unique_oclc

  # ── Build lookup and join back ────────────────────────────────────────────────
  lookup <- tibble(
    .oclc_key  = names(result_vec),
    !!new_col := unname(result_vec)
  )

  data$.oclc_key <- as.character(oclc_values)
  data <- left_join(data, lookup, by = ".oclc_key")
  data$.oclc_key <- NULL

  stopifnot(
    "Row count changed after holding count enrichment — this should never happen." =
      nrow(data) == original_n
  )

  matched <- sum(!is.na(data[[new_col]]))
  message("Done. ", matched, " of ", original_n, " rows got a holding count.")

  data
}

# ╔════════════════════════════════════════════════════════════════════════════╗
# ║  FUNCTION 5: SELECTOR ASSIGNMENT                                           ║
# ║                                                                            ║
# ║  Assigns a Selector and Role to each row based on its LC call number,      ║
# ║  using a lookup table stored in a Google Sheet.                            ║
# ║                                                                            ║
# ║  The lookup is fetched from Google Sheets using GOOGLE_AUTH_EMAIL and      ║
# ║  SELECTOR_LOOKUP_SHEET_ID from .Renviron OR using a service account in     ║
# ║  which case it is directly passed as lookup_df                             ║
# ║  If the lookup table is already loaded (e.g. in a Shiny app), pass it      ║
# ║  directly via lookup_df to skip the Google Sheets fetch.                   ║
# ║                                                                            ║
# ║  Takes a data frame and the name of the column containing call numbers.    ║
# ║  Appends two new columns: "Selector" and "Role".                           ║
# ║  Deduplicates call numbers for performance and preserves the original      ║
# ║  row count exactly. Rows that cannot be matched receive NA.                ║
# ║                                                                            ║
# ║  Usage:                                                                    ║
# ║    df <- df |> enrich_selectors(call_number_col = "Call Number")           ║
# ║    df <- df |> enrich_selectors(call_number_col = "Call Number",           ║
# ║                                 lookup_df = my_lookup)                     ║
# ╚════════════════════════════════════════════════════════════════════════════╝

enrich_selectors <- function(data,
                             call_number_col,
                             lookup_df  = NULL,
                             auth_email = NULL,
                             sheet_id   = NULL) {

  # If no lookup provided, fetch from Google Sheets
  if (is.null(lookup_df)) {
    auth_email <- auth_email %||% Sys.getenv("GOOGLE_AUTH_EMAIL")
    sheet_id   <- sheet_id   %||% Sys.getenv("SELECTOR_LOOKUP_SHEET_ID")

    if (auth_email == "" || sheet_id == "") {
      stop("Google credentials not found. Set GOOGLE_AUTH_EMAIL and SELECTOR_LOOKUP_SHEET_ID in .Renviron, ",
           "or pass lookup_df directly.",
           call. = FALSE)
    }

    message("Fetching selector lookup from Google Sheets...")
    gs4_auth(email = auth_email)
    lookup_df <- read_sheet(sheet_id)
  }

  original_n   <- nrow(data)
  call_numbers <- data[[call_number_col]]

  # Deduplicate for performance — match unique values, then join back
  unique_cn <- unique(na.omit(call_numbers))
  n_unique  <- length(unique_cn)

  message("Assigning selectors for ", n_unique, " unique call numbers...")

  results <- map_dfr(unique_cn, function(cn) {
    res <- match_call_number(cn, lookup_df)
    tibble(
      .cn_key  = res$CallNumber,
      Selector = res$Selector,
      Role     = res$Role
    )
  })

  # Join back to original data
  data$.cn_key <- call_numbers
  data <- left_join(data, results, by = ".cn_key")
  data$.cn_key <- NULL

  stopifnot(
    "Row count changed after selector assignment — this should never happen." =
      nrow(data) == original_n
  )

  matched <- sum(!is.na(data$Selector) & data$Selector != "ERROR")
  message("Done. ", matched, " of ", original_n, " rows matched.")

  data
}

# Internal helper: matches a single call number against the lookup table.
# Returns a list with CallNumber, Selector, and Role.
match_call_number <- function(call_number, df) {
  tryCatch({
    subclass <- gsub("^(\\p{L}+).*", "\\1", call_number, perl = TRUE)
    number   <- as.numeric(gsub("[^0-9.]", "", regmatches(
      call_number, regexpr("\\d+(\\.\\d+)?", call_number)
    )))

    matching_row <- df[df$Subclass == subclass, ]

    if (nrow(matching_row) == 0) {
      return(list(CallNumber = call_number, Selector = NA, Role = NA))
    }

    if (any(!is.na(matching_row$`Number range`))) {
      for (i in 1:nrow(matching_row)) {
        range <- as.numeric(unlist(strsplit(matching_row$`Number range`[i], "-")))
        if (length(range) == 1) {
          if (number == range) {
            return(list(CallNumber = call_number,
                        Selector   = matching_row$Selector[i],
                        Role       = matching_row$Role[i]))
          }
        } else if (length(range) == 2) {
          if (number >= range[1] && number <= range[2]) {
            return(list(CallNumber = call_number,
                        Selector   = matching_row$Selector[i],
                        Role       = matching_row$Role[i]))
          }
        }
      }
    } else {
      return(list(CallNumber = call_number,
                  Selector   = matching_row$Selector[1],
                  Role       = matching_row$Role[1]))
    }

    return(list(CallNumber = call_number, Selector = NA, Role = NA))
  }, error = function(e) {
    return(list(CallNumber = call_number, Selector = "ERROR", Role = "ERROR"))
  })
}


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║  FUNCTION 6: VERNACULAR TITLE EXTRACTION                                   ║
# ║                                                                            ║
# ║  Extracts a clean vernacular (880) title from a MARC Local Param column    ║
# ║  that contains $$6 245 linked fields. Strips the $$6 prefix and removes    ║
# ║  $$b (subtitle) and $$c (statement of responsibility) subfield markers.    ║
# ║                                                                            ║
# ║  Takes a data frame and the name of the column containing the raw MARC     ║
# ║  local param string. Appends a new column (default: "Title_Vernacular")    ║
# ║  with the cleaned title. Preserves the original row count exactly.         ║
# ║                                                                            ║
# ║  Usage:                                                                    ║
# ║    df <- df |> enrich_vernacular_title(marc_col = "Local Param 01")        ║
# ╚════════════════════════════════════════════════════════════════════════════╝

enrich_vernacular_title <- function(data,
                                    marc_col,
                                    new_col = "Title_Vernacular") {

  original_n <- nrow(data)

  raw <- data[[marc_col]]

  cleaned <- raw |>
    str_extract("\\$\\$6 245-(01|02)(/\\$1)? \\$\\$a(.*?)(?=; \\$\\$6)") |>
    str_replace("^\\$\\$6 245-(01|02)(/\\$1)? \\$\\$a", "") |>
    str_replace("(= )?\\$\\$b", "") |>
    str_replace("(= )?\\$\\$c", "")

  data[[new_col]] <- cleaned

  stopifnot(
    "Row count changed after vernacular title extraction — this should never happen." =
      nrow(data) == original_n
  )

  matched <- sum(!is.na(data[[new_col]]))
  message("Done. ", matched, " of ", original_n, " rows had a vernacular title.")

  data
}
# ╔════════════════════════════════════════════════════════════════════════════╗
# ║  FUNCTION 7: HATHITRUST API — RIGHTS STATUS & CATALOG URL                  ║
# ║                                                                            ║
# ║  Uses HathiTrust's public Bibliographic API to look up rights status and   ║
# ║  a catalog URL for each identifier in a data frame. No API key required    ║
# ║                                                                            ║
# ║  Takes a data frame and the name of the column containing identifiers.     ║
# ║  id_type can be "oclc", "isbn", "issn", or "lccn" (default "oclc").        ║
# ║                                                                            ║
# ║  Appends 4 columns with the full rights picture, not a collapsed           ║
# ║  Yes/No (that binary form is useful only for comparing against             ║
# ║  GreenGlass-style exports -- see validate_hathitrust.R):                   ║
# ║    "HathiTrust Rights Status"       - raw usRightsString ("Full view" /    ║
# ║                                       "Limited (search-only)")             ║
# ║    "HathiTrust Rights Code"         - raw rightsCode(s), e.g. "pd",        ║
# ║                                       "cc-by-4.0"; semicolon-joined if     ║
# ║                                       a title's items disagree             ║
# ║    "HathiTrust Rights Description"  - rightsCode(s) mapped to their full   ║
# ║                                       description via HATHITRUST_RIGHTS_   ║
# ║                                       CODES (sourced from HathiTrust's     ║
# ║                                       Rights Database docs)                ║
# ║    "HathiTrust URL"                 - link to the catalog record           ║
# ║                                                                            ║
# ║                                                                            ║
# ║  Identifiers are normalized and deduplicated before querying               ║
# ║                                                                            ║
# ║                                                                            ║
# ║  Identifiers are batched up to `chunk_size` (default 20 — HathiTrust's     ║
# ║  documented cap) per HTTP request via their multi-id search spec           ║
# ║                                                                            ║
# ║  Requests are fired sequentially with a pause of `delay_seconds`           ║
# ║  (default 0.3s) between each.                                              ║
# ║                                                                            ║
# ║  checkpoint_path (optional): pass a file path to persist results as they   ║
# ║  come in and resume automatically on the next call, skipping identifiers   ║
# ║  already resolved. Recommended for very large one-off runs (tens of        ║
# ║  thousands of identifiers+), so an interruption doesn't lose progress.     ║
# ║                                                                            ║
# ║  Usage:                                                                    ║
# ║    df <- df |> enrich_hathitrust(id_col = "OCLC Number")                   ║
# ║    df <- df |> enrich_hathitrust(id_col = "ISBN", id_type = "isbn")        ║
# ║    df <- df |> enrich_hathitrust(id_col = "OCLC Number",                   ║
# ║                                  checkpoint_path = "ht_checkpoint.csv")    ║
# ╚════════════════════════════════════════════════════════════════════════════╝


#  Identifier normalization 
# Real library data is messy in predictable ways. Without this, identifiers
# that are perfectly valid but non-canonically formatted (hyphenated ISBNs,
# OCLC numbers with library-system prefixes, ISSNs that lost a leading zero
# somewhere upstream) silently come back "Not Found" -- which looks like a
# missing HathiTrust record, but is actually a request we built wrong.
# Full rightsCode -> description mapping, sourced directly from HathiTrust's
# Rights Database documentation "Attributes" table:
# https://www.hathitrust.org/the-collection/preservation/rights-database/
# Used to give enrich_hathitrust() callers the actual rights description,
# not just a collapsed Yes/No -- last checked/updated per that page's
# current content as of this writing.
HATHITRUST_RIGHTS_CODES <- c(
  "pd"              = "Public domain",
  "ic"              = "In-copyright",
  "op"              = "Out-of-print (implies in-copyright)",
  "orph"            = "Copyright-orphaned (implies copyright)",
  "und"             = "Undetermined copyright status",
  "umall"           = "Available to UM affiliates and walk-in patrons (all campuses)",
  "ic-world"        = "In-copyright, permitted as world viewable by the copyright holder",
  "nobody"          = "Available to nobody; blocked for all users",
  "pdus"            = "Public domain only when viewed in the US",
  "cc-by-3.0"       = "Creative Commons Attribution license, 3.0 Unported",
  "cc-by-nd-3.0"    = "Creative Commons Attribution-NoDerivatives license, 3.0 Unported",
  "cc-by-nc-nd-3.0" = "Creative Commons Attribution-NonCommercial-NoDerivatives license, 3.0 Unported",
  "cc-by-nc-3.0"    = "Creative Commons Attribution-NonCommercial license, 3.0 Unported",
  "cc-by-nc-sa-3.0" = "Creative Commons Attribution-NonCommercial-ShareAlike license, 3.0 Unported",
  "cc-by-sa-3.0"    = "Creative Commons Attribution-ShareAlike license, 3.0 Unported",
  "orphcand"        = "Orphan candidate, in 90-day holding period (implies in-copyright)",
  "cc-zero"         = "Creative Commons Zero license (implies public domain)",
  "und-world"       = "Undetermined copyright status, permitted as world viewable by the depositor",
  "icus"            = "In-copyright in the US",
  "cc-by-4.0"       = "Creative Commons Attribution 4.0 International license",
  "cc-by-nd-4.0"    = "Creative Commons Attribution-NoDerivatives 4.0 International license",
  "cc-by-nc-nd-4.0" = "Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International license",
  "cc-by-nc-4.0"    = "Creative Commons Attribution-NonCommercial 4.0 International license",
  "cc-by-nc-sa-4.0" = "Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International license",
  "cc-by-sa-4.0"    = "Creative Commons Attribution-ShareAlike 4.0 International license",
  "pd-pvt"          = "Public domain, but access limited due to privacy concerns",
  "supp"            = "Suppressed from view"
)
 
normalize_identifier <- function(x, id_type) {
  x <- trimws(x)

  if (id_type == "oclc") {
    # Strip common MARC-style OCLC prefixes: (OCoLC)12345, ocm12345,
    # ocn12345, on12345 -- then anything else non-digit.
    x <- gsub("^\\(OCoLC\\)", "", x, ignore.case = TRUE)
    x <- gsub("^(ocm|ocn|on)", "", x, ignore.case = TRUE)
    x <- gsub("[^0-9]", "", x)
  } else if (id_type %in% c("isbn", "issn")) {
    # Strip hyphens/spaces (e.g. "978-0-13-468599-1" -> "9780134685991").
    # Keep any trailing X check digit (valid in both ISBN-10 and ISSN).
    x <- gsub("[- ]", "", x)
    x <- toupper(x)
  }

  if (id_type == "issn") {
    # ISSNs are always 8 characters; a leading zero lost somewhere upstream
    # (e.g. Excel treating the column as numeric) breaks an exact match.
    needs_pad <- !is.na(x) & x != "" & nchar(x) < 8 & nchar(x) > 0
    x[needs_pad] <- formatC(x[needs_pad], width = 8, flag = "0")
  }

  x[x == ""] <- NA_character_
  x
}

enrich_hathitrust <- function(data,
                              id_col           = NULL,
                              id_type          = c("oclc", "isbn", "issn", "lccn"),
                              id_cols          = NULL,
                              new_col_status   = "HathiTrust Rights Status",
                              new_col_code     = "HathiTrust Rights Code",
                              new_col_desc     = "HathiTrust Rights Description",
                              new_col_url      = "HathiTrust URL",
                              new_col_url_all  = "HathiTrust URL (All Records)",
                              new_col_manual_url = "HathiTrust Manual Check URL",
                              new_col_id_type  = "HathiTrust ID Type Used",
                              new_col_id_value = "HathiTrust ID Value Used",
                              chunk_size       = 20,
                              delay_seconds    = 0.3,
                              checkpoint_path  = NULL) {
 
  valid_types <- c("oclc", "isbn", "issn", "lccn")
 
  #  Input validation: exactly one of id_col or id_cols must be given 
  if (is.null(id_col) && is.null(id_cols)) {
    stop("Must supply either id_col (a single identifier column) or id_cols ",
         "(a named vector of columns in priority order, e.g. c(oclc = ",
         "\"OCLC No.\", issn = \"ISSN\", isbn = \"ISBN\")).", call. = FALSE)
  }
  if (!is.null(id_col) && !is.null(id_cols)) {
    stop("Supply either id_col or id_cols, not both.", call. = FALSE)
  }
 
  if (!is.null(id_cols)) {
    if (is.null(names(id_cols)) || any(names(id_cols) == "")) {
      stop("id_cols must be a named vector -- names give the id_type per ",
           "column, in priority order, e.g. c(oclc = \"OCLC No.\", issn = ",
           "\"ISSN\", isbn = \"ISBN\").", call. = FALSE)
    }
    bad_types <- setdiff(names(id_cols), valid_types)
    if (length(bad_types) > 0) {
      stop("id_cols has invalid id_type name(s): ", paste(bad_types, collapse = ", "),
           ". Valid types: ", paste(valid_types, collapse = ", "), call. = FALSE)
    }
    missing_cols <- setdiff(unname(id_cols), names(data))
    if (length(missing_cols) > 0) {
      stop("id_cols references column(s) not found in data: ",
           paste(missing_cols, collapse = ", "), call. = FALSE)
    }
  } else {
    id_type <- match.arg(id_type)
    if (!id_col %in% names(data)) {
      stop(sprintf("Column '%s' not found in data frame.", id_col), call. = FALSE)
    }
  }
 
  if (chunk_size > 20) {
    warning("chunk_size > 20 requested; HathiTrust's documented cap is 20. Clamping to 20.",
            call. = FALSE)
    chunk_size <- 20
  }
 
  original_n <- nrow(data)
 
  #  Resolve, per row, which identifier to actually use. 
  # Single-column mode (id_col/id_type): every row uses the same type.
  # Multi-column mode (id_cols): tries each column in the order given,
  # taking the first one that isn't blank/NA for that row -- e.g. OCLC first,
  # falling back to ISSN, then ISBN, if OCLC is missing for that particular row.
  if (!is.null(id_cols)) {
    chosen_type <- rep(NA_character_, original_n)
    chosen_raw  <- rep(NA_character_, original_n)
 
    for (t in names(id_cols)) {
      col <- id_cols[[t]]
      raw_vals <- trimws(as.character(data[[col]]))
      raw_vals[raw_vals %in% c("", "NA")] <- NA_character_
      needs_fill <- is.na(chosen_type) & !is.na(raw_vals)
      chosen_type[needs_fill] <- t
      chosen_raw[needs_fill]  <- raw_vals[needs_fill]
    }
 
    clean_ids <- rep(NA_character_, original_n)
    for (t in unique(stats::na.omit(chosen_type))) {
      rows_t <- which(chosen_type == t)
      clean_ids[rows_t] <- normalize_identifier(chosen_raw[rows_t], t)
    }
    id_type_per_row <- chosen_type
    id_type_per_row[is.na(clean_ids)] <- NA_character_
 
    message("Priority order for identifiers: ", paste(names(id_cols), collapse = " > "))
  } else {
    id_values <- data[[id_col]]
    clean_ids <- normalize_identifier(trimws(as.character(id_values)), id_type)
    id_type_per_row <- rep(id_type, original_n)
    id_type_per_row[is.na(clean_ids)] <- NA_character_
  }
 
  # Combined key ("type:value") -- used for dedup/chunking/checkpointing so
  # an identifier is never ambiguous about which id_type it was queried as
  # (matters once more than one type can appear in the same run).
  combined_key <- ifelse(is.na(clean_ids) | is.na(id_type_per_row), NA_character_,
                          paste0(id_type_per_row, ":", clean_ids))
 
  key_df <- data.frame(key = combined_key, type = id_type_per_row, value = clean_ids,
                        stringsAsFactors = FALSE)
  key_df <- key_df[!is.na(key_df$key), ]
  unique_keys_df <- key_df[!duplicated(key_df$key), ]
  n_unique <- nrow(unique_keys_df)
 
  message("Looking up HathiTrust rights status for ", n_unique, " unique identifier(s) ",
          "(chunk_size=", chunk_size, ", delay_seconds=", delay_seconds, ")...")
 
  # Checkpoint support: for very large or long-running jobs, skip
  # identifiers already resolved in a previous (possibly interrupted) run,
  # and continue to give new results as we go so a crash mid-run doesn't lose
  # everything already looked up. 
  checkpoint_data <- NULL
  if (!is.null(checkpoint_path) && file.exists(checkpoint_path)) {
    checkpoint_data <- tryCatch(
      utils::read.csv(checkpoint_path, stringsAsFactors = FALSE, colClasses = "character"),
      error = function(e) {
        stop(
          "Could not read existing checkpoint file: ", checkpoint_path, "\n",
          "Original error: ", conditionMessage(e), "\n",
          "This usually means the file is corrupted or incomplete (e.g. from a crash\n",
          "during a write). Check the file manually -- if it's unusable, either fix it\n",
          "or delete it and re-run (you'll just re-query everything from scratch).",
          call. = FALSE
        )
      }
    )
    message("Resuming from checkpoint: ", nrow(checkpoint_data),
            " identifiers already resolved in ", checkpoint_path)
  }
 
  already_done <- if (!is.null(checkpoint_data)) checkpoint_data$identifier else character(0)
  keys_to_query <- setdiff(unique_keys_df$key, already_done)
  pending <- unique_keys_df[match(keys_to_query, unique_keys_df$key), ]
 
  if (nrow(pending) < n_unique) {
    message(n_unique - nrow(pending), " of ", n_unique,
            " identifiers already in checkpoint; querying the remaining ",
            nrow(pending), ".")
  }
 
  chunks <- if (nrow(pending) > 0) {
    split(seq_len(nrow(pending)), ceiling(seq_len(nrow(pending)) / chunk_size))
  } else {
    list()
  }
 
  # Build one request per chunk. HathiTrust's multi-id search spec allows
  # mixing identifier types within a single request (e.g. an OCLC number
  # and an ISSN together) -- each entry carries its own type, so a
  # priority-fallback batch doesn't need separate requests per type. 
  requests <- lapply(chunks, function(idx) {
    spec <- paste(
      vapply(seq_along(idx), function(i) {
        row <- idx[i]
        paste0("id:", i, ";", pending$type[row], ":",
               utils::URLencode(pending$value[row], reserved = TRUE))
      }, character(1)),
      collapse = "|"
    )
    request(paste0("https://catalog.hathitrust.org/api/volumes/brief/json/", spec)) |>
      req_user_agent("csd-functions-library (UCSB Library Collection Strategies)") |>
      req_retry(max_tries = 3, backoff = ~2^.x)
  })
 
  # ── Fetch, parse, and checkpoint ONE CHUNK AT A TIME -- an interruption
  #    risks losing at most one chunk's worth of identifiers, not the whole
  #    run. Results are keyed on the combined "type:value" key. ────────────
  status_vec     <- character(nrow(pending))
  code_vec       <- character(nrow(pending))
  desc_vec       <- character(nrow(pending))
  url_vec        <- character(nrow(pending))
  url_all_vec    <- character(nrow(pending))
  manual_url_vec <- character(nrow(pending))
  names(status_vec) <- names(code_vec) <- names(desc_vec) <- names(url_vec) <-
    names(url_all_vec) <- names(manual_url_vec) <- pending$key
 
  running_results <- if (!is.null(checkpoint_data)) {
    checkpoint_data[, c("identifier", "status", "code", "desc", "url", "url_all", "manual_url")]
  } else {
    data.frame(identifier = character(0), status = character(0), code = character(0),
               desc = character(0), url = character(0), url_all = character(0),
               manual_url = character(0), stringsAsFactors = FALSE)
  }
 
  for (c_i in seq_along(chunks)) {
    idx <- chunks[[c_i]]
 
    resp   <- tryCatch(req_perform(requests[[c_i]]), error = function(e) NULL)
    parsed <- if (is.null(resp)) NULL else tryCatch(resp_body_json(resp), error = function(e) NULL)
 
    for (i in seq_along(idx)) {
      row <- idx[i]
      key <- pending$key[row]
 
      # Built for every identifier regardless of outcome -- most useful on
      # "Not Found"/"Error" rows, so a librarian can manually search
      # HathiTrust's broader catalog (not just an exact-identifier match)
      # in case the title exists under a different identifier than the one
      # queried -- exactly the pattern found during validation (e.g. a
      # record attached to a different OCLC number than the source data).
      # NOTE: this search URL format (VuFind-style) has not been confirmed
      # live against HathiTrust's catalog -- test one manually before
      # relying on it.
      manual_url_vec[key] <- paste0(
        "https://catalog.hathitrust.org/Search/Home?lookfor=",
        utils::URLencode(pending$value[row], reserved = TRUE),
        "&type=all"
      )
 
      if (is.null(parsed)) {
        status_vec[key]  <- "Error"
        code_vec[key]    <- "Error"
        desc_vec[key]    <- "Error -- the request failed after retries. This is not a real answer about the collection; try again, and check the manual search link if it keeps failing."
        url_vec[key]     <- NA_character_
        url_all_vec[key] <- NA_character_
        next
      }
 
      entry   <- parsed[[as.character(i)]]
      records <- entry$records
      items   <- entry$items
 
      if (is.null(records) || length(records) == 0 || is.null(items) || length(items) == 0) {
        status_vec[key]  <- "Not Found"
        code_vec[key]    <- "Not Found"
        desc_vec[key]    <- "Not Found -- no HathiTrust record matched this specific identifier. This does not necessarily mean HathiTrust lacks the title -- it may be catalogued under a different identifier (see the manual search link)."
        url_vec[key]     <- NA_character_
        url_all_vec[key] <- NA_character_
        next
      }
 
      rights_strings <- vapply(items, function(it) {
        if (is.null(it$usRightsString)) NA_character_ else it$usRightsString
      }, character(1))
      rights_codes <- vapply(items, function(it) {
        if (is.null(it$rightsCode)) NA_character_ else it$rightsCode
      }, character(1))
 
      # A title can (rarely) have multiple scanned items with different
      # rights statuses. unique() preserves first-occurrence order from the
      # API's `items` array, which isn't guaranteed stable between separate
      # calls -- sort so the same title always renders identically.
      unique_codes  <- sort(unique(stats::na.omit(rights_codes)))
      unique_status <- sort(unique(stats::na.omit(rights_strings)))
 
      status_vec[key] <- if (length(unique_status) > 0) paste(unique_status, collapse = "; ") else NA_character_
      code_vec[key]   <- if (length(unique_codes) > 0) paste(unique_codes, collapse = "; ") else NA_character_
 
      descriptions <- HATHITRUST_RIGHTS_CODES[unique_codes]
      descriptions[is.na(descriptions)] <- paste0("Unknown code: ", unique_codes[is.na(descriptions)])
      desc_vec[key] <- if (length(descriptions) > 0) paste(descriptions, collapse = "; ") else NA_character_
 
      # A title can also match multiple separate catalog records, each with
      # its own recordURL -- same ordering caveat as above, so sort before
      # picking a deterministic "primary" URL. url_vec keeps one clickable
      # link; url_all_vec keeps every record found.
      record_urls <- vapply(records, function(rec) {
        if (is.null(rec$recordURL)) NA_character_ else rec$recordURL
      }, character(1))
      unique_urls <- sort(unique(stats::na.omit(record_urls)))
 
      url_vec[key]     <- if (length(unique_urls) > 0) unique_urls[1] else NA_character_
      url_all_vec[key] <- if (length(unique_urls) > 0) paste(unique_urls, collapse = "; ") else NA_character_
    }
 
    chunk_keys <- pending$key[idx]
    chunk_results <- data.frame(
      identifier  = chunk_keys,
      status      = unname(status_vec[chunk_keys]),
      code        = unname(code_vec[chunk_keys]),
      desc        = unname(desc_vec[chunk_keys]),
      url         = unname(url_vec[chunk_keys]),
      url_all     = unname(url_all_vec[chunk_keys]),
      manual_url  = unname(manual_url_vec[chunk_keys]),
      stringsAsFactors = FALSE
    )
    running_results <- rbind(running_results, chunk_results)
 
    if (!is.null(checkpoint_path)) {
      tmp_path <- paste0(checkpoint_path, ".tmp")
      utils::write.csv(running_results, tmp_path, row.names = FALSE)
      file.rename(tmp_path, checkpoint_path)
    }
 
    if (c_i < length(chunks)) Sys.sleep(delay_seconds)
  }
 
  all_results <- running_results
 
  # Build lookup and join back, keyed on the same combined "type:value"
  # key that was actually queried against the API 
  lookup <- tibble(
    .id_key             = all_results$identifier,
    !!new_col_status    := all_results$status,
    !!new_col_code      := all_results$code,
    !!new_col_desc      := all_results$desc,
    !!new_col_url       := all_results$url,
    !!new_col_url_all   := all_results$url_all,
    !!new_col_manual_url := all_results$manual_url
  )
 
  data$.id_key <- combined_key
  data <- left_join(data, lookup, by = ".id_key")
  data$.id_key <- NULL
 
  # Records which identifier type actually got used per row -- most useful
  # in id_cols (priority-fallback) mode, where it can vary row to row, but
  # populated in single-column mode too for consistency.
  data[[new_col_id_type]]  <- id_type_per_row
  data[[new_col_id_value]] <- clean_ids
 
  stopifnot(
    "Row count changed after HathiTrust enrichment — this should never happen." =
      nrow(data) == original_n
  )
 
  matched <- sum(!is.na(data[[new_col_url]]))
  message("Done. ", matched, " of ", original_n, " rows matched a HathiTrust record.")
 
  data
}
}