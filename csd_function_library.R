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

# ── Identifier normalization ─────────────────────────────────────────────────
# Full rightsCode -> description mapping, sourced directly from HathiTrust's
# Rights Database documentation "Attributes" table:
# https://www.hathitrust.org/the-collection/preservation/rights-database/
# Used to give enrich_hathitrust() callers the actual rights description,
# - last checked/updated per that page's current content as of this writing.

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
    # ocn12345, on12345 - then anything else non-digit.
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
                              id_col,
                              id_type          = c("oclc", "isbn", "issn", "lccn"),
                              new_col_status   = "HathiTrust Rights Status",
                              new_col_code     = "HathiTrust Rights Code",
                              new_col_desc     = "HathiTrust Rights Description",
                              new_col_url      = "HathiTrust URL",
                              chunk_size   = 20,
                              delay_seconds = 0.3,
                              checkpoint_path = NULL) {

  id_type <- match.arg(id_type)

  if (chunk_size > 20) {
    warning("chunk_size > 20 requested; HathiTrust's documented cap is 20. Clamping to 20.",
            call. = FALSE)
    chunk_size <- 20
  }

  original_n <- nrow(data)
  id_values  <- data[[id_col]]

  clean_ids  <- normalize_identifier(trimws(as.character(id_values)), id_type)
  unique_ids <- unique(na.omit(clean_ids))
  n_unique   <- length(unique_ids)

  message("Looking up HathiTrust rights status for ", n_unique, " unique ", toupper(id_type),
          " values (chunk_size=", chunk_size, ", delay_seconds=", delay_seconds, ")...")

  #  Checkpoint support: for very large or long-running jobs, skip
  #  identifiers already resolved in a previous (possibly interrupted) run,
  #  and persist new results as we go so a crash mid-run doesn't lose
  #  everything already looked up. 
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
  ids_to_query <- setdiff(unique_ids, already_done)

  if (length(ids_to_query) < n_unique) {
    message(n_unique - length(ids_to_query), " of ", n_unique,
            " identifiers already in checkpoint; querying the remaining ",
            length(ids_to_query), ".")
  }

  chunks <- split(ids_to_query, ceiling(seq_along(ids_to_query) / chunk_size))
  if (length(ids_to_query) == 0) chunks <- list()

  #  Build one request per chunk (each covers up to chunk_size identifiers
  #  via HathiTrust's documented multi-id search spec) 
  requests <- lapply(chunks, function(chunk) {
    spec <- paste(
      vapply(seq_along(chunk), function(i) {
        paste0("id:", i, ";", id_type, ":", utils::URLencode(chunk[i], reserved = TRUE))
      }, character(1)),
      collapse = "|"
    )
    request(paste0("https://catalog.hathitrust.org/api/volumes/brief/json/", spec)) |>
      req_user_agent("csd-functions-library (UCSB Library Collection Strategies)") |>
      req_retry(max_tries = 3, backoff = ~2^.x)
  })

  # Fetch, parse, and checkpoint one chunk at a time.
  # Processing and saving each chunk fully before moving to the next means at most 
  # one chunk's worth of work (up to chunk_size identifiers) is ever at risk.
  # NOTE on sizing: status_vec/code_vec/desc_vec/url_vec below only hold
  # results for `ids_to_query` - the identifiers being fetched THIS run,
  # not every identifier in the original data. Anything already resolved
  # in a prior run was excluded from ids_to_query earlier (see
  # "already_done"/"setdiff" above); it's carried forward via
  # `running_results` (seeded from the checkpoint)
  n_query <- length(ids_to_query)
  status_vec <- character(n_query)  # raw usRightsString ("Full view" / "Limited (search-only)")
  code_vec   <- character(n_query)  # raw rightsCode(s), semicolon-joined if more than one
  desc_vec   <- character(n_query)  # rightsCode(s) mapped to their full description
  url_vec    <- character(n_query)
  names(status_vec) <- names(code_vec) <- names(desc_vec) <- names(url_vec) <- ids_to_query

  running_results <- if (!is.null(checkpoint_data)) {
    checkpoint_data[, c("identifier", "status", "code", "desc", "url")]
  } else {
    data.frame(identifier = character(0), status = character(0), code = character(0),
               desc = character(0), url = character(0), stringsAsFactors = FALSE)
  }

  for (c_i in seq_along(chunks)) {
    chunk <- chunks[[c_i]]

    resp   <- tryCatch(req_perform(requests[[c_i]]), error = function(e) NULL)
    parsed <- if (is.null(resp)) NULL else tryCatch(resp_body_json(resp), error = function(e) NULL)

    for (i in seq_along(chunk)) {
      ident <- chunk[i]

      if (is.null(parsed)) {
        status_vec[ident] <- "Error"
        code_vec[ident]   <- "Error"
        desc_vec[ident]   <- "Error"
        url_vec[ident]    <- NA_character_
        next
      }

      entry   <- parsed[[as.character(i)]]
      records <- entry$records
      items   <- entry$items

      if (is.null(records) || length(records) == 0 || is.null(items) || length(items) == 0) {
        status_vec[ident] <- "Not Found"
        code_vec[ident]   <- "Not Found"
        desc_vec[ident]   <- "Not Found"
        url_vec[ident]    <- NA_character_
        next
      }

      rights_strings <- vapply(items, function(it) {
        if (is.null(it$usRightsString)) NA_character_ else it$usRightsString
      }, character(1))
      rights_codes <- vapply(items, function(it) {
        if (is.null(it$rightsCode)) NA_character_ else it$rightsCode
      }, character(1))

      # A title can (rarely) have multiple scanned items with different
      # rights statuses. Report every distinct code/status found, rather
      # than silently collapsing to one
      # unique() preserves first-occurrence order from the API's `items`
      # array, which isn't guaranteed stable between separate calls. Sort so
      # a title with multiple rights statuses always renders the same way
      # regardless of what order HathiTrust happened to list its items in.
      unique_codes <- sort(unique(stats::na.omit(rights_codes)))
      unique_status <- sort(unique(stats::na.omit(rights_strings)))

      status_vec[ident] <- if (length(unique_status) > 0) paste(unique_status, collapse = "; ") else NA_character_
      code_vec[ident]   <- if (length(unique_codes) > 0) paste(unique_codes, collapse = "; ") else NA_character_

      descriptions <- HATHITRUST_RIGHTS_CODES[unique_codes]
      descriptions[is.na(descriptions)] <- paste0("Unknown code: ", unique_codes[is.na(descriptions)])
      desc_vec[ident] <- if (length(descriptions) > 0) paste(descriptions, collapse = "; ") else NA_character_

      first_record    <- records[[1]]
      url_vec[ident]  <- if (is.null(first_record$recordURL)) NA_character_ else first_record$recordURL
    }

    # Pushes THIS chunk's results immediately, before moving on
    chunk_results <- data.frame(
      identifier = chunk,
      status = unname(status_vec[chunk]),
      code   = unname(code_vec[chunk]),
      desc   = unname(desc_vec[chunk]),
      url    = unname(url_vec[chunk]),
      stringsAsFactors = FALSE
    )
    running_results <- rbind(running_results, chunk_results)

    if (!is.null(checkpoint_path)) {
      # Write to a temp file, then rename over the real path.
      # Guards against a crash happening mid-write 
      tmp_path <- paste0(checkpoint_path, ".tmp")
      utils::write.csv(running_results, tmp_path, row.names = FALSE)
      file.rename(tmp_path, checkpoint_path)
    }

    if (c_i < length(chunks)) Sys.sleep(delay_seconds)
  }

  all_results <- running_results

  # Build lookup and join back, keyed on the same normalized identifier
  # that was queried against the API 
  lookup <- tibble(
    .id_key          = all_results$identifier,
    !!new_col_status := all_results$status,
    !!new_col_code   := all_results$code,
    !!new_col_desc   := all_results$desc,
    !!new_col_url    := all_results$url
  )

  data$.id_key <- clean_ids
  data <- left_join(data, lookup, by = ".id_key")
  data$.id_key <- NULL

  stopifnot(
    "Row count changed after HathiTrust enrichment — this should never happen." =
      nrow(data) == original_n
  )

  matched <- sum(!is.na(data[[new_col_url]]))
  message("Done. ", matched, " of ", original_n, " rows matched a HathiTrust record.")

  data
}
