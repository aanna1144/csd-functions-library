# CSD-Analytics-Functions-Library
Consists of frequently used functions in CSD data workflows, analysis or dashboard development.

Dependencies:
1) googlesheets4_1.1.2
2) stringr_1.6.0
3) purrr_1.2.1 
4) dplyr_1.1.4 
5) httr2_1.2.2 

R version: 4.5.2

Functions:
1) get_oclc_token: Fetches a new OCLC access token for the specified API (currently supports WorldCatMetaDataAPI & WorldCatSearchAPI) and stores it.
        &
   ensure_valid_token: Checks if the current token for the specified API is still valid; refreshes if not.
   
2) enrich_lc_classification: Uses the WorldCat Metadata API classification-bibs endpoint to get the most popular LC call number for each OCLC number in a data frame.
3) enrich_uc_overlap:  Uses the WorldCat Search API bibs-holdings endpoint filtered by UC institution symbols. For each OCLC number, returns a pipe-separated list of UC institution names that hold the item.
4) enrich_total_holdings: Uses the WorldCat Search API bibs-holdings endpoint to get the total worldwide holding count for each OCLC number.
5) enrich_selectors: Assigns a Selector and Role to each row based on its LC call number, using a lookup table stored in a Google Sheets maintained by CSD Selectors & Director.    
6) enrich_vernacular_title: Extracts a clean vernacular (880) title from a MARC Local Param column that contains $$6 245 linked fields. Strips the $$6 prefix and removes $$b (subtitle) and $$c (statement of responsibility) subfield markers.

This script requires a Renviron file; example for which is included in the repository.

The following repositories make use of this functions library:
1) 
