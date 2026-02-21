# CSD Analytics Functions Library 📊

A centralized collection of R functions designed to streamline CSD data workflows, data analysis, and dashboard development.

---

## 🛠 Environment & Dependencies

![Static Badge](https://img.shields.io/badge/R%20Version-%E2%89%A54.5.2-blue.svg)

![Static Badge](https://img.shields.io/badge/googlesheets4-%E2%89%A51.1.2-blue.svg)
![Static Badge](https://img.shields.io/badge/stringr-%E2%89%A51.6.0-blue.svg)
![Static Badge](https://img.shields.io/badge/purrr-%E2%89%A51.2.1-blue.svg)
![Static Badge](https://img.shields.io/badge/dplyr-%E2%89%A51.1.4-blue.svg)
![Static Badge](https://img.shields.io/badge/httr2-%E2%89%A51.2.2-blue.svg)

---

## 🚀 Functions

### Authentication
* **`get_oclc_token()`**: Fetches a new OCLC access token for specified APIs (WorldCat Metadata/Search). |
* **`ensure_valid_token()`**: Checks token expiration and auto-refreshes if necessary. |

### Enrichment & Data Processing
* **`enrich_lc_classification()`**: Hits the WorldCat Metadata API `classification-bibs` endpoint to retrieve the most frequent LC call number for a given OCLC list.
* **`enrich_uc_overlap()`**: Uses the WorldCat Search API to identify UC-wide holdings. Returns a pipe-separated list of UC institutions holding the item.
* **`enrich_total_holdings()`**: Retrieves the worldwide holding count for OCLC numbers via the `bibs-holdings` endpoint.
* **`enrich_selectors()`**: Joins LC call numbers against a master Google Sheet lookup table to assign Selectors and Roles.
* **`enrich_vernacular_title()`**: Cleans MARC Local Param columns (field 880). It strips `$$6` prefixes and removes `$$b` (subtitle) or `$$c` markers to return a readable vernacular title.

---

## ⚙️ Configuration

### Environment Variables
This library requires an `.Renviron` file to store sensitive API credentials and other details. 
> 💡 **Note:** See `.Renviron.example` in this repository for the required naming conventions and structure.

---

## ⚡ Quick Start

```r
library(httr2)
library(dplyr)
library(purrr)
library(stringr)          #only needed if using enrich_vernacular_title()
library(googlesheets4)    #only needed if using enrich_selectors()

source("csd_function_library.R")
```

---
## 🔄 Dashboards & Apps Using This Kit

- **ILL Borrowing Data Dashboard**: Interlibrary loan analytics
- **Selector Assignment Tool**: Batch call number to selector matching
