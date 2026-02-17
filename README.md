# CSD Analytics Functions Library 📊

A centralized collection of R functions designed to streamline CSD data workflows, bibliometric analysis, and dashboard development.

---

## 🛠 Environment & Dependencies

| Requirement | Version |
| :--- | :--- |
| **R Version** | `4.5.2` |

### Required Packages
This library relies on the following R packages for API interaction and data manipulation:

* `googlesheets4` (v1.1.2)
* `stringr` (v1.6.0)
* `purrr` (v1.2.1)
* `dplyr` (v1.1.4)
* `httr2` (v1.2.2)

---

## 🚀 Key Functions

### Authentication
| Function | Description |
| :--- | :--- |
| `get_oclc_token()` | Fetches a new OCLC access token for specified APIs (WorldCat Metadata/Search). |
| `ensure_valid_token()` | Checks token expiration and auto-refreshes if necessary. |

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

