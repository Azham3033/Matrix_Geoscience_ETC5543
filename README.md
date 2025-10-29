<h1 align="center">AgriSuit</h1>
<h3 align="center">Parcel-level crop suitability for Tasmania — where data meets soil, and insights grow into opportunity</h3>

<p align="center">
  <a href="https://github.com/<USER>/<REPO>/actions">
    <img src="https://github.com/<USER>/<REPO>/actions/workflows/R-CMD-check.yaml/badge.svg" alt="R-CMD-check" />
  </a>
  <a href="https://cran.r-project.org/">
    <img src="https://img.shields.io/badge/R-%3E%3D%204.3+-blue.svg" alt="R version" />
  </a>
  <a href="LICENSE">
    <img src="https://img.shields.io/badge/license-CC%20BY%204.0-lightgrey.svg" alt="License: CC BY 4.0" />
  </a>
  <a href="#">
    <img src="https://img.shields.io/badge/status-research%20prototype-informational.svg" alt="Status" />
  </a>
</p>

---

<p align="center">
  <img src="docs/img/agrisuit-demo.gif" alt="AgriSuit demo" width="85%"/>
</p>

AgriSuit is an R Shiny app that visualises **biophysical crop suitability** at the **parcel** level using Tasmania’s Enterprise Suitability Mapping (ESM) rasters. It helps agriculture farmers and investors in general make **low-risk land-purchase decisions** by turning suitability layers into **per-parcel counts and maps**.

---

## Features

- Parcel-level suitability counts with fixed class labels (e.g., **1.0 Well Suited**, **1.1 Well Suited (with soil management)**)
- Two-map Leaflet layout with base layers (Satellite, Topography, CartoDB) and WMS 1.1.1 overlays
- Fast queries via DuckDB; exportable tables (CSV/Excel) and SQL preview
- Smart filters: crop search, class filter with “All”, municipality, or full address (must provide one)
- Majority-rule pixel accounting (≥ 50% of pixel inside parcel)
- Defensive validation and parameterised SQL (`sqlInterpolate`) for safety

## Quick start

```r
# R >= 4.3
install.packages(c(
  "shiny","shinyjs","sf","terra","exactextractr","leaflet","DBI","duckdb",
  "dplyr","DT","stringr","foreign","purrr","tidyr","readr","lubridate"
))

# set your paths once (either here or via .Renviron)
Sys.setenv(DB_PATH = "TESM.duckdb")
Sys.setenv(ESM_DIR = "ESM_Tiff/ESM_Tiff")   # contains *.tif + *.tif.vat.dbf

shiny::runApp("app.R")
```


## Minimal config

DB_PATH: DuckDB file with summary tables (created by the pipeline script).

ESM_DIR: folder with ESM rasters and VAT DBFs.

For building DuckDB tables and refreshing summaries, see `Data_Flow.qmd`.


## Method (brief)

- Inputs: ESM GeoTIFF (~80 m) + corresponding .tif.vat.dbf (class labels)

- Geometry: analysis in EPSG:28355; web maps in WGS84

- Counting: exactextractr with majority ≥ 0.5 inclusion threshold

- Validation: visual overlay, area balance, cross-check with terra::extract

See /docs/methodology.md
 and /docs/validation.md
 for details.


## Raw Datasets (Hyperlinks)

1. [Property Listing Dataset](https://www.domain.com.au/sale/?excludeunderoffer=1&landsize=100000-any&landsizeunit=ha&state=tas) — pre-filtered for ≥ 10 ha land in TAS.

<sub>Tip: the link opens with filters applied; no extra instructions needed.</sub>

2. [Cadastral Parcel Data (LIST Open Data)](https://listdata.thelist.tas.gov.au/opendata/)

3. [Address Points Data (LIST Open Data)](https://listdata.thelist.tas.gov.au/opendata/)

4. [Crop Rules Dataset (reference rules)(https://drive.google.com/file/d/1Ng79ZmtG-Ssd4yhOUoprvve3QhnzEqJ1/view?usp=drive_link)

Data are not bundled with the repository. Respect original data licenses/terms from each provider.


## Stack & packages

| Functionality                                | Details                                                                                                        |
| -------------------------------------------- | -------------------------------------------------------------------------------------------------------------- |
| Main programming language to develop the app | R v4.3+                                                                                                        |
| Data wrangling                               | `rvest`, `dplyr`, `stringr`, `tidyr`, `readr`, `purrr`, `tidygeocoder`                                         |
| Data preprocessing                           | `DBI`, `RSQLite`, `tidyverse`, `dbplyr`, `lubridate`, `sf`, `duckdb`                                           |
| Pixel class labels counting                  | `readr`, `dplyr`, `purrr`, `sf`, `terra`, `foreign`, `exactextractr`, `stringr`, `janitor`, `tibble`, `raster` |
| Pixels mapping & web maps                    | `sf`, `terra`, `leaflet`, `stringr`, `foreign`                                                                 |


## Roadmap

- Property-listing integration (≥10 ha) + notifications

- Uncertainty treatments; yield/economic overlays

- WMTS/WCS options, legend improvements, and QA scripts hardening


## Acknowledgments

Built with: R Shiny, sf, terra, exactextractr, leaflet, DuckDB, dplyr, DT, shinyjs.
Data courtesy of NRE Tasmania / LIST data.

## Contact

Issues and feature requests: use GitHub Issues. For general questions, open a Discussion or email us at 
aven0024@student.monash.edu or rram0049@student.monash.edu.
