
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OTCore

<!-- badges: start -->

<!-- badges: end -->

The `OTCore` package provides convenience functions to facilitate
analysis of organic transistor measurement data. The analysis procedure
handles data representation, exploration, analysis, summarization and
visualization, together with helper functions for importing data and
exporting analysis results.

## Installation

You can install the development version of OTCore from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tsunghengtsai/OTCore")
```

## S3 class

Two primary data objects used in the `OTCore` analysis procedure are

- `OTDataSet`: a list of two elements used to restore current-voltage
  (I-V) measurement data from organic transistors and metadata.
  - Constructor: `new_OTDataSet()`
  - Validator: `validate_OTDataSet()`
  - Helper: `OTDataSet()`
- `OTAnalysis`: an extended class of `OTDataSet` to include extra
  details about analysis results summarized at the sweep-level and
  transistor-level.
  - Constructor: `new_OTAnalysis()`
  - Validator: `validate_OTAnalysis()`

## Core functionality

### Access/wrangling methods for `OTDataSet` and `OTAnalysis`

- Quick overview: `print()`, `as.data.frame()`, `head()`, `tail()`
- Data wrangling: `nest()`, `arrange()`, `filter()`, `slice()`,
  `mutate()`, `select()`

### Analysis

- Core analysis function: `OT_analyze()` with the following analysis
  steps
  - Quality assessment: `assess_transfer_curve()`
  - Parameter estimation: `characterize_transfer_curve()`
  - Sweep-to-transistor summary: `summarize_sweeps()`
- Evaluation and correction of outlying sweeps/transistors:
  - `OT_flag_display()`
  - `OT_flag_modify()`

### Results

- Group-level summary: `OT_results()`

### Plots

- Plot organic transistor measurement curve(s): `OT_plot_curve()`
- Plot estimates of organic transistor parameters: `OT_plot_param()`

### Input/output helpers

- Create an `OTDataSet` from files: `OTDataSet_from_files()`
- Export analysis results: `OT_export_results()`
- Export measurement curve plots: `OT_export_curves()`
