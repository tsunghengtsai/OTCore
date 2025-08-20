# OTCore

Data structures and functions for analysis organic transistor experimental data.

The `OTCore` package provides convenience functions to facilitate analysis of 
organic transistor measurement data. The analysis procedure handles data 
representation, exploration, analysis, summarization and visualization, together 
with helper functions for importing data and exporting analysis results.

## S3 class

* `OTDataSet`: a list of two elements used to restore current-voltage (I-V) 
measurement data from organic transistors and metadata. 

* `OTAnalysis`: an extended class of `OTDataSet` to include extra details about
analysis results summarized at the sweep-level and transistor-level. 

## Functionality

### S3 object constructors and validators

* `new_OTDataSet()`
* `validate_OTDataSet()`
* `OTDataSet()`
* `new_OTAnalysis()`
* `validate_OTAnalysis()`

### Access/wrangling methods for `OTDataSet` and `OTAnalysis`

* `print()`
* `as.data.frame()`
* `head()`
* `tail()`
* `nest()`
* `arrange()`
* `filter()`
* `slice()`
* `mutate()`
* `select()`

### Analysis

* `OT_analyze()`

### Results

* `OT_results()`

### Plotting

* `OT_plot_curve()`
* `OT_plot_param()`

### Import and export

* `OTDataSet_from_files()`
* `OT_export_results()`
* `OT_export_curves()`
* `OT_export_params()`
