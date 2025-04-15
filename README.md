**scm** is an R package that facilitates pre- & post-processing of data on peer nomination & social preference analyzed with _SCM 4.0_ (Leung, 1989) computer program.
The package provides:

a) peer nomination
- as part of data pre-processing (before data analysis with SCM 4.0):
-- quality control of data
-- formatting quality control corrected data as required for SCM 4.0 & exporting as .txt separated by school/stream
- as part of data post-processing (after data analysis with SCM 4.0):
-- import result outputs from SCM 4.0 and format results of interest to one data frame across schools/streams

b) social preference
- calculate social preference within school/stream & format into one result data frame

## Installation

**scm** can be installed from GitHub using the following code. Note: On Windows, [Rtools](https://cran.r-project.org/bin/windows/Rtools/) have to be installed.

```r
if(!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("kattamatta/scm")
```
