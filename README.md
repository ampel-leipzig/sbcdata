# sbcdata

<!-- badges: start -->
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![license](https://img.shields.io/badge/license-CC%20BY%204.0-brightgreen.svg?style=flat)](https://creativecommons.org/licenses/by/4.0/)
[![R build status](https://github.com/ampel-leipzig/sbcdata/workflows/R-CMD-check/badge.svg)](https://github.com/ampel-leipzig/sbcdata/actions)
[![Codecov test coverage](https://codecov.io/gh/ampel-leipzig/sbcdata/branch/main/graph/badge.svg)](https://codecov.io/gh/ampel-leipzig/sbcdata?branch=main)
<!-- badges: end -->

## Installation

You can install the released version of `sbcdata`
from directly from github:

```r
# install.packages("remotes")
remotes::install_github("ampel-leipzig/sbcdata")
```

## About the data

The data were collected as part of the [AMPEL](https://ampel.care) project
(Analysis and Reporting System for the Improvement of Patient Safety
through Real-Time Integration of Laboratory Findings).
They include administration data, sepsis labels based on ICD10 codes
and laboratory diagnostics from patients admitted to the
University Hospital Leipzig between Januar 2014 and December 2019 and to the
University Hospital Greifswald between Januar 2015 and December 2020,
respectively.
Additionally it provides functions to generate a similar
dataset from MIMIC-IV [Johnson et al. 2021](https://doi.org/10.13026/S6N6-XD98).

## License

The datasets are available under the
[CC-BY](https://creativecommons.org/by/4.0/) license.

## Citation

To cite the `sbcdata` package, please use:

> Sebastian Gibb, Paul Ahrens, Daniel Steinbach and Thorsten Kaiser
(2021). sbcdata: Laboratory Diagnostics from Septic and Non-septic
Patients Used in the AMPEL Project. R package version 0.0.1.
https://github.com/ampel-leipzig/sbcdata
