# sbcdata

<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![license](https://img.shields.io/badge/license-CC%20BY%204.0-brightgreen.svg?style=flat)](https://creativecommons.org/licenses/by/4.0/)
[![R-CMD-check](https://github.com/ampel-leipzig/sbcdata/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ampel-leipzig/sbcdata/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/ampel-leipzig/sbcdata/branch/main/graph/badge.svg)](https://codecov.io/gh/ampel-leipzig/sbcdata?branch=main)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6922967.svg)](https://doi.org/10.5281/zenodo.6922967)
<!-- badges: end -->

## Installation

You can install the released version of `sbcdata`
from directly from github:

```r
# install.packages("remotes")
remotes::install_github("ampel-leipzig/sbcdata")
```

## About the data

The data were collected as part of the [AMPEL](http://www.ampel-cdss.com/) project
(Analysis and Reporting System for the Improvement of Patient Safety
through Real-Time Integration of Laboratory Findings).
They include administration data, sepsis labels based on ICD10 codes
and laboratory diagnostics from patients admitted to the
University Hospital Leipzig between Januar 2014 and December 2021 and to the
University Hospital Greifswald between Januar 2015 and December 2020,
respectively.
Additionally it provides functions to generate a similar
dataset from MIMIC-IV [Johnson et al. 2021](https://doi.org/10.13026/S6N6-XD98).

## License

The datasets are available under the
[CC-BY](https://creativecommons.org/by/4.0/) license.

## Citation

To cite the `sbcdata` package, please use:

> Applying Machine Learning to Blood Count Data Predicts Sepsis with ICU Admission.
> D. Steinbach, P. C. Ahrens, M. Schmidt, M. Federbusch, L. Heuft, Ch. Lübbert, M. Nauck, M. Gründling, B. Isermann, S. Gibb, Th. Kaiser
> 2024. Clinical Chemistry.
> DOI: [10.1093/clinchem/hvae001](http://doi.org/10.1093/clinchem/hvae001).
