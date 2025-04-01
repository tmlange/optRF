# optRF: Optimising random forest stability by determining the optimal number of trees  <img src="man/figures/logo.png" align="right" height="120" alt="" />

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/optRF?color=green)](https://CRAN.R-project.org/package=optRF)
![CRAN Downloads month](https://cranlogs.r-pkg.org/badges/optRF?color=brightgreen)
![CRAN Downloads overall](https://cranlogs.r-pkg.org/badges/grand-total/optRF?color=brightgreen)
<!-- badges: end -->

The optRF package provides tools for optimizing the number of trees in a random forest to improve model stability and reproducibility. Since random forest is a non-deterministic method, variable importance and prediction results can vary between runs. The optRF package estimates the stability of random forest based on the number of trees and helps users determine the optimal number of trees required for reliable predictions and variable selection.

## Installation
To install the optRF R package from CRAN, just run

``` r
install.packages("optRF")
```
R version >= 3.6 is required.  
You can install the development version of optRF from [GitHub](https://github.com/tmlange/optRF) using `devtools` with:

``` r
devtools::install_github("tmlange/optRF")
```

## Usage

The optRF package includes the `SNPdata` data set for demonstration purposes. The two main functions are:

* `opt_prediction` – Finds the optimal number of trees for stable predictions.
* `opt_importance` – Finds the optimal number of trees for stable variable importance estimates.

``` r
library(optRF)

# Load example data set
data(SNPdata)

# Optimise random forest for predicting the first column in SNPdata
result_optpred = opt_prediction(y = SNPdata[,1], X=SNPdata[,-1])
summary(result_optpred)

# Optimise random forest for calculating variable importance
result_optimp = opt_importance(y = SNPdata[,1], X=SNPdata[,-1]) 
summary(result_optimp)
```
For detailed examples and explanations, refer to the package vignettes:  

* `optRF` – General package overview  
* `opt_prediction` – Optimizing random forest predictions  
* `opt_importance` – Optimizing random forest variable importance estimation  

## Citing optRF
If you use optRF in your research, please cite:  
[Lange, T.M., Gültas, M., Schmitt, A.O. & Heinrich, F. optRF: Optimising random forest stability by determining the optimal number of trees. BMC Bioinformatics 26, 95 (2025).](https://doi.org/10.1186/s12859-025-06097-1)
