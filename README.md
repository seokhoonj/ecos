## ecos package

<!-- badges: start -->

[![R-CMD-check](https://github.com/seokhoonj/ecos/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/seokhoonj/ecos/actions/workflows/R-CMD-check.yaml) 
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/ecos)](https://cran.r-project.org/package=ecos)

<!-- badges: end -->

## Introduction

Economic Statistics System of Bank of Korea (Open API Service)\
(<https://ecos.bok.or.kr/api/#/>)

## Installation

``` r
install.packages("ecos")

# Alternatively
if (!require(devtools)) install.packages("devtools"); require(devtools)  
devtools::install_github("seokhoonj/ecos", force = TRUE)  
```

## examples

``` r
library(ecos)

# data search (if you don't know the stat_code / item_code)
interest_rate <- statSearch(api_key = _your_api_token_)
Please insert stat_code: 902Y006
Please insert item_code1: US

# or simply
interest_rate <- statSearch(api_key = _your_api_token_, stat_code = "902Y006", item_code1 = "US")
```
