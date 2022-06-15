ecos package
---

### Introduction
Economic Statistics System of Bank of Korea through the OPEN API  
(https://ecos.bok.or.kr/api/#/)

### Installation
```r
if (!require(devtools)) install.packages("devtools"); require(devtools)  
devtools::install_github("seokhoonj/ecos", force = TRUE)  
library(ecos)
```

### examples
```r
# data search (if you don't know the stat_code / item_code)
interest_rate <- statSearch(api_key = _your_api_token_)
Please insert stat_code: 902Y006
Please insert item_code1: US

# or simply
interest_rate <- statSearch(api_key = _your_api_token_, stat_code = "902Y006", item_code1 = "US")
```
