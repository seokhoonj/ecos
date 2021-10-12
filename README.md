ecos package
---

### Introduction
한국은행 경제통계 시스템  

Economic Statistics System of Bank of Korea through the OPEN API  
(https://ecos.bok.or.kr/jsp/openapi/OpenApiController.jsp?t=main)

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
Please insert stat_code: I10Y014
Please insert item_code: US

# or simply
interest_rate <- statSearch(api_key = _your_api_token_, stat_code = "I10Y014", item_code = "US")

```

