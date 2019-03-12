ecos package
---

### Introduction
한국은행 경제통계 시스템  

Economic Statistics System of Bank of Korea through the OPEN API  
(https://ecos.bok.or.kr/jsp/openapi/OpenApiController.jsp?t=main)

### Installation
```r
if (!require(devtools)) install.packages("devtools"); require(devtools)  
devtools::install_github("seokhoonj/ecos")  
library(ecos)
```

### example 
```r
# possible table list
data("tableList")
tableList_srch_Y <- tableList[tableList$srch_yn == "Y",] 

# data search 
market_interest_rates <- statSearch(stat_code = "028Y001", item_code = "BEEA14")

# visualization
library(ggplot2)
ggplot(data = market_interest_rates, aes(x = time, y = data_value)) + geom_line(stat = "identity")
```

