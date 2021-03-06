% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statItemList.R
\name{statItemList}
\alias{statItemList}
\title{statItemList Function}
\usage{
statItemList(api_key, format, lang, count, stat_code)
}
\arguments{
\item{api_key, format, lang, count, stat_code}{input parameters}
}
\description{
You can access economic statistics from Bank of Korea through the OPEN API (https://ecos.bok.or.kr/jsp/openapi/OpenApiController.jsp)
}
\examples{
# Economic Statistic Item List from Bank of Korea through the OPEN API
df <- statItemList(api_key = your_api_key, format = "xml", lang = "kr", count = 100, stat_code = "010Y002")
head(df)
}
\keyword{ecos,}
\keyword{statItemList}
