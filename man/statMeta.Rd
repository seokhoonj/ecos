% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statMeta.R
\name{statMeta}
\alias{statMeta}
\title{statMeta Function}
\usage{
statMeta(api_key, format, lang, count, meta)
}
\arguments{
\item{api_key, format, lang, count, meta}{input parameters}
}
\description{
You can access economic statistics from Bank of Korea through the open API (https://ecos.bok.or.kr/jsp/openapi/OpenApiController.jsp)
}
\examples{
# Economic Statistics Meta from Bank of Korea through the OPEN API
df <- statMeta(api_key = your_api_key, format = "xml", lang = "kr", count = 16, meta = "경제심리")
head(df)
}
\keyword{ecos,}
\keyword{statMeta}
