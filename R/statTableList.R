##' Table list of statistical tables
##'
##' @details
##' \preformatted{
##' ## Example
##' statTableList(lang = "en", count = 100)
##' }
##'
## @param api_key A string specifying ECOS API key. Need not be specified if
##   the key was stored as an environment variable via \code{\link{setKey}} or
##   .Renviron.
##' @param format A string specifying the file format to process - xml, json
##' @param lang A string specifying the language of result value - kr (Korean),
##'   en (English)
##' @param count An integer specifying the number of requests
##' @param stat_code A string specifying the statistical table code
##' @return A data.frame object containing queried information
##' @export
statTableList <- function(format = c("xml", "json"), lang = c("kr", "en"), 
                          count = 1000, stat_code) {
  api_key <- ecos.getKey()
  format <- match.arg(format)
  lang <- match.arg(lang)
  if (missing(stat_code)) 
    stat_code <- ""
  url <- URLencode(
    sprintf("https://ecos.bok.or.kr/api/StatisticTableList/%s/%s/%s/1/%s/%s",
            api_key, format, lang, count, stat_code)
  )
  html <- httr::GET(url)
  content <- rawToChar(html$content)
	if (format == "xml") {
    parseXML(content, type = "table")
	} else {
    parseJSON(content, type = "table")
	}
}
