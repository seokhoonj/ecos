##' Item list of statistics
##'
##' @details
##' \preformatted{
##' ## Example
##' statItemList(lang = "en", count = 100, stat_code = "902Y001")
##' }
##'
##' @param api_key A string specifying ECOS API key. Need not be specified if
##'   the key was stored as an environment variable via \code{\link{setKey}} or
##'   .Renviron.
##' @param stat_code A string specifying the statistical table code
##' @param format A string specifying the file format to process - xml, json
##' @param lang A string specifying the language of result value - kr (Korean),
##'   en (English)
##' @param count An integer specifying the number of requests
##' @return A data.frame object containing queried information
##' @export
statItemList <- function(api_key, stat_code, format = c("xml", "json"),
                         lang = c("kr", "en"), count = 1000) {
	if (missing(api_key)) {
	  # stop("Please create your api key from website 'https://ecos.bok.or.kr/api/#/AuthKeyApply'")
    api_key <- .getKey()
  }
  format <- match.arg(format)
  lang <- match.arg(lang)
	if (missing(stat_code)) {
	  # op <- options("max.print" = .Machine$integer.max)
	  showStatTableList(api_key, format = format, lang = lang)
	  # on.exit(op)
	  stat_code <- readline("Please insert stat_code: ")
	}
  url <- URLencode(
    sprintf("http://ecos.bok.or.kr/api/StatisticItemList/%s/%s/%s/1/%s/%s/",
            api_key, format, lang, count, stat_code)
  )
  html <- GET(url)
  content <- rawToChar(html$content)
	if (format == "xml") {
    .parse_xml(content, type = "item")
	} else {
    .parse_json(content, type = "item")
	}
}
