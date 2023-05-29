##' @details
##'
##' To use this package, you will first need to get your API key from the
##' website \url{https://ecos.bok.or.kr/api/#/AuthKeyApply}. Once you have your
##' key, you can save it as an environment variable for the current session
##' using the \code{\link{setKey}} function. Alternatively, you can set it
##' permanently by adding the following line to your .Renviron file:
##'
##' ECOS_API_KEY = PASTE YOUR API KEY
##'
##' Any functions that require your API key try to retrieve it via
##' \code{Sys.getenv("ECOS_API_KEY")} (unless API key is explicitly specified as
##' a function argument).
##'
##' @importFrom utils URLencode head tail
##' @importFrom httr GET
##' @importFrom XML xmlParse xpathApply xmlToList xmlToDataFrame
##' @importFrom jsonlite fromJSON
##' @importFrom stringr str_pad
"_PACKAGE"
