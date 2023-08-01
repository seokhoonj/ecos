##' Set ECOS API Key
##'
##' @description
##' Save ECOS API key for the current session. To set it permanently, please add
##' the following line to your .Renvrion file:
##'
##' ECOS_API_KEY = YOUR API KEY
##'
##' @param api_key A string specifying ECOS API key
##' @return No return value, called to set api key
##' @examples
##'
##' ## Set API Key for the current session
##' \donttest{ecos.setKey("your_api_key")}
##'
##' ## Check API key
##' ecos.printKey()
##'
##' @export
ecos.setKey <- function(api_key) {
  Sys.setenv(ECOS_API_KEY = api_key)
}

##' @export
##' @rdname ecos.setKey
ecos.printKey <- function() {
  Sys.getenv("ECOS_API_KEY")
}

ecos.getKey <- function() {
  api_key <- Sys.getenv("ECOS_API_KEY")
  if (api_key == "") {
    stop("Please run this code to provide your ECOS API Key: ecos.setKey('your_api_key').", call. = FALSE)
  }
  api_key
}
