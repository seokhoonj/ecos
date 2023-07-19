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
##' \donttest{setKey("SetYourKey")}
##'
##' ## Check API key
##' printKey()
##'
##' @export
setKey <- function(api_key) {
  Sys.setenv(ECOS_API_KEY = api_key)
}

##' @export
##' @rdname setKey
printKey <- function() {
  Sys.getenv("ECOS_API_KEY")
}

.getKey <- function() {
  ecos_key <- Sys.getenv("ECOS_API_KEY")
  if (ecos_key == "") {
    stop("Please provide your ECOS API Key", call. = FALSE)
  }
  ecos_key
}
