#' keyStatList Function
#'
#' You can access economic statistics from Bank of Korea through the open API (https://ecos.bok.or.kr/jsp/openapi/OpenApiController.jsp)
#' @param api_key,format,lang,count input parameters
#' @keywords ecos, keyStatList 
#' @export
#' @examples
#' # 100 Key Economic Statistics from Bank of Korea through the OPEN API
#' df <- keyStatList(api_key = your_api_key, format = "xml", lang = "kr", count = 100)
#' head(df)
keyStatList <- function(api_key, format, lang, count) {

	if (missing(api_key))
	  stop("Please get your api key from website 'https://ecos.bok.or.kr/jsp/openapi/OpenApiController.jsp'")

	if (missing(format))
	  format <- "xml"

	if (missing(lang))
	  lang <- "kr"

	if (missing(count))
	  count <- 100

	if (format == "xml") {

		url <- URLencode(sprintf("http://ecos.bok.or.kr/api/KeyStatisticList/%s/%s/%s/1/%s/",
								 api_key, format, lang, count))
		html <- getURLContent(url)
		xml_all <- xmlParse(html)

		if (is.null(unlist(xpathApply(xml_all, "//RESULT")))) {

			xml_cnt <- xpathApply(xml_all, "//list_total_count")[[1]]
			cnt <- as.integer(xmlToList(xml_cnt))
			xml_row <- xpathApply(xml_all, "//row") 
			df <- xmlToDataFrame(xml_row, stringsAsFactors = FALSE)
			names(df) <- tolower(names(df))
			attr(df, "list_total_count") <- cnt 

		} else {

			code <- xmlToList(xpathApply(xml_all, "//CODE")[[1]])
			msg  <- xmlToList(xpathApply(xml_all, "//MESSAGE")[[1]])

			stop(paste0(code, "\n ", msg))

		}

	} else if (format == "json") {

		url <- URLencode(sprintf("http://ecos.bok.or.kr/api/KeyStatisticList/%s/%s/%s/1/%s/",
								 api_key, format, lang, count))
		html <- getURLContent(url)
		json_all <- fromJSON(html)

		if (is.null(json_all$RESULT)) {

			cnt <- json_all$KeyStatisticList$list_total_count
			df  <- json_all$KeyStatisticList$row
			names(df) <- tolower(names(df))
			attr(df, "list_total_count") <- cnt

		} else {

			code <- json_all$RESULT$CODE	
			msg  <- json_all$RESULT$MESSAGE	

			stop(paste0(code, "\n ", msg))

		}

	} else {

		stop("not supported data format.")

	}

	return(df); gc()
}
