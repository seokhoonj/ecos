#' statTableList Function
#'
#' You can access economic statistics from Bank of Korea through the OPEN API (https://ecos.bok.or.kr/jsp/openapi/OpenApiController.jsp)
#' @param api_key,format,lang,count input parameters
#' @keywords ecos, statTableList 
#' @export
#' @examples
#' # Economic Statistics Table List from Bank of Korea through the OPEN API
#' df <- statTableList(api_key = your_api_key, format = "xml", lang = "kr", count = 1000)
#' head(df)
statTableList <- function(api_key, format = c("xml", "json"), lang = c("kr", "en"), count) {
	
	if (missing(api_key))
	  stop("Please get your api key from website 'https://ecos.bok.or.kr/jsp/openapi/OpenApiController.jsp'")

	if (missing(count))
		count <- 1000 
		
	if (format[[1]] == "xml") {

		url <- URLencode(sprintf("http://ecos.bok.or.kr/api/StatisticTableList/%s/%s/%s/1/%s/?", 
		                         api_key, format[[1]], lang[[1]], count))
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

	} else if (format[[1]] == "json") {

		url <- URLencode(sprintf("http://ecos.bok.or.kr/api/StatisticTableList/%s/%s/%s/1/%s/?", 
		                         api_key, format[[1]], lang[[1]], count))
		html <- getURLContent(url)
		json_all <- fromJSON(html)

		if (is.null(json_all$RESULT)) {

			cnt <- json_all$StatisticTableList$list_total_count
			df  <- json_all$StatisticTableList$row
			names(df) <- tolower(names(df))
			attr(df, "list_total_count") <- cnt

		} else {

			code <- json_all$RESULT$CODE	
			msg  <- json_all$RESULT$MESSAGE	

			stop(paste0(code, "\n ", msg))

		}

	} else {

		stop("The file format is not supported.")

	}

	return(df); gc()
}

