#' statMeta Function
#'
#' @title Statistics Meta
#' @param api_key,format,lang,count,meta input parameters
#' @keywords ecos, statMeta 
#' @export
#' @examples
#' statMeta(api_key = your_api_key, format = "xml", lang = "en", count = 16, meta = "Economic Sentiment Index")
#' statMeta(api_key = your_api_key, format = "xml", lang = "kr", count = 16, meta = "경제심리지수")
#' 
statMeta <- function(api_key, format = c("xml", "json"), lang = c("kr", "en"), count, meta) {

	if (missing(api_key))
	  stop("Please create your api key from website 'https://ecos.bok.or.kr/jsp/openapi/OpenApiController.jsp'")

	if (missing(count))
		count <- 16 

	if (missing(meta))
		meta <- "경제심리지수" 

	if (format[[1]] == "xml") {

		url <- URLencode(sprintf("http://ecos.bok.or.kr/api/StatisticMeta/%s/%s/%s/1/%s/%s/", 
		                         api_key, format[[1]], lang[[1]], count, meta))
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

		url <- URLencode(sprintf("http://ecos.bok.or.kr/api/StatisticMeta/%s/%s/%s/1/%s/%s/", 
		                         api_key, format[[1]], lang[[1]], count, meta))
		html <- getURLContent(url)
		json_all <- fromJSON(html)

		if (is.null(json_all$RESULT)) {

			cnt <- json_all$StatisticMeta$list_total_count
			df  <- json_all$StatisticMeta$row
			names(df) <- tolower(names(df))
			attr(df, "list_total_count") <- cnt 

		} else {

			code <- json_all$RESULT$CODE	
			msg  <- json_all$RESULT$MESSAGE	

			stop(paste0(code, "\n ", msg))

		}

	} else {

		stop("This file format is not supported.")

	}

	return(df); gc()
}
