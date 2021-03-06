#' statSearch Function
#'
#' You can access economic statistics from Bank of Korea through the open API (https://ecos.bok.or.kr/jsp/openapi/OpenApiController.jsp)
#' @param api_key,format,lang,count,stat_code,cycle,start_time,end_time,item_code input parameters
#' @keywords ecos, statSearch
#' @export
#' @examples
#' # stock index futures 
#' df <- statSearch(api_key = your_api_key, format = "xml", lang = "kr", stat_code = "085Y007", item_code = "S25B", cycle = "MM", start_time = "196001", end_time = "201812", count = 1000)
#' head(df)
statSearch <- function(api_key, format, lang, stat_code, item_code, cycle, start_time, end_time, count) {
 
	if (missing(api_key)) 
	  stop("Please get your api key from website 'https://ecos.bok.or.kr/jsp/openapi/OpenApiController.jsp'")

	if (missing(format)) 
	  format <- "xml"		# file format

	if (missing(lang)) 
	  lang <- "kr"		# en is second option
	
	if (missing(stat_code)) { 
	  
	  options('max.print'=.Machine$integer.max)
	  stat_list <- statTableList(api_key=api_key)
	  print(stat_list[, -length(stat_list)])
	  options('max.print'=1000)
	  stat_code <- readline('Please insert stat_code: ')
	  item_list <- statItemList(api_key = api_key, stat_code = stat_code)
	  
	} else {
	  
	  item_list <- statItemList(api_key = api_key, stat_code = stat_code)
	  
	}
	
	if (missing(item_code)) {
	  
	  print(item_list)
	  item_code <- readline('Please insert item_code: ')
	  item_args <- item_list[item_list$item_code==item_code,]
	  
	} else {
	  
	  item_args <- item_list[item_list$item_code==item_code,]
	  
	}
	
	if (missing(cycle)) 
	  cycle <- item_args$cycle
	
	if (missing(start_time)) 
	  start_time <- item_args$start_time
	
	if (missing(end_time)) 
	  end_time <- item_args$end_time
	
	if (missing(count)) 
	  count <- item_args$data_cnt
	
	if (format == "xml") {

		url <- URLencode(sprintf("http://ecos.bok.or.kr/api/StatisticSearch/%s/%s/%s/1/%s/%s/%s/%s/%s/%s/?/?/", api_key, format, lang, count, stat_code, cycle, start_time, end_time, item_code))
		html <- getURLContent(url)
		xml_all <- xmlParse(html)

		if (is.null(unlist(xpathApply(xml_all, "//RESULT")))) {

			xml_cnt <- xpathApply(xml_all, "//list_total_count")[[1]]
			cnt <- as.integer(xmlToList(xml_cnt))
			xml_row <- xpathApply(xml_all, "//row") 
			df <- xmlToDataFrame(xml_row, stringsAsFactors = FALSE)
			df[] <- lapply(df, trimws)
			names(df) <- tolower(names(df))
			df$time <- as.Date(paste0(as.character(df$time), "01"), format = "%Y%m%d")
			df$data_value <- as.numeric(df$data_value)
			attr(df, "list_total_count") <- cnt 

		} else {

			code <- xmlToList(xpathApply(xml_all, "//CODE")[[1]])
			msg  <- xmlToList(xpathApply(xml_all, "//MESSAGE")[[1]])

			stop(paste0(code, "\n ", msg))

		}

	} else if (format == "json") {

		url <- URLencode(sprintf("http://ecos.bok.or.kr/api/StatisticSearch/%s/%s/%s/1/%s/%s/%s/%s/%s/%s/?/?/", api_key, format, lang, count, stat_code, cycle, start_time, end_time, item_code))
		html <- getURLContent(url)
		json_all <- fromJSON(html)

		if (is.null(json_all$RESULT)) {

			cnt  <- json_all$StatisticSearch$list_total_count
			df   <- json_all$StatisticSearch$row
			df[] <- lapply(df, trimws)
			names(df) <- tolower(names(df))
			df$time <- as.Date(paste0(df$time, "01"), format = "%Y%m%d")
			df$data_value <- as.numeric(df$data_value)
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
