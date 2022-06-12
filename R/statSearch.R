#' statSearch Function
#'
#' @title Search Statistics
#' @param api_key,format,lang,count,stat_code,cycle,start_time,end_time,item_code1,item_code2,item_code3,item_code4 input parameters
#' @keywords ecos, statSearch
#' @export
#' @examples
#' statSearch(api_key = your_api_key, lang = "en", stat_code = "102Y004", item_code1 = "ABA1", 
#' cycle = "M", start_time = "196001", end_time = "201812", count = 1000)
#' statSearch(api_key = your_api_key, lang = "kr", stat_code = "102Y004", item_code1 = "ABA1", 
#' cycle = "M", start_time = "196001", end_time = "201812", count = 1000)
#' 
statSearch <- function(api_key, format = c("xml", "json"), lang = c("kr", "en"), stat_code, item_code1, item_code2, item_code3, item_code4, cycle, start_time, end_time, count) {
	if (missing(api_key)) 
	  stop("Please create your api key from website 'https://ecos.bok.or.kr/api/#/AuthKeyApply'")
	if (missing(stat_code)) { 
	  options("max.print" = .Machine$integer.max)
	  showStatTableList(api_key)
	  options("max.print" = 1e3)
	  stat_code <- readline("Please insert stat_code: ")
	  item_list <- statItemList(api_key = api_key, stat_code = stat_code)
	} else {
	  item_list <- statItemList(api_key = api_key, stat_code = stat_code)
	}
	if (missing(item_code1)) {
	  print(item_list)
	  item_code1 <- readline("Please insert item_code1: ")
	  item_args <- item_list[item_list$item_code == item_code1,]
	} else {
	  item_args <- item_list[item_list$item_code == item_code1,]
	}
  if (missing(item_code2))
    item_code2 <- "?"
  if (missing(item_code3))
    item_code3 <- "?"
  if (missing(item_code4))
    item_code4 <- "?"
	if (missing(cycle)) 
	  cycle <- item_args$cycle
	if (missing(start_time)) 
	  start_time <- item_args$start_time
	if (missing(end_time)) 
	  end_time <- item_args$end_time
	if (missing(count)) 
	  count <- item_args$data_cnt
	if (format[[1]] == "xml") {
		url <- URLencode(sprintf("http://ecos.bok.or.kr/api/StatisticSearch/%s/%s/%s/1/%s/%s/%s/%s/%s/%s/%s/%s/%s", 
		                         api_key, format[[1]], lang[[1]], count, stat_code, cycle, start_time, end_time, item_code1, item_code2, item_code3, item_code4))
		html <- GET(url)
		content <- rawToChar(html$content)
		xml_all <- xmlParse(content)
		if (is.null(unlist(xpathApply(xml_all, "//RESULT")))) {
			xml_cnt <- xpathApply(xml_all, "//list_total_count")[[1]]
			cnt <- as.integer(xmlToList(xml_cnt)[[1]])
			xml_row <- xpathApply(xml_all, "//row") 
			df <- xmlToDataFrame(xml_row, stringsAsFactors = FALSE)
			df[] <- lapply(df, trimws)
			names(df) <- tolower(names(df))
			df <- df[order(df$time),]
			df$data_value <- as.numeric(df$data_value)
			attr(df, "list_total_count") <- cnt 
		} else {
			code <- xmlToList(xpathApply(xml_all, "//CODE")[[1]])
			msg  <- xmlToList(xpathApply(xml_all, "//MESSAGE")[[1]])
			stop(paste0(code, "\n ", msg))
		}
	} else if (format[[1]] == "json") {
		url <- URLencode(sprintf("http://ecos.bok.or.kr/api/StatisticSearch/%s/%s/%s/1/%s/%s/%s/%s/%s/%s/%s/%s/%s", 
		                         api_key, format[[1]], lang[[1]], count, stat_code, cycle, start_time, end_time, item_code1, item_code2, item_code3, item_code4))
		html <- GET(url)
		content <- rawToChar(html$content)
		json_all <- fromJSON(content)
		if (is.null(json_all$RESULT)) {
			cnt  <- json_all$StatisticSearch$list_total_count
			df   <- json_all$StatisticSearch$row
			df[] <- lapply(df, trimws)
			names(df) <- tolower(names(df))
			df$data_value <- as.numeric(df$data_value)
			df <- df[order(df$time),]
			attr(df, "list_total_count") <- cnt 
		} else {
			code <- json_all$RESULT$CODE	
			msg  <- json_all$RESULT$MESSAGE	
			stop(paste0(code, "\n ", msg))
		}
	} else {
		stop("This file format is not supported.")
	}
	df <- orderStatSearchColumns(df)
	return(df); gc()
}
