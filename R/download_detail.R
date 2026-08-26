#' Build urls of detail climate data in Japan
#'
#' Japan Meteorological Agency ('JMA') provides detail climate data
#' (yearly, monthly, daily, hourly and 10 minutes values) for each station.
#' detail_url() builds urls of the data,
#' and download_detail() downloads the data of the url.
#' You can see web page as below.
#' https://www.data.jma.go.jp/obd/stats/etrn/index.php
#'
#' A station is specified by a pair of prec_no (area) and block_no (station).
#' Use download_prec_no() and download_block_no() to get the numbers.
#' Note that block_no of "kansho" (weather station) has 5 digits (ex. "47759")
#' and that of "AMeDAS" has 4 digits (ex. "0588").
#' Suffix of item ("_s" or "_a") is set to match the type of block_no.
#'
#' item can be a short name in the following list,
#' or a name of php file without extension (ex. "daily_s1").
#' \describe{
#'   \item{annually   }{Yearly values.}
#'   \item{monthly    }{Monthly values in a year (year is required).}
#'   \item{monthly_all}{Monthly values of all years ("kansho" only).}
#'   \item{daily      }{Daily values in a month (year and month are required).}
#'   \item{hourly     }{Hourly values in a day (year, month and day are required).}
#'   \item{10min      }{10 minutes values in a day (year, month and day are required).}
#'   \item{nml_ym     }{Normals of each month.}
#'   \item{nml_daily  }{Normals of each day in a month (month is required).}
#'   \item{nml_season }{Normals of first and last day of frost, snow and ice ("kansho" only).}
#' }
#'
#' @name detail_url
#' @param prec_no  A string or numeric of area no.
#' @param block_no A string or numeric of station no.
#'                 Keep leading zero of 'AMeDAS' as a string (ex. "0588").
#' @param item     A string to specify data type. See details.
#' @param year     A numeric or string of year.
#' @param month    A numeric or string of month.
#' @param day      A numeric or string of day.
#' @return         A string vector of urls.
#' @examples
#' detail_url(61, 47759, "daily", 2023, 6)
#' detail_url(61, "0588", "hourly", 2023, 6, 20)
#' detail_url(61, 47759, "daily", 2023, 1:3)
#'
#' @export
detail_url <- function(prec_no, block_no, item = "daily",
                       year = NULL, month = NULL, day = NULL){
  base <- "https://www.data.jma.go.jp/obd/stats/etrn/view/"
  php  <- detail_item(item, block_no)
  query <-
    stringr::str_c("?prec_no=" , prec_no,
                   "&block_no=", block_no,
                   "&year="    , null_to_empty(year),
                   "&month="   , null_to_empty(month),
                   "&day="     , null_to_empty(day),
                   "&view=")
  return(stringr::str_c(base, php, ".php", query))
}

#' @rdname detail_url
#' @export
detail_item <- function(item, block_no){
  items <-
    c("annually"    = "annually_s|annually_a",
      "monthly"     = "monthly_s1|monthly_a1",
      "monthly_all" = "monthly_s3|monthly_s3",
      "daily"       = "daily_s1|daily_a1",
      "hourly"      = "hourly_s1|hourly_a1",
      "10min"       = "10min_s1|10min_a1",
      "nml_ym"      = "nml_sfc_ym|nml_amd_ym",
      "nml_daily"   = "nml_sfc_d|nml_amd_d",
      "nml_season"  = "nml_sfc_season|nml_sfc_season")
    # item is treated as a name of php file when not in the list
  if(!item %in% names(items)) return(item)
  php    <- stringr::str_split(items[[item]], "\\|", simplify = TRUE)
  kansho <- stringr::str_length(as.character(block_no)) >= 5
  return(ifelse(kansho, php[1], php[2]))
}

null_to_empty <- function(x){
  if(is.null(x)) return("")
  return(x)
}

#' Download detail climate data in Japan
#'
#' For polite scraping, 5 sec interval is set in download_detail().
#' Please do not download too many data at once.
#' Use detail_url() to build an url of the data.
#'
#' @name download_detail
#' @param url         A string to specify target html.
#' @param as_numeric  A logical. If TRUE, columns of numbers are
#'                    converted into numeric. Marks of 'JMA'
#'                    for missing values are treated as NA.
#' @return  A tibble including detail climate data, or NULL when failed.
#' @examples
#' \donttest{
#' url <- detail_url(61, 47759, "daily", 2023, 6)
#' download_detail(url)
#' }
#' @export
download_detail <- function(url, as_numeric = TRUE){
  sleep()
  html <- gracefully_fail(url)
  if(is.null(html)) return(NULL)
  html  <- rvest::read_html(html)
  table <- detail_table(html)
  if(is.null(table)){
    message("No data table is found: ", url)
    return(invisible(NULL))
  }
  rows <- rvest::html_elements(table, "tr")
  is_header <-
    vapply(rows,
           function(x){
             length(rvest::html_elements(x, "td")) == 0 &&
             length(rvest::html_elements(x, "th")) >  0
           },
           logical(1))
  contents <-
    table %>%
    rvest::html_table(header = FALSE) %>%
    as.data.frame(stringsAsFactors = FALSE)
  if(nrow(contents) != length(rows)){
    message("Unexpected table structure: ", url)
    return(invisible(NULL))
  }
  header <- contents[ is_header, , drop = FALSE]
  values <- contents[!is_header, , drop = FALSE]
  if(nrow(values) == 0){
    message("No data is found: ", url)
    return(invisible(NULL))
  }
  colnames(values) <- detail_colnames(header)
  values <- lapply(values, clean_detail_value)
  if(as_numeric) values <- lapply(values, to_numeric_when_all)
  detail <-
    values %>%
    tibble::as_tibble(.name_repair = "minimal") %>%
    dplyr::mutate("url" := url)
  return(detail)
}

detail_table <- function(html){
  table <- rvest::html_element(html, "#tablefix1")
  if(!inherits(table, "xml_missing")) return(table)
    # some pages (ex. nml_sfc_season) have no table of id "tablefix1"
  tables <- rvest::html_elements(html, "table")
  if(length(tables) == 0) return(NULL)
  n_row <-
    vapply(tables,
           function(x) length(rvest::html_elements(x, "tr")), integer(1))
  return(tables[[which.max(n_row)]])
}

#' Helper functions for download_detail()
#'
#' detail_colnames() joins header rows into column names,
#' clean_detail_value() removes marks of 'JMA' from values,
#' and to_numeric_when_all() converts a vector into numeric
#' only when all values can be converted.
#'
#' @name detail_colnames
#' @param header  A dataframe of header rows.
#' @param x       A string vector of values.
#' @return  detail_colnames(): a string vector of column names.
#'          clean_detail_value(): a string vector.
#'          to_numeric_when_all(): a numeric vector or a string vector.
#' @examples
#' header <- data.frame(x = c("temperature", "temperature"),
#'                      y = c("temperature", "mean"))
#' detail_colnames(header)
#' clean_detail_value(c("21.6", "0.0 )", "--", "///"))
#' to_numeric_when_all(c("21.6", "0.0", NA))
#' to_numeric_when_all(c("21.6", "north"))
#'
#' @export
detail_colnames <- function(header){
  if(nrow(header) == 0) return(colnames(header))
  cols <-
    vapply(header,
           function(x){
             x <- unique(stringr::str_squish(as.character(x)))
             x <- x[!is.na(x) & x != ""]
             stringr::str_c(x, collapse = "_")
           },
           character(1))
  cols <- ifelse(cols == "", stringr::str_c("V", seq_along(cols)), cols)
  return(make.unique(unname(cols)))
}

#' @rdname detail_colnames
#' @export
clean_detail_value <- function(x){
    # ")", "]" and "#" show quality of the value,
    # "--", "///" and multiplication sign show no value
  no_value <- c("--", "///", stringi::stri_unescape_unicode("\\u00d7"), "")
  cleaned <-
    x %>%
    as.character() %>%
    stringr::str_remove_all("[\\)\\]#]") %>%
    stringr::str_squish()
  return(ifelse(cleaned %in% no_value, NA_character_, cleaned))
}

#' @rdname detail_colnames
#' @export
to_numeric_when_all <- function(x){
  if(is.numeric(x)) return(x)
  num <- as_numeric_without_warnings(x)
  if(any(is.na(num) & !is.na(x))) return(x)
  if(all(is.na(num))) return(x)
  return(num)
}

#' Download prec_no and block_no of stations in Japan
#'
#' A station of detail climate data is specified by a pair of
#' prec_no (area) and block_no (station).
#' download_prec_no() downloads all prec_no,
#' and download_block_no() downloads block_no of the area.
#' For polite scraping, 5 sec interval is set in both functions.
#' You can see web page as below.
#' https://www.data.jma.go.jp/obd/stats/etrn/index.php
#'
#' @name download_prec_no
#' @param prec_no  A string or numeric of area no.
#' @return  download_prec_no(): a tibble of prec_no and area.
#'          download_block_no(): a tibble of prec_no, block_no, station and type.
#'          NULL when failed.
#' @examples
#' \donttest{
#' prec_no <- download_prec_no()
#' prec_no
#' download_block_no(61)
#' }
#' @export
download_prec_no <- function(){
  url <- "https://www.data.jma.go.jp/obd/stats/etrn/select/prefecture00.php"
  areas <- detail_areas(url)
  if(is.null(areas)) return(invisible(NULL))
  prec_no <-
    areas %>%
    rvest::html_attr("href") %>%
    stringr::str_match("prec_no=([0-9]+)") %>%
    `[`( , 2)
  prec <-
    tibble::tibble(prec_no = prec_no,
                   area    = rvest::html_attr(areas, "alt")) %>%
    stats::na.omit() %>%
    dplyr::distinct()
  return(prec)
}

#' @rdname download_prec_no
#' @export
download_block_no <- function(prec_no){
  url <-
    stringr::str_c(
      "https://www.data.jma.go.jp/obd/stats/etrn/select/prefecture.php",
      "?prec_no=", prec_no, "&block_no=&year=&month=&day=&view=")
  areas <- detail_areas(url)
  if(is.null(areas)) return(invisible(NULL))
  block_no <-
    areas %>%
    rvest::html_attr("href") %>%
    stringr::str_match("block_no=([0-9a-zA-Z]+)") %>%
    `[`( , 2)
  block <-
    tibble::tibble(prec_no  = as.character(prec_no),
                   block_no = block_no,
                   station  = rvest::html_attr(areas, "alt")) %>%
    stats::na.omit() %>%
    dplyr::distinct() %>%
    dplyr::mutate("type" :=
      ifelse(stringr::str_length(.data[["block_no"]]) >= 5, "kansho", "amedas"))
  return(block)
}

detail_areas <- function(url){
  sleep()
  html <- gracefully_fail(url)
  if(is.null(html)) return(NULL)
  areas <-
    html %>%
    rvest::read_html() %>%
    rvest::html_elements("area")
  if(length(areas) == 0){
    message("No station is found: ", url)
    return(NULL)
  }
  return(areas)
}
