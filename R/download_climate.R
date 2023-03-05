#' Download climate data of the world
#' 
#' For polite scraping, 5 sec interval is set in download_climate(), 
#' it takes over 5 hours to get climate data of all stations. 
#' Please use existing links by "data(climate_world)", 
#' if you do not need to renew climate data. 
#' You can see web page as below. 
#' https://www.data.jma.go.jp/gmd/cpd/monitor/nrmlist/
#' 
#' @name download_climate
#' @param url      A String to specify target html.
#' @return  A tibble including climate and station information, or NULL when failed.
#' @examples
#' # If you want all climate data, remove head().
#' # The codes take > 5 sec because of poliste scraping.
#' \donttest{
#' library(magrittr)
#' library(stringi)
#' library(dplyr)
#' data(station_links)
#' station_links <-
#'   station_links %>%
#'   dplyr::mutate_all(stringi::stri_unescape_unicode) %>%
#'   head(3) %T>%
#'   { 
#'      continent <<- `$`(., "continent") 
#'      no        <<- `$`(., "no") 
#'   } %>%
#'   `$`("url")
#' 
#' climate <- list()
#' for(i in seq_along(station_links)){
#'   print(stringr::str_c(i, " / ", length(station_links)))
#'   climate[[i]] <- download_climate(station_links[i])
#' }
#'   # run only when download_climate() successed
#' if(sum(is.null(climate[[1]]), 
#'        is.null(climate[[2]]), 
#'        is.null(climate[[3]])) == 0){
#'   month_per_year <- 12
#'   climate_world <- 
#'     dplyr::bind_rows(climate) %>%
#'     dplyr::bind_cols(
#'       tibble::tibble(continent = rep(continent, month_per_year))) %>%
#'     dplyr::bind_cols(
#'       tibble::tibble(no        = rep(no,        month_per_year))) %>%
#'     dplyr::relocate(no, continent, country, station)
#'   climate_world
#' }
#' }
#' @export
download_climate <- function(url){
  sleep()
  html <- gracefully_fail(url)
  if(is.null(html)) return(NULL)
  html <- rvest::read_html(html)
  station <- 
    html %>%
    rvest::html_elements("#main") %>%
    rvest::html_elements("h3") %>%
    rvest::html_text() %>%
    clean_station()
  climate <- 
    html %>%
    rvest::html_elements("#main") %>%
    rvest::html_table() %>%
    `[[`(1) %>%
    magrittr::set_colnames(c("month" , "temperature", "precipitation")) %>%
    dplyr::mutate_all(as_numeric_without_warnings) %>%
    utils::tail(-1)
  combined <- 
    dplyr::bind_cols(station, climate) %>%
    dplyr::mutate(url = url)
  return(combined)
}

#' Clean up station information
#' 
#' Helper function for download_climate().
#' @param station  A String of station information.
#' @return  A tibble including station information.
#' @examples
#' data(station_links)
#' station_links %>%
#'   head(1) %>%
#'   `$`("station") %>%
#'   stringi::stri_unescape_unicode() %>%
#'   clean_station()
#' 
#' @export
clean_station <- function(station){
  cols <- 
      c("station", "country", "latitude", "NS", "longitude", "WE", "altitude")
  sep <- 
    "\\u5ea6\\uff1a|\\u00b0" %>%
    stringi::stri_unescape_unicode()
  sep <- stringr::str_c("\\s+.", sep)
  cleaned_station <- 
    station %>%
    stringr::str_replace_all("\\s+"           , " "          ) %>%
    stringr::str_replace_all("- \\(m\\)"      , "NA"         ) %>%
    stringr::str_replace_all("-(\\d+) \\(m\\)", "minus\\1"   ) %>%
    stringr::str_replace_all(" \\(m\\)"       , ""           ) %>%
    stringr::str_replace_all("(.+)-(.+)-(.+)" , "\\1_\\2:\\3") %>%
    stringr::str_replace_all(" - "            , ":"          ) %>%
    stringr::str_replace_all(sep              , ":"          ) %>%
    stringr::str_replace_all("minus"          , "-"          ) %>%
    tibble::as_tibble() %>%
    tidyr::separate(.data[["value"]], into = cols, sep = ":") %>%
      # avoid duplication (two or more stations have identical name)
    dplyr::mutate("station" := 
                  stringr::str_c(.data[["station"]], "_", .data[["country"]]))
  return(cleaned_station)
}
