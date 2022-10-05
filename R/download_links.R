#' download links for areas, countries and stations
#' 
#' For polite scraping, 5 sec interval is set in download_links(), 
#' it takes about 15 minutes to get all station links. 
#' Please use existing links by "data(station_links)", 
#' if you do not need to renew links. 
#' You can see web page as below. 
#' https://www.data.jma.go.jp/gmd/cpd/monitor/nrmlist/
#' 
#' @name download_links
#' @param url  A String to specify target html.
#' @return     A string vector of url links.
#' @examples
#' # If you want links for all countries and all sations, remove head().
#' # The codes take over 5 sec because of poliste scraping.
#' \donttest{
#' library(tidyverse)
#' area_links <- download_area_links()
#' station_links <- NULL
#' area_links <- head(area_links, 1)  # for test
#' for(i in seq_along(area_links)){
#'     print(stringr::str_c("area: ", i, " / ", length(area_links)))
#'     country_links <- download_links(area_links[i])
#'     country_links <- head(country_links, 1)  # for test
#'     for(j in seq_along(country_links)){
#'         print(stringr::str_c("    country: ", j, " / ", length(country_links)))
#'         station_links <- c(station_links, download_links(country_links[j]))
#'     }
#' }
#' station_links <- tibble::tibble(url = station_links)
#' station_links
#' }
#' @export
download_area_links <- function(
  url = "https://www.data.jma.go.jp/gmd/cpd/monitor/nrmlist/"){
  href <- 
    url %>%
    rvest::read_html() %>%
    rvest::html_elements("#main") %>%
    rvest::html_elements("area") %>%
    rvest::html_attr("href") %>%
    stringr::str_sub(start = 3)
  return(stringr::str_c(url, href))
}

#' @rdname download_links
#' @export
download_links <- function(url){
  sleep()
  href <- 
    url %>%
    rvest::read_html() %>%
    rvest::html_elements("#main") %>%
    rvest::html_elements("a") %>% 
    rvest::html_attr("href")
  url <- 
    "https://www.data.jma.go.jp/gmd/cpd/monitor/nrmlist/" %>%
    stringr::str_c(href)
  return(url)
}
