## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(clidatajp)
library(tidyverse)

## ----station_links------------------------------------------------------------
  # existing data
data(station_links)
station_links %>%
  dplyr::mutate("station" := stringi::stri_unescape_unicode(station)) %>%
  print() %>%
  `$`("station") %>%
  clean_station() %>%
  dplyr::bind_cols(station_links["url"])

  # Download new data
  # If you want links for all countries and all sations, remove head().
url <- "https://www.data.jma.go.jp/gmd/cpd/monitor/nrmlist/"
res <- gracefully_fail(url)
if(!is.null(res)){
  area_links <- download_area_links()
  station_links <- NULL
  area_links <- head(area_links)  # for test
  for(i in seq_along(area_links)){
      print(stringr::str_c("area: ", i, " / ", length(area_links)))
      country_links <- download_links(area_links[i])
      country_links <- head(country_links)  # for test
      for(j in seq_along(country_links)){
          print(stringr::str_c("    country: ", j, " / ", length(country_links)))
          station_links <- c(station_links, download_links(country_links[j]))
      }
  }
  station_links <- tibble::tibble(url = station_links)
  station_links
}

## ----climate_data-------------------------------------------------------------
  # existing data
data(japan_climate)
japan_climate %>%
  dplyr::mutate_if(is.character, stringi::stri_unescape_unicode)

data(world_climate)
world_climate %>%
  dplyr::mutate_if(is.character, stringi::stri_unescape_unicode)

  # Download new data
  # If you want links for all countries and all sations, remove head().
url <- "https://www.data.jma.go.jp/gmd/cpd/monitor/nrmlist/"
res <- gracefully_fail(url)
if(!is.null(res)){
  station_links <-
    station_links %>%
    head() %>%
    `$`("url")
  climate <- list()
  for(i in seq_along(station_links)){
    print(stringr::str_c(i, " / ", length(station_links)))
    climate[[i]] <- download_climate(station_links[i])
  }
  world_climate <- dplyr::bind_rows(climate)
  world_climate
}

