## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(clidatajp)
library(magrittr)
library(dplyr)
library(tibble)
library(ggplot2)
library(stringi)

## ----station_links, eval = FALSE----------------------------------------------
#    # existing data
#  data(station_links)
#  station_links %>%
#    dplyr::mutate("station" := stringi::stri_unescape_unicode(station)) %>%
#    print() %>%
#    `$`("station") %>%
#    clean_station() %>%
#    dplyr::bind_cols(station_links["url"])
#  
#    # Download new data
#    # If you want links for all countries and all sations, remove head().
#  url <- "https://www.data.jma.go.jp/gmd/cpd/monitor/nrmlist/"
#  res <- gracefully_fail(url)
#  if(!is.null(res)){
#    area_links <- download_area_links()
#    station_links <- NULL
#    area_links <- head(area_links)  # for test
#    for(i in seq_along(area_links)){
#        print(stringr::str_c("area: ", i, " / ", length(area_links)))
#        country_links <- download_links(area_links[i])
#        country_links <- head(country_links)  # for test
#        for(j in seq_along(country_links)){
#            print(stringr::str_c("    country: ", j, " / ", length(country_links)))
#            station_links <- c(station_links, download_links(country_links[j]))
#        }
#    }
#    station_links <- tibble::tibble(url = station_links)
#    station_links
#  }

## ----climate_data, eval = FALSE-----------------------------------------------
#    # existing data
#  data(climate_jp)
#  climate_jp %>%
#    dplyr::mutate_if(is.character, stringi::stri_unescape_unicode)
#  
#  data(climate_world)
#  climate_world %>%
#    dplyr::mutate_if(is.character, stringi::stri_unescape_unicode)
#  
#    # Download new data
#    # If you want links for all countries and all sations, remove head().
#  url <- "https://www.data.jma.go.jp/gmd/cpd/monitor/nrmlist/"
#  res <- gracefully_fail(url)
#  if(!is.null(res)){
#    station_links <-
#      station_links %>%
#      head() %>%
#      `$`("url")
#    climate <- list()
#    for(i in seq_along(station_links)){
#      print(stringr::str_c(i, " / ", length(station_links)))
#      climate[[i]] <- download_climate(station_links[i])
#    }
#    world_climate <- dplyr::bind_rows(climate)
#    world_climate
#  }

## ----clean_data---------------------------------------------------------------
data(climate_world)
data(climate_jp)
climate <- 
  dplyr::bind_rows(climate_world, climate_jp) %>%
  dplyr::mutate_if(is.character, stringi::stri_unescape_unicode)  %>%
  dplyr::group_by(country, station) %>%
  dplyr::filter(sum(is.na(temperature), is.na(precipitation)) == 0) %>%
  dplyr::filter(period != "1991-2020" | is.na(period))

climate <- 
  climate %>%
  dplyr::summarise(temp = mean(as.numeric(temperature)), prec = sum(as.numeric(precipitation))) %>%
  dplyr::left_join(dplyr::distinct(dplyr::select(climate, station:altitude))) %>%
  dplyr::left_join(tibble::tibble(NS = c("S", "N"), ns = c(-1, 1))) %>%
  dplyr::left_join(tibble::tibble(WE = c("W", "E"), we = c(-1, 1))) %>%
  dplyr::group_by(station) %>%
  dplyr::mutate(lat = latitude * ns, lon = longitude * we)

## ----temperature--------------------------------------------------------------
climate %>%
  ggplot2::ggplot(aes(lon, lat, colour = temp)) +
    scale_colour_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 15) + 
    geom_point() + 
    coord_fixed() + 
    theme_bw() + 
    theme(legend.key.size = unit(0.3, 'cm'))
    # ggsave("temperature.png")

## ----precipitation------------------------------------------------------------
climate %>%
  dplyr::filter(prec < 5000) %>%
  ggplot2::ggplot(aes(lon, lat, colour = prec)) +
    scale_colour_gradient2(low = "yellow", mid = "gray", high = "blue", midpoint = 1500) + 
    geom_point() + 
    coord_fixed() + 
    theme_bw() + 
    theme(legend.key.size = unit(0.3, 'cm'))
  # ggsave("precipitation.png")

## ----except_japan-------------------------------------------------------------
japan <- stringi::stri_unescape_unicode("\\u65e5\\u672c")
climate %>%
  dplyr::filter(country != japan) %>%
  ggplot2::ggplot(aes(temp, prec)) + 
  geom_point() + 
  theme_bw() + 
  theme(legend.position="none")
  # ggsave("climate_nojp.png")

## ----all_data-----------------------------------------------------------------
climate %>%
  ggplot2::ggplot(aes(temp, prec)) + 
    geom_point() + 
    theme_bw()
  # ggsave("climate_all.png")

## ----compare_japan------------------------------------------------------------
climate %>%
  dplyr::mutate(jp = (country == japan)) %>%
  ggplot2::ggplot(aes(temp, prec, colour = jp)) + 
    geom_point() + 
    theme_bw() +
    theme(legend.position="none")
  # ggsave("climate_compare_jp.png")

