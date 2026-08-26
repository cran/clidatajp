#' Climate data in Japan
#' 
#' Climate data downloaded from Japan Meteorological Agency web pages.
#' URLs of each station are listed in data(station_links).
#' https://www.data.jma.go.jp/gmd/cpd/monitor/nrmlist/
#' @format A data frame with 3768 (157 stations * 12 months * 2 periods) rows 
#' and 14 variable: 
#' \describe{
#'   \item{no           }{Station no}
#'   \item{month        }{Month}
#'   \item{period       }{Period of observations}
#'   \item{temperature  }{Mean temperature}
#'   \item{precipitation}{Mean precipitation}
#'   \item{snowfall     }{Mean snowfall}
#'   \item{insolation   }{Mean insolation}
#'   \item{station      }{Station name. To avoid duplication, including country name after station name. Can split by "_". Escaped by stringi::stri_escape_unicode().}
#'   \item{country      }{Country name. Escaped by stringi::stri_escape_unicode().}
#'   \item{latitude     }{Latitude. (degree)}
#'   \item{NS           }{North or South.}
#'   \item{longitude    }{Longitude. (degree)}
#'   \item{WE           }{West or East.}
#'   \item{altitude     }{Altitude (m)}
#' }
#' @examples
#' library(magrittr)
#' library(stringi)
#' library(dplyr)
#' data(japan_climate)
#' japan_climate %>%
#'   dplyr::mutate_all(stringi::stri_unescape_unicode)
"climate_jp"

#' Renamed to climate_jp and will be removed in the future version.
#' @rdname climate_jp
"japan_climate"

#' Climate data in the world
#' 
#' Climate data downloaded from Japan Meteorological Agency web pages.
#' URLs of each station are listed in data(station_links).
#' https://www.data.jma.go.jp/gmd/cpd/monitor/nrmlist/
#' 
#' @format A data frame with 41328 (3444 stations * 12 months) rows 
#' and 12 variable: 
#' \describe{
#'   \item{no           }{Station no}
#'   \item{continent    }{Continent. Escaped by stringi::stri_escape_unicode().}
#'   \item{country      }{Country name. Escaped by stringi::stri_escape_unicode().}
#'   \item{station      }{Station name. To avoid duplication, including country name after station name. Can split by "_". Escaped by stringi::stri_escape_unicode().}
#'   \item{month        }{Month}
#'   \item{temperature  }{Mean temperature}
#'   \item{precipitation}{Mean precipitation}
#'   \item{latitude     }{Latitude. (degree)}
#'   \item{NS           }{North or South.}
#'   \item{longitude    }{Longitude. (degree)}
#'   \item{WE           }{West or East.}
#'   \item{altitude     }{Altitude (m)}
#' }
#' @examples
#' library(magrittr)
#' library(stringi)
#' library(dplyr)
#' data(world_climate)
#' world_climate %>%
#'   dplyr::mutate_all(stringi::stri_unescape_unicode)
"climate_world"

#' Renamed to climate_world and will be removed in the future version.
#' @rdname climate_world
"world_climate"

#' Station name and its URL
#'
#' @format A data frame with  3444 rows and 4 variable: 
#' \describe{
#'   \item{no           }{Station no}
#'   \item{station      }{Station information including no, month, temperature, precipitation, station, country, latitude, NS, longitude, WE, altitude. The information is NOT cleaned Row information downloaded from each URL. Escaped by stringi::stri_escape_unicode().}
#'   \item{url         }{URL of station.}
#'   \item{continent    }{Continent. Escaped by stringi::stri_escape_unicode().}
#' }
#' @examples
#' library(magrittr)
#' library(stringi)
#' library(dplyr)
#' data(station_links)
#' station_links %>%
#'   dplyr::mutate_all(stringi::stri_unescape_unicode)
"station_links"

#' Climate stations in Japan
#' 
#' @format A data frame with 157 rows and 11 variable: 
#' \describe{
#'   \item{region       }{Region. Escaped by stringi::stri_escape_unicode().}
#'   \item{pref         }{Prefecture. Escaped by stringi::stri_escape_unicode()}
#'   \item{no           }{Station no.}
#'   \item{station      }{Station name. To avoid duplication, including country name after station name. Can split by "_". Escaped by stringi::stri_escape_unicode().}
#'   \item{altitude     }{Altitude. (m)}
#'   \item{latitude     }{Latitude. (degree)}
#'   \item{longitude    }{Longitude. (degree)}
#'   \item{NS           }{North or South.}
#'   \item{WE           }{West or East.}
#'   \item{yomi         }{Pronunciation in Japanese. Escaped by stringi::stri_escape_unicode()}
#'   \item{city         }{City name. Escaped by stringi::stri_escape_unicode().}
#' }
#' @examples
#' library(magrittr)
#' library(stringi)
#' library(dplyr)
#' data(station_jp)
#' station_jp %>%
#'   dplyr::mutate_all(stringi::stri_unescape_unicode)
"station_jp"

#' Climate stations of the world
#' 
#' @format A data frame with 3444 rows and 9 variable: 
#' \describe{
#'   \item{no           }{Station no}
#'   \item{station      }{Station name. To avoid duplication, including country name after station name. Can split by "_". Escaped by stringi::stri_escape_unicode().}
#'   \item{continent    }{Continent. Escaped by stringi::stri_escape_unicode().}
#'   \item{country      }{Country name. Escaped by stringi::stri_escape_unicode().}
#'   \item{altitude     }{Altitude (m)}
#'   \item{latitude     }{Latitude (degree)}
#'   \item{NS           }{North or South.}
#'   \item{longitude    }{Longitude (degree)}
#'   \item{WE           }{West or East}
#' }
#' @examples
#' library(magrittr)
#' library(stringi)
#' library(dplyr)
#' data(station_world)
#' station_world %>%
#'   dplyr::mutate_all(stringi::stri_unescape_unicode)
"station_world"

#' Climate normals of all stations in Japan
#'
#' Monthly normals downloaded from Japan Meteorological Agency web pages
#' for all stations in data(station_jp_full).
#' Values are raw strings of the web pages,
#' because they include marks of 'JMA'
#' ("@@", ")", "]" and "#" show quality of the value,
#' "///" and "---" show no value).
#' Use as_numeric_without_warnings() after removing the marks,
#' or use download_detail(), which cleans the marks.
#' Stations of "AMeDAS" have no value in 11 of the columns,
#' because only 10 items are observed.
#' https://www.data.jma.go.jp/obd/stats/etrn/index.php
#'
#' @format A data frame with 21697 rows (1669 stations * 13 rows)
#' and 21 variables. All variables are character.
#' \describe{
#'   \item{month                 }{Month (1 to 12) or yearly value. Escaped by stringi::stri_escape_unicode().}
#'   \item{air_pressure_land     }{Mean air pressure on land (hPa)}
#'   \item{air_pressure_sea      }{Mean air pressure on sea level (hPa)}
#'   \item{precipitation         }{Mean precipitation (mm)}
#'   \item{temperature           }{Mean temperature (degree Celsius)}
#'   \item{temperature_max       }{Mean of daily maximum temperature (degree Celsius)}
#'   \item{temperature_min       }{Mean of daily minimum temperature (degree Celsius)}
#'   \item{steam_pressure        }{Mean steam pressure (hPa)}
#'   \item{moisture              }{Mean relative humidity (percent)}
#'   \item{wind_speed            }{Mean wind speed (m/s)}
#'   \item{wind_direction        }{Most frequent wind direction. Escaped by stringi::stri_escape_unicode().}
#'   \item{sunshine              }{Mean sunshine duration (hour)}
#'   \item{global_solar_radiation}{Mean global solar radiation (MJ/m^2)}
#'   \item{snow_fall             }{Mean snow fall (cm)}
#'   \item{snow_fall_max_per_day }{Mean of daily maximum snow fall (cm)}
#'   \item{snow_deepest          }{Mean of deepest snow (cm)}
#'   \item{cloud_cover           }{Mean cloud cover}
#'   \item{days_of_snow          }{Mean days of snow}
#'   \item{days_of_fog           }{Mean days of fog}
#'   \item{days_of_thunderstorm  }{Mean days of thunderstorm}
#'   \item{station_no            }{Station no. Can be joined to station_no of data(station_jp_full). Escaped by stringi::stri_escape_unicode().}
#' }
#' @examples
#' library(magrittr)
#' library(stringi)
#' library(dplyr)
#' data(climate_jp_full)
#' climate_jp_full %>%
#'   head_3() %>%
#'   dplyr::mutate_if(is.character, stringi::stri_unescape_unicode)
"climate_jp_full"

#' Downloaded tables of climate normals in Japan
#'
#' Raw tables downloaded from Japan Meteorological Agency web pages,
#' before cleaning into data(climate_jp_full).
#' Kept for reproducibility of data-raw/climate_jp_full.R.
#' Each element is a table of a station of data(station_jp_full),
#' and the order of the elements is the same as the rows of the data.
#' Column names are not set, and the first rows are headers of the web page.
#' https://www.data.jma.go.jp/obd/stats/etrn/index.php
#'
#' @format A list of 1673 tibbles.
#' \describe{
#'   \item{21 columns}{157 tables of "kansho" (weather station).}
#'   \item{10 columns}{1512 tables of "AMeDAS".}
#'   \item{NULL      }{4 stations, which have no normals.}
#' }
#' @examples
#' data(mean_cli)
#' length(mean_cli)
#' dim(mean_cli[[1]])
"mean_cli"

#' All stations in Japan
#'
#' Station information downloaded from Japan Meteorological Agency web pages.
#' data(station_jp) includes only 157 stations of climate data,
#' while this data includes all 1673 stations.
#' A pair of prec_no and station_no specifies a station
#' in detail_url() and download_detail().
#' Note that station_no of "kansho" (weather station) has 5 digits (159 stations)
#' and that of "AMeDAS" has 4 digits (1514 stations).
#' https://www.data.jma.go.jp/obd/stats/etrn/index.php
#'
#' @format A data frame with 1673 rows and 16 variables:
#' \describe{
#'   \item{station      }{Station name. Escaped by stringi::stri_escape_unicode().}
#'   \item{prec_no      }{Area no of 'JMA'. Escaped by stringi::stri_escape_unicode().}
#'   \item{station_no   }{Station no of 'JMA', which is used as block_no in detail_url(). Escaped by stringi::stri_escape_unicode().}
#'   \item{yomi         }{Pronunciation in Japanese. Escaped by stringi::stri_escape_unicode().}
#'   \item{altitude     }{Altitude (m)}
#'   \item{precipitation}{Flag of observation (1: observed, 0: not observed). Character, not numeric.}
#'   \item{wind         }{Flag of observation (1: observed, 0: not observed)}
#'   \item{temperature  }{Flag of observation (1: observed, 0: not observed)}
#'   \item{sunshine     }{Flag of observation (1: observed, 0: not observed)}
#'   \item{snow         }{Flag of observation (1: observed, 0: not observed)}
#'   \item{moisture     }{Flag of observation (1: observed, 0: not observed)}
#'   \item{prec         }{Area name of prec_no. Escaped by stringi::stri_escape_unicode().}
#'   \item{latitude     }{Latitude (degree)}
#'   \item{longitude    }{Longitude (degree)}
#'   \item{end_date     }{Date of the end of observation. "9999-99-99" shows that the station is in operation (1287 stations). Escaped by stringi::stri_escape_unicode().}
#'   \item{memo         }{Note of 'JMA'. Escaped by stringi::stri_escape_unicode().}
#' }
#' @examples
#' library(magrittr)
#' library(stringi)
#' library(dplyr)
#' data(station_jp_full)
#' station_jp_full %>%
#'   head_3() %>%
#'   dplyr::mutate_if(is.character, stringi::stri_unescape_unicode)
"station_jp_full"
