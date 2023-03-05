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
#' @format A data frame with 3444 rows and 4 variable: 
#' \describe{
#'   \item{region       }{Rejon. Escaped by stringi::stri_escape_unicode().}
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
