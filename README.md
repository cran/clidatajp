
<!-- README.md is generated from README.Rmd. Please edit that file -->

# clidatajp

<!-- badges: start -->
<!-- badges: end -->

The goal of clidatajp is to provide climate data from Japan
Meteorological Agency (‘JMA’). Data was downloaded from ‘JMA’ and
edited. You can also download climate data from ‘JMA’.

## Installation

You can install the development version of clidatajp from
[GitHub](https://github.com/). You can see climate data directly from
‘JMA’ ( <https://www.data.jma.go.jp/gmd/cpd/monitor/nrmlist/> ).

``` r
  # CRAN
install.packages("clidatajp")

  # development
  # install.packages("devtools")
devtools::install_github("matutosi/clidatajp")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(clidatajp)
library(tidyverse)
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
#> ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
#> ✔ tibble  3.1.7     ✔ dplyr   1.0.9
#> ✔ tidyr   1.2.0     ✔ stringr 1.4.0
#> ✔ readr   2.1.2     ✔ forcats 0.5.1
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()

  # show station information and link
data(station_links)
station_links %>%
  dplyr::mutate("station" := stringi::stri_unescape_unicode(station))
#> # A tibble: 3,444 × 3
#>    no    station                                                         url    
#>    <chr> <chr>                                                           <chr>  
#>  1 1     アインセフラ - アルジェリア   緯度：32.77°N   経度：0.60°W   …  https:…
#>  2 2     アドラル - アルジェリア   緯度：27.88°N   経度：0.18°W   高度…  https:…
#>  3 3     アルジェ - アルジェリア   緯度：36.77°N   経度：3.10°E   高度…  https:…
#>  4 4     アンナバ - アルジェリア   緯度：36.83°N   経度：7.82°E   高度…  https:…
#>  5 5     イナメナス - アルジェリア   緯度：28.05°N   経度：9.63°E   高…  https:…
#>  6 6     イリジ - アルジェリア   緯度：26.50°N   経度：8.42°E   高度：5… https:…
#>  7 7     インゲザム - アルジェリア   緯度：19.57°N   経度：5.77°E   高…  https:…
#>  8 8     インサラー - アルジェリア   緯度：27.23°N   経度：2.50°E   高…  https:…
#>  9 9     ウェド - アルジェリア   緯度：33.50°N   経度：6.78°E   高度：6… https:…
#> 10 10    ウームエルブワギー - アルジェリア   緯度：35.87°N   経度：7.12… https:…
#> # … with 3,434 more rows

  # show climate data
data(japan_climate)
japan_climate %>%
  dplyr::mutate_if(is.character, stringi::stri_unescape_unicode)
#> # A tibble: 3,768 × 14
#>       no station month temperature precipitation snowfall insolation country
#>    <dbl> <chr>   <dbl>       <dbl>         <dbl>    <dbl>      <dbl> <chr>  
#>  1 47401 稚内        1        -4.3          84.6      129       40.6 日本   
#>  2 47401 稚内        2        -4.3          60.6      105       74.7 日本   
#>  3 47401 稚内        3        -0.6          55.1       68      138.  日本   
#>  4 47401 稚内        4         4.5          50.3        9      174.  日本   
#>  5 47401 稚内        5         9.1          68.1        0      182.  日本   
#>  6 47401 稚内        6        13            65.8       NA      155.  日本   
#>  7 47401 稚内        7        17.2         101.        NA      143.  日本   
#>  8 47401 稚内        8        19.5         123.        NA      151.  日本   
#>  9 47401 稚内        9        17.2         137.        NA      172.  日本   
#> 10 47401 稚内       10        11.3         130.         1      135.  日本   
#> # … with 3,758 more rows, and 6 more variables: period <chr>, altitude <dbl>,
#> #   latitude <dbl>, longitude <dbl>, NS <chr>, WE <chr>

data(world_climate)
world_climate %>%
  dplyr::mutate_if(is.character, stringi::stri_unescape_unicode)
#> # A tibble: 41,328 × 11
#>       no month temperature precipitation station          country latitude NS   
#>    <dbl> <dbl>       <dbl>         <dbl> <chr>            <chr>      <dbl> <chr>
#>  1     1     1         7.1          14.9 アインセフラ_ア… アルジ…     32.8 N    
#>  2     1     2         9.2          11.2 アインセフラ_ア… アルジ…     32.8 N    
#>  3     1     3        12.9          15.9 アインセフラ_ア… アルジ…     32.8 N    
#>  4     1     4        16.8          16.9 アインセフラ_ア… アルジ…     32.8 N    
#>  5     1     5        21.5          15   アインセフラ_ア… アルジ…     32.8 N    
#>  6     1     6        26.7           6.9 アインセフラ_ア… アルジ…     32.8 N    
#>  7     1     7        31             4.1 アインセフラ_ア… アルジ…     32.8 N    
#>  8     1     8        29.5          13.5 アインセフラ_ア… アルジ…     32.8 N    
#>  9     1     9        24.4          21   アインセフラ_ア… アルジ…     32.8 N    
#> 10     1    10        18.6          25.8 アインセフラ_ア… アルジ…     32.8 N    
#> # … with 41,318 more rows, and 3 more variables: longitude <dbl>, WE <chr>,
#> #   altitude <dbl>

  # download climate data
station_links %>%
  `$`("url") %>%
  `[[`(1) %>%
  download_climate()
#> # A tibble: 12 × 11
#>    station     country latitude NS    longitude WE    altitude month temperature
#>    <chr>       <chr>   <chr>    <chr> <chr>     <chr> <chr>    <dbl>       <dbl>
#>  1 アインセフ… アルジ… 32.77    N     0.60      W     1058         1         7.1
#>  2 アインセフ… アルジ… 32.77    N     0.60      W     1058         2         9.2
#>  3 アインセフ… アルジ… 32.77    N     0.60      W     1058         3        12.9
#>  4 アインセフ… アルジ… 32.77    N     0.60      W     1058         4        16.8
#>  5 アインセフ… アルジ… 32.77    N     0.60      W     1058         5        21.5
#>  6 アインセフ… アルジ… 32.77    N     0.60      W     1058         6        26.7
#>  7 アインセフ… アルジ… 32.77    N     0.60      W     1058         7        31  
#>  8 アインセフ… アルジ… 32.77    N     0.60      W     1058         8        29.5
#>  9 アインセフ… アルジ… 32.77    N     0.60      W     1058         9        24.4
#> 10 アインセフ… アルジ… 32.77    N     0.60      W     1058        10        18.6
#> 11 アインセフ… アルジ… 32.77    N     0.60      W     1058        11        12  
#> 12 アインセフ… アルジ… 32.77    N     0.60      W     1058        12         8.2
#> # … with 2 more variables: precipitation <dbl>, url <chr>
```

## Citation

Toshikazu Matsumura (2022) Tools for download climate data from Japan
Meteorological Agency with R. <https://github.com/matutosi/clidatajp/>.
