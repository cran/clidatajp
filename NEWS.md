# clidatajp release news

# clidatajp 0.5.2.9000

* TODO
  * Add function for downloading detail data from 'JMA'
  * Add functions for calculating 'ci' (cool index) and 'wi' (warm index).

# clidatajp 0.5.2

* 2023-03-05

* Add data
    * `climate_station` include ULR of 3444 stations
    * `station_jp` include information of 157 stations
    * `station_world` include information of 3444 stations

* Rename data
    * `japan_climate` to `climate_jp`
    * `world_climate` to `climate_world`

# clidatajp 0.5.1

* 2022-11-05

* Add graceful fail to match the CRAN policy: 
    * 'Packages which use Internet resources should fail gracefully with an informative message if the resource is not available or has changed (and not give a check warning nor error).'
    * Added graceful fail in download_climate(), download_area_links(), and download_links().
    * Improved tests.

# clidatajp 0.5.0

* 2022-10-05

* First release
    * `data(japan_climate)` include japan climate data from Japan Meteorological Agency ('JMA').
    * `data(world_climate)` include world climate data from 'JMA'.
    * `data(station_links)` include station information and its links for 'JMA'.
    * `download_area_links()` and `* `download_links()` download links for climate data. 
    * `download_climate()` download climate data from 'JMA'.
