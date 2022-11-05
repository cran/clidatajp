# clidatajp release news


# clidatajp 0.5.1.9000

* TODO
  * Add function for downloading detail data from 'JMA'

# clidatajp 0.5.1

* Add graceful fail to match the CRAN policy: 
    * 'Packages which use Internet resources should fail gracefully with an informative message if the resource is not available or has changed (and not give a check warning nor error).'
    * Added graceful fail in download_climate(), download_area_links(), and download_links().
    * Improved tests.

# clidatajp 0.5.0

* First release
* `data(japan_climate)` include japan climate data from Japan Meteorological Agency ('JMA').
* `data(world_climate)` include world climate data from 'JMA'.
* `data(station_links)` include station information and its links for 'JMA'.
* `download_area_links()` and `* `download_links()` download links for climate data. 
* `download_climate()` download climate data from 'JMA'.
