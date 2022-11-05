test_that("download_area_links(), download_links() and download_climate() works", {
    # area links
  area <- download_area_links()
  exep <- 
    stringr::str_c("https://www.data.jma.go.jp/gmd/cpd/monitor/nrmlist/", 
      c("CountryList.php?rcode=01", "CountryList.php?rcode=02", "CountryList.php?rcode=03", 
        "CountryList.php?rcode=04", "CountryList.php?rcode=05", "CountryList.php?rcode=06"))
  if(is.null(area)){
    skip("download_area_links() is not available")
  }else{
    expect_equal(area, exep)
  }

    # country links
  country <- download_links(sample(area, 1))
  if(is.null(country)){
    skip("url of area is not available")
  }else{
    zero <- sum(!stringr::str_detect(country, "https://www.data.jma.go.jp/gmd/cpd/monitor/nrmlist/"))
    expect_equal(zero, 0)
  }

    # station links
  station <- download_links(sample(country, 1))
  if(is.null(station)){
    skip("url of country is not available")
  }else{
    zero <- sum(!stringr::str_detect(station, "https://www.data.jma.go.jp/gmd/cpd/monitor/nrmlist/"))
    expect_equal(zero, 0)
  }

    # climate data
  climate <- download_climate(sample(station, 1))
  if(is.null(climate)){
    skip("url of station is not available")
  }else{
    expect_equal(nrow(climate), 12)
    expect_equal(ncol(climate), 11)
  }

})
