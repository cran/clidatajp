test_that("detail_url() and detail_item() work", {
  base <- "https://www.data.jma.go.jp/obd/stats/etrn/view/"
    # kansho (block_no has 5 digits) and amedas (4 digits)
  expect_equal(detail_item("daily" , 47759), "daily_s1")
  expect_equal(detail_item("daily" , "0588"), "daily_a1")
  expect_equal(detail_item("nml_ym", 47759), "nml_sfc_ym")
  expect_equal(detail_item("nml_ym", "0588"), "nml_amd_ym")
    # item is used as it is when not in the list
  expect_equal(detail_item("daily_s1", "0588"), "daily_s1")

  expect_equal(
    detail_url(61, 47759, "daily", 2023, 6),
    stringr::str_c(base, "daily_s1.php?prec_no=61&block_no=47759",
                   "&year=2023&month=6&day=&view="))
  expect_equal(
    detail_url(61, "0588", "hourly", 2023, 6, 20),
    stringr::str_c(base, "hourly_a1.php?prec_no=61&block_no=0588",
                   "&year=2023&month=6&day=20&view="))
    # vectorized
  expect_equal(length(detail_url(61, 47759, "daily", 2023, 1:3)), 3)
})

test_that("helper functions of download_detail() work", {
  header <- data.frame(x = c("temperature", "temperature"),
                       y = c("temperature", "mean"),
                       z = c("", ""))
  expect_equal(detail_colnames(header),
               c("temperature", "temperature_mean", "V3"))
    # marks of quality are removed, marks of no value become NA
  expect_equal(clean_detail_value(c("21.6", "0.0 )", "5.0 ]", "1.0 #")),
               c("21.6", "0.0", "5.0", "1.0"))
  expect_equal(clean_detail_value(c("--", "///", "")),
               rep(NA_character_, 3))
    # converted only when all values can be converted
  expect_equal(to_numeric_when_all(c("21.6", "0.0", NA)), c(21.6, 0.0, NA))
  expect_equal(to_numeric_when_all(c("21.6", "north")), c("21.6", "north"))
  expect_equal(to_numeric_when_all(c(NA, NA)), c(NA, NA))
})

test_that("download_detail(), download_prec_no() and download_block_no() work", {
  skip_on_cran()

    # daily data of a kansho station (Kyoto, June 2023 has 30 days)
  daily <- download_detail(detail_url(61, 47759, "daily", 2023, 6))
  if(is.null(daily)){
    skip("detail data is not available")
  }else{
    expect_equal(nrow(daily), 30)
    expect_true(is.numeric(daily[[1]]))
    expect_true("url" %in% colnames(daily))
  }

  prec <- download_prec_no()
  if(is.null(prec)){
    skip("prec_no is not available")
  }else{
    expect_equal(colnames(prec), c("prec_no", "area"))
    expect_true(nrow(prec) > 0)
  }

  block <- download_block_no(61)
  if(is.null(block)){
    skip("block_no is not available")
  }else{
    expect_equal(colnames(block),
                 c("prec_no", "block_no", "station", "type"))
    expect_true(all(block[["type"]] %in% c("kansho", "amedas")))
  }
})
