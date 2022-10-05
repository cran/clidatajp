test_that("sleep works", {
  before <- Sys.time()
  sleep()  # default: 5 sec
  after <- Sys.time()
  expect_identical((after - before) > 5, TRUE)

  before <- Sys.time()
  sleep(3)
  after <- Sys.time()
  expect_identical((after - before) > 3, TRUE)
})
