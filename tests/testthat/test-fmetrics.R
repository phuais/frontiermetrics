test_that("loads init_FrontierMetric and creates FrontierMetric", {
  expect_s4_class({
    curl::curl_download(frontiermetrics_data[4], file.path(tempdir(), "copo_dataset.RDS"))
    copo_dataset <- readRDS(file.path(tempdir(), "copo_dataset.RDS"))
    copo_metrics2 <- fmetrics(copo_dataset, metrics = c("baseline", "speed", "left",
                                                        "onset", "activeness", "left"),
                              silent = T)
    copo_metrics2
  }, "FrontierMetric")

  expect_equal({
    any(is.na(copo_metrics2@data$baseline))
  }, FALSE)

  expect_equal({
    any(is.na(copo_metrics2@data$speed))
  }, FALSE)

  expect_equal({
    any(is.na(copo_metrics2@data$left))
  }, FALSE)

  expect_equal({
    any(is.na(copo_metrics2@data$onset))
  }, FALSE)

  expect_equal({
    any(is.na(copo_metrics2@data$activeness))
  }, FALSE)

  expect_equal({
    any(is.na(copo_metrics2@data$left))
  }, FALSE)
})

test_that("loads FrontierMetric, creates summary and generates plot", {
  expect_s4_class({
    copo_metrics
  }, "FrontierMetric")

  expect_equal({
    summ <- fmetrics_summary(copo_metrics)
    unique(class(summ@summary_stats), class(summ@classes_areas))
  }, "data.frame")

  expect_s3_class({
    fmetrics_plot(x = copo_metrics)
  }, "gtable")

})
