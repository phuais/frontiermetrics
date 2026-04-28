test_that("creates object of class init_FrontierMetric", {
  expect_s4_class({
    tmp_file1 <- file.path(tempdir(), "tree_cover.tif")
    tmp_file2 <- file.path(tempdir(), "loss_year.tif")
    curl::curl_download(frontiermetrics_data[3], tmp_file1)
    curl::curl_download(frontiermetrics_data[4], tmp_file2)
    rast_cover <- terra::rast(tmp_file1)
    rast_loss <- terra::rast(tmp_file2)
    copo_dataset <- init_fmetrics(raster = list(rast_cover, rast_loss),
                                  time_frame = c(2000, 2024),
                                  tag = "Copo National Park",
                                  ncores = 1)
    unlink(tmp_file1)
    unlink(tmp_file2)
    copo_dataset
  }, "init_FrontierMetric")

  expect_s4_class({
    tmp_file1 <- file.path(tempdir(), "cover_series.tif")
    study_area <- terra::ext(-61.86 - 0.36,-61.86 + 0.36,
                             -25.81 - 0.36, -25.81 + 0.36)
    study_area <- terra::as.polygons(study_area)
    terra::crs(study_area) <- "EPSG:4326"
    curl::curl_download(frontiermetrics_data[6], tmp_file1)
    cover_series <- terra::rast(tmp_file1)
    cover_series <- terra::crop(cover_series, study_area)
    copo_dataset <- init_fmetrics(raster = cover_series,
                                  tag = "Copo National Park - MapBiomas",
                                  is_series = TRUE,
                                  is_continuous = FALSE,
                                  aggregation = c(3, 10),
                                  time_frame = c(2000, 2023))
    unlink(tmp_file1)
    copo_dataset
  }, "init_FrontierMetric")
})

test_that("loads init_FrontierMetric and creates FrontierMetric, with paramater variations", {
  expect_s4_class({
    tmp_file <- file.path(tempdir(), "copo_dataset.RDS")
    curl::curl_download(frontiermetrics_data[5], tmp_file)
    copo_dataset <- readRDS(tmp_file)
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

  expect_s4_class({
    copo_metrics2 <- fmetrics(copo_dataset, metrics = c("baseline", "speed", "left",
                                                        "onset", "activeness", "left"),
                              summary = FALSE,
                              silent = T)
    copo_metrics2
  }, "FrontierMetric")

  expect_s4_class({
    copo_metrics2 <- fmetrics(copo_dataset, metrics = c("baseline", "speed", "left",
                                                        "onset", "activeness", "left"),
                              params = list(activeness_levels = list(very.old = 1:14, old = 15:18, emerging = 19:20)),
                              silent = T)
    copo_metrics2
  }, "FrontierMetric")

  expect_s4_class({
    copo_metrics2 <- fmetrics(copo_dataset, metrics = c("baseline", "speed", "left",
                                                        "onset", "activeness", "left"),
                              params = list(onset_min_years = 4),
                              silent = T)
    copo_metrics2
  }, "FrontierMetric")

  expect_s4_class({
    breaks_1 <- breaks_rules(baseline = list(c(5, 20, 70, Inf), c("Low", "Medium", "High")))
    copo_metrics2 <- fmetrics(x = copo_dataset,
                              metrics = "baseline",
                              breaks = breaks_1,
                              silent = T)
    copo_metrics2
  }, "FrontierMetric")

  expect_s4_class({
  ud_loss_ha <- function(x){
    fm_ds <- x@data[, lapply(.SD, sum, na.rm = TRUE),
                    by = id_cell,
                    .SDcols = x@fl_cols]
    fm_ds$ud_loss_ha <- rowSums(fm_ds[, 2:ncol(fm_ds)]) * 100
    fm_ds <- fm_ds[, c("id_cell", "ud_loss_ha"), with = F]
    return(fm_ds)
  }
  copo_metrics2 <- fmetrics(x = copo_dataset, metrics = c("ud_loss_ha"),  silent = T)
  unlink(tmp_file)
  copo_metrics2
  }, "FrontierMetric")

})

test_that("loads FrontierMetric, creates summary and generates plot, with variations", {
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

  expect_s3_class({
    fmetrics_plot(x = copo_metrics, what = "values")
  }, "gtable")

  expect_s3_class({
    fmetrics_plot(x = copo_metrics, what = "archetypes", archetypes = list(1, 2, 3))
  }, "ggplot")

  expect_s3_class({
    fmetrics_plot(x = copo_metrics, what = "classes")
  }, "gtable")

  expect_s3_class({
    fmetrics_plot(x = copo_metrics, ncol = 2)
  }, "gtable")

  expect_s3_class({
    fmetrics_plot(x = copo_metrics, palette = "turbo")
  }, "gtable")

  expect_s3_class({
    fmetrics_plot(x = copo_metrics, direction = -1)
  }, "gtable")

  expect_s3_class({
    fmetrics_plot(x = copo_metrics, background = c("white", "black"))
  }, "gtable")

})
