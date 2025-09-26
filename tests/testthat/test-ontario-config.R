test_that("Ontario config + context setup is working", {
  skip_on_cran()
  skip_if_not_installed("box")

  ## project: Ontario_AOU_ROF ----------------------------------------------------------------------
  prjDir <- "~/GitHub/Ontario_AOU_ROF"
  prjMod <- file.path(prjDir, "box", "onnrv.R")

  skip_if_not(file.exists(prjMod))

  .linkProject(prjMod)

  box::use(./box/onnrv)

  ## NOTE: user expected to add their preamble module to their project config
  pr_mods <- list(
    "Biomass_borealDataPrep",
    "Biomass_core",
    "Biomass_regeneration",
    "Biomass_speciesData",
    "Biomass_speciesFactorial",
    "Biomass_speciesParameters",
    "canClimateData",
    "fireSense",
    "fireSense_dataPrepFit",
    "fireSense_dataPrepPredict",
    "fireSense_EscapeFit",
    "fireSense_EscapePredict",
    "fireSense_IgnitionFit",
    "fireSense_IgnitionPredict",
    "fireSense_SpreadFit",
    "fireSense_SpreadPredict",
    "gmcsDataPrep"
  )
  names(pr_mods) <- pr_mods
  dv_mods <- pr_mods
  pp_mods <- list("Biomass_summary", "fireSense_summary", "birds_BRT", "NRV_summary")
  names(pp_mods) <- pp_mods

  .studyAreaName <- "ON_AOU_5"

  config.onnrv <- onnrv$onnrvConfig$new(
    projectPath = prjDir,
    climateGCM = "CanESM5",
    climateSSP = 370,
    mode = "development",
    nrvType = "hrv",
    rep = 1L,
    res = 250,
    studyAreaName = .studyAreaName
  )$update()$validate() ## TODO: update & fix tests

  ## context
  expect_equal(
    config.onnrv$context[["runName"]],
    "ON_AOU_5_CanESM5_SSP370_rep01",
    ignore_attr = TRUE
  )

  ## args
  expect_equal(config.onnrv$args[["delayStart"]], 0L)
  expect_equal(config.onnrv$args[["simYears"]], list(start = 2011, end = 2100))

  ## modules
  expect_identical(config.onnrv$modules, pr_mods)

  ## options
  expect_equal(
    config.onnrv$options[["reproducible.destinationPath"]],
    config.onnrv$paths[["inputPath"]],
    ignore_attr = TRUE
  )

  ## params
  expect_identical(names(config.onnrv$params), c(".globals", names(config.onnrv$modules)))
  expect_identical(config.onnrv$params$.globals$.studyAreaName, "ON_AOU_5")
  expect_identical(
    config.onnrv$params$Biomass_borealDataPrep$.studyAreaName,
    config.onnrv$params$.globals$.studyAreaName
  )
  expect_identical(
    config.onnrv$params$Biomass_borealDataPrep$.plots,
    config.onnrv$params$.globals$.plots
  )
  expect_identical(config.onnrv$params$Biomass_speciesData$types, "KNN")

  ## paths
  fs_is_abs_paths <- fs::is_absolute_path(unlist(config.onnrv$paths))
  names(fs_is_abs_paths) <- names(unlist(config.onnrv$paths))
  expect_identical(
    fs_is_abs_paths,
    c(
      cachePath = FALSE,
      inputPath = FALSE,
      logPath = FALSE,
      modulePath = FALSE,
      outputPath = FALSE,
      projectPath = TRUE,
      scratchPath = TRUE
    )
  )
  expect_identical(
    .getRelativePath(config.onnrv$paths[["outputPath"]], prjDir),
    file.path("outputs", "ON_AOU_5_CanESM5_SSP370", "rep01")
  )
  expect_identical(
    .getRelativePath(config.onnrv$paths[["logPath"]], prjDir),
    file.path(.getRelativePath(config.onnrv$paths[["outputPath"]], prjDir), "log")
  )

  ## context 'fit' ---------------------------------------------------------------------------------
  config.onnrv$context[["mode"]] <- c("development", "fit")
  config.onnrv$update()$validate()

  ## context
  expect_equal(
    config.onnrv$context[["runName"]],
    "ON_AOU_5_CanESM5_SSP370_rep01",
    ignore_attr = TRUE
  )

  ## args
  expect_equal(config.onnrv$args[["delayStart"]], 0L)
  expect_equal(config.onnrv$args[["simYears"]], list(start = 2011, end = 2100))

  ## modules
  expect_identical(config.onnrv$modules, pr_mods)

  ## options
  expect_equal(
    config.onnrv$options[["reproducible.destinationPath"]],
    config.onnrv$paths[["inputPath"]],
    ignore_attr = TRUE
  )

  ## params
  expect_identical(names(config.onnrv$params), c(".globals", names(config.onnrv$modules)))
  expect_identical(config.onnrv$params$.globals$.studyAreaName, "ON_AOU_5")
  expect_identical(
    config.onnrv$params$Biomass_borealDataPrep$.studyAreaName,
    config.onnrv$params$.globals$.studyAreaName
  )
  expect_identical(
    config.onnrv$params$Biomass_borealDataPrep$.plots,
    config.onnrv$params$.globals$.plots
  )
  expect_identical(config.onnrv$params$Biomass_speciesData$types, "KNN")

  ## paths
  fs_is_abs_paths <- fs::is_absolute_path(unlist(config.onnrv$paths))
  names(fs_is_abs_paths) <- names(unlist(config.onnrv$paths))
  expect_identical(
    fs_is_abs_paths,
    c(
      cachePath = FALSE,
      inputPath = FALSE,
      logPath = FALSE,
      modulePath = FALSE,
      outputPath = FALSE,
      projectPath = TRUE,
      scratchPath = TRUE
    )
  )
  expect_identical(
    .getRelativePath(config.onnrv$paths[["outputPath"]], prjDir),
    file.path("outputs", "ON_AOU_5_CanESM5_SSP370", "rep01")
  )
  expect_identical(
    .getRelativePath(config.onnrv$paths[["logPath"]], prjDir),
    file.path(.getRelativePath(config.onnrv$paths[["outputPath"]], prjDir), "log")
  )

  ## climate scenarios -----------------------------------------------------------------------------
  config.onnrv$context[["climateSSP"]] <- 585
  config.onnrv$update()$validate()

  ## context
  expect_equal(
    config.onnrv$context[["runName"]],
    "ON_AOU_5_CanESM5_SSP585_rep01",
    ignore_attr = TRUE
  )

  ## args
  ##

  ## modules
  ##

  ## options
  ##

  ## params
  ##

  ## paths
  fs_is_abs_paths <- fs::is_absolute_path(unlist(config.onnrv$paths))
  names(fs_is_abs_paths) <- names(unlist(config.onnrv$paths))
  expect_identical(
    fs_is_abs_paths,
    c(
      cachePath = FALSE,
      inputPath = FALSE,
      logPath = FALSE,
      modulePath = FALSE,
      outputPath = FALSE,
      projectPath = TRUE,
      scratchPath = TRUE
    )
  )
  expect_identical(
    .getRelativePath(config.onnrv$paths[["outputPath"]], prjDir),
    file.path("outputs", "ON_AOU_5_CanESM5_SSP585", "rep01")
  )
  expect_identical(
    .getRelativePath(config.onnrv$paths[["logPath"]], prjDir),
    file.path(.getRelativePath(config.onnrv$paths[["outputPath"]], prjDir), "log")
  )

  ## mode postprocess ------------------------------------------------------------------------------
  config.onnrv$context$mode <- "postprocess"
  config.onnrv$update()$validate()

  ## context
  expect_equal(config.onnrv$context[["runName"]], "ON_AOU_5_CanESM5_SSP585", ignore_attr = TRUE)

  ## args
  expect_equal(config.onnrv$args[["delayStart"]], 0L)

  ## modules
  expect_identical(config.onnrv$modules, pp_mods)

  ## options
  ##

  ## params
  ##

  ## paths
  fs_is_abs_paths <- fs::is_absolute_path(unlist(config.onnrv$paths))
  names(fs_is_abs_paths) <- names(unlist(config.onnrv$paths))
  expect_identical(
    fs_is_abs_paths,
    c(
      cachePath = FALSE,
      inputPath = FALSE,
      logPath = FALSE,
      modulePath = FALSE,
      outputPath = FALSE,
      projectPath = TRUE,
      scratchPath = TRUE
    )
  )
  expect_identical(
    .getRelativePath(config.onnrv$paths[["outputPath"]], prjDir),
    file.path("outputs", "ON_AOU_5_CanESM5_SSP585")
  )
  expect_identical(
    .getRelativePath(config.onnrv$paths[["logPath"]], prjDir),
    file.path(.getRelativePath(config.onnrv$paths[["outputPath"]], prjDir), "log")
  )
  rm(config.onnrv)
})
