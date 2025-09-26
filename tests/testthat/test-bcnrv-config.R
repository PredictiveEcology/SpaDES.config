test_that("BC NRV [SCFM] config + context setup is working", {
  skip_on_cran()
  skip_if_not_installed("box")

  ## project: BC_HRV -------------------------------------------------------------------------------
  prjDir <- "~/GitHub/BC_HRV"
  prjMod <- file.path(prjDir, "box", "bcnrv.R")

  skip_if_not(file.exists(prjMod))

  .linkProject(prjMod)

  box::use(./box/bcnrv)

  ### ----------------------------------------------------------------------------------------------

  pr_mods <- list(
    "BC_HRV_preamble",
    "Biomass_borealDataPrep",
    "Biomass_core",
    "Biomass_regeneration",
    "Biomass_speciesData",
    "Biomass_speciesParameters",
    "LandWeb_output",
    "timeSinceFire"
  ) |>
    append(list(
      "scfmDiagnostics",
      "scfmDriver",
      "scfmEscape",
      "scfmIgnition",
      "scfmLandcoverInit",
      "scfmRegime",
      "scfmSpread"
    ))
  names(pr_mods) <- pr_mods
  dv_mods <- pr_mods
  pp_mods <- list(
    "BC_HRV_preamble",
    "Biomass_speciesData",
    "HSI_PineMarten",
    "LandWeb_summary",
    "NRV_summary"
  ) |>
    append(list("scfmDiagnostics"))
  names(pp_mods) <- pp_mods

  .studyAreaName <- c("Corkscrew", "Christenson Creek", "Downton", "Punky Moore")

  config.bc <- bcnrv$bcnrvConfig$new(
    projectPath = prjDir,
    fireModel = "scfm",
    nrvType = "hrv",
    mode = "development",
    rep = 1L,
    studyAreaName = .studyAreaName
  )$update()$validate() ## TODO: update & fix tests

  ## context
  expect_equal(
    config.bc$context[["runName"]],
    "multiple_LUs_n04_d6b6441dd2d0e467_scfm_hrv_BECSUBZONE_res125_rep01",
    ignore_attr = TRUE
  )

  ## args
  expect_equal(config.bc$args[["delayStart"]], 0L)

  ## modules
  expect_identical(config.bc$modules, pr_mods)

  ## options
  expect_equal(
    config.bc$options[["reproducible.destinationPath"]],
    config.bc$paths[["inputPath"]],
    ignore_attr = TRUE
  )

  ## params
  expect_identical(names(config.bc$params), c(".globals", names(config.bc$modules)))
  expect_identical(config.bc$params$.globals$.studyAreaName, "multiple_LUs_n04_d6b6441dd2d0e467")
  expect_identical(
    config.bc$params$Biomass_borealDataPrep$.studyAreaName,
    config.bc$params$.globals$.studyAreaName
  )
  expect_identical(config.bc$params$Biomass_borealDataPrep$.plots, config.bc$params$.globals$.plots)
  expect_identical(config.bc$params$Biomass_speciesData$types, NULL)
  expect_identical(config.bc$params$BC_HRV_preamble$pixelSize, 125)
  expect_identical(config.bc$params$BC_HRV_preamble$fireRegimePolysType, "BECSUBZONE")
  expect_identical(config.bc$params$scfmDiagnostics$mode, "single")

  ## paths
  fs_is_abs_paths <- fs::is_absolute_path(unlist(config.bc$paths))
  names(fs_is_abs_paths) <- names(unlist(config.bc$paths))
  expect_identical(
    fs_is_abs_paths,
    c(
      cachePath = FALSE,
      inputPath = FALSE,
      logPath = FALSE,
      modulePath1 = FALSE,
      modulePath2 = FALSE,
      outputPath = FALSE,
      projectPath = TRUE,
      scratchPath = TRUE
    )
  )
  expect_identical(
    SpaDES.config:::.getRelativePath(config.bc$paths[["outputPath"]], prjDir),
    file.path("outputs", "multiple_LUs_n04_d6b6441dd2d0e467_scfm_hrv_BECSUBZONE_res125", "rep01")
  )
  expect_identical(
    SpaDES.config:::.getRelativePath(config.bc$paths[["logPath"]], prjDir),
    file.path(SpaDES.config:::.getRelativePath(config.bc$paths[["outputPath"]], prjDir), "log")
  )

  ## single study area name using NRD/NRR ----------------------------------------------------------
  config.bc$context$studyAreaName <- "NRR_Cariboo"
  config.bc$update()$validate()

  ## context
  expect_equal(
    config.bc$context[["runName"]],
    "NRR_Cariboo_scfm_hrv_BECSUBZONE_res125_rep01",
    ignore_attr = TRUE
  )

  ## args
  ##

  ## modules
  ##

  ## options
  ##

  ## params
  #expect_identical(config.bc$params$BC_HRV_preamble$landscapeUnits, "NRR_Cariboo") ## TODO:

  ## paths
  fs_is_abs_paths <- fs::is_absolute_path(unlist(config.bc$paths))
  names(fs_is_abs_paths) <- names(unlist(config.bc$paths))
  expect_identical(
    fs_is_abs_paths,
    c(
      cachePath = FALSE,
      inputPath = FALSE,
      logPath = FALSE,
      modulePath1 = FALSE,
      modulePath2 = FALSE,
      outputPath = FALSE,
      projectPath = TRUE,
      scratchPath = TRUE
    )
  )
  expect_identical(
    SpaDES.config:::.getRelativePath(config.bc$paths[["outputPath"]], prjDir),
    file.path("outputs", "NRR_Cariboo_scfm_hrv_BECSUBZONE_res125", "rep01")
  )

  ## fire regime polygon types ---------------------------------------------------------------------
  config.bc$context$frpType <- "ECODISTRICT"
  config.bc$update()$validate()

  ## context
  expect_equal(
    config.bc$context[["runName"]],
    "NRR_Cariboo_scfm_hrv_ECODISTRICT_res125_rep01",
    ignore_attr = TRUE
  )

  ## args
  ##

  ## modules
  ##

  ## options
  ##

  ## params
  expect_identical(config.bc$params$BC_HRV_preamble$fireRegimePolysType, "ECODISTRICT")

  ## paths
  fs_is_abs_paths <- fs::is_absolute_path(unlist(config.bc$paths))
  names(fs_is_abs_paths) <- names(unlist(config.bc$paths))
  expect_identical(
    fs_is_abs_paths,
    c(
      cachePath = FALSE,
      inputPath = FALSE,
      logPath = FALSE,
      modulePath1 = FALSE,
      modulePath2 = FALSE,
      outputPath = FALSE,
      projectPath = TRUE,
      scratchPath = TRUE
    )
  )
  expect_identical(
    SpaDES.config:::.getRelativePath(config.bc$paths[["outputPath"]], prjDir),
    file.path("outputs", "NRR_Cariboo_scfm_hrv_ECODISTRICT_res125", "rep01")
  )
  expect_identical(
    SpaDES.config:::.getRelativePath(config.bc$paths[["logPath"]], prjDir),
    file.path(SpaDES.config:::.getRelativePath(config.bc$paths[["outputPath"]], prjDir), "log")
  )

  ## mode postprocess ------------------------------------------------------------------------------
  config.bc$context$mode <- "postprocess"
  config.bc$update()$validate()

  ## context
  expect_equal(
    config.bc$context[["runName"]],
    "NRR_Cariboo_scfm_hrv_ECODISTRICT_res125",
    ignore_attr = TRUE
  )

  ## args
  expect_equal(config.bc$args[["delayStart"]], 0L)

  ## modules
  expect_identical(config.bc$modules, pp_mods)

  ## options
  ##

  ## params
  expect_identical(config.bc$params$BC_HRV_preamble$fireRegimePolysType, "ECODISTRICT")
  expect_identical(config.bc$params$scfmDiagnostics$mode, "multi")

  ## paths
  fs_is_abs_paths <- fs::is_absolute_path(unlist(config.bc$paths))
  names(fs_is_abs_paths) <- names(unlist(config.bc$paths))
  expect_identical(
    fs_is_abs_paths,
    c(
      cachePath = FALSE,
      inputPath = FALSE,
      logPath = FALSE,
      modulePath1 = FALSE,
      modulePath2 = FALSE,
      outputPath = FALSE,
      projectPath = TRUE,
      scratchPath = TRUE
    )
  )
  expect_identical(
    SpaDES.config:::.getRelativePath(config.bc$paths[["outputPath"]], prjDir),
    file.path("outputs", "NRR_Cariboo_scfm_hrv_ECODISTRICT_res125")
  )
  expect_identical(
    SpaDES.config:::.getRelativePath(config.bc$paths[["logPath"]], prjDir),
    file.path(SpaDES.config:::.getRelativePath(config.bc$paths[["outputPath"]], prjDir), "log")
  )

  rm(config.bc)
})

test_that("BC NRV [FIRESENSE] config + context setup is working", {
  skip_on_cran()
  skip_if_not_installed("box")

  ## project: BC_HRV -------------------------------------------------------------------------------
  prjDir <- "~/GitHub/BC_HRV"
  prjMod <- file.path(prjDir, "box", "bcnrv.R")

  skip_if_not(file.exists(prjMod))

  .linkProject(prjMod)

  box::use(./box/bcnrv)

  pr_mods <- list(
    "BC_HRV_preamble",
    "Biomass_borealDataPrep",
    "Biomass_core",
    "Biomass_regeneration",
    "Biomass_speciesData",
    "Biomass_speciesParameters",
    "LandWeb_output",
    "timeSinceFire"
  ) |>
    append(list(
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
    ))
  names(pr_mods) <- pr_mods
  dv_mods <- pr_mods
  pp_mods <- list(
    "BC_HRV_preamble",
    "Biomass_speciesData",
    "HSI_PineMarten",
    "LandWeb_summary",
    "NRV_summary"
  ) |>
    append(list("Biomass_summary", "fireSense_summary"))
  names(pp_mods) <- pp_mods

  .studyAreaName <- c("Corkscrew", "Christenson Creek", "Downton", "Punky Moore")

  config.bc <- bcnrv$bcnrvConfig$new(
    projectPath = prjDir,
    fireModel = "fireSense",
    nrvType = "hrv",
    mode = "development",
    rep = 1L,
    studyAreaName = .studyAreaName
  )$update()$validate()

  ## context
  expect_equal(
    config.bc$context[["runName"]],
    "multiple_LUs_n04_d6b6441dd2d0e467_firesense_hrv_BECSUBZONE_res125_rep01",
    ignore_attr = TRUE
  )

  ## args
  expect_equal(config.bc$args[["delayStart"]], 0L)

  ## modules
  expect_identical(config.bc$modules, pr_mods)

  ## options
  expect_equal(
    config.bc$options[["reproducible.destinationPath"]],
    config.bc$paths[["inputPath"]],
    ignore_attr = TRUE
  )

  ## params
  expect_identical(names(config.bc$params), c(".globals", names(config.bc$modules)))
  expect_identical(config.bc$params$.globals$.studyAreaName, "multiple_LUs_n04_d6b6441dd2d0e467")
  expect_identical(
    config.bc$params$Biomass_borealDataPrep$.studyAreaName,
    config.bc$params$.globals$.studyAreaName
  )
  expect_identical(config.bc$params$Biomass_borealDataPrep$.plots, config.bc$params$.globals$.plots)
  expect_identical(config.bc$params$Biomass_speciesData$types, NULL)
  expect_identical(config.bc$params$BC_HRV_preamble$pixelSize, 125)
  expect_identical(config.bc$params$BC_HRV_preamble$fireRegimePolysType, "BECSUBZONE")
  expect_identical(config.bc$params$scfmDiagnostics$mode, "single")

  ## paths
  fs_is_abs_paths <- fs::is_absolute_path(unlist(config.bc$paths))
  names(fs_is_abs_paths) <- names(unlist(config.bc$paths))
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
    SpaDES.config:::.getRelativePath(config.bc$paths[["outputPath"]], prjDir),
    file.path(
      "outputs",
      "multiple_LUs_n04_d6b6441dd2d0e467_firesense_hrv_BECSUBZONE_res125",
      "rep01"
    )
  )
  expect_identical(
    SpaDES.config:::.getRelativePath(config.bc$paths[["logPath"]], prjDir),
    file.path(SpaDES.config:::.getRelativePath(config.bc$paths[["outputPath"]], prjDir), "log")
  )

  ## single study area name using NRD/NRR ----------------------------------------------------------
  config.bc$context$studyAreaName <- "NRR_Cariboo"
  config.bc$update()$validate()

  ## context
  expect_equal(
    config.bc$context[["runName"]],
    "NRR_Cariboo_firesense_hrv_BECSUBZONE_res125_rep01",
    ignore_attr = TRUE
  )

  ## args
  ##

  ## modules
  ##

  ## options
  ##

  ## params
  #expect_identical(config.bc$params$BC_HRV_preamble$landscapeUnits, "NRR_Cariboo") ## TODO:

  ## paths
  fs_is_abs_paths <- fs::is_absolute_path(unlist(config.bc$paths))
  names(fs_is_abs_paths) <- names(unlist(config.bc$paths))
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
    SpaDES.config:::.getRelativePath(config.bc$paths[["outputPath"]], prjDir),
    file.path("outputs", "NRR_Cariboo_firesense_hrv_BECSUBZONE_res125", "rep01")
  )

  ## fire regime polygon types ---------------------------------------------------------------------
  config.bc$context$frpType <- "ECODISTRICT"
  config.bc$update()$validate()

  ## context
  expect_equal(
    config.bc$context[["runName"]],
    "NRR_Cariboo_firesense_hrv_ECODISTRICT_res125_rep01",
    ignore_attr = TRUE
  )

  ## args
  ##

  ## modules
  ##

  ## options
  ##

  ## params
  expect_identical(config.bc$params$BC_HRV_preamble$fireRegimePolysType, "ECODISTRICT")

  ## paths
  fs_is_abs_paths <- fs::is_absolute_path(unlist(config.bc$paths))
  names(fs_is_abs_paths) <- names(unlist(config.bc$paths))
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
    SpaDES.config:::.getRelativePath(config.bc$paths[["outputPath"]], prjDir),
    file.path("outputs", "NRR_Cariboo_firesense_hrv_ECODISTRICT_res125", "rep01")
  )
  expect_identical(
    SpaDES.config:::.getRelativePath(config.bc$paths[["logPath"]], prjDir),
    file.path(SpaDES.config:::.getRelativePath(config.bc$paths[["outputPath"]], prjDir), "log")
  )

  ## mode postprocess ------------------------------------------------------------------------------
  config.bc$context$mode <- "postprocess"
  config.bc$update()$validate()

  ## context
  expect_equal(
    config.bc$context[["runName"]],
    "NRR_Cariboo_firesense_hrv_ECODISTRICT_res125",
    ignore_attr = TRUE
  )

  ## args
  expect_equal(config.bc$args[["delayStart"]], 0L)

  ## modules
  expect_identical(config.bc$modules, pp_mods)

  ## options
  ##

  ## params
  ##

  ## paths
  fs_is_abs_paths <- fs::is_absolute_path(unlist(config.bc$paths))
  names(fs_is_abs_paths) <- names(unlist(config.bc$paths))
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
    SpaDES.config:::.getRelativePath(config.bc$paths[["outputPath"]], prjDir),
    file.path("outputs", "NRR_Cariboo_firesense_hrv_ECODISTRICT_res125")
  )
  expect_identical(
    SpaDES.config:::.getRelativePath(config.bc$paths[["logPath"]], prjDir),
    file.path(SpaDES.config:::.getRelativePath(config.bc$paths[["outputPath"]], prjDir), "log")
  )

  rm(config.bc)
})
