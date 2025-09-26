test_that("LandWeb config + context setup is working", {
  skip_on_cran()
  skip_if_not_installed("box")

  ## project: landweb ------------------------------------------------------------------------------
  prjDir <- "~/GitHub/LandWeb"
  prjMod <- file.path(prjDir, "box", "landweb.R")

  skip_if_not(file.exists(prjMod))

  .linkProject(prjMod)

  box::use(./box/landweb)

  pr_mods <- list(
    "Biomass_borealDataPrep",
    "Biomass_core",
    "Biomass_regeneration",
    "Biomass_speciesData",
    "Biomass_speciesFactorial",
    "Biomass_speciesParameters",
    "LandMine",
    "LandWeb_output",
    "LandWeb_preamble",
    "timeSinceFire"
  )
  names(pr_mods) <- pr_mods
  dv_mods <- pr_mods
  pp_mods <- list(
    "LandWeb_preamble",
    "Biomass_speciesFactorial",
    "Biomass_speciesData",
    "Biomass_speciesParameters",
    "burnSummaries",
    "LandMine",
    "LandWeb_summary"
  )
  names(pp_mods) <- pp_mods

  ## study area: LandWeb ---------------------------------------------------------------------------

  config.lw <- landweb$landwebConfig$new(
    projectName = "LandWeb",
    projectPath = prjDir,
    mode = "development",
    rep = 1L,
    studyAreaName = "LandWeb"
  )$update()$validate() ## TODO: update & fix tests

  ## context
  expect_equal(config.lw$context[["runName"]], "LandWeb_full_v3_rep01", ignore_attr = TRUE)

  ## args
  expect_equal(config.lw$args[["delayStart"]], 0L)

  ## modules
  expect_identical(config.lw$modules, pr_mods)

  ## options
  expect_equal(
    config.lw$options[["reproducible.destinationPath"]],
    config.lw$paths[["inputPath"]],
    ignore_attr = TRUE
  )

  ## params
  expect_identical(names(config.lw$params), c(".globals", names(config.lw$modules)))
  expect_identical(config.lw$params$.globals$.studyAreaName, "LandWeb_full_v3")
  expect_identical(config.lw$params$Biomass_core$initialB, 10)
  expect_identical(
    config.lw$params$Biomass_borealDataPrep$.studyAreaName,
    config.lw$params$.globals$.studyAreaName
  )
  expect_identical(config.lw$params$Biomass_borealDataPrep$.plots, config.lw$params$.globals$.plots)
  expect_identical(
    config.lw$params$Biomass_speciesData$types,
    c("KNN", "CASFRI", "Pickell", "ForestInventory")
  )

  ## paths
  expect_identical(
    fs::is_absolute_path(unlist(config.lw$paths)),
    c(
      cachePath = FALSE,
      inputPath = FALSE,
      logPath = FALSE,
      modulePath = FALSE,
      outputPath = FALSE,
      projectPath = TRUE,
      scratchPath = TRUE
    ) |>
      unname()
  )
  expect_identical(
    .getRelativePath(config.lw$paths[["logPath"]], prjDir),
    file.path("outputs", "LandWeb_full_v3", "rep01", "log")
  )

  rm(config.lw)

  ## study area: manitoba --------------------------------------------------------------------------
  config.mb <- landweb$landwebConfig$new(
    projectName = "LandWeb",
    projectPath = prjDir,
    mode = "production",
    rep = 5,
    ROStype = "burny",
    studyAreaName = "provMB"
  )

  ## context
  expect_equal(config.mb$context[["runName"]], "provMB_v3_burnyROS_rep05", ignore_attr = TRUE)

  ## args
  expect_gt(config.mb$args[["delayStart"]], 0L)

  ## modules
  expect_identical(config.mb$modules, dv_mods)

  ## options
  expect_equal(
    config.mb$options[["reproducible.destinationPath"]],
    config.mb$paths[["inputPath"]],
    ignore_attr = TRUE
  )

  ## params
  expect_identical(config.mb$params[[".globals"]][[".studyAreaName"]], "provMB_v3")
  expect_identical(
    config.mb$params[["Biomass_speciesData"]][["types"]],
    c("KNN", "CASFRI", "Pickell", "MBFRI")
  )

  ## paths
  expect_identical(
    .getRelativePath(config.mb$paths[["logPath"]], prjDir),
    file.path("outputs", "provMB_v3_burnyROS", "rep05", "log")
  )

  rm(config.mb)

  ## study area: Tolko_AB_N ------------------------------------------------------------------------
  ## mode:       postprocess
  config.pp.tolko <- landweb$landwebConfig$new(
    projectName = "LandWeb",
    projectPath = prjDir,
    mode = "postprocess",
    rep = NA_integer_,
    studyAreaName = "Tolko_AB_N"
  )$update()$validate()

  ## context
  expect_equal(
    config.pp.tolko$context[["runName"]],
    "Tolko_AB_N",
    ignore_attr = TRUE
  )

  ## args
  expect_equal(config.pp.tolko$args[["delayStart"]], 0L)

  ## modules
  pp_mods <- list(
    "LandWeb_preamble",
    "Biomass_speciesData",
    "burnSummaries",
    "LandMine",
    "LandWeb_summary"
  )
  names(pp_mods) <- pp_mods
  expect_identical(config.pp.tolko$modules, pp_mods)

  ## params
  expect_identical(config.pp.tolko$params[[".globals"]][[".studyAreaName"]], "Tolko_AB_N")
  expect_identical(config.pp.tolko$params[[".globals"]][["initialB"]], NA_real_)
  expect_identical(
    config.pp.tolko$params[["Biomass_speciesData"]][["types"]],
    c("KNN", "CASFRI", "Pickell", "ForestInventory")
  )
  expect_identical(config.pp.tolko$params[["LandWeb_summary"]][["reps"]], 1L:15L)

  expect_identical(
    .getRelativePath(config.pp.tolko$paths[["logPath"]], prjDir),
    file.path("outputs", "Tolko_AB_N_aspenDispersal_logROS", "log")
  )

  rm(config.pp.tolko)

  ## study area: FMU E14 ---------------------------------------------------------------------------
  ## mode:       production
  config.pp.e14 <- landweb$landwebConfig$new(
    projectName = "LandWeb",
    projectPath = prjDir,
    mode = "production",
    rep = 10,
    studyAreaName = "FMU_E14"
  )$update()$validate()

  ## context
  expect_equal(config.pp.e14$context[["pixelSize"]], 250)

  config.pp.e14$context[["pixelSize"]] <- 125
  config.pp.e14$update() ## required after context changes
  expect_equal(config.pp.e14$context[["pixelSize"]], 125)

  ## args
  expect_gt(config.pp.e14$args[["delayStart"]], 0L)

  ## modules
  expect_identical(config.pp.e14$modules, pr_mods)

  ## params
  expect_identical(config.pp.e14$params[[".globals"]][[".studyAreaName"]], "FMU_E14")
  expect_identical(config.pp.e14[["params"]][["Biomass_core"]][["initialB"]], NA_real_)
  expect_identical(
    config.pp.e14$params[["Biomass_speciesData"]][["types"]],
    c("KNN", "CASFRI", "Pickell", "ForestInventory")
  )

  expect_identical(
    .getRelativePath(config.pp.e14$paths[["logPath"]], prjDir),
    file.path("outputs", "FMU_E14_highDispersal_logROS_res125", "rep10", "log")
  )

  rm(config.pp.e14)
})
