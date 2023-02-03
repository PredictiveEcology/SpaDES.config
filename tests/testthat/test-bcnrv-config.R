test_that("BC NRV config + context setup is working", {
  skip("Run manually with latest version of BC_HRV Project on test machine.")

  ## project: BC_HRV ------------------------------------------------------------------------------
  prjDir <- "~/GitHub/BC_HRV"

  pr_mods <- list("BC_HRV_preamble", "Biomass_borealDataPrep", "Biomass_core",
                  "Biomass_regeneration", "Biomass_speciesData", "LandWeb_output",
                  "scfmDiagnostics", "scfmDriver", "scfmEscape", "scfmIgnition",
                  "scfmLandcoverInit", "scfmRegime", "scfmSpread", "timeSinceFire")
  names(pr_mods) <- pr_mods
  dv_mods <- pr_mods
  pp_mods <- list("BC_HRV_preamble", "Biomass_speciesData", "HSI_PineMarten",
                  "LandWeb_summary", "NRV_summary", "scfmDiagnostics")
  names(pp_mods) <- pp_mods

  .studyAreaName <- c("Corkscrew", "Christenson Creek", "Downton", "Punky Moore")
  config.bc <- suppressWarnings({
    useConfig(projectName = "BC_NRV", projectPath = prjDir,
              mode = "development", rep = 1L, studyAreaName = .studyAreaName)
  })

  ## context
  expect_equal(config.bc$context[["runName"]],
               "multiple_LUs_n04_d6b6441dd2d0e467_BECSUBZONE_res125_rep01", ignore_attr = TRUE)

  ## args
  expect_equal(config.bc$args[["delayStart"]], 0L)

  ## modules
  expect_identical(config.bc$modules, pr_mods)

  ## options
  expect_equal(config.bc$options[["map.dataPath"]], config.bc$paths[["inputPath"]], ignore_attr = TRUE)
  expect_equal(config.bc$options[["map.tilePath"]], config.bc$paths[["tilePath"]], ignore_attr = TRUE)
  expect_equal(config.bc$options[["reproducible.destinationPath"]], config.bc$paths[["inputPath"]], ignore_attr = TRUE)

  ## params
  expect_identical(names(config.bc$params), c(".globals", names(config.bc$modules)))
  expect_identical(config.bc$params$.globals$.studyAreaName, "multiple_LUs_n04_d6b6441dd2d0e467")
  expect_identical(config.bc$params$Biomass_borealDataPrep$.studyAreaName, config.bc$params$.globals$.studyAreaName)
  expect_identical(config.bc$params$Biomass_borealDataPrep$.plots, config.bc$params$.globals$.plots)
  expect_identical(config.bc$params$Biomass_speciesData$types, NULL)
  expect_identical(config.bc$params$BC_HRV_preamble$pixelSize, 125)
  expect_identical(config.bc$params$BC_HRV_preamble$fireRegimePolysType, "BECSUBZONE")
  expect_identical(config.bc$params$scfmDiagnostics$mode, "single")

  ## paths
  expect_identical(.isAbsolutePath(unlist(config.bc$paths)),
                   c(cachePath = FALSE, inputPath = FALSE, logPath = FALSE,
                     modulePath1 = FALSE, modulePath2 = FALSE, outputPath = FALSE,
                     projectPath = TRUE, scratchPath = TRUE, tilePath = FALSE))
  expect_identical(
    .getRelativePath(config.bc$paths[["outputPath"]], prjDir),
    file.path("outputs", "multiple_LUs_n04_d6b6441dd2d0e467_BECSUBZONE_res125", "rep01")
  )
  expect_identical(
    .getRelativePath(config.bc$paths[["logPath"]], prjDir),
    file.path(.getRelativePath(config.bc$paths[["outputPath"]], prjDir), "log")
  )
  expect_identical(
    .getRelativePath(config.bc$paths[["tilePath"]], prjDir),
    file.path(.getRelativePath(config.bc$paths[["outputPath"]], prjDir), "tiles")
  )

  ## single study area name using NRD/NRR ----------------------------------------------------------
  config.bc$context$studyAreaName <- "NRR_Cariboo"
  config.bc$update()$validate()

  ## context
  expect_equal(config.bc$context[["runName"]],
               "NRR_Cariboo_BECSUBZONE_res125_rep01", ignore_attr = TRUE)

  ## args
  ##

  ## modules
  ##

  ## options
  ##

  ## params
  #expect_identical(config.bc$params$BC_HRV_preamble$landscapeUnits, "NRR_Cariboo") ## TODO:

  ## paths
  expect_identical(.isAbsolutePath(unlist(config.bc$paths)),
                   c(cachePath = FALSE, inputPath = FALSE, logPath = FALSE,
                     modulePath1 = FALSE, modulePath2 = FALSE, outputPath = FALSE,
                     projectPath = TRUE, scratchPath = TRUE, tilePath = FALSE))
  expect_identical(
    .getRelativePath(config.bc$paths[["outputPath"]], prjDir),
    file.path("outputs", "NRR_Cariboo_BECSUBZONE_res125", "rep01")
  )

  ## fire regime polygon types ---------------------------------------------------------------------
  config.bc$context$frpType <- "ECODISTRICT"
  config.bc$update()$validate()

  ## context
  expect_equal(config.bc$context[["runName"]],
               "multiple_LUs_n04_d6b6441dd2d0e467_ECODISTRICT_res125_rep01", ignore_attr = TRUE)

  ## args
  ##

  ## modules
  ##

  ## options
  ##

  ## params
  expect_identical(config.bc$params$BC_HRV_preamble$fireRegimePolysType, "ECODISTRICT")

  ## paths
  expect_identical(.isAbsolutePath(unlist(config.bc$paths)),
                   c(cachePath = FALSE, inputPath = FALSE, logPath = FALSE,
                     modulePath1 = FALSE, modulePath2 = FALSE, outputPath = FALSE,
                     projectPath = TRUE, scratchPath = TRUE, tilePath = FALSE))
  expect_identical(
    .getRelativePath(config.bc$paths[["outputPath"]], prjDir),
    file.path("outputs", "multiple_LUs_n04_d6b6441dd2d0e467_ECODISTRICT_res125", "rep01")
  )
  expect_identical(
    .getRelativePath(config.bc$paths[["logPath"]], prjDir),
    file.path(.getRelativePath(config.bc$paths[["outputPath"]], prjDir), "log")
  )
  expect_identical(
    .getRelativePath(config.bc$paths[["tilePath"]], prjDir),
    file.path(.getRelativePath(config.bc$paths[["outputPath"]], prjDir), "tiles")
  )

  ## mode postprocess ------------------------------------------------------------------------------
  config.bc$context$mode <- "postprocess"
  config.bc$update()$validate()

  ## context
  expect_equal(config.bc$context[["runName"]],
               "multiple_LUs_n04_d6b6441dd2d0e467_ECODISTRICT_res125", ignore_attr = TRUE)

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
  expect_identical(.isAbsolutePath(unlist(config.bc$paths)),
                   c(cachePath = FALSE, inputPath = FALSE, logPath = FALSE,
                     modulePath1 = FALSE, modulePath2 = FALSE, outputPath = FALSE,
                     projectPath = TRUE, scratchPath = TRUE, tilePath = FALSE))
  expect_identical(
    .getRelativePath(config.bc$paths[["outputPath"]], prjDir),
    file.path("outputs", "multiple_LUs_n04_d6b6441dd2d0e467_ECODISTRICT_res125")
  )
  expect_identical(
    .getRelativePath(config.bc$paths[["logPath"]], prjDir),
    file.path(.getRelativePath(config.bc$paths[["outputPath"]], prjDir), "log")
  )
  expect_identical(
    .getRelativePath(config.bc$paths[["tilePath"]], prjDir),
    file.path(.getRelativePath(config.bc$paths[["outputPath"]], prjDir), "tiles")
  )

  rm(config.bc)
})
