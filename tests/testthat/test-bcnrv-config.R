test_that("BC NRV config + context setup is working", {
  skip("Run manually with latest version of BC_HRV Project on test machine.")

  ## project: BC_HRV ------------------------------------------------------------------------------
  prjDir <- "~/GitHub/BC_HRV"

  pr_mods <- list("ageModule", "BC_HRV_preamble", "Biomass_borealDataPrep", "Biomass_core",
                  "Biomass_regeneration", "Biomass_speciesData", "LandWeb_output",
                  "scfmDriver", "scfmEscape", "scfmIgnition", "scfmLandcoverInit",
                  "scfmRegime", "scfmSpread", "timeSinceFire")
  names(pr_mods) <- pr_mods
  dv_mods <- pr_mods
  pp_mods <- list("BC_HRV_preamble", "Biomass_speciesData", "LandWeb_summary", "NRV_summary")
  names(pp_mods) <- pp_mods

  .studyAreaName <- c("Corkscrew", "Christenson Creek", "Downton", "Punky Moore")
  config.bc <- suppressWarnings({
    useConfig(projectName = "BC_NRV", projectPath = prjDir,
              mode = "development", rep = 1L, studyAreaName = .studyAreaName)
  }) ## TODO: spurious warning re: unused module LandWeb_summary

  ## context
  expect_equal(config.bc$context[["runName"]], "multiple_LUs_n04_d6b6441dd2d0e467_res125_rep01", ignore_attr = TRUE)

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

  ## paths
  expect_identical(.isAbsolutePath(unlist(config.bc$paths)),
                   c(cachePath = FALSE, inputPath = FALSE, logPath = FALSE,
                     modulePath1 = FALSE, modulePath2 = FALSE, outputPath = FALSE,
                     projectPath = TRUE, scratchPath = TRUE, tilePath = FALSE))
  expect_identical(
    .getRelativePath(config.bc$paths[["logPath"]], prjDir),
    file.path("outputs", "multiple_LUs_n04_d6b6441dd2d0e467_res125", "rep01", "log")
  )
  expect_identical(
    .getRelativePath(config.bc$paths[["tilePath"]], prjDir),
    file.path("outputs", "multiple_LUs_n04_d6b6441dd2d0e467_res125", "rep01", "tiles")
  )

  ## mode prostprocess -----------------------------------------------------------------------------
  config.bc$context$mode <- "postprocess"
  config.bc$update()$validate()

  ## context
  expect_equal(config.bc$context[["runName"]], "multiple_LUs_n04_d6b6441dd2d0e467_res125", ignore_attr = TRUE)

  ## args
  expect_equal(config.bc$args[["delayStart"]], 0L)

  ## modules
  expect_identical(config.bc$modules, pp_mods)

  ## options
  ##

  ## params
  ##

  ## paths
  ##

  rm(config.bc)
})
