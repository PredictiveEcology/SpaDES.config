test_that("LandR-fireSense config + context setup is working", {
  skip("Run manually with latest version of BC_HRV Project on test machine.")

  ## project: Ontario_AOU_ROF ----------------------------------------------------------------------
  prjDir <- "~/GitHub/Ontario_AOU_ROF"

  ## NOTE: user expected to add their preamble module to their project config
  pr_mods <- list("Biomass_borealDataPrep", "Biomass_core", "Biomass_regeneration",
                  "Biomass_speciesData", "Biomass_speciesFactorial", "Biomass_speciesParameters",
                  "canClimateData", "fireSense", "fireSense_dataPrepFit",
                  "fireSense_dataPrepPredict", "fireSense_EscapeFit", "fireSense_EscapePredict",
                  "fireSense_IgnitionFit", "fireSense_IgnitionPredict", "fireSense_SpreadFit",
                  "fireSense_SpreadPredict", "gmcsDataPrep")
  names(pr_mods) <- pr_mods
  dv_mods <- pr_mods
  pp_mods <- list("Biomass_summary", "fireSense_summary")
  names(pp_mods) <- pp_mods

  .studyAreaName <- "ON_AOU_6.2"
  config.landrfs <- suppressWarnings({
    useConfig(projectName = "LandRfS", projectPath = prjDir,
              climateGCM = "CanESM5", climateSSP = 370,
              mode = "development", rep = 1L, res = 250, studyAreaName = .studyAreaName)
  })

  ## context
  expect_equal(config.landrfs$context[["runName"]],
               "ON_AOU_6.2_CanESM5_SSP370_rep01", ignore_attr = TRUE)

  ## args
  expect_equal(config.landrfs$args[["delayStart"]], 0L)
  expect_equal(config.landrfs$args[["simYears"]], list(start = 2011, end = 2100))

  ## modules
  expect_identical(config.landrfs$modules, pr_mods)

  ## options
  expect_equal(config.landrfs$options[["reproducible.destinationPath"]], config.landrfs$paths[["inputPath"]], ignore_attr = TRUE)

  ## params
  expect_identical(names(config.landrfs$params), c(".globals", names(config.landrfs$modules)))
  expect_identical(config.landrfs$params$.globals$.studyAreaName, "ON_AOU_6.2")
  expect_identical(config.landrfs$params$Biomass_borealDataPrep$.studyAreaName, config.landrfs$params$.globals$.studyAreaName)
  expect_identical(config.landrfs$params$Biomass_borealDataPrep$.plots, config.landrfs$params$.globals$.plots)
  expect_identical(config.landrfs$params$Biomass_speciesData$types, "KNN")

  ## paths
  expect_identical(.isAbsolutePath(unlist(config.landrfs$paths)),
                   c(cachePath = FALSE, inputPath = FALSE, logPath = FALSE,
                     modulePath = FALSE, outputPath = FALSE,
                     projectPath = TRUE, scratchPath = TRUE, tilePath = FALSE))
  expect_identical(
    .getRelativePath(config.landrfs$paths[["outputPath"]], prjDir),
    file.path("outputs", "ON_AOU_6.2_CanESM5_SSP370", "rep01")
  )
  expect_identical(
    .getRelativePath(config.landrfs$paths[["logPath"]], prjDir),
    file.path(.getRelativePath(config.landrfs$paths[["outputPath"]], prjDir), "log")
  )
  expect_identical(
    .getRelativePath(config.landrfs$paths[["tilePath"]], prjDir),
    file.path(.getRelativePath(config.landrfs$paths[["outputPath"]], prjDir), "tiles")
  )

  ## fire regime polygon types ---------------------------------------------------------------------
  config.landrfs$context$climateSSP <- 585
  config.landrfs$update()$validate()

  ## context
  expect_equal(config.landrfs$context[["runName"]], "ON_AOU_6.2_CanESM5_SSP585_rep01", ignore_attr = TRUE)

  ## args
  ##

  ## modules
  ##

  ## options
  ##

  ## params
  ##

  ## paths
  expect_identical(.isAbsolutePath(unlist(config.landrfs$paths)),
                   c(cachePath = FALSE, inputPath = FALSE, logPath = FALSE,
                     modulePath = FALSE, outputPath = FALSE,
                     projectPath = TRUE, scratchPath = TRUE, tilePath = FALSE))
  expect_identical(
    .getRelativePath(config.landrfs$paths[["outputPath"]], prjDir),
    file.path("outputs", "ON_AOU_6.2_CanESM5_SSP585", "rep01")
  )
  expect_identical(
    .getRelativePath(config.landrfs$paths[["logPath"]], prjDir),
    file.path(.getRelativePath(config.landrfs$paths[["outputPath"]], prjDir), "log")
  )
  expect_identical(
    .getRelativePath(config.landrfs$paths[["tilePath"]], prjDir),
    file.path(.getRelativePath(config.landrfs$paths[["outputPath"]], prjDir), "tiles")
  )

  ## mode postprocess ------------------------------------------------------------------------------
  config.landrfs$context$mode <- "postprocess"
  config.landrfs$update()$validate()

  ## context
  expect_equal(config.landrfs$context[["runName"]], "ON_AOU_6.2_CanESM5_SSP585", ignore_attr = TRUE)

  ## args
  expect_equal(config.landrfs$args[["delayStart"]], 0L)

  ## modules
  expect_identical(config.landrfs$modules, pp_mods)

  ## options
  ##

  ## params
  ##

  ## paths
  expect_identical(.isAbsolutePath(unlist(config.landrfs$paths)),
                   c(cachePath = FALSE, inputPath = FALSE, logPath = FALSE,
                     modulePath = FALSE, outputPath = FALSE,
                     projectPath = TRUE, scratchPath = TRUE, tilePath = FALSE))
  expect_identical(
    .getRelativePath(config.landrfs$paths[["outputPath"]], prjDir),
    file.path("outputs", "ON_AOU_6.2_CanESM5_SSP585")
  )
  expect_identical(
    .getRelativePath(config.landrfs$paths[["logPath"]], prjDir),
    file.path(.getRelativePath(config.landrfs$paths[["outputPath"]], prjDir), "log")
  )
  expect_identical(
    .getRelativePath(config.landrfs$paths[["tilePath"]], prjDir),
    file.path(.getRelativePath(config.landrfs$paths[["outputPath"]], prjDir), "tiles")
  )
  rm(config.landrfs)
})
