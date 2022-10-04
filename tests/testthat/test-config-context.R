test_that("LandWeb config + context setup is working", {
  skip("Run manually with latest version of LandWeb Project on test machine")

  ## project: default ------------------------------------------------------------------------------
  foo <- useContext("fooProj")
  info <- printRunInfo(foo)
  foo[["studyAreaName"]] <- "BCR6_NT1"
  expect_identical(foo[["runName"]], "BCR6_NT1_rep01")
  foo[["rep"]] <- 10
  expect_identical(foo[["rep"]], 10L)
  expect_identical(foo[["runName"]], "BCR6_NT1_rep10")

  rm(foo, info)

  ## project: landweb ------------------------------------------------------------------------------
  prjDir <- "~/GitHub/LandWeb"

  config.lw <- suppressWarnings({
    useConfig(projectName = "LandWeb", projectPath = prjDir,
              mode = "development", rep = 1L, studyAreaName = "LandWeb", version = 3)
  }) ## TODO: spurious warning re: unused module LandWeb_summary

  ## context
  expect_equal(config.lw$context[["runName"]], "LandWeb_full_v3_rep01", ignore_attr = TRUE)

  ## args
  expect_equal(config.lw$args[["delayStart"]], 0L)

  ## modules
  pr_mods <- list("Biomass_borealDataPrep", "Biomass_core", "Biomass_regeneration",
                  "Biomass_speciesData",  "LandMine", "LandWeb_output", "LandWeb_preamble",
                  "timeSinceFire")
  names(pr_mods) <- pr_mods
  expect_identical(config.lw$modules, pr_mods)

  ## options
  expect_equal(config.lw$options[["map.dataPath"]], config.lw$paths[["inputPath"]], ignore_attr = TRUE)
  expect_equal(config.lw$options[["map.tilePath"]], config.lw$paths[["tilePath"]], ignore_attr = TRUE)
  expect_equal(config.lw$options[["reproducible.destinationPath"]], config.lw$paths[["inputPath"]], ignore_attr = TRUE)

  ## params
  expect_identical(names(config.lw$params), c(".globals", names(config.lw$modules)))
  expect_identical(config.lw$params$.globals$.studyAreaName, "LandWeb_full_v3")
  expect_identical(config.lw$params$Biomass_borealDataPrep$.studyAreaName, config.lw$params$.globals$.studyAreaName)
  expect_identical(config.lw$params$Biomass_borealDataPrep$.plots, config.lw$params$.globals$.plots)
  expect_identical(config.lw$params$Biomass_speciesData$types, c("KNN", "CASFRI", "Pickell", "ForestInventory"))

  ## paths
  expect_identical(.isAbsolutePath(unlist(config.lw$paths)),
                   c(cachePath = FALSE, inputPath = FALSE, modulePath = FALSE, outputPath = FALSE,
                     projectPath = TRUE, scratchPath = TRUE, tilePath = FALSE))
  expect_identical(
    .getRelativePath(config.lw$paths[["tilePath"]], prjDir),
    file.path("outputs", "LandWeb_full_v3", "rep01", "tiles")
  )

  rm(config.lw)

  ## study area: manitoba --------------------------------------------------------------------------
  config.mb <- suppressWarnings({
    useConfig(projectName = "LandWeb", projectPath = prjDir,
              mode = "production", rep = 5, studyAreaName = "provMB", version = 3)
  }) ## TODO: spurious warning re: unused module LandWeb_summary

  ## context
  expect_equal(config.mb$context[["runName"]], "provMB_v3_rep05", ignore_attr = TRUE)

  ## args
  expect_gt(config.mb$args[["delayStart"]], 0L)

  ## modules
  dv_mods <- pr_mods
  expect_identical(config.mb$modules, dv_mods)

  ## options
  expect_equal(config.mb$options[["map.dataPath"]], config.mb$paths[["inputPath"]], ignore_attr = TRUE)
  expect_equal(config.mb$options[["map.tilePath"]], config.mb$paths[["tilePath"]], ignore_attr = TRUE)
  expect_equal(config.mb$options[["reproducible.destinationPath"]], config.mb$paths[["inputPath"]], ignore_attr = TRUE)

  ## params
  expect_identical(config.mb$params[[".globals"]][[".studyAreaName"]], "provMB_v3")
  expect_identical(config.mb$params[["Biomass_speciesData"]][["types"]], c("KNN", "CASFRI", "Pickell", "MBFRI"))

  ## paths
  expect_identical(
    .getRelativePath(config.mb$paths[["tilePath"]], prjDir),
    file.path("outputs", "provMB_v3", "rep05", "tiles")
  )

  rm(config.mb)

  ## study area: Tolko_AB_N (v2) -------------------------------------------------------------------
  ## mode:       postprocess
  config.pp.tolko <- suppressWarnings({
    useConfig(projectName = "LandWeb", projectPath = prjDir,
              mode = "postprocess", rep = NA_integer_, studyAreaName = "Tolko_AB_N", version = 2)
  }) ## TODO: currently warns about params specified for "missing" modules not used in postprocess

  ## context
  expect_equal(config.pp.tolko$context[["runName"]], "Tolko_AB_N_highDispersal_logROS", ignore_attr = TRUE)

  ## args
  expect_equal(config.pp.tolko$args[["delayStart"]], 0L)

  ## modules
  pp_mods <- list("LandWeb_preamble", "Biomass_speciesData", "LandWeb_summary")
  names(pp_mods) <- pp_mods
  expect_identical(config.pp.tolko$modules, pp_mods)

  ## params
  expect_identical(config.pp.tolko$params[[".globals"]][[".studyAreaName"]], "Tolko_AB_N")
  expect_identical(config.pp.tolko$params[["Biomass_speciesData"]][["types"]], c("KNN", "CASFRI", "Pickell", "ForestInventory"))

  expect_identical(
    .getRelativePath(config.pp.tolko$paths[["tilePath"]], prjDir),
    file.path("outputs", "Tolko_AB_N_highDispersal_logROS", "tiles")
  )

  rm(config.pp.tolko)
})
