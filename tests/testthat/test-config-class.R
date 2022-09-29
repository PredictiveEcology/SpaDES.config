test_that("LandWeb config + context setup is working", {
  skip("Run manually with latest version of LandWeb Project on test machine")

  prjDir <- "~/GitHub/LandWeb"

  config <- landwebConfig$new()
  config$update(
    paths = list(
      projectPath = prjDir
    )
  )
  config$validate()

  ## landweb ---------------------------------------------------------------------------------------
  context.lw <- useContext("LandWeb", mode = "development", studyAreaName = "LandWeb", version = 3)
  config.lw <- updateLandWebConfig(config, context = context.lw)

  expect_equal(config.lw$args$delayStart, 0L)

  expect_identical(config.lw$params$.globals$.studyAreaName, "LandWeb_v3")
  expect_identical(config.lw$params$Biomass_speciesData$types, c("KNN", "CASFRI", "Pickell", "ForestInventory"))

  expect_identical(
    .getRelativePath(config.lw$paths$tilePath, prjDir),
    file.path("outputs", "LandWeb_v3", "rep01", "tiles")
  )

  ## manitoba --------------------------------------------------------------------------------------
  context.mb <- useContext("LandWeb", mode = "production", studyAreaName = "provMB", version = 3)
  config.mb <- updateLandWebConfig(config, context = context.mb)

  expect_gt(config.mb$args$delayStart, 0L)

  expect_identical(config.mb$params$.globals$.studyAreaName, "provMB_v3")
  expect_identical(config.mb$params$Biomass_speciesData$types, c("KNN", "CASFRI", "Pickell", "MBFRI"))

  expect_identical(
    .getRelativePath(config.mb$paths$tilePath, prjDir),
    file.path("outputs", "provMB_v3", "rep01", "tiles")
  )

  ## postprocess Tolko_AB_N (v2) -------------------------------------------------------------------
  context.pp.tolko <- useContext("LandWeb", mode = "postprocess", studyAreaName = "Tolko_AB_N", version = 2)
  config.pp.tolko <- suppressWarnings({
    updateLandWebConfig(config, context = context.pp.tolko)
  }) ## TODO: fix upstream?

  expect_equal(config.pp.tolko$args$delayStart, 0L)

  pp_mods <- list("LandWeb_preamble", "Biomass_speciesData", "LandWeb_summary")
  names(pp_mods) <- pp_mods
  expect_identical(config.pp.tolko$modules, pp_mods) ## TODO: faling - not updating correctly

  expect_identical(config.pp.tolko$params$.globals$.studyAreaName, "Tolko_AB_N")
  expect_identical(config.pp.tolko$params$Biomass_speciesData$types, c("KNN", "CASFRI", "Pickell", "ForestInventory"))

  expect_identical(
    .getRelativePath(config.pp.tolko$paths$tilePath, prjDir),
    file.path("outputs", "Tolko_AB_N_highDispersal_logROS", "tiles")
  )

  ## cleanup ---------------------------------------------------------------------------------------
  rm(config, config.lw, config.mb, config.pp.tolko)
})
