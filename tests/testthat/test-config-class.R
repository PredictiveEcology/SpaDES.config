test_that("LandWeb config + context setup is working", {
  skip("Run manually with latest version of LandWeb Project on test machine")

  prjDir <- "~/GitHub/LandWeb"

  config.lw <- landwebConfig$new()
  config.lw$update(
    paths = list(
      projectPath = prjDir
    )
  )
  config.lw$validate()

  context.lw <- useContext("LandWeb", mode = "development", studyAreaName = "LandWeb", version = 3)
  config.lw <- updateLandWebConfig(config.lw, context = context.lw)

  expect_identical(config.lw$args$delayStart, 0L)

  expect_identical(config.lw$params$.globals$.studyAreaName, "LandWeb_v3")

  ## verify paths ----------------------------------------------------------------------------------
  expect_identical(
    .getRelativePath(config.lw$paths$tilePath, prjDir),
    file.path("outputs", "LandWeb_v3", "rep01", "tiles")
  )

  rm(config.lw)
})
