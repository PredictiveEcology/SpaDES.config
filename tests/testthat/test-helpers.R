test_that("working with relative paths behaves sensibly", {
  paths1 <- c(
    "outputs",
    "~/GitHub/LandWeb/outputs",
    "/home/achubaty/Documents/GitHub/LandWeb/outputs",
    "/mnt/projects/HRV/LandWeb/outputs"
  )

  paths2 <- c(
    ".",
    "~/GitHub/LandWeb",
    "/home/achubaty/Documents/GitHub/LandWeb",
    "/home/achubaty/Documents/GitHub/LandWeb"
  )

  expect_identical(length(paths1), length(paths2))

  relPaths <- .getRelativePath(paths1, paths2)
  expect_true(all(relPaths == "outputs"))

  newRelPaths <- .updateRelativePath(paths1, paths2)
  expect_true(all(newRelPaths == "outputs"))

  ## ----------------------------------------------------------------

  paths3 <- c(
    "outputs/LandWeb_v3/rep01/tiles",
    "~/GitHub/LandWeb/outputs/LandWeb_v3/rep01/tiles",
    "/home/achubaty/Documents/GitHub/LandWeb/outputs/LandWeb_v3/rep01/tiles",
    "/mnt/projects/HRV/LandWeb/outputs/LandWeb_v3/rep01/tiles"
  )

  paths4 <- c(
    ".",
    "~/GitHub/LandWeb",
    "/home/achubaty/Documents/GitHub/LandWeb",
    "/home/achubaty/Documents/GitHub/LandWeb"
  )

  expect_identical(length(paths3), length(paths4))

  relPaths2 <- .getRelativePath(paths3, paths4)
  expect_true(all(relPaths2 == "outputs/LandWeb_v3/rep01/tiles"))

  newRelPaths2 <- .updateRelativePath(paths3, paths4)
  expect_true(all(newRelPaths2 == "outputs/LandWeb_v3/rep01/tiles"))
})
