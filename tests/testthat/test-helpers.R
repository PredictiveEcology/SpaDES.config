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

  relPaths <- character(length(paths1))
  for (i in 1:length(paths1)) {
    relPaths[i] <- .getRelativePath(paths1[i], paths2[i])
  }

  expect_true(all(relPaths == "outputs"))

  newRelPaths <- character(length(paths1))
  for (i in 1:length(paths1)) {
    newRelPaths[i] <- .updateRelativePath(paths1[i], paths2[i])
  }

  expect_true(all(newRelPaths == file.path(paths2, "outputs")))

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

  relPaths2 <- character(length(paths3))
  for (i in 1:length(paths3)) {
    relPaths2[i] <- .getRelativePath(paths3[i], paths4[i])
  }

  expect_true(all(relPaths2 == "outputs/LandWeb_v3/rep01/tiles"))

  newRelPaths2 <- character(length(paths3))
  for (i in 1:length(paths3)) {
    newRelPaths2[i] <- .updateRelativePath(paths3[i], paths4[i])
  }

  expect_true(all(newRelPaths2 == file.path(paths4, "outputs/LandWeb_v3/rep01/tiles")))
})
