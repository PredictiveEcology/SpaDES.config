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

test_that(".getRelativePath falls back gracefully on edge cases", {
  ## no shared component: previously `max(which(a %in% b))` returned -Inf (warning)
  ## and produced NA-filled garbage; now it falls back to a plain relative path.
  expect_match(
    .getRelativePath("/separate/storage/outputs", "/home/user/myProject"),
    "separate/storage/outputs$"
  )

  ## path equals the reference (deepest shared component is the leaf itself) -> "."
  expect_identical(
    .getRelativePath("/home/user/myProject", "/home/user/myProject"),
    "."
  )
})
