test_that("generic config + context setup is working", {
  skip("run manually")

  ## project: default ------------------------------------------------------------------------------
  foo <- useContext("fooProj")
  info <- printRunInfo(foo)
  foo[["studyAreaName"]] <- "BCR6_NT1"
  expect_identical(foo[["runName"]], "BCR6_NT1_rep01")
  foo[["rep"]] <- 10
  expect_identical(foo[["rep"]], 10L)
  expect_identical(foo[["runName"]], "BCR6_NT1_rep10")

  rm(foo, info)

  ## TODO: test generic config
})
