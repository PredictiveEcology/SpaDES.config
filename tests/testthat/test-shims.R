test_that("getModule shim is compatible with original function", {
  ## failure indicates major change in SpaDES.project::getModule()
  expect_true(shim_get_module_compatible())
})
