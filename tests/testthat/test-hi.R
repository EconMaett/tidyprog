test_that(desc = "hi() works", code = {
  expect_output(object = hi(), regexp = "Wow")
  expect_output(object = hi("Hello"), regexp = "Hello")
})
