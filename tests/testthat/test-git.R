test_that("git repository can be deduced", {
  skip_if_not(isTRUE(as.logical(Sys.getenv("CI", FALSE))), "Skipping when running local")
  remote <- get_git_remote()
  expect_equal(remote$host, "https://github.com")
  expect_equal(remote$repository, "r-lib/producethis")
})
