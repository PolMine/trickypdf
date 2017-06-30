library(trickypdf)

testthat::context("testing trickypdf")

test_that("", {
  expect_equal(
    {
      x <- c("This is a sample text. We freq-", "quently encounter issues with bro-", "ken lines.")
      y <- restore_paragraphs(x)
      length(y)
    }, 1
    )
})

