context("trickypdf")

test_that("restore_paragraphs", {
  expect_equal(
    {
      x <- c("This is a sample text. We freq-", "quently encounter issues with bro-", "ken lines.")
      y <- restore_paragraphs(x)
      length(y)
    },
    1
    )
})


test_that("PDF", {
  expect_equal(
    {
      cdu_pdf <- system.file(package = "trickypdf", "extdata", "pdf", "cdu.pdf")
      P <- PDF$new(filename_pdf = cdu_pdf, first = 7, last = 119)
      P$add_box(box = c(top = 75, height = 700, left = 44, width = 500))
      P$remove_unboxed_text_from_all_pages()
      P$get_text_from_pages()
      P$purge()
      P$xmlify()
      P$xml2html()
      nchar(as.character(P$html))
    },
    296610
  )
})
