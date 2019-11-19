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


test_that("one_column_layout", {
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

test_that("two_column_layout_decolumnize", {
  expect_equal(
    {
      plenaryprotocol <- system.file(package = "trickypdf", "extdata", "pdf", "18238.pdf")
      P <- PDF$new(filename_pdf = plenaryprotocol, first = 5, last = 73)
      P$add_box(c(left = 58, width = 480, top = 70, height = 705))
      P$remove_unboxed_text_from_all_pages()
      P$deviation <- 10L
      P$decolumnize()
      P$get_text_from_pages(paragraphs = FALSE)
      P$purge()
      P$xmlify()
      P$xml2html()
      nchar(as.character(P$html))
    },
    422378
  )
})

test_that("two_column_layout_boxes", {
  expect_equal(
    {
      doc <- system.file(package = "trickypdf", "extdata", "pdf", "UN_GeneralAssembly_2016.pdf")
      UN <- PDF$new(filename_pdf = doc)
      UN$add_box(page = 1, box = c(top = 380, height = 250, left = 52, width = 255))
      UN$add_box(page = 1, box = c(top = 232, height = 400, left = 303, width = 255), replace = FALSE)
      UN$add_box(page = 2, box = c(top = 80, height = 595, left = 52, width = 255))
      UN$add_box(page = 2, box = c(top = 80, height = 595, left = 303, width = 255), replace = FALSE)
      UN$get_text_from_boxes(paragraphs = TRUE)
      UN$xmlify()
      UN$xml2html()
      nchar(as.character(UN$html))
      
    },
    8625
  )
})


test_that("one_page_document",{
  expect_equal(
    {
      doc <- system.file(package = "trickypdf", "extdata", "pdf", "unga_one_page.pdf")
      UN <- PDF$new(filename_pdf = doc)
      UN$add_box(page = 1, box = c(top = 240, height = 400, left = 55, width = 250))
      UN$add_box(page = 1, box = c(top = 240, height = 400, left = 308, width = 250), replace = FALSE)
      UN$get_text_from_boxes(paragraphs = TRUE)
      sum(nchar(unname(unlist(UN$pages))))
    },
    840
  )
})

test_that("jitter",{
  expect_equal(
    {
      doc <- system.file(package = "trickypdf", "extdata", "pdf", "N9986515_jitter_test.pdf")
      UN <- PDF$new(filename_pdf = doc)
      UN$add_box(box = c(top = 67, height = 645, left = 55, width = 250), page = NULL, replace = TRUE)
      UN$add_box(box = c(top = 67, height = 645, left = 308, width = 250), page = NULL, replace = FALSE)
      UN$add_box(box = c(top = 250, height = 422, left = 55, width = 250), page = 1, replace = TRUE)
      UN$add_box(box = c(top = 250, height = 422, left = 308, width = 250), page = 1, replace = FALSE)
      UN$get_text_from_boxes(paragraphs = FALSE)
      grep("challenges of mine", unname(unlist(UN$pages)), value = TRUE)
    },
    "the challenges of mine action. Non-governmental"
  )
})
