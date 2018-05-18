context("trickypdf")

test_that("jitter", {
  austria_protocol <- system.file(package = "trickypdf", "extdata", "pdf", "AT_XXIII_009.pdf")
  P <- PDF$new(austria_protocol)
  P$add_box(box = c(top = 78, height = 700, left = 72, width = 450), page = NULL, replace = TRUE)
  P$jitter <- 4
  P$get_text_from_boxes(paragraphs = FALSE)
  expect_equal(any(grepl("^\\s*Pr.sidentin\\s+Mag\\.\\s+Barbara\\s+Prammer:\\s+Hohes\\s+Haus!.*$", P$pages[[1]][[1]])), TRUE)
  expect_equal(any(grepl("Silvestertag\\s+ist\\s+Frau\\s+Bundesministerin\\s+Liese\\s+Prokop\\s+f.r\\s+alle\\s+vollkommen\\s+", P$pages[[1]][[1]])), TRUE)
})
