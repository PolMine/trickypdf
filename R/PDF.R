setOldClass("xml_document")
setOldClass("html")

#' Convert pdf document to plain text/XML.
#' 
#' To get the margins of the text: Use Rechteckige Auswahl, and Werkzeuge/Allgemeine Informationen
#' 
#' @field filename_pdf a character vector (length 1) providing a filename
#' @field first first integer, the first page
#' @field last last integer, the last page
#' @field importFrom xml2 read_xml xml_find_all xml_attrs
#' @field xml parsed xml
#' @field text a list of character vectors
#' @field jitter integer, the deviation in lines that will be checked
#' @field margin a named integer vector ("top", "bottom", "left", "right") indicating the margins of the text
#' @field deviation allowed deviation of columns from page center
#' @field xmlification xml to output
#' @field no_pages number of pages of the pdf document (after pdf2xml)
#' @param filename_pdf path to a pdf document
#' @param first first page of the pdf document to be included
#' @param last last page of the pdf document to be included
#' @param jitter points up and down for reconstructing tilted lines
#' @param deviation points that lines may deviate from middle of page
#' @param margins named vector (top, bottom, left, right)
#' @param root name of the root node
#' @param metadata named character vector, attribtutes of root node of output xml document
#' @param filename character vector
#' @param viewer the viewer to use to inspect pdf or html documents
#' @importFrom xml2 xml_find_all write_xml xml_attrs xml_set_attrs xml_add_child xml_new_root xml_text xml_replace
#' @importFrom pbapply pblapply
#' @importFrom methods setRefClass new
#' @importFrom htmltools HTML html_print
#' @importFrom Rpoppler PDF_info
#' @importFrom stringi stri_extract_all
#' @importFrom markdown markdownToHTML
#' @importFrom plyr ldply
#' @rdname PDF-class
#' @name PDF
#' @export PDF
#' @examples
#' \dontrun{
#' # Basic scenario: A straight-forward pdf without columns;
#' # it's only page numbers and text on the margins that disturbs
#' 
#' cdu_pdf <- system.file(package = "trickypdf", "extdata", "pdf", "cdu.pdf")
#' P <- PDF$new(filename_pdf = cdu_pdf, first = 7, last = 119)
#' # P$show_pdf()
#' P$add_box(box = c(top = 75, height = 700, left = 44, width = 500))
#' P$remove_unboxed_text_from_all_pages()
#' P$get_text_from_pages()
#' P$purge()
#' P$xmlify()
#' P$xml2html()
#' P$browse()
#' output <- tempfile(fileext = ".xml")
#' P$write(filename = output)
#' 
#' 
#' # Advanced scenario I: Get text from pdf with columns, here: define boxes
#' 
#' doc <- system.file(package = "trickypdf", "extdata", "pdf", "UN_GeneralAssembly_2016.pdf")
#' UN <- PDF$new(filename_pdf = doc)
#' # UN$show_pdf()
#' UN$add_box(page = 1, box = c(top = 380, height = 250, left = 52, width = 255))
#' UN$add_box(page = 1, box = c(top = 232, height = 400, left = 303, width = 255), replace = FALSE)
#' UN$add_box(page = 2, box = c(top = 80, height = 595, left = 52, width = 255))
#' UN$add_box(page = 2, box = c(top = 80, height = 595, left = 303, width = 255), replace = FALSE)
#' UN$get_text_from_boxes(paragraphs = TRUE)
#' UN$xmlify()
#' UN$xml2html()
#' UN$browse()
#' 
#' # Advanced scenario II: Get text from pdf with columns, long version
#' 
#' plenaryprotocol <- system.file(package = "trickypdf", "extdata", "pdf", "18238.pdf")
#' P <- PDF$new(filename_pdf = plenaryprotocol, first = 5, last = 73)
#' # P$show_pdf()
#' P$add_box(c(left = 58, width = 480, top = 70, height = 705))
#' P$remove_unboxed_text_from_all_pages()
#' P$deviation <- 10L
#' P$decolumnize()
#' P$get_text_from_pages()
#' P$purge()
#' P$xmlify()
#' P$xml2html()
#' P$browse()
#' }
PDF <- setRefClass(
  
  "PDF",
  
  fields = list(
    filename_pdf = "character",
    first = "integer",
    last = "integer",
    xml = "xml_document",
    jitter = "numeric",
    pages = "list",
    no_pages = "integer",
    margins = "integer",
    deviation = "integer",
    xmlification = "xml_document",
    pagesizes = "data.frame",
    markdown = "character",
    html = "html",
    boxes = "data.frame"
  ),
  
  methods = list(
    
    initialize = function(filename_pdf, first = NA, last = NA, jitter = 2, deviation = 10L, margins = integer()){
      
      "Initialize a new instance of the class PDF to process a pdf document."
      
      if (length(filename_pdf) > 1){
        stop("Length of argument 'filename_pdf' > 1: The class can only process one pdf document at a time.")
      }
      .self$filename_pdf <- filename_pdf
      
      stopifnot(
        length(first) == 1,
        length(last) == 1,
        length(jitter) == 1
        )
      .self$first <- if (is.na(first)) 1L else as.integer(first)
      .self$last <- if (is.na(last)) as.integer(PDF_info(filename_pdf)$Pages) else as.integer(last)
      .self$jitter <- jitter
      .self$margins <- margins
      .self$boxes <- data.frame(
        page = integer(), left = numeric(), top = numeric(), width = numeric(),
        height = numeric(), bottom = numeric(), right = numeric()
      )
      .self$pdf2xml()
      .self$get_pagesizes()
      .self$add_box(box = NULL, page = NULL)
    },
    
    show_pdf = function(){
      
      "Show the pdf document."
      
      browseURL(.self$filename_pdf)
    },
    
    pdf2xml = function(){
      
      "Turn pdf document into a raw XML document that will be kept in the field 'xml'."
      
      cmd <- c(
        "pdftohtml",
        "-xml", # output for XML post-processing
        "-hidden", # output hidden text
        if (!is.na(.self$first)) c("-f", .self$first) else NA,
        if (!is.na(.self$last)) c("-l", .self$last) else NA,
        "-q", # don't print any messages or errors
        "-stdout", # use standard output
        "-i", # ignore images
        .self$filename_pdf
        )
      if (any(is.na(cmd))) cmd <- cmd[-which(is.na(cmd))]
      cmd <- paste(cmd, collapse = " ")
      xmlChar <- system(cmd, intern = TRUE)
      .self$xml <- xml2::read_xml(x = paste(xmlChar, collapse = "\n"))
      .self$no_pages <- length(xml_find_all(.self$xml, xpath = "/pdf2xml/page"))
    },
    
    make_box = function(box = NULL, page){
      
      "Generate a box for the data.frame in the field 'boxes'. Coordinates a assumed to
      be in points and are recalibrated into pdf units."
      
      if (is.null(box)){
        newBox <- data.frame(
          page = page, left = 0, top = 0,
          width = .self$pagesizes[page, "width"], height = .self$pagesizes[page, "height"],
          bottom = .self$pagesizes[page, "height"], right = .self$pagesizes[page, "width"]
        )
      } else {
        stopifnot(all(c("left", "top", "width", "height") %in% names(box)))
        
        horizontal <- .self$pagesizes[page, "width"] / .self$pagesizes[page, "width.pts"]
        vertical <- .self$pagesizes[page, "height"] / .self$pagesizes[page, "height.pts"]
        
        newBox <- data.frame(
          page = page,
          left = box[["left"]] * horizontal,
          top = box[["top"]] * vertical,
          width = box[["width"]] * horizontal,
          height = box[["height"]] * vertical,
          bottom = (box[["top"]] + box[["height"]]) * vertical,
          right = (box[["left"]] + box[["width"]]) * horizontal
        )
      }
      newBox
    },
    
    add_box = function(box = NULL, page = NULL, replace = TRUE){
      
      "Add a box that will serve as a crop box."
      
      if (is.null(page)){
        boxList <- lapply(
          1:nrow(.self$pagesizes),
          function(i) .self$make_box(box = box, page = i)
        )
        newBoxDataFrame <- as.data.frame(do.call(rbind, boxList))
        if (replace == TRUE){
          .self$boxes <- newBoxDataFrame
        } else {
          .self$boxes <- rbind(.self$boxes, newBoxDataFrame)
          .self$boxes <- .self$boxes[order(.self$boxes[["page"]])]
        }
      } else {
        for (i in page){
          newBox <- .self$make_box(box = box, page = page)
          if (replace == TRUE){
            .self$boxes <- .self$boxes[-which(.self$boxes[["page"]] == i),]
            .self$boxes <- rbind(.self$boxes, newBox)
            .self$boxes <- .self$boxes[order(.self$boxes[["page"]]),]
          } else {
            .self$boxes <- rbind(.self$boxes, newBox)
            .self$boxes <- .self$boxes[order(.self$boxes[["page"]]),]
          }
        }
      }
      invisible(.self$boxes)
    },
    
    drop_unboxed_text_nodes = function(node, boxes, copy = FALSE){
      
      "Remove any nodes that are not within defined boxes."
      
      if (copy){
        node_returned <- xml_new_root(node, .copy = TRUE)
      } else {
        node_returned <- node
      }
      lapply(
        xml2::xml_find_all(node_returned, xpath = "//text"),
        function(text_node){
          position <- xml2::xml_attrs(text_node)
          position <- setNames(as.integer(position), names(position))
          boxed <- sapply(
            boxes,
            function(box){
              if (
                position["top"] > box["top"] && position["top"] < box["bottom"]
                && position["left"] > box["left"] && position["left"] < box["right"]
              ) {
                TRUE
              } else {
                FALSE
              }
            }
          )
          if (any(boxed) == FALSE) xml2::xml_remove(text_node) # if not in any of the boxes: remove textNode
        }
      )
      node_returned
    },
    
    remove_unboxed_text_from_all_pages = function(){
      
      "Remove anything that is printed on pages beyond the defined boxes."
      
      page_nodes <- xml2::xml_find_all(.self$xml, xpath = "/pdf2xml/page")
      pblapply(
        1:length(page_nodes), # iterate through pages
        function(i){
          boxesDataFrame <- .self$boxes[which(.self$boxes[["page"]] == i),]
          boxList <- lapply(1:nrow(boxesDataFrame), function(j) as.vector(boxesDataFrame[j,]))
          node_new <- .self$drop_unboxed_text_nodes(node = page_nodes[[i]], boxes = boxList, copy = TRUE)
          xml_replace(.x = page_nodes[[i]], .value = node_new)
        }
      )
      invisible()
    },
    
    decolumnize = function(){
      
      "Remove columnization, if pages are typeset with two columns. Muli-column layouts
      with three or more columns are not supported so far. The procedure adjusts the
      coordinates of text right of the the horizontal page center, i.e. the page height
      is added to the top position, and half of the page width substracted from the
      left position."
      
      pageNodes <- xml2::xml_find_all(.self$xml, xpath = "/pdf2xml/page")
      lapply(
        pageNodes, # iterate through pages
        function(page){
          pageNodeAttrs <- xml2::xml_attrs(page)[c("height", "width")]
          pageNodeAttrs <- setNames(as.numeric(pageNodeAttrs), names(pageNodeAttrs))
          
          pageCenter <- floor(pageNodeAttrs["width"] / 2)
          pageHeight <- as.numeric(pageNodeAttrs["height"])

          textNodes <- xml2::xml_find_all(page, xpath = "./text")
          lapply(
            textNodes, # iterate through text nodes
            function(textNode){
              position <- xml2::xml_attrs(textNode)
              position <- setNames(as.integer(position), names(position))
              if (as.numeric(position["left"]) > pageCenter ){
                xml2::xml_attrs(textNode)["left"] <- position["left"] - pageCenter
                xml2::xml_attrs(textNode)["top"] <- position["top"] + pageHeight
              }
            }
          )
        }
      )
      invisible()
    },
    
    get_pagesizes = function(){
      
      "Get page width and height (points/pts and pdf units). The pdf units are extracted from
      the xmlified pdf document. To get sizes in points (pts), PDF_info (package Rpoppler) is
      used. The result is a data.frame in the field pagesizes. The method is called when parsing
      the pdf document."
      
      # get width and height (pdf units)
      xmlAttrsPages <- lapply(xml2::xml_find_all(.self$xml, xpath = "/pdf2xml/page"), xml_attrs)
      .self$pagesizes <- as.data.frame(do.call(rbind, xmlAttrsPages), stringsAsFactors = FALSE)
      colnames(.self$pagesizes)[which(colnames(.self$pagesizes) == "number")] <- "page"
      for (what in c("page", "top", "left", "height", "width")){
        .self$pagesizes[[what]] <- as.integer(.self$pagesizes[[what]])
      }
      
      sizes <- stri_extract_all(str = Rpoppler::PDF_info(.self$filename_pdf)$Sizes, regex = "\\d+(\\.\\d+|)")
      sizes.pts <- do.call(rbind, lapply(sizes, as.numeric))[.self$first:.self$last, 1:2]
      colnames(sizes.pts) <- c("width.pts", "height.pts")
      .self$pagesizes <- cbind(.self$pagesizes, sizes.pts)
      invisible(.self$pagesizes)
    },
    
    get_text = function(node, paragraphs = TRUE){
      text_nodes <- xml2::xml_find_all(node, xpath = "./text")
      txt <- character()
      txtPosition <- integer()
      counter <- 1L
      for (i in 1:length(text_nodes)){
        if (i == 1){
          txt[1] <- xml2::xml_text(text_nodes[[i]])
          txtPosition[1] <- as.integer(xml2::xml_attrs(text_nodes[[i]])["top"])
        } else if ( i > 1 ){
          topCurrentNode <- as.integer(xml2::xml_attrs(text_nodes[[i]])["top"])
          topPreviousNode <- as.integer(xml2::xml_attrs(text_nodes[[i-1]])["top"])
          if (abs(topCurrentNode - topPreviousNode) > .self$jitter) {
            counter <- counter + 1L
            txt[counter] <- xml2::xml_text(text_nodes[[i]])
            txtPosition[counter] <- topCurrentNode
          } else {
            txt[counter] <- paste(txt[counter], xml2::xml_text(text_nodes[[i]]), sep = "")
          }
        }
      }
      txt <- txt[order(txtPosition)] # if order of text nodes is screwed up
      if (paragraphs) txt <- restore_paragraphs(txt)
      
    },
    
    get_text_from_pages = function(paragraphs = TRUE){
      
      "Extract text from pages."
      
      .self$pages <- lapply(
        xml2::xml_find_all(.self$xml, xpath = "/pdf2xml/page"),
        function(page) .self$get_text(page, paragraphs = paragraphs)
      )
    },
    
    get_text_from_boxes = function(paragraphs){
      
      "Iterate through pages, and extract text as defined by boxes from pages. The result
      will be assigned to field 'pages'."
      
      page_nodes <- xml2::xml_find_all(.self$xml, xpath = "/pdf2xml/page")
      .self$pages <- dlply(
        .data = .self$boxes, .variables = .(page), # create sub-data.frames for bages
        .fun = function(df){
          df[["box_id"]] <- 1:nrow(df)
          page_no <- unique(df[["page"]])
          dlply(
            .data = df, .variables = .(box_id),
            .fun = function(df2){
              page_node <- xml_new_root(page_nodes[[ df2[["page"]] ]], .copy = TRUE)
              box_list <- lapply(
                1:nrow(df2),
                function(j) setNames(as.numeric(df2[j,]), colnames(df2))
                )
              node_page <- .self$drop_unboxed_text_nodes(node = page_node, box_list, copy = TRUE)
              .self$get_text(node_page, paragraphs = paragraphs)
            }
          )
        }
      )
    },
    
    find = function(regex){
      
      "Find matches for regex on pages. The method returns the pages with at least one
      match for the regex."
      
      page_nodes <- xml2::xml_find_all(.self$xml, xpath = "/pdf2xml/page")
      matching <- lapply(
        1:length(page_nodes), # iterate through pages
        function(i){
          match_logical <- sapply(
            xml2::xml_find_all(page_nodes[[i]], xpath = "./text"),
            function(text_node) grepl(regex, xml_text(text_node))
          )
          any(match_logical)
        }
      )
      which(matching == TRUE)
    },
    
    reorder = function(){
      
      "Reorder text nodes on a page. Not yet functional!"
      
      .self$xml <- pblapply(
        xml_find_all(.self$xmlification, xpath = "/document/page"),
        function(page){
          
        }
      )
    },
    
    cut = function(){
      
      "Cut the document until a match with a regex occurs. Not yet working!"
      
    },
    
    concatenate = function(){
      
      "Reconstruct paragraphs based on the following heuristic: If a line ends with a hyphen
      and is not stump, lines are concatenated."
      
      if (typeof(.self$pages[[1]]) == "list"){
        # option 1: pages include boxes
        .self$pages <- pblapply(
          .self$pages,
          function(page) lapply(page, restore_paragraphs)
        )
      } else {
        .self$pages <- pblapply(.self$pages, restore_paragraphs)
      }
    },
    
    purge = function(){
      
      "Remove noise, surplus whitespace signs from the text."
      
      if (typeof(.self$pages[[1]]) == "list"){
        .self$pages <- lapply(.self$pages, function(page) lapply(page, broom))
      } else {
        .self$pages <- lapply(.self$pages, broom)
      }
      
    },
    
    xmlify = function(root = "document", metadata = NULL){
      
      "Turn text in the pages field into a XML document."
      
      .self$xmlification <- xml_new_root(.value = root)
      if (!is.null(metadata)){
        if (is.null(names(metadata))){
          stop(
            "If argument metadata is given, it needs to be a named character vector. ",
            "The names need to provide attribute names."
            )
        }
        xml_set_attrs(.self$xmlification, value = metadata)
      }
      dummy <- pblapply(
        .self$pages,
        function(page){
          xml2::xml_add_child(.self$xmlification, .value = "page")
          pageNodes <- xml_find_all(.self$xmlification, xpath = sprintf("/%s/page", root))
          newPageNode <- pageNodes[[length(pageNodes)]]
          if (is.list(page)){
            lapply(
              1:length(page),
              function(i){
                xml2::xml_add_child(newPageNode, .value = "box")
                boxNodes <- xml_find_all(newPageNode, xpath = "./box")
                newBoxNode <- boxNodes[[length(boxNodes)]]
                xml_set_attr(newBoxNode, attr = "n", value = i)
                lapply(
                  page[[i]],
                  function(paragraph) xml_add_child(newBoxNode, .value = "p", paragraph)
                )
              }
            )
          } else {
            lapply(
              page,
              function(paragraph) xml_add_child(newPageNode, .value = "p", paragraph)
            )
          }
          NULL
        }
      )
      invisible(.self$xmlification)
    },
    
    xml2md = function(){
      
      "Turn xmlified document into markdown (will be stored in field 'markdown')."
      
      if (length(xml_find_all(.self$xmlification, xpath = "/document/page/box")) > 0){
        boxed <- TRUE
      } else {
        boxed <- FALSE
      }
      pagesMarkdown <- lapply(
        xml_find_all(.self$xmlification, xpath = "/document/page"),
        function(page){
          if (boxed == TRUE){
            box_txt <- lapply(
              xml_find_all(page, xpath = "./box"),
              function(box){
                paras <- sapply(xml_find_all(box, xpath = "./p"), function(para) xml_text(para))
                paste(paras, collapse = "\n\n")
              }
            )
            txt <- paste(paste(box_txt, collapse = "\n\n---\n\n"), "\n")
          } else {
            paras <- sapply(
              xml_find_all(page, xpath = "./p"),
              function(paragraph) xml_text(paragraph)
            )
            txt <- paste(paras, collapse = "\n\n")
          }
          txt
        }
      )
      .self$markdown <- paste(paste(pagesMarkdown, collapse = "\n\n* * *\n\n"), "\n")
      invisible(.self$markdown)
    },
    
    md2html = function(){
      
      "Turn markdown (field 'markdown') into html document that will be stored in the field 'html'."
      
      if (length(.self$markdown) == 0) .self$xml2md()
      mdFilename <- tempfile(fileext = ".md")
      htmlFile <- tempfile(fileext = ".html")
      cat(.self$markdown, file = mdFilename)
      markdown::markdownToHTML(file = mdFilename, output = htmlFile)
      htmldoc <- scan(file = htmlFile, what = character(), sep = "\n", quiet = TRUE)
      htmldoc <- paste(htmldoc, collapse = "\n")
      .self$html <- HTML(htmldoc)
      invisible(.self$html)
    },
    
    xml2html = function(){
      
      "Turn xmlification of pdf document into html document."
      
      .self$xml2md()
      .self$md2html()
    },
    
    browse = function(viewer = getOption("viewer", utils::browseURL)){
      
      "Show html document in browser."
      
      if (is.null(.self$html)) .self$md2html()
      htmltools::html_print(.self$html, viewer = viewer)
    },
    
    write = function(filename){
      
      "Save xmlified document (available in the field 'xmlification') to a file."
      
      write_xml(.self$xmlification, file = filename)
    }
  )
)
