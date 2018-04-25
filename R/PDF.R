setOldClass("xml_document")
setOldClass("html")


#' Convert pdf document to plain text/XML.
#' 
#' To get the margins of the text: Use Rechteckige Auswahl, and Werkzeuge/Allgemeine Informationen
#' 
#' @field filename_pdf a character vector (length 1) providing a filename
#' @field first first integer, the first page
#' @field last last integer, the last page
#' @field page a specific page number
#' @field importFrom xml2 read_xml xml_find_all xml_attrs
#' @field xml parsed xml
#' @field text a list of character vectors
#' @field jitter integer, the deviation in lines that will be checked
#' @field margin a named integer vector ("top", "bottom", "left", "right") indicating the margins of the text
#' @field deviation allowed deviation of columns from page center
#' @field xmlification xml to output
#' @field no_pages number of pages of the pdf document (after pdf2xml)
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{$new(filename_pdf, first = NA, last = NA, jitter = 2, deviation
#'   = 10L, margins = integer())}}{Initialize a new instance of the class PDF to
#'   process a pdf document.}
#'   \item{\code{$validate()}}{Check all fields for correct content.}
#'   \item{\code{$show_pdf()}}{Show the pdf document.}
#'   \item{\code{$make_box(box = NULL, page)}}{Generate a box for the data.frame
#'   in the field \code{$boxes}. Coordinates a assumed to be in points and are
#'   recalibrated into pdf units.}
#'   \item{\code{$add_box(box = NULL, page = NULL, replace = TRUE)}}{Add a box that will serve as a crop box.}
#'   \item{\code{$drop_unboxed_text_nodes(node, boxes, copy = FALSE)}}{Remove any nodes that are not within defined boxes.}
#'   \item{\code{drop_page(page)}}{drop a page from from the XML of the pdf document}
#'   \item{\code{$remove_unboxed_text_from_all_pages()}}{Remove anything that is printed on pages beyond the defined boxes.}
#'   \item{\code{$decolumnize}}{Remove columnization, if pages are typeset with
#'   two columns. Muli-column layouts with three or more columns are not
#'   supported so far. The procedure adjusts the coordinates of text right of
#'   the the horizontal page center, i.e. the page height is added to the top
#'   position, and half of the page width substracted from the left position.}
#'   \item{\code{$get_pagesizes()}}{Get page width and height (points/pts and
#'   pdf units). The pdf units are extracted from the xmlified pdf document. To
#'   get sizes in points (pts), PDF_info (package Rpoppler) is used. The result
#'   is a data.frame in the field pagesizes. The method is called when parsing
#'   the pdf document.}
#'   \item{\code{$get_text(node, paragraphs = TRUE)}}{Get the text from document
#'   in field 'xml'.}
#'   \item{\code{$get_number_of_pages()}}{get number of pages of XML document of pdf}
#'   \item{\code{$get_text_from_pages(paragraphs = TRUE)}}{}
#'   \item{\code{$get_text_from_boxes(paragraphs)}}{Iterate through pages, and
#'   extract text as defined by boxes from pages. The result will be assigned to
#'   field \code{pages}.}
#'   \item{\code{regex}}{Find matches for regex on pages. The method returns the
#'   pages with at least one match for the regex.}
#'   \item{\code{$reorder()}}{Reorder text nodes on a page. Not yet functional!}
#'   \item{\code{$cut()}}{NOT WORKING}
#'   \item{\code{$reconstruct_paragraphs}}{Reconstruct paragraphs based on the
#'   following heuristic: If a line ends with a hyphen and is not stump, lines
#'   are concatenated.}
#'   \item{\code{$purge()}}{Remove noise, surplus whitespace signs from the text.}
#'   \item{\code{$xmlify(root = "document", metadata = NULL)}}{Turn content of
#'   field 'pages' into a XML document, optionally adding metadata.}
#'   \item{\code{$xml2md}}{Turn xmlified document into markdown (will be stored in
#'   field 'markdown').}
#'   \item{\code{$md2html()}}{Turn markdown (field \code{markdown}) into html
#'   document that will be stored in the field \code{html}.}
#'   \item{\code{$xml2html()}}{Turn xmlification of pdf document into html document to
#'   support quality checks.}
#'   \item{\code{$browse(viewer = getOption("viewer", utils::browseURL))}}{Show
#'   html document in browser.}
#'   \item{\code{$wrixte()}}{Save xmlified document (available in the field 'xmlification') to a file.}
#' }
#' 
#' @section Arguments:
#' \describe{
#'   \item{\code{filename_pdf}}{path to a pdf document}
#'   \item{\code{first}}{first page of the pdf document to be included}
#'   \item{\code{last}}{last page of the pdf document to be included}
#'   \item{\code{jitter}}{points up and down for reconstructing tilted lines}
#'   \item{\code{deviation}}{points that lines may deviate from middle of page}
#'   \item{\code{margins}}{named vector (top, bottom, left, right)}
#'   \item{\code{root}}{name of the root node}
#'   \item{\code{metadata}}{named character vector, attribtutes of root node of output xml document}
#'   \item{\code{filename}}{character vector}
#'   \item{\code{viewer}}{the viewer to use to inspect pdf or html documents}
#' }
#' 
#' @importFrom xml2 xml_find_all write_xml xml_attrs xml_set_attr xml_set_attrs xml_add_child xml_new_root xml_text xml_replace
#' @importFrom pbapply pblapply
#' @importFrom R6 R6Class
#' @importFrom htmltools HTML html_print
#' @importFrom Rpoppler PDF_info
#' @importFrom stringi stri_extract_all
#' @importFrom markdown markdownToHTML
#' @importFrom plyr dlply .
#' @importFrom methods setOldClass
#' @rdname PDF-class
#' @name PDF
#' @export PDF
#' @examples
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
#' # P$browse()
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
#' if (interactive()) UN$browse()
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
#' if (interactive()) P$browse()
PDF <- R6::R6Class(
  
  "PDF",
  
  public = list(
    
    # fields
    
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
    boxes = "data.frame",
    
    
    # methods
    
    initialize = function(filename_pdf, first = NA, last = NA, jitter = 2, deviation = 10L, margins = integer()){

      if (length(filename_pdf) > 1){
        stop("Length of argument 'filename_pdf' > 1: The class can only process one pdf document at a time.")
      }
      self$filename_pdf <- filename_pdf
      
      stopifnot(length(first) == 1, length(last) == 1, length(jitter) == 1)
      self$first <- if (is.na(first)) 1L else as.integer(first)
      self$last <- if (is.na(last)) as.integer(Rpoppler::PDF_info(filename_pdf)$Pages) else as.integer(last)
      self$jitter <- jitter
      self$margins <- margins
      self$boxes <- data.frame(
        page_node = integer(), left = numeric(), top = numeric(), width = numeric(),
        height = numeric(), bottom = numeric(), right = numeric()
      )
      self$xml <- pdf_to_xml(filename = self$filename_pdf, first = self$first, last = self$last)
      self$no_pages <- self$get_number_of_pages()
      self$get_pagesizes()
      # self$add_box(box = NULL, page = NULL)
      invisible(self)
    },
    
    validate = function(){
      
      if (!is.null(self$filename_pdf)) stopifnot(is.character(filename_pdf))
      if (!is.null(self$first)) stopifnot(is.integer(first))
      if (!is.null(self$last)) stopifnot(is.integer(last))
      if (!is.null(self$xml)) stopifnot(class(xml)[1] == "xml_document")
      if (!is.null(self$jitter)) stopifnot(is.numeric(jitter))
      if (!is.null(self$pages)) stopifnot(is.list(pages))
      if (!is.null(self$no_pages)) stopifnot(is.integer(no_pages))
      if (!is.null(self$margin)) stopifnot(is.integer(margins))
      if (!is.null(self$deviation)) stopifnot(is.integer(deviation))
      if (!is.null(self$xmlification)) stopifnot(class(xmlification)[1] == "xml_document")
      if (!is.null(self$pagesizes)) stopifnot(is.data.frame(pagesizes))
      if (!is.null(self$markdown)) stopifnot(is.character(markdown))
      if (!is.null(self$html)) stopifnot(class(html)[1] == "html")
      if (!is.null(self$boxes)) stopifnot(is.data.frame(boxes))
      
    },
    
    show_pdf = function(){
      browseURL(self$filename_pdf)
    },

    get_number_of_pages = function(){
      length(self$get_page_nodes())
    },
    
    make_box = function(box = NULL, page){
      
      if (is.null(box)){
        newBox <- data.frame(
          page_node = page, left = 0, top = 0,
          width = self$pagesizes[page, "width"], height = self$pagesizes[page, "height"],
          bottom = self$pagesizes[page, "height"], right = self$pagesizes[page, "width"]
        )
      } else {
        stopifnot(all(c("left", "top", "width", "height") %in% names(box)))
        
        horizontal <- self$pagesizes[page, "width"] / self$pagesizes[page, "width.pts"]
        vertical <- self$pagesizes[page, "height"] / self$pagesizes[page, "height.pts"]
        
        newBox <- data.frame(
          page_node = page,
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
      
      if (is.null(page)){
        boxList <- lapply(
          1:nrow(self$pagesizes),
          function(i) self$make_box(box = box, page = i)
        )
        newBoxDataFrame <- as.data.frame(do.call(rbind, boxList))
        if (replace){
          self$boxes <- newBoxDataFrame
        } else {
          self$boxes <- rbind(self$boxes, newBoxDataFrame)
          self$boxes <- self$boxes[order(self$boxes[["page_node"]]),]
        }
      } else {
        for (i in page){
          newBox <- self$make_box(box = box, page = page)
          if (replace){
            hit <- which(self$boxes[["page_node"]] == i)
            if (length(hit) > 0) self$boxes <- self$boxes[-hit,]
          }
          self$boxes <- rbind(self$boxes, newBox)
          self$boxes <- self$boxes[order(self$boxes[["page_node"]]),]
        }
      }
      invisible(self$boxes)
    },
    
    drop_page = function(page){
      
      if (nrow(self$boxes) > 0) stop("removing pages should only be done before defining boxes")
      if (missing(page)) stop("drop_page-method: at least one page needs to be specified")
      page <- as.integer(page)
      if (any(is.na(page))) stop("drop_page-method: values of page cannot be coerced to integer",
                                 "without creating NAs")
      page_nodes <- self$get_page_nodes()
      if (any(page > length(page_nodes))) stop("at least one page number beyond end of document") 
      
      page <- page[order(page, decreasing = TRUE)] 
      for (i in page)xml2::xml_remove(page_nodes[[i]])
      self$no_pages <- self$get_number_of_pages()
      self$pagesizes <- self$pagesizes[which(1:nrow(self$pagesizes) %in% page == FALSE), ]
      self$pagesizes[["page_node"]] <- 1L:nrow(self$pagesizes)
      self$boxes
      invisible(self)
    },
    
    drop_unboxed_text_nodes = function(node, boxes, copy = FALSE){
      
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
      
      page_nodes <- self$get_page_nodes()
      pblapply(
        1:length(page_nodes), # iterate through pages
        function(i){
          boxesDataFrame <- self$boxes[which(self$boxes[["page_node"]] == i),]
          boxList <- lapply(1:nrow(boxesDataFrame), function(j) as.vector(boxesDataFrame[j,]))
          node_new <- self$drop_unboxed_text_nodes(node = page_nodes[[i]], boxes = boxList, copy = TRUE)
          xml_replace(.x = page_nodes[[i]], .value = node_new)
        }
      )
      invisible()
    },
    
    decolumnize = function(){

      pageNodes <- self$get_page_nodes()
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
      
      # get width and height (pdf units)
      xmlAttrsPages <- lapply(self$get_page_nodes(), xml_attrs)
      self$pagesizes <- as.data.frame(do.call(rbind, xmlAttrsPages), stringsAsFactors = FALSE)
      colnames(self$pagesizes)[which(colnames(self$pagesizes) == "number")] <- "page_node"
      for (what in c("page_node", "top", "left", "height", "width")){
        self$pagesizes[[what]] <- as.integer(self$pagesizes[[what]])
      }
      
      sizes <- stri_extract_all(str = Rpoppler::PDF_info(self$filename_pdf)$Sizes, regex = "\\d+(\\.\\d+|)")
      if (length(sizes) > 1){
        sizes.pts <- do.call(rbind, lapply(sizes, as.numeric))[self$first:self$last, 1:2]
        colnames(sizes.pts) <- c("width.pts", "height.pts")
      } else {
        sizes.pts <- data.frame(
          width.pts = as.numeric(sizes[[1]][1]),
          height.pts = as.numeric(sizes[[1]][2])
          )
      }
      self$pagesizes <- cbind(self$pagesizes, sizes.pts)
      invisible(self$pagesizes)
    },
    
    get_text = function(node, paragraphs = TRUE){
      
      text_nodes <- xml2::xml_find_all(node, xpath = "./text")
      txt <- character()
      txtPosition <- integer()
      counter <- 1L
      if (length(text_nodes) == 0) return(txt)
      for (i in 1:length(text_nodes)){
        if (i == 1){
          txt[1] <- xml2::xml_text(text_nodes[[i]])
          txtPosition[1] <- as.integer(xml2::xml_attrs(text_nodes[[i]])["top"])
        } else if ( i > 1 ){
          topCurrentNode <- as.integer(xml2::xml_attrs(text_nodes[[i]])["top"])
          topPreviousNode <- as.integer(xml2::xml_attrs(text_nodes[[i-1]])["top"])
          if (abs(topCurrentNode - topPreviousNode) > self$jitter) {
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
      txt
    },
    
    get_text_from_pages = function(paragraphs = TRUE){

      self$pages <- lapply(
        self$get_page_nodes(),
        function(page) self$get_text(page, paragraphs = paragraphs)
      )
    },
    
    get_text_from_boxes = function(paragraphs){
      
      page_nodes <- self$get_page_nodes()
      self$pages <- dlply(
        .data = self$boxes, .variables = .(page_node), # create sub-data.frames for bages
        .fun = function(df){
          df[["box_id"]] <- 1:nrow(df)
          page_no <- unique(df[["page_node"]])
          dlply(
            .data = df, .variables = .(box_id),
            .fun = function(df2){
              page_node <- xml_new_root(page_nodes[[ df2[["page_node"]] ]], .copy = TRUE)
              box_list <- lapply(
                1:nrow(df2),
                function(j) setNames(as.numeric(df2[j,]), colnames(df2))
              )
              node_page <- self$drop_unboxed_text_nodes(node = page_node, box_list, copy = TRUE)
              self$get_text(node_page, paragraphs = paragraphs)
            }
          )
        }
      )
    },
    
    find = function(regex){

      page_nodes <- self$get_page_nodes()
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

      self$xml <- pblapply(
        xml_find_all(self$xmlification, xpath = "/document/page"),
        function(page){
          
        }
      )
    },
    
    cut = function(){
      stop()
    },
    
    reconstruct_paragraphs = function(){
      
      if (typeof(self$pages[[1]]) == "list"){
        # option 1: pages include boxes
        self$pages <- pblapply(
          self$pages,
          function(page) lapply(page, restore_paragraphs)
        )
      } else {
        self$pages <- pblapply(self$pages, restore_paragraphs)
      }
    },
    
    purge = function(){

      if (typeof(self$pages[[1]]) == "list"){
        self$pages <- lapply(self$pages, function(page) lapply(page, broom))
      } else {
        self$pages <- lapply(self$pages, broom)
      }
      
    },
    
    xmlify = function(root = "document", metadata = NULL){
      
      self$xmlification <- xml_new_root(.value = root)
      if (!is.null(metadata)){
        if (is.null(names(metadata))){
          stop(
            "If argument metadata is given, it needs to be a named character vector. ",
            "The names need to provide attribute names."
          )
        }
        xml_set_attrs(self$xmlification, value = metadata)
      }
      dummy <- pblapply(
        self$pages,
        function(page){
          xml2::xml_add_child(self$xmlification, .value = "page")
          pageNodes <- xml_find_all(self$xmlification, xpath = sprintf("/%s/page", root))
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
      invisible(self$xmlification)
    },
    
    xml2md = function(){

      box_nodes <- xml_find_all(self$xmlification, xpath = "/document/page/box")
      boxed <- if (length(box_nodes) > 0) TRUE else FALSE
      
      pagesMarkdown <- lapply(
        xml_find_all(self$xmlification, xpath = "/document/page"),
        function(page){
          if (boxed){
            box_txt <- lapply(
              xml_find_all(page, xpath = "./box"),
              function(box){
                paras <- sapply(xml_find_all(box, xpath = "./p"), xml_text)
                paste(paras, collapse = "\n\n")
              }
            )
            txt <- paste(paste(box_txt, collapse = "\n\n---\n\n"), "\n")
          } else {
            paras <- sapply(xml_find_all(page, xpath = "./p"), xml_text)
            txt <- paste(paras, collapse = "\n\n")
          }
          txt
        }
      )
      self$markdown <- paste(paste(pagesMarkdown, collapse = "\n\n* * *\n\n"), "\n")
      invisible(self$markdown)
    },
    
    md2html = function(){
      
      if (length(self$markdown) == 0) self$xml2md()
      mdFilename <- tempfile(fileext = ".md")
      htmlFile <- tempfile(fileext = ".html")
      cat(self$markdown, file = mdFilename)
      markdown::markdownToHTML(file = mdFilename, output = htmlFile)
      htmldoc <- scan(file = htmlFile, what = character(), sep = "\n", quiet = TRUE)
      htmldoc <- paste(htmldoc, collapse = "\n")
      self$html <- HTML(htmldoc)
      invisible(self$html)
    },
    
    xml2html = function(){
      self$xml2md()
      self$md2html()
    },
    
    browse = function(viewer = getOption("viewer", utils::browseURL)){
      if (is.null(self$html)) self$md2html()
      htmltools::html_print(self$html, viewer = viewer)
    },
    
    write = function(filename){
      write_xml(self$xmlification, file = filename)
    },
    
    get_page_nodes = function() xml2::xml_find_all(self$xml, xpath = "/pdf2xml/page")
  )
)
