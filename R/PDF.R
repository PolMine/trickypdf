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
#'   get sizes in points (pts), \code{pdf_pagesize} (package pdftools) is used.
#'   The result is a data.frame in the field pagesizes. The method is called
#'   when parsing the pdf document.}
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
#'   
#'   
#'   
#'
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
#' @importFrom stringi stri_extract_all
#' @importFrom markdown markdownToHTML
#' @importFrom plyr dlply .
#' @importFrom methods setOldClass
#' @importFrom pdftools pdf_info pdf_pagesize
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
      self$last <- if (is.na(last)) pdftools::pdf_info(filename_pdf)[["pages"]] else as.integer(last)
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
      xml_attrs_pages <- lapply(self$get_page_nodes(), xml_attrs)
      self$pagesizes <- as.data.frame(do.call(rbind, xml_attrs_pages), stringsAsFactors = FALSE)
      colnames(self$pagesizes)[which(colnames(self$pagesizes) == "number")] <- "page_node"
      for (what in c("page_node", "top", "left", "height", "width")){
        self$pagesizes[[what]] <- as.integer(self$pagesizes[[what]])
      }
      
      sizes_pts <- pdftools::pdf_pagesize(pdf = self$filename_pdf)[, c("width", "height")]
      colnames(sizes_pts) <- c("width.pts", "height.pts")
      self$pagesizes <- cbind(self$pagesizes, sizes_pts[self$first:self$last,])
      invisible(self$pagesizes)
    },
    
    get_text = function(node, paragraphs = TRUE){
      
      text_nodes <- xml2::xml_find_all(node, xpath = "./text")
      txt <- character()
      txt_position <- integer()
      counter <- 1L
      if (length(text_nodes) == 0L) return(txt)
      for (i in 1:length(text_nodes)){
        if (i == 1){
          txt[1] <- xml2::xml_text(text_nodes[[i]])
          txt_position[1] <- as.integer(xml2::xml_attrs(text_nodes[[i]])["top"])
        } else if ( i > 1 ){
          position_y_current_node <- as.integer(xml2::xml_attrs(text_nodes[[i]])["top"])
          position_y_previous_node <- as.integer(xml2::xml_attrs(text_nodes[[i-1]])["top"])
          if (abs(position_y_current_node - position_y_previous_node) > self$jitter) {
            counter <- counter + 1L
            txt[counter] <- xml2::xml_text(text_nodes[[i]])
            txt_position[counter] <- position_y_current_node
          } else {
            position_x_current_node <- as.integer(xml2::xml_attrs(text_nodes[[i]])["left"])
            position_x_previous_node <- as.integer(xml2::xml_attrs(text_nodes[[i-1]])["left"])
            if (position_x_current_node >= position_x_previous_node){
              txt[counter] <- paste(
                txt[counter],
                xml2::xml_text(text_nodes[[i]]),
                sep = " "
                )
            } else {
              txt[counter] <- paste(
                xml2::xml_text(text_nodes[[i]]),
                txt[counter],
                sep = " "
                )
            }
            txt_position[counter] <- position_y_current_node
          }
        }
      }
      txt <- txt[order(txt_position)] # if order of text nodes is screwed up
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
    
    ######
    
    get_headline_boxes = function(page) {
      pageNodes <- self$get_page_nodes()
      
      message("...detecting headline boxes")
      headline_boxes <- pblapply(
        pageNodes, # iterate through pages
        function(page){
          
          pageNodeAttrs <- xml2::xml_attrs(page)[c("height", "width")]
          pageNodeAttrs <- setNames(as.numeric(pageNodeAttrs), names(pageNodeAttrs))
          pageCenter <- floor(pageNodeAttrs["width"] / 2)
          
          textNodes <- xml2::xml_find_all(page, xpath = "./text")
          lapply(
            textNodes, # iterate through text nodes
            function(textNode){
              position <- xml2::xml_attrs(textNode)
              position <- setNames(as.integer(position), names(position))
              
              # old solution with purrr
              # typicalLeftValues <- sort(
              #   as.integer(names(head(sort(table(unlist(purrr::map(xml2::xml_attrs(textNodes), "left"))),
              #                              decreasing = TRUE), 2))))
              
              typicalLeftValues <- sort(table(as.integer(xml2::xml_attr(textNodes, attr = "left"))), decreasing = TRUE)
              typicalLeftValues <- sort(as.integer(names(typicalLeftValues[1:2])))
              
              # naming the vector ll (left left: left boundary of left column) 
              # and rl (right left: left boundary of right column)
              
              if (length(typicalLeftValues == 2)) names(typicalLeftValues) <- c("ll", "rl") else stop("...page has only one column")
              
              # Headline boxes do not need to be wider than a typical column, but they 
              # cross the center of the page. Hence:
              # If they start somewhere different than aligned left and cross the center of the page.
              # NB: This isn't failproof as in scanned documents the alignment varies from line to line which is why we add or substract 5 to each typical value
              # NB: even in digitally produced documents, the xml output of the pdf suggests a longer line sometimes which is why we add 10 to the center to avoid false positives 
              
              if (as.numeric(position["left"]) > typicalLeftValues["ll"] + 5 && as.numeric(position["left"]) < typicalLeftValues["rl"] - 5 && as.numeric(position["left"]) + as.numeric(position["width"]) > pageCenter + 10) {
                
                page_no <- as.integer(xml2::xml_attrs(page)["number"])
                box_attributes <- xml2::xml_attrs(textNode)[c("top", "left", "width", "height")]
                
                headline_box <- data.frame(page = page_no, top = as.integer(box_attributes["top"]), left = as.integer(box_attributes["left"]), width = as.integer(box_attributes["width"]), height = as.integer(box_attributes["height"]), stringsAsFactors = FALSE)
                
                
              }
            }
          )
          
        }
      )
      return(headline_boxes)
    },  
    
    
    concatenate_headline_boxes = function() {
      boxes <- self$get_headline_boxes()
      # add line index to detect consecutive lines
      headline_box_dataframe <- data.frame()
      for (i in 1:length(boxes)) {
        box_list <- boxes[[i]]
        names(box_list) <- seq_along(box_list)
        page_boxes <- do.call(rbind, box_list)
        page_boxes$idx <- as.integer(rownames(page_boxes))
        headline_box_dataframe <- rbind(headline_box_dataframe, page_boxes)
      }
      
      # the names of the page nodes of the already existing boxes in self$boxes are changed during dropping pages
      # this is repeated for the newly created boxes here, in which pages were dropped but page node names weren't changed
      
      headline_box_dataframe$page <- headline_box_dataframe$page - (min(headline_box_dataframe$page) - min(self$boxes$page_node))
      
      # soft filtering of boxes which are out of bounds or too narrow to be actual headlines (5 pt)
      
      mean_vertical <- mean(self$pagesizes[, "height"]) / mean(self$pagesizes[, "height.pts"])
      
      top_page <- min(self$boxes$top) # the smallest top value of any box
      height_page <- max(self$boxes$height) # the greatest height value of any box
      
      headline_box_dataframe <- subset(headline_box_dataframe, top > top_page & top < (top_page + height_page)) 
      headline_box_dataframe <- subset(headline_box_dataframe, width > 5) # hardcoding rausnehmen?
      
      if (nrow(headline_box_dataframe) > 0) {
        for (i in 2:nrow(headline_box_dataframe)) {
          
          # if line directly follows the previously detected line and top of line is greater than top of previous line 
          if (headline_box_dataframe[i,"idx"] == headline_box_dataframe[i-1,"idx"]+1 && headline_box_dataframe[i,"top"] > headline_box_dataframe[i-1,"top"]) {
            
            # use the top value of the previously detected line
            linedist <- headline_box_dataframe[i, "top"] - headline_box_dataframe[i-1, "top"]
            
            headline_box_dataframe[i, "top"] <- headline_box_dataframe[i-1, "top"]
            
            # compare both "left" values and use smaller one
            headline_box_dataframe[i, "left"] <- ifelse(headline_box_dataframe[i, "left"] < headline_box_dataframe[i-1, "left"], headline_box_dataframe[i, "left"], headline_box_dataframe[i-1, "left"])
            
            # compare both "width" values and use greater one
            headline_box_dataframe[i, "width"] <- ifelse(headline_box_dataframe[i, "width"] > headline_box_dataframe[i-1, "width"], headline_box_dataframe[i, "width"], headline_box_dataframe[i-1, "width"])
            
            # add "height" value of previos line to current line plus the distance between the lines
            headline_box_dataframe[i, "height"] <- headline_box_dataframe[i, "height"] + linedist
            
            # after incorporating previos line, mark its row for later removal (otherwise the indexing gets confused)
            headline_box_dataframe[i-1, "idx"] <- 0
          }
        }
      } else {
        message("no headlines detected")
      }
      
      
      # remove all rows from headline_box_dataframe with idx of 0, because they are all concatenated in one box now
      
      headline_box_dataframe <- headline_box_dataframe[-which(headline_box_dataframe$idx == 0), ]
      return(headline_box_dataframe)
      
    },
    
    
    merge_boxes = function(page_top = NULL, page_height = NULL) {
      headline_boxes <- self$concatenate_headline_boxes()
      self$boxes$headline <- FALSE
      pages <- as.integer(unique(headline_boxes$page))
      
      for (page in pages) {
        
        vertical <- self$pagesizes[page, "height"] / self$pagesizes[page, "height.pts"]
        tmp_box <- headline_boxes[which(headline_boxes$page == page),]
        
        # maybe take top and height from other boxes
        
        tmp_box$bottom <- tmp_box$top + tmp_box$height
        tmp_box$right <- tmp_box$left + tmp_box$width
        
        # drop boxes which are out of bounds
        
        top_page <- min(self$boxes$top[self$boxes$page_node == page]) # the smallest top value of any box on page
        height_page <- max(self$boxes$height[self$boxes$page_node == page]) # the greatest height value of any box on page
        
        
        tmp_box <- subset(tmp_box, top > (top_page) & top < (top_page + height_page))
        
        # here, it might be that all the detected headline boxes were indeed out of bounds (i.e. header boxes etc.)
        if (nrow(tmp_box) > 0) {
          names(tmp_box)[names(tmp_box) == 'page'] <- 'page_node'  
          tmp_box$idx <- NULL
          tmp_box$headline <- TRUE
        }
        
        
        self$boxes <- rbind(self$boxes, tmp_box)
        
      }
      
      return(self$boxes)
      
    },
    
    add_headline_boxes = function(page_top = page_top, page_height = page_height) {
      self$boxes <- self$merge_boxes()
      
      # Wenn es eine Box gibt mit headline = TRUE
      
      true_pages <- self$boxes[which(self$boxes$headline == TRUE), ]
      true_pages <- as.integer(unique(true_pages$page_node))
      
      for (page in true_pages) {
        
        # split box dataframe into one box per page and get headline box seperately
        tmp_box <- self$boxes[which(self$boxes$page_node == page),]
        tmp_hl <- tmp_box[which(tmp_box$headline == TRUE), ]
        
        # adjust margins of highlight boxes as well as height and width slightly to avoid too narrow boxes
        tmp_hl$top <- tmp_hl$top - 1
        tmp_hl$left <- tmp_hl$left - 1
        tmp_hl$height <- tmp_hl$height + 2
        tmp_hl$width <- tmp_hl$width + 2
        
        
        # get greatest possible height from one of the ordinary boxes
        general_height <- tmp_box[which(tmp_box$headline == FALSE), "height"][1]
        
        # to calculate new heights we need the height of the boxes plus the height of the space above the first box (i.e. the top margin)
        combined_height <- general_height + tmp_box[which(tmp_box$headline == FALSE), "top"][1]
        
        # get top value of headline box
        top_headline <- tmp_box[which(tmp_box$headline == TRUE), "top"]
        
        # get height of headline box
        height_headline <- tmp_box[which(tmp_box$headline == TRUE), "height"]
        
        # get all non-headline boxes
        tmp_box <- tmp_box[which(tmp_box$headline == FALSE), ]
        
        top_page <- min(self$boxes$top[self$boxes$page_node == page]) # the smallest top value of any box on page
        
        median_lineheight <- 20
        
        
        # for each headline box
        for (j in 1:length(top_headline)) {
          
          # if there is only one headline which is on the very top of the page, shorten boxes underneath
          
          if (length(top_headline) == 1 && top_headline[j]/(self$pagesizes[page, "height"] / self$pagesizes[page, "height.pts"]) - top_page < median_lineheight) {
            
            # shorten box underneath the headline and adjust top value
            
            tmp_box[, "top"] <- top_headline[j] + height_headline[j]
            tmp_box[, "height"] <- combined_height - top_headline[j] - height_headline[j]
            
            # if there are multiple headline boxes but the first is on the very top
            
          } else if (j == 1 && top_headline[j]/(self$pagesizes[page, "height"] / self$pagesizes[page, "height.pts"]) - top_page < median_lineheight) {
            
            # shorten box underneath the headline and adjust top value
            
            tmp_box[, "top"] <- top_headline[j] + height_headline[j]
            tmp_box[, "height"] <- top_headline[j+1] - top_headline[j] - height_headline[j]
            tmp_box[, "bottom"] <- tmp_box[, "top"] + tmp_box[, "height"]
            
            
          } else {
            # if there are headline boxes but not on top
            
            if (length(top_headline[j-1]) == 0) {
              tmp_box[, "height"] <- tmp_box[, "height"] - (combined_height - top_headline[j]) - 3
              tmp_box[, "bottom"] <- tmp_box[, "top"] + tmp_box[, "height"]
            }
            # a new box must follow the headline box
            
            top_new_box <- top_headline[j] + height_headline[j]
            
            # the new box must be as high as the entire possible length minus the already occupied 
            # space above and minus the (potentially) already occupied space underneath
            
            if (!is.na(top_headline[j+1])) {
              height_new_box <- combined_height - top_new_box - (combined_height - top_headline[j+1]) - 3
            } else {
              height_new_box <- combined_height - top_new_box
            }
            
            
            # copy values of old box and replace top, height and bottom for box underneath 
            
            if (j == 1) {
              for (i in 1:nrow(tmp_box)) {
                
                new_box <- tmp_box[i,]
                new_box$height <- height_new_box
                new_box$top <- top_new_box
                new_box$bottom <- new_box$top + new_box$height
                tmp_box <- rbind(tmp_box, new_box)
              }
              
            } else {
              
              for (i in 1:nrow(tmp_box[ which(tmp_box[,"top"] > top_headline[j-1]), ])) {
                
                new_box <- tmp_box[ which(tmp_box[,"top"] > top_headline[j-1]), ][i,]
                new_box$height <- height_new_box
                new_box$top <- top_new_box
                new_box$bottom <- new_box$top + new_box$height
                tmp_box <- rbind(tmp_box, new_box)
              }
            }
          }
        }
        
        # remove previous text boxes and replace with newly calculated
        self$boxes <- self$boxes[-which(self$boxes$page_node == page), ]
        x <- rbind(tmp_box, tmp_hl)
        self$boxes <- rbind(self$boxes, x)
        
        
      }
      
      # as for now, the order of the boxes in plain text are determined by their respective position in the data frame. 
      # Reorder here accordingly
      
      self$boxes <- self$boxes[order(self$boxes[,"page_node"], self$boxes[,"top"], self$boxes[,"left"]),]
      
    },

    get_page_nodes = function() xml2::xml_find_all(self$xml, xpath = "/pdf2xml/page")
  )
)
