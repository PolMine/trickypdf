setOldClass("xml_document")

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
#' @param filename_pdf path to a pdf document
#' @param first first page of the pdf document to be included
#' @param last last page of the pdf document to be included
#' @param jitter points up and down for reconstructing tilted lines
#' @param deviation points that lines may deviate from middle of page
#' @param margins named vector (top, bottom, left, right)
#' @param root name of the root node
#' @param metadata named character vector, attribtutes of root node of output xml document
#' @param filename character vector
#' @importFrom xml2 xml_find_all write_xml xml_attrs xml_set_attrs xml_add_child xml_new_root
#' @importFrom pbapply pblapply
#' @importFrom methods setRefClass new
#' @rdname PDF-class
#' @name PDF
#' @examples
#' \dontrun{
#' P <- PDF$new(filename_pdf = "/Users/blaette/Lab/tmp/cdu.pdf", first = 7, last = 119)
#' P$parse()
#' P$getPagesizes()
#' P$margins <- c(top = 120L, bottom = 1262L, left = 55L, right = 892L)
#' P$applyMargins()
#' P$getText()
#' P$concatenate()
#' P$purge()
#' P$xmlify()
#' P$write(filename = "/Users/blaette/Lab/tmp/cdu.xml")
#' 
#' plenaryprotocol <- system.file(package = "pdf2xml", "extdata", "pdf", "18238.pdf")
#' P <- PDF$new(filename_pdf = plenaryprotocol, first = 5, last = 73)
#' P$parse()
#' P$margins <- c(top = 70L, bottom = 1262L, left = 70L, right = 800L)
#' P$applyMargins()
#' P$deviation <- 10L
#' P$decolumnize()
#' P$getText()
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
    margins = "integer",
    deviation = "integer",
    xmlification = "xml_document",
    pagesizes = "data.frame"
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
      .self$first <- as.integer(first)
      .self$last <- as.integer(last)
      .self$jitter <- jitter
      .self$margins <- margins
    },
    
    parse = function(){
      
      "Parse a pdf document, i.e. turn it into a raw XML document kept in the field xml."
      
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
    },
    
    applyMargins = function(){
      
      "Remove anything that is printed on pages beyond the margins given."
      
      pageNodes <- xml2::xml_find_all(.self$xml, xpath = "/pdf2xml/page")
      lapply(
        pageNodes, # iterate through pages
        function(page){
          textNodes <- xml2::xml_find_all(page, xpath = "./text")
          lapply(
            textNodes, # iterate through text nodes
            function(textNode){
              textNodeAttrs <- xml2::xml_attrs(textNode)
              textNodeAttrs <- setNames(as.integer(textNodeAttrs), names(textNodeAttrs))
              if (
                textNodeAttrs["top"] > .self$margins["top"]
                && textNodeAttrs["top"] < .self$margins["bottom"]
                && textNodeAttrs["left"] > .self$margins["left"]
                && textNodeAttrs["left"] < .self$margins["right"]
              ){
                # do nothing, text node is within margins
              } else {
                xml2::xml_remove(textNode)
              }
            }
          )
        }
      )
      invisible()
    },
    
    decolumnize = function(){
      
      "Do away with columns, if pages include two columns."
      
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
              textNodeAttrs <- xml2::xml_attrs(textNode)
              textNodeAttrs <- setNames(as.integer(textNodeAttrs), names(textNodeAttrs))
              if (as.numeric(textNodeAttrs["left"]) > pageCenter ){
                xml2::xml_attrs(textNode)["left"] <- textNodeAttrs["left"] - pageCenter
                xml2::xml_attrs(textNode)["top"] <- textNodeAttrs["top"] + pageHeight
              }
            }
          )
        }
      )
      invisible()
    },
    
    getPagesizes = function(){
      
      "Get page width and height."
      
      xmlAttrsPages <- lapply(
        xml2::xml_find_all(.self$xml, xpath = "/pdf2xml/page"),
        function(page) xml_attrs(page)
      )
      .self$pagesizes <- as.data.frame(do.call(rbind, xmlAttrsPages), stringsAsFactors = FALSE)
      for (what in c("number", "top", "left", "height", "width")){
        .self$pagesizes[[what]] <- as.integer(.self$pagesizes[[what]])
      }
      invisible(.self$pagesizes)
    },
    
    getText = function(){
      
      "Extract text from pages."
      
      .self$pages <- lapply(
        xml2::xml_find_all(.self$xml, xpath = "/pdf2xml/page"),
        function(page){
          txt <- character()
          txtPosition <- integer()
          textNodes <- xml2::xml_find_all(page, xpath = "./text")
          counter <- 1L
          for (i in 1:length(textNodes)){
            if (i == 1){
              txt[1] <- xml2::xml_text(textNodes[[i]])
              txtPosition[1] <- as.integer(xml2::xml_attrs(textNodes[[i]])["top"])
            } else if ( i > 1 ){
              topCurrentNode <- as.integer(xml2::xml_attrs(textNodes[[i]])["top"])
              topPreviousNode <- as.integer(xml2::xml_attrs(textNodes[[i-1]])["top"])
              if (abs(topCurrentNode - topPreviousNode) > .self$jitter) {
                counter <- counter + 1L
                txt[counter] <- xml2::xml_text(textNodes[[i]])
                txtPosition[counter] <- topCurrentNode
              } else {
                txt[counter] <- paste(txt[counter], xml2::xml_text(textNodes[[i]]), sep = "")
              }
            }
          }
          txt[order(txtPosition)] # if order of text nodes is screwed up
        }
      )
    },
    
    concatenate = function(){
      
      "Reconstruct paragraphs based on the following heuristic: If a line ends with a hyphen
      and is not stump, lines are concatenated."
      
      .self$pages <- pblapply(
        .self$pages,
        function(lines){
          if (length(lines) > 2){
            for (i in length(lines):2){
              if (nchar(lines[i-1]) < 40 && grepl("[\\.?!]\\s*$", lines[i-1]) == TRUE){
                # do nothing if preceding line ist short and ends with a satzzeichen
              } else {
                if (grepl("-\\s*$", lines[i-1]) && grepl("^\\s*[A-Z]", lines[i]) == FALSE){
                  lines[i-1] <- gsub("-\\s*$", "", lines[i-1]) # remove hyphen
                  lines[i-1] <- paste(lines[i-1], lines[i], sep = "")
                  lines <- lines[-i]
                } else {
                  lines[i-1] <- paste(lines[i-1], lines[i], sep = " ")
                  lines <- lines[-i]
                }
              }
            }
          }
          lines
        }
      )
    },
    
    purge = function(){
      
      "Remove noise, surplus whitespace signs from the text."
      
      .self$pages <- lapply(
        .self$pages,
        function(page){
          page <- gsub("\uf038", "", page)
          page <- gsub("\\s+", " ", page)
          page <- gsub("^\\s*(.*?)\\s*$", "\\1", page)
          page
        }
      )
    },
    
    xmlify = function(root = "document", metadata = NULL){
      
      "Turn text in the pages fild into a XML document."
      
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
          lapply(
            page,
            function(paragraph) xml_add_child(newPageNode, .value = "p", paragraph)
          )
          NULL
        }
      )
      invisible(.self$xmlification)
    },
    
    write = function(filename){
      
      "Save xmlified document to a file."
      
      write_xml(.self$xmlification, file = filename)
    }
  )
)
