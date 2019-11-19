# v0.1.2
* the utility function `pdf_to_xml()` is now the worker to convert pdf documents to xml;
* invalid xml (happens occasionally) is repaired using xmllint --recover, if availably (issue #3)
* bug fix (issue #2) - PDF class can now process one-page-documents
* bug fix (issue #1) - whitespace in filename does not cause crash
* missing whitespace when concatenating lines solved
* Unit tests adapted to conform to new handling of whitespace
* Package 'knitr' has been moved in DESCRIPTION from "Depends:" to "Suggests:", which is the usual practice.
* Package 'magrittr' has been dropped from 'Dependes:' in DESCRIPTION; code using magrittr has been reformulated 
to avoid using magrittr within a package.
* Christoph Leonhardt listed as package author.


# v0.1.1
* bug in method $get_pages() removed that occurrs when there are not text nodes on page
* bug removed when adding boxes to existing box data.frame
* vignette explains scenario of two column layout now (using two approaches)
* moved to R6 class system, replacing reference classes
* columns "page" in field $pagesizes and $boxes renamed to page_node (to avoid confusion)
* remove_page-method added to PDF class


# v0.1.0

* Added a `NEWS.md` file to track changes to the package.



