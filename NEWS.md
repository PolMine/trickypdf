# v0.1.2
* the utility function pdf_to_xml() is now the worker to convert pdf documents to xml;
invalid xml (happens occasionally) is repaired using xmllint --recover, if availably
* bug fix (issue #2) - PDF class can now process one-page-documents

# v0.1.1
* bug in method $get_pages() removed that occurrs when there are not text nodes on page
* bug removed when adding boxes to existing box data.frame
* vignette explains scenario of two column layout now (using two approaches)
* moved to R6 class system, replacing reference classes
* columns "page" in field $pagesizes and $boxes renamed to page_node (to avoid confusion)
* remove_page-method added to PDF class


# v0.1.0

* Added a `NEWS.md` file to track changes to the package.



