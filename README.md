
<!-- README.md is generated from README.Rmd. Please edit that file -->
trickypdf
=========

Purpose
-------

At a basic level, R users have efficient tools to extract text from pdf documents. The [pdftools](https://CRAN.R-project.org/package=pdftools), and the [Rpoppler](https://CRAN.R-project.org/package=Rpoppler) packages are powerful tools.

The *trickypdf* package offers a class *PDF* to handle tricky problems that reoccurringly cause headaches when processing pdf documents with . , i.e.:

-   remove stuff outside the main text region (page headers, page numbers etc) as a preprocessing step.

-   handle multi-column layouts;

-   reconstruct lines of text, if the (OCRed) document has been scanned in tilted fashion;

-   reconstruct paragraphs.

The output will be a valid XML document, with optional document metadata. The XML output is meant to serve as the input to a Natural Language Processing (NLP) pipeline. A method to create browsable html from the xmlified pdf document is meant to assist quality checking in corpus preparation.

Installation
------------

Usage
-----

Contributing to package development
-----------------------------------

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
