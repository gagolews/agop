#!/bin/bash
gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 \
   -dPDFSETTINGS=/ebook -sOutputFile=../../inst/doc/agop-Tutorial.pdf agop-Tutorial.pdf
