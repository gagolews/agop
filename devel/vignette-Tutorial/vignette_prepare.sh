#!/bin/bash
gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 \
   -dPDFSETTINGS=/ebook \
   -sOutputFile=../../vignettes/agop-Tutorial.pdf \
   agop-Tutorial.pdf
