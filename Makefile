# Copyright (c) 2021-2023, Marek Gagolewski <https://www.gagolewski.com/>


.PHONY:  r check build clean purge test

.NOTPARALLEL: r check build clean purge test

PKGNAME="agop"

all: r

autoconf:
	Rscript -e "\
	    library('roxygen2');\
	    roxygenise(\
	        roclets=c('rd', 'collate', 'namespace')\
	    )"

r: autoconf
	R CMD INSTALL . --html

stop-on-utf8:
	# Stop if some files are not in ASCII:
	[ -z "`file -i DESCRIPTION configure configure.win \
	        NAMESPACE cleanup R/* src/* man/* inst/* tools/* | \
	    grep 'text/' | grep -v 'us-ascii' | tee /dev/stderr`" ]

build:
	cd .. && R CMD build ${PKGNAME}

check: stop-on-utf8 build
	cd .. && R CMD check `ls -t ${PKGNAME}*.tar.gz | head -1` --no-manual

check-cran: stop-on-utf8 build
	cd .. && R_DEFAULT_INTERNET_TIMEOUT=240 \
	    _R_CHECK_CRAN_INCOMING_REMOTE_=FALSE \
	    R CMD check `ls -t ${PKGNAME}*.tar.gz | head -1` --as-cran

clean:
	rm -rf .devel/sphinx/_build/
	rm -rf .devel/sphinx/rapi/
	rm -rf revdep/
	rm -rf src/*.o src/*.so

testthat:
	Rscript -e 'source(".devel/testthat/run_package_test.R")'

test: r testthat

purge: clean
# 	rm -f man/*.Rd
# 	rm -rf docs/
