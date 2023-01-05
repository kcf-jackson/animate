# The file builds the app from the sketch R source files
build:
	Rscript inst/dev/build.R

copy:
	rsync -av --exclude=".*" ../animate/ ../../../git_repos/animate_master/animate/

cran-clean:
	rm -r ../animate-CRAN

cran-build:
	mkdir -p ../animate-CRAN
	cp DESCRIPTION ../animate-CRAN/DESCRIPTION
	cp NAMESPACE ../animate-CRAN/NAMESPACE
	cp .Rbuildignore ../animate-CRAN/.Rbuildignore
	cp -r R ../animate-CRAN/R
	cp -r man ../animate-CRAN/man
	cp -r inst ../animate-CRAN/inst
	cp LICENSE ../animate-CRAN/LICENSE
	cp LICENSE.md ../animate-CRAN/inst/LICENSE.note
	cp -r vignettes ../animate-CRAN/vignettes
	cd ../ && R CMD build animate-CRAN

cran-check:
	R CMD check --as-cran "../animate_0.3.9.3.tar.gz"

cran: cran-clean cran-build cran-check
