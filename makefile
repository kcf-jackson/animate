# The file builds the app from the sketch R source files
build:
	Rscript inst/dev/build.R

copy:
	rsync -av --exclude=".*" ../animate/ ../../../git_repos/animate_master/animate/
