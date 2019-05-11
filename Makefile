SRC := \
    ssg.scm \
    md2html.scm \

all:
	@chicken-install -n

list_deps:
	@ls -1 $(SRC) Makefile ssg.egg

.PHONY: all list_deps
