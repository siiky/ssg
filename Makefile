SRC := \
    fold-args.scm \
    md2html.scm \
    ssg.scm \

all:
	@chicken-install -n

install:
	@chicken-install

list_deps:
	@ls -1 $(SRC) Makefile ssg.egg

.PHONY: all list_deps
