SRC := \
    ssg.scm \
    md2html.scm \

EXEC := ssg

all: ssg

ssg: ssg.scm md2html.scm
	@chicken-install -n

list_deps:
	@ls -1 $(SRC) Makefile ssg.egg

.PHONY: all
