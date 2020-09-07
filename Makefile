SRC := \
	ssg.condition.scm          \
	ssg.converters.lowdown.scm \
	ssg.converters.pandoc.scm  \
	ssg.converters.scm         \
	ssg.css.scm                \
	ssg.date.scm               \
	ssg.feed.atom.scm          \
	ssg.feed.scm               \
	ssg.index.scm              \
	ssg.result.scm             \
	ssg.scm                    \
	ssg.site.scm               \

all:
	@chicken-install -n
	@echo DONE

install:
	@chicken-install

list_deps:
	@ls -1 $(SRC) Makefile ssg.egg

.PHONY: all install list_deps
