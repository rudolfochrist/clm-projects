# clm-projects

.POSIX:
.SUFFIXES:

# variables
quicklisp_dist = https://beta.quicklisp.org/dist/quicklisp.txt

# paths
srcdir != pwd
ql_projects = $(srcdir)/quicklisp-projects/projects
projects = $(srcdir)/projects

# programs
INSTALL=/usr/bin/install
INSTALL_PROGRAM=$(INSTALL)
INSTALL_DATA=$(INSTALL) -m 644
LS=/usr/local/bin/gls
MAKEINFO=/usr/local/bin/makeinfo

all: quicklisp-sources.txt quicklisp-systems.txt

clean:
	-rm quicklisp-sources.txt dist.txt dist-url.txt quicklisp-systems

quicklisp-sources.txt:
	@for proj in $(ql_projects)/* ; do \
	  name=`basename $$proj` ; \
	  content=`cat $$proj/source.txt | cut -d ' ' -f 2 | head -n 1` ; \
	  echo "$$name $$content" >> quicklisp-sources.txt ;\
	done

dist.txt:
	$(shell curl $(quicklisp_dist) -o dist.txt)

dist-url.txt: dist.txt
	$(shell cat $< | grep "system-index-url" | cut -d ' ' -f 2 > dist-url.txt)

quicklisp-systems.txt: dist-url.txt
	$(shell curl $(shell cat $<) -o quicklisp-systems.txt)

.PHONY: all clean
