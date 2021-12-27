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

.PHONY:
all: new-dist

.PHONY:
clean:
	-rm quicklisp-sources.txt dist.txt dist-url.txt quicklisp-systems.txt systems.txt version.txt *.fasl

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

systems.txt: quicklisp-systems.txt quicklisp-sources.txt
	sbcl --script indexer.lisp systems.txt quicklisp-systems.txt quicklisp-sources.txt
	@for prj in $(projects)/* ; do \
	  cat $$prj >> systems.txt
	done

.PHONY:
prepare-dist:
	-mkdir -p dists
	-rm systems.txt version.txt

.PHONY:
.ONESHELL:
new-dist: prepare-dist systems.txt
	@read -p "Enter new version: " version
	cp systems.txt dists/system-$$version.txt
	echo "$$version" | sed -E s/-\|\\.//g  >> version.txt
