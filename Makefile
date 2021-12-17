# clm-projects

.POSIX:
.SUFFIXES:

# variables

# paths
scrdir=.
ql_projects=$(srcdir)/quickklisp-projects/projects
projects=$(srcdir)/projects

# programs
INSTALL=/usr/bin/install
INSTALL_PROGRAM=$(INSTALL)
INSTALL_DATA=$(INSTALL) -m 644
LS=/usr/local/bin/gls
MAKEINFO=/usr/local/bin/makeinfo

all:

clean:

.PHONY: all clean
