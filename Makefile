#!/usr/bin/make
#
# Makefile for RIVIERA

EMACS   = emacs
CP      = cp -p
RM      = rm -f
INSTALL = install

.el.elc:
	$(EMACS) -Q --batch --eval "(add-to-list 'load-path \".\")" --eval '(byte-compile-file "$<")'

SRCS    = dbgp.el riviera.el
OBJS    = $(SRCS:%.el=%.elc)
IMGDIR  = tree-widget/riviera
IMGS    = $(wildcard $(IMGDIR)/*.png)

GUESS-SITELISP := $(shell $(EMACS) -Q --batch --eval '	       \
  (let (tbl)						       \
    (mapc (lambda (path)				       \
	    (if (string-match "^\\(.*/site-lisp\\)\\b/?" path) \
		(let* ((spath (match-string 1 path))	       \
		       (pair (assoc spath tbl)))	       \
		  (if pair				       \
		      (setcdr pair (1+ (cdr pair)))	       \
		    (setq tbl (cons (cons spath 1) tbl))))))   \
	  load-path)					       \
    (princ (or (car (car (sort tbl (lambda (a b)	       \
				     (> (cdr a) (cdr b))))))   \
	       "")))')

ifndef SITELISP
SITELISP := $(GUESS-SITELISP)
ifeq ($(SITELISP), nil)
$(error Cannot find appropriate site-lisp directory.)
endif
endif


DEST = $(SITELISP)/riviera
DEST-IMG = $(DEST)/tree-widget/riviera

.PHONY: all
all: $(OBJS)

.PHONY: install
install: all
	$(INSTALL) -m 755 -d $(DEST)
	$(INSTALL) -m 644 $(SRCS) $(OBJS) $(DEST)
	$(INSTALL) -m 755 -d $(DEST-IMG)
	$(INSTALL) -m 644 $(IMGS) $(DEST-IMG)

.PHONY: clean
clean:
	$(RM) $(OBJS)
