# Automatically Generated Makefile by EDE.
# For use with: make
# Relative File Name: cedet/Makefile
#
# DO NOT MODIFY THIS FILE OR YOUR CHANGES MAY BE LOST.
# EDE is the Emacs Development Environment.
# http://cedet.sourceforge.net/ede.shtml
#

top=../
ede_FILES=Project.ede Makefile

lisp_LISP=cedet-java.el cedet-android.el
EMACSFLAGS=-batch --no-site-file --eval '(setq debug-on-error t)'
EMACS=emacs
EMACSFLAGS=-batch --no-site-file --eval '(setq debug-on-error t)'
LOADDEFS=extension-loaddefs.el
require=$(foreach r,$(1),(require (quote $(r))))
LOADPATH= ./
LOADDIRS=.
VERSION=1.0
DISTDIR=$(top)cedet-extensions-$(VERSION)/cedet



all: lisp extension-loaddefs semantic ede

%.elc: %.el
	$(EMACS) $(EMACSFLAGS) $(addprefix -L ,$(LOADPATH)) --eval '(progn $(call require, $(PRELOADS)))' -f batch-byte-compile $^

.PHONY: lisp
lisp: $(addsuffix c, $(lisp_LISP))

.PHONY: clean-autoloads
clean-autoloads: 
	rm -f $(LOADDEFS)

.PHONY: extension-loaddefs
extension-loaddefs: 
	$(EMACS) $(EMACSFLAGS) $(addprefix -L ,$(LOADPATH)) --eval '(setq generated-autoload-file "$(abspath $(LOADDEFS))")' -f batch-update-autoloads $(abspath $(LOADDIRS))


.PHONY:semantic
semantic:
	$(MAKE) -C semantic

.PHONY:ede
ede:
	$(MAKE) -C ede

tags: 
	$(MAKE) -C semantic/ $(MFLAGS) $@
	$(MAKE) -C ede/ $(MFLAGS) $@


clean:
	rm -f *.elc

.PHONY: dist

dist: extension-loaddefs
	mkdir $(DISTDIR)
	cp $(lisp_LISP) extension-loaddefs.el $(ede_FILES) $(DISTDIR)
	$(MAKE) -C semantic $(MFLAGS) DISTDIR=$(DISTDIR)/semantic dist
	$(MAKE) -C ede $(MFLAGS) DISTDIR=$(DISTDIR)/ede dist

Makefile: Project.ede
	@echo Makefile is out of date!  It needs to be regenerated by EDE.
	@echo If you have not modified Project.ede, you can use ‘touch’ to update the Makefile time stamp.
	@false



# End of Makefile
