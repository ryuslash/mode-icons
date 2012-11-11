EMACS = emacs

version := $(shell $(EMACS) -Q -batch -visit mode-icons.el -eval \
	"(progn (require 'package) (princ (elt (package-buffer-info) 3)))")
pkgname := mode-icons-$(version)
sources := mode-icons.el $(wildcard icons/*.*)
dests := $(addprefix $(pkgname)/,$(sources))

all:
dist: $(pkgname).tar

$(pkgname).tar: $(pkgname)/mode-icons-pkg.el $(dests)
	tar cf $(pkgname).tar $(pkgname)

$(pkgname)/mode-icons-pkg.el: mode-icons.el
	if [ ! -d "$(pkgname)" ]; then mkdir $(pkgname); fi
	$(EMACS) -batch -Q -script "$(CURDIR)/scripts/genpkg.el" > \
		$(pkgname)/mode-icons-pkg.el

$(dests): $(pkgname)/%:
	if [ ! -d "$(dir $@)" ]; then mkdir -p $(dir $@); fi
	cp $* $@

clean:
	rm -f $(pkgname).tar
	rm -rf $(pkgname)/
