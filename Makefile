NAME := vc-p4
VERSION := 0.2.1
DESCRIPTION := "Perforce integration for VC"

FULLNAME := "$(NAME)-$(VERSION)"

.PHONY: $(NAME)-pkg.el
.SUFFIXES:

$(FULLNAME).tar: $(FULLNAME)/
	tar cf $@ $^

$(FULLNAME)/: $(FULLNAME)/$(NAME)-pkg.el $(FULLNAME)/$(NAME).el $(FULLNAME)/p4-lowlevel.el

$(FULLNAME)/%: ./%
	test -d $$(dirname $@) || mkdir $$(dirname $@)
	cp $^ $@

$(NAME)-pkg.el:
	echo "(define-package \"$(NAME)\" \"$(VERSION)\" \"$(DESCRIPTION)\" '((emacs \"25\")))" > $@

clean:
	rm -rf $(NAME)-pkg.el $(NAME)-$(VERSION).tar $(FULLNAME)/
