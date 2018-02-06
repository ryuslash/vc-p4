NAME=vc-p4
VERSION=0.1.0
DESCRIPTION="Perforce integration for VC"

.PHONY: vc-p4-pkg.el

$(NAME)-$(VERSION).tar: $(NAME)-pkg.el $(NAME).el p4-lowlevel.el
	tar cf $@ --xform="s/^/$(NAME)-$(VERSION)\//" $^

$(NAME)-pkg.el:
	echo "(define-package \"$(NAME)\" \"$(VERSION)\" \"$(DESCRIPTION)\")" > $@

clean:
	rm -f $(NAME)-pkg.el $(NAME)-$(VERSION).tar
