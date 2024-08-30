LISP ?= sbcl
ASD := bks.asd
ASD_PATH := $(shell pwd)/$(ASD)
BINARY := bks
INSTALL_PATH := ~/.local/bin
DEFAULT_CONFIG_PATH := ~/.local/share/bks/default
CONFIG_FILE := bksrc.lisp

load:
	rlwrap -c $(LISP) --eval "(asdf:load-asd #P\"$(ASD_PATH)\")" \
		--eval '(ql:quickload :$(BINARY))' \
		--eval '(if (cl-ppcre:all-matches "Vlime" (format nil "~A" (swank:list-threads))) 0 (vlime:main))'

build:
	$(LISP) --eval "(asdf:load-asd #P\"$(ASD_PATH)\")" \
		--eval '(ql:quickload :$(BINARY))' \
		--eval "(sb-ext:save-lisp-and-die #P\"bin/$(BINARY)\" :toplevel #'main :executable t :compression t)" \
		--eval '(quit)'

install:
	install -vD bin/$(BINARY) $(INSTALL_PATH)/$(BINARY)
	install -vD share/$(CONFIG_FILE) $(DEFAULT_CONFIG_PATH)/$(CONFIG_FILE)

clean:
	rm -v bin/$(BINARY)

uninstall:
	rm -v $(INSTALL_PATH)/$(BINARY)
