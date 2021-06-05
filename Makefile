PROJECT_LOCATION := $(PWD)
SBCL_BINARY := /usr/bin/sbcl
BINS := $(PROJECT_LOCATION)/binary
OUT_BINARY := $(BINS)/funny
CACHE :=~/.cache/common-lisp
# TESTS := $(shell find $(PROJECT_LOCATION) -name "*.test" -printf "\\\"%p\\\" ")
.PHONY: build

build:
	@export LDA_HOME=$(PROJECT_LOCATION); \
        echo "(require :asdf) \
	      (format t \"~A\" (asdf:ensure-output-translations)) \
              (defconstant cl-user::+lda-home+ \"$(PROJECT_LOCATION)\") \
              (load \"$(PROJECT_LOCATION)/loader.lsp\") \
	      (save-lisp-and-die \"$(OUT_BINARY)\" :executable t)" | $(SBCL_BINARY) --disable-debugger
	@chmod +x $(OUT_BINARY)

clean:
	-@rm -rf $(BINS)/*

rebuild: clean build


# testing:
# 	@echo "(require :asdf) \
# 	       (push \"$(PROJECT_LOCATION)\" asdf:*central-registry*) \
# 	       (push \"$(PROJECT_LOCATION)lib/\" asdf:*central-registry*) \
# 	       (asdf:load-system :lisp-deps-analyzer) \
# 	       (translator/tests:run-tests \"$(PROJECT_LOCATION)/tests/CodeGen-tests/\") \
# 	       (sb-ext:exit)" | $(SBCL_BINARY)

# testlist:
# 	@echo "$(TESTS)"
