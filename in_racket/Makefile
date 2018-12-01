.DEFAULT_GOAL := all

MAIN_FILES := $(wildcard ./*.rkt)

.PHONY: $(MAIN_FILES)

$(MAIN_FILES):
	racket $@

all: $(MAIN_FILES)
