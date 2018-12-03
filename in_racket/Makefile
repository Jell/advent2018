.DEFAULT_GOAL := all

MAIN_FILES := $(wildcard ./*.rkt)

.PHONY: $(MAIN_FILES)

deps:
	raco pkg install --auto --skip-installed threading

$(MAIN_FILES): deps
	racket $@

all: $(MAIN_FILES)
