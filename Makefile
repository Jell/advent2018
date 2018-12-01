.DEFAULT_GOAL := all

MAIN_FILES := $(wildcard ./in_*)

.PHONY: $(MAIN_FILES)

$(MAIN_FILES):
	cd $@ && make

all: $(MAIN_FILES)
