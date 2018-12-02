.DEFAULT_GOAL := all

MAIN_FILES := $(wildcard ./in_*)

.PHONY: $(MAIN_FILES)

$(MAIN_FILES):
	cd $@ && make | grep "Day [0-9]*"
all: $(MAIN_FILES)
