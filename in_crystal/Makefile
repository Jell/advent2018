.DEFAULT_GOAL := all

MAIN_FILES := $(wildcard ./*.cr)

.PHONY: $(MAIN_FILES)

$(MAIN_FILES):
	crystal run $@

all: $(MAIN_FILES)
