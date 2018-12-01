.DEFAULT_GOAL := all

MAIN_FILES := $(wildcard ./*.rb)

.PHONY: $(MAIN_FILES)

$(MAIN_FILES):
	ruby $@

all: $(MAIN_FILES)
