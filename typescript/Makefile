.DEFAULT_GOAL := all

MAIN_FILES := $(wildcard ./src/*.ts)

.PHONY: $(MAIN_FILES)

$(MAIN_FILES):
	yarn ts-node $@

all: $(MAIN_FILES)
