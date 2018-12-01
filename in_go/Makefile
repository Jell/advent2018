.DEFAULT_GOAL := all

MAIN_FILES := $(wildcard ./**/main.go)

.PHONY: $(MAIN_FILES)

$(MAIN_FILES):
	go run $@

all: $(MAIN_FILES)
