.DEFAULT_GOAL := all

MAIN_FILES := $(wildcard ./**/main.go)

.PHONY: $(MAIN_FILES)

$(MAIN_FILES):
	cd $(dir $@) && go run $(notdir $@)

all: $(MAIN_FILES)
