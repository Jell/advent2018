.DEFAULT_GOAL := all

MAIN_FILES := $(wildcard ./*.cr)

.PHONY: $(MAIN_FILES)

$(MAIN_FILES):
	crystal build $@ --release -o ./build/$(basename $@)
	./build/$(basename $@)

all: $(MAIN_FILES)
