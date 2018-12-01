.DEFAULT_GOAL := all

MAIN_FILES := $(wildcard ./in_*)

.PHONY: $(MAIN_FILES)

$(MAIN_FILES):
	cd $@ && make
	printf "\n\n\n\n"

all: $(MAIN_FILES)
