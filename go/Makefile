.DEFAULT_GOAL := all

GO_FILES := $(wildcard ./**/*.go)

.PHONY: $(GO_FILES)

$(GO_FILES):
	go run $@

all: $(GO_FILES)
