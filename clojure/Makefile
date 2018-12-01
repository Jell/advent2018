.DEFAULT_GOAL := all

MAIN_FILES := $(wildcard ./*.clj)

.PHONY: $(MAIN_FILES)

$(MAIN_FILES):
	clj $@

all: $(MAIN_FILES)

repl:
	clj -J-Dclojure.server.myrepl="{:port 5555,:accept,clojure.core.server/repl}"
