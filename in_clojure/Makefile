.DEFAULT_GOAL := all

MAIN_FILES := $(wildcard ./*.clj)

.PHONY: $(MAIN_FILES)

$(MAIN_FILES):
	clj -J-ss10000k $@

all: $(MAIN_FILES)

repl:
	clj -J-ss10000k -J-Dclojure.server.myrepl="{:port 5555,:accept,clojure.core.server/repl}"
