
HSFLAGS = -Wall
SRC_FOLDER = src
BUILD_FOLDER = build
TEST_FOLDER = test
BIN_NAME = flp21-fun
ARCHIVE_NAME = flp-fun-xkrejc69.zip

.PHONY: clean test

$(BIN_NAME): $(SRC_FOLDER)/*.hs
	ghc $(HSFLAGS) $^ -odir $(BUILD_FOLDER) -hidir $(BUILD_FOLDER) -o flp21-fun

clean:
	rm -f $(BIN_NAME) $(ARCHIVE_NAME) build/*

test: $(BIN_NAME)
	python3 $(TEST_FOLDER)/test.py

zip:
	zip $(ARCHIVE_NAME) src/* Makefile README.md test/*
