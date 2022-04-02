
HSFLAGS = -Wall
SRC_FOLDER = src
BUILD_FOLDER = build
TEST_FOLDER = test
BIN_NAME = flp21-fun

.PHONY: clean test

$(BIN_NAME): $(SRC_FOLDER)/*.hs
	ghc $(HSFLAGS) $^ -odir $(BUILD_FOLDER) -hidir $(BUILD_FOLDER) -o flp21-fun

clean:
	rm -f flp21-fun build/*

test: $(BIN_NAME)
	python3 $(TEST_FOLDER)/test.py
