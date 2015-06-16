
.PHONY: all clean check

TEST_PROGRAM = test.native
TEST_FILES   = $(wildcard t/*.ml)
SRC_FILES    = $(wildcard *.ml)
BUILD        = ocamlbuild
PKGS         = oUnit,str,num # add packages here
BUILD_OPT    = -use-ocamlfind -pkgs $(PKGS) -cflags "-annot -w A-4-33-40-41-42-43-34-44"

all: $(TEST_PROGRAM)

clean:
	$(BUILD) -clean

$(TEST_PROGRAM): $(SRC_FILES) $(TEST_FILES)
	$(BUILD) -I t $(BUILD_OPT) $(TEST_PROGRAM)

check: $(TEST_PROGRAM)
	./$(TEST_PROGRAM)
