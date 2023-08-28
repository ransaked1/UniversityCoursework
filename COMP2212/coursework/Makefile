#!/bin/bash
MAIN_NAME = Tsl
TOKENIZER_NAME = BitGridTokenizer
PARSER_NAME = BitGridParser
TYPES_NAME = BitGridTypeChecker

TOKENS_NAME = BitGridTokens.hs
GRAMMAR_NAME = BitGridGrammar.hs
TYPE_CHECKER_NAME = BitGridTypeChecker.hs

MAIN = Tsl.hs
TOKENIZER_MAIN = BitGridTokenizer.hs
PARSER_MAIN = BitGridParser.hs
TYPES_MAIN = BitGridTypes.hs

RM=rm -rf

all: tokens parser
	 @ghc $(MAIN)
	 @make clean
	 @echo "MAKE ALL IS DONE"

tokens:
		@$(RM) $(TOKENS_NAME)
		@alex BitGridTokens.x -o $(TOKENS_NAME)
		@echo "ALEX TOKEN PARSER REGENERATED"

parser:
		@$(RM) $(GRAMMAR_NAME)
		@happy BitGridGrammar.y -o $(GRAMMAR_NAME) -i
		@echo "HAPPY GRAMMAR PARSER REGENERATED"

types: tokens parser
		@$(RM) $(TYPES_NAME)
		@ghc -o $(TYPES_NAME) $(TOKENS_NAME) $(GRAMMAR_NAME) $(TYPE_CHECKER_NAME) $(TYPES_MAIN)
		@make clean
		@echo "TYPE CHECKER REGENERATED"

setup:
	@ghc $(MAIN)
	@cp progs/pr1.tsl automarker/pr1
	@cp progs/pr2.tsl automarker/pr2
	@cp progs/pr3.tsl automarker/pr3
	@cp progs/pr4.tsl automarker/pr4
	@cp progs/pr5.tsl automarker/pr5
	@cp Tsl automarker/
	@chmod 777 automarker/Tsl
	@echo "SETUP COMPLETED"

test: run_tokens run_parser run_types

run_tokens: tokens
	@ghc -o $(TOKENIZER_NAME) $(TOKENS_NAME) $(TOKENIZER_MAIN)
	@make clean
	@echo ""
	@echo "TESTING TOKENIZER:"
	@echo "TOKENIZING PROBLEM 1:"
	@./$(TOKENIZER_NAME) progs/pr1.tsl
	@echo ""
	@echo "TOKENIZING PROBLEM 2:"
	@./$(TOKENIZER_NAME) progs/pr2.tsl
	@echo ""
	@echo "TOKENIZING PROBLEM 3:"
	@./$(TOKENIZER_NAME) progs/pr3.tsl
	@echo ""
	@echo "TOKENIZING PROBLEM 4:"
	@./$(TOKENIZER_NAME) progs/pr4.tsl
	@echo ""
	@echo "TOKENIZING PROBLEM 5:"
	@./$(TOKENIZER_NAME) progs/pr5.tsl
	@echo ""
	@$(RM) $(TOKENIZER_NAME)

run_parser: parser
	@ghc -o $(PARSER_NAME) $(GRAMMAR_NAME) $(PARSER_MAIN)
	@make clean
	@echo ""
	@echo "TESTING PARSING:"
	@echo "PARSING PROBLEM 1:"
	@./$(PARSER_NAME) progs/pr1.tsl
	@echo ""
	@echo "PARSING PROBLEM 2:"
	@./$(PARSER_NAME) progs/pr2.tsl
	@echo ""
	@echo "PARSING PROBLEM 3:"
	@./$(PARSER_NAME) progs/pr3.tsl
	@echo ""
	@echo "PARSING PROBLEM 4:"
	@./$(PARSER_NAME) progs/pr4.tsl
	@echo ""
	@echo "PARSING PROBLEM 5:"
	@./$(PARSER_NAME) progs/pr5.tsl
	@echo ""
	@$(RM) $(PARSER_NAME)

run_types: types
	@echo ""
	@echo "TESTING TYPE CHECKING"
	@echo "TYPE CHECKING PROBLEM 1:"
	@./$(TYPES_NAME) progs/pr1.tsl
	@echo ""
	@echo "TYPE CHECKING PROBLEM 2:"
	@./$(TYPES_NAME) progs/pr2.tsl
	@echo ""
	@echo "TYPE CHECKING PROBLEM 3:"
	@./$(TYPES_NAME) progs/pr3.tsl
	@echo ""
	@echo "TYPE CHECKING PROBLEM 4:"
	@./$(TYPES_NAME) progs/pr4.tsl
	@echo ""
	@echo "TYPE CHECKING PROBLEM 5:"
	@./$(TYPES_NAME) progs/pr5.tsl
	@echo ""
	@$(RM) $(TYPES_NAME)

clean:
	@$(RM) *.o
	@$(RM) *.hi
	@$(RM) *.info
	@echo "MAKE CLEAN IS DONE"

fclean: clean
	@$(RM) $(MAIN_NAME)
	@$(RM) $(TOKENIZER_NAME)
	@$(RM) $(PARSER_NAME)
	@$(RM) $(TYPES_NAME)
	@$(RM) automarker/Tsl
	@echo "MAKE FCLEAN IS DONE"

re: fclean all
	@echo "MAKE RE IS DONE"