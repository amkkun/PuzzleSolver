SRCS = sudoku.c
EXES = $(SRCS:%.c=%)

CC = gcc
CFLAGS = -std=c99 -pedantic -Wall -Werror $(OPTFLAGS) $(DBGFLAGS)
OPTFLAGS = -O3
DBGFLAGS = # -g
CPPFLAGS =
LDFLAGS =

RM = rm -f

.PHONY: all clean allclean

all: $(EXES)

clean:
	$(RM) $(EXES)

allclean: clean
	$(RM) *~ a.out
	$(RM) -r *.dSYM


