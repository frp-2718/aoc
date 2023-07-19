CC = clang
CFLAGS = -Wall -Wextra -pedantic -Werror -std=c17 -g
VPATH = utils
EXE = 14
OBJ = $(EXE).o data.o input.o

.PHONY: clean

$(EXE): $(OBJ)
	$(CC) -o $(EXE) $(OBJ)
	
%.o: %.c
	$(CC) $(CFLAGS) -o $@ -c $<

clean:
	rm -f $(EXE) *.o

# https://perso.univ-lyon1.fr/jean-claude.iehl/Public/educ/Makefile.html
