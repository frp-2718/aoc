CC = clang
CFLAGS = -Wall -Wextra -pedantic -Werror -std=c17 -g
VPATH = ..
EXE = test_data
OBJ = $(EXE).o data.o input.o

.PHONY: clean

$(EXE): $(OBJ)
	$(CC) -o $(EXE) $(OBJ)
	
%.o: %.c
	$(CC) $(CFLAGS) -o $@ -c $<

clean:
	rm -f $(EXE) *.o

