CC = clang
CFLAGS = -Wall -Wextra -pedantic -Werror -std=c11
VPATH = ..
OBJ = test_data.o data.o

.PHONY: clean

test_data: $(OBJ)
	$(CC) -o test_data $(OBJ)
	
%.o: %.c
	$(CC) $(CFLAGS) -o $@ -c $<

test: test_data
	./test_data

clean:
	rm -f test_data *.o
