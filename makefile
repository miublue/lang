CC = cc
CFLAGS = -O2 -ansi -std=c89 -Wno-builtin-declaration-mismatch

all: n

.PHONY: n
n:
	${CC} -o n n.c ${CFLAGS}

