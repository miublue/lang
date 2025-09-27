CC = cc
CFLAGS = -O2 -std=c89 -Wno-builtin-declaration-mismatch
BINDIR = /usr/local/bin
OUT = n

all:
	${CC} -o ${OUT} n.c ${CFLAGS}

install: ${OUT}
	strip -s ${OUT}
	mkdir -p ${BINDIR}
	install ${OUT} ${BINDIR}

uninstall:
	rm ${BINDIR}/${OUT}

.PHONY: all install uninstall
