CC = cc
CFLAGS = -O2 -std=c89
OUT = n
PREFIX = /usr/local/

all:
	${CC} -o ${OUT} n.c ${CFLAGS}

install: ${OUT}
	mkdir -p ${PREFIX}/bin
	install -s ${OUT} ${PREFIX}/bin

uninstall:
	rm ${PREFIX}/bin/${OUT}

.PHONY: all install uninstall
