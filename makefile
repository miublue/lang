CC = cc
CFLAGS = -O2 -ansi -std=c89 -Wno-builtin-declaration-mismatch
OUT = n

all: ${OUT}

.PHONY: ${OUT}
${OUT}:
	${CC} -o ${OUT} n.c ${CFLAGS}

.PHONY: install
install: ${OUT}
	install ${OUT} /usr/local/bin

.PHONY: uninstall
uninstall:
	rm /usr/local/bin/${OUT}

