.SUFFIXES: .$(EXT) .asm .obj .exe
LANG=diy
EXT=diy
LIB=lib
UTIL=util # compiler library: lib$(LIB).a
RUN=run
EXS=exs # examples directory
ARCH=
AS=nasm -felf32
#ARCH=-DpfARM
#AS=as
CC=gcc
CFLAGS=-g -ansi -pedantic -DYYDEBUG $(ARCH)

$(LANG): $(LANG).y $(LANG).l $(LANG).brg
	make -C $(LIB)
	make -C $(RUN)
	byacc -dv $(LANG).y
	flex -l $(LANG).l
	pburg -T $(LANG).brg
	$(LINK.c) -o $(LANG) $(ARCH) -I$(LIB) lex.yy.c y.tab.c yyselect.c -L$(LIB) -l$(UTIL)

examples:: $(LANG)
	make -C $(EXS)

clean::
	make -C $(LIB) clean
	make -C $(RUN) clean
	make -C $(EXS) clean
	rm -f *.o $(LANG) lex.yy.c y.tab.c y.tab.h y.output yyselect.c *.asm *~
