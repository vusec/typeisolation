CC=clang
METAPAGETABLEDIR=../metapagetable
INCLUDES=-I.
CFLAGS=-c -Werror -Wall -flto -fPIC -std=gnu11
OBJDIR=./obj

ifeq ($(DISABLE_OPT),1)
	CFLAGS += -O0
else
	CFLAGS += -O3
endif

EXE=$(OBJDIR)/libtypesafestack.a

SRCS    := $(wildcard *.c)
OBJS    := $(patsubst %.c,$(OBJDIR)/%.o,$(SRCS))
DEPS    := $(OBJS:.o=.d)

all: directories $(EXE) $(EXE2)

clean:
	rm -f $(OBJS)
	rm -f $(DEPS)
	rm -f $(EXE)

$(EXE): $(OBJS) directories
	llvm-ar crv $@ $(OBJS)
	llvm-ranlib $@

-include $(DEPS)

$(OBJDIR)/%.o: %.c directories
	$(CC) $(INCLUDES) $(CFLAGS) -MMD -o $@ $< 

directories:
	mkdir -p $(OBJDIR)

