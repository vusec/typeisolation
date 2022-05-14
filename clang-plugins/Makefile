


CC=g++

METALLOC_HOME=..
LIBSRCDIR=$(METALLOC_HOME)/staticlib
PREFIXDIR=$(METALLOC_HOME)/autosetup.dir/install/common
TARGETDIR=$(METALLOC_HOME)/autosetup.dir/framework/obj/clang-plugins

LLVM_CONFIG=$(PREFIXDIR)/bin/llvm-config


INCLUDES=-I.
CFLAGS=-c -Werror -Wall -std=c++0x -g `$(LLVM_CONFIG) --cxxflags` -O1 -Wno-comment -fPIC
LDFLAGS=-g -shared -fPIC
OBJDIR=$(TARGETDIR)/obj

EXE=$(TARGETDIR)/libplugins.so

SRCS    := $(wildcard *.cpp)
OBJS    := $(patsubst %.cpp,$(OBJDIR)/%.o,$(SRCS))
DEPS    := $(OBJS:.o=.d)

all: $(EXE) $(EXE2)

clean:
	rm -f $(OBJDIR)/*.o
	rm -f $(OBJDIR)/*.d
	rm -f $(EXE)

$(EXE): $(OBJS) | $(TARGETDIR)
	$(CC) $(OBJS) $(LDFLAGS) -o $@

-include $(DEPS)

$(OBJDIR)/%.o: %.cpp | $(OBJDIR)
	$(CC) $(INCLUDES) $(CFLAGS) -MMD -o $@ $<

$(OBJDIR):
	mkdir -p $(OBJDIR)

$(TARGETDIR):
	mkdir -p $(TARGETDIR)

