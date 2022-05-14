


CC=g++

METALLOC_HOME=..
LIBSRCDIR=$(METALLOC_HOME)/staticlib
PREFIXDIR=$(METALLOC_HOME)/autosetup.dir/install/common
TARGETDIR=$(METALLOC_HOME)/autosetup.dir/framework/obj/llvm-plugins
OBJDIR=$(TARGETDIR)/obj
LLVM_CONFIG=$(PREFIXDIR)/bin/llvm-config
GOLDINSTDIR=$(PREFIXDIR)

INCLUDES=-I. -I./utils/ -I$(LIBSRCDIR)/metalloc -I$(LIBSRCDIR)/metapagetable
CFLAGS=-c -Werror -Wall -std=c++11 -g `$(LLVM_CONFIG) --cxxflags|sed 's/-Wno-maybe-uninitialized//'` -O0 -fPIC
LDFLAGS=-g -shared -fPIC
LINKWITHGOLDFLAGS=-Wl,--no-undefined -L $(GOLDINSTDIR)/lib/ -l:LLVMgold.so -Wl,--rpath=$(GOLDINSTDIR)/lib

EXE=$(TARGETDIR)/libplugins.so
EXE2=$(TARGETDIR)/libplugins-opt.so

SUBDIRS := utils metachecker
SRCS_3  := $(wildcard *.cpp) $(foreach d,$(SUBDIRS),$(wildcard $d/*.cpp))
SRCS_2  := $(filter-out GlobalTracker.cpp,$(SRCS_3))
SRCS_1  := $(filter-out StackTracker.cpp,$(SRCS_2))
SRCS    := $(filter-out WriteTracker.cpp,$(SRCS_1))
OBJS    := $(patsubst %.cpp,$(OBJDIR)/%.o,$(SRCS))
DEPS    := $(OBJS:.o=.d)

all: directories $(EXE) $(EXE2)

clean:
	rm -rf $(OBJDIR)/*
	rm -f $(EXE)
	rm -f $(EXE2)

$(EXE): $(OBJS) | $(TARGETDIR)
	$(CC) $(OBJS) $(LDFLAGS) $(LINKWITHGOLDFLAGS) -o $@

$(EXE2): $(OBJS) | $(TARGETDIR)
	$(CC) $(OBJS) $(LDFLAGS) -o $@

-include $(DEPS)

$(OBJDIR)/%.o: %.cpp | $(OBJDIR)
	@mkdir -p $(@D)
	$(CC) $(INCLUDES) $(CFLAGS) -MMD -o $@ $<

.PHONY: directories
directories: $(OBJDIR) $(TARGETDIR)

$(OBJDIR):
	mkdir -p $(OBJDIR)

$(TARGETDIR):
	mkdir -p $(TARGETDIR)
