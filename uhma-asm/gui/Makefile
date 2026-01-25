# GUI Makefile for UHMA Visualizer
NASM = nasm
CC = gcc
LD = gcc
NASMFLAGS = -f elf64 -I ../include/
LDFLAGS = -lX11 -lc -no-pie

TARGET = uhma-viz
SRCS = gfx.asm visualizer.asm viz_main.asm
OBJS = $(SRCS:.asm=.o)

# Link with main UHMA objects (except boot.o which has _start)
UHMA_OBJS = ../io.o ../format.o ../signal.o ../surface.o ../repl.o \
            ../vsa.o ../dispatch.o ../emit.o ../learn.o ../hooks.o \
            ../observe.o ../modify.o ../gate.o ../decode.o ../drives.o \
            ../evolve.o ../dreams.o ../persist.o ../introspect.o \
            ../trace.o ../symbolic.o

.PHONY: all clean

all: $(TARGET)

$(TARGET): $(OBJS) $(UHMA_OBJS)
	$(LD) -o $@ $^ $(LDFLAGS)

%.o: %.asm
	$(NASM) $(NASMFLAGS) -o $@ $<

# Ensure UHMA objects are built
$(UHMA_OBJS):
	$(MAKE) -C ..

clean:
	rm -f $(OBJS) $(TARGET)
