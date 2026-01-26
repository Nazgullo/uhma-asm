# GUI Makefile for UHMA GUI (MCP wrapper)
# No longer links UHMA directly - communicates via MCP server
NASM = nasm
CC = gcc
LD = gcc
NASMFLAGS = -f elf64 -I ../include/
LDFLAGS = -lX11 -lc -no-pie

TARGET = uhma-viz
SRCS = gfx.asm visualizer.asm viz_main.asm mcp_client.asm
OBJS = $(SRCS:.asm=.o)

.PHONY: all clean

all: $(TARGET)

$(TARGET): $(OBJS)
	$(LD) -o $@ $^ $(LDFLAGS)

%.o: %.asm
	$(NASM) $(NASMFLAGS) -o $@ $<

clean:
	rm -f $(OBJS) $(TARGET)
