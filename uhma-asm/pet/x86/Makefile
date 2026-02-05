NASM = nasm
LD = gcc
NASMFLAGS = -f elf64
LDFLAGS = -lX11 -lc -lm -no-pie

# GUI with creature in carousel
TARGET = uhma-pet
SRCS = viz_main.asm visualizer.asm gfx.asm creature.asm render.asm mcp_client.asm
OBJS = $(SRCS:.asm=.o)

# Standalone creature (no GUI)
TARGET2 = creature-standalone
SRCS2 = main.asm gfx.asm creature.asm render.asm
OBJS2 = $(SRCS2:.asm=.o)

.PHONY: all clean run standalone

all: $(TARGET)

$(TARGET): $(OBJS)
	$(LD) -o $@ $^ $(LDFLAGS)

standalone: $(OBJS2)
	$(LD) -o $(TARGET2) $^ $(LDFLAGS)

%.o: %.asm constants.inc
	$(NASM) $(NASMFLAGS) -o $@ $<

run: $(TARGET)
	./$(TARGET)

clean:
	rm -f *.o $(TARGET) $(TARGET2)
