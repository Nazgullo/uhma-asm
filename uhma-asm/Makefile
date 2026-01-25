NASM = nasm
LD = ld
NASMFLAGS = -f elf64 -I include/
LDFLAGS = -z execstack

TARGET = uhma
SRCS = boot.asm io.asm format.asm signal.asm surface.asm repl.asm \
       vsa.asm vsa_ops.asm dispatch.asm emit.asm learn.asm hooks.asm \
       observe.asm modify.asm gate.asm decode.asm drives.asm \
       evolve.asm dreams.asm persist.asm introspect.asm trace.asm \
       symbolic.asm genes.asm factor.asm verify.asm presence.asm \
       receipt.asm maturity.asm

OBJS = $(SRCS:.asm=.o)

.PHONY: all clean run

all: $(TARGET)

$(TARGET): $(OBJS)
	$(LD) $(LDFLAGS) -o $@ $^

%.o: %.asm include/constants.inc include/syscalls.inc include/vsa_ops.inc
	$(NASM) $(NASMFLAGS) -o $@ $<

clean:
	rm -f $(OBJS) $(TARGET)

run: $(TARGET)
	./$(TARGET)
