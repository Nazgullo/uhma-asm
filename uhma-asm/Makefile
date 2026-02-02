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
       receipt.asm maturity.asm channels.asm hub_client.asm narrate.asm \
       holo_mem.asm

# Standalone hub server (separate binary)
HUB_TARGET = hub
HUB_SRCS = hub.asm

# Tools (separate binaries, replace Python/Bash scripts)
FEEDER_TARGET = tools/feeder
FEEDER_SRCS = tools/feeder.asm

MCP_TARGET = tools/mcp_server
MCP_SRCS = tools/mcp_server.asm

OBJS = $(SRCS:.asm=.o)

.PHONY: all clean run hub tools feeder mcp_server

all: $(TARGET) $(HUB_TARGET) tools

tools: $(FEEDER_TARGET) $(MCP_TARGET)

$(TARGET): $(OBJS)
	$(LD) $(LDFLAGS) -o $@ $^

$(HUB_TARGET): hub.o
	$(LD) $(LDFLAGS) -o $@ $^

$(FEEDER_TARGET): tools/feeder.o
	$(LD) $(LDFLAGS) -o $@ $^

$(MCP_TARGET): tools/mcp_server.o
	$(LD) $(LDFLAGS) -o $@ $^

tools/%.o: tools/%.asm include/syscalls.inc
	$(NASM) $(NASMFLAGS) -o $@ $<

%.o: %.asm include/constants.inc include/syscalls.inc include/vsa_ops.inc
	$(NASM) $(NASMFLAGS) -o $@ $<

clean:
	rm -f $(OBJS) $(TARGET) $(HUB_TARGET) hub.o
	rm -f tools/*.o $(FEEDER_TARGET) $(MCP_TARGET)

run: $(TARGET)
	./$(TARGET)

feeder: $(FEEDER_TARGET)

mcp_server: $(MCP_TARGET)
