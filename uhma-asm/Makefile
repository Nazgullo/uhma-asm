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
       receipt.asm maturity.asm gateway.asm hub_client.asm narrate.asm

# MPNet semantic embeddings (768-dim → 8192-dim projection)
EMBED_SRCS = embed/matmul.asm embed/attention.asm embed/tokenize.asm embed/embed.asm
EMBED_OBJS = $(EMBED_SRCS:.asm=.o)

# Standalone hub server (separate binary)
HUB_TARGET = hub
HUB_SRCS = hub.asm

# Tools (separate binaries, replace Python/Bash scripts)
FEEDER_TARGET = tools/feeder
FEEDER_SRCS = tools/feeder.asm

MCP_TARGET = tools/mcp_server
MCP_SRCS = tools/mcp_server.asm

OBJS = $(SRCS:.asm=.o)

# GUI visualizer
GUI_TARGET = gui/uhma-viz
GUI_SRCS = gui/viz_main.asm gui/visualizer.asm gui/gfx.asm gui/mcp_client.asm
GUI_OBJS = $(GUI_SRCS:.asm=.o)

.PHONY: all clean run hub tools feeder mcp_server gui

all: $(TARGET) $(HUB_TARGET) tools

tools: $(FEEDER_TARGET) $(MCP_TARGET)

$(TARGET): $(OBJS)
	$(LD) $(LDFLAGS) -o $@ $^

$(HUB_TARGET): hub.o
	$(LD) $(LDFLAGS) -o $@ $^

$(FEEDER_TARGET): tools/feeder.o
	$(LD) $(LDFLAGS) -o $@ $^

$(MCP_TARGET): tools/mcp_server.o holo_mem.o format.o $(EMBED_OBJS)
	$(LD) $(LDFLAGS) -o $@ $^

# GUI linked with gcc (required for X11/libc)
$(GUI_TARGET): $(GUI_OBJS)
	gcc -o $@ $^ -lX11 -no-pie

gui: $(GUI_TARGET)

gui/%.o: gui/%.asm include/constants.inc
	$(NASM) $(NASMFLAGS) -o $@ $<

tools/%.o: tools/%.asm include/syscalls.inc
	$(NASM) $(NASMFLAGS) -o $@ $<

embed/%.o: embed/%.asm include/syscalls.inc
	$(NASM) $(NASMFLAGS) -o $@ $<

%.o: %.asm include/constants.inc include/syscalls.inc include/vsa_ops.inc
	$(NASM) $(NASMFLAGS) -o $@ $<

clean:
	rm -f $(OBJS) $(EMBED_OBJS) $(TARGET) $(HUB_TARGET) hub.o
	rm -f tools/*.o $(FEEDER_TARGET) $(MCP_TARGET)

run: $(TARGET)
	./$(TARGET)

feeder: $(FEEDER_TARGET)

mcp_server: $(MCP_TARGET)
