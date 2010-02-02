# Target: ARC embedded system
TDEPFILES= arc-tdep.o arc-jtag.o arc-jtag-tdep.o arc-jtag-ops.o arc-jtag-actionpoints.o
DEPRECATED_TM_FILE= tm-embed.h
SIM_OBS = remote-sim.o
SIM = ../sim/arc/libsim.a
