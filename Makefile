IRPF90 = ${HOME}/irpf90/bin/irpf90 --align=32 #-a -d# --no_directives #-d --align=32 #-a -d
FC     = ifort -openmp
FCFLAGS= -g -O2 -axAVX  #-traceback -C #-heap-arrays

SRC=
OBJ=
LIB= -mkl=sequential

include irpf90.make

irpf90.make: $(wildcard *.irp.f)
	$(IRPF90)
