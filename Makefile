IRPF90 = irpf90 --align=32 #-a -d# --no_directives #-d --align=32 #-a -d
FC     = ifort -openmp
FCFLAGS= -g -O3 -i8 -axAVX #-traceback -C #-heap-arrays

SRC=
OBJ=
LIB= -mkl=sequential

include irpf90.make

irpf90.make: $(wildcard *.irp.f)
	$(IRPF90)
