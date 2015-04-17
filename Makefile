IRPF90 = irpf90 --align=32 #-a -d# --no_directives #-d --align=32 #-a -d
FC     = ifort -openmp
FCFLAGS= -i8 -O3 -axAVX,SSE4.2  -g -traceback -C #-heap-arrays

SRC=
OBJ=
LIB= -mkl=sequential

include irpf90.make

irpf90.make: $(wildcard *.irp.f)
	$(IRPF90)
