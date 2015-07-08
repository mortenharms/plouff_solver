# MAKEFILE for plouff_solver

EXECUTABLE = plouff.x

#-----------------------------------
# set default compiler and other
#-----------------------------------
FC = gfortran
FFLAGS = -fbounds-check -Wall -Wtabs  -O0

#-----------------------------------
# list of allowed suffixes (first cleared)
#-----------------------------------
.SUFFIXES:
.SUFFIXES: .f .f95 .o

#-----------------------------------
# general compilation rules
#-----------------------------------
.f.o:
	$(FC) -c $(FFLAGS) $<
.f95.o:
	$(FC) -c $(FFLAGS) $<

#-----------------------------------
# targets
#-----------------------------------
.PHONY: run clean

$(EXECUTABLE): main.o mod_functions.o
	$(FC) -o $(EXECUTABLE) main.o mod_functions.o

run: $(EXECUTABLE)
	./$(EXECUTABLE)
	
plot: $(EXECUTABLE)
	./$(EXECUTABLE) && gnuplot sphere.plt 


clean:
	rm -f a.out $(EXECUTABLE) *.o *.mod

#-----------------------------------
# dependencies
#-----------------------------------
main.o: main.f95 mod_functions.o
mod_functions.o: mod_functions.f95
