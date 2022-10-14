programName = TEST

FC = gfortran
#OPT = -O3 -Mflushz -Mfunc32 -Kieee -Minfo #NVIDIA compiler options
#OPT = -g -O3 -Mfunc32 -Kieee -mp=gpu -gpu=flushz -Minfo=mp -Kieee#NVIDIA compiler options
OPT = -g -O3 -ffree-line-length-0 -cpp -DGNU_OACC -fopenmp -foffload="-O3 -lgfortran -lgomp -lm" -foffload=nvptx-none # Gfortran compiler options
#OPT = -O3 -g -march=core-avx2 -fma -qopt-report0 -ftz -align all -fno-alias -align array32byte -traceback -assume realloc_lhs -fpe3 -fp-model consistent -assume noold_maxminloc -align dcommons # Ifort compiler options
OBJ = test.o 

%.o : %.F90
	$(FC) -c -o $@ $^ $(OPT)

$(programName) : $(OBJ)
	$(FC) -o $@ $^ $(OPT)

clean :
	rm -rf *.o *.mod $(programName) *~ *.out output_data s_*

