gfortran -ffree-line-length-none -c init_value_run_plotter_new.f90 -I. 
gfortran -O -o xplot_init_value_run_zonal2 init_value_run_plotter_new.o  -I. -I/Users/7yg/ornldev/Codes/silo-4.10.2/include -L/Users/7yg/ornldev/Codes/silo-4.10.2/lib -lsilo
