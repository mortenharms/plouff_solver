#
# GNUPLOT script for displaying 1 polygon
#

## display plot
set terminal postscript portrait enhanced mono dashed lw 1 "Helvetica" 14
set output "one_prism.ps"
set xlabel "Entfernung [m]"
set ylabel "Entfernung [m]"
set zlabel "Tiefe [m]"
splot 'output_1_prism.dat' with lines notitle





