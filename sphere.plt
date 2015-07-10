#
# GNUPLOT script for displaying the sphere
#

## display plot
set zlabel "Tiefe [m]"
set xlabel "Entfernung [m]"
set ylabel "Entfernung [m]"

set view 60,30
set terminal postscript portrait enhanced mono dashed lw 1 "Helvetica" 14
set output "sphere.ps"
set view equal xyz

splot 'output_sphere.dat' with lines notitle


## save as png
#set terminal png size 400,300 enhanced font "Helvetica,20"
#set output 'sphere.png'
#replot


