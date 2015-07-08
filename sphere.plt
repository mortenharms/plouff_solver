#
# GNUPLOT script for displaying the sphere
#

## display plot
set title "for testing purpose the sphere"
set view 90,0

set view equal xyz

splot 'output_sphere.dat' with lines notitle

## save as postcript
set terminal postscript 
set output 'sphere.ps'
replot

## save as png
#set terminal png size 400,300 enhanced font "Helvetica,20"
#set output 'sphere.png'
#replot

pause -1

