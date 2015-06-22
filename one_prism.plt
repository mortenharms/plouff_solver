#
# GNUPLOT script for displaying 1 polygon
#

## display plot
set title "for testing purpose one prism"
splot 'output_1_prism.dat' with lines notitle

## save as postcript
#set terminal postscript 
#set output 'one_prism.ps'
replot

## save as png
#set terminal png size 400,300 enhanced font "Helvetica,20"
#set output 'one_prism.png'
#replot

pause -1

