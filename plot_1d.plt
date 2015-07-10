#
# GNUPLOT script for displaying 1 polygon
#

## display plot
set terminal postscript portrait enhanced mono dashed lw 1 "Helvetica" 14
set output "1d_profil.ps"
set xlabel "Entfernung [m]"
set ylabel "Anomalie [mgal]"
plot 'profil_1D.dat' with lines, 'profil_1D_anal.dat' with lines



