# Gnuplotskript

set terminal postscript portrait enhanced mono dashed lw 1 "Helvetica" 14
set output "error_edges.ps"
set xlabel "Anzahl der Ecken"
set ylabel "Differenz zwischen numerischer und analytischer Loesung"
plot 'differenz_edges.dat' with lines

