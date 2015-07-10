# Gnuplotskript

set terminal postscript portrait enhanced mono dashed lw 1 "Helvetica" 14
set output "error_layers.ps"
set xlabel "Anzahl der Schichten"
set ylabel "Differenz zwischen numerischer und analytischer Loesung"
plot 'differenz_layers.dat' with lines

