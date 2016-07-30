# Note you need gnuplot 4.4 for the pdfcairo terminal.

# set terminal pdfcairo font "Gill Sans,9" linewidth 4 rounded fontscale 1.0
set terminal pdfcairo font "Gill Sans,7" linewidth 1 rounded fontscale 1.0

# Line style for axes
set style line 80 lt rgb "#808080"

# Line style for grid
set style line 81 lt 0  # dashed
set style line 81 lt rgb "#808080"  # grey

set grid back linestyle 81
set border 3 back linestyle 80

set style line 1 lc rgb "#A00000" lw 2 pt 7 ps 0.5
set style line 2 lc rgb "#00A000" lw 2 pt 7 ps 0.5
set style line 3 lc rgb "#5060D0" lw 2 pt 7 ps 0.5

set output outputname
set xlabel "Partition Probability"
set ylabel "Divergence"

set key bottom right

# set yrange [0:*]

# Titles with spaces are not allowed
# These titles should be separated by "_" and here we replace by " "
pretty(title) = system("echo ".title." | sed 's/_/ /g'")

# Input file contains comma-separated values fields
set datafile separator ","

plot for [i=0:words(inputnames) - 1] word(inputnames, i + 1) using 1:2 title pretty(word(titles, i + 1)) with linespoints ls i + 1
