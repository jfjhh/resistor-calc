#!/usr/bin/gnuplot
reset
unset key
set terminal pngcairo size 1024,768 font "Bitstream Vera Sans Mono,10"
set output "out.png"

set title "Limited E12 Resistor Combination Integer-Valued Differences — Alex Striff"
set xlabel "Relative Difference (%)"
set ylabel "Frequency"
set xrange [0:15]
set xtics border nomirror center scale 0 0,1,15
set ytics border nomirror scale 1.5

Min=0
binwidth=0.1
bin(x) = binwidth*(floor((x-Min)/binwidth)+0.5) + Min

set boxwidth binwidth
set style fill solid 0.42
set style line 1 linewidth 2 linecolor rgb "green"

set datafile separator "\t"
plot "values.dat" using (bin($7)):(1.0) smooth freq with boxes ls 1

