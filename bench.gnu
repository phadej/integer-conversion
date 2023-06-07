stats 'bench.txt'

unset logscale xy
set logscale xy

f(x) = alpha*x*x+beta*x+gamma
fit f(x) 'bench.txt' using 1:2 via alpha, beta, gamma

g(x) = alp * x * sqrt(x) + bet*x + gam
fit g(x) 'bench.txt' using 1:3 via alp, bet, gam

set title "Integer conversion"
set xlabel "digits"
set ylabel "time (nanoseconds)"
set key left top

plot 'bench.txt' using 1:2 t "naive"       lt rgb "red", \
      f(x)                 t "naive fit"   lt rgb "red", \
     'bench.txt' using 1:3 t "this"        lt rgb "blue", \
      g(x)                 t "this fit"    lt rgb "blue", \
     'bench.txt' using 1:4 t "alternative" lt rgb "cyan"
