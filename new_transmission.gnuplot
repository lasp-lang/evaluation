# set terminal png transparent nocrop enhanced size 450,320 font "arial,8" 
# set output 'histograms.8.png'
set border 3 front lt black linewidth 1.000 dashtype solid
set boxwidth 0.8 absolute
set style fill   solid 1.00 noborder
set grid nopolar
set grid noxtics nomxtics ytics nomytics noztics nomztics \
 nox2tics nomx2tics noy2tics nomy2tics nocbtics nomcbtics
set grid layerdefault   lt 0 linewidth 0.500,  lt 0 linewidth 0.500
set key bmargin center horizontal Left reverse noenhanced autotitle columnhead nobox maxcols 2 maxrows 1
set style histogram rowstacked title textcolor lt -1 offset character 2, 0.25, 0
set style data histograms
set cbtics border in scale 0,0 mirror norotate  autojustify
set rtics axis in scale 0,0 nomirror norotate  autojustify
set title "Advertisement Impression Counter" 
set xlabel "(Client Number)" 
set xlabel  offset character 0, -1, 0 font "" textcolor lt -1 norotate
set ylabel "MB Transmitted" 
set yrange [ 0.00000 : 900000. ] noreverse nowriteback
set linetype 1 lc rgb "#A00000"
set linetype 2 lc rgb "#00A000"
set linetype 3 lc rgb "#5060D0"
set linetype 4 lc rgb "#F25900"
plot newhistogram "32", 'immigration.dat' using "32_s":xtic(1) t 'Server' lc 3, '' u "32_c" t 'Client' lc 4, \
     newhistogram "64", '' u "64_s":xtic(1) t '' lc 3, '' u "64_c" t '' lc 4, \
     newhistogram "128", '' u "128_s":xtic(1) t '' lc 3, '' u "128_c" t '' lc 4, \
     newhistogram "256", '' u "256_s":xtic(1) t '' lc 3, '' u "256_c" t '' lc 4, \
     newhistogram "512", '' u "512_s":xtic(1) t '' lc 3, '' u "512_c" t '' lc 4
