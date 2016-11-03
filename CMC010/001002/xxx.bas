10 open "du0:" as file 1%, mode 16384%
20 get #1%, record 1%
30 field #1%, recount as b$
35 print "len="; len(b$)
40 print i%, cvt$%(mid(b$, i%, 2%)) for i% = 1% to 20% step 2%
50 print
60 print i%, swap%(cvt$%(mid(b$, 11%, 2%))) and  (2% ^ i%) for i% = 0% to 15%
70 print rad$(swap%(cvt$%(mid(b$, 13%, 2%))))
90 stop
100 field #1%, 10% as x$, 2% as x$
102 print "XXX"; cvt$%(x$)
105 lset x$ = cvt%$(cvt$%(x$) and not(8%))
110 print cvt$%(x$)
190 put #1%, record 1%
900 close #1%
