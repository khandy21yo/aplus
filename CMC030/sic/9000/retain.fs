FORM NORMAL

TITLE


"<COMPANY116>"
"<REPORT116>"

TAB(50);"FOR THE PERIOD ENDED "
"<PERIOD116>"



END
FORMAT 1 "                              \                                   \";/
"#,###,###,###.##",2,(0)       
FORMAT 3 "                              \                                   \";/
"#,###,###,###.##",2,-(0)
FORMAT 4 "                                                                  ";/
"                 ----------------"
FORMAT 6 "                                                                  ";/
"                 ================"
FORMAT 7 "                             \                                   \";/
"                 #,###,###,###.##",X,X
PRINT TITLE
SUM INTO T5 F1 A???????
SUM INTO T1 F3 L???????
SUM INTO T1 F3 OE??????
SUM INTO T2 F3 OR??????
ADD T6=T5-T1-T2
PRINT F7,RETAINED EARNINGS - BEGINNING BALANCE,T2(1)
PRINT
PRINT F7,NET EARNINGS (LOSS),T6(1)
PRINT F4
ADD T3=T2+T6
PRINT F7,RETAINED EARNINGS - ENDING BALANCE,T3(1)
PRINT F6
END
