FORM NORMAL

TITLE


"<COMPANY80>"
"<REPORT80>"
TAB(34);"FOR THE PERIOD"
"<PERIOD80>"


END

FORMAT 1 "                          \                                   \";/
"#,###,###,###.##",2,-ADP(0)
FORMAT 4 "                                                               ";/
"                 ----------------"
FORMAT 6 "                                     ";/
"        =========="
FORMAT 7 "\      \ ##,###,###.##  #####.###";/
"     \   \ ####,###.##  \          \",X,X,X,X,X,X
FORMAT 8 " \      \                        ";/
"           ####,###.##  \          \",X,X,X

PRINT TITLE
SUM INTO T1 F1 RS****SC**
SUM INTO T2 F1 RS****SCPW
SUM INTO T3 F1 RS****SCIF
PRINT
PRINT
PRINT     "BASIS         AMOUNT        RATE   SLM CODE  COMMISSION  ACCOUNT#"
PRINT     "-----         -------       -----  --------  ----------  ----------"
PRINT F7, TOTAL,T1(1),.010,DJ,T1(1)*.010,6110.03
PRINT F7, PASCO,T2(1),.015,OS,T2(1)*.015,6110.08
PRINT F7, I.F. ,T3(1),.015,MJ,T3(1)*.015,6110.09
PRINT F7, TOTAL,T1(1),.005,KJ,T1(1)*.005,6110.04
PRINT F6
ADD T9(1)=T1(1)*.010+T2(1)*.015+T3(1)*.015+T1(1)*.005
PRINT F8,TOTAL,T9(1),2045.00
END

