FORM NORMAL

TITLE


"<COMPANY116>"
"<REPORT116>"

TAB(56);"<DATE>"

END
FORMAT 1 "#,###,###,###.##  #,###,###,###.##  #,###,###,###.##",/
(0),(1),(0)-(1)
FORMAT 2 "                        \                     \ ";/
"#,###,###,###.##  #,###,###,###.##  #,###,###,###.##",X,X,X,X
FORMAT 3 "#,###,###,###.##",-((0)-(1))
FORMAT 4 "                         \                      ";/
"                  \                 #,###,###,###.##",X,X
FORMAT 5 "#,###,###,###.##",(0)
FORMAT 6 "#,###,###,###.##",-(0)
FORMAT 7 "                                               ";/
"                                    ----------------"
FORMAT 8 "                                               ";/
"                                    ================"
FORMAT 9 "                                               ";/
"================  ================  ================"
PRINT TITLE
PRINT "                                                        CURRENT            PRIOR         DIFFERENCE"
PRINT "                                                         MONTH             MONTH"
PRINT TAB(21);"CHANGE IN CASH:            ---------------   ---------------   ---------------"
SUM INTO T1 F1 ?CASH
PRINT AND SUM INTO T2 F2,CASH,T1(1),T1(2),T1(3)
PRINT F9
PRINT
PRINT
PRINT TAB(21);"CASH PROVIDED BY:"
SUM INTO T3 F3 ?INC?
PRINT AND SUM INTO T4 F4,NET INCOME,T3(1)
ADD T3=0
SUM INTO T3 F3 ?INCN
ADD T3=-T3
PRINT AND SUM INTO T4 F4,ADD EXPENSES NOT REQUIRING WORKING CAPITAL,T3(1)
PRINT F7
PRINT F4,CASH PROVIDED BY OPERATIONS,T4(1)
ADD T3=0
SUM INTO T3 F5 IAC??
PRINT AND SUM INTO T4 F4,DECREASE IN CURRENT ASSETS,T3(1)
ADD T3=0
SUM INTO T3 F5 IAF??
PRINT AND SUM INTO T4 F4,DECREASE IN FIXED ASSETS,T3(1)
ADD T3=0
SUM INTO T3 F5 IAO??
PRINT AND SUM INTO T4 F4,DECREASE IN OTHER ASSETS,T3(1)
ADD T3=0
SUM INTO T3 F5 ILC??
PRINT AND SUM INTO T4 F4,INCREASE IN CURRENT LIABILITES,T3(1)
ADD T3=0
SUM INTO T3 F5 ILL??
PRINT AND SUM INTO T4 F4,INCREASE IN LONG-TERM LIABILITES,T3(1)
ADD T3=0
SUM INTO T3 F5 IOE??
PRINT AND SUM INTO T4 F4,OTHER SOURCES OF CASH,T3(1)
PRINT F7
PRINT F4,     TOTAL SOURCES OF CASH,T4(1)
PRINT F7
PRINT
PRINT TAB(21);"APPLICATION OF CASH:"
ADD T3=0
SUM INTO T3 F6 DAC??
PRINT AND SUM INTO T5 F4,INCREASE IN CURRENT ASSETS,T3(1)
ADD T3=0
SUM INTO T3 F6 DAF??
PRINT AND SUM INTO T5 F4,INCREASE IN FIXED ASSETS,T3(1)
ADD T3=0
SUM INTO T3 F6 DAO??
PRINT AND SUM INTO T5 F4,INCREASE IN OTHER ASSETS,T3(1)
ADD T3=0
SUM INTO T3 F5 DLC??
PRINT AND SUM INTO T5 F4,DECREASE IN CURRENT LIABILITES,T3(1)
ADD T3=0
SUM INTO T3 F6 DLL??
PRINT AND SUM INTO T5 F4,DECREASE IN LONG-TERM LIABILITES,T3(1)
ADD T3=0
SUM INTO T3 F6 DOE??
PRINT AND SUM INTO T5 F4,OTHER APPLICATIONS OF CASH,T3(1)
PRINT F7
PRINT F4,     TOTAL APPLICATIONS OF CASH,T5(1)
PRINT F7
ADD T4=T4-T5
PRINT F4,     CHANGES IN CASH,T4(1)
PRINT F8
END
