5 A% = 512%
7 B% = 12%
10 OPEN "_MM0:" AS FILE 1%, RECORDSIZE A%, MODE B%
20 GET #1%
30 FIELD #1%, RECOUNT AS A$
40 PRINT \ PRINT \ PRINT
50 PRINT LEN(A$)
60 PRINT A$
70 GOTO 20
