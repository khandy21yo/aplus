1	%TITLE "Convert AR Customers from MicroData"
	%SBTTL "AR_CONV_MDOPEN"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
	!
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be used and
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies thereof may not be provided or otherwise made
	! available to any other person.  No title to and ownership of
	! the software is hereby transferred.
	!
	! The information in this software is subject to change without
	! notice and should not be construed as a commitment by
	! Computer Management Center, Inc.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	! Abstract:HELP
	!	.B
	!	.LM +5
	!	.LM -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_CONV_MDOPEN/LINE
	!	$ LINK/EXEC:AR_EXE AR_CONV_MDOPEN,FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_CONV_MDOPEN.OBJ;*
	!
	! Author:
	!
	!	06/25/91 - Kevin Handy
	!
	! Modification history:
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/09/99 - Kevin Handy
	!		Lose line 800 (Dead code)
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

10	ON ERROR GOTO 19000

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN)			AR_OPEN_CDD	AR_OPEN
	MAP (AR_OPEN_BLANK)		AR_OPEN_CDD	AR_OPEN_BLANK

	EXTERNAL LONG    FUNCTION DATE_DAYCODE
	EXTERNAL STRING  FUNCTION DATE_INVDCODE

	CALL ASSG_CHANNEL(ARSYS.CH%,STAT%)

	!
	! Open the keyboard
	!
	CALL READ_INITIALIZE

	INPUT "For store number"; STORENO$

	START_DATE% = DATE_DAYCODE("19671231")


200	! RESUME LINE

 !	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.CRE"

	!======================================================================
	! AR_OPEN file (create, open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AR_OPEN.CH%, STAT%)
	CALL READ_DEVICE("AR_OPEN",AR_OPEN.DEV$, STAT%)
	CALL READ_PROTECTION("AR_OPEN",AR_OPEN.PRO$,STAT%)
	CALL READ_CURPROTECTION(OLD_PROT$,STAT%)
	CALL WRIT_CURPROTECTION(AR_OPEN.PRO$, STAT%)

	AR_OPEN.NAME$ = AR_OPEN.DEV$+"AR_OPEN.LED"

	OPEN AR_OPEN.NAME$ FOR OUTPUT AS FILE AR_OPEN.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AR_OPEN, &
		BUFFER 32%, &
		PRIMARY KEY &
		( &
			AR_OPEN::CUSNUM, &
			AR_OPEN::INVNUM, &
			AR_OPEN::TRATYP &
		)	DUPLICATES, &
		ALTERNATE KEY &
			AR_OPEN::BATCH &
			DUPLICATES CHANGES, &
		ALTERNATE KEY &
		( &
			AR_OPEN::SALNUM, &
			AR_OPEN::CUSNUM, &
			AR_OPEN::INVNUM, &
			AR_OPEN::TRATYP &
		)	DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW NONE

	CALL WRIT_CURPROTECTION(OLD_PROT$, STAT%)


250	OPEN ARSYS_ASC.DEV$ + "AR.ASC" FOR INPUT AS FILE ARSYS.CH%, &
		RECORDSIZE 512%, &
		ACCESS READ

	AR_OPEN_BLANK::CUSNUM	= ""
	AR_OPEN_BLANK::INVNUM	= ""
	AR_OPEN_BLANK::TRATYP	= ""
	AR_OPEN_BLANK::TRADAT	= ""
	AR_OPEN_BLANK::SALAMT	= 0.0
	AR_OPEN_BLANK::DISAMT	= 0.0
	AR_OPEN_BLANK::OTHCHG	= 0.0
	AR_OPEN_BLANK::RECNUM	= ""
	AR_OPEN_BLANK::CHKNUM	= ""
	AR_OPEN_BLANK::ARACCT	= ""
	AR_OPEN_BLANK::SUBACC	= ""
	AR_OPEN_BLANK::DESCR	= ""
	AR_OPEN_BLANK::SALNUM	= ""
	AR_OPEN_BLANK::BATCH	= ""
	AR_OPEN_BLANK::UPDATED	= ""
	AR_OPEN_BLANK::CLOSEDATE	= ""

	AR_OPEN = AR_OPEN_BLANK
	A5$, A6$, A7$, A13$, A14$ = ""

550	INPUT LINE #ARSYS.CH%, INP$
	INP$ = EDIT$(INP$, 4%)

560	if right(inp$, len(inp$)-1%) = "<>"
	then
		INPUT LINE #ARSYS.CH%, INP1$
		INP$ = LEFT(inp$,LEN(INP$)-2%) + EDIT$(INP1$, 4%)
		goto 560
	end if

	GOTO 550 IF INP$ = ""

	I2% = INSTR(1%, INP$, ">")
	FLD$ = SEG$(INP$, 2%, I2%-1%)
	DTA$ = RIGHT(INP$, I2%+1%)

 ! PRINT FLD$; TAB(20%); DTA$

	SELECT FLD$

		CASE "0", "SLSMN"
			AR_OPEN::INVNUM = RIGHT(DTA$, 8%)
			THISSTORE$ = MID(DTA$, 5%, 3%)

		CASE "1", "CUSTOMER#"
			AR_OPEN::CUSNUM = DTA$

		CASE "2", "PO#/ORDER#"
			AR_OPEN::DESCR = DTA$

		CASE "3"
			AR_OPEN::TRADAT = DATE_INVDCODE(START_DATE% + VAL%(DTA$))

		CASE "5"
			A5$ = DTA$

		CASE "6"
			A6$ = DTA$

		CASE "7"
			A7$ = DTA$

		CASE "9"
			!A9$ = DTA$

		CASE "11", "SALESMAN"
			AR_OPEN::SALNUM = DTA$

		CASE "13", "CHK-NO"
			A13$ = DTA$

		CASE "14", "CK.DATE"
			A14$ = DTA$

600		CASE "END"

			IF STORENO$ <> THISSTORE$
			THEN
				AR_OPEN = AR_OPEN_BLANK
				A5$, A6$, A7$, A13$, A14$ = ""
				GOTO 550
			END IF

690			WHILE A6$ <> ""

				I1% = INSTR(1%, A5$ + '253'C, '253'C)
				!A5A$ = LEFT(A5$, I1% - 1%)
				A5$ = RIGHT(A5$, I1% + 1%)

				I1% = INSTR(1%, A6$ + '253'C, '253'C)
				A6A$ = LEFT(A6$, I1% - 1%)
				A6$ = RIGHT(A6$, I1% + 1%)

				I1% = INSTR(1%, A7$ + '253'C, '253'C)
				A7A$ = LEFT(A7$, I1% - 1%)
				A7$ = RIGHT(A7$, I1% + 1%)

				SELECT A6A$

				CASE "I"
					AR_OPEN::TRATYP = "01"
					!AR_OPEN::DISAMT = VAL(A8$)

				CASE "S"
					AR_OPEN::TRATYP = "04"
					AR_OPEN::DISAMT = 0.0

				CASE "C"
					I1% = INSTR(1%, A13$ + '253'C, '253'C)
					A13A$ = LEFT(A13$, I1% - 1%)
					A13$ = RIGHT(A13$, I1% + 1%)

					I1% = INSTR(1%, A14$ + '253'C, '253'C)
					A14A$ = LEFT(A14$, I1% - 1%)
					A14$ = RIGHT(A14$, I1% + 1%)

					AR_OPEN::TRATYP = "09"
					AR_OPEN::CHKNUM = A13A$
					AR_OPEN::TRADAT = DATE_INVDCODE(START_DATE% + &
						VAL%(A14A$))
					AR_OPEN::DISAMT = 0.0

				CASE "CM"
					AR_OPEN::TRATYP = "08"
					AR_OPEN::DISAMT = 0.0

				CASE ELSE
					AR_OPEN::TRATYP = "01"
					!AR_OPEN::DISAMT = VAL(A8$)
					PRINT "Undefined type "; A6A$

				END SELECT

				AR_OPEN::SALAMT = VAL(A7A$) / 100.0

				!
				! Write out record
				!
				PUT #AR_OPEN.CH%
			NEXT

 PRINT AR_OPEN::CUSNUM; " "; AR_OPEN::INVNUM

695			AR_OPEN = AR_OPEN_BLANK
			A5$, A6$, A7$, A13$, A14$ = ""

		CASE ELSE
 !			PRINT "Undefined code '"; FLD$; "'"
	END SELECT

	GOTO 550

 !800
	!
	! Done
	!
 !	CLOSE AR_OPEN.CH%
 !
	!PRINT "Store list"; STORELIST$
 !
 !	GOTO 32767

	%PAGE

19000	!*******************************************************************
	! Trap Errors
	!*******************************************************************

	SELECT ERL

		CASE 550%

		CASE 560%
			PRINT "%Val"; AR_OPEN::CUSNUM; " "; AR_OPEN::INVNUM; " "; &
				ERR; " "; ERT$(ERR)
			RESUME 695

		CASE 690%
			PRINT "%Put"; AR_OPEN::CUSNUM; " "; AR_OPEN::INVNUM; " "; &
				ERR; " "; ERT$(ERR)
			RESUME 695

	END SELECT

	ON ERROR GOTO 0

32767	END
