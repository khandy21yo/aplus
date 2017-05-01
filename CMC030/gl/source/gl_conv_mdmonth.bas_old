1	%TITLE "Convert AP CHART from MicroData"
	%SBTTL "GL_CONV_MDMONTH"
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
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_CONV_MDMONTH/LINE
	!	$ LINK/EXEC:GL_EXE GL_CONV_MDMONTH,FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_CONV_MDMONTH.OBJ;*
	!
	! Author:
	!
	!	08/26/91 - Kevin Handy
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP)	GL_YYYY_PP_CDD	GL_YYYY_PP
	MAP (GL_YYYY_PP_BLANK)	GL_YYYY_PP_CDD	GL_YYYY_PP_BLANK

	EXTERNAL LONG    FUNCTION DATE_DAYCODE
	EXTERNAL STRING  FUNCTION DATE_INVDCODE

	START_DATE% = DATE_DAYCODE("19671231")

	def fnform_acct$(a$) = left(a$, 2%) + "-" + mid(a$,3%, 3%) + "-" + &
		mid(trm$(a$), 6%, 2%) + "-" + accnoc$


10	ON ERROR GOTO 19000

	CALL ASSG_CHANNEL(APSYS.CH%,STAT%)
	!
	! Open the keyboard
	!
	CALL READ_INITIALIZE

	INPUT "For store number"; STORENO$
	accnoc$ = mid(storeno$, 2%, 2%)

200	! RESUME LINE

250	OPEN APSYS_ASC.DEV$ + "GLM.ASC" FOR INPUT AS FILE APSYS.CH%, &
		RECORDSIZE 512%, &
		ACCESS READ


	GL_YYYY_PP_BLANK::ACCT		= ""
	GL_YYYY_PP_BLANK::SOURCE	= ""
	GL_YYYY_PP_BLANK::REFNO		= ""
	GL_YYYY_PP_BLANK::TRANDAT	= ""
	GL_YYYY_PP_BLANK::DESCR		= ""
	GL_YYYY_PP_BLANK::AMOUNT	= 0.0
	GL_YYYY_PP_BLANK::XREFNO	= ""
	GL_YYYY_PP_BLANK::POSTIM	= ""
	GL_YYYY_PP_BLANK::POSDAT	= ""
	GL_YYYY_PP_BLANK::CKNO		= ""
	GL_YYYY_PP_BLANK::TRANKEY	= ""
	GL_YYYY_PP_BLANK::SUBACC	= ""
	GL_YYYY_PP_BLANK::OPERATION	= ""
	GL_YYYY_PP_BLANK::UNITS		= 0.0
	GL_YYYY_PP_BLANK::HOURS		= 0.0
	GL_YYYY_PP_BLANK::UPDSTA	= ""
	GL_YYYY_PP_BLANK::BTHNUM	= ""

500	GL_YYYY_PP = GL_YYYY_PP_BLANK

550	INPUT LINE #APSYS.CH%, INP$
	INP$ = EDIT$(INP$, 4%)

560	if right(inp$, len(inp$)-1%) = "<>"
	then
		INPUT LINE #APSYS.CH%, INP1$
		INP$ = LEFT(inp$, LEN(INP$)-2%) + EDIT$(INP1$, 4%)
		goto 560
	end if

	GOTO 550 IF INP$ = ""

	I2% = INSTR(1%, INP$, ">")
	FLD$ = SEG$(INP$, 2%, I2%-1%)
	DTA$ = RIGHT(INP$, I2%+1%)

 ! PRINT FLD$; TAB(20%); DTA$

	SELECT FLD$

		CASE "0", "LOC"
			GL_YYYY_PP::ACCT = FNFORM_ACCT$(RIGHT(DTA$, 8%))
			THISSTORE$ = MID(DTA$, 5%, 3%)
			STORELIST$ = STORELIST$ + "!" + THISSTORE$ &
				IF INSTR(1%, STORELIST$, THISSTORE$) = 0%

		CASE "1","TR-DATE"
			GL_YYYY_PP::TRANDAT = DATE_INVDCODE(START_DATE% + VAL%(DTA$))

		CASE "2", "FWD"
			GL_YYYY_PP::REFNO = DTA$

		CASE "3", "DESCDTE"
			GL_YYYY_PP::DESCR = DTA$

		CASE "4", "SRC"
			GL_YYYY_PP::SOURCE = DTA$

		CASE "5", "MON"
			MONTH% = VAL(DTA$)

		CASE "6", "TR-AMOUNT"
			GL_YYYY_PP::AMOUNT = VAL(DTA$) / 100.0

		CASE "7", "TDESC"
			! Unknown

600		CASE "END"

			IF (STORENO$ <> THISSTORE$) OR (GL_YYYY_PP::AMOUNT = 0.0)
			THEN
				GOTO 500
			END IF

690			!
			! Write out record
			!
			GOSUB OpenMonth IF MONTH.CH%(MONTH%) = 0%

			PUT #MONTH.CH%(MONTH%)

 ! PRINT GL_YYYY_PP::ACCT; month%

695			!
			GOTO 500

		CASE ELSE
			PRINT "Undefined code '"; FLD$; "'"; dta$; "'"
	END SELECT

	GOTO 550

800	!
	! Done
	!
	CLOSE GL_YYYY_PP.CH%

	PRINT "Store list"; STORELIST$

	GOTO 32767

	%PAGE

10000	!*******************************************************************
	! Open up a month file if is has not yet been opened
	!*******************************************************************

 OpenMonth:

	GL_YYYY_PP.CH% = 0%

	YYYY_PP$ = "1991_" + FORMAT$(MONTH%, "<0>#")

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.CRE"

	MONTH.CH%(MONTH%) = GL_YYYY_PP.CH%

	RETURN

19000	!*******************************************************************
	! Trap Errors
	!*******************************************************************

	SELECT ERL

	CASE 550%
		RESUME 800

	CASE 560%
		RESUME 550

	CASE 690%
		PRINT "%Put"; GL_YYYY_PP::acct; " "; ERR; " "; ERT$(ERR)
		RESUME 695

	END SELECT

	ON ERROR GOTO 0

32767	END
