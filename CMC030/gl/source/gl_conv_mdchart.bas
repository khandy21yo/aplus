1	%TITLE "Convert AP CHART from MicroData"
	%SBTTL "GL_CONV_MDCHART"
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
	!	$ BAS GL_SOURCE:GL_CONV_MDCHART/LINE
	!	$ LINK/EXEC:GL_EXE GL_CONV_MDCHART,FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_CONV_MDCHART.OBJ;*
	!
	! Author:
	!
	!	06/20/91 - Kevin Handy
	!
	! Modification history:
	!
	!	08/26/91 - Kevin Handy
	!		Modified to lose last two difits, and add
	!		-store to account number.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/09/99 - Kevin Handy
	!		Lose line 800 (Dead code)
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	def fnform_acct$(a$) = left(a$, 2%) + "-" + mid(a$,3%, 3%) + "-" + &
		mid(trm$(a$), 6%, 2%) + "-" + accnoc$


10	ON ERROR GOTO 19000

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD	GL_CHART
	MAP (GL_CHART_BLANK)	GL_CHART_CDD	GL_CHART_BLANK

	CALL ASSG_CHANNEL(APSYS.CH%,STAT%)
	!
	! Open the keyboard
	!
	CALL READ_INITIALIZE

	INPUT "For store number"; STORENO$
	accnoc$ = mid(storeno$, 2%, 2%)

200	! RESUME LINE

 !	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.CRE"
	!======================================================================
	! GL_CHART file (create, open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(GL_CHART.CH%, STAT%)
	CALL READ_DEVICE("GL_CHART",GL_CHART.DEV$, STAT%)
	CALL READ_PROTECTION("GL_CHART",GL_CHART.PRO$,STAT%)
	CALL READ_CURPROTECTION(OLD_PROT$,STAT%)
	CALL WRIT_CURPROTECTION(GL_CHART.PRO$, STAT%)

	GL_CHART.NAME$ = GL_CHART.DEV$+"GL_CHART.MAS"

	OPEN GL_CHART.NAME$ AS FILE GL_CHART.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP GL_CHART, &
		BUFFER 32%, &
		PRIMARY KEY &
			GL_CHART::ACCT, &
		ALTERNATE KEY &
			GL_CHART::FLOW &
			DUPLICATES CHANGES, &
		ALTERNATE KEY &
			GL_CHART::WORK &
			DUPLICATES CHANGES, &
		ALTERNATE KEY &
			GL_CHART::FINTYPE &
			DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW NONE

	CALL WRIT_CURPROTECTION(OLD_PROT$, STAT%)


250	OPEN APSYS_ASC.DEV$ + "chart.ASC" FOR INPUT AS FILE APSYS.CH%, &
		RECORDSIZE 512%, &
		ACCESS READ


	GL_CHART_BLANK::ACCT	= ""
	GL_CHART_BLANK::DESCR	= ""
	GL_CHART_BLANK::ACCTYPE	= ""
	GL_CHART_BLANK::FLOW	= ""
	GL_CHART_BLANK::WORK	= ""
	GL_CHART_BLANK::FINTYPE	= ""
	GL_CHART_BLANK::SUMMARY	= ""
	FOR I% = 0% TO 20%
		GL_CHART_BLANK::DOLLAR(I%)	= 0.0
		GL_CHART_BLANK::UNIT(I%)	= 0.0
		GL_CHART_BLANK::HOUR(I%)	= 0.0
	NEXT I%
	GL_CHART_BLANK::CPERIOD	= 0%
	GL_CHART_BLANK::RUNDOL	= 0.0
	GL_CHART_BLANK::RUNUNIT	= 0.0
	GL_CHART_BLANK::RUNHOUR	= 0.0
	GL_CHART_BLANK::CURDOL	= 0.0
	GL_CHART_BLANK::CURUNIT	= 0.0
	GL_CHART_BLANK::CURHOUR	= 0.0
	GL_CHART_BLANK::BATCH	= ""


	GL_CHART = GL_CHART_BLANK

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

 !	PRINT FLD$; TAB(20%); DTA$

	SELECT FLD$

		CASE "0", "G/L"
			GL_CHART::ACCT = FNFORM_ACCT$(RIGHT(DTA$, 8%))
			THISSTORE$ = MID(DTA$, 5%, 3%)
			STORELIST$ = STORELIST$ + "!" + THISSTORE$ &
				IF INSTR(1%, STORELIST$, THISSTORE$) = 0%

		CASE "1"
			GL_CHART::DESCR = DTA$

600		CASE "END"

			IF STORENO$ <> THISSTORE$
			THEN
				GL_CHART = GL_CHART_BLANK
				GOTO 550
			END IF

690			!
			! Write out record
			!
			PUT #GL_CHART.CH%
			PRINT GL_CHART::ACCT

695			GL_CHART = GL_CHART_BLANK

		CASE ELSE
 !			PRINT "Undefined code '"; FLD$; "'"
	END SELECT

	GOTO 550

 !800
	!
	! Done
	!
 !	CLOSE GL_CHART.CH%
 !
 !	PRINT "Store list"; STORELIST$
 !
 !	GOTO 32767

	%PAGE

19000	!*******************************************************************
	! Trap Errors
	!*******************************************************************

	SELECT ERL

	CASE 550%

	CASE 690%
		PRINT "%Put"; GL_CHART::acct; " "; ERR; " "; ERT$(ERR)
		RESUME 695

	END SELECT

	ON ERROR GOTO 0

32767	END
