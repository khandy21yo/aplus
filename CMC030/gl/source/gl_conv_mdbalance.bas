1	%TITLE "Convert AP CHART from MicroData"
	%SBTTL "GL_CONV_MDBALANCE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1992 BY
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
	!	$ BAS GL_SOURCE:GL_CONV_MDBALANCE/LINE
	!	$ LINK/EXEC:GL_EXE GL_CONV_MDBALANCE,FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_CONV_MDBALANCE.OBJ;*
	!
	! Author:
	!
	!	10/22/92 - Kevin Handy
	!
	! Modification history:
	!
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

	CALL ASSG_CHANNEL(APSYS.CH%,STAT%)

	DIM BALANCE(40%)

	!
	! Open the keyboard
	!
	CALL READ_INITIALIZE

	GLLIST$ = ""

	INPUT "For store number"; STORENO$
	accnoc$ = mid(storeno$, 2%, 2%)

	INPUT "Field # for 1st month"; FIELDNO%

200	! RESUME LINE

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.MOD"

250	OPEN APSYS_ASC.DEV$ + "ca.ASC" FOR INPUT AS FILE APSYS.CH%, &
		RECORDSIZE 512%, &
		ACCESS READ

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
			ACCOUNT$ = FNFORM_ACCT$(RIGHT(DTA$, 8%))
			THISSTORE$ = MID(DTA$, 5%, 3%)
			STORELIST$ = STORELIST$ + "!" + THISSTORE$ &
				IF INSTR(1%, STORELIST$, THISSTORE$) = 0%

600		CASE "END"

			IF STORENO$ <> THISSTORE$
			THEN
				GOTO 695
			END IF

			IF INSTR(1%, GLLIST$, ACCOUNT$)
			THEN
				PRINT "Account "; ACOUNT$; " DUPLICATED!"
				GOTO 695
			ELSE
				GLLIST$ = GLLIST$ + "," + ACCOUNT$
			END IF

680			!
			! Write out record
			!
			GET #GL_CHART.CH%, KEY#0% EQ ACCOUNT$

690			GL_CHART::DOLLAR(I%) = BALANCE(I%) FOR I% = 0% TO 20%

			UPDATE #GL_CHART.CH%
			PRINT GL_CHART::ACCT

695			BALANCE(I%) = 0.0 FOR I% = 0% TO 20%

		CASE ELSE
			FLD% = VAL(FLD$)

			SELECT FLD%

				CASE 4% TO FIELDNO%
					BALANCE(FIELDNO%-FLD%) = &
						VAL(DTA$) / 100.0

				CASE 17% TO 29%
					BALANCE((29%-FLD%)+(FIELDNO%-3%)) = &
						VAL(DTA$) / 100.0
 !				CASE ELSE
 !					PRINT "Undefined code '"; FLD$; "'"
			END SELECT

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

	CASE 680%
		PRINT "Account "; ACCOUNT$; " is undefined"
		PRINT "  "; I%, FORMAT$(BALANCE(I%), "##########.##") &
			IF BALANCE(I%) <> 0.0 &
			FOR I% = 0% TO 20%
		RESUME 695

	CASE 690%
		PRINT "%Put"; GL_CHART::acct; " "; ERR; " "; ERT$(ERR)
		RESUME 695

	END SELECT

	ON ERROR GOTO 0

32767	END
