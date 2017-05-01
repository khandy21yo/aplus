1	%TITLE "Create Folder For Beginning Balances"
	%SBTTL "GL_SPEC_GENFOLDER"
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
	!	.b
	!	.lm +5
	!	The ^*Genfolder\* program is used to create a GL folder
	!	from information in the chart history to obtain balances that
	!	have not previously been entered in a folder and updated.
	!	.lm -5
	!
	! Index:
	!	.x General Ledger>Generate
	!	.x Generate>General Ledger
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_SPEC_GENFOLDER/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_SPEC_GENFOLDER, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_SPEC_GENFOLDER.OBJ;*
	!
	! Author:
	!
	!	03/07/92 - Kevin Handy
	!
	! Modification history:
	!
	!	03/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/11/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Remove unsolicited_input stuff.
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/13/99 - Kevin Handy
	!		Allow generating for PERIOD(0) [sugar]
	!
	!	09/27/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)		GL_PERIOD_CDD	GL_PERIOD

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD	GL_CHART

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP)	GL_YYYY_PP_CDD	GL_YYYY_PP

	!
	! Declare
	!
	DIM GLPERIOD$(20%)

	%PAGE

	ON ERROR GOTO 19000

	CALL READ_INITIALIZE

310	!
	! Figure out what in the world needs done (a whole lot)
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.UPD"
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	IF GL_PERIOD::CLOSEFLAG = "2"
	THEN
		CALL HELP_34MESSAGE(SCOPE, "GL Reset in process", &
			"E", SCOPE::PRG_PROGRAM, "", "GLFLAG2")

		GOTO ExitProgram
	END IF

	LASTPERCLO% = GL_PERIOD::LASTPERCLO
	YEAR$ = GL_PERIOD::YEAR
	FPFY% = GL_PERIOD::FPFY

	FOR I% = 0% TO 20%
		GLPERIOD$(I%) = YEAR$ + FORMAT$(LASTPERCLO%, "<0>#")
		LASTPERCLO% = LASTPERCLO% - 1%
		IF LASTPERCLO% < 1%
		THEN
			LASTPERCLO% = FPFY%
			YEAR$ = FORMAT$(VAL%(YEAR$) - 1%, "<0>###")
		END IF
	NEXT I%

	!
	! Paint the background, and confirm close
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		78%, &
		SMG_SCREEN_DATA%, &
		SMG$M_BORDER &
	)

	SMG_STATUS% = SMG$LABEL_BORDER &
	( &
		SMG_SCREEN_DATA%, &
		"General Ledger Gen for " + TRM$(SCOPE::PRG_COMPANY), &
		SMG$K_TOP &
	)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)

	SCOPE::PRG_ITEM = "CONFIRM"
	!++
	! Abstract:CONFIRM
	!	^*General Ledger Generate\*
	!	.b
	!	.lm +5
	!	The ^*General Ledger Generate\* requests a confirmation asking if the period
	!	displayed is to really be generated. If the Ledger is closed, no transactions
	!	may be posted to it nor General Ledger or Trial Balance reports printed.
	!	.lm -5
	!
	! Index:
	!
	!--
 InputQuestion:
	INP$ = GLPERIOD$(1%)
	INP$ = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
		"", "Period To Calculate", INP$, 0%, "", "")

	THIS_PERIOD% = -1%
	THIS_PERIOD% = I% IF GLPERIOD$(I%) = INP$ FOR I% = 0% TO 20%

	IF THIS_PERIOD% = -1%
	THEN
		GOTO InputQuestion
	END IF

	!
	! Do they really mean it?
	!
	INP$ = ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, &
		"", "Confirm", "N", 0%, "", "")

	IF INP$ <> "Y"
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	END IF

	!
	! Set help to program help
	!
	SCOPE::PRG_ITEM = "HELP"

	YYYY_PP$ = LEFT(GLPERIOD$(THIS_PERIOD%), 4%) + "_" + &
		RIGHT(GLPERIOD$(THIS_PERIOD%), 5%)

320	!
	! Open chart of accounts
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

330	!
	! Open period file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"
	USE
		CONTINUE 335 IF ERR = 5%
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

	CALL HELP_34MESSAGE(SCOPE, "GL Folder Already Exists", &
		"E", SCOPE::PRG_PROGRAM, "", "GLEXIST")

	GOTO ExitProgram

335	!
	! Open period file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.CRE"
	USE
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

340	!
	! Get set file information
	!
	SCOPE::PRG_IDENT = "PROG"
	SCOPE::PRG_PROGRAM = "GL_SPEC_GENFOLDER"

500	!

	%PAGE

1000	!******************************************************************
	! Close Chart of accounts
	!******************************************************************

	CALL ENTR_3MESSAGE(SCOPE, "Closing", 1%)

	WHEN ERROR IN
		RESET #GL_CHART.CH%
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

1010	WHEN ERROR IN
		GET #GL_CHART.CH%, REGARDLESS
	USE
		CONTINUE 1090 IF ERR = 11%
		CONTINUE HelpError
	END WHEN

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, GL_YYYY_PP::ACCT, &
		6%, 14%)

	THIS_DOLLAR	= GL_CHART::DOLLAR(THIS_PERIOD%) - &
		GL_CHART::DOLLAR(THIS_PERIOD% + 1%)
	THIS_UNIT	= GL_CHART::UNIT(THIS_PERIOD%) - &
		GL_CHART::UNIT(THIS_PERIOD% + 1%)
	THIS_HOUR	= GL_CHART::HOUR(THIS_PERIOD%) - &
		GL_CHART::HOUR(THIS_PERIOD% + 1%)

	GOSUB MakeAccount

	GOTO 1010

1090	!
	! Update period file
	!
	CLOSE GL_PERIOD.CH%, GL_CHART.CH%, GL_YYYY_PP.CH%

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

 MakeAccount:
1100	!
	! Find First record in the general ledger
	!
	RETURN IF (THIS_DOLLAR = 0.0) AND (THIS_UNIT = 0.0) AND &
		(THIS_HOUR = 0.0)

	GL_YYYY_PP::ACCT	= GL_CHART::ACCT
	GL_YYYY_PP::SOURCE	= "BBAL"
	GL_YYYY_PP::REFNO	= ""
	GL_YYYY_PP::TRANDAT	= DATE_TODAY
	GL_YYYY_PP::DESCR	= "BEGINNING BALANCES"
	GL_YYYY_PP::AMOUNT	= THIS_DOLLAR
	GL_YYYY_PP::XREFNO	= ""
	GL_YYYY_PP::POSTIM	= ""
	GL_YYYY_PP::POSDAT	= DATE_TODAY
	GL_YYYY_PP::CKNO	= ""
	GL_YYYY_PP::TRANKEY	= ""
	GL_YYYY_PP::SUBACC	= ""
	GL_YYYY_PP::OPERATION	= ""
	GL_YYYY_PP::UNITS	= THIS_UNIT
	GL_YYYY_PP::HOURS	= THIS_HOUR
	GL_YYYY_PP::UPDSTA	= ""
	GL_YYYY_PP::BTHNUM	= ""

1120	!
	! Main loop starts here
	!
	WHEN ERROR IN
		PUT #GL_YYYY_PP.CH%
	USE
		CONTINUE 1190 IF ERR = 11%
		CONTINUE HelpError
	END WHEN

1190	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

32767	END
