1	%TITLE "General Ledger Closing Program"
	%SBTTL "GL_CLOS_BACKWARDS"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	!	The ^*BACKWARDS\* close is used to help recreate
	!	historical periods in the history after a reset has
	!	been done. It ^*cannot\* close correctly over a
	!	year break because of the nominal accounts, but
	!	what it creates may help regenerate the figures.
	!	.LM -5
	!
	! Index:
	!	.x General Ledger>Close
	!	.x Close>General Ledger
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_CLOS_BACKWARDS/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_CLOS_BACKWARDS, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_CLOS_BACKWARDS.OBJ;*
	!
	! Author:
	!
	!	02/26/92 - Kevin Handy
	!
	! Modification history:
	!
	!	03/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/11/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Took out unsolicited input setup, which was
	!		then ignored.
	!
	!	07/02/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/01/97 - Kevin Handy
	!		Clean up source code.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	07/27/98 - Kevin Handy
	!		Change out of %CDD
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/30/2000 - Kevin Handy
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
		"General Ledger Close for " + TRM$(SCOPE::PRG_COMPANY), &
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
	!	^*General Ledger Close\*
	!	.B
	!	.LM +5
	!	The ^*General Ledger Close\* requests a confirmation asking if the period
	!	displayed is the correct period to be closed. If the Ledger is closed
	!	no transactions may be posted to it nor General Ledger or Trial Balance
	!	reports printed.
	!	.LM -5
	!
	! Index:
	!
	!--
 InputQuestion:
	INP$ = GLPERIOD$(1%)
	INP$ = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
		"", "Period To Calculate", INP$, 0%, "", "")

	THIS_PERIOD% = 0%
	THIS_PERIOD% = I% IF GLPERIOD$(I%) = INP$ FOR I% = 1% TO 20%

	IF THIS_PERIOD% = 0%
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

	YYYY_PP$ = LEFT(GLPERIOD$(THIS_PERIOD% - 1%), 4%) + "_" + &
		RIGHT(GLPERIOD$(THIS_PERIOD% - 1%), 5%)

320	!
	! Open chart of accounts
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.UPD"
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

330	!
	! Open period file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.UPD"
	USE
		FILENAME$ = "GL_" +YYYY_PP$
		CONTINUE HelpError
	END WHEN

340	!
	! Get set file information
	!
	SCOPE::PRG_IDENT = "PROG"
	SCOPE::PRG_PROGRAM = "GL_CLOS_BACKWARDS"

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
		GET #GL_CHART.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 1090 IF ERR = 11%
		CONTINUE HelpError
	END WHEN

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, GL_YYYY_PP::ACCT, &
		6%, 14%)

	THIS_DOLLAR = GL_CHART::DOLLAR(THIS_PERIOD% - 1%)
	THIS_UNIT = GL_CHART::UNIT(THIS_PERIOD% - 1%)
	THIS_HOUR = GL_CHART::HOUR(THIS_PERIOD% - 1%)

	GOSUB ReadAccount

	GL_CHART::DOLLAR(THIS_PERIOD%)	= THIS_DOLLAR
	GL_CHART::UNIT(THIS_PERIOD%)	= THIS_UNIT
	GL_CHART::HOUR(THIS_PERIOD%)	= THIS_HOUR

	UPDATE #GL_CHART.CH%

	GOTO 1010

1090	!
	! Update period file
	!
	CLOSE GL_PERIOD.CH%, GL_CHART.CH%, GL_YYYY_PP.CH%

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")



 ReadAccount:
1100	!
	! Find First record in the general ledger
	!
	WHEN ERROR IN
		FIND #GL_YYYY_PP.CH%, KEY # 0% EQ GL_CHART::ACCT, REGARDLESS
	USE
		CONTINUE 1190
	END WHEN

1120	!
	! Main loop starts here
	!
	WHEN ERROR IN
		GET #GL_YYYY_PP.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 1190 IF ERR = 11%
		CONTINUE HelpError
	END WHEN

	IF GL_YYYY_PP::ACCT = GL_CHART::ACCT
	THEN
		THIS_DOLLAR = FUNC_ROUND(THIS_DOLLAR - GL_YYYY_PP::AMOUNT, 2%)
		THIS_UNIT = FUNC_ROUND(THIS_UNIT - GL_YYYY_PP::UNITS, 2%)
		THIS_HOUR = FUNC_ROUND(THIS_HOUR - GL_YYYY_PP::HOURS, 2%)
		GOTO 1120
	END IF

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
