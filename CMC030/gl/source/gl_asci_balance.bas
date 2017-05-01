1	%TITLE "Chart of Account balances to ascii file"
	%SBTTL "GL_ASCI_BALANCE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2000 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	Converts the chart of account balances into an ascii
	!	tab seperated text file.
	!	.lm -5
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_ASCI_BALANCE/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_ASCI_BALANCE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_ASCI_BALANCE.OBJ;*
	!
	! Author:
	!
	!	03/17/2000 - Kevin Handy
	!
	! Modification history:
	!
	!	11/10/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!******************************************************************
	! External modules needed
	!******************************************************************

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!******************************************************************
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs)
	!******************************************************************

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD	GL_CHART

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP	(GL_PERIOD)	GL_PERIOD_CDD	GL_PERIOD

	!
	! Dimension arrays
	!
	DIM	REAL	TOT.DOLLAR(20%), TOT.UNIT(20%), TOT.HOUR(20%)
	DIM	STRING	GLPERIOD(20%)

	%PAGE

	!******************************************************************
	! Take care of anything else before starting the report
	!******************************************************************

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Initialization:
	!******************************************************************
	! Get ready to begin
	!******************************************************************

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
		CLOSE GL_PERIOD.CH%
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	!
	! Examine status of CLOSEFLAG
	!
	SELECT GL_PERIOD::CLOSEFLAG

	CASE "1"
		CALL HELP_3MESSAGE(SCOPE, &
			"GL Close in process", "ERR", "GL_CLOSE", &
			"ERROR_CLOSE")
		UTL_REPORTX::STAT = -1%
		GOTO ExitProgram

	CASE "2"
		CALL HELP_3MESSAGE(SCOPE, &
			"GL Reset in process", "ERR", "GL_RESET", &
			"ERROR_RESET")
		UTL_REPORTX::STAT = -1%
		GOTO ExitProgram

	END SELECT

	LASTPERCLO% = GL_PERIOD::LASTPERCLO
	YEAR$ = GL_PERIOD::YEAR
	FPFY% = GL_PERIOD::FPFY

	FOR LOOP% = 0% TO 20%
		GLPERIOD(LOOP%) = YEAR$ + FORMAT$(LASTPERCLO%, "<0>#")
		LASTPERCLO% = LASTPERCLO% - 1%
		IF LASTPERCLO% < 1%
		THEN
			LASTPERCLO% = FPFY%
			YEAR$ = FORMAT$(VAL%(YEAR$) - 1%, "<0>###")
		END IF
	NEXT LOOP%

	!
	! Open GL Chart of Accounts file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

390	OPEN "GLBALANCE.TXT" FOR OUTPUT AS FILE 1%, &
		RECORDSIZE 128%

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		RESET #GL_CHART.CH%
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17100	!******************************************************************
	! Main report loop starts here
	!******************************************************************

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #GL_CHART.CH%, REGARDLESS
	USE
		CONTINUE 17900 IF ERR = 11%
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

17300	!
	! Print out one line
	!
	FOR I% = 0% TO 20%

		TEXT$ = TRM$(GL_CHART::ACCT) + "	" + &
			TRM$(GLPERIOD(I%)) + "	" + &
			NUM1$(GL_CHART::DOLLAR(I%)) + "	" + &
			NUM1$(GL_CHART::UNIT(I%)) + "	" + &
			NUM1$(GL_CHART::HOUR(I%))

		PRINT #1%, TEXT$

	NEXT I%

	!
	! Try for next record
	!
	GOTO GetNextRec

17900	!******************************************************************
	! Handle totals and other items before EXITing
	!******************************************************************

 ExitTotal:
	!
	! Print out totals
	!

 ExitProgram:
	!
	! Exit to next program or menu
	!
	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

	%PAGE

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Handle untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32767	!******************************************************************
	! End of report GL_ASCI_BALANCE
	!******************************************************************
	END
