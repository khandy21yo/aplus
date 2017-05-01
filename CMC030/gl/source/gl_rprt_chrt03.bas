1	%TITLE "Chart of Accounts History Report"
	%SBTTL "GL_RPRT_CHRT03"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1988 BY
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
	! ID:CHR003
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Chart History\* option prints the Chart of
	!	Accounts including Dollar, Unit, and Hour balances for each account for
	!	the last accounting period closed and the preceding twenty (20) periods.
	!	.b
	!	The report will include the following fields:
	!	.table 30
	!	.te
	!	Account
	!	.te
	!	Description
	!	.te
	!	Month
	!	.te
	!	Budget Dollars
	!	.te
	!	Budget Units
	!	.te
	!	Budget Hours
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Chart History
	!	.x Chart of Account>Report
	!	.x Print>Chart History
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_RPRT_CHRT03/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_RPRT_CHRT03, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_RPRT_CHRT03.OBJ;*
	!
	! Author:
	!
	!	11/07/86 - B. Craig Larsen
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/15/95 - Kevin Handy
	!		Reformat source code closer to 80 columns.
	!		Change RIGHT(NUM1$()) for FORMAT$()
	!
	!	02/07/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/20/97 - Kevin Handy
	!		Don't need to allocate channel for report
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/13/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	02/02/2001 - Kevin Handy
	!		Get headings for the columns.
	!
	!	02/02/2001 - Kevin Handy
	!		Add from/to period options
	!
	!	12/10/2004 - Kevin Handy
	!		Add totals only option.
	!
	!	03/23/2005 - Kevin Handy
	!		Added wildcard balance/income code.
	!		Added FUNC_ROUND to totaling routine.
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
	DIM	REAL	TOT_DOLLAR(20%), TOT_UNIT(20%), TOT_HOUR(20%)
	DIM	STRING	GLPERIOD(20%), GLNAME(20%)

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

	!
	! Initialize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Account\*
	!	.b
	!	.lm +5
	!	The ^*From Account\* field causes
	!	printing to begin with a specified
	!	account number.
	!	.b
	!	If the report is to begin with the first account in the file, this
	!	field should be left blank.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Account\*
	!	.b
	!	.lm +5
	!	The ^*To Account\* field causes
	!	printing to end with a specified
	!	account number.
	!	.b
	!	If the report is to end with the last account in the file, this
	!	field should be left blank.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* setting enables the user to print a
	!	report including selected accounts only, using the wildcard
	!	technique.
	!	.b
	!	Example: If the Chart of Accounts format were 99999-99 and
	!	the two rightmost numbers represented a department number and a
	!	report were to be printed to include department "02" only, the
	!	Wildcard setting of "?????-02" would cause the report to print
	!	all accounts with the suffix 02.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD_BAL$ = TRM$(UTL_REPORTX::OPTDEF(3%))

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard Balance/Income code\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* setting enables the user to print a
	!	report including selected accounts only, using the wildcard
	!	technique.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_PERIOD$ = TRM$(UTL_REPORTX::OPTDEF(5%))
	TO_PERIOD% = 20%

	!++
	! Abstract:FLD06
	!	^*(06) From Period\*
	!	.b
	!	.lm +5
	!	The period to start with. Defaults to the first period
	!	stored in the history file.
	!	.lm -5
	!
	! Index:
	!
	!--

	FROM_PERIOD$ = TRM$(UTL_REPORTX::OPTDEF(6%))
	FROM_PERIOD% = 0%

	!++
	! Abstract:FLD07
	!	^*(07) To Period\*
	!	.b
	!	.lm +5
	!	The last period to display. Defaults to the last period
	!	stored in the history file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TOTALS_ONLY$ = LEFT$(UTL_REPORTX::OPTDEF(7%), 1%)

	!++
	! Abstract:FLD08
	!	^*(08) Totals Only\*
	!	.b
	!	.lm +5
	!	Should detail (all the individual accounts) be left
	!	off the report, displaying only the grand total.
	!	.lm -5
	!
	! Index:
	!
	!--

	TOT_DOLLAR(I%), TOT_UNIT(I%), TOT_HOUR(I%) = 0.0 &
		FOR I% = 0% TO 20%

	%PAGE

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

	CASE ELSE
		LASTPERCLO% = GL_PERIOD::LASTPERCLO
		YEAR$ = GL_PERIOD::YEAR
		FPFY% = GL_PERIOD::FPFY

		FOR LOOP% = 0% TO 20%
			GLPERIOD(LOOP%) = YEAR$ + " " + &
				GL_PERIOD::PERIOD(LASTPERCLO%)
			GLNAME(LOOP%) = YEAR$ + FORMAT$(LASTPERCLO%, "<0>#")

			IF GLNAME(LOOP%) = FROM_PERIOD$
			THEN
				FROM_PERIOD% = LOOP%
			END IF

			IF GLNAME(LOOP%) = TO_PERIOD$
			THEN
				TO_PERIOD% = LOOP%
			END IF

			LASTPERCLO% = LASTPERCLO% - 1%
			IF LASTPERCLO% < 1%
			THEN
				LASTPERCLO% = FPFY%
				YEAR$ = FORMAT$(VAL%(YEAR$) - 1%, "<0>###")
			END IF
		NEXT LOOP%

	END SELECT

	IF TO_PERIOD% < FROM_PERIOD%
	THEN
		TEMP% = TO_PERIOD%
		TO_PERIOD% = FROM_PERIOD%
		FROM_PERIOD% = TEMP%
	END IF

	!
	! Open GL Chart of Accounts file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Titles
	!
	TITLE$(1%) = "Chart History Listing"
	TITLE$(2%) = ""

	!
	! Headings
	!
	TITLE$(3%) = "Account            Description                " + &
		"             Period                  Dollars          " + &
		"Units          Hours"
	TITLE$(4%) = ""

	!
	! Layout of lines printed
	!
	LYT_LINE$ = "$Account:018,$Descr:058,$Period:075,VDollar:090," + &
		"VUnit:105,VHour:120"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #GL_CHART.CH%
		ELSE
			FIND #GL_CHART.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
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

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Check current record
	!
	GOTO 17900 IF (GL_CHART::ACCT > TO_ITEM$) AND TO_ITEM$ <> ""

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(GL_CHART::ACCT, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(GL_CHART::FINTYPE, -1%), WLDCRD_BAL$) = 0% &
		AND WLDCRD_BAL$ <> ""

17300	!
	! Print out one line
	!
	TEXT$ = GL_CHART::ACCT + " " + LEFT(GL_CHART::DESCR, 39%) + " "

	FOR I% = FROM_PERIOD% TO TO_PERIOD%

		IF TOTALS_ONLY$ <> "Y"
		THEN
			TEXT$ = TEXT$ + LEFT(GLPERIOD(I%) + SPACE$(16%), 16%) + &
				FORMAT$(GL_CHART::DOLLAR(I%), " ###,###,###.##") + &
				FORMAT$(GL_CHART::UNIT(I%), " ###,###,###.##") + &
				FORMAT$(GL_CHART::HOUR(I%), " ###,###,###.##")

			CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF

		TOT_DOLLAR(I%) = FUNC_ROUND( &
			TOT_DOLLAR(I%) + GL_CHART::DOLLAR(I%), 2%)
		TOT_UNIT(I%) = TOT_UNIT(I%) + GL_CHART::UNIT(I%)
		TOT_HOUR(I%) = TOT_HOUR(I%) + GL_CHART::HOUR(I%)

		TEXT$ = SPACE$(59%)

	NEXT I%

	IF TOTALS_ONLY$ <> "Y"
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

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
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 2000%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = "TOTALS" + SPACE$(53%)

	FOR I% = FROM_PERIOD% TO TO_PERIOD%

		TEXT$ = TEXT$ + LEFT(GLPERIOD(I%) + SPACE$(16%), 16%) + &
			FORMAT$(TOT_DOLLAR(I%), " ###,###,###.##") + &
			FORMAT$(TOT_UNIT(I%), " ###,###,###.##") + &
			FORMAT$(TOT_HOUR(I%), " ###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		TEXT$ = SPACE$(59%)

	NEXT I%

 ExitProgram:
	!
	! Finish up report
	!
	CALL OUTP_FINISH(UTL_REPORTX)

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

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Handle untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

	END
