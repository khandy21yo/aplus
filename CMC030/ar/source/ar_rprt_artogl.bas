1	%TITLE "Print Accounts Receivable to General Ledger Comparison"
	%SBTTL "AR_RPRT_ARTOGL"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1988, 1990 BY
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
	! ID:AR021
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Accounts Receivable to General Ledger Comparison\* report compares
	!	the accounts between the General Ledger and the Accounts Receivable Ledger.
	!	A result of the comparison is printed showing the differences between the
	!	two ledgers.
	!	.b
	!	^*Note:\*  This report uses the accounts defined in the control file
	!	(under the accounT option) to determine what items in the
	!	general ledger should be included in this report.
	!	If items are missing or there are extra items in the
	!	GL column, check these account numbers.
	!	.lm -5
	!
	! Index:
	!	.x AR to GL Comparison>Report
	!	.x Report>AR to GL Comparison
	!	.y GL
	!	.Y GENERALLEDGER
	!
	! Option:
	!
	! Author:
	!
	!	03/18/88 - Aaron Redd
	!
	! Compile:
	!
	!	$  BAS AR_SOURCE:AR_RPRT_ARTOGL.BAS/LINE
	!	$  LINK/EXE=AR_EXE:*.EXE AR_RPRT_ARTOGL,FUNC_LIB:CMCLINK/OPTION
	!	$  DELETE AR_RPRT_ARTOGL.OBJ;*
	!
	! Modification history:
	!
	!	04/22/88 - Kevin Handy
	!		Modified so it doesn't print zero balances.
	!
	!	02/13/89 - Kevin Handy
	!		Modified to scan through the GL file and
	!		the AR file instead of the CUSTOMER
	!		file, so that undefined customers will
	!		show up.
	!
	!	10/23/89 - Kevin Handy
	!		Modified so that it would print the total
	!		when finishing properly.
	!
	!	06/12/90 - Aaron Redd
	!		Modified to use GL files from periods other than
	!		the present.
	!
	!	06/21/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		also be sent to a spreadsheet or a DIF file.
	!
	!	07/18/90 - Kevin Handy
	!		Modified to look into the closed file as well as
	!		the open file, so that a comparison can be done
	!		better for old periods that may have been closed
	!		out.
	!
	!	05/24/91 - Kevin Handy
	!		Modified to close GL_PERIOD file as soon as possible.
	!
	!	12/19/91 - Dan Perkins
	!		Added two more numerical digits to output fields to
	!		accomodate larger numbers.
	!
	!	02/22/93 - Kevin Handy
	!		Fixed bug where wasn't handling type "02" records
	!		correctly.
	!
	!	03/01/93 - Kevin Handy
	!		Fixed bug (re 02/22/93) where I missed the closed
	!		file.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/14/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		Change RIGHT(NUM1$()) to FORMAT$().
	!
	!	06/19/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/13/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/24/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/08/99 - Kevin Handy
	!		Use "WHEN ERROR IN"
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE	UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP	(AR_OPEN)		AR_OPEN_CDD	AR_OPEN

	%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.HB"
	MAP	(AR_CLOSED)		AR_CLOSED_CDD	AR_CLOSED

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP	(AR_35CUSTOM)		AR_35CUSTOM_CDD	AR_35CUSTOM

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP	(GL_YYYY_PP)		GL_YYYY_PP_CDD	GL_YYYY_PP

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP	(GL_PERIOD)		GL_PERIOD_CDD	GL_PERIOD

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL_ACCT.HB"
	MAP	(AR_CONTROL_ACCT)	AR_CONTROL_ACCT_CDD	AR_CONTROL_ACCT

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP	(AR_CONTROL)		AR_CONTROL_CDD	AR_CONTROL

	!
	! Declare variables and constants
	!
	DECLARE GFLOAT	DIFFER, AR_AMOUNT, GL_AMOUNT, AR_TOTAL, &
		GL_TOTAL, DIF_TOTAL
	DECLARE	STRING	ACCT_LIST, LYT_LINE

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Init:
	!
	! Initialize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Set up the from-user input
	!

	PERIOD$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^*(01) For Period\*
	!	.b
	!	.lm +5
	!	The ^*For Period\* field is used to specify which
	!	General Ledger Period the comparison is for.
	!	.b
	!	If this field is left blank, it will print the
	!	report for the currently open period as defined by
	!	the AR Control file.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!	.x For Period>AR to GL Comparison
	!	.x AR to GL Comparison>For Period
	!	.x Period>AR to GL Comparison
	!	.x AR to GL Comparison>Period
	!
	!--

	!
	! Set initial variable values
	!
	AR_TOTAL, GL_TOTAL, DIF_TOTAL = 0.00
	AR_EOF%, GL_EOF% = 0%

	%PAGE

300	!
	! Open the AR Open Item file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.OPN"
	USE
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

305	!
	! Open the AR Open Item file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.OPN"
	USE
		IF ERR = 5%
		THEN
			ARC_EOF% = -1%
			CONTINUE 310
		END IF
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

310	!
	! Open the AR Customer file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

320	!
	! Open the GL Control File and grab the record
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
		CLOSE #GL_PERIOD.CH%
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

330	!
	! Open the AR Control file, and grab the record
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.OPN"
		GET #AR_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

	!
	! Check the closing flag
	!
	SELECT AR_CONTROL::CLOSEFLAG

	CASE "1"
		CALL HELP_3MESSAGE(SCOPE, "AR Close in process", &
			"ERR", "AR_CLOSE", &
			"ERROR_CLOSE")
		GOTO ExitProgram

	CASE "2"
		CALL HELP_3MESSAGE(SCOPE, "AR Reset in process", &
			"ERR", "AR_RESET", &
			"ERROR_RESET")
		GOTO ExitProgram

	CASE "3"
		CALL HELP_3MESSAGE(SCOPE, "AR Purge in process", &
			"ERR", "AR_PURGE", &
			"ERROR_PURGE")
		GOTO ExitProgram

	END SELECT

	!
	! If the user did not specify a period, use the current one
	!
	IF (PERIOD$ = "")
	THEN

		CUR_PERIOD% = AR_CONTROL::LASTPERCLOSE + 1%
		YEAR$ = AR_CONTROL::YEAR

		IF CUR_PERIOD% > GL_PERIOD::FPFY
		THEN
			CUR_PERIOD% = 1%
			YEAR$ = FORMAT$(VAL%(YEAR$) + 1%, "<0>###")
		END IF

		PERIOD$ = YEAR$ + FORMAT$(CUR_PERIOD%, "<0>#")

	END IF

	YYYY_PP$ = LEFT(PERIOD$, 4%) + "_" + RIGHT(PERIOD$, 5%)

	!
	! Open the appropriate GL Period file
	!
340	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	!
	! Open the AR Account Control file, and load the accounts into a string
	!
350	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL_ACCT.OPN"
	USE
		FILENAME$ = "AR_CONTROL_ACCT"
		CONTINUE HelpError
	END WHEN

400	WHEN ERROR IN
		GET #AR_CONTROL_ACCT.CH%, REGARDLESS
	USE
		CONTINUE ReportTitle
	END WHEN

	ACCT_LIST = ACCT_LIST + AR_CONTROL_ACCT::ACCT + ","
	GOTO 400

	%PAGE

 ReportTitle:
	!
	! Titles
	!
	TITLE$(1%) = "AR to GL comparison"
	TITLE$(2%) = "List of AR and GL Items"
	TITLE$(3%) = "For Period Ended " + RIGHT(PERIOD$, 5%) + " " + &
		LEFT(PERIOD$, 4%)

	TITLE$(4%) = ""

	!
	! Headings
	!
	TITLE$(5%) = LEFT(AR_CONTROL::CTITLE, 10%) + " Name            " + &
		"                                         AR Amount      "  + &
		"GL Amount        Dif +/-"

	TITLE$(6%) = ""

	!
	! Line layout
	!
	LYT_LINE = "$" + AR_CONTROL::CTITLE + ":010,$Name:052," + &
		"VAR_Amount:063,VGL_Amount:074,VDifference:085"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		RESET #AR_OPEN.CH%

		GET #AR_OPEN.CH%, REGARDLESS
	USE
		AR_EOF% = -1%
		CONTINUE 17005
	END WHEN

17005	WHEN ERROR IN
		RESET #GL_YYYY_PP.CH%, KEY #2%

		GET #GL_YYYY_PP.CH%, REGARDLESS
	USE
		GL_EOF% = -1%
		CONTINUE 17010
	END WHEN

17010	WHEN ERROR IN
		RESET #AR_CLOSED.CH%

		GET #AR_CLOSED.CH%, REGARDLESS
	USE
		ARC_EOF% = -1%
		CONTINUE 17020
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Check to see if we have reached the end of the files
	!
	GOTO ExitTotal IF (AR_EOF% <> 0%) AND (GL_EOF% <> 0%) AND &
		(ARC_EOF% <> 0%)

	!
	! Decide which customer to use
	!
	THIS_CUSNUM$ = STRING$(16%, 255%)
	IF AR_EOF% = 0%
	THEN
		THIS_CUSNUM$ = AR_OPEN::CUSNUM
	END IF

	IF ARC_EOF% = 0%
	THEN
		IF THIS_CUSNUM$ > AR_CLOSED::CUSNUM
		THEN
			THIS_CUSNUM$ = AR_CLOSED::CUSNUM
		END IF
	END IF

	IF GL_EOF% = 0%
	THEN
		IF GL_YYYY_PP::XREFNO < THIS_CUSNUM$
		THEN
			THIS_CUSNUM$ = GL_YYYY_PP::XREFNO
		END IF
	END IF

	AR_AMOUNT = 0.0
	GL_AMOUNT = 0.0

17030	!
	! Get the sale amount from AR_OPEN file and add it to AR_AMOUNT
	!
	WHILE (AR_OPEN::CUSNUM = THIS_CUSNUM$) AND (AR_EOF% = 0%)

		AR_AMOUNT = AR_AMOUNT + AR_OPEN::SALAMT &
			IF (AR_OPEN::UPDATED = PERIOD$) &
			AND (AR_OPEN::TRATYP <> "02")
		WHEN ERROR IN
			GET #AR_OPEN.CH%, REGARDLESS
		USE
			AR_EOF% = -1%
			CONTINUE 17035
		END WHEN
	NEXT

17035	WHILE (AR_CLOSED::CUSNUM = THIS_CUSNUM$) AND (ARC_EOF% = 0%)

		AR_AMOUNT = AR_AMOUNT + AR_CLOSED::SALAMT &
			IF (AR_CLOSED::UPDATED = PERIOD$) &
			AND (AR_CLOSED::TRATYP <> "02")
		WHEN ERROR IN
			GET #AR_CLOSED.CH%, REGARDLESS
		USE
			ARC_EOF% = -1%
			CONTINUE 17040
		END WHEN
	NEXT

17040	!
	! Pull in total of GL information
	!
 GetNextGLAmount:
	!
	! Get the amount from GL_YYYY_PP file and add it to GL_AMOUNT
	!
	WHILE (GL_YYYY_PP::XREFNO = THIS_CUSNUM$) AND (GL_EOF% = 0%)

		GL_AMOUNT = GL_AMOUNT + GL_YYYY_PP::AMOUNT &
			IF INSTR(1%, ACCT_LIST, GL_YYYY_PP::ACCT)

		WHEN ERROR IN
			GET #GL_YYYY_PP.CH%, REGARDLESS
		USE
			GL_EOF% = -1%
			CONTINUE 17050
		END WHEN

		GOTO GetNextGLAmount
	NEXT

17050	!
	! Skip this one if there is nothing to print
	!
	GOTO 17200 IF (AR_AMOUNT = 0.0) AND (GL_AMOUNT = 0.0)

	!
	! Get next record
	!
	AR_35CUSTOM::CUSNUM = THIS_CUSNUM$
	AR_35CUSTOM::CUSNAM = "??????????????????????????????"

	WHEN ERROR IN
		GET #AR_35CUSTOM.CH%, KEY #0% EQ THIS_CUSNUM$, REGARDLESS
	USE
		CONTINUE 17060
	END WHEN

17060	!
	! Print results
	!
	DIFFER = AR_AMOUNT - GL_AMOUNT

	AR_TOTAL = AR_TOTAL + AR_AMOUNT
	GL_TOTAL = GL_TOTAL + GL_AMOUNT
	DIF_TOTAL = DIF_TOTAL + DIFFER

	!
	! Print the customer line
	!
	TEXT$ = AR_35CUSTOM::CUSNUM + "  " + &
		AR_35CUSTOM::CUSNAM + "  " + &
		FORMAT$(AR_AMOUNT, "##,###,###.##") + "  " + &
		FORMAT$(GL_AMOUNT, "##,###,###.##") + "  " + &
		FORMAT$(DIFFER, "##,###,###.##")

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

17200	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
	!
	! Print totals
	!
	TEXT$ = "            Grand Total:          " + &
		"                              " + &
		FORMAT$(AR_TOTAL, "##,###,###.##") + "  " + &
		FORMAT$(GL_TOTAL, "##,###,###.##") + "  " + &
		FORMAT$(DIF_TOTAL, "##,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO ExitProgram

 ExitProgram:
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

	FILENAME$ = ""

	!
	! Untrapped error
	!
	RESUME HelpError

32767	END
