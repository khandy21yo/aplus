1	%TITLE "Print General Ledger Sequential Entry Report"
	%SBTTL "GL_RPRT_AUDT02"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1998 BY
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
	! ID:GLAUDT
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This report lists all
	!	transactions in a specific General Ledger file in the order in
	!	which the entries were posted.
	!	.b
	!	The following information is included in the Sequential Entry Report:
	!	.table 30
	!	.te
	!	Batch Number
	!	.te
	!	Account Number
	!	.te
	!	Transaction Date
	!	.te
	!	Source
	!	.te
	!	Reference
	!	.te
	!	Check Number
	!	.te
	!	Description
	!	.te
	!	Cross Reference
	!	.te
	!	Subaccount
	!	.te
	!	Transaction Amount
	!	.te
	!	Running Total Amount
	!	.end table
	!	.LM -15
	!
	! Index:
	!	.x Print>GL Sequential Entry Report
	!	.x GL Sequential Entry>Print
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_RPRT_AUDT02/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_RPRT_AUDT02, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_RPRT_AUDT02.OBJ;*
	!
	! Author:
	!
	!	12/10/98 - Kevin Handy
	!
	! Modification history:
	!
	!	01/06/98 - Kevin Handy
	!		Added from/to date fields 09 and 10 (LL)
	!
	!	01/28/2000 - Kevin Handy
	!		Added more information to title of this report.
	!
	!	06/22/2000 - Kevin Handy
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
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP	(GL_YYYY_PP)	GL_YYYY_PP_CDD	GL_YYYY_PP

	!
	! Declare some variables
	!
	DECLARE INTEGER CONSTANT MAX_RANGE = 500%
	DIM GL_YYYY_PP_FILE$(MAX_RANGE)

	DECLARE REAL	RUNNING_TOTAL

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
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Look up all the GL files present
	!
	CALL READ_DEVICE("GL_YYYY_PP", GL_YYYY_PP.DEV$, STAT%)

	CALL FIND_FILE(GL_YYYY_PP.DEV$ + "GL_*.LED", GL_YYYY_PP_FILE$(), &
		16%, "", "")

	GL_YYYY_PP_FILE% = VAL%(GL_YYYY_PP_FILE$(0%))

	IF GL_YYYY_PP_FILE% = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"General ledger files do not exist", 0%)
		GOTO ExitProgram
	ELSE
		GL_YYYY_PP_FILE$(LOOP%) = &
			MID(GL_YYYY_PP_FILE$(LOOP%), 4%, 4%) + &
			MID(GL_YYYY_PP_FILE$(LOOP%), 9%, 2%) &
			FOR LOOP% = 1% TO GL_YYYY_PP_FILE%
	END IF


	FROM_ITEM$ = LEFT(EDIT$(UTL_REPORTX::OPTDEF(0%), 132%), 6%)

	!++
	! Abstract:FLD01
	!	^*(01) From Batch _#\*
	!	.b
	!	.lm +5
	!	The ^*From Batch _#\* field selects a
	!	batch number to begin printing with.
	!	.b
	!	If the report is to begin with the first batch _# in the file,
	!	this field should be left blank.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = LEFT(EDIT$(UTL_REPORTX::OPTDEF(1%), 132%), 6%)

	!++
	! Abstract:FLD02
	!	^*(02) To Batch _#\*
	!	.b
	!	.lm +5
	!	The ^*To Batch _#\* field selects a batch
	!	number with which the report will end printing.
	!	.b
	!	If the report is to end with the last batch number in the file, this
	!	field should be left blank.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) Account Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Account Wildcard\* setting prints a General
	!	Ledger Audit Report including selected accounts only, using
	!	wildcard techniques. An ^*_*\* or blank in this setting
	!	will cause an Audit Report to list transactions in ^&all\&
	!	batches.
	!	.b
	!	Note: In lieu of using wildcard techniques to print
	!	an Audit Report, consideration may be given to
	!	using the General Ledger Query option.\*
	!	.lm -5
	!
	! Index:
	!
	!--


	WLDSRC$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 132%)

	!++
	! Abstract:FLD05
	!	^*(05) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Source Wildcard\* setting prints a General
	!	Ledger Audit Report including selected sources only, using
	!	wildcard techniques. An ^*_*\* or blank in this setting
	!	will cause an Audit Report to list transactions in ^&all\&
	!	batches.
	!	.b
	!	Note: In lieu of using wildcard techniques to print
	!	an Audit Report, consideration may be given to
	!	using the General Ledger Query option.\*
	!	.lm -5
	!
	! Index:
	!
	!--


	FROM_YYYY_PP$ = TRM$(UTL_REPORTX::OPTDEF(5%))

	FOR TEST% = 1% TO GL_YYYY_PP_FILE%
		FROM_PERIOD% = TEST% IF FROM_YYYY_PP$ = &
			GL_YYYY_PP_FILE$(TEST%)
	NEXT TEST%

	!++
	! Abstract:FLD06
	!	^*(06) From Period\*
	!	.b
	!	.lm +5
	!	The ^*From Period\* refers to the accounting period that will be considered
	!	when running the report.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_YYYY_PP$ = TRM$(UTL_REPORTX::OPTDEF(6%))

	FOR TEST% = 1% TO GL_YYYY_PP_FILE%
		TO_PERIOD% = TEST% IF TO_YYYY_PP$ = &
			GL_YYYY_PP_FILE$(TEST%)
	NEXT TEST%

	!++
	! Abstract:FLD07
	!	^*(07) To Period\*
	!	.b
	!	.lm +5
	!	The ^*To Period\* refers to the accounting period that will be considered
	!	when running the report.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!
	!--

	FROM_DATE$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(8%))

	!++
	! Abstract:FLD09
	!	^*(09) From Date\*
	!	.b
	!	.lm +5
	!	The ^*From Date\* Field is used to limit the report
	!	to those items that have a date on (or after) the specified
	!	date within the specified periods (declared on fields
	!	'06' and '07').
	!	.b
	!	Leaving this fiels blank will include all dates.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_DATE$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(9%))

	!++
	! Abstract:FLD10
	!	^*(10) To Date\*
	!	.b
	!	.lm +5
	!	The ^*To Date\* Field is used to limit the report
	!	to those items that have a date on (or before) the specified
	!	date within the specified periods (declared on fields
	!	'06' and '07').
	!	.b
	!	Leaving this fiels blank will include all dates.
	!	.lm -5
	!
	! Index:
	!
	!--

	RUNNING_TOTAL = 0.0

	%PAGE

 ReportTitle:
	!
	! Titles
	!
	OFFSET% = 0%
	TITLE$(1%) = "GL Audit Report"
	TITLE$(2%) = "Period " + FROM_YYYY_PP$ + " to " + TO_YYYY_PP$

	IF FROM_ITEM$ <> "" OR TO_ITEM$ <> ""
	THEN
		TITLE$(3% + OFFSET%) = "From Account " + &
			FROM_ITEM$ + " to " + &
			TO_ITEM$
		OFFSET% = OFFSET% + 1%
	END IF

	IF FROM_DATE$ > "00000000" OR TO_DATE$ > "00000000"
	THEN
		TITLE$(3% + OFFSET%) = "From Date " + &
			PRNT_DATE(FROM_DATE$, 8%) + " to " + &
			PRNT_DATE(TO_DATE$, 8%)
		OFFSET% = OFFSET% + 1%
	END IF

	IF WLDCRD$ <> ""
	THEN
		TITLE$(3% + OFFSET%) = "Account Wildcard " + WLDCRD$
		OFFSET% = OFFSET% + 1%
	END IF

	IF WLDSRC$ <> ""
	THEN
		TITLE$(3% + OFFSET%) = "Source Wildcard " + WLDSRC$
		OFFSET% = OFFSET% + 1%
	END IF

	TITLE$(3% + OFFSET%) = ""

	!
	! Headings
	!
	TITLE$(4% + OFFSET%) = "      Period       Dollars          Hours          Units"
	TITLE$(5% + OFFSET%) = ""

	!
	! Layouts for printed lines
	!
	LYT_LINE$ = "$BatchNum:006,$Account:025,DTranDate:034," + &
		"$Source:039,$RefNum:056,$CheckNum:063,$Descr:084," + &
		"$XRefNum:091,$SubAcct:102,VAmount:116,VTotal:131"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	FOR FILE_LOOP% = FROM_PERIOD% TO TO_PERIOD%

		CLOSE #GL_YYYY_PP.CH%

		YYYY_PP$ = LEFT(GL_YYYY_PP_FILE$(FILE_LOOP%), 4%) + "_" + &
			RIGHT(GL_YYYY_PP_FILE$(FILE_LOOP%), 5%)

		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"
		USE
			FILENAME$ = "GL_" + YYYY_PP$
			CONTINUE HelpError
		END WHEN

		GOSUB 17050

	NEXT FILE_LOOP%

	GOTO ExitTotal

17050	!
	! Work on one file
	!
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #GL_YYYY_PP.CH%, KEY #0%
		ELSE
			FIND #GL_YYYY_PP.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		FILENAME$ = "GL_" + YYYY_PP$
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
		GET #GL_YYYY_PP.CH%, REGARDLESS
	USE
		CONTINUE 17200 IF ERR = 11%
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Check current record
	!
	GOTO 17200 IF (TRM$(GL_YYYY_PP::ACCT) > TO_ITEM$) &
		AND TO_ITEM$ <> ""

	IF WLDCRD$ <> ""
	THEN
		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(GL_YYYY_PP::ACCT, -1%), &
			WLDCRD$) = 0%
	END IF

	IF WLDSRC$ <> ""
	THEN
		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(GL_YYYY_PP::SOURCE, -1%), &
			WLDSRC$) = 0%
	END IF

 ! TEXT$ = "TEST: FROM=" + FROM_DATE$ + ", TO=" + TO_DATE$ + ", TRAN=" + GL_YYYY_PP::TRANDAT
 ! CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	IF FROM_DATE$ > "00000000"
	THEN
		GOTO GetNextRec &
			IF GL_YYYY_PP::TRANDAT < FROM_DATE$
	END IF

	IF TO_DATE$ > "00000000"
	THEN
		GOTO GetNextRec &
			IF GL_YYYY_PP::TRANDAT > TO_DATE$
	END IF

	!
	! Print out one line of the report
	!
	RUNNING_TOTAL = RUNNING_TOTAL + GL_YYYY_PP::AMOUNT
	RUNNING_HOURS = RUNNING_HOURS + GL_YYYY_PP::HOURS
	RUNNING_UNITS = RUNNING_UNITS + GL_YYYY_PP::UNITS

	GRAND.TOTAL = GRAND.TOTAL + GL_YYYY_PP::AMOUNT
	GRAND.HOURS = GRAND.HOURS + GL_YYYY_PP::HOURS
	GRAND.UNITS = GRAND.UNITS + GL_YYYY_PP::UNITS

	!
	! Try for next record
	!
	GOTO GetNextRec

17200	!
	! Print Out Totals for the period
	!
	TEXT$ = "Total " + &
		GL_YYYY_PP_FILE$(FILE_LOOP%) + " " + &
		FORMAT$(RUNNING_TOTAL, "##,###,###.## ") + &
		FORMAT$(RUNNING_HOURS, "###,###,###.## ") + &
		FORMAT$(RUNNING_UNITS, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	RUNNING_TOTAL = 0.0
	RUNNING_HOURS = 0.0
	RUNNING_UNITS = 0.0

	RETURN

	%PAGE

17900	!******************************************************************
	! Handle totals and other items before EXITing
	!******************************************************************

 ExitTotal:
	!
	! Print out totals
	!
	!
	! Print Out Totals for the period
	!
	TEXT$ = "Grand Total  " + &
		FORMAT$(GRAND.TOTAL, "##,###,###.## ") + &
		FORMAT$(GRAND.HOURS, "###,###,###.## ") + &
		FORMAT$(GRAND.UNITS, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -3%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -2%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

 ExitProgram:
	!
	! Finish up the report
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
	! End of report GL_RPRT_AUDT02
	!******************************************************************
	END
