1	%TITLE "Daily Audit Report"
	%SBTTL "GL_RPRT_AUDIT_SUBACCOUNT"
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
	! ID:GL0005
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Daily Audit Report\* lists the transmittal information for
	!	each batch, which has been posted to a specified General
	!	Ledger file.
	!	.b
	!	The following columns are included:
	!	.table 30
	!	.te
	!	Account Number
	!	.te
	!	Description
	!	.te
	!	Debit Amount
	!	.te
	!	Credit Amount
	!	.end table
	!	.LM -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_RPRT_AUDIT_SUBACCOUNT/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_RPRT_AUDIT_SUBACCOUNT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_RPRT_AUDIT_SUBACCOUNT.OBJ;*
	!
	! Author:
	!
	!	02/22/2000 - Kevin Handy
	!
	! Modification history:
	!
	!	02/24/2000 - Kevin Handy
	!		Add subtotals and grand totals
	!
	!	11/02/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Add several REGARDLESS clauses.
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
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP)	GL_YYYY_PP_CDD		GL_YYYY_PP

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	DECLARE			GL_CHART_CDD		GL_CHART_EXAM

	%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.HB"
	MAP (SB_ACCOUNT)	SB_ACCOUNT_CDD		SB_ACCOUNT

	!
	! Define record for the array(s)
	!
	RECORD TOTAL_RECORD
		STRING	SUBACCT = 10%
		STRING	ACCT = 18%
		STRING	FIRSTPERIOD = 8%
		REAL	AMOUNT
	END RECORD

	MAP (GL_WORK) TOTAL_RECORD GL_WORK

	!
	! Dimension statements
	!
	DIM GL_YYYY_PP_FILE$(1000%)
	DIM ACCOUNT$(400%)
	DIM TEXT$(400%)

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

	START_PERIOD$ = TRM$(LEFT(UTL_REPORTX::OPTDEF(0%), 4%) + "_" + &
		RIGHT(UTL_REPORTX::OPTDEF(0%), 5%))

	!++
	! Abstract:FLD01
	!	^*(01) Report Date\*
	!	.b
	!	.lm +5
	!	The ^*Report Date\* allows printing of a report
	!	including only those sub accounts with transactions on or before
	!	the Report Date.
	!	.b
	!	A blank field will cause the report to print all
	!	backorders in the register file.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!
	!--

	END_PERIOD$ = TRM$(LEFT(UTL_REPORTX::OPTDEF(1%), 4%) + "_" + &
		RIGHT(UTL_REPORTX::OPTDEF(1%), 5%))

	!++
	! Abstract:FLD02
	!	^*(02) Period\*
	!	.b
	!	.lm +5
	!	^*Period\* refers to the accounting period that will be considered when
	!	running the report.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) From SubAccount _#\*
	!	.b
	!	.lm +5
	!	The ^*From SubAccount _#\* setting selects
	!	a sub account number from which to begin printing.
	!	.b
	!	If the report is to begin with the first sub account _# in the
	!	file, this field should be left blank.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) To SubAccount _#\*
	!	.b
	!	.lm +5
	!	The ^*To SubAccount _#\* setting selects a
	!	sub account number with which the report is to end.
	!	.b
	!	If the report is to end with the last sub account _# in the
	!	file, this field should be left blank.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) SubAccount Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*SubAccount Wildcard\* setting prints
	!	the report including selected sub accounts only, using
	!	wildcard techniques. A blank or an _* in this field will
	!	cause the report to list ^&all\& sub accounts.
	!	.lm -5
	!
	! Index:
	!
	!--


300	SYSTEM$ = "JC"

	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.OPN"
		FIND #SB_ACCOUNT.CH%, KEY #0% GE SYSTEM$, REGARDLESS
	USE
		FILENAME$ = "SB_ACCOUNT"
		CONTINUE HelpError
	END WHEN

 NextAcct:
	WHEN ERROR IN
		GET #SB_ACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE 330
	END WHEN

	IF SYSTEM$ = SB_ACCOUNT::SYSTEM
	THEN
		ACCOUNT% = ACCOUNT% + 1%
		ACCOUNT$(ACCOUNT%) = TRM$(SB_ACCOUNT::ACCOUNT)
	END IF

	GOTO NextAcct

330	!
	! Create temporary file
	!
	CALL ASSG_CHANNEL(GL_WORK.CH%, STAT%)
	WHEN ERROR IN
		OPEN "GL_WORK.TMP" FOR OUTPUT AS FILE GL_WORK.CH%, &
			ORGANIZATION INDEXED FIXED, &
			TEMPORARY, &
			BUFFER 32%, &
			MAP GL_WORK, &
			PRIMARY KEY &
				(GL_WORK::SUBACCT, GL_WORK::ACCT) &
				NODUPLICATES, &
			ACCESS MODIFY, &
			ALLOW NONE
	USE
		FILENAME$ = "GL_WORK"
		CONTINUE HelpError
	END WHEN

400	!
	! Process GL Files
	!
	CALL READ_DEVICE("GL_YYYY_PP", GL_YYYY_PP.DEV$, STAT%)

	CALL FIND_FILE( GL_YYYY_PP.DEV$ + "GL_*.LED", GL_YYYY_PP_FILE$(), &
		16%, "", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram
	END SELECT

	GL_YYYY_PP_FILE% = VAL%(GL_YYYY_PP_FILE$(0%))

	IF GL_YYYY_PP_FILE%
	THEN
		FOR LOOP% = 1% TO GL_YYYY_PP_FILE%
			YYYY_PP$ = MID(GL_YYYY_PP_FILE$(LOOP%), 4%, 7%)

			IF YYYY_PP$ >= START_PERIOD$ AND &
				YYYY_PP$ <= END_PERIOD$
			THEN
				GOSUB ScanFile
			END IF
		NEXT LOOP%

		GOTO ReportTitle
	ELSE
		GOTO ExitProgram
	END IF

	!
	! Process one period file
	!
 ScanFile:
1300	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"
	USE
		CONTINUE 1390
	END WHEN

	CALL ENTR_3MESSAGE(SCOPE, "Processing: " + YYYY_PP$, 1%)

	WHEN ERROR IN
		IF FROM_ITEM$ <> ""
		THEN
			FIND #GL_YYYY_PP.CH%, &
				KEY #1% GE FROM_ITEM$, &
				REGARDLESS
		ELSE
			FIND #GL_YYYY_PP.CH%, &
				KEY #1% GT "          ", &
				REGARDLESS
		END IF
	USE
		CONTINUE 1390
	END WHEN

1310	WHEN ERROR IN
		GET #GL_YYYY_PP.CH%, REGARDLESS
	USE
		CONTINUE 1390
	END WHEN

	IF TO_ITEM$ <> "" AND GL_YYYY_PP::SUBACC > TO_ITEM$
	THEN
		GOTO 1390
	END IF

	IF WLDCRD$ <> ""
	THEN
		GOTO 1310 IF COMP_STRING(GL_YYYY_PP::SUBACC, WLDCRD$) = 0%
	END IF

	GOTO 1320 IF COMP_STRING(TRM$(GL_YYYY_PP::ACCT), ACCOUNT$(I%)) <> 0% &
		FOR I% = 1% TO ACCOUNT%

	GOTO 1310

1320	WHEN ERROR IN
		GET #GL_WORK.CH%, &
			KEY #0% EQ GL_YYYY_PP::SUBACC + GL_YYYY_PP::ACCT
	USE
		GL_WORK::SUBACCT = GL_YYYY_PP::SUBACC
		GL_WORK::ACCT = GL_YYYY_PP::ACCT
		GL_WORK::FIRSTPERIOD = YYYY_PP$
		GL_WORK::AMOUNT = GL_YYYY_PP::AMOUNT

		PUT #GL_WORK.CH%

		CONTINUE 1310
	END WHEN

	GL_WORK::AMOUNT = FUNC_ROUND(GL_WORK::AMOUNT + GL_YYYY_PP::AMOUNT, 2%)

	WHEN ERROR IN
		UPDATE #GL_WORK.CH%
	USE
		FILENAME$ = "GL_WORK"
		CONTINUE HelpError
	END WHEN

	GOTO 1310

1390	CLOSE #GL_YYYY_PP.CH%

	RETURN

	%PAGE


 ReportTitle:
	!
	! Titles
	!
	TITLE$(1%) = "SUBACCOUNT AUDIT REPORT"
	TITLE$(2%) = "FROM PERIOD " + START_PERIOD$ + " TO " + END_PERIOD$
	TITLE$(3%) = "GL System"
	TITLE$(4%) = ""

	!
	! Headers
	!
	TITLE$(5%) = "Subaccount Account            Period         Total"
	TITLE$(6%) = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		RESET #GL_WORK.CH%
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!******************************************************************
	! Main report loop starts here
	!******************************************************************

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #GL_WORK.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Total up this Sub Account if they have changed
	!
	IF GL_WORK::SUBACCT <> TEST_SUB$
	THEN
		GOSUB SubAcct IF SUBTOTAL <> 0.0
		TEXT% = 0%
		TEST_SUB$ = GL_WORK::SUBACCT
		SUBTOTAL = 0.0
	END IF

	!
	! Create Text items
	!
	IF GL_WORK::AMOUNT <> 0.0
	THEN
		TEXT% = TEXT% + 1%
		TEXT$(TEXT%) = GL_WORK::SUBACCT + " " + &
			GL_WORK::ACCT + " " + &
			GL_WORK::FIRSTPERIOD + " " + &
			FORMAT$(GL_WORK::AMOUNT, "########.##")
		SUBTOTAL = FUNC_ROUND(SUBTOTAL + GL_WORK::AMOUNT, 2%)
	END IF

!
	! Try for next record
	!
	GOTO GetNextRec

	%PAGE

 ExitTotal:
	!
	! Total up last Sub Account
	!
	GOSUB SubAcct

	TEXT$ = "          " + " " + &
		"TOTAL             " + " " + &
		"        " + " " + &
		FORMAT$(SUBTOTAL, "########.##")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

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

 SubAcct:
	!******************************************************************
	! Print out this subaccount infrmation
	!******************************************************************

	IF TEXT%
	THEN
		FOR I% = 1% TO TEXT%
			TEXT$ = TEXT$(I%)
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		NEXT I%

		TEXT$ = "          " + " " + &
			"SUB TOTAL         " + " " + &
			"        " + " " + &
			FORMAT$(SUBTOTAL, "########.##")
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)

		TEXT$ = ""
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -2%)

		TOTAL = FUNC_ROUND(TOTAL + SUBTOTAL, 2%)
	END IF

	RETURN

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

32767	END
