1	%TITLE "Summary Period Ledger Report"
	%SBTTL "JC_RPRT_SUMMMARY"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986, 1988, 1989, 1990 BY
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
	! ID:JC0015
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program prints the ^*Summary Period Ledger list\* from
	!	the Job Costing system. The following fields are included in this list:
	!	.table 3,25
	!	.te
	!	Job Number	Job Description
	!	.te
	!	Reference Number	Type
	!	.te
	!	Class	Source
	!	.te
	!	Date	Operation
	!	.te
	!	Account Number	Account Description
	!	.te
	!	Item Description	Beginning Balance
	!	.te
	!	Period Balance	End Balance
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Summary Period Ledger List
	!	.x List>Summary Period Ledger
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS JC_SOURCE:JC_RPRT_SUMMARY/LINE
	!	$ LINK/EXE=JC_EXE: JC_RPRT_SUMMARY, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE JC_RPRT_SUMMARY.OBJ;*
	!
	! Author:
	!
	!	12/15/92 - Frank F. Starman
	!
	! Modification History:
	!
	!	01/08/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/18/93 - Kevin Handy
	!		Added parameter to GL_OUTP_ACCTSUM for units.
	!
	!	04/01/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/11/93 - Dan Perkins
	!		Don't print jobs if there is a zero end balance.
	!
	!	06/09/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	01/26/95 - Kevin Handy
	!		Increaded dimension for BAL_ARRAY from 50 to 100
	!		for DWI crash.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	07/08/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/26/97 - Kevin Handy
	!		Increase size of description from 20 to 30 when
	!		TOTALS.ONLY is "YES" (LL)
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/31/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.HB"
	MAP (SB_CONTROL)	SB_CONTROL_CDD		SB_CONTROL

	%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.HB"
	MAP (SB_BALANCE)	SB_BALANCE_CDD		SB_BALANCE
	DECLARE			SB_BALANCE_CDD		BAL_ARRAY(100%)

	%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.HB"
	MAP (SB_ACCOUNT)	SB_ACCOUNT_CDD		SB_ACCOUNT

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)		GL_PERIOD_CDD		GL_PERIOD

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	DECLARE			GL_CHART_CDD		GL_CHART_EXAM

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION GL_EXAM_CHART
	EXTERNAL LONG    FUNCTION GL_OUTP_ACCTSUM

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	.x Sort By
	!	^*(01) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field causes the report to print in
	!	a selected order.
	!	.b
	!	The following values are valid:
	!	.table 3,25
	!	.te
	!	^*C\* - Class
	!	.te
	!	^*J\* - Job Number
	!	.te
	!	^*T\* - Job Type
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes the printing
	!	to begin with a selected item.  The value entered must be in
	!	agreement with the value in field (01) Sort by.
	!	.b
	!	A blank field causes the report to begin with the first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* causes the report to end printing
	!	with a selected item.  The value entered must be in agreement
	!	with the value in field (01) Sort by.
	!	.b
	!	A blank field causes the report to end with the last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field prints a report including selected
	!	items only using the "wildcarding" technique.
	!	.b
	!	For more information on "Wildcarding", see Appendix A.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

	STAT_WC$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Status Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Status Wildcard\* field selects the
	!	Job Status to be included by using the "wildcarding"
	!	technique.
	!	.b
	!	Valid job status flags are:
	!	.table 3,25
	!	.te
	!	^*A\* - Active Jobs
	!	.te
	!	^*C\* - Closed Jobs
	!	.te
	!	^*I\* - Inactive Jobs
	!	.end table
	!	For more information on "Wildcarding", see Appendix A.
	!	.lm -5
	!
	! Index:
	!	.x Status Wildcard
	!
	!--

	TOTAL_ONLY$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 132%)

	!++
	! Abstract:FLD06
	!	^*(06) Totals Only\*
	!	.b
	!	.lm +5
	!	The ^*Totals Only\* field causes the report to print
	!	only job totals.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Totals Only
	!
	!--

	EXCL_ZERO$ = EDIT$(UTL_REPORTX::OPTDEF(6%), 132%)

	!++
	! Abstract:FLD07
	!	^*(07) Exclude Zero\*
	!	.b
	!	.lm +5
	!	The ^*Exclude Zero\* flag field causes the
	!	report to print only jobs with balances.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Exclude Zero
	!
	!--

	DEF_SYSTEM$ = "JC"
	DEF_SUBJECT$ = "J"

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
	USE
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.OPN"
		GET #SB_CONTROL.CH%, KEY #0% EQ DEF_SYSTEM$, REGARDLESS
	USE
		FILENAME$ = "SB_CONTROL"
		CONTINUE HelpError
	END WHEN

	YYYYPP$ = SB_CONTROL::PERIOD
	YYYY_PP$ = LEFT(YYYYPP$, 4%) + "_" + RIGHT(YYYYPP$, 5%)

	CLOSE SB_CONTROL.CH%, GL_PERIOD.CH%

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.OPN"
	USE
		FILENAME$ = "SB_BALANCE"
		CONTINUE HelpError
	END WHEN

330	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.OPN"
	USE
		FILENAME$ = "SB_ACCOUNT"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	SELECT SORT_BY$

	CASE "J"
		SORT_KEY% = 0%
		ADD_TITLE$ = "BY JOB NUMBER"

	CASE "T"
		SORT_KEY% = 1%
		ADD_TITLE$ = "BY JOB TYPE"

	CASE "C"
		SORT_KEY% = 2%
		ADD_TITLE$ = "BY JOB CLASS"
	END SELECT

	TITLE$(1%) = "JOB SUMMARY REPORT " + ADD_TITLE$
	TITLE$(2%) = "Job Costing System"
	TITLE$(3%) = "Period " + YYYYPP$
	TITLE$(4%) = ""

	!
	! Heading
	!
	IF TOTAL_ONLY$ <> "Y"
	THEN
		TITLE$(5%) = "Job#       Description          Ty Clas " + &
			"Operation Account#           Description      " + &
			"    BegBalance       ChangeBal      EndBalance"
	ELSE
		TITLE$(5%) = "Job#       Description                    Ty Clas " + &
			"StartDate   CloseDate               " + &
			"    BegBalance       ChangeBal      EndBalance"
	END IF

	TITLE$(6%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		FIND #SB_SUBACCOUNT.CH%, &
			KEY #SORT_KEY% GE "J" + FROM_ITEM$, &
			REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

	RRECORD% = -1%

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #SB_SUBACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

	GOTO ExitTotal IF SB_SUBACCOUNT::SUBJECT <> DEF_SUBJECT$

	GOTO GetNextRec IF STAT_WC$ <> "" AND COMP_ARRAY(EDIT$( &
		SB_SUBACCOUNT::SSTATUS, -1%), STAT_WC$) = 0%

	SELECT SORT_BY$

	CASE "J"
		GOTO ExitTotal IF (SB_SUBACCOUNT::SUBACCOUNT > &
			TO_ITEM$) AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$( &
			SB_SUBACCOUNT::SUBACCOUNT, -1%), WLDCRD$) = 0%

	CASE "T"
		GOTO ExitTotal IF (SB_SUBACCOUNT::TTYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$( &
			SB_SUBACCOUNT::TTYPE, -1%), WLDCRD$) = 0%

		IF OLD_TYPE$ <> SB_SUBACCOUNT::TTYPE OR RRECORD% = -1%
		THEN
			GOSUB SubTotal IF RRECORD% = 0%
			OLD_TYPE$ = SB_SUBACCOUNT::TTYPE
			RRECORD% = 0%
		END IF

	CASE "C"
		GOTO ExitTotal IF (SB_SUBACCOUNT::CLASS > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$( &
			SB_SUBACCOUNT::CLASS, -1%), WLDCRD$) = 0%

		IF OLD_CLASS$ <> SB_SUBACCOUNT::CLASS OR RRECORD% = -1%
		THEN
			GOSUB SubTotal IF RRECORD% = 0%
			OLD_CLASS$ = SB_SUBACCOUNT::CLASS
			RRECORD% = 0%
		END IF

	END SELECT

	COUNTER% = 0%

17100	WHEN ERROR IN
		FIND #SB_BALANCE.CH%, &
			KEY #0% EQ DEF_SYSTEM$ + SB_SUBACCOUNT::SUBACCOUNT, &
			REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155%
		FILENAME$ = "SB_BALANCE"
		CONTINUE HelpError
	END WHEN

 GetNextRec2:
17120	WHEN ERROR IN
		GET #SB_BALANCE.CH%, REGARDLESS
	USE
		CONTINUE JobTot IF ERR = 11%
		FILENAME$ = "SB_BALANCE"
		CONTINUE HelpError
	END WHEN

	GOTO JobTot IF SB_BALANCE::SYSTEM <> DEF_SYSTEM$ OR &
		SB_BALANCE::SUBACCOUNT <> SB_SUBACCOUNT::SUBACCOUNT

	GOTO GetNextRec2 IF SB_BALANCE::PERIOD <> YYYYPP$

	!
	! Find an account for the balance record
	!
17200	WHEN ERROR IN
		FIND #SB_ACCOUNT.CH%, KEY #0% EQ SB_BALANCE::SYSTEM, REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155%
		CONTINUE HelpError
	END WHEN

 NextAcct:
17220	WHEN ERROR IN
		GET #SB_ACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 11%
		CONTINUE HelpError
	END WHEN

	GOTO GetNextRec2 IF SB_ACCOUNT::SYSTEM <> SB_BALANCE::SYSTEM

	IF COMP_ARRAY(SB_BALANCE::ACCOUNT, SB_ACCOUNT::ACCOUNT) = 0%
	THEN
		GOTO NextAcct
	END IF

	JT_BEG_BAL = FUNC_ROUND(JT_BEG_BAL + SB_BALANCE::BEG_AMOUNT, 2%)

	JT_CUR_BAL = FUNC_ROUND(JT_CUR_BAL + SB_BALANCE::AMOUNT, 2%)

	JT_END_BAL = FUNC_ROUND(JT_END_BAL + SB_BALANCE::BEG_AMOUNT + &
		SB_BALANCE::AMOUNT, 2%)

	GOTO GetNextRec2 IF (FUNC_ROUND(SB_BALANCE::BEG_AMOUNT, 2%) = 0.0) &
		AND (FUNC_ROUND(SB_BALANCE::AMOUNT, 2%) = 0.0)

	!
	! Load array
	!
	COUNTER% = COUNTER% + 1%
	BAL_ARRAY(COUNTER%) = SB_BALANCE

	GOTO GetNextRec2

 JobTot:
	GOTO OutaHere IF (EXCL_ZERO$ = "Y") AND (JT_END_BAL = 0.0) OR &
		(COUNTER% = 0%)

	FOR I% = 1% TO COUNTER%

		CCHANGE = FUNC_ROUND(BAL_ARRAY(I%)::AMOUNT, 2%)

		BAL_ARRAY(I%)::BEG_AMOUNT  = &
			FUNC_ROUND(BAL_ARRAY(I%)::BEG_AMOUNT, 2%)

		GOTO ExitProgram IF GL_OUTP_ACCTSUM (OPT_ADDREC, BAL_ARRAY(I%)::ACCOUNT, &
			BAL_ARRAY(I%)::BEG_AMOUNT, CCHANGE, 0.0, TITLE$(), UTL_REPORTX) <> CMC$_NORMAL

	NEXT I%

	IF TOTAL_ONLY$ = "Y"
	THEN
		TEXT$ = SB_SUBACCOUNT::SUBACCOUNT + " "  + &
			LEFT(SB_SUBACCOUNT::DESCR, 30%) + " "  + &
			SB_SUBACCOUNT::TTYPE + " "  + &
			SB_SUBACCOUNT::CLASS + " "  + &
			PRNT_DATE(SB_SUBACCOUNT::BDATE, 8%) + "  " + &
			PRNT_DATE(SB_SUBACCOUNT::EDATE, 8%) + "  " + &
			STRING$(10%, A"."B) + " "  + &
			FORMAT$(JT_BEG_BAL, " ###,###,###.##") + "  " + &
			FORMAT$(JT_CUR_BAL, "###,###,###.##")  + "  " + &
			FORMAT$(JT_END_BAL, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	ELSE
		FOR I% = 1% TO COUNTER%

			V% = GL_EXAM_CHART(BAL_ARRAY(I%)::ACCOUNT, GL_CHART_EXAM)

			!
			! Print out one line
			!
			TEXT$ = SB_SUBACCOUNT::SUBACCOUNT + " "  + &
				LEFT(SB_SUBACCOUNT::DESCR, 20%) + " "  + &
				SB_SUBACCOUNT::TTYPE + " "  + &
				SB_SUBACCOUNT::CLASS + " "  + &
				BAL_ARRAY(I%)::OPERATION + "  " + &
				BAL_ARRAY(I%)::ACCOUNT + " "  + &
				LEFT(GL_CHART_EXAM::DESCR, 15%) + "  " + &
				FORMAT$(BAL_ARRAY(I%)::BEG_AMOUNT, &
					"###,###,###.##") + "  " + &
				FORMAT$(BAL_ARRAY(I%)::AMOUNT, &
					"###,###,###.##") + "  " + &
				FORMAT$(BAL_ARRAY(I%)::BEG_AMOUNT + &
					BAL_ARRAY(I%)::AMOUNT, "###,###,###.##")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			GOTO ExitProgram IF UTL_REPORTX::STAT

			SB_SUBACCOUNT::DESCR = &
				STRING$(LEN(SB_SUBACCOUNT::DESCR), A"."B)
			SB_SUBACCOUNT::TTYPE = &
				STRING$(LEN(SB_SUBACCOUNT::TTYPE), A"."B)
			SB_SUBACCOUNT::CLASS = &
				STRING$(LEN(SB_SUBACCOUNT::CLASS), A"."B)

		NEXT I%

		TEXT$ = SB_SUBACCOUNT::SUBACCOUNT + " "  + &
			"JOB TOTALS " + STRING$(63%, A"."B) + &
			FORMAT$(JT_BEG_BAL, " ###,###,###.##") + "  " + &
			FORMAT$(JT_CUR_BAL, "###,###,###.##") + "  " + &
			FORMAT$(JT_END_BAL, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	END IF

	GOTO ExitProgram IF UTL_REPORTX::STAT

	ST_BEG_BAL = ST_BEG_BAL	+ JT_BEG_BAL
	ST_CUR_BAL = ST_CUR_BAL	+ JT_CUR_BAL
	ST_END_BAL = ST_END_BAL	+ JT_END_BAL

 OutaHere:
	JT_BEG_BAL, JT_CUR_BAL, JT_END_BAL = 0.0

	GOTO GetNextRec

 ExitTotal:
17400	!
	! Handle end of report
	!
	SELECT	SORT_BY$

	CASE "J"
		GT_BEG_BAL = GT_BEG_BAL	+ ST_BEG_BAL
		GT_CUR_BAL = GT_CUR_BAL	+ ST_CUR_BAL
		GT_END_BAL = GT_END_BAL	+ ST_END_BAL

	CASE "T", "C"
		GOSUB SubTotal

	END SELECT

	TEXT$ = "GRAND TOTALS" + SPACE$(74%) + &
		FORMAT$(GT_BEG_BAL, "###,###,###.##") + "  " + &
		FORMAT$(GT_CUR_BAL, "###,###,###.##") + "  " + &
		FORMAT$(GT_END_BAL, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	V% = GL_OUTP_ACCTSUM(OPT_SUMMARY + SUBOPT_DETAIL, "", &
		0.0, 0.0, 0.0, TITLE$(), UTL_REPORTX)

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

 SubTotal:
	SELECT SORT_BY$

	CASE "T"
		TEXT$ = "TYPE  " + OLD_TYPE$ + " TOTALS" + &
			SPACE$(73% - LEN(OLD_TYPE$))

	CASE "C"
		TEXT$ = "CLASS " + OLD_CLASS$ + " TOTALS" + &
			SPACE$(73% - LEN(OLD_CLASS$))

	END SELECT

	TEXT$ = TEXT$ + &
		FORMAT$(ST_BEG_BAL, "###,###,###.##") + "  " + &
		FORMAT$(ST_CUR_BAL, "###,###,###.##") + "  " + &
		FORMAT$(ST_END_BAL, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GT_BEG_BAL = GT_BEG_BAL	+ ST_BEG_BAL
	GT_CUR_BAL = GT_CUR_BAL	+ ST_CUR_BAL
	GT_END_BAL = GT_END_BAL	+ ST_END_BAL

	ST_BEG_BAL, ST_CUR_BAL, ST_END_BAL = 0.0

	RETURN

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
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
	!+-+-+
	!++
	! Abstract:FLD08
	!	^*(08) Exclude Zero\*
	!	.b
	!	.lm +5
	!	The ^*Exclude Zero\* flag field causes the report to print
	!	only jobs with balances.
	!	.lm -5
	!
	! Index:
	!	.x Exclude Zero
	!
	!--
