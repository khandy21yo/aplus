1	%TITLE "Summary Period Ledger List"
	%SBTTL "JC_RPRT_SUMPERLED_per"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2005 BY
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
	! ID:JC0005
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
	!	$ BAS JC_SOURCE:JC_RPRT_SUMPERLED_per/LINE
	!	$ LINK/EXE=JC_EXE: JC_RPRT_SUMPERLED_per, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE JC_RPRT_SUMPERLED_per.OBJ;*
	!
	! Author:
	!
	!	03/04/2005 - Kevin Handy
	!
	! Modification History:
	!
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

 !	%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.HB"
 !	MAP (SB_BALANCE)	SB_BALANCE_CDD		SB_BALANCE
 !	DECLARE SB_BALANCE_CDD SB_BALANCE_USE

	%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.HB"
	MAP (SB_ACCOUNT)	SB_ACCOUNT_CDD		SB_ACCOUNT

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	DECLARE			GL_CHART_CDD		GL_CHART_EXAM

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP)	GL_YYYY_PP_CDD		GL_YYYY_PP

	DECLARE INTEGER CONSTANT MAX_PERIODS = 120%
	DIM GL_YYYY_PP_CDD GL_DATA(MAX_PERIODS)
	DIM GL_DATA.CH%(MAX_PERIODS)
	DIM GL_EOF%(MAX_PERIODS)
	DIM GL_YYYY_PP_FILE$(512%)

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
	!	.x Sort By
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes the printing
	!	to begin with a selected item.
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
	!	The ^*To Item\* field causes the report to end printing
	!	with a selected item.
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
	!	^*(05) Status\*
	!	.b
	!	.lm +5
	!	The ^*Status\* field enters the status of the job at
	!	the current time.
	!	.lm -5
	!
	! Index:
	!	.x Status
	!
	!--

	DET_TOT$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 132%)

	!++
	! Abstract:FLD06
	!	^*(06) Detail or Total\*
	!	.b
	!	.lm +5
	!	The ^*Detail or Total\* field prints the report containing
	!	only the totals found in the Job Costing system by entering a ^*T\*.
	!	By entering a ^*D\*, detail from the General Ledger is read and the totals
	!	are compared with the totals from the Job Costing. If the totals do not agree,
	!	the ^*Resync\* program must be run.
	!	.lm -5
	!
	! Index:
	!	.x Detail or Total
	!
	!--

	FROM_PERIOD$ = TRM$(UTL_REPORTX::OPTDEF(8%))

	!++
	! Abstract:FLD09
	!	^*(09) From Period\*
	!	.b
	!	.lm +5
	!	Period to start the report on.
	!	.lm -5
	!
	! Index:
	!	.x From Period
	!
	!--

	TO_PERIOD$ = TRM$(UTL_REPORTX::OPTDEF(9%))

	!++
	! Abstract:FLD10
	!	^*(10) To Period\*
	!	.b
	!	.lm +5
	!	Period to end the report on.
	!	.lm -5
	!
	! Index:
	!	.x To Period
	!
	!--

	DEF_SYSTEM$ = "JC"
	DEF_SUBJECT$ = "J"

	SELECT SORT_BY$

	CASE "J"
		SORT_KEY% = 0%
		ADD_TITLE$ = "BY  JOB  NUMBER"

	CASE "T"
		SORT_KEY% = 1%
		ADD_TITLE$ = "BY  JOB  TYPE"

	CASE "C"
		SORT_KEY% = 2%
		ADD_TITLE$ = "BY  JOB  CLASS"
	END SELECT

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
	USE
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.OPN"
		GET #SB_CONTROL.CH%, KEY #0% EQ DEF_SYSTEM$, REGARDLESS
		CLOSE SB_CONTROL.CH%
	USE
		FILENAME$ = "SB_CONTROL"
		CONTINUE HelpError
	END WHEN

	YYYYPP$ = SB_CONTROL::PERIOD
	YYYY_PP$ = LEFT(YYYYPP$, 4%) + "_" + RIGHT(YYYYPP$, 5%)

320 !	WHEN ERROR IN
 !		%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.OPN"
 !	USE
 !		FILENAME$ = "SB_BALANCE"
 !		CONTINUE HelpError
 !	END WHEN

330	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.OPN"
	USE
		FILENAME$ = "SB_ACCOUNT"
		CONTINUE HelpError
	END WHEN

	!
	! Generate a fast searchable list of valic accounts
	!
	SB_ACCOUNT_LIST$ = ""

	WHEN ERROR IN
		FIND #SB_ACCOUNT.CH%, KEY #0% GE DEF_SYSTEM$, REGARDLESS
	USE
		FILENAME$ = "SB_ACCOUNT"
		CONTINUE HelpError
	END WHEN

335	WHEN ERROR IN
		GET #SB_ACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE 340 IF ERR = 11%
		FILENAME$ = "SB_ACCOUNT"
		CONTINUE HelpError
	END WHEN

	IF SB_ACCOUNT::SYSTEM = DEF_SYSTEM$
	THEN
		IF SB_ACCOUNT_LIST$ = ""
		THEN
			SB_ACCOUNT_LIST$ = TRM$(SB_ACCOUNT::ACCOUNT)
		ELSE
			SB_ACCOUNT_LIST$ = SB_ACCOUNT_LIST$ + &
				"," + TRM$(SB_ACCOUNT::ACCOUNT)
		END IF

		GOTO 335
	END IF

	CLOSE SB_ACCOUNT.CH%

	CALL ASSG_FREECHANNEL(SB_ACCOUNT.CH%)

340	!

370	!
	! Open up all GL periods
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
		GL_FILE% = 0%
		FOR LOOP% = 1% TO GL_YYYY_PP_FILE%
			TEMP$ = MID(GL_YYYY_PP_FILE$(LOOP%), 4%, 4%) + &
				MID(GL_YYYY_PP_FILE$(LOOP%), 9%, 2%)

			IF TEMP$ >= FROM_PERIOD$ AND TEMP$ <= TO_PERIOD$
			THEN
				GL_FILE% = GL_FILE% + 1%
				GL_YYYY_PP_FILE$(GL_FILE%) = &
					MID(GL_YYYY_PP_FILE$(LOOP%), 4%, 7%)
			END IF
		NEXT LOOP%
	END IF

	IF GL_FILE% = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"No General ledger files within range", 0%)
		GOTO ExitProgram
	END IF

	FOR LOOP% = 1% TO GL_FILE%
		YYYY_PP$ = GL_YYYY_PP_FILE$(LOOP%)
		GL_YYYY_PP.CH% = 0%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"
		USE
			CONTINUE ReportTitle IF ERR = 5%
			FILENAME$ = "GL_YYYY_PP"
			CONTINUE HelpError
		END WHEN

		GL_DATA.CH%(LOOP%) = GL_YYYY_PP.CH%
	NEXT LOOP%

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "SUMMARY  PERIOD  LEDGER  REPORT  " + ADD_TITLE$
	TITLE$(2%) = "Job Costing System"
	TITLE$(3%) = "Period " + YYYYPP$
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "Job#       Description          Ty Clas " + &
		"Operation Account#           Description      " + &
		"    BegBalance       PeriodBal      EndBalance"

	IF DET_TOT$ = "D"
	THEN
		TITLE$(6%) = "                Reference#       Source Date     " + &
			"  Description            XRef#"

		TITLE$(7%) = "."
	ELSE
		TITLE$(6%) = "."
	END IF

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		FIND #SB_SUBACCOUNT.CH%, &
			KEY #SORT_KEY% GE "J" + FROM_ITEM$, &
			REGARDLESS
	USE
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to find beginning record!", 0%)
		CONTINUE ExitProgram
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

	GOTO GetNextRec &
		IF STAT_WC$ <> "" AND COMP_ARRAY(EDIT$( &
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
			COMP_ARRAY(EDIT$(SB_SUBACCOUNT::CLASS, -1%), &
			WLDCRD$) = 0%

		IF OLD_CLASS$ <> SB_SUBACCOUNT::CLASS OR RRECORD% = -1%
		THEN
			GOSUB SubTotal IF RRECORD% = 0%
			OLD_CLASS$ = SB_SUBACCOUNT::CLASS
			RRECORD% = 0%
		END IF

	END SELECT

	TEST_JOB% = 0%

 !	LAST_OPER$ = STRING$(LEN(SB_BALANCE::OPERATION), 0%)
 !	LAST_ACCOUNT$ = STRING$(LEN(SB_BALANCE::ACCOUNT), 0%)
	LAST_OPER$ = STRING$(LEN(GL_YYYY_PP::OPERATION), 0%)
	LAST_ACCOUNT$ = STRING$(LEN(GL_YYYY_PP::ACCT), 0%)

17030 !	WHEN ERROR IN
 !		FIND #SB_BALANCE.CH%, &
 !			KEY #0% EQ DEF_SYSTEM$ + SB_SUBACCOUNT::SUBACCOUNT, &
 !			REGARDLESS
 !	USE
 !		CONTINUE GetNextRec IF ERR = 155%
 !		FILENAME$ = "SB_BALANCE"
 !		CONTINUE HelpError
 !	END WHEN

 GetNextRec2:
17050 !	WHEN ERROR IN
 !		GET #SB_BALANCE.CH%, REGARDLESS
 !	USE
 !		IF ERR = 11%
 !		THEN
 !			SB_BALANCE::SYSTEM = '0'C
 !			CONTINUE 17052
 !		END IF
 !		FILENAME$ = "SB_BALANCE"
 !		CONTINUE HelpError
 !	END WHEN
 !
 !	GOTO 17050 IF SB_BALANCE::PERIOD <> YYYYPP$

17052	NEXT_OPER$ = STRING$(LEN(GL_YYYY_PP::OPERATION), 255%)
	NEXT_ACCOUNT$ = STRING$(LEN(GL_YYYY_PP::ACCT), 255%)
	ALL_EOF% = -1%

	FOR LOOP% = 1% TO GL_FILE%

		WHEN ERROR IN
			GET #GL_DATA.CH%(LOOP%), &
				KEY #1% GT SB_SUBACCOUNT::SUBACCOUNT + &
				LAST_OPER$ + LAST_ACCOUNT$, &
				REGARDLESS
		USE
			CONTINUE 17053
		END WHEN

		IF (GL_YYYY_PP::SUBACC = SB_SUBACCOUNT::SUBACCOUNT) AND &
			(GL_YYYY_PP::ACCT + GL_YYYY_PP::OPERATION < &
			NEXT_ACCOUNT$ + NEXT_OPER$)
		THEN
			ALL_EOF% = 0%
			NEXT_ACCOUNT$ = GL_YYYY_PP::ACCT
			NEXT_OPER$ = GL_YYYY_PP::OPERATION
		END IF

17053	NEXT LOOP%

17054	IF ALL_EOF% = 0%
	THEN
		GOSUB DoThisitem

		LAST_OPER$ = NEXT_OPER$
		LAST_ACCOUNT$ = NEXT_ACCOUNT$

		GOTO 17052
	ELSE
		GOTO PrintJobTot
	END IF

	%PAGE

	!*******************************************************************
	! Print out information for this group
	!*******************************************************************

 DoThisItem:
17060	RETURN IF COMP_ARRAY(NEXT_ACCOUNT$, SB_ACCOUNT_LIST$) = 0%

17080	CCHANGE = 0.0
	GLFLAG% = 0%

	FOR LOOP% = 1% TO GL_FILE%
		WHEN ERROR IN
			FIND #GL_DATA.CH%(LOOP%), &
				KEY #1% EQ SB_SUBACCOUNT::SUBACCOUNT + &
				NEXT_OPER$ + NEXT_ACCOUNT$, &
				REGARDLESS
		USE
			CONTINUE 17200
		END WHEN

17090		WHEN ERROR IN
			GET #GL_DATA.CH%(LOOP%), REGARDLESS
		USE
			CONTINUE 17200
		END WHEN

		GOTO 17200 IF SB_SUBACCOUNT::SUBACCOUNT <> GL_YYYY_PP::SUBACC OR &
			NEXT_OPER$ <> GL_YYYY_PP::OPERATION OR &
			NEXT_ACCOUNT$ <> GL_YYYY_PP::ACCT

		CCHANGE = FUNC_ROUND(CCHANGE + GL_YYYY_PP::AMOUNT, 2%)
		GLFLAG% = -1%

		GOTO 17090

17200	NEXT LOOP%

 !	JT_BEG_BAL = JT_BEG_BAL + SB_BALANCE_USE::BEG_AMOUNT
	JT_CUR_BAL = JT_CUR_BAL + CCHANGE
 !	JT_END_BAL = JT_END_BAL + SB_BALANCE_USE::BEG_AMOUNT + CCHANGE
	JT_END_BAL = JT_END_BAL + CCHANGE

	RETURN IF (ABS(CCHANGE) = 0.0)

	V% = GL_EXAM_CHART(NEXT_ACCOUNT$, GL_CHART_EXAM)

	!
	! Print out one line
	!
	TEXT$ = SB_SUBACCOUNT::SUBACCOUNT + " " + &
		LEFT(SB_SUBACCOUNT::DESCR, 20%) + " " + &
		SB_SUBACCOUNT::TTYPE + " " + &
		SB_SUBACCOUNT::CLASS + " " + &
		NEXT_OPER$ + "  " + &
		NEXT_ACCOUNT$ + " "  + &
		LEFT(GL_CHART_EXAM::DESCR, 15%) + "  " + &
		FORMAT$(0.0, "###,###,###.##") + "  " + &
		FORMAT$(CCHANGE, "###,###,###.##") + "  " + &
		FORMAT$(CCHANGE, "###,###,###.##")

	GOTO ExitProgram IF GL_OUTP_ACCTSUM(OPT_ADDREC, &
		NEXT_ACCOUNT$, &
		0.0, CCHANGE, 0.0, TITLE$(), &
		UTL_REPORTX) <> CMC$_NORMAL

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEST_JOB% = -1%

	SB_SUBACCOUNT::DESCR = STRING$(LEN(SB_SUBACCOUNT::DESCR), A"."B)
	SB_SUBACCOUNT::TTYPE = STRING$(LEN(SB_SUBACCOUNT::TTYPE), A"."B)
	SB_SUBACCOUNT::CLASS = STRING$(LEN(SB_SUBACCOUNT::CLASS), A"."B)

17210	GOTO ExitRec3 IF DET_TOT$ <> "D"

	FOR LOOP% = 1% TO GL_FILE%
		WHEN ERROR IN
			FIND #GL_DATA.CH%(LOOP%), &
				KEY #1% EQ SB_SUBACCOUNT::SUBACCOUNT + &
				NEXT_OPER$ + NEXT_ACCOUNT$, &
				REGARDLESS
		USE
			CONTINUE ExitRec3a IF ERR = 155% OR ERR = 9%
			FILENAME$ = "GL_YYYY_PP"
			CONTINUE HelpError
		END WHEN

 GetNextRec3:
17220		WHEN ERROR IN
			GET #GL_DATA.CH%(LOOP%), REGARDLESS
		USE
			CONTINUE ExitRec3a IF ERR = 11%
			FILENAME$ = "GL_YYYY_PP"
			CONTINUE HelpError
		END WHEN

		GOTO ExitRec3a IF SB_SUBACCOUNT::SUBACCOUNT <> GL_YYYY_PP::SUBACC OR &
			NEXT_OPER$ <> GL_YYYY_PP::OPERATION OR &
			NEXT_ACCOUNT$ <> GL_YYYY_PP::ACCT

		TEXT$ = SB_SUBACCOUNT::SUBACCOUNT + SPACE$(6%) + &
			GL_YYYY_PP::REFNO + " " + &
			GL_YYYY_PP::SOURCE + "   " + &
			PRNT_DATE(GL_YYYY_PP::TRANDAT, 8%) + " " + &
			LEFT(GL_YYYY_PP::DESCR, 22%) + " " + &
			GL_YYYY_PP::XREFNO + SPACE$(10%) + &
			FORMAT$(GL_YYYY_PP::AMOUNT, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		!
		! Try for next record
		!
		GOTO GetNextRec3

 ExitRec3a:
	NEXT LOOP%

 ExitRec3:
	!
	! Try for next record
	!
	RETURN

 PrintJobTot:
	IF TEST_JOB%
	THEN
		TEXT$ = SB_SUBACCOUNT::SUBACCOUNT + " "  + &
			"JOB TOTALS " + STRING$(63%, A"."B) + &
			FORMAT$(JT_BEG_BAL, " ###,###,###.##") + "  " + &
			FORMAT$(JT_CUR_BAL, "###,###,###.##") + "  " + &
			FORMAT$(JT_END_BAL, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		ST_BEG_BAL = ST_BEG_BAL + JT_BEG_BAL
		ST_CUR_BAL = ST_CUR_BAL + JT_CUR_BAL
		ST_END_BAL = ST_END_BAL + JT_END_BAL

	END IF

	JT_BEG_BAL, JT_CUR_BAL, JT_END_BAL = 0.0

	GOTO GetNextRec

 ExitTotal:
17400	!
	! Handle end of report
	!
	SELECT SORT_BY$

	CASE "J"
		GT_BEG_BAL = GT_BEG_BAL + ST_BEG_BAL
		GT_CUR_BAL = GT_CUR_BAL + ST_CUR_BAL
		GT_END_BAL = GT_END_BAL + ST_END_BAL

	CASE "T", "C"
		GOSUB SubTotal
	END SELECT

	TEXT$ = "GRAND TOTALS" + SPACE$(74%) + &
		FORMAT$(GT_BEG_BAL, "###,###,###.##") + "  " + &
		FORMAT$(GT_CUR_BAL, "###,###,###.##") + "  " + &
		FORMAT$(GT_END_BAL, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
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

	GT_BEG_BAL = GT_BEG_BAL + ST_BEG_BAL
	GT_CUR_BAL = GT_CUR_BAL + ST_CUR_BAL
	GT_END_BAL = GT_END_BAL + ST_END_BAL

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
