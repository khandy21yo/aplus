1	%TITLE "Summary Period Ledger List"
	%SBTTL "EL_RPRT_SUMPERLED"
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
	! ID:EL0005
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program
	!	prints the ^*Summary Period Ledger list\*.
	!	The following fields are included in this list:
	!	.table 3,25
	!	.te
	!	Equipment Number	Equipment Description
	!	.te
	!	Reference Number	Equipment Type
	!	.te
	!	Equipment Class	Source
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
	!
	! Compile:
	!
	!	$ BAS EL_SOURCE:EL_RPRT_SUMPERLED/LINE
	!	$ LINK/EXE=EL_EXE: EL_RPRT_SUMPERLED, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE EL_RPRT_SUMPERLED.OBJ;*
	!
	! Author:
	!
	!	10/15/92 - Dan Perkins
	!
	! Modification History:
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/18/93 - Kevin Handy
	!		Added parameter to GL_OUTP_SUMPERLID for units.
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/13/95 - Kevin Handy
	!		Reformat closer to 80 columns.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/27/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
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

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)		GL_PERIOD_CDD		GL_PERIOD

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	DECLARE			GL_CHART_CDD		GL_CHART_EXAM

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP)	GL_YYYY_PP_CDD		GL_YYYY_PP

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
	!	The ^*Sort by\*
	!	field causes the report to print in
	!	a selected order. The following values are valid:
	!	.table 3,25
	!	.te
	!	^*E\* - Equipment Number
	!	.te
	!	^*C\* - Equipment Class
	!	.te
	!	^*T\* - Equipment Type
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
	!	The ^*From Item\* entered in this field
	!	causes the printing
	!	to begin with a selected item.
	!	.b
	!	A blank field causes the report to begin
	!	with the first item in the file.
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
	!	The ^*To Item\* entered in this field
	!	causes the report to end printing
	!	with a selected item.
	!	.b
	!	A blank field causes the report to end
	!	with the last item in the file.
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
	!	The ^*Wildcard\* field enables the user to print a
	!	report including selected items only
	!	using the "wildcarding" technique.
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
	!	The ^*Status\* field enters the status of
	!	the equipment at the current time.
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
	!	The ^*Detail or Total\* field
	!	prints the report containing
	!	only the totals found in the Equipment Ledger
	!	system by entering a ^*T\*.
	!	By entering a ^*D\*, detail from the
	!	General Ledger is read and the totals
	!	are compared with the totals from
	!	the Equipment Ledger.  If the totals do not agree,
	!	the ^*Resync\* program must be run.
	!	.lm -5
	!
	! Index:
	!	.x Detail or Total
	!
	!--

	DEF_SYSTEM$ = "EL"
	DEF_SUBJECT$ = "E"

	SELECT SORT_BY$

	CASE "E"
		SORT_KEY% = 0%
		ADD_TITLE$ = "BY  EQUIPMENT  NUMBER"

	CASE "T"
		SORT_KEY% = 1%
		ADD_TITLE$ = "BY  EQUIPMENT  TYPE"

	CASE "C"
		SORT_KEY% = 2%
		ADD_TITLE$ = "BY  EQUIPMENT  CLASS"

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
	USE
		FILENAME$ = "SB_CONTROL"
		CONTINUE HelpError
	END WHEN

	YYYYPP$  = SB_CONTROL::PERIOD
	YYYY_PP$ = LEFT(YYYYPP$, 4%) + "_" + RIGHT(YYYYPP$, 5%)

	CLOSE SB_CONTROL.CH%, GL_PERIOD.CH%

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.OPN"
	USE
		FILENAME$ = "SB_BALANCE"
		CONTINUE HelpError
	END WHEN

370	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "GL_YYYY_PP"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "SUMMARY  PERIOD  LEDGER  REPORT  " + ADD_TITLE$
	TITLE$(2%) = "Equipment Ledger System"
	TITLE$(3%) = "Period " + YYYYPP$
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "Equip#     Description          Ty Clas "        + &
		"Operation Account#           Description       "  + &
		"BegBalance      DbPer      CrPer  EndBalance"

	IF DET_TOT$ = "D"
	THEN
		TITLE$(6%) = "                Reference#       " + &
			"Source Date       Description            XRef#"

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
			KEY #SORT_KEY% GE "E" + FROM_ITEM$, &
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

	GOTO GetNextRec IF STAT_WC$ <> "" AND COMP_STRING(EDIT$( &
		SB_SUBACCOUNT::SSTATUS, -1%), STAT_WC$) = 0%

	SELECT SORT_BY$

	CASE "E"
		GOTO ExitTotal IF (SB_SUBACCOUNT::SUBACCOUNT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND COMP_STRING(EDIT$( &
			SB_SUBACCOUNT::SUBACCOUNT, -1%), WLDCRD$) = 0%

	CASE "T"
		GOTO ExitTotal IF (SB_SUBACCOUNT::TTYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND COMP_STRING(EDIT$( &
			SB_SUBACCOUNT::TTYPE, -1%), WLDCRD$) = 0%

		IF OLD_TYPE$ <> SB_SUBACCOUNT::TTYPE OR RRECORD% = -1%
		THEN
			GOSUB SubTotal IF RRECORD% = 0%
			OLD_TYPE$ = SB_SUBACCOUNT::TTYPE
			RRECORD% = 0%
		END IF

	CASE "C"
		GOTO ExitTotal IF (SB_SUBACCOUNT::CLASS > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND COMP_STRING(EDIT$( &
			SB_SUBACCOUNT::CLASS, -1%), WLDCRD$) = 0%

		IF OLD_CLASS$ <> SB_SUBACCOUNT::CLASS OR RRECORD% = -1%
		THEN
			GOSUB SubTotal IF RRECORD% = 0%
			OLD_CLASS$ = SB_SUBACCOUNT::CLASS
			RRECORD% = 0%
		END IF

	END SELECT

17030	WHEN ERROR IN
		FIND #SB_BALANCE.CH%, &
			KEY #0% EQ DEF_SYSTEM$ + &
			SB_SUBACCOUNT::SUBACCOUNT, &
			REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155%
		FILENAME$ = "SB_BALANCE"
		CONTINUE HelpError
	END WHEN

 GetNextRec2:
17050	WHEN ERROR IN
		GET #SB_BALANCE.CH%, REGARDLESS
	USE
		CONTINUE 17350 IF ERR = 11%
		FILENAME$ = "SB_BALANCE"
		CONTINUE HelpError
	END WHEN

	GOTO 17350 IF SB_BALANCE::SYSTEM <> DEF_SYSTEM$ OR &
		SB_BALANCE::SUBACCOUNT <> SB_SUBACCOUNT::SUBACCOUNT

	GOTO 17050 IF SB_BALANCE::PERIOD <> YYYYPP$

	LOOP% = 1%
	GOTO 17210

17200	JT_BEG_BAL = JT_BEG_BAL + FUNC_ROUND(SB_BALANCE::BEG_AMOUNT, 2%)
	JT_CUR_DBAL = JT_CUR_DBAL + DCHANGE
	JT_CUR_CBAL = JT_CUR_CBAL + CCHANGE
	JT_END_BAL = JT_END_BAL + FUNC_ROUND(SB_BALANCE::BEG_AMOUNT, 2%) + &
		DCHANGE + CCHANGE

	GOTO GetNextRec2 &
		IF ABS(FUNC_ROUND(SB_BALANCE::BEG_AMOUNT, 2%)) + ABS(CCHANGE) + &
		ABS(DCHANGE) = 0.0

	V% = GL_EXAM_CHART(SB_BALANCE::ACCOUNT, GL_CHART_EXAM)

	!
	! Print out one line
	!
	TEXT$ = SB_SUBACCOUNT::SUBACCOUNT + " "  + &
		LEFT(SB_SUBACCOUNT::DESCR, 20%) + " "  + &
		SB_SUBACCOUNT::TTYPE + " "  + &
		SB_SUBACCOUNT::CLASS + " "  + &
		SB_BALANCE::OPERATION + "  " + &
		SB_BALANCE::ACCOUNT + " "  + &
		LEFT(GL_CHART_EXAM::DESCR, 15%) + " " + &
		FORMAT$(SB_BALANCE::BEG_AMOUNT, "#,###,###.##") + " " + &
		FORMAT$(DCHANGE, "###,###.##") + " " + &
		FORMAT$(-CCHANGE, "###,###.##") + &
		FORMAT$(SB_BALANCE::BEG_AMOUNT + DCHANGE + CCHANGE, &
		"#,###,###.##")

	GOTO ExitProgram &
		IF GL_OUTP_ACCTSUM (OPT_ADDREC, SB_BALANCE::ACCOUNT, &
		SB_BALANCE::BEG_AMOUNT, CCHANGE, 0.0, &
		TITLE$(), UTL_REPORTX) <> CMC$_NORMAL

	GOTO ExitProgram IF GL_OUTP_ACCTSUM (OPT_ADDREC, SB_BALANCE::ACCOUNT, &
		0.0, DCHANGE, 0.0, TITLE$(), UTL_REPORTX) <> CMC$_NORMAL

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	SB_SUBACCOUNT::DESCR = &
		STRING$(LEN(SB_SUBACCOUNT::DESCR), A"."B)

	SB_SUBACCOUNT::TTYPE = &
		STRING$(LEN(SB_SUBACCOUNT::TTYPE), A"."B)

	SB_SUBACCOUNT::CLASS = &
		STRING$(LEN(SB_SUBACCOUNT::CLASS), A"."B)

17210	DCHANGE, CCHANGE = 0.0

	WHEN ERROR IN
		FIND #GL_YYYY_PP.CH%, &
			KEY #1% EQ SB_SUBACCOUNT::SUBACCOUNT + &
			SB_BALANCE::OPERATION + SB_BALANCE::ACCOUNT, &
			REGARDLESS
	USE
		CONTINUE 17245 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "GL_YYYY_PP"
		CONTINUE HelpError
	END WHEN

 GetNextRec3:
17220	WHEN ERROR IN
		GET #GL_YYYY_PP.CH%, REGARDLESS
	USE
		CONTINUE 17245 IF ERR = 11%
		FILENAME$ = "GL_YYYY_PP"
		CONTINUE HelpError
	END WHEN

	GOTO 17245 IF SB_SUBACCOUNT::SUBACCOUNT <> GL_YYYY_PP::SUBACC OR &
		SB_BALANCE::OPERATION <> GL_YYYY_PP::OPERATION OR &
		SB_BALANCE::ACCOUNT <> GL_YYYY_PP::ACCT

	IF GL_YYYY_PP::AMOUNT > 0.0
	THEN
		DCHANGE = FUNC_ROUND( DCHANGE + GL_YYYY_PP::AMOUNT, 2%)
	ELSE
		CCHANGE = FUNC_ROUND( CCHANGE + GL_YYYY_PP::AMOUNT, 2%)
	END IF

	IF DET_TOT$ = "D" AND LOOP% = 2%
	THEN
		TEXT$ = SB_SUBACCOUNT::SUBACCOUNT + SPACE$(6%) + &
			GL_YYYY_PP::REFNO + " " + &
			GL_YYYY_PP::SOURCE + "   " + &
			PRNT_DATE(GL_YYYY_PP::TRANDAT, 8%) + " " + &
			LEFT(GL_YYYY_PP::DESCR, 22%) + " " + &
			GL_YYYY_PP::XREFNO + SPACE$(12%) + &
			FORMAT$(GL_YYYY_PP::AMOUNT, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	!
	! Try for next record
	!
	GOTO GetNextRec3

17245	IF LOOP% = 1%
	THEN
		LOOP% = 2%
		GOTO 17200
	ELSE
		!
		! Try for next record
		!
		GOTO GetNextRec2
	END IF

17350	!
	! Try for next record
	!
	IF ABS(JT_BEG_BAL) + ABS(JT_CUR_DBAL) + ABS(JT_CUR_CBAL) <> 0.0
	THEN
		TEXT$ = SB_SUBACCOUNT::SUBACCOUNT + "  " + &
			"EQUIPMENT TOTALS " + STRING$(55%, A"."B) + &
			FORMAT$(JT_BEG_BAL, " #,###,###.##") + " " + &
			FORMAT$(JT_CUR_DBAL, "###,###.##") + " " + &
			FORMAT$(-JT_CUR_CBAL, "###,###.##") + &
			FORMAT$(JT_END_BAL, "#,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		ST_BEG_BAL = ST_BEG_BAL + JT_BEG_BAL
		ST_CUR_DBAL = ST_CUR_DBAL + JT_CUR_DBAL
		ST_CUR_CBAL = ST_CUR_CBAL + JT_CUR_CBAL
		ST_END_BAL = ST_END_BAL + JT_END_BAL

	END IF

	JT_BEG_BAL, JT_CUR_DBAL, JT_CUR_CBAL, JT_END_BAL = 0.0

	GOTO GetNextRec

 ExitTotal:
17400	!
	! Handle end of report
	!
	SELECT SORT_BY$

	CASE "E"
		GT_BEG_BAL = GT_BEG_BAL + ST_BEG_BAL
		GT_CUR_DBAL = GT_CUR_DBAL + ST_CUR_DBAL
		GT_CUR_CBAL = GT_CUR_CBAL + ST_CUR_CBAL
		GT_END_BAL = GT_END_BAL + ST_END_BAL

	CASE "T", "C"
		GOSUB SubTotal

	END SELECT

	TEXT$ = "GRAND TOTALS" + SPACE$(73%) + &
		FORMAT$(GT_BEG_BAL, "#,###,###.##") + " " + &
		FORMAT$(GT_CUR_DBAL, "###,###.##") + " " + &
		FORMAT$(-GT_CUR_CBAL, "###,###.##") + &
		FORMAT$(GT_END_BAL, "#,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	V% = GL_OUTP_ACCTSUM (OPT_SUMMARY + SUBOPT_DETAIL, "", &
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
		FORMAT$(ST_BEG_BAL, "#,###,###.##") + " " + &
		FORMAT$(ST_CUR_DBAL, "###,###.##") + " " + &
		FORMAT$(-ST_CUR_CBAL, "###,###.##") + &
		FORMAT$(ST_END_BAL, "#,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GT_BEG_BAL = GT_BEG_BAL + ST_BEG_BAL
	GT_CUR_DBAL = GT_CUR_DBAL + ST_CUR_DBAL
	GT_CUR_CBAL = GT_CUR_CBAL + ST_CUR_CBAL
	GT_END_BAL = GT_END_BAL + ST_END_BAL

	ST_BEG_BAL, ST_CUR_DBAL, ST_CUR_CBAL, ST_END_BAL = 0.0

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
