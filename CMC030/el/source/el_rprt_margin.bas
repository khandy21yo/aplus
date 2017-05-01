1	%TITLE "Gross Margin Report"
	%SBTTL "EL_RPRT_MARGIN"
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
	! ID:EL0004
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Gross Margin Report\* option prints
	!	a report listing all Gross Margins for the period. Included in this
	!	report are the following fields:
	!	.table
	!	.te
	!	Equipment Number
	!	.te
	!	Description
	!	.te
	!	Equipment Type
	!	.te
	!	Equipment Class
	!	.te
	!	Begin Date
	!	.te
	!	Revenue
	!	.te
	!	Labor
	!	.te
	!	Parts
	!	.te
	!	Raw Material
	!	.te
	!	Gross Margin
	!	.te
	!	Margin Percentage
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Gross Margin Report
	!	.x Report>Gross Margin
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS EL_SOURCE:EL_RPRT_MARGIN/LINE
	!	$ LINK/EXE=EL_EXE: EL_RPRT_MARGIN, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE EL_RPRT_MARGIN.OBJ;*
	!
	! Author:
	!
	!	10/14/92 - Dan Perkins
	!
	! Modification History:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/15/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		Change RIGHT(NUM1$()) for FORMAT$().
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!		Lose unecessary function definitions
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/27/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Add some REGARDLESS
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

	%INCLUDE "SOURCE:[EL.OPEN]EL_EQUIPMENT.HB"
	MAP (SB_SUBACCOUNT)	EL_EQUIPMENT_CDD	EL_EQUIPMENT

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.HB"
	MAP (SB_CONTROL)	SB_CONTROL_CDD		SB_CONTROL

	%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.HB"
	MAP (SB_BALANCE)	SB_BALANCE_CDD		SB_BALANCE

	%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.HB"
	MAP (SB_ACCOUNT)	SB_ACCOUNT_CDD		SB_ACCOUNT

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)		GL_PERIOD_CDD		GL_PERIOD

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
	!	The ^*Sort by\* field causes the report to print
	!	in a selected order. The following values are valid:
	!	.table 3,25
	!	.te
	!	^*E\* - Equipment Number
	!	.te
	!	^*C\* - Equipment Class
	!	.te
	!	^*T\* - Equipment Type
	!	.end table
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
	!	The ^*From Item\* entered in this field causes the printing
	!	to begin with the selected item.  The
	!	value entered must be in agreement with
	!	field (01) Sort by.
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
	!	with the selected item.  The value entered must be in agreement
	!	with field (01) Sort by.
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
	!	The ^*Wildcard\* field enables the user to print a report
	!	including selected items only using the "wildcarding" technique.
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
	!	.table 3,25
	!	.te
	!	^*A\* - Active
	!	.te
	!	^*I\* - Inactive
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Status
	!
	!--

	DEF_SYSTEM$ = "EL"
	DEF_SUBJECT$ = "E"

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

340	!
	! Figure out the current period
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	IF SB_CONTROL::PERIOD = ""
	THEN
		YEAR$ = LEFT(SB_CONTROL::CDATE, 4%)
		YEAR$ = GL_PERIOD::YEAR	IF YEAR$ = ""
		YEAR$ = LEFT(DATE_TODAY, 4%) IF YEAR$ = ""

		CUR_PERIOD% = 1%
	ELSE
		CUR_PERIOD% = VAL%(RIGHT(SB_CONTROL::PERIOD, 5%))
		YEAR$ = LEFT(SB_CONTROL::PERIOD, 4%)

		IF CUR_PERIOD% > GL_PERIOD::FPFY
		THEN
			CUR_PERIOD% = 1%
			YEAR$ = FORMAT$(VAL%(YEAR$) + 1%, "<0>###")
		END IF
	END IF

	CLOSE SB_CONTROL.CH%, GL_PERIOD.CH%

	YYYYPP$ = YEAR$ + FORMAT$(CUR_PERIOD%, "<0>#")

 ReportTitle:
	!
	! Title
	!
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

	!
	! Heading
	!
	TITLE$(1%) = "MARGIN  REPORT  " + ADD_TITLE$
	TITLE$(2%) = "Equipment Ledger System"
	TITLE$(3%) = ""

	TITLE$(4%) = "Equip#     Description          Type "       + &
		"Class BeginDate       Revenue        Labor " + &
		"       Parts  RawMaterial  GrossMargin Percent"

	TITLE$(5%) = "."

	%PAGE

	!
	! Initialize variables
	!
	REVENUE, LABOR, PARTS, RAW, DIF = 0.0
	ST_REVENUE, ST_LABOR, ST_PARTS, ST_RAW, ST_DIF = 0.0
	GT_REVENUE, GT_LABOR, GT_PARTS, GT_RAW, GT_DIF = 0.0

	PRINTED_FLAG% = 0%

	OLD_EQUIPMENTTYPE$, OLD_CLASS$ = ""

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			FIND #SB_SUBACCOUNT.CH%, &
				KEY #SORT_KEY% GE "E", &
				REGARDLESS
		ELSE
			FIND #SB_SUBACCOUNT.CH%, &
				KEY #SORT_KEY% GE "E" + FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

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

	GOTO ExitTotal IF EL_EQUIPMENT::SUBJECT <> DEF_SUBJECT$

	GOTO GetNextRec IF STAT_WC$ <> "" AND COMP_STRING(EDIT$( &
		EL_EQUIPMENT::SSTATUS, -1%), STAT_WC$) = 0%

	SELECT SORT_BY$

	CASE "E"
		GOTO ExitTotal IF (EL_EQUIPMENT::EQNUM > &
			TO_ITEM$) AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$( &
			EL_EQUIPMENT::EQNUM, -1%), WLDCRD$) = 0%

	CASE "T"
		GOTO ExitTotal IF (EL_EQUIPMENT::TTYPE > &
			TO_ITEM$) AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$( &
			EL_EQUIPMENT::TTYPE, -1%), WLDCRD$) = 0%

		IF OLD_EQUIPMENTTYPE$ <> EL_EQUIPMENT::TTYPE
		THEN
			GOSUB SubTotal IF PRINTED_FLAG%
			OLD_EQUIPMENTTYPE$ = EL_EQUIPMENT::TTYPE
			PRINTED_FLAG% = 0%
		END IF

	CASE "C"
		GOTO ExitTotal IF (EL_EQUIPMENT::CLASS > &
			TO_ITEM$) AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$( &
			EL_EQUIPMENT::CLASS, -1%), WLDCRD$) = 0%

		IF OLD_CLASS$ <> EL_EQUIPMENT::CLASS
		THEN
			GOSUB SubTotal IF PRINTED_FLAG%
			OLD_CLASS$ = EL_EQUIPMENT::CLASS
			PRINTED_FLAG% = 0%
		END IF

	END SELECT

17100	WHEN ERROR IN
		FIND #SB_BALANCE.CH%, &
			KEY #1% EQ YYYYPP$ + DEF_SYSTEM$ + &
			EL_EQUIPMENT::EQNUM, &
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
		CONTINUE PrintLine IF ERR = 11%
		FILENAME$ = "SB_BALANCE"
		CONTINUE HelpError
	END WHEN

	GOTO PrintLine IF SB_BALANCE::PERIOD <> YYYYPP$ OR &
		SB_BALANCE::SYSTEM <> DEF_SYSTEM$ OR &
		SB_BALANCE::SUBACCOUNT <> EL_EQUIPMENT::EQNUM

	WHEN ERROR IN
		RESET #SB_ACCOUNT.CH%
	USE
		FILENAME$ = "SB_ACCOUNT"
		CONTINUE HelpError
	END WHEN

 GetAccountRec:
17200	WHEN ERROR IN
		GET #SB_ACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE GetNextRec2 IF ERR = 11%
		FILENAME$ = "SB_ACCOUNT"
		CONTINUE HelpError
	END WHEN

	IF COMP_STRING(SB_BALANCE::ACCOUNT, SB_ACCOUNT::ACCOUNT)
	THEN
		SELECT SB_ACCOUNT::ACCTGROUP

		CASE "BILL"
			REVENUE = REVENUE - SB_BALANCE::AMOUNT - &
				SB_BALANCE::BEG_AMOUNT

		CASE "DLAB", "ILAB"
			LABOR = LABOR + SB_BALANCE::AMOUNT + &
				SB_BALANCE::BEG_AMOUNT

		CASE "PMAT"
			PARTS = PARTS + SB_BALANCE::AMOUNT + &
				SB_BALANCE::BEG_AMOUNT

		CASE "RMAT"
			RAW = RAW + SB_BALANCE::AMOUNT + &
				SB_BALANCE::BEG_AMOUNT

		END SELECT

		GOTO GetNextRec2
	END IF

	!
	! Try for next record
	!
	GOTO GetAccountRec

 PrintLine:
17300	!
	! Print out one line
	!
	DIF = REVENUE - (LABOR + PARTS + RAW)

	IF REVENUE = 0.0
	THEN
		PERCENT = 0.0
	ELSE
		PERCENT = FUNC_ROUND((DIF / REVENUE) * 100.0, 2%)
	END IF

	TEXT$ = EL_EQUIPMENT::EQNUM + " " + &
		LEFT(EL_EQUIPMENT::DESCR, 20%) + " " + &
		EL_EQUIPMENT::TTYPE + "   " + &
		EL_EQUIPMENT::CLASS + "  " + &
		PRNT_DATE(EL_EQUIPMENT::BDATE, 8%) + " " + &
		FORMAT$(REVENUE, "#,###,###.##") + " " + &
		FORMAT$(LABOR, "#,###,###.##") + " " + &
		FORMAT$(PARTS, "#,###,###.##") + " " + &
		FORMAT$(RAW, "#,###,###.##") + " " + &
		FORMAT$(DIF, "#,###,###.##") + "  "  + &
		FORMAT$(PERCENT, "###.##%")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	ST_REVENUE = ST_REVENUE + REVENUE
	ST_LABOR   = ST_LABOR   + LABOR
	ST_PARTS   = ST_PARTS   + PARTS
	ST_RAW     = ST_RAW     + RAW

	GT_REVENUE = GT_REVENUE + REVENUE
	GT_LABOR   = GT_LABOR   + LABOR
	GT_PARTS   = GT_PARTS   + PARTS
	GT_RAW     = GT_RAW     + RAW

	REVENUE, LABOR, PARTS, RAW, DIF = 0.0

	PRINTED_FLAG% = -1%

	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!
	SELECT	SORT_BY$

	CASE "T", "C"
		GOSUB SubTotal IF PRINTED_FLAG%

	END SELECT

	GT_DIF = GT_REVENUE - (GT_LABOR + GT_PARTS + GT_RAW)

	IF GT_REVENUE = 0.0
	THEN
		GT_PERCENT = 0.0
	ELSE
		GT_PERCENT = FUNC_ROUND((GT_DIF / GT_REVENUE) * 100.0, 2%)
	END IF

	TEXT$ = "           GRAND TOTALS" + SPACE$(31%) + &
		FORMAT$(GT_REVENUE, "#,###,###.##") + " "  + &
		FORMAT$(GT_LABOR, "#,###,###.##") + " "  + &
		FORMAT$(GT_PARTS, "#,###,###.##") + " "  + &
		FORMAT$(GT_RAW, "#,###,###.##") + " "  + &
		FORMAT$(GT_DIF, "#,###,###.##") + "  " + &
		FORMAT$(GT_PERCENT, "###.##%")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 2%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

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
	SELECT	SORT_BY$

	CASE "T"
		TEXT$ = "TYPE  TOTALS"

	CASE "C"
		TEXT$ = "CLASS TOTALS"

	END SELECT

	ST_DIF = ST_REVENUE - (ST_LABOR + ST_PARTS + ST_RAW)

	IF ST_REVENUE = 0.0
	THEN
		ST_PERCENT = 0.0
	ELSE
		ST_PERCENT = FUNC_ROUND((ST_DIF / ST_REVENUE) * 100.0, 2%)
	END IF

	TEXT$ = "           " + TEXT$ + SPACE$(31%) + &
		FORMAT$(ST_REVENUE, "#,###,###.##") + " " + &
		FORMAT$(ST_LABOR, "#,###,###.##") + " " + &
		FORMAT$(ST_PARTS, "#,###,###.##") + " " + &
		FORMAT$(ST_RAW, "#,###,###.##") + " " + &
		FORMAT$(ST_DIF, "#,###,###.##") + "  " + &
		FORMAT$(ST_PERCENT, "###.##%")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 2%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	ST_REVENUE, ST_LABOR, ST_PARTS, ST_RAW = 0.0

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
