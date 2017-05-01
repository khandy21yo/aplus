1	%TITLE "Accounts Receivable Aging Report"
	%SBTTL "AR_RPRT_CUSACCT_AGE"
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
	! ID:AR042
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Accounts Receivable Aging Report\* option
	!	prints an Accounts Receivable aging report. Using the report settings, a
	!	base is selected from which aging is calculated. The report may
	!	also be set to print accounts in order of location, customer number,
	!	or customer name.
	!	.b
	!	^*Note:\*  If items are not aging as you think they should, the most
	!	common problems are the cutoff date in the register
	!	and the open item/balance forward in the customer file (in balance forward,
	!	anything in the register is current, regardless of the date).
	!	.lm -5
	!
	! Index:
	!	.x Aging Report>Print
	!	.x Report>Aging
	!
	! Option:
	!
	!
	! Input:
	!
	!
	! Output:
	!
	!
	! Example:
	!
	! Author:
	!
	!	07/19/91 - Craig Tanner
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_RPRT_CUSACCT_AGE/LINE
	!	$ LINK/EXECUTABLE=AR_EXE:*.EXE AR_RPRT_CUSACCT_AGE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_RPRT_CUSACCT_AGE.OBJ;*
	!
	! Modification history:
	!
	!	06/14/93 - Kevin Handy
	!		Added REGARDLESS and CLOSE to AR_CONTROL.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/11/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/26/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.HB"
	MAP (AR_CUSBAL)		AR_CUSBAL_CDD		AR_CUSBAL
	DIM			AR_CUSBAL_CDD		ARRAY_CUSBAL(50%)
	DIM			AR_CUSBAL_CDD		TOTAL_CUSBAL(1000%)
	DECLARE			AR_CUSBAL_CDD		GRAND_CUSBAL

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD		AR_CONTROL

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN)		AR_OPEN_CDD		AR_OPEN

	COM (CH_AR_35CUSTOM) AR_35CUSTOM.CH%
	COM (CH_AR_CUSBAL) AR_CUSBAL.CH%
	COM (CH_AR_OPEN)   AR_OPEN.CH%

	!
	! External functions
	!
	EXTERNAL LONG		FUNCTION AR_FUNC_AGE

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes the printing
	!	to begin with the selected Item _#.
	!	The value must be in agreement with field
	!	(03), Sort by.
	!	.b
	!	A blank field will cause the report to start with the first
	!	Item _# in the file.
	!	.lm -5
	!
	! Index:
	!	.X From>Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item _#\* field causes the printing
	!	to end with the selected Item _#. The
	!	value must be in agreement with field (03), Sort
	!	by.
	!	.b
	!	A blank field will cause the report to end with the last
	!	Item _# in the file.
	!	.lm -5
	!
	! Index:
	!	.x To>Item
	!
	!--

	GL_WILDCARD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) Account Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Account Wildcard\* field
	!	selects designated items to be printed by entering
	!	a "wildcard" value in this field.
	!	.lm -5
	!
	! Index:
	!	.x Account>Wildcard
	!	.x Wildcard>Account
	!
	!--

	ZERO_BALANCE$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) Print zero balance\*
	!	.b
	!	.lm +5
	!	The ^*Print zero balance\* field suppresses
	!	the printing of accounts with a zero balance.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes
	!	.te
	!	^*N\* - No
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Print>Zero
	!	.x Zero>Balance
	!	.x Balance>Zero
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines the order
	!	in which the report will print.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*N\* - Number
	!	.te
	!	^*T\* - Type
	!	.te
	!	^*C\* - Category
	!	.te
	!	^*A\* - Alphabetical
	!	.end table
	!	A setting is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort by
	!
	!--

	AGE_DATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(5%), -1%))

	!++
	! Abstract:FLD06
	!	^*(06) Cutoff Period\*
	!	.b
	!	.lm +5
	!	The ^*Cutoff Period\* will be entered with the date corresponding
	!	to the statement date. No transactions beyond the date indicated
	!	will appear on the report.
	!	.lm -5
	!
	! Index:
	!	.x Cutoff>Period
	!	.x Period>Cutoff
	!
	!--

	CUTOFF$ = EDIT$(UTL_REPORTX::OPTDEF(6%), -1%)

	!++
	! Abstract:FLD07
	!	^*(07) From Period\*
	!	.b
	!	.lm +5
	!	The ^*From Period\* field causes the printing
	!	to include only those customers who have
	!	an account out of balance in the periods listed between the From
	!	and To period.
	!	.b
	!	Valid periods are:
	!	.table 3,25
	!	.te
	!	^*0\* - Current
	!	.te
	!	^*1\* - 31 to 60 days
	!	.te
	!	^*2\* - 61 to 90 days
	!	.te
	!	^*3\* - 91 to 120 days
	!	.te
	!	^*4\* - 121 and over
	!	.end table
	!
	! Index:
	!	.X From>Period
	!
	!--

	FROM_PERIOD% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(7%), 132%))

	!++
	! Abstract:FLD08
	!	^*(08) To Period\*
	!	.b
	!	.lm +5
	!	A ^*To Period\* field causes the printing
	!	to include only those customers who have
	!	an account out of balance in the periods between From and To period.
	!	.b
	!	Valid periods are:
	!	.table 3,25
	!	.te
	!	^*0\* - Current
	!	.te
	!	^*1\* - 31 to 60 days
	!	.te
	!	^*2\* - 61 to 90 days
	!	.te
	!	^*3\* - 91 to 120 days
	!	.te
	!	^*4\* - 121 and over
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.X To>Period
	!
	!--

	TO_PERIOD% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(8%), 132%))

	!++
	! Abstract:FLD09
	!	^*(09) To Period\*
	!	.b
	!	.lm +5
	!	The ^*To Period\* field causes the
	!	printing to include only those
	!	customers who have an account out of balance in the periods
	!	between From and To period.
	!	.b
	!	Valid periods are:
	!	.table 3,25
	!	.te
	!	^*0\* - Current
	!	.te
	!	^*1\* - 31 to 60 days
	!	.te
	!	^*2\* - 61 to 90 days
	!	.te
	!	^*3\* - 91 to 120 days
	!	.te
	!	^*4\* - 121 and over
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.X To>Period
	!
	!--

	TO_PERIOD% = 4% IF TO_PERIOD% = 0%

	GL_WILDCARD$ = "*" IF GL_WILDCARD$ = ""

	SELECT SORTBY$
	CASE "N"
		K_NUM% = 0%
	CASE "T"
		K_NUM% = 1%
	CASE "C"
		K_NUM% = 2%
	CASE "A"
		K_NUM% = 3%
	END SELECT

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

310	!
	! Open control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.OPN"
		GET #AR_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE AR_CONTROL.CH%
	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

320	!
	! Open customer balance file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.OPN"
	USE
		CONTINUE 330
	END WHEN

330	!
	! Open open itemfile
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.OPN"
	USE
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Aging Report for " + PRNT_DATE(AGE_DATE$, 8%)
	TITLE$(2%) = "AR System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	INTRVL$(I%) = LEFT(SPACE$(16% - &
		LEN(EDIT$(AR_CONTROL::AGENAM(I%), 140%))) + &
		EDIT$(AR_CONTROL::AGENAM(I%), 140%), 16%) &
		FOR I% = 0% TO 4%

	TITLE$(4%) = LEFT(AR_CONTROL::CTITLE, 10%) + &
		"   Name                                                Phone  "
	TITLE$(5%) = "    Account           " + &
		INTRVL$(0%) + INTRVL$(1%) + INTRVL$(2%) + &
		INTRVL$(3%) + INTRVL$(4%) + &
		" Service Charge        Balance"
	TITLE$(6%) = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AR_35CUSTOM.CH%, KEY #K_NUM%
		ELSE
			FIND #AR_35CUSTOM.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	LAST_CUSTOM$ = "01234567890123456789"
	TOTAL_CUSBAL% = 0%

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AR_35CUSTOM.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORTBY$
	CASE "N"
		GOTO ExitTotal IF (AR_35CUSTOM::CUSNUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "T"
		GOTO ExitTotal IF (AR_35CUSTOM::TTYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "C"
		GOTO ExitTotal IF (AR_35CUSTOM::CATEGORY > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "A"
		GOTO ExitTotal IF (AR_35CUSTOM::ALPSRT > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	END SELECT

	!
	! Collect aging information
	!
	IF AR_FUNC_AGE(AR_35CUSTOM::CUSNUM, AR_35CUSTOM::METHOD, &
		AGE_DATE$, CUTOFF$, &
		NUM_ACCT%, ARRAY_CUSBAL()) <> 0%
	THEN
		!
		! Bitch about aging problems
		!
		TEXT$ = AR_35CUSTOM::CUSNUM + "  " + &
			AR_35CUSTOM::CUSNAM + "  " + &
			PRNT_PHONE(AR_35CUSTOM::PHONE, 0%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		TEXT$ = "           *** Unable to age ***"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		GOTO GetNextRec
	END IF

	!
	! Skip if no aging information
	!
	GOTO GetNextRec IF NUM_ACCT% = 0%

	!
	! Set print flag to zero
	!
	PRINT_FLAG% = 0%

	!
	! Print out aging information
	!
	FOR LOOP% = 1% TO NUM_ACCT%

		GOTO SkipPrint IF COMP_STRING(EDIT$(ARRAY_CUSBAL(LOOP%)::ACCT, &
			-1%), GL_WILDCARD$) = 0% AND GL_WILDCARD$ <> ""

		FOR INDEX% = FROM_PERIOD% TO TO_PERIOD%
			BALANCE = BALANCE + ARRAY_CUSBAL(LOOP%)::AGING(INDEX%)
		NEXT INDEX%

		BALANCE = &
			FUNC_ROUND(ARRAY_CUSBAL(LOOP%)::CHARGE + &
			ARRAY_CUSBAL(LOOP%)::FUTURE + BALANCE, 2%)

		!
		! Code to skip over those that are not wanted
		!
		IF BALANCE = 0.0 AND ZERO_BALANCE$ <> "Y"
		THEN
			GOTO SkipPrint
		END IF

		PRINT_FLAG% = -1%

		IF LAST_CUSTOM$ <> AR_35CUSTOM::CUSNUM
		THEN
			!
			! Print out customer name
			!
			TEXT$ = AR_35CUSTOM::CUSNUM + "   " + &
				AR_35CUSTOM::CUSNAM + "  " + &
				PRNT_PHONE(AR_35CUSTOM::PHONE, 0%)

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			GOTO ExitProgram IF UTL_REPORTX::STAT

			LAST_CUSTOM$ = AR_35CUSTOM::CUSNUM + ""
		END IF

		TEXT$ = "    " + &
			ARRAY_CUSBAL(LOOP%)::ACCT + " " + &
			FORMAT$(ARRAY_CUSBAL(LOOP%)::AGING(0%) + &
				ARRAY_CUSBAL(LOOP%)::FUTURE, &
				"  ##########.## ") + &
			FORMAT$(ARRAY_CUSBAL(LOOP%)::AGING(1%), &
				"  ##########.## ") + &
			FORMAT$(ARRAY_CUSBAL(LOOP%)::AGING(2%), &
				"  ##########.## ") + &
			FORMAT$(ARRAY_CUSBAL(LOOP%)::AGING(3%), &
				"  ##########.## ") + &
			FORMAT$(ARRAY_CUSBAL(LOOP%)::AGING(4%), &
				"  ##########.## ") + &
			FORMAT$(ARRAY_CUSBAL(LOOP%)::CHARGE, &
				" ##########.## ") + &
			FORMAT$(BALANCE, &
				" ##########.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		!
		! Summarize for totals
		!
		FOR I% = 1% TO TOTAL_CUSBAL%

			GOTO 17110 &
				IF TOTAL_CUSBAL(I%)::ACCT = &
				ARRAY_CUSBAL(LOOP%)::ACCT

			IF TOTAL_CUSBAL(I%)::ACCT > ARRAY_CUSBAL(LOOP%)::ACCT
			THEN
				TOTAL_CUSBAL(J% + 1%) = TOTAL_CUSBAL(J%) &
					FOR J% = TOTAL_CUSBAL% TO I% STEP -1%
				TOTAL_CUSBAL% = TOTAL_CUSBAL% + 1%
				TOTAL_CUSBAL(I%)::ACCT = ARRAY_CUSBAL(LOOP%)::ACCT
				TOTAL_CUSBAL(I%)::AGING(J%) = 0.0 &
					FOR J% = 0% TO 4%
				TOTAL_CUSBAL(I%)::CHARGE = 0.0
				TOTAL_CUSBAL(I%)::CREDIT = 0.0
				TOTAL_CUSBAL(I%)::FUTURE = 0.0

				GOTO 17110
			END IF

		NEXT I%

		TOTAL_CUSBAL%, I% = TOTAL_CUSBAL% + 1%
		TOTAL_CUSBAL(I%)::ACCT = ARRAY_CUSBAL(LOOP%)::ACCT
		TOTAL_CUSBAL(I%)::AGING(J%) = 0.0 &
			FOR J% = 0% TO 4%
		TOTAL_CUSBAL(I%)::CHARGE = 0.0
		TOTAL_CUSBAL(I%)::CREDIT = 0.0
		TOTAL_CUSBAL(I%)::FUTURE = 0.0

17110		TOTAL_CUSBAL(I%)::AGING(J%) = TOTAL_CUSBAL(I%)::AGING(J%) + &
			ARRAY_CUSBAL(LOOP%)::AGING(J%) &
			FOR J% = 0% TO 4%
		TOTAL_CUSBAL(I%)::CHARGE = TOTAL_CUSBAL(I%)::CHARGE + &
			ARRAY_CUSBAL(LOOP%)::CHARGE
		TOTAL_CUSBAL(I%)::CREDIT = TOTAL_CUSBAL(I%)::CREDIT + &
			ARRAY_CUSBAL(LOOP%)::CREDIT
		TOTAL_CUSBAL(I%)::FUTURE = TOTAL_CUSBAL(I%)::FUTURE + &
			ARRAY_CUSBAL(LOOP%)::FUTURE

 SkipPrint:
	NEXT LOOP%

	IF PRINT_FLAG%
	THEN
		!
		! Add a blank line to seperate the records
		!
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 10%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
		"*** Grand Totals **", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GRAND_CUSBAL::AGING(J%) = 0.0 &
		FOR J% = 0% TO 4%
	GRAND_CUSBAL::CHARGE = 0.0
	GRAND_CUSBAL::CREDIT = 0.0
	GRAND_CUSBAL::FUTURE = 0.0

	FOR LOOP% = 1% TO TOTAL_CUSBAL%

		BALANCE = &
			TOTAL_CUSBAL(LOOP%)::AGING(0%) + &
			TOTAL_CUSBAL(LOOP%)::AGING(1%) + &
			TOTAL_CUSBAL(LOOP%)::AGING(2%) + &
			TOTAL_CUSBAL(LOOP%)::AGING(3%) + &
			TOTAL_CUSBAL(LOOP%)::AGING(4%) + &
			TOTAL_CUSBAL(LOOP%)::CHARGE

		TEXT$ = "    " + &
			TOTAL_CUSBAL(LOOP%)::ACCT + " " + &
			FORMAT$(TOTAL_CUSBAL(LOOP%)::AGING(0%) + &
				TOTAL_CUSBAL(LOOP%)::FUTURE, &
				"  ##########.## ") + &
			FORMAT$(TOTAL_CUSBAL(LOOP%)::AGING(1%), &
				"  ##########.## ") + &
			FORMAT$(TOTAL_CUSBAL(LOOP%)::AGING(2%), &
				"  ##########.## ") + &
			FORMAT$(TOTAL_CUSBAL(LOOP%)::AGING(3%), &
				"  ##########.## ") + &
			FORMAT$(TOTAL_CUSBAL(LOOP%)::AGING(4%), &
				"  ##########.## ") + &
			FORMAT$(TOTAL_CUSBAL(LOOP%)::CHARGE, &
				" ##########.## ") + &
			FORMAT$(BALANCE, &
				" ##########.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		GRAND_CUSBAL::AGING(J%) = GRAND_CUSBAL::AGING(J%) + &
			TOTAL_CUSBAL(LOOP%)::AGING(J%) &
			FOR J% = 0% TO 4%
		GRAND_CUSBAL::CHARGE = GRAND_CUSBAL::CHARGE + &
			TOTAL_CUSBAL(LOOP%)::CHARGE
		GRAND_CUSBAL::CREDIT = GRAND_CUSBAL::CREDIT + &
			TOTAL_CUSBAL(LOOP%)::CREDIT
		GRAND_CUSBAL::FUTURE = GRAND_CUSBAL::FUTURE + &
			TOTAL_CUSBAL(LOOP%)::FUTURE

	NEXT LOOP%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	BALANCE = &
		GRAND_CUSBAL::AGING(0%) + &
		GRAND_CUSBAL::AGING(1%) + &
		GRAND_CUSBAL::AGING(2%) + &
		GRAND_CUSBAL::AGING(3%) + &
		GRAND_CUSBAL::AGING(4%) + &
		GRAND_CUSBAL::CHARGE

	TEXT$ = "    " + &
		"Grand Total        " + &
		FORMAT$(GRAND_CUSBAL::AGING(0%) + &
			GRAND_CUSBAL::FUTURE, &
			"  ##########.## ") + &
		FORMAT$(GRAND_CUSBAL::AGING(1%), &
			"  ##########.## ") + &
		FORMAT$(GRAND_CUSBAL::AGING(2%), &
			"  ##########.## ") + &
		FORMAT$(GRAND_CUSBAL::AGING(3%), &
			"  ##########.## ") + &
		FORMAT$(GRAND_CUSBAL::AGING(4%), &
			"  ##########.## ") + &
		FORMAT$(GRAND_CUSBAL::CHARGE, &
			" ##########.## ") + &
		FORMAT$(BALANCE, &
			" ##########.## ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT


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

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
