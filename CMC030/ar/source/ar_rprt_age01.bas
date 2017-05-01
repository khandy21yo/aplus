1	%TITLE "AR Aging Report"
	%SBTTL "AR_RPRT_AGE01"
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
	! ID:AR035
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program prints the AR Aging report.
	!	.lm -5
	!
	! Index:
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
	!	03/03/88 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_RPRT_AGE01/LINE
	!	$ LINK/EXECUTABLE=AR_EXE:*.EXE AR_RPRT_AGE01, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_RPRT_AGE01.OBJ;*
	!
	! Modification history:
	!
	!	04/22/88 - Kevin Handy
	!		Added totals to bottom of report
	!
	!	06/21/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		also be sent to either a spreadsheet or a DIF file.
	!
	!	10/03/91 - Kevin Handy
	!		Modified so that CUSBAL open error is handled better.
	!
	!	06/14/93 - Kevin Handy
	!		Added REGARDLESS and CLOSE to AR_CONTROL.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/29/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	05/05/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.HB"
	MAP (AR_CUSBAL)		AR_CUSBAL_CDD		AR_CUSBAL
	DIM			AR_CUSBAL_CDD		ARRAY_CUSBAL(50%)

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

	!
	! Declare variables and constants
	!
	DECLARE	STRING		LYT_LINE

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Init:
	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)
	AGE_DATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(3%), -1%))
	ZERO_BALANCE$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

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
		CONTINUE 330 IF ERR = 5%
		FILENAME$ = "AR_CUSBAL"
		CONTINUE HelpError
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
	TITLE$(1%) = "Aging Report as of " + PRNT_DATE(AGE_DATE$, 8%)
	TITLE$(2%) = "AR System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	INTRVL$(I%) = &
		LEFT(SPACE$(12% - LEN(EDIT$(AR_CONTROL::AGENAM(I%), -1%))) + &
		EDIT$(AR_CONTROL::AGENAM(I%), 140%), 12%) + " " &
		FOR I% = 0% TO 4%

	TITLE$(4%) = LEFT(AR_CONTROL::CTITLE, 10%) + &
		" Name                           Phone        " + &
		INTRVL$(0%) + INTRVL$(1%) + INTRVL$(2%) + &
		INTRVL$(3%) + INTRVL$(4%) + "    Balance"

	TITLE$(5%) = ""

	!
	! Layouts for printed lines
	!
	LYT_LINE = "$" + AR_CONTROL::CTITLE + ":010,$Name:041," + &
		"PPhoneNum:055,V" + INTRVL$(0%) + "Balance:067,V" + &
		INTRVL$(1%) + "Balance:080,V" + INTRVL$(2%) + &
		"Balance:0093,V" + INTRVL$(3%) + "Balance:0106,V" + &
		INTRVL$(4%) + "Balance:119,VEndBalance:132"

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
		FILENAME$ = "AR_35CUSTOM"
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
		AGE_DATE$, "", NUM_ACCT%, ARRAY_CUSBAL()) <> 0%
	THEN
		!
		! Bitch about aging problems
		!
		TEXT$ = AR_35CUSTOM::CUSNUM + "  " + &
			LEFT(AR_35CUSTOM::CUSNAM, 30%) + "  " + &
			PRNT_PHONE(AR_35CUSTOM::PHONE, 0%) + &
			"           *** Unable to age ***"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		GOTO GetNextRec
	END IF

	!
	! Skip if no aging information
	!
	GOTO GetNextRec IF NUM_ACCT% = 0%

	!
	! Zero balances
	!
	CUSBAL(J%) = 0.0 FOR J% = 0% TO 4%
	SRVCHG = 0.0

	!
	! Accumulate aging information
	!
	FOR LOOP% = 1% TO NUM_ACCT%
		!
		! Customer total
		!
		CUSBAL(J%) = CUSBAL(J%) + ARRAY_CUSBAL(LOOP%)::AGING(J%) &
			FOR J% = 0% TO 4%
		SRVCHG = SRVCHG + ARRAY_CUSBAL(LOOP%)::CHARGE

		!
		! Grand total
		!
		TOTAL_CUSBAL(J%) = TOTAL_CUSBAL(J%) + &
			ARRAY_CUSBAL(LOOP%)::AGING(J%) &
			FOR J% = 0% TO 4%
		TOTAL_SRVCHG = TOTAL_SRVCHG + ARRAY_CUSBAL(LOOP%)::CHARGE
	NEXT LOOP%

	!
	! Accumulate balance
	!
	BALANCE = &
		FUNC_ROUND(CUSBAL(0%) + &
		CUSBAL(1%) + &
		CUSBAL(2%) + &
		CUSBAL(3%) + &
		CUSBAL(4%) + &
		SRVCHG, 2%)

	IF BALANCE <> 0.0 OR ZERO_BALANCE$ = "Y"
	THEN
		!
		! Print aging line
		!
		TEXT$ = AR_35CUSTOM::CUSNUM + " " + &
			LEFT(AR_35CUSTOM::CUSNAM, 30%) + " " + &
			PRNT_PHONE(AR_35CUSTOM::PHONE, 0%) + &
			FORMAT$(CUSBAL(0%) + SRVCHG, "#########.## ") + &
			FORMAT$(CUSBAL(1%), "#########.## ") + &
			FORMAT$(CUSBAL(2%), "#########.## ") + &
			FORMAT$(CUSBAL(3%), "#########.## ") + &
			FORMAT$(CUSBAL(4%), "#########.## ") + &
			FORMAT$(BALANCE, "########.##")

		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

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
	BALANCE = &
		TOTAL_CUSBAL(0%) + &
		TOTAL_CUSBAL(1%) + &
		TOTAL_CUSBAL(2%) + &
		TOTAL_CUSBAL(3%) + &
		TOTAL_CUSBAL(4%) + &
		TOTAL_SRVCHG

	TEXT$ = "           " + &
		"Total" + SPACE$(39%) + &
		FORMAT$(TOTAL_CUSBAL(0%) + TOTAL_SRVCHG, "#########.## ") + &
		FORMAT$(TOTAL_CUSBAL(1%), "#########.## ") + &
		FORMAT$(TOTAL_CUSBAL(2%), "#########.## ") + &
		FORMAT$(TOTAL_CUSBAL(3%), "#########.## ") + &
		FORMAT$(TOTAL_CUSBAL(4%), "#########.## ") + &
		FORMAT$(BALANCE, "########.##")

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
