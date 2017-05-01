1	%TITLE "Credit Memo History Report"
	%SBTTL "AR_RPRT_CREDITHIST"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
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
	! ID:AR009
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Credit Memo History Report\* program prints a report of issued
	!	Credit Memos.  The report prints a Credit Memo History from a given
	!	date range. The report shows totals for each customer and a grand
	!	total is printed for the total number of credit memos.
	!	.lm -5
	!
	! Index:
	!	.x Credit Memo History
	!
	! Option:
	!
	! Author:
	!
	!	11/13/91 - Dan Perkins
	!
	! Compile:
	!
	!	$  BAS AR_SOURCE:AR_RPRT_CREDITHIST.BAS/LINE
	!	$  LINK/EXE=AR_EXE:*.EXE AR_RPRT_CREDITHIST, FUNC_LIB:CMCLINK/OPTION
	!	$  DELETE AR_RPRT_CREDITHIST.OBJ;*
	!
	! Modification history:
	!
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/19/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/13/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/20/2000 - Kevin Handy
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
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN)		AR_OPEN_CDD		AR_OPEN

	%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.HB"
	MAP (AR_CLOSED)		AR_CLOSED_CDD		AR_CLOSED

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Set some initial variable values
	!
	TOTAL = 0.0
	THIS_TOTAL = 0.0

 Init:
	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_DATE$ = DATE_STOREDATE(TRM$(UTL_REPORTX::OPTDEF(0%)))

	!++
	! Abstract:FLD01
	!	^*(01) From Date\*
	!	.b
	!	.lm +5
	!	The ^*From Date\* field refers to the starting date for
	!	which the report will be printed.
	!	.b
	!	A blank field will cause the report to begin with the
	!	earliest dated item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Date>Credit Memo History
	!	.x Credit Memo History>Start Date
	!
	!--

	TO_DATE$ = DATE_STOREDATE(TRM$(UTL_REPORTX::OPTDEF(1%)))

	!++
	! Abstract:FLD02
	!	^*(02) To Date\*
	!	.b
	!	.lm +5
	!	The ^*To Date\* field refers to the ending date for
	!	which the report will be printed.
	!	.b
	!	A blank field will cause the report to end with the
	!	most recent date in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Date>Credit Memo History
	!	.x Credit Memo History>End Date
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) From Customer\*
	!	.b
	!	.lm +5
	!	The ^*From Customer\* field causes the
	!	printing to begin with a particular
	!	customer.
	!	.b
	!	A blank field will cause the report to begin with
	!	the first customer in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Customer>Credit Memo History
	!	.x Credit Memo History>From Customer
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) To Customer\*
	!	.b
	!	.lm +5
	!	The ^*To Customer\* field causes the
	!	printing to end with a particular
	!	customer.
	!	.b
	!	A blank field causes the report to end with the
	!	last customer in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Customer>Credit Memo History
	!	.x Credit Memo History>To Customer
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated
	!	items to be printed by entering a "wildcard" for
	!	Wildcarding Technique.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Credit Memo History
	!	.x Credit Memo History>Wildcard
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.OPN"
	USE
		CONTINUE 320 IF ERR = 5%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "CLOSED"
		CONTINUE HelpError
	END WHEN

	%PAGE

	!*******************************************************************
	! Titles
	!*******************************************************************

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = " CREDIT MEMO HISTORY REPORT"
	TITLE$(2%) = "AR System"
	TITLE$(3%) = "From " + PRNT_DATE(FROM_DATE$, 8%) + &
		" To " + PRNT_DATE(TO_DATE$, 8%)
	TITLE$(3%) = "Before " + PRNT_DATE(TO_DATE$, 8%) IF FROM_DATE$ = ""
	TITLE$(3%) = "After " + PRNT_DATE(FROM_DATE$, 8%) IF TO_DATE$ = ""
	TITLE$(3%) = "For All Dates" IF FROM_DATE$ + TO_DATE$ = ""
	TITLE$(4%) = ""

	!
	! Column headings
	!
	TITLE$(5%) = "Custom #   Name" + SPACE$(27%) + &
		"Memo #   Date     Salesman # " + &
		"Descripton" + SPACE$(24%) + "Amount"

	TITLE$(6%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AR_35CUSTOM.CH%, KEY #0%
		ELSE
			FIND #AR_35CUSTOM.CH%, &
				KEY #0% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitTotal IF ERR = 155%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		GET #AR_35CUSTOM.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	GOTO ExitTotal IF (AR_35CUSTOM::CUSNUM > TO_ITEM$) AND TO_ITEM$ <> ""

	GOTO GetNextRec IF COMP_STRING(EDIT$( &
		AR_35CUSTOM::CUSNUM, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

17100	WHEN ERROR IN
		FIND #AR_OPEN.CH%, KEY #0% EQ AR_35CUSTOM::CUSNUM, REGARDLESS
	USE
		CONTINUE 17200 IF ERR = 9% OR ERR = 155%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

 GetOpenRec:
	WHEN ERROR IN
		GET #AR_OPEN.CH%, REGARDLESS
	USE
		CONTINUE 17200 IF ERR = 9% OR ERR = 11%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record if should be printed
	!
	GOTO 17200 IF AR_OPEN::CUSNUM <> AR_35CUSTOM::CUSNUM

	GOTO GetOpenRec IF AR_OPEN::TRATYP <> "08"

	GOTO GetOpenRec IF AR_OPEN::TRADAT < FROM_DATE$
	GOTO GetOpenRec IF AR_OPEN::TRADAT > TO_DATE$ AND TO_DATE$ <> ""

	!
	! Print the cash receipt header
	!
	TEXT$ = AR_OPEN::CUSNUM	+ " " + &
		LEFT(AR_35CUSTOm::CUSNAM, 30%) + " " + &
		AR_OPEN::INVNUM + " " + &
		PRNT_DATE(AR_OPEN::TRADAT, 6%) + " " + &
		AR_OPEN::SALNUM + " " + &
		AR_OPEN::DESCR + " " + &
		FORMAT$(AR_OPEN::SALAMT, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TOTAL = TOTAL + AR_OPEN::SALAMT
	THIS_TOTAL = THIS_TOTAL + AR_OPEN::SALAMT

	!
	! Try for next record
	!
	GOTO GetOpenRec

17200	WHEN ERROR IN
		FIND #AR_CLOSED.CH%, KEY #0% EQ AR_35CUSTOM::CUSNUM, REGARDLESS
	USE
		CONTINUE PrintTotal IF ERR = 9% OR ERR = 155%
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

 GetClosedRec:
	WHEN ERROR IN
		GET #AR_CLOSED.CH%, REGARDLESS
	USE
		CONTINUE PrintTotal IF ERR = 9% OR ERR = 11%
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN


	!
	! Check current record if should be printed
	!
	GOTO PrintTotal IF AR_CLOSED::CUSNUM <> AR_35CUSTOM::CUSNUM

	GOTO GetClosedRec IF AR_CLOSED::TRATYP <> "08"

	GOTO GetClosedRec IF AR_CLOSED::TRADAT < FROM_DATE$
	GOTO GetClosedRec IF AR_CLOSED::TRADAT > TO_DATE$ AND TO_DATE$ <> ""

	!
	! Print the cash receipt header
	!
	TEXT$ = AR_CLOSED::CUSNUM + " " + &
		LEFT(AR_35CUSTOM::CUSNAM, 30%) + " " + &
		AR_CLOSED::INVNUM + " " + &
		PRNT_DATE(AR_CLOSED::TRADAT, 6%) + " " + &
		AR_CLOSED::SALNUM + " " + &
		AR_CLOSED::DESCR + " " + &
		FORMAT$(AR_CLOSED::SALAMT, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TOTAL = TOTAL + AR_CLOSED::SALAMT
	THIS_TOTAL = THIS_TOTAL + AR_CLOSED::SALAMT

	!
	! Try for next record
	!
	GOTO GetClosedRec

	!
	! Print a total for one salesman
	!
 PrintTotal:
	IF THIS_TOTAL <> 0.0
	THEN
		THIS_TOTAL = FUNC_ROUND(THIS_TOTAL, 2%)
		TEXT$ = SPACE$(80%) + &
			"Customer Total:  " + &
			FORMAT$(THIS_TOTAL, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -2%)
			GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	THIS_TOTAL = 0.0

	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!
	TOTAL = FUNC_ROUND(TOTAL, 2%)
	TEXT$ = SPACE$(83%) + &
		"Grand Total:  " + FORMAT$(TOTAL, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)
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

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
