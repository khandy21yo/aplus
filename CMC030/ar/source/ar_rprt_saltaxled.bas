1	%TITLE "Sales Tax Monthly Report"
	%SBTTL "AR_RPRT_SALTAXLED"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1988 BY
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
	! ID:AR023
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Sales Tax Ledger\* will provide a report which will list
	!	all sales for each state, by sales tax taxability.
	!	.b
	!	The following fields are included:
	!	.table 3,25
	!	.te
	!	Tax Type
	!	.te
	!	Customer
	!	.te
	!	Description
	!	.te
	!	Invoice
	!	.te
	!	Date
	!	.te
	!	Amount
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Sales Tax Ledger
	!	.x Sales Tax>Ledger
	!	.x Print>Sales Tax Ledger
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$  BAS AR_SOURCE:AR_RPRT_SALTAXLED.BAS/LINE
	!	$  LINK/EXE=AR_EXE:*.EXE AR_RPRT_SALTAXLED, FUNC_LIB:CMCLINK/OPTION
	!	$  DELETE AR_RPRT_SALTAXLED.OBJ;*
	!
	! Author:
	!
	!	02/25/88 - Aaron Redd
	!
	! Modification history:
	!
	!	06/03/88 - J. Shad Rydalch
	!		Reformated output, added option to print zero,
	!		Added total for tax type
	!
	!	01/29/93 - Dan Perkins
	!		Re-initialize SUB_TOTAL even if we don't print it.
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/30/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/26/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[AR.OPEN]AR_SALTAXLED.HB"
	MAP (AR_SALTAXLED)	AR_SALTAXLED_CDD	AR_SALTAXLED

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD		AR_CONTROL

	%PAGE

	ON ERROR GOTO 19000

	!
	! Declare temporary variables and totals
	!
	DECLARE STRING	TEMP_TAX, TEMP_CUST
	DECLARE REAL	SUB_TOTAL, TAX_TOTAL, GRAND_TOTAL
	DECLARE LONG	NUM_LINES

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_DATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(0%), 132%))

	!++
	! Abstract:FLD01
	!	^*(01) From Date\*
	!	.b
	!	.lm +5
	!	A ^*From Date\* field causes the printing
	!	to begin with the selected date.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x From Date>Print Sales Tax Ledger
	!	.x Print Sales Tax Ledger>From Date
	!
	!--

	TO_DATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(1%), 132%))

	!++
	! Abstract:FLD02
	!	^*(02) To Date\*
	!	.b
	!	.lm +5
	!	A ^*To Date\* field causes the printing
	!	to end with the selected date.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x To Date>Print Sales Tax Ledger
	!	.x Print Sales Tax Ledger>To Date
	!
	!--

	PRNT_ZERO$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) Print Zeros\*
	!	.b
	!	.lm +5
	!	A ^*Print Zero\* Y for Yes in this field will print all
	!	records.  An N for No will suppress the
	!	printing of records with an amount of zero.
	!	.b
	!	The format for entry is Y or N.
	!	.lm -5
	!
	! Index:
	!	.x Print Sales Tax Ledger>Print Zeros
	!
	!--

300	%INCLUDE "SOURCE:[AR.OPEN]AR_SALTAXLED.OPN"

310	!
	! Open AR_35CUSTOM file
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"

320	!
	! Open control file
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.OPN"

	GET #AR_CONTROL.CH%, RECORD 1%, REGARDLESS

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Sales Tax File List"
	TITLE$(2%) = "List of Sales Taxes"

	TITLE$(2%) = TITLE$(2%) + "  From " + PRNT_DATE(FROM_DATE$, 8%) &
		IF EDIT$(FROM_DATE$, -1%) <> ""

	TITLE$(2%) = TITLE$(2%) + "  To " + PRNT_DATE(TO_DATE$, 8%) &
		IF EDIT$(TO_DATE$, -1%) <> ""

	TITLE$(3%) = ""

	TITLE$(4%) = "Tax Type   " + &
		LEFT(AR_CONTROL::CTITLE, 10%) + &
		"   Name                                               " + &
		"Invoice    Date            Amount"

	TITLE$(5%) = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		RESET #AR_SALTAXLED.CH%
	USE
		CONTINUE ExitProgram
	END WHEN

	GRAND_TOTAL = 0.0
	TAX_TOTAL = 0.0
	SUB_TOTAL = 0.0
	NUM_LINES = 0%
	TEMP_TAX = ""
	TEMP_CUST = ""

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AR_SALTAXLED.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram
	END WHEN

	!
	! Skip record if amount is zero and user wants to skip zeros
	!
	GOTO GetNextRec IF PRNT_ZERO$ = "N" AND AR_SALTAXLED::AMOUNT = 0.0

	!
	! Skip record if date is out of range
	!
	GOTO GetNextRec IF (AR_SALTAXLED::TRADAT < FROM_DATE$)

	GOTO GetNextRec IF (AR_SALTAXLED::TRADAT > TO_DATE$) AND &
		(TO_DATE$ > "00000000")

	!
	! Check if it is time to print some totals
	!
17040	IF TEMP_TAX <> AR_SALTAXLED::TAXTYP OR TEMP_CUST <> AR_SALTAXLED::CUSNUM
	THEN

		GOSUB SubTotal

		GOSUB TaxTotal

		!
		! Get the AR_CUSNUM customer name
		!
		WHEN ERROR IN
			GET #AR_35CUSTOM.CH%, &
				KEY #0% EQ AR_SALTAXLED::CUSNUM, &
				REGARDLESS
		USE
			AR_35CUSTOM::CUSNAM = &
				STRING$(LEN(AR_35CUSTOM::CUSNAM), A"?"B)

			CONTINUE 17050
		END WHEN
	END IF

	!
17050	! Put togeter a line to print out
	!
	TEXT$ = AR_SALTAXLED::TAXTYP + "          " + &
		AR_SALTAXLED::CUSNUM + "   " + &
		AR_35CUSTOM::CUSNAM + " " + &
		AR_SALTAXLED::INVNUM + "   " + &
		PRNT_DATE(AR_SALTAXLED::TRADAT, 8%) + "   " + &
		FORMAT$(AR_SALTAXLED::AMOUNT, "######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	AR_35CUSTOM::CUSNAM = ""

	!
	! Put Tax Type and Customer number into a temporary variable
	!
	TEMP_TAX = AR_SALTAXLED::TAXTYP + ""
	TEMP_CUST = AR_SALTAXLED::CUSNUM + ""

	!
	! Take care of totals and line counter
	!
	TAX_TOTAL = TAX_TOTAL + AR_SALTAXLED::AMOUNT
	SUB_TOTAL = SUB_TOTAL + AR_SALTAXLED::AMOUNT
	GRAND_TOTAL = GRAND_TOTAL + AR_SALTAXLED::AMOUNT
	NUM_LINES = NUM_LINES + 1%

17200	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitProgram:
	!
	! Set test variable so that final totals will print
	!
	AR_SALTAXLED::TAXTYP = ""

	GOSUB SubTotal

	GOSUB TaxTotal

	TEXT$ = "                        Grand Total:" + SPACE$(61%) + &
		FORMAT$(GRAND_TOTAL, "########.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

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
	IF TEMP_TAX + TEMP_CUST <> ""
	THEN
		IF NUM_LINES > 1%
		THEN
			TEXT$ = TEMP_TAX + "          " + &
				TEMP_CUST + "   " + &
			"SUB-Total:" + SPACE$(63%) + &
			FORMAT$(SUB_TOTAL, "########.##")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		ELSE
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		END IF

	END IF

	SUB_TOTAL = 0.0
	NUM_LINES = 0%

	RETURN

 TaxTotal:
	IF LEN(TEMP_TAX) > 0% AND TEMP_TAX <> AR_SALTAXLED::TAXTYP
	THEN
		TEXT$ = TEMP_TAX + "                       " + &
			"Tax Type Total:" + SPACE$(58%) + &
			FORMAT$(TAX_TOTAL, "########.##")

		TAX_TOTAL = 0.0

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	END IF

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
