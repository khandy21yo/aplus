1	%TITLE "Accounts Payable Age Detail Not Yet Due Report"
	%SBTTL "AP_RPRT_AGE_DETNOTDUE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1993 BY
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
	! ID:APAGEN
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Payables Detail Aging Not Yet Due Report\* option
	!	prints a
	!	report displaying each item owed each vendor and aging those
	!	items in categories ranging from "Current" to "91 days _& Over"
	!	in thirty day increments.  The report also contains a column
	!	in which payments Not Yet Due are listed.
	!	.b
	!	At the user's option, the report may be aged from the date
	!	of the vendors' invoices or from the date the payment is due on
	!	the invoices.
	!	The ^*Aging Report\* contains the following fields:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Vendor Number
	!	.le
	!	Vendor Name
	!	.le
	!	Invoice Date
	!	.le
	!	Invoice Number
	!	.le
	!	Current
	!	.le
	!	31 to 60 Days
	!	.le
	!	61 to 90 Days
	!	.le
	!	91 Days and Over
	!	.le
	!	Not Yet Due
	!	.le
	!	Balance
	!	.els
	!
	! Index:
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_RPRT_AGE_DETNOTDUE/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_RPRT_AGE_DETNOTDUE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_RPRT_AGE_DETNOTDUE.OBJ;*
	!
	! Author:
	!
	!	04/07/93 - Dan Perkins
	!
	! Modification history:
	!
	!	04/11/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/19/93 - Kevin Handy
	!		Modified to include not-due in the total column,
	!		instead of just due items.
	!
	!	06/14/93 - Kevin Handy
	!		Fixed another total not including not-due.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/13/96 - Kevin Handy
	!		Reformat source code.
	!		Lose extra '&' before 'else'.
	!
	!	05/20/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/20/97 - Kevin Handy
	!		Don't need to assign channel for report.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/26/2000 - Kevin Handy
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
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs, etc.)
	!******************************************************************

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP (AP_OPEN)		AP_OPEN_CDD		AP_OPEN

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR)		AP_VENDOR_CDD		AP_VENDOR

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
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^*(01) Sort By (NU,NA,S)\*
	!	.b
	!	.lm +5
	!	The ^*Sort By (NU,NA,S)\* field determines if the
	!	report is to be printed in vendor number order, vendor name
	!	order, or in alphabetical order.
	!	.lm 15
	!	.b
	!	.ls "*"
	!	.le
	!	^*NU\*#=#Vendor Number order
	!	.le
	!	^*NA\*#=#Vendor Nameorder
	!	.le
	!	^*S\*##=#Alphabetical order
	!	.els
	!	.lm -5
	!	There are no other valid values for this field.
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
	!	The ^*From Item\* field causes the report to print
	!	beginning with an item entered in this field. The value must be
	!	in agreement with field (01) Sort by.
	!	.b
	!	A blank field will cause the report to print beginning with the
	!	first item in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes the report to end
	!	with the item entered in this field. The value must be
	!	in agreement with field (01) Sort by.
	!	.b
	!	A blank field will cause the report to print to the end of the
	!	file.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated items to be
	!	printed by entering a "wildcard" using the wildcarding technique. The value
	!	entered must be in agreement with field (02) Sort by.
	!	.b
	!	For information on "wildcarding", refer to Appendix B.
	!	.lm -5
	!
	! Index:
	!
	!--

	DATE_TO_USE$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Date to use (I,D)\*
	!	.b
	!	.lm +5
	!	The ^*Date to use (I,D)\* field determines if the
	!	aging is dependent upon the dates of vendors' invoices or the dates
	!	that payment of the invoices is due.
	!	.lm 15
	!	.b
	!	.ls "*"
	!	.le
	!	^*I\*#=#Invoice dates
	!	.le
	!	^*D\*#=#Due Dates
	!	.els
	!	.b
	!	.lm -5
	!	There are no other valid values for this field.
	!
	! Index:
	!
	!--

	REP_DATE$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(5%))

	!++
	! Abstract:FLD06
	!	^*(06) Age Date (MMDDYYYY)\*
	!	.b
	!	.lm +5
	!	The ^*Age Date\* field determines the base date
	!	from which ages of the Accounts Payable items will be calculated.
	!	The date is to be entered in MMDDYYYY format.
	!	.lm -5
	!
	! Index:
	!
	!--

	CUTOFF_DATE$ = EDIT$(UTL_REPORTX::OPTDEF(6%), -1%)
	BASE_DAY% = DATE_DAYCODE(REP_DATE$)

	!++
	! Abstract:FLD07
	!	^*(07) Cutoff Period\*
	!	.b
	!	.lm +5
	!	The ^*Cutoff Period\* field generates
	!	a "cutoff register" aging report.  Entries posted after the
	!	cutoff period will not appear on the report.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!
	!--

	ZERO_BALANCE$ = EDIT$(UTL_REPORTX::OPTDEF(7%), -1%)

	!++
	! Abstract:FLD08
	!	^*(08) Show Zero Balance\*
	!	.b
	!	.lm +5
	!	The ^*Show Zero Balance\* field indicates if the report
	!	is to contain invoices that balance to zero (Y), or to
	!	only show invoices with a non-zero amount (N).
	!	.lm -5
	!
	! Index:
	!
	!--

	%PAGE

	!
	! Open the AP Open file
	!
300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.OPN"
	USE
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

	!
	! Open the AP Vendor file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.OPN"
	USE
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	SELECT SORTBY$

	CASE "NU"
		K_NUM% = 0%
		ADD_TITLE$ = "BY VENDOR NUMBER"

	CASE "NA"
		K_NUM% = 1%
		ADD_TITLE$ = "BY VENDOR NAME"

	CASE "S"
		K_NUM% = 2%
		ADD_TITLE$ = "BY VENDOR ALPHA"

	END SELECT

	!
	! Title
	!
	IF DATE_TO_USE$ = "D"
	THEN
		TITLE$(1%) = "NOT YET DUE AGED DETAIL REPORT " + &
			ADD_TITLE$ + " USING THE DUE DATE"
	ELSE
		TITLE$(1%) = "NOT YET DUE AGED DETAIL REPORT " + &
			ADD_TITLE$ + " USING THE INVOICE DATE"
	END IF

	TITLE$(2%) = "Aged as of " + PRNT_DATE(REP_DATE$, 8%)

	TITLE$(3%) = "AP System"

	I% = 4%

	IF CUTOFF_DATE$ <> ""
	THEN
		TITLE$(I%) = "Cutoff Period: " + CUTOFF_DATE$
		I% = I% + 1%
	END IF

	TITLE$(I%) = ""

	!
	! Heading
	!
	IF DATE_TO_USE$ = "D"
	THEN
		TITLE$(I% + 1%) = "VendorNum  VendorName          DueDate  " + &
			"InvoiceNum      Not Yet Due      1 to 30    " + &
			" 31 to 60     61 to 90    91 & Over      Balance"
	ELSE
		TITLE$(I% + 1%) = "VendorNum  VendorName          InvDate  " + &
			"InvoiceNum      Not Yet Due      1 to 30    " + &
			" 31 to 60     61 to 90    91 & Over      Balance"
	END IF

	TITLE$(I% + 2%) = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AP_VENDOR.CH%, KEY #K_NUM%
		ELSE
			FIND #AP_VENDOR.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitTotal IF ERR = 155%
		FILENAME$ = "AP_VENDOR"
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
		GET #AP_VENDOR.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Check current record
	!
	SELECT SORTBY$

	CASE "NU"
		GOTO ExitTotal IF (AP_VENDOR::VENNUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(AP_VENDOR::VENNUM, -1%), &
			WLDCRD$) = 0%

	CASE "NA"
		GOTO ExitTotal IF (AP_VENDOR::VENNAM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(AP_VENDOR::VENNAM, -1%), &
			WLDCRD$) = 0%

	CASE "S"
		GOTO ExitTotal IF (AP_VENDOR::ALPSRT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(AP_VENDOR::ALPSRT, -1%), &
			WLDCRD$) = 0%

	END SELECT

	!
	! Get ready to start a sub-loop
	!
17100	WHEN ERROR IN
		FIND #AP_OPEN.CH%, KEY #0% EQ AP_VENDOR::VENNUM, REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155%
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

	TEXT$ = AP_VENDOR::VENNUM + " " + LEFT(AP_VENDOR::VENNAM, 19%) + " "
	TRANKEY$ = "ZZZZZZZZZZZZZZZZZ"
	INVKEY$ = ""

	AGE_BAL(LOOP%), VENDOR_TOTAL(LOOP%) = 0.0 &
		FOR LOOP% = 0% TO 5%

	LINE_HAS_BEEN_PRINTED% = 0%
	PRINT_LINE% = 0%

	!
	! Sub-loop starts here
	!
 GetOpen:
17120	WHEN ERROR IN
		GET #AP_OPEN.CH%, REGARDLESS
	USE
		CONTINUE ExitOpen IF ERR = 11%
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

	GOTO ExitOpen IF AP_VENDOR::VENNUM <> AP_OPEN::VENNUM

	!
	! Skip record if past cutoff date
	!
	GOTO GetOpen IF (CUTOFF_DATE$ <> "") AND &
		(CUTOFF_DATE$ < LEFT(AP_OPEN::UPDATED, 6%))

	IF AP_OPEN::TRANKEY <> TRANKEY$
	THEN
		!
		! Print info before screwing around with the titles
		!
		IF PRINT_LINE%
		THEN
			GOSUB PrintLine
			PRINT_LINE% = 0%
		END IF

		IF DATE_TO_USE$ = "D"
		THEN
			AGE_DAY% = BASE_DAY% - DATE_DAYCODE(AP_OPEN::DUEDAT)
			AGE_DAY$ = PRNT_DATE(AP_OPEN::DUEDAT, 6%)
		ELSE
			AGE_DAY% = BASE_DAY% - DATE_DAYCODE(AP_OPEN::INVDAT)
			AGE_DAY$ = PRNT_DATE(AP_OPEN::INVDAT, 6%)
		END IF

		!
		! Calculate Aging Period
		!
		IF AGE_DAY% < 0%
		THEN
			AGE_INTRVL% = 0%
		ELSE
			AGE_INTRVL% = ((AGE_DAY% + 1%) / 30%) + 1%
			AGE_INTRVL% = 4% IF AGE_INTRVL% > 4%
		END IF

	END IF

	TRANKEY$ = AP_OPEN::TRANKEY
	INVKEY$ = AP_OPEN::INVNUM

	AGE_BAL(AGE_INTRVL%) = FUNC_ROUND(AGE_BAL(AGE_INTRVL%) + &
		(AP_OPEN::INVAMT - AP_OPEN::DISAMT) - AP_OPEN::CKAMT, 2%)

	PRINT_LINE% = -1%

	GOTO GetOpen

	!
	! Exit from Sub-loop; print out Vendor totals
	!
 ExitOpen:
	GOSUB PrintLine IF PRINT_LINE%

	GOSUB VendorTotal
	GOTO GetNextRec

	%PAGE

	!******************************************************************
	! Handle totals and other items before EXITing
	!******************************************************************

 ExitTotal:
	!
	! Print out totals
	!
	TEXT$ = "          Grand Total" + SPACE$(33%)

	FOR LOOP% = 0% TO 5%

		IF TOTAL(LOOP%) <> 0.0
		THEN
			TEXT$ = TEXT$ + FORMAT$(TOTAL(LOOP%), " #,###,###.##")
		ELSE
			TEXT$ = TEXT$ + SPACE$(13%)
		END IF

	NEXT LOOP%

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

 PrintLine:
	!******************************************************************
	! Subroutine to print one line of the report
	!******************************************************************

	AGE_BAL(5%) = 0.0

	AGE_BAL(5%) = FUNC_ROUND(AGE_BAL(5%) + AGE_BAL(LOOP%), 2%) &
		FOR LOOP% = 0% TO 4%

	IF (AGE_BAL(5%) <> 0.0) OR (ZERO_BALANCE$ = "Y") OR (AGE_BAL(0%) <> 0.0)
	THEN
		TEXT$ = TEXT$ + AGE_DAY$ + " " + LEFT(INVKEY$, 13%) + " "

		FOR LOOP% = 0% TO 4%

			IF AGE_BAL(LOOP%) <> 0.0
			THEN
				TEXT$ = TEXT$ + &
					FORMAT$(AGE_BAL(LOOP%), " #,###,###.##")
			ELSE
				TEXT$ = TEXT$ + SPACE$(13%)
			END IF

		NEXT LOOP%

		TEXT$ = TEXT$ + FORMAT$(AGE_BAL(5%), " #,###,###.##")

		VENDOR_TOTAL(LOOP%) = &
			FUNC_ROUND(VENDOR_TOTAL(LOOP%) + AGE_BAL(LOOP%), 2%) &
			FOR LOOP% = 0% TO 5%

		LINE_HAS_BEEN_PRINTED% = -1%

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = SPACE$(31%)

	END IF

	AGE_BAL(LOOP%) = 0.0 FOR LOOP% = 0% TO 5%

 ComeBack1:
	RETURN

	%PAGE

 VendorTotal:
	!******************************************************************
	! Subroutine to print AP Vendor totals
	!******************************************************************
	VENDOR_TOTAL_TEST% = 0%

	VENDOR_TOTAL_TEST% = -1% &
		IF FUNC_ROUND(VENDOR_TOTAL(LOOP%), 2%) <> 0.0 &
		FOR LOOP% = 0% TO 4%

	GOTO ComeBack2 &
		IF (VENDOR_TOTAL_TEST% = 0%) AND &
		(LINE_HAS_BEEN_PRINTED% = 0%)

	VENDOR_TOTAL(5%) = 0.0

	VENDOR_TOTAL(5%) = FUNC_ROUND(VENDOR_TOTAL(5%) + &
		VENDOR_TOTAL(LOOP%), 2%) &
		FOR LOOP% = 0% TO 4%

	TEXT$ = "     Vendor Total" + SPACE$(37%)

	FOR LOOP% = 0% TO 4%

		IF VENDOR_TOTAL(LOOP%) <> 0.0
		THEN
			TEXT$ = TEXT$ + &
				FORMAT$(VENDOR_TOTAL(LOOP%), " #,###,###.##")
		ELSE
			TEXT$ = TEXT$ + SPACE$(13%)
		END IF

	NEXT LOOP%

	TEXT$ = TEXT$ + FORMAT$(VENDOR_TOTAL(5%), " #,###,###.##")

	TOTAL(LOOP%) = FUNC_ROUND(TOTAL(LOOP%) + VENDOR_TOTAL(LOOP%), 2%) &
		FOR LOOP% = 0% TO 5%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ComeBack2 IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

 ComeBack2:
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
	! Handle untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32767	!******************************************************************
	! End of report AP_RPRT_AGE_DETNOTDUE
	!******************************************************************
	END
