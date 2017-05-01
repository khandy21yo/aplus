1	%TITLE "Accounts Receivable Detailed Aging Report"
	%SBTTL "AR_RPRT_AGE_DETAIL"
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
	! ID:AR003
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Accounts Receivable Detailed Aging Report\* option
	!	prints an Accounts Receivable aging report. Using the
	!	report settings, a base is selected from which aging is calculated.
	!	The report may also be set to print accounts in order of location,
	!	customer number, or customer name.
	!	.b
	!	^*Note:\* If items are not aging as you think they should, the
	!	most common problems are the cutoff date in the register
	!	and the open item/balance forward in the customer file (in balance forward,
	!	anything in the register is current, whatever it is dated).
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
	! Author:
	!
	!	09/10/91 - Dan Perkins
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_RPRT_AGE_DETAIL/LINE
	!	$ LINK/EXECUTABLE=AR_EXE:*.EXE AR_RPRT_AGE_DETAIL, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_RPRT_AGE_DETAIL.OBJ;*
	!
	! Modification history:
	!
	!	10/03/91 - Kevin Handy
	!		Modified so that AR_CUSBAL error is handled
	!		better.
	!
	!	03/11/92 - Dan Perkins
	!		Print each invoice to account for multiple dates
	!		of same invoice.  Added FUTURE column.
	!
	!	03/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	03/13/92 - Frank F. Starman
	!		Allow to have more records in the register with
	!		the type 01 for the same invoice. Move credits
	!		to the current.
	!
	!	03/22/92 - Kevin Handy
	!		Clean up (check)
	!
	!	05/04/92 - Dan Perkins
	!		Add future to AR balance.
	!
	!	06/16/92 - Kevin Handy
	!		Clean up (check)
	!
	!	07/02/92 - Dan Perkins
	!		Display last payment and last payment amount.
	!
	!	08/14/92 - Kevin Handy
	!		Corrected error trapping for closed file.
	!
	!	08/19/92 - Dan Perkins
	!		Modified to print only past due invoices if desired.
	!
	!	09/21/92 - Kevin Handy
	!		Clean up (check)
	!
	!	10/19/92 - Dan Perkins
	!		Print future summary at end of report.
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/09/92 - Dan Perkins
	!		Combined fields to print "Past Due" or "Futures".
	!		Print invoice information if we print futures.
	!
	!	11/17/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	02/10/93 - Dan Perkins
	!		Added option to age by TRADAT, DUEDATE, or
	!		DISCOUNTDATE.  Added contact and terms information
	!		to report header.  Added either a due date or
	!		discount date column to the report depending on
	!		user selection.
	!
	!	02/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/14/93 - Kevin Handy
	!		Added REGARDLESS and CLOSE to AR_CONTROL.
	!
	!	06/25/93 - Kevin Handy
	!		Modified so that aging by due date will age the
	!		same way as all other aging functions.
	!
	!	07/02/93 - Kevin Handy
	!		Modified so that aging by due date will age the
	!		same way as all other aging functions.
	!
	!	07/26/93 - Kevin Handy
	!		Fixed bug in changing to match AR_FUNC_AGE
	!		(the 07/02/93 change).
	!
	!	01/13/94 - Kevin Handy
	!		Modified to display cutoff date in title.
	!
	!	01/14/94 - Kevin Handy
	!		Modified to use "age by date" in the future
	!		summary (invoice, discount, due).
	!
	!	01/26/94 - Kevin Handy
	!		Modified to display "cutoff period" in period
	!		format instead of a funny date format.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/25/95 - Kevin Handy
	!		Reformat closer to 80 columns.
	!
	!	05/26/95 - Kevin Handy
	!		Rewrite sections to loose the "two passes"
	!		using massive numbers of interlocked flags.
	!
	!	05/30/95 - Kevin Handy
	!		Modifications to pull up due date based on what
	!		has been paid.
	!
	!	05/31/95 - Kevin Handy
	!		Modifications to make due date code work correctly.
	!
	!	09/15/95 - Kevin Handy
	!		Added a processing message so I could see where
	!		Double L is locking up at.
	!
	!	12/04/95 - Kevin Handy
	!		Lose commented out code.
	!
	!	12/04/96 - Kevin Handy
	!		Changed PTDSALES to LAST_PAID.
	!
	!	06/19/96 - Kevin Handy
	!		Add new transaction type "11", adjustments.
	!		Reformat source code.
	!
	!	09/24/96 - Kevin Handy
	!		Added fast/slow option, to make report run faster
	!		for Double L.
	!
	!	10/01/96 - Kevin Handy
	!		More modifications trying to speed up this pig
	!		for Double L.
	!		1. Modify IF statements for wildcard GL to check
	!		for '*' specially.
	!		2. Create variable AGE_DATE% so don't have to keep
	!		recalculating it.
	!		3. ANYTHING_FOUND% flag added to skip second
	!		search when first one didn't turn up anything.
	!
	!	04/07/97 - Kevin Handy
	!		Ability to sort by salesman number.
	!
	!	04/09/97 - Kevin Handy
	!		Increase number of digits on future summary.
	!
	!	04/11/97 - Kevin Handy
	!		Fix so that doesn't print blank salesman
	!		groupings.
	!
	!	04/16/97 - Kevin Handy
	!		Fix so doesn't page between salesman unless
	!		printing by salesman
	!
	!	04/21/97 - Kevin Handy
	!		Fix bug where only recognized '01' as an
	!		invoice header, when there may be '02' '03',
	!		or '04' as invoice dates.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/14/98 - Kevin Handy
	!		Fix problem with balance forward customers. (FJ)
	!
	!	02/12/99 - Kevin Handy
	!		Check for enough lines for some detail to show
	!		under the customer name.
	!
	!	09/17/99 - Kevin Handy
	!		Added ability to have a summary only printout
	!
	!	06/05/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/20/2000 - Kevin Handy
	!		Use A"x"B
	!
	!	12/26/2000 - Kevin Handy
	!		Add code for AGEFORMULA to handle different aging
	!		formulas.
	!
	!	01/03/2000 - Kevin Handy
	!		Zero out TOPAY value in spread out loop so it
	!		doesn't apply multiple tiles.
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

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTACT.HB"
	MAP (AR_CONTACT)	AR_CONTACT_CDD		AR_CONTACT

	%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.HB"
	MAP (AR_CUSBAL)		AR_CUSBAL_CDD		AR_CUSBAL
	DECLARE			AR_CUSBAL_CDD		AR_TEMP

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD		AR_CONTROL

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN)		AR_OPEN_CDD		AR_OPEN

	%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.HB"
	MAP (AR_CLOSED)		AR_CLOSED_CDD		AR_CLOSED

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TERMS.HB"
	MAP (UTL_TERMS)		UTL_TERMS_CDD		UTL_TERMS

	RECORD FUTURE_RECORD
		STRING FUT_DATE = 4%, &
		REAL   FUT_AMT
	END RECORD

	DIM FUTURE_RECORD	FUTURE(50%)

	RECORD DUEDATE_RECORD
		STRING	DUEDATE = 8%
		REAL	AMOUNT
	END RECORD

	DIM DUEDATE_RECORD DUEDATE_LIST(100%)
	DECLARE DUEDATE_RECORD DUEDATE_TEMP

	!
	! Initialize variables
	!
	GRANDTOTAL(I%) = 0.0 FOR I% = 0% TO 4%
	GRANDTOTAL_CHARGE = 0.0
	GRANDTOTAL_BALANCE = 0.0
	GRANDTOTAL_FUTURE = 0.0
	TOTAL_FUTURE = 0.0

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
	!	to begin with the selected Item _#. The
	!	value must be in agreement with field (03) Sort by.
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
	!	A ^*To Item _#\* entered in this field causes the printing
	!	to end with the selected Item _#. The
	!	value must be in agreement with field (03) Sort by.
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
	GL_WILDCARD$ = "*" IF GL_WILDCARD$ = ""

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
	!	.x Print>Zero
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
	!	.x Zero>Balance
	!	.x Balance>Zero
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	.x Sort by
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
	!
	!--

	AGE_DATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(5%), -1%))
	AGE_DATE% = DATE_DAYCODE(AGE_DATE$)

	!++
	! Abstract:FLD06
	!	.x Age>Date
	!	^*(06) Age as of date\*
	!	.b
	!	.lm +5
	!	The ^*Age as of date\* field enters
	!	the base date from which account balances will be aged.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Date>Age
	!
	!--

	DATE_OPTION$ = EDIT$(UTL_REPORTX::OPTDEF(6%), -1%)

	!++
	! Abstract:FLD07
	!	^*(07) Date Option\*
	!	.b
	!	.lm +5
	!	The ^*Date Option\* field
	!	determines the type of date to be used in aging the report.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*I\* - Age by Invoice Date
	!	.te
	!	^*D\* - Age by Due Date
	!	.te
	!	^*T\* - Age by Discount Date
	!	.end table
	!	A setting is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	CUTOFF$ = EDIT$(UTL_REPORTX::OPTDEF(7%), -1%)

	!++
	! Abstract:FLD08
	!	.x Cutoff>Period
	!	^*(08) Cutoff Period\*
	!	.b
	!	.lm +5
	!	The ^*Cutoff Period\* will be entered with the accounting period corresponding
	!	to the statement date. No transactions beyond the date entered
	!	will appear.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!	.x Period>Cutoff
	!
	!--

	OPTIONS$ = LEFT$(UTL_REPORTX::OPTDEF(8%), 1%)
	AGEFORMULA$ = MID(UTL_REPORTX::OPTDEF(8%), 2%, 1%)

	!++
	! Abstract:FLD09
	!	^*(09) Report Options\*
	!	.b
	!	.lm +5
	!	The ^*Report Options\* field
	!	determines how the report will be printed.
	!	The setting consists of two characters, the first
	!	determines the report format, and the second
	!	determines how the aging is handled.
	!	.b
	!	Valid settings for the first character are:
	!	.table 3,25
	!	.te
	!	^*F\* - Futures Only
	!	.te
	!	^*P\* - Past Due Aging Only
	!	.te
	!	^*T\* - Total Aging Report
	!	.end table
	!	.b
	!	Valid settings for the second character are:
	!	.table 3,25
	!	.te
	!	*1 - Age items based on the payment date.
	!	It doesn't try to match payments to the parts of the
	!	invoice.
	!	This is the default if a bad or undefined
	!	setting is used.
	!	.te
	!	*2 - Apply payments to service charge first, then
	!	apply to oldest invoice parts, and leave any
	!	overpayments in the current column.
	!	Future amounts do not get applied to.
	!	Payments won't be applied to other invoices.
	!	.end table
	!	A setting is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	FAST$ = LEFT$(UTL_REPORTX::OPTDEF(9%), 1%)

	!++
	! Abstract:FLD10
	!	^*(10) Fast/Slow\*
	!	.b
	!	.lm +5
	!	This determines if the program should take shortcuts
	!	to arrive at various computations, such as the last
	!	payment date, or if it should do the longer (more accurate)
	!	methods. Note that the aging amounts will not change.
	!	.B
	!	The available options for this field are:
	!	.list 0,"*"
	!	.le
	!	*V Very fast. never look into the closed file for last payment date.
	!	.le
	!	*F Fast. Only look in the closed file for the last payment date when
	!	no payments were found in the open file
	!	.le
	!	*S Slow. Always look in the closed file for the last payment date.
	!	.le
	!	*T Totals only. Like "V", but doesn't display any detail,
	!	only the totals will be shown.
	!	.els
	!	.lm -5
	!
	! Index:
	!
	!--

300	!
	! Open Customer file
	!
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

340	!
	! Open open itemfile
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

350	!
	! Open open CONTACT file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTACT.OPN"
	USE
		CONTINUE 360 IF ERR = 5%
		FILENAME$ = "AR_CONTACT"
		CONTINUE HelpError
	END WHEN

360	!
	! Open open TERMS file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_TERMS.OPN"
	USE
		CONTINUE 400 IF ERR = 5%
		FILENAME$ = "UTL_TERMS"
		CONTINUE HelpError
	END WHEN

400	!
	! Generate a sort file if necessary
	!
	GOTO ReportTitle UNLESS SORTBY$ = "SA"

	CALL ENTR_3MESSAGE(SCOPE, "Generating Sort File", 1)

	!======================================================================
	! AR_35CUSTOM file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AR_35CUSTOM_SORT.CH%, STAT%)
	AR_35CUSTOM_SORT.NAME$ = AR_35CUSTOM.DEV$ + "AR_35CUSTOM.TEMP"

	WHEN ERROR IN
		OPEN AR_35CUSTOM_SORT.NAME$ FOR OUTPUT AS FILE AR_35CUSTOM_SORT.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP AR_35CUSTOM, &
			TEMPORARY, &
			EXTENDSIZE 48%, &
			PRIMARY KEY &
			( &
				AR_35CUSTOM::SALESMAN, &
				AR_35CUSTOM::CUSNUM &
			), &
			ACCESS MODIFY, ALLOW NONE
	USE
		FILENAME$ = "SORT_FILE"
		CONTINUE HelpError
	END WHEN

410	!
	! Loop through customer file, pulling off only those
	! customers that we want to print and generating a sort
	! file with the same record layout as the customer file.
	!
	WHEN ERROR IN
		RESET #AR_35CUSTOM.CH%
	USE
		CONTINUE 490
	END WHEN

420	WHEN ERROR IN
		GET #AR_35CUSTOM.CH%, REGARDLESS
	USE
		CONTINUE 490
	END WHEN

430	IF (AR_35CUSTOM::SALESMAN >= FROM_ITEM$) AND &
		((TO_ITEM$ = "") OR (AR_35CUSTOM::SALESMAN <= TO_ITEM$))
	THEN
		WHEN ERROR IN
			PUT #AR_35CUSTOM_SORT.CH%
		USE
			FILENAME$ = "SORT_FILE"
			CONTINUE HelpError
		END WHEN
	END IF

	GOTO 420

490	CLOSE AR_35CUSTOM.CH%

	CALL ASSG_FREECHANNEL(AR_35CUSTOM.CH%)

	AR_35CUSTOM.CH% = AR_35CUSTOM_SORT.CH%

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "DETAILED AGING REPORT FOR " + PRNT_DATE(AGE_DATE$, 8%)

	SELECT DATE_OPTION$

	CASE "I"
		TITLE$(2%) = "AGED BY INVOICE DATE"

	CASE "D"
		TITLE$(2%) = "AGED BY DUE DATE"

	CASE "T"
		TITLE$(2%) = "AGED BY DISCOUNT DATE"

	END SELECT

	IF CUTOFF$ <= "000000"
	THEN
		TITLE$(3%) = "AR System"
	ELSE
		TITLE$(3%) = "Cutoff Period " + CUTOFF$
	END IF

	TITLE$(4%) = ""

	TITLE$(5%) = LEFT(AR_CONTROL::CTITLE, 10%) + &
		"   Name                                              " + &
		"  CustType CustCat  Salesman   Phone          " + &
		"LstPmtDate    LstPmtAmt"

	TITLE$(6%) = SPACE$(16%) + &
		"Contact                         " + &
		"Title                 Phone          " + &
		"Ext.  Terms  Description"

	IF DATE_OPTION$ <> "T"
	THEN
		TITDATE$ = "DueDate  "
	ELSE
		TITDATE$ = "DisDate  "
	END IF

	INTRVL$(I%) = LEFT(SPACE$(11% - &
		LEN(EDIT$(AR_CONTROL::AGENAM(I%), 140%))) + &
		EDIT$(AR_CONTROL::AGENAM(I%), 140%), 11%) &
		FOR I% = 0% TO 4%

	SELECT OPTIONS$

	CASE "F"
		TITLE$(7%) = "    Inv #     InvDate  " + TITDATE$ + &
			"     Future"

	CASE "P"
		TITLE$(7%) = "    Inv #     InvDate  "  + TITDATE$ + &
			SPACE$(LEN(INTRVL$(0%))) + " " + INTRVL$(1%) + " " + &
			INTRVL$(2%) + " " + INTRVL$(3%) + " " + &
			INTRVL$(4%) + "  ServCharge     Balance"

	CASE "T"
		TITLE$(7%) = "    Inv #     InvDate  "  + TITDATE$ + &
			INTRVL$(0%) + " " + INTRVL$(1%) + " " + &
			INTRVL$(2%) + " " + INTRVL$(3%) + " " + &
			INTRVL$(4%) + "  ServCharge     Balance      Future"

	END SELECT

	TITLE$(8%) = "."

	SELECT SORTBY$

	CASE "N"
		K_NUM% = 0%

	CASE "T"
		K_NUM% = 1%

	CASE "C"
		K_NUM% = 2%

	CASE "A"
		K_NUM% = 3%

	CASE "SA"
		K_NUM% = 0%
	END SELECT

	%PAGE

	ITEM_LOOP% = 0%

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
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	LAST_SALESMAN$ = "~~~~~~~~~~~"
	SALESMAN_COUNT% = 0%

 GetCustomerRecord:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	LAST_INVNUM$ = STRING$((LEN(AR_OPEN::INVNUM) + 1%), A"?"B)
	LAST_TRADAT$ = STRING$((LEN(AR_OPEN::TRADAT) + 1%), A"?"B)

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
	!	(Don't need to check SA sort again)
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
	! See if we can find a contact for this customer
	!
	AR_CONTACT::CONTACT_NAME = ""
	AR_CONTACT::TITLE        = ""
	AR_CONTACT::PHONE        = ""
	AR_CONTACT::EXTENSION    = ""

	CALL ENTR_3MESSAGE(SCOPE, "Processing " + AR_35CUSTOM::CUSNUM, 1%)

17050	WHEN ERROR IN
		GET #AR_CONTACT.CH%, &
			KEY #0% EQ AR_35CUSTOM::CUSNUM, &
			REGARDLESS
	USE
		CONTINUE 17060 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AR_CONTACT"
		CONTINUE HelpError
	END WHEN

	!
	! See if we can find the terms description
	!
17060	IF TRM$(AR_35CUSTOM::TERMS) <> ""
	THEN
		UTL_TERMS::DESCR = ""

		WHEN ERROR IN
			GET #UTL_TERMS.CH%, &
				KEY #0% EQ AR_35CUSTOM::TERMS, &
				REGARDLESS
		USE
			CONTINUE 17100 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "UTL_TERMS"
			CONTINUE HelpError
		END WHEN
	END IF

17100	!*******************************************************************
	! Pass 1: Look up the last payment and last payment date for all
	! of the customers records. This is for the customer title line.
	!*******************************************************************

	GOTO ExitProgram IF UTL_REPORTX::STAT

	ANYTHING_FOUND% = 0%
	LAST_PAYMENT$ = ""
	LAST_PAYAMT = 0.0

	!
	! Initialize the aging information
	!
	IF AR_35CUSTOM::METHOD = "B"
	THEN
		WHEN ERROR IN
			GET #AR_CUSBAL.CH%, &
				KEY #0% EQ AR_35CUSTOM::CUSNUM, &
				REGARDLESS
		USE
			CONTINUE 17110
		END WHEN

		LAST_INVNUM$ = "Bal Fwd "
		LAST_TRADAT$ = ""
		LAST_PAYMENT$ = AR_CUSBAL::LAST_PAYMENT + ""
		AR_TEMP::AGING(I%) = AR_CUSBAL::AGING(I%) FOR I% = 0% TO 4%
		AR_TEMP::CHARGE = AR_CUSBAL::CHARGE
	ELSE
		!
		! Initialize invoice record
		!
		AR_TEMP::CUSNUM = ""
		AR_TEMP::ACCT = ""
		AR_TEMP::CREDIT = 0.0
		AR_TEMP::FUTURE = 0.0
		AR_TEMP::AGING(X%) = 0.0 FOR X% = 0% TO 4%
		AR_TEMP::YTDSERVICE = 0.0
		AR_TEMP::LAST_PAID = 0.0
		AR_TEMP::YTDSALES = 0.0
		AR_TEMP::CHARGE = 0.0
		AR_TEMP::LAST_CHARGE = ""
		AR_TEMP::LAST_PAYMENT = ""
		AR_TEMP::LAST_UPDATE = ""
	END IF

17110	WHEN ERROR IN
		FIND #AR_OPEN.CH%, KEY #0% EQ AR_35CUSTOM::CUSNUM, REGARDLESS
	USE
		CONTINUE 17150 IF ERR = 11% OR ERR = 9% OR ERR = 155%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

17120	!
	! Get AR_OPEN record
	!
	WHEN ERROR IN
		GET #AR_OPEN.CH%, REGARDLESS
	USE
		CONTINUE 17150 IF ERR = 11% OR ERR = 9% OR ERR = 155%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	!
	! Skip out if done with this customer number
	!
	GOTO 17120 IF AR_OPEN::CUSNUM <> AR_35CUSTOM::CUSNUM

	!
	! Ignore item if update date is too late
	!
	GOTO 17120 &
		IF (LEFT(AR_OPEN::UPDATED, 6%) > LEFT(CUTOFF$, 6%)) &
		AND (CUTOFF$ > "00000000")

	IF (GL_WILDCARD$ <> "*")
	THEN
		GOTO 17120 &
			IF COMP_STRING(EDIT$(AR_OPEN::ARACCT, -1%), &
				GL_WILDCARD$) = 0%
	END IF

	ANYTHING_FOUND% = -1%

	SELECT AR_OPEN::TRATYP

	CASE "09", "10", "11"
		IF LAST_PAYMENT$ = AR_OPEN::TRADAT
		THEN
			LAST_PAYAMT = LAST_PAYAMT - AR_OPEN::SALAMT
		END IF

		IF LAST_PAYMENT$ < AR_OPEN::TRADAT
		THEN
			LAST_PAYMENT$ = AR_OPEN::TRADAT + ""
			LAST_PAYAMT = -AR_OPEN::SALAMT
		END IF
	END SELECT

	GOTO 17120

	!
	! Check the CLOSED file for any payments.
	! We're just looking for the last payment date and amount here.
	!
17150	!
	! Skip ckecking for last payment date if we are running fast,
	! and have any payment date already.
	!

	!
	! Skip out if no records found in open file
	!
	GOTO 17300 IF ANYTHING_FOUND% = 0%

	GOTO 17200 IF (FAST$ = "V") OR (FAST$ = "T")
	GOTO 17200 IF (FAST$ = "F") AND (LAST_PAYAMT$ > "00000000")

	WHEN ERROR IN
		FIND #AR_CLOSED.CH%, KEY #0% EQ AR_35CUSTOM::CUSNUM, REGARDLESS
	USE
		CONTINUE 17200 IF ERR = 9% OR ERR = 11% OR ERR = 155%
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

17160	WHEN ERROR IN
		GET #AR_CLOSED.CH%, REGARDLESS
	USE
		CONTINUE 17200 IF ERR = 9% OR ERR = 11% OR ERR = 155%
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

	GOTO 17200 IF AR_CLOSED::CUSNUM <> AR_35CUSTOM::CUSNUM

	SELECT AR_CLOSED::TRATYP

	CASE "09", "10", "11"
		IF LAST_PAYMENT$ = AR_CLOSED::TRADAT
		THEN
			LAST_PAYAMT = LAST_PAYAMT - AR_CLOSED::SALAMT
		END IF

		IF LAST_PAYMENT$ < AR_CLOSED::TRADAT
		THEN
			LAST_PAYMENT$ = AR_CLOSED::TRADAT + ""
			LAST_PAYAMT = -AR_CLOSED::SALAMT
		END IF
	END SELECT

	GOTO 17160

17200	!*******************************************************************
	! Pass 2: Invoice loop starts here
	!*******************************************************************

	DUEDATE_LIST% = 0%
	DUEDATE_PAID = 0.0
	TOPAY = 0.0

	!
	! Search for first invoice for this customer
	!
	WHEN ERROR IN
		FIND #AR_OPEN.CH%, KEY #0% EQ AR_35CUSTOM::CUSNUM, REGARDLESS
	USE
		CONTINUE GetCustomerRecord IF ERR = 155%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

 GetInvoiceRecord:
17220	!
	! Get AR_OPEN record
	!
	WHEN ERROR IN
		GET #AR_OPEN.CH%, REGARDLESS
	USE
		CONTINUE 17300 IF ERR = 11%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	!
	! Skip out if done with this customer number
	!
	GOTO 17300 IF AR_OPEN::CUSNUM <> AR_35CUSTOM::CUSNUM

	!
	! Ignore item if update date is too late
	!
	GOTO GetInvoiceRecord &
		IF (LEFT(AR_OPEN::UPDATED, 6%) > LEFT(CUTOFF$, 6%)) &
		AND (CUTOFF$ > "00000000")

	IF GL_WILDCARD$ <> "*"
	THEN
		GOTO GetInvoiceRecord IF &
			COMP_STRING(EDIT$(AR_OPEN::ARACCT, -1%), &
			GL_WILDCARD$) = 0%
	END IF

	AMOUNT = AR_OPEN::SALAMT
	AMOUNT = 0.0 IF AR_OPEN::TRATYP = "02"

	!
	! Handle open item (O)
	!
	IF AR_OPEN::INVNUM <> LAST_INVNUM$ OR &
		AR_OPEN::TRATYP = "01" OR &
		AR_OPEN::TRATYP = "02" OR &
		AR_OPEN::TRATYP = "03" OR &
		AR_OPEN::TRATYP = "04"
	THEN
		GOSUB PrintCustomer IF AR_OPEN::INVNUM <> LAST_INVNUM$ &
			AND LAST_INVNUM$ <> &
			STRING$((LEN(AR_OPEN::INVNUM) + 1%), A"?"B)

		!
		! Zip out duedate list
		!
		IF AR_OPEN::INVNUM <> LAST_INVNUM$
		THEN
			DUEDATE_LIST% = 0%
			DUEDATE_PAID = 0.0
		END IF

		!
		! See how we want to age this report
		!
		! Default the date to the TRADAT which is the invoice date
		!
		DUEDATE$ = AR_OPEN::TRADAT
		AGE.DAY% = AGE_DATE% - DATE_DAYCODE(DUEDATE$)

		SELECT DATE_OPTION$

		CASE "D"
			IF AR_OPEN::DUEDATE <> ""
			THEN
				DUEDATE$ = AR_OPEN::DUEDATE
				AGE.DAY% = AGE_DATE% - DATE_DAYCODE(DUEDATE$) + &
					AR_CONTROL::AGEPER(0%)
			END IF

		CASE "T"
			IF AR_OPEN::DISCOUNTDATE <> ""
			THEN
				DUEDATE$ = AR_OPEN::DISCOUNTDATE
				AGE.DAY% = AGE_DATE% - DATE_DAYCODE(DUEDATE$)
			END IF

		END SELECT

		!
		! Test for future date
		!
		IF AGE.DAY% < 0%
		THEN
			AGEFLAG% = 1%
		ELSE
			!
			! If it is service charge, then handle it properly
			!
			IF AR_OPEN::TRATYP = "04"
			THEN
				AGEFLAG% = 2%
			ELSE
				!
				! Calculate aging period
				!
				AGEDAY% = AR_CONTROL::AGEPER(0%)
				D% = 0%

				WHILE (AGE.DAY% > AGEDAY%) AND (D% < 3%)
					D% = D% + 1%
					AGEDAY% = AGEDAY% + &
						AR_CONTROL::AGEPER(D%)
				NEXT

				D% = 4% IF AGE.DAY% > AGEDAY%
				AGEFLAG% = 3%
			END IF
		END IF

		!
		! If this is the first invoice with this number, handle
		! the switchover.
		!
		IF AR_OPEN::INVNUM <> LAST_INVNUM$
		THEN
			LAST_TRADAT$ = AR_OPEN::TRADAT
			LAST_INVNUM$ = AR_OPEN::INVNUM

			!
			! Remember the 1st invoice date
			!
			FIRST.D%     = D%
			FIRST.AGE%   = AGEFLAG%
		END IF
	ELSE
		!
		! Make anything else age as of the 1st invoice date,
		! not the last one.
		!
		AGEFLAG% = FIRST.AGE%
		D% = FIRST.D%
	END IF

	!
	! Handle adding one invoice item
	!
	SELECT AGEFLAG%

	!
	! Future
	!
	CASE 1%
		AR_TEMP::FUTURE = AR_TEMP::FUTURE + AMOUNT

		!
		! Figure out this future date array junk
		!
		FUT.DATE$ = MID(DUEDATE$, 3%, 4%)

		FOR I% = 1% TO ITEM_LOOP%

			GOTO ExitLoop IF FUT.DATE$ = &
				FUTURE(I%)::FUT_DATE

		NEXT I%

		ITEM_LOOP%, I% = ITEM_LOOP% + 1%
		FUTURE(ITEM_LOOP%)::FUT_DATE = FUT.DATE$
		FUTURE(ITEM_LOOP%)::FUT_AMT  = 0.0
 ExitLoop:
		FUTURE(I%)::FUT_AMT = FUTURE(I%)::FUT_AMT + AMOUNT

	!
	! Service Charge
	!
	CASE 2%
		AR_TEMP::CHARGE = AR_TEMP::CHARGE + AMOUNT

	!
	! Period
	!
	CASE 3%
		IF AR_OPEN::TRATYP = "09" AND AGEFORMULA$ = "2"
		THEN
			!
			! Type '2' aging requires that payments be handled
			! after we get all of the invoicing options
			!
			TOPAY = FUNC_ROUND(TOPAY + AMOUNT, 2%)
		ELSE
			AR_TEMP::AGING(D%) = AR_TEMP::AGING(D%) + AMOUNT
		END IF

	END SELECT

	!
	! Put in due-date list as necessary
	!
	SELECT AR_OPEN::TRATYP

	CASE "09", "10", "11"
		!
		! Payments
		!
		DUEDATE_PAID = DUEDATE_PAID + AMOUNT

	CASE ELSE
		!
		! Invoice
		!
		IF AMOUNT <> 0.0
		THEN
			DUEDATE_LIST% = DUEDATE_LIST% + 1%
			IF DATE_OPTION$ <> "T"
			THEN
				DUEDATE_LIST(DUEDATE_LIST%)::DUEDATE = &
					AR_OPEN::DUEDATE
			ELSE
				DUEDATE_LIST(DUEDATE_LIST%)::DUEDATE = &
					AR_OPEN::DISCOUNTDATE
			END IF

			IF DUEDATE_LIST(DUEDATE_LIST%)::DUEDATE <= "00000000"
			THEN
				DUEDATE_LIST(DUEDATE_LIST%)::DUEDATE = &
					AR_OPEN::TRADAT
			END IF

			DUEDATE_LIST(DUEDATE_LIST%)::AMOUNT = AMOUNT
		END IF

	END SELECT

	GOTO GetInvoiceRecord

17300	!
	! Finish up last invoice
	!
	IF LAST_INVNUM$ <> STRING$((LEN(AR_OPEN::INVNUM) + 1%), A"?"B)
	THEN
		GOSUB PrintCustomer
		GOSUB CustomerTotal
	END IF

	GOTO GetCustomerRecord

  ExitTotal:
	SELECT OPTIONS$

	CASE "F"
		TEXT$ = "    Grand Total" + SPACE$(17%) + &
			FORMAT$(GRANDTOTAL_FUTURE, "########.##")

	CASE "P"
		TEXT$ = "    Grand Total" + SPACE$(29%) + &
			FORMAT$(GRANDTOTAL(1%), "########.## ") + &
			FORMAT$(GRANDTOTAL(2%), "########.## ") + &
			FORMAT$(GRANDTOTAL(3%), "########.## ") + &
			FORMAT$(GRANDTOTAL(4%), "########.## ") + &
			FORMAT$(GRANDTOTAL_CHARGE, "########.## ") + &
			FORMAT$(GRANDTOTAL_BALANCE, "########.## ")

	CASE "T"
		TEXT$ = "    Grand Total" + SPACE$(17%) + &
			FORMAT$(GRANDTOTAL(0%), "########.## ") + &
			FORMAT$(GRANDTOTAL(1%), "########.## ") + &
			FORMAT$(GRANDTOTAL(2%), "########.## ") + &
			FORMAT$(GRANDTOTAL(3%), "########.## ") + &
			FORMAT$(GRANDTOTAL(4%), "########.## ") + &
			FORMAT$(GRANDTOTAL_CHARGE, "########.## ") + &
			FORMAT$(GRANDTOTAL_BALANCE, "########.## ") + &
			FORMAT$(GRANDTOTAL_FUTURE, "########.##")

	END SELECT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	IF ITEM_LOOP% > 0% AND OPTIONS$ <> "P"
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

		TEXT$ = SPACE$(63%) + "FUTURES"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

		TEXT$ = SPACE$(50%) + "Month" + &
			SPACE$(10%) + "Year"  + &
			SPACE$(14%) + "Amount"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		!
		! Get ready to print out the future array
		!
		! Sort array
		!
		FOR I% = 1% TO ITEM_LOOP% - 1%

			INDEX% = I%
			FUTURE(0%) = FUTURE(I%)

			FOR J% = I% + 1% TO ITEM_LOOP%

				IF FUTURE(J%)::FUT_DATE < FUTURE(0%)::FUT_DATE
				THEN
					FUTURE(0%) = FUTURE(J%)
					INDEX% = J%
				END IF

			NEXT J%

			FUTURE(INDEX%) = FUTURE(I%)
			FUTURE(I%)     = FUTURE(0%)

		NEXT I%

		!
		! Print this mess
		!
		FOR I% = 1% TO ITEM_LOOP%

			SELECT RIGHT(FUTURE(I%)::FUT_DATE, 3%)

			CASE "01"
				MONTH$ = "JAN"

			CASE "02"
				MONTH$ = "FEB"

			CASE "03"
				MONTH$ = "MAR"

			CASE "04"
				MONTH$ = "APR"

			CASE "05"
				MONTH$ = "MAY"

			CASE "06"
				MONTH$ = "JUN"

			CASE "07"
				MONTH$ = "JUL"

			CASE "08"
				MONTH$ = "AUG"

			CASE "09"
				MONTH$ = "SEP"

			CASE "10"
				MONTH$ = "OCT"

			CASE "11"
				MONTH$ = "NOV"

			CASE "12"
				MONTH$ = "DEC"

			CASE ELSE
				MONTH$ = RIGHT(FUTURE(I%)::FUT_DATE, 3%) + " "

			END SELECT

			TEXT$ = SPACE$(50%) + &
				MONTH$ + "  " + &
				SPACE$(10%) + &
				LEFT(FUTURE(I%)::FUT_DATE, 2%) + "  " + &
				SPACE$(6%) + &
				FORMAT$(FUTURE(I%)::FUT_AMT, "###,###,###.##")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

			TOTAL_FUTURE = FUNC_ROUND(TOTAL_FUTURE + &
				FUTURE(I%)::FUT_AMT, 2%)

		NEXT I%

		TEXT$ = SPACE$(50%) + "Future Total" + SPACE$(13%) + &
			FORMAT$(TOTAL_FUTURE, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	END IF

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

	GOTO 32767

	%PAGE

 PrintCustomer:

	!
	! Handle post-processing on the various aging formulas
	!
	SELECT AGEFORMULA$
	CASE "2"
		!
		! Age payments as if they pay the oldest amounts first,
		! rolling back to the most current. Service charge gets
		! handled first though. Anything left over goes into
		! current, since the future invoices haven't happened yet.
		!
		AR_TEMP::CHARGE = &
			FUNC_ROUND(AR_TEMP::CHARGE + &
			TOPAY, 2%)
		TOPAY = 0.0

		IF AR_TEMP::CHARGE < 0.0
		THEN
			TOPAY = AR_TEMP::CHARGE
			AR_TEMP::CHARGE = 0.0
		END IF

		FOR LOOP% = 4% TO 1% STEP -1%

			AR_TEMP::AGING(LOOP%) = &
				FUNC_ROUND(AR_TEMP::AGING(LOOP%) + &
				TOPAY, 2%)
			TOPAY = 0.0

			IF AR_TEMP::AGING(LOOP%) < 0%
			THEN
				TOPAY = AR_TEMP::AGING(LOOP%)
				AR_TEMP::AGING(LOOP%) = 0.0
			END IF
		NEXT LOOP%

		AR_TEMP::AGING(0%) = &
			FUNC_ROUND(AR_TEMP::AGING(0%) + &
			TOPAY, 2%)
		TOPAY = 0.0

	END SELECT

	!
	! Handle various output options
	!
	SELECT OPTIONS$

	CASE "F"
		BALANCE = FUNC_ROUND(AR_TEMP::FUTURE, 2%)

		GOTO InitInv IF ZERO_BALANCE$ <> "Y" AND BALANCE = 0.0

	CASE "P"
		BALANCE = FUNC_ROUND(AR_TEMP::AGING(1%) + &
			AR_TEMP::AGING(2%) + &
			AR_TEMP::AGING(3%) + &
			AR_TEMP::AGING(4%) + &
			AR_TEMP::CHARGE, 2%)

		GOTO InitInv IF ZERO_BALANCE$ <> "Y" AND BALANCE = 0.0

	CASE "T"
		BALANCE = FUNC_ROUND(AR_TEMP::AGING(0%) + &
			AR_TEMP::AGING(1%) + &
			AR_TEMP::AGING(2%) + &
			AR_TEMP::AGING(3%) + &
			AR_TEMP::AGING(4%) + &
			AR_TEMP::CHARGE + &
			AR_TEMP::FUTURE, 2%)

		GOTO InitInv IF ZERO_BALANCE$ <> "Y" AND BALANCE = 0.0 &
			AND AR_TEMP::FUTURE = 0.0

	END SELECT

	IF PRINT_FLAG% = 0%
	THEN
		IF (LAST_SALESMAN$ <> AR_35CUSTOM::SALESMAN) AND &
			(SORTBY$ = "SA")
		THEN
			GOSUB SalesmanTotal UNLESS SALESMAN_COUNT% = 0%
			SALESMAN_COUNT% = 0%

			GOSUB SalesmanHeader
			LAST_SALESMAN$ = AR_35CUSTOM::SALESMAN
		END IF

		TEXT$ = AR_35CUSTOM::CUSNUM + "   " + &
			AR_35CUSTOM::CUSNAM + "   " + &
			AR_35CUSTOM::TTYPE + "     " + &
			AR_35CUSTOM::CATEGORY + "     " + &
			AR_35CUSTOM::SALESMAN + " " + &
			PRNT_PHONE(AR_35CUSTOM::PHONE, 0%) + "  " + &
			PRNT_DATE(LAST_PAYMENT$, 8%) + "  " + &
			FORMAT$(LAST_PAYAMT, "<%>#######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 5%)

		TEXT$ = SPACE$(16%) + &
			AR_CONTACT::CONTACT_NAME + "  " + &
			AR_CONTACT::TITLE + "  " + &
			PRNT_PHONE(AR_CONTACT::PHONE, 0%) + "  " + &
			AR_CONTACT::EXTENSION + "  " + &
			AR_35CUSTOM::TERMS + "     " + &
			UTL_TERMS::DESCR

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		IF FAST$ <> "T"
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		END IF
		GOTO ExitProgram IF UTL_REPORTX::STAT

		PRINT_FLAG% = -1%
	END IF

	SALESMAN_COUNT% = SALESMAN_COUNT% + 1%

	!
	! Calculate the correct due date for this invoice
	!
	!	1. Sort the list
	!	2. Apply payments until we see positive amount
	!	4. Select that date, or last due date available,
	!		or just grab a date from any invoice.
	!
	FOR XLOOP% = 1% TO DUEDATE_LIST%

		FOR YLOOP% = 1% TO DUEDATE_LIST% - XLOOP%

			IF DUEDATE_LIST(YLOOP%)::DUEDATE > &
				DUEDATE_LIST(YLOOP% + 1%)::DUEDATE
			THEN
				DUEDATE_TEMP = DUEDATE_LIST(YLOOP%)
				DUEDATE_LIST(YLOOP%) = DUEDATE_LIST(YLOOP% + 1%)
				DUEDATE_LIST(YLOOP% + 1%) = DUEDATE_TEMP
			END IF

		NEXT YLOOP%

	NEXT XLOOP%

	FOR XLOOP% = 1% TO DUEDATE_LIST%

		DUEDATE_PAID = FUNC_ROUND(DUEDATE_PAID + &
			DUEDATE_LIST(XLOOP%)::AMOUNT, 2%)

		IF (DUEDATE_PAID > 0.0)
		THEN
			LAST_DUEDAT$ = DUEDATE_LIST(XLOOP%)::DUEDATE
			GOTO ExitDuedateLoop
		END IF

	NEXT XLOOP%

	IF DUEDATE_LIST% > 0%
	THEN
		LAST_DUEDAT$ = DUEDATE_LIST(DUEDATE_LIST%)::DUEDATE
	ELSE
		LAST_DUEDAT$ = LAST_TRADAT$
	END IF

 ExitDuedateLoop:
	!
	! Print result
	!
	IF (LAST_SALESMAN$ <> AR_35CUSTOM::SALESMAN) AND &
		(SORTBY$ = "SA")
	THEN
		GOSUB SalesmanTotal UNLESS SALESMAN_COUNT% = 0%
		SALESMAN_COUNT% = 0%

		GOSUB SalesmanHeader
		LAST_SALESMAN$ = AR_35CUSTOM::SALESMAN
	END IF

	SELECT OPTIONS$

	CASE "F"
		TEXT$ = "    " + &
			LAST_INVNUM$ + "  " + &
			PRNT_DATE(LAST_TRADAT$, 6%) + " " + &
			PRNT_DATE(LAST_DUEDAT$, 6%) + " " + &
			FORMAT$(AR_TEMP::FUTURE, "########.##")

	CASE "P"
		TEXT$ = "    " + &
			LAST_INVNUM$ + "  " + &
			PRNT_DATE(LAST_TRADAT$, 6%) + " "  + &
			PRNT_DATE(LAST_DUEDAT$, 6%) + SPACE$(13%) + &
			FORMAT$(AR_TEMP::AGING(1%), "########.## ") + &
			FORMAT$(AR_TEMP::AGING(2%), "########.## ") + &
			FORMAT$(AR_TEMP::AGING(3%), "########.## ") + &
			FORMAT$(AR_TEMP::AGING(4%), "########.## ") + &
			FORMAT$(AR_TEMP::CHARGE, "########.## ") + &
			FORMAT$(BALANCE, "########.## ")

	CASE "T"
		TEXT$ = "    " + &
			LAST_INVNUM$ + "  " + &
			PRNT_DATE(LAST_TRADAT$, 6%) + " "  + &
			PRNT_DATE(LAST_DUEDAT$, 6%) + " "  + &
			FORMAT$(AR_TEMP::AGING(0%), "########.## ") + &
			FORMAT$(AR_TEMP::AGING(1%), "########.## ") + &
			FORMAT$(AR_TEMP::AGING(2%), "########.## ") + &
			FORMAT$(AR_TEMP::AGING(3%), "########.## ") + &
			FORMAT$(AR_TEMP::AGING(4%), "########.## ") + &
			FORMAT$(AR_TEMP::CHARGE, "########.## ") + &
			FORMAT$(BALANCE, "########.## ") + &
			FORMAT$(AR_TEMP::FUTURE, "########.##")

	END SELECT

	IF FAST$ <> "T"
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

 InvTotals:
	!
	! Totals for one customer
	!
	INVTOTALS(I%) = INVTOTALS(I%) + AR_TEMP::AGING(I%) &
		FOR I% = 0% TO 4%

	INVTOTALS.CHARGE = INVTOTALS.CHARGE + AR_TEMP::CHARGE
	INVTOTALS.BALANCE = INVTOTALS.BALANCE + BALANCE
	INVTOTALS.FUTURE = INVTOTALS.FUTURE + AR_TEMP::FUTURE

 InitInv:
	!
	! Initialize invoice record
	!
	AR_TEMP::CUSNUM = ""
	AR_TEMP::ACCT = ""
	AR_TEMP::CREDIT = 0.0
	AR_TEMP::FUTURE = 0.0
	AR_TEMP::AGING(X%) = 0.0 FOR X% = 0% TO 4%
	AR_TEMP::YTDSERVICE = 0.0
	AR_TEMP::LAST_PAID = 0.0
	AR_TEMP::YTDSALES = 0.0
	AR_TEMP::CHARGE = 0.0
	AR_TEMP::LAST_CHARGE = ""
	AR_TEMP::LAST_PAYMENT = ""
	AR_TEMP::LAST_UPDATE = ""

	RETURN

 CustomerTotal:
	!
	! Handle end of invoices for one customer
	!
	IF PRINT_FLAG%
	THEN
		IF (LAST_SALESMAN$ <> AR_35CUSTOM::SALESMAN) AND &
			(SORTBY$ = "SA")
		THEN
			GOSUB SalesmanTotal UNLESS SALESMAN_COUNT% = 0%
			SALESMAN_COUNT% = 0%

			GOSUB SalesmanHeader
			LAST_SALESMAN$ = AR_35CUSTOM::SALESMAN
		END IF

		!
		! Print INVTOTALS variables
		!
		SELECT OPTIONS$

		CASE "F"
			TEXT$ = "Future Totals" + SPACE$(19%) + &
				FORMAT$(INVTOTALS.FUTURE, "########.## ")

		CASE "P"
			TEXT$ = "Past Due Totals" + SPACE$(29%) + &
				FORMAT$(INVTOTALS(1%), "########.## ") + &
				FORMAT$(INVTOTALS(2%), "########.## ") + &
				FORMAT$(INVTOTALS(3%), "########.## ") + &
				FORMAT$(INVTOTALS(4%), "########.## ") + &
				FORMAT$(INVTOTALS.CHARGE, "########.## ") + &
				FORMAT$(INVTOTALS.BALANCE, "########.## ")

		CASE "T"
			TEXT$ = "Invoice Totals" + SPACE$(18%) + &
				FORMAT$(INVTOTALS(0%), "########.## ") + &
				FORMAT$(INVTOTALS(1%), "########.## ") + &
				FORMAT$(INVTOTALS(2%), "########.## ") + &
				FORMAT$(INVTOTALS(3%), "########.## ") + &
				FORMAT$(INVTOTALS(4%), "########.## ") + &
				FORMAT$(INVTOTALS.CHARGE, "########.## ") + &
				FORMAT$(INVTOTALS.BALANCE, "########.## ") + &
				FORMAT$(INVTOTALS.FUTURE, "########.##")

		END SELECT

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

		!
		! Summarize for totals
		!
		GRANDTOTAL(I%) = GRANDTOTAL(I%) + INVTOTALS(I%) &
			FOR I% = 0% TO 4%
		GRANDTOTAL_CHARGE  = GRANDTOTAL_CHARGE + INVTOTALS.CHARGE
		GRANDTOTAL_BALANCE = GRANDTOTAL_BALANCE + INVTOTALS.BALANCE
		GRANDTOTAL_FUTURE  = GRANDTOTAL_FUTURE + INVTOTALS.FUTURE
	END IF

	!
	! Reinitialize INVTOTALS variables
	!
	INVTOTALS(I%) = 0.0 FOR I% = 0% TO 4%
	INVTOTALS.CHARGE = 0.0
	INVTOTALS.BALANCE = 0.0
	INVTOTALS.FUTURE = 0.0

	PRINT_FLAG% = 0%

	RETURN

	!*******************************************************************
	! Print total for this salesman
	!*******************************************************************
 SalesmanTotal:
	!
	! Force a new page
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 3000%)
	RETURN

	!*******************************************************************
	! Print a title for this salesman
	!*******************************************************************
 SalesmanHeader:
	!
	! Output a simple header for the salesman
	!
	TEXT$ = "Salesman: " + AR_35CUSTOM::SALESMAN
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 5%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
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
