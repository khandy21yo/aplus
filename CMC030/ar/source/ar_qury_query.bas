1	%TITLE "Query the Accounts Receivable System"
	%SBTTL "AR_QURY_QUERY"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	! ID:ARQURY
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Query Accounts Receivable\* option
	!	makes on-line inquiries into any selected customer's account.
	!	Inquiry can be made to both open and closed files.
	!	.b
	!	The Print option prints
	!	a report containing the following fields:
	!	.table 3,25
	!	.te
	!	Invoice _#	Description
	!	.te
	!	Account _#	Date
	!	.te
	!	Sale Amount	Discount Amount
	!	.te
	!	Other	Gross Amount
	!	.te
	!	Receipt _#	Check _#
	!	.te
	!	Date	Amount
	!	.te
	!	Balance	Account
	!	.TE
	!	Service Charge	Future
	!	.TE
	!	Balance	Credit Limit
	!	.te
	!	Open File Total	Closed File Total
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Query>Accounts Receivable
	!	.x Accounts Receivable>Query
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_QURY_QUERY/LINE
	!	$ LINK/EXECUTABLE=AR_EXE: AR_QURY_QUERY,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_QURY_QUERY.OBJ;*
	!
	! Author:
	!
	!	12/22/86 - Kevin Handy
	!
	! Modification history:
	!
	!	08/10/90 - Kevin Handy
	!		Modified to put credit memo's on the left hand side
	!		of the page, instead of the right hand side.
	!
	!	06/06/91 - Kevin Handy
	!		Removed some garbage from error trapping.
	!
	!	06/24/91 - Val James Allen
	!		added more information to display (ie: credit limit,
	!		total sales, last invoice date etc.)
	!		See subroutine ExtraStuff
	!
	!	07/14/91 - Kevin Handy
	!		Removed extra space in front of line 1.
	!
	!	09/04/91 - Frank F. Starman
	!		Trap error at line 4300.
	!
	!	09/06/91 - Kevin Handy
	!		Modified "Find" option so that if customer
	!		is not found, it will pull up the next one in
	!		the file, and not the one after the current
	!		record.
	!
	!	02/12/92 - Frank F. Starman
	!		Look for the first payment on invoice and assume that
	!		all what needs to be paid.
	!
	!	02/13/92 - Frank F. Starman
	!		Add REGARDLESS while do GET.
	!
	!	02/24/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	02/27/92 - Dan Perkins
	!		Put error trapping case numbers in order.
	!		Added error trapping for lines 4310-4330.
	!
	!	03/10/92 - Dan Perkins
	!		Modified error trapping for AR_CLOSED and AR_CUSBAL.
	!
	!	03/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	03/18/92 - Frank F. Starman
	!		Add Ave day Pay figures.
	!
	!	03/22/92 - Kevin Handy
	!		Clean up (check)
	!
	!	07/01/92 - Dan Perkins
	!		Show amount of last payment.
	!
	!	07/30/92 - Dan Perkins
	!		Display OE information before AR_OPEN or AR_CLOSED
	!		info.
	!
	!	08/14/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	09/11/92 - Kevin Handy
	!		Modified so it will not print balance forward
	!		amount (beginning balance) on the open item customers.
	!
	!	09/11/92 - Kevin Handy
	!		Reworked the average calculations so that it doesn't
	!		report negitives on everyone.
	!
	!	11/30/92 - Dan Perkins
	!		Added another digit to the DISAMT field so it
	!		would not overflow on large discounts.
	!
	!	10/28/94 - Kevin Handy
	!		Hundreds of thousands of changes in the generation
	!		of the additional informat displayed at the top
	!		of the query (pay days, coll days, etc.)
	!
	!	11/15/94 - Kevin Handy
	!		Added YTD Invoice and 12 month Invoice amounts.
	!
	!	11/16/94 - Kevin Handy
	!		Added Fiscal YTD amount. Made adjustments for
	!		average days calculations.
	!
	!	11/17/94 - Kevin Handy
	!		Fixed bug where it was printing the wrong variable
	!		for Fiscal YTD.
	!
	!	01/18/95 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/12/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Change SMG_QUERY to SMG_QUERY%.
	!
	!	01/29/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to SPACE$(...) in
	!		several places.
	!
	!	06/19/96 - Kevin Handy
	!		Added new transaction type "11", adjustment.
	!		Reformat source code.
	!
	!	12/03/96 - Kevin Handy
	!		Lose unecessary definitions.
	!
	!	05/09/97 - Kevin Handy
	!		Use OUTP_INITFORM function
	!
	!	07/07/97 - Kevin Handy
	!		Lost lines 5090, 5190 (blank line)
	!		Lost several '"+"' for efficiency.
	!
	!	07/07/97 - Kevin Handy
	!		Add code for 'Periods for Summary' stuff.
	!		Added 'Restore' option.
	!
	!	08/25/97 - Kevin Handy
	!		Clean up (Check)
	!		Lose error trap for line 4000, which does'nt exist
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/01/98 - Kevin Handy
	!		Increase dimensions from 200 to 500 (FJ)
	!
	!	11/05/98 - Kevin Handy
	!		Add a cutoff number of periods option (FJ)
	!
	!	06/16/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
	!
	!	04/05/2001 - Kevin Handy
	!		Adjust column titles in aging
	!
	!	10/03/2001 - Kevin Handy
	!		Move field for customer over 4 spaces so it
	!		won't overlap the title.
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"

	!
	! Map file
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.HB"
	MAP (AR_CLOSED)		AR_CLOSED_CDD		AR_CLOSED
	DIM			AR_CLOSED_CDD		LEFT_CLOSED(1000%), &
							RIGHT_CLOSED(1000%)

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN)		AR_OPEN_CDD		AR_OPEN
	DIM			AR_OPEN_CDD		LEFT_OPEN(500%), &
							RIGHT_OPEN(500%)

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.HB"
	MAP (AR_CUSBAL)		AR_CUSBAL_CDD		AR_CUSBAL
	DIM			AR_CUSBAL_CDD		ARRAY_CUSBAL(50%)

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD		AR_CONTROL

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TERMS.HB"
	MAP (UTL_TERMS)		UTL_TERMS_CDD		UTL_TERMS

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	MAP (OE_REGHEADER)	OE_REGHEADER_CDD	OE_REGHEADER

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	MAP (OE_REGLINE)	OE_REGLINE_CDD		OE_REGLINE

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)	GL_PERIOD_CDD		GL_PERIOD

	!
	! Common Statements
	!
	COM (CH_AR_35CUSTOM) AR_35CUSTOM.CH%
	COM (CH_AR_CUSBAL) AR_CUSBAL.CH%
	COM (CH_AR_OPEN) AR_OPEN.CH%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION AR_FUNC_AGE
	EXTERNAL LONG	FUNCTION OUTP_INITFORM

	!
	! Declare constants
	!
	DECLARE LONG YLONG

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	SCOPE::PRG_IDENT, RESTORE_IDENT$ = "PROG"
	SCOPE::PRG_PROGRAM, RESTORE_PROGRAM$ = "AR_QURY_QUERY"

	REPORT$ = "ARQURY"

300	!
	! Open Customer File
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

310	!
	! Open AR open file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.OPN"
	USE
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

320	!
	! Open AR close file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.OPN"
	USE
		CONTINUE 322 IF ERR = 5%
		FILENAME$ = "AR_CLOSE"
		CONTINUE HelpError
	END WHEN

322	!
	! Open control file
	!
	AR_CONTROL::CTITLE = "Customer"

	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.OPN"
		GET #AR_CONTROL.CH%, RECORD 1%, REGARDLESS

		IF EDIT$(AR_CONTROL::CTITLE, -1%) = ""
		THEN
			AR_CONTROL::CTITLE = "Customer"
		END IF

		IF AR_CONTROL::AGEPER(0%) = 0%
		THEN
			AR_CONTROL::AGEPER(0) = 30%
			AR_CONTROL::AGENAM(0) = "Current"
			AR_CONTROL::AGEPER(1) = 30%
			AR_CONTROL::AGENAM(1) = "31 to 60 days"
			AR_CONTROL::AGEPER(2) = 30%
			AR_CONTROL::AGENAM(2) = "61 to 90 days"
			AR_CONTROL::AGEPER(3) = 30%
			AR_CONTROL::AGENAM(3) = "91 to 120 days"
			AR_CONTROL::AGEPER(4) = 30%
			AR_CONTROL::AGENAM(4) = "121 and over"
		END IF
	USE
		IF ERR = 5%
		THEN
			!
			! Fake a control file
			!
			AR_CONTROL::AR_ACCT = ""
			AR_CONTROL::RETAIN = 0%
			AR_CONTROL::LASTPERCLOSE = 0%
			AR_CONTROL::YEAR = ""
			AR_CONTROL::CLOSEFLAG = "0"
			AR_CONTROL::CTITLE = "Customer"
			AR_CONTROL::AGEPER(0) = 30%
			AR_CONTROL::AGENAM(0) = "Current"
			AR_CONTROL::AGEPER(1) = 30%
			AR_CONTROL::AGENAM(1) = "31 to 60 days"
			AR_CONTROL::AGEPER(2) = 30%
			AR_CONTROL::AGENAM(2) = "61 to 90 days"
			AR_CONTROL::AGEPER(3) = 30%
			AR_CONTROL::AGENAM(3) = "91 to 120 days"
			AR_CONTROL::AGEPER(4) = 30%
			AR_CONTROL::AGENAM(4) = "121 and over"

			CONTINUE 324
		END IF

		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

324	!
	! Open customer balance file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.OPN"
	USE
		CONTINUE 325 IF ERR = 5%
		FILENAME$ = "AR_CUSBAL"
		CONTINUE HelpError
	END WHEN

325	!
	! Open Order register header file if applicable
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.OPN"
		NOORDERSYS$ = "Y"
	USE
		NOORDERSYS$ = "N"
		CONTINUE 326 IF ERR = 5%
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

326	!
	! Open Order register line file if applicable
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.OPN"
	USE
		CONTINUE 327 IF ERR = 5%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

327	!
	! Open Terms description file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_TERMS.OPN"
	USE
		CONTINUE 328 IF ERR = 5%
		FILENAME$ = "UTL_TERMS"
		CONTINUE HelpError
	END WHEN

328	!
	! Open Chart control file, and get last month/date if
	! possible
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
		FISCAL_DATE$ = GL_PERIOD::YEAR + "01"
		CLOSE #GL_PERIOD.CH%
	USE
		FISCAL_DATE$ = LEFT(DATE_TODAY, 4%) + "01"
		CONTINUE 330 IF ERR = 5%
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

330	!

500	GOSUB Initialize

900	!
	! Create a display window
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		78%, &
		SMG_QUERY%, &
		SMG$M_BORDER &
	)

	!
	! Label the display
	!
	SMG_STATUS% = SMG$LABEL_BORDER(SMG_QUERY%, &
		"AR Query for " + TRM$(SCOPE::PRG_COMPANY))

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, AR_CONTROL::CTITLE, 2%, 2%)

	GOSUB Repaint

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_QUERY%, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)

	%PAGE

	OPT$ = "F"
	GOTO SelectOption

1000	!******************************************************************
	! Main option menu
	!******************************************************************

1100	!
	! Enter options
	!
	SCOPE::PRG_ITEM = ""
	SCOPE::PRG_IDENT = RESTORE_IDENT$
	SCOPE::PRG_PROGRAM = RESTORE_PROGRAM$

	OPTLIST$ = "Find Next Restore Print Help eXit"
	OPT$ = ENTR_3OPTION(SCOPE, "COMMAND", OPTLIST$, OPT%, 0%)

	SELECT SCOPE::SCOPE_EXIT
	!
	! Control c
	!
	CASE 3%
		GOTO 1000

	!
	! Exit key
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	END SELECT

 SelectOption:
	SELECT OPT$

	!
	! Call the help message
	!
	CASE "H"
		CALL HELP_34MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, &
			SCOPE::PRG_PROGRAM, "", "HELP")

	CASE "F"
1120		LAST_CUSNUM$ = AR_35CUSTOM::CUSNUM

		AR_35CUSTOM::CUSNUM = ENTR_3STRING(SCOPE, SMG_QUERY%, "2;20", &
			TRM$(AR_CONTROL::CTITLE) + &
			" #", AR_35CUSTOM::CUSNUM, FLAG%, "'E", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT
		!
		! List Choices
		!
		CASE SMG$K_TRM_F14
			SCOPE::PRG_ITEM = "FLD001"

			IF MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, "VX") <> 1%
			THEN
				AR_35CUSTOM::CUSNUM = LAST_CUSNUM$
				GOTO 1120
			ELSE
				GOTO 2000
			END IF

		!
		! Control c
		!
		CASE 3%
			GOTO 1120

		!
		! Exit key
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT

		IF AR_35CUSTOM::CUSNUM = "??????????"
		THEN
			GOTO 1000
		END IF

2000		WHEN ERROR IN
			GET #AR_35CUSTOM.CH%, &
				KEY #0% GE AR_35CUSTOM::CUSNUM, &
				REGARDLESS
		USE
			CONTINUE 2010 IF ERR = 155% OR ERR = 11%
			FILENAME$ = "AR_35CUSTOM"
			CONTINUE HelpError
		END WHEN

2010		GOSUB Repaint

	CASE "N"
3000		WHEN ERROR IN
			GET #AR_35CUSTOM.CH%, REGARDLESS
		USE
			CONTINUE 1000 IF ERR = 11%
			FILENAME$ = "AR_35CUSTOM"
			CONTINUE HelpError
		END WHEN

		GOSUB Repaint

	CASE "R"
3010		WHEN ERROR IN
			RESET #AR_35CUSTOM.CH%
			GET #AR_35CUSTOM.CH%, REGARDLESS
		USE
			CONTINUE 1000 IF ERR = 11%
			FILENAME$ = "AR_35CUSTOM"
			CONTINUE HelpError
		END WHEN

		GOSUB Repaint

	CASE "P"
		!*****************************************************
		! List all records in all files
		!*****************************************************

		CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)

		!
		! Ask user to change settings
		!
		GOTO 1000 &
			IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "") <> &
			CMC$_NORMAL

	!++
	! Abstract:FLD01
	!	^*(01) Periods For Summary\*
	!	.p
	!	Determines how many periods worth of data should be used
	!	in the calculation of several fields in the summary at
	!	the top.
	!	A blank/zero entry will use all periods in the registers.
	!	.p
	!	This field is used by the "Average Days Collected"
	!	and the "Average Days Paid" fields in the summary.
	!
	! Index:
	!	.x Query>Periods
	!	.x Periods>Query
	!
	!--
		DETAIL_PERIODS% = VAL%(TRM$(UTL_REPORTX::OPTDEF(1%)))

	!++
	! Abstract:FLD02
	!	^*(02) Periods For Detail\*
	!	.p
	!	Determines how many periods worth of data should be
	!	displayed in the detail section.
	!	.p
	!	A blank/zero entry will use all periods in the registers.
	!	.p
	!	This field is used by the "Average Days Collected"
	!	and the "Average Days Paid" fields in the summary.
	!
	! Index:
	!	.x Query>Periods
	!	.x Periods>Query
	!
	!--
		SCAN_PERIODS% = VAL%(TRM$(UTL_REPORTX::OPTDEF(0%)))

		!
		! Starting period calculations
		!
		IF SCAN_PERIODS% <> 0%
		THEN
			START_PERIOD% = VAL%(AR_CONTROL::YEAR) * 12 + &
				AR_CONTROL::LASTPERCLOSE - SCAN_PERIODS% + 1%
			START_PERIOD% = 0% IF START_PERIOD% < 0%
			START_PERIOD$ = &
				FORMAT$(FIX(START_PERIOD% / 12%), "<0>###") + &
				FORMAT$(START_PERIOD% - (START_PERIOD% / 12%) * 12%, &
				"<0>#")
		ELSE
			START_PERIOD$ = "      "
		END IF

		!
		! Detail period calculations
		!
		IF DETAIL_PERIODS% <> 0%
		THEN
			DETAIL_PERIOD% = VAL%(AR_CONTROL::YEAR) * 12 + &
				AR_CONTROL::LASTPERCLOSE - DETAIL_PERIODS% + 1%
			DETAIL_PERIOD% = 0% IF DETAIL_PERIOD% < 0%
			DETAIL_PERIOD$ = &
				FORMAT$(FIX(DETAIL_PERIOD% / 12%), "<0>###") + &
				FORMAT$(DETAIL_PERIOD% - (DETAIL_PERIOD% / 12%) * 12%, &
				"<0>#")
		ELSE
			DETAIL_PERIOD$ = "      "
		END IF

		!
		! Set up titles
		!
		TITLE$(1%) = "Accounts Receivable Query"
		TITLE$(2%) = ""

		TITLE$(3%) = "Customer#   CustomerName" + &
			SPACE$(38%) + "Phone"

		TITLE$(4%) = ""

		TEXT$ = AR_35CUSTOM::CUSNUM + " " + &
			AR_35CUSTOM::CUSNAM + " " + &
			PRNT_PHONE(AR_35CUSTOM::PHONE, 0%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

		GOSUB ExtraStuff

		TITLE$(3%) = "Invoice  Descr     Account            " + &
			"Date        SaleAmt   DisAmt    Other " + &
			"    GrsAmt Receipt Chck #  Date  " + &
			"      Amount    Balance"

		TITLE$(4%) = ""

4050		INTRVL$(I%) = LEFT(SPACE$(11% - &
			LEN(TRM$(AR_CONTROL::AGENAM(I%)))) + &
			TRM$(AR_CONTROL::AGENAM(I%)), 11%) &
			FOR I% = 0% TO 4%

		TEXT$ = "Account            " + &
			INTRVL$(0%) + " " + INTRVL$(1%) + " " + &
			INTRVL$(2%) + " " + &
			INTRVL$(3%) + " " + INTRVL$(4%) + " " + &
			"    Service      Future     Balance"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		IF AR_FUNC_AGE(AR_35CUSTOM::CUSNUM, AR_35CUSTOM::METHOD, &
			DATE_TODAY, "", NUM_ACCT%, ARRAY_CUSBAL()) <> 0%
		THEN
			TEXT$ = "           *** Unable to age ***"
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		ELSE
			FOR LOOP% = 1% TO NUM_ACCT%

				BALANCE = &
					ARRAY_CUSBAL(LOOP%)::AGING(0%) + &
					ARRAY_CUSBAL(LOOP%)::AGING(1%) + &
					ARRAY_CUSBAL(LOOP%)::AGING(2%) + &
					ARRAY_CUSBAL(LOOP%)::AGING(3%) + &
					ARRAY_CUSBAL(LOOP%)::AGING(4%) + &
					ARRAY_CUSBAL(LOOP%)::CHARGE + &
					ARRAY_CUSBAL(LOOP%)::FUTURE

				TEXT$ = ARRAY_CUSBAL(LOOP%)::ACCT + " " + &
					FORMAT$(ARRAY_CUSBAL(LOOP%)::AGING(0%), &
					"########.## ") + &
					FORMAT$(ARRAY_CUSBAL(LOOP%)::AGING(1%), &
					"########.## ") + &
					FORMAT$(ARRAY_CUSBAL(LOOP%)::AGING(2%), &
					"########.## ") + &
					FORMAT$(ARRAY_CUSBAL(LOOP%)::AGING(3%), &
					"########.## ") + &
					FORMAT$(ARRAY_CUSBAL(LOOP%)::AGING(4%), &
					"########.## ") + &
					FORMAT$(ARRAY_CUSBAL(LOOP%)::CHARGE, &
					"########.## ") + &
					FORMAT$(ARRAY_CUSBAL(LOOP%)::FUTURE, &
					"########.## ") + &
					FORMAT$(BALANCE, &
					"########.## ")

				CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
				GOTO 4900 IF UTL_REPORTX::STAT

			NEXT LOOP%

		END IF

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

4300		GOTO 4400 IF NOORDERSYS$ = "N"

		TITLE$(3%) = "----- O p e n  S a l e s  O r d e r s  -----"

		TITLE$(4%) = "Order Number  Order Date  Open Sales Dollars"

		TITLE$(5%) = ""

		TEXT$ = "-----  O p e n  S a l e s  O r d e r s  -----"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO 4900 IF UTL_REPORTX::STAT

		TEXT$ = "Order Number  Order Date  Open Sales Dollars"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO 4900 IF UTL_REPORTX::STAT

		ONORDERBAL = 0.0

		!
		! Process the open orders if there are any
		!
		WHEN ERROR IN
			FIND #OE_REGHEADER.CH%, &
				KEY #3% EQ AR_35CUSTOM::CUSNUM, &
				REGARDLESS
		USE
			CONTINUE 4390 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "OE_REGHEADER"
			CONTINUE HelpError
		END WHEN

4310		WHEN ERROR IN
			GET #OE_REGHEADER.CH%, REGARDLESS
		USE
			CONTINUE 4390 IF ERR = 11%
			FILENAME$ = "OE_REGHEADER"
			CONTINUE HelpError
		END WHEN

		GOTO 4390 IF OE_REGHEADER::CUSNUM <> AR_35CUSTOM::CUSNUM

		TOTORD = 0.0

4320		WHEN ERROR IN
			FIND #OE_REGLINE.CH%, &
				KEY #0% EQ OE_REGHEADER::ORDNUM, &
				REGARDLESS
		USE
			CONTINUE 4380 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "OE_REGLINE"
			CONTINUE HelpError
		END WHEN

4330		WHEN ERROR IN
			GET #OE_REGLINE.CH%, REGARDLESS
		USE
			CONTINUE 4380 IF ERR = 11%
			FILENAME$ = "OE_REGLINE"
			CONTINUE HelpError
		END WHEN

		GOTO 4380 IF OE_REGLINE::ORDNUM <> OE_REGHEADER::ORDNUM

		IF OE_REGLINE::TRANTYPE = "01"
		THEN
			TOTORD = TOTORD + (OE_REGLINE::QTY * OE_REGLINE::PRICE)
		END IF

		IF OE_REGLINE::TRANTYPE = "02"
		THEN
			TOTORD = TOTORD - (OE_REGLINE::QTY * OE_REGLINE::PRICE)
		END IF

		IF OE_REGLINE::TRANTYPE = "03"
		THEN
			TOTORD = TOTORD - (OE_REGLINE::QTY * OE_REGLINE::PRICE)
		END IF

		GOTO 4330

4380		IF TOTORD > 0.0
		THEN
			TEXT$ = CONV_STRING(OE_REGHEADER::ORDNUM, CMC$_LEFT) + "    "   + &
				PRNT_DATE(OE_REGHEADER::ORDDATE, 8%) + "      " + &
				FORMAT$(TOTORD, "###,###,###.##")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			GOTO 4900 IF UTL_REPORTX::STAT

			ONORDERBAL = ONORDERBAL + TOTORD

		END IF

		GOTO 4310

4390		TEXT$ = "Total Open Sales Orders:      " + &
			FORMAT$(ONORDERBAL, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		GOTO 4900 IF UTL_REPORTX::STAT

4400		TEXT$ = "------------------------------------------" + &
			"  O p e n  A c c o u n t s  R e c e i v a b l e  " + &
			"-----------------------------------------"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO 4900 IF UTL_REPORTX::STAT

		TEXT$ = "Invoice  Descr     Account            " + &
			"Date        SaleAmt   DisAmt    Other " + &
			"    GrsAmt Receipt Chck #  Date  " + &
			"      Amount    Balance"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO 4900 IF UTL_REPORTX::STAT

		LEFT_OPEN% = 0%			! Total # items in left column
		RIGHT_OPEN% = 0%		! Total # items in right column
		THIS_INVOICE$ = "1234567890123"	! Impossible invoice number.
		CUSTOMER_PRINTED% = 0%		! Has customer name been printed?

		CUS_SALAMT = 0.0		! Zero customer totals
		CUS_DISAMT = 0.0
		CUS_OTHCHG = 0.0
		CUS_CREDIT = 0.0

4410		!
		! Do some Balance Forward junk
		!
		IF AR_35CUSTOM::METHOD <> "B"
		THEN
			GOTO 4500
		END IF

		!
		! Search for first record
		!
		WHEN ERROR IN
			FIND #AR_CUSBAL.CH%, &
				KEY #0% GE AR_35CUSTOM::CUSNUM, &
				REGARDLESS
		USE
			CONTINUE 4500 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "AR_CUSBAL"
			CONTINUE HelpError
		END WHEN

4420		!
		! Pull in next record
		!
		WHEN ERROR IN
			GET #AR_CUSBAL.CH%, REGARDLESS
		USE
			CONTINUE 4500 IF ERR = 11%
			FILENAME$ = "AR_CUSBAL"
			CONTINUE HelpError
		END WHEN

		GOTO 4500 IF AR_CUSBAL::CUSNUM <> AR_35CUSTOM::CUSNUM

		INVTOTAL = AR_CUSBAL::CHARGE + AR_CUSBAL::FUTURE

		INVTOTAL = INVTOTAL + AR_CUSBAL::AGING(LOOP%) &
			FOR LOOP% = 0% TO 4%

		GOTO 4420 IF INVTOTAL = 0.0

		IF CUSTOMER_PRINTED% = 0%
		THEN
			TEXT$ = AR_35CUSTOM::CUSNUM + "  " + &
				AR_35CUSTOM::CUSNAM + &
				PRNT_PHONE(AR_35CUSTOM::PHONE, 0%)

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			GOTO ExitProgram IF UTL_REPORTX::STAT

			CUSTOMER_PRINTED% = -1%
		END IF

		TEXT$ = "Beginning Balance  " + &
			AR_CUSBAL::ACCT + "          " + &
			FORMAT$(INVTOTAL, "#######.## ") + &
			SPACE$(64%) + &
			FORMAT$(INVTOTAL, "#######.## ")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		CUS_SALAMT = CUS_SALAMT + INVTOTAL

		GOTO 4420

4500		!
		! Search for first record
		!
		WHEN ERROR IN
			FIND #AR_OPEN.CH%, &
				KEY #0% EQ AR_35CUSTOM::CUSNUM, &
				REGARDLESS
		USE
			CONTINUE 4550 IF ERR = 155%
			FILENAME$ = "AR_OPEN"
			CONTINUE HelpError
		END WHEN

4510		!
		! Pull in next record
		!
		WHEN ERROR IN
			GET #AR_OPEN.CH%, REGARDLESS
		USE
			CONTINUE 4550 IF ERR = 11%
			FILENAME$ = "AR_OPEN"
			CONTINUE HelpError
		END WHEN

		GOTO 4550 IF AR_35CUSTOM::CUSNUM <> AR_OPEN::CUSNUM

		!
		! Cutoff period
		!
		GOTO 4510 IF (AR_OPEN::UPDATED < DETAIL_PERIOD$) AND &
			(DETAIL_PERIOD$ <> "")

		IF (AR_OPEN::INVNUM <> THIS_INVOICE$)
		THEN
			GOSUB DumpInvoice
			GOTO 4900 IF UTL_REPORTX::STAT
		END IF

		SELECT AR_OPEN::TRATYP

		CASE "02"
			LEFT_OPEN% = LEFT_OPEN% + 1%
			LEFT_OPEN(LEFT_OPEN%) = AR_OPEN
			RIGHT_OPEN% = RIGHT_OPEN% + 1%
			RIGHT_OPEN(RIGHT_OPEN%) = AR_OPEN
			RIGHT_OPEN(RIGHT_OPEN%)::SALAMT = &
				-RIGHT_OPEN(RIGHT_OPEN%)::SALAMT

		CASE "09", "10", "11"
			RIGHT_OPEN% = RIGHT_OPEN% + 1%
			RIGHT_OPEN(RIGHT_OPEN%) = AR_OPEN

		CASE ELSE
			LEFT_OPEN% = LEFT_OPEN% + 1%
			LEFT_OPEN(LEFT_OPEN%) = AR_OPEN

		END SELECT

		GOTO 4510

4550		GOSUB DumpInvoice
		GOTO 4900 IF UTL_REPORTX::STAT

		TEXT$ = SPACE$(17%) + "Open File Total" + SPACE$(15%) + &
			FORMAT$(CUS_SALAMT - &
				CUS_DISAMT - &
				CUS_OTHCHG, "#######.##") + &
			FORMAT$(CUS_DISAMT, "######.## ") + &
			FORMAT$(CUS_OTHCHG, "#####.## ") + &
			FORMAT$(CUS_SALAMT, "#######.## ") + &
			SPACE$(24%) + &
			FORMAT$(-CUS_CREDIT, "#######.##") + &
			FORMAT$(CUS_SALAMT + &
				CUS_CREDIT, "########.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		GOTO 4900 IF UTL_REPORTX::STAT

		TEXT$ = "----------------------------------------  " + &
			"C l o s e d  A c c o u n t s  R e c e i v a b l e  " + &
			"---------------------------------------"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

4700		!
		! Search for first invoice record for this customer
		!
		LEFT_CLOSED% = 0%		! Total # items in left column
		RIGHT_CLOSED% = 0%		! Total # items in right column
		THIS_INVOICE$ = "1234567890123"	! Impossible invoice number.
		CUSTOMER_PRINTED% = 0%		! Has customer name been printed?

		CUS_SALAMT = 0.0		! Zero customer totals
		CUS_DISAMT = 0.0
		CUS_OTHCHG = 0.0
		CUS_CREDIT = 0.0

		!
		! Search for first record
		!
		WHEN ERROR IN
			FIND #AR_CLOSED.CH%, &
				KEY #0% EQ AR_35CUSTOM::CUSNUM, &
				REGARDLESS
		USE
			CONTINUE 4750 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "AR_CLOSED"
			CONTINUE HelpError
		END WHEN

4710		!
		! Pull in next record
		!
		WHEN ERROR IN
			GET #AR_CLOSED.CH%, REGARDLESS
		USE
			CONTINUE 4750 IF ERR = 11%
			FILENAME$ = "AR_CLOSED"
			CONTINUE HelpError
		END WHEN

		GOTO 4750 IF AR_35CUSTOM::CUSNUM <> AR_CLOSED::CUSNUM

		!
		! Cutoff period
		!
		GOTO 4710 IF (AR_CLOSED::UPDATED < DETAIL_PERIOD$) AND &
			(DETAIL_PERIOD$ <> "")

		IF (AR_CLOSED::INVNUM <> THIS_INVOICE$)
		THEN
			GOSUB DumpInvoiceClosed
			GOTO 4900 IF UTL_REPORTX::STAT
		END IF

		SELECT AR_CLOSED::TRATYP

		CASE "02"
			!
			! Cash sale.  Goes in both columns.  Balances to zero.
			!
			LEFT_CLOSED% = LEFT_CLOSED% + 1%
			LEFT_CLOSED(LEFT_CLOSED%) = AR_CLOSED
			RIGHT_CLOSED% = RIGHT_CLOSED% + 1%
			RIGHT_CLOSED(RIGHT_CLOSED%) = AR_CLOSED
			RIGHT_CLOSED(RIGHT_CLOSED%)::SALAMT = &
				-RIGHT_CLOSED(RIGHT_CLOSED%)::SALAMT

		CASE "09", "10", "11"
			!
			! Payments.  Goes in right column.
			!
			RIGHT_CLOSED% = RIGHT_CLOSED% + 1%
			RIGHT_CLOSED(RIGHT_CLOSED%) = AR_CLOSED

		CASE ELSE
			!
			! All else goes in left column
			!
			LEFT_CLOSED% = LEFT_CLOSED% + 1%
			LEFT_CLOSED(LEFT_CLOSED%) = AR_CLOSED

		END SELECT

		GOTO 4710

4750		GOSUB DumpInvoiceClosed
		GOTO 4900 IF UTL_REPORTX::STAT

		TEXT$ = SPACE$(17%) + "Closed File Total" + SPACE$(13%) + &
			FORMAT$(CUS_SALAMT - &
				CUS_DISAMT - &
				CUS_OTHCHG, "#######.##") + &
			FORMAT$(CUS_DISAMT, "######.## ") + &
			FORMAT$(CUS_OTHCHG, "#####.## ") + &
			FORMAT$(CUS_SALAMT, "#######.## ") + &
			SPACE$(24%) + &
			FORMAT$(-CUS_CREDIT, "#######.##") + &
			FORMAT$(CUS_SALAMT + &
				CUS_CREDIT, "########.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		GOTO 4900 IF UTL_REPORTX::STAT

4900		!
		! Finish up
		!
		CALL OUTP_FINISH(UTL_REPORTX)

		GOTO 900

	CASE "X"
		GOTO ExitProgram

	END SELECT

	GOTO 1100

	%PAGE

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")


 ExtraStuff:
	!*****************************************************************
	!This here routine prints out all the extra stuff on the report
	!and if it don't work blame Frank
	!*****************************************************************

	!
	!Setup initial variables and all that stuff
	!
	LASTINVDATE$ = ""
	LASTPAYDATE$ = ""
	HIGHINVOICE = 0.0
	TOTALSALE = 0.0
	TOTALINV% = 0%
	TOTALPAYINV = 0.0
	LASTORDER$ = ""
	ONORDERBAL = 0.0

	DAYSCOLL% = 0%
	DAYSCOLLCNT% = 0%
	DAYSPAYCNT% = 0%
	DAYSPAYREC$ = ""

	AMTPAID = 0.0

	YEAR_INVOICE = 0.0
	YTD_INVOICE = 0.0
	FIS_INVOICE = 0.0

	THIS_DATE$ = DATE_TODAY
	DATE_TODAY% = DATE_DAYCODE(THIS_DATE$)

	LASTINV$ = SPACE$(LEN(AR_OPEN::INVNUM) + 1%)
	INVOICEDATE$ = ""
	FIRSTPAY$ = ""

	!
	! Calculate dates to compare against for 12 month sales and YTD
	! sales.
	!
	YEAR_DATE$ = DATE_TODAY
	YEAR_DATE$ = FORMAT$(VAL%(LEFT(YEAR_DATE$, 4%)) - 1%, "<0>###") + &
		RIGHT(YEAR_DATE$, 5%)

	YTD_DATE$ = DATE_TODAY
	YTD_DATE$ = LEFT(YTD_DATE$, 4%) + "0101"

	!
	! Read AR_OPEN file for all the related stuff
	!
5000	WHEN ERROR IN
		FIND #AR_OPEN.CH%, &
			KEY #0% GE AR_35CUSTOM::CUSNUM, &
			REGARDLESS
	USE
		CONTINUE 5100 IF ERR = 155%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	DATE_TODAY% = DATE_DAYCODE(DATE_TODAY)

5010	WHEN ERROR IN
		GET #AR_OPEN.CH%, REGARDLESS
	USE
		CONTINUE 5100 IF ERR = 11%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	GOTO 5100 IF AR_OPEN::CUSNUM <> AR_35CUSTOM::CUSNUM

	GOSUB DoCalculate

	GOTO 5010

5100	!
	! Read AR_CLOSED file for all the related stuff
	!
	WHEN ERROR IN
		FIND #AR_CLOSED.CH%, KEY #0% GE AR_35CUSTOM::CUSNUM, REGARDLESS
	USE
		CONTINUE 5200 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

5110	WHEN ERROR IN
		GET #AR_CLOSED.CH%, REGARDLESS
	USE
		CONTINUE 5200 IF ERR = 11%
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

	GOTO 5200 IF AR_CLOSED::CUSNUM <> AR_35CUSTOM::CUSNUM

	AR_OPEN = AR_CLOSED

	GOSUB DoCalculate

	GOTO 5110

5200	!
	! Process the open orders if there are any
	!
	GOTO 5900 IF NOORDERSYS$ = "N"

	WHEN ERROR IN
		FIND #OE_REGHEADER.CH%, &
			KEY #3% EQ AR_35CUSTOM::CUSNUM, &
			REGARDLESS
	USE
		CONTINUE 5900 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

5210	WHEN ERROR IN
		GET #OE_REGHEADER.CH%, REGARDLESS
	USE
		CONTINUE 5900 IF ERR = 11%
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

	GOTO 5900 IF OE_REGHEADER::CUSNUM <> AR_35CUSTOM::CUSNUM

	LASTORDER$ = OE_REGHEADER::ORDDATE IF OE_REGHEADER::ORDDATE > LASTORDER$

5220	WHEN ERROR IN
		FIND #OE_REGLINE.CH%, &
			KEY #0% EQ OE_REGHEADER::ORDNUM, &
			REGARDLESS
	USE
		CONTINUE 5210 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

5230	WHEN ERROR IN
		GET #OE_REGLINE.CH%, REGARDLESS
	USE
		CONTINUE 5210 IF ERR = 11%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

	GOTO 5210 IF OE_REGLINE::ORDNUM <> OE_REGHEADER::ORDNUM

	IF OE_REGLINE::TRANTYPE = "01"
	THEN
		ONORDERBAL = ONORDERBAL + (OE_REGLINE::QTY * OE_REGLINE::PRICE)
	END IF

	IF OE_REGLINE::TRANTYPE = "02"
	THEN
		ONORDERBAL = ONORDERBAL - (OE_REGLINE::QTY * OE_REGLINE::PRICE)
	END IF

	IF OE_REGLINE::TRANTYPE = "03"
	THEN
		ONORDERBAL = ONORDERBAL - (OE_REGLINE::QTY * OE_REGLINE::PRICE)
	END IF

	GOTO 5230

 DoCalculate:
	IF (AR_OPEN::INVNUM <> LASTINV$)
	THEN
		INVOICEDATE$ = ""
		LASTINV$ = AR_OPEN::INVNUM
	END IF

	SELECT AR_OPEN::TRATYP

	CASE "01"
		IF (INVOICEDATE$  = "")
		THEN
			TOTALINV% = TOTALINV% + 1%
			INVOICEDATE$ = AR_OPEN::TRADAT
		END IF

		TOTALSALE = TOTALSALE + AR_OPEN::SALAMT

		IF AR_OPEN::TRADAT > LASTINVDATE$
		THEN
			LASTINVDATE$ = AR_OPEN::TRADAT
		END IF

		IF AR_OPEN::SALAMT > HIGHINVOICE
		THEN
			HIGHINVOICE = AR_OPEN::SALAMT
		END IF

		IF AR_OPEN::TRADAT >= YEAR_DATE$
		THEN
			YEAR_INVOICE = YEAR_INVOICE + AR_OPEN::SALAMT
		END IF

		IF AR_OPEN::TRADAT >= YTD_DATE$
		THEN
			YTD_INVOICE = YTD_INVOICE + AR_OPEN::SALAMT
		END IF

		IF AR_OPEN::UPDATED >= FISCAL_DATE$
		THEN
			FIS_INVOICE = FIS_INVOICE + AR_OPEN::SALAMT
		END IF

	CASE "02"
		IF (INVOICEDATE$  = "")
		THEN
			TOTALINV% = TOTALINV% + 1%
			INVOICEDATE$ = AR_OPEN::TRADAT
		END IF

		IF (AR_OPEN::TRADAT <> "") AND &
			(AR_OPEN::UPDATED >= START_PERIOD$)
		THEN
			FIRSTPAY$ = AR_OPEN::TRADAT &
				IF FIRSTPAY$ > AR_OPEN::TRADAT OR &
				FIRSTPAY$ = ""
		END IF

		TOTALSALE = TOTALSALE + AR_OPEN::SALAMT
		TOTALINV% = TOTALINV% + 1%

		IF AR_OPEN::TRADAT > LASTINVDATE$
		THEN
			LASTINVDATE$ = AR_OPEN::TRADAT
		END IF

		IF AR_OPEN::SALAMT > HIGHINVOICE
		THEN
			HIGHINVOICE = AR_OPEN::SALAMT
		END IF

		IF AR_OPEN::TRADAT = LASTPAYDATE$
		THEN
			AMTPAID = AMTPAID + AR_OPEN::SALAMT
		END IF

		!
		! Collected on same day as paid (0 days to collect)
		!
		IF (AR_OPEN::UPDATED >= START_PERIOD$)
		THEN
			DAYSCOLLCNT% = DAYSCOLCNT% + 1%

			IF INSTR(1%, DAYSPAYREC$, "," + AR_OPEN::CHKNUM) = 0%
			THEN
				DAYSPAYCNT% = DAYSPAYCNT% + 1%
				DAYSPAYREC$ = DAYSPAYREC$ + "," + &
					AR_OPEN::CHKNUM
			END IF
		END IF

		!
		! Collect summaries
		!
		IF AR_OPEN::TRADAT >= YEAR_DATE$
		THEN
			YEAR_INVOICE = YEAR_INVOICE + AR_OPEN::SALAMT
		END IF

		IF AR_OPEN::TRADAT >= YTD_DATE$
		THEN
			YTD_INVOICE = YTD_INVOICE + AR_OPEN::SALAMT
		END IF

		IF AR_OPEN::UPDATED >= FISCAL_DATE$
		THEN
			FIS_INVOICE = FIS_INVOICE + AR_OPEN::SALAMT
		END IF

	CASE "04"
		IF (INVOICEDATE$  = "")
		THEN
			INVOICEDATE$ = AR_OPEN::TRADAT
		END IF

	CASE "08"
		IF (INVOICEDATE$  = "")
		THEN
			TOTALINV% = TOTALINV% + 1%
			INVOICEDATE$ = AR_OPEN::TRADAT
		END IF

	CASE "09", "10", "11"
		IF (INVOICEDATE$  = "")
		THEN
			TOTALINV% = TOTALINV% + 1%
			INVOICEDATE$ = AR_OPEN::TRADAT
		END IF

		IF (AR_OPEN::TRADAT <> "") AND &
			(AR_OPEN::UPDATED >= START_PERIOD$)
		THEN
			FIRSTPAY$ = AR_OPEN::TRADAT &
				IF FIRSTPAY$ > AR_OPEN::TRADAT OR &
				FIRSTPAY$ = ""
		END IF

		!
		! Collect date totals
		!
		IF (AR_OPEN::UPDATED >= START_PERIOD$)
		THEN
			DAYSCOLL% = DAYSCOLL% + &
				DATE_DAYCODE(AR_OPEN::TRADAT) - &
				DATE_DAYCODE(INVOICEDATE$)
			DAYSCOLLCNT% = DAYSCOLLCNT% + 1%

			IF INSTR(1%, DAYSPAYREC$, AR_OPEN::CHKNUM) = 0%
			THEN
				DAYSPAYCNT% = DAYSPAYCNT% + 1%
				DAYSPAYREC$ = DAYSPAYREC$ + "," + &
					AR_OPEN::CHKNUM
			END IF
		END IF

		IF AR_OPEN::TRADAT = LASTPAYDATE$
		THEN
			AMTPAID = AMTPAID + AR_OPEN::SALAMT
		END IF

		IF AR_OPEN::TRADAT > LASTPAYDATE$ OR LASTPAYDATE$ = ""
		THEN
			LASTPAYDATE$ = AR_OPEN::TRADAT
			AMTPAID      = AR_OPEN::SALAMT
		END IF

	END SELECT

	RETURN

5900	!
	! Finish calculating stuff
	!
	IF DAYSCOLLCNT% = 0%
	THEN
		AVEDAYCOLL = 0.0
	ELSE
		AVEDAYCOLL = DAYSCOLL% / DAYSCOLLCNT%
	END IF

	IF DAYSPAYCNT% > 1%
	THEN
		AVEDAYPAY = (DATE_DAYCODE(LASTPAYDATE$) - &
			DATE_DAYCODE(FIRSTPAY$)) / &
			(DAYSPAYCNT% - 1%)
	ELSE
		AVEDAYPAY = 0.0
	END IF

5910	WHEN ERROR IN
		GET #UTL_TERMS.CH%, KEY #0% EQ AR_35CUSTOM::TERMS, REGARDLESS
	USE
		UTL_TERMS::DESCR = ""
		CONTINUE 5920 IF ERR = 11% OR ERR = 9% OR ERR = 155%
		FILENAME$ = "UTL_TERMS"
		CONTINUE HelpError
	END WHEN

5920	!
	! Lets get printing
	!
	TEXT$ = "Terms: " + AR_35CUSTOM::TERMS + " " + UTL_TERMS::DESCR

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "Opened:       " + PRNT_DATE(AR_35CUSTOM::BDATE, 8%) + &
		"            Ave. Days Coll:        " + &
		FORMAT$(AVEDAYCOLL, "#####") + &
		"            Ave. Days Pay:        " + &
		FORMAT$(AVEDAYPAY, "#####")
	IF START_PERIOD$ > "000000"
	THEN
		TEXT$ = TEXT$ + "     Since: " + START_PERIOD$
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "Last Invoice: " + PRNT_DATE(LASTINVDATE$, 8%) + &
		"            " + &
		"On Order Balance: " + FORMAT$(ONORDERBAL, "#######.##") + &
		"            Total Sales:     " + &
		FORMAT$(TOTALSALE, "#######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "Last Payment: " + PRNT_DATE(LASTPAYDATE$, 8%) + &
		"            " + &
		"Last Amount Paid: " + FORMAT$(-AMTPAID, "#######.##") + &
		"            12 Month Sales:  " + &
		FORMAT$(YEAR_INVOICE, "#######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "Credit Limit: " + &
		FORMAT$(AR_35CUSTOM::CREDITLIM, "#######.##") + &
		"            " + &
		"High Invoice:     " + FORMAT$(HIGHINVOICE, "#######.##") + &
		"            YTD Sales:       " + &
		FORMAT$(YTD_INVOICE, "#######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "Ave Invoice:  " + FORMAT$(AVEINV, "#######.##") + &
		"            " + &
		"Last Order:       " + PRNT_DATE(LASTORDER$, 8%) + &
		"            Fiscal YTD Sales:" + &
		FORMAT$(FIS_INVOICE, "#######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	RETURN

 Repaint:
	!**************************************************************
	! Repaint Customer name
	!**************************************************************

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		AR_35CUSTOM::CUSNUM, 2%, 20%,, SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		AR_35CUSTOM::CUSNAM, 6%, 3%,, SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		AR_35CUSTOM::ADD1, 7%, 3%,, SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		AR_35CUSTOM::ADD2, 8%, 3%,, SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		AR_35CUSTOM::CITY + " " + AR_35CUSTOM::STATE + " " + &
		AR_35CUSTOM::ZIP, 9%, 3%,, SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		AR_35CUSTOM::COUNTRY, 10%, 3%,, SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		PRNT_PHONE(AR_35CUSTOM::PHONE, 0%), 11%, 3%,, SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		AR_35CUSTOM::ALPSRT, 12%, 3%,, SMG$M_BOLD)

	RETURN

	%Page

 Initialize:
	!*******************************************************************
	! Set Initialize values
	!*******************************************************************

	AR_35CUSTOM::CUSNUM	= STRING$(10%, A"?"B)
	AR_35CUSTOM::CUSNAM	= STRING$(40%, A"?"B)
	AR_35CUSTOM::ADD1	= STRING$(25%, A"?"B)
	AR_35CUSTOM::ADD2	= STRING$(21%, A"?"B)
	AR_35CUSTOM::CITY	= STRING$(15%, A"?"B)
	AR_35CUSTOM::STATE	= STRING$(2%, A"?"B)
	AR_35CUSTOM::COUNTRY	= STRING$(8%, A"?"B)
	AR_35CUSTOM::ZIP	= STRING$(10%, A"?"B)
	AR_35CUSTOM::PHONE	= STRING$(10%, A"?"B)
	AR_35CUSTOM::ALPSRT	= STRING$(15%, A"?"B)

	RETURN

	%PAGE

 DumpInvoice:
	!*******************************************************************
	! Dump out one invoice
	!*******************************************************************

	!
	! Force item into left size for titles if none there
	!
	LEFT_OPEN(1%) = RIGHT_OPEN(1%) IF LEFT_OPEN% = 0%

	!
	! Calculate total for this invoice
	!
	INVTOTAL = 0.0
	INVTOTAL = INVTOTAL + LEFT_OPEN(LOOP%)::SALAMT &
		FOR LOOP% = 1% TO LEFT_OPEN%
	INVTOTAL = INVTOTAL + RIGHT_OPEN(LOOP%)::SALAMT &
		FOR LOOP% = 1% TO RIGHT_OPEN%

	!
	! Loop through all of the collected records
	!
	IF LEFT_OPEN% > RIGHT_OPEN%
	THEN
		ENDLOOP% = LEFT_OPEN%
	ELSE
		ENDLOOP% = RIGHT_OPEN%
	END IF

	FOR LOOP% = 1% TO ENDLOOP%
		!
		! Show invoice number only on first line of type
		!
		IF LOOP% = 1%
		THEN
			TEXT$ = LEFT_OPEN(LOOP%)::INVNUM + " " + &
				LEFT(LEFT_OPEN(LOOP%)::DESCR, 9%) + " " + &
				LEFT_OPEN(LOOP%)::ARACCT + " "
		ELSE
			TEXT$ = "         " + &
				"          " + &
				"                   "
		END IF

		!
		! Handle left column
		!
		IF LOOP% <= LEFT_OPEN%
		THEN
			TEXT$ = TEXT$ + &
				PRNT_DATE(LEFT_OPEN(LOOP%)::TRADAT, 6%) + " " + &
				FORMAT$(LEFT_OPEN(LOOP%)::SALAMT - &
				LEFT_OPEN(LOOP%)::DISAMT - &
				LEFT_OPEN(LOOP%)::OTHCHG, "#######.##") + &
				FORMAT$(LEFT_OPEN(LOOP%)::DISAMT, "######.## ") + &
				FORMAT$(LEFT_OPEN(LOOP%)::OTHCHG, "#####.## ") + &
					FORMAT$(LEFT_OPEN(LOOP%)::SALAMT, "#######.## ")

			CUS_SALAMT = CUS_SALAMT + LEFT_OPEN(LOOP%)::SALAMT
			CUS_DISAMT = CUS_DISAMT + LEFT_OPEN(LOOP%)::DISAMT
			CUS_OTHCHG = CUS_OTHCHG + LEFT_OPEN(LOOP%)::OTHCHG

		ELSE
			TEXT$ = TEXT$ + SPACE$(49%)
		END IF

		!
		! Handle right column
		!
		IF LOOP% <= RIGHT_OPEN%
		THEN
			TEXT$ = TEXT$ + &
				RIGHT_OPEN(LOOP%)::RECNUM + " " + &
				RIGHT_OPEN(LOOP%)::CHKNUM + " " + &
				PRNT_DATE(RIGHT_OPEN(LOOP%)::TRADAT, 6%) + &
				FORMAT$(-RIGHT_OPEN(LOOP%)::SALAMT, &
				"#######.## ")

			CUS_CREDIT = CUS_CREDIT + RIGHT_OPEN(LOOP%)::SALAMT
		ELSE
			TEXT$ = TEXT$ + SPACE$(35%)
		END IF

		!
		! Handle final total
		!
		IF LOOP% = ENDLOOP%
		THEN
			TEXT$ = TEXT$ + FORMAT$(INVTOTAL, "#######.##")
		END IF

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
		GOTO DumpInvoice1 IF UTL_REPORTX::STAT

	NEXT LOOP%

	!
	! Prepare for next invoice number
	!
	LEFT_OPEN% = 0%
	RIGHT_OPEN% = 0%
	THIS_INVOICE$ = AR_OPEN::INVNUM + ""

 DumpInvoice1:
	RETURN

 DumpInvoiceClosed:
	!*******************************************************************
	! Dump out one invoice
	!*******************************************************************

	!
	! Force item into left size for titles if none there
	!
	LEFT_CLOSED(1%) = RIGHT_CLOSED(1%) IF LEFT_CLOSED% = 0%

	!
	! Calculate total for this invoice
	!
	INVTOTAL = 0.0
	INVTOTAL = INVTOTAL + LEFT_CLOSED(LOOP%)::SALAMT &
		FOR LOOP% = 1% TO LEFT_CLOSED%
	INVTOTAL = INVTOTAL + RIGHT_CLOSED(LOOP%)::SALAMT &
		FOR LOOP% = 1% TO RIGHT_CLOSED%

	!
	! Loop through all of the collected records
	!
	IF LEFT_CLOSED% > RIGHT_CLOSED%
	THEN
		ENDLOOP% = LEFT_CLOSED%
	ELSE
		ENDLOOP% = RIGHT_CLOSED%
	END IF

	FOR LOOP% = 1% TO ENDLOOP%
		!
		! Show invoice number only on first line of type
		!
		IF LOOP% = 1%
		THEN
			TEXT$ = LEFT_CLOSED(LOOP%)::INVNUM + " " + &
				LEFT(LEFT_CLOSED(LOOP%)::DESCR, 9%) + " " + &
				LEFT_CLOSED(LOOP%)::ARACCT + " "
		ELSE
			TEXT$ = "         " + &
				"          " + &
				"                   "
		END IF

		!
		! Handle left column
		!
		IF LOOP% <= LEFT_CLOSED%
		THEN
			TEXT$ = TEXT$ + &
				PRNT_DATE(LEFT_CLOSED(LOOP%)::TRADAT, 6%) + " " + &
				FORMAT$(LEFT_CLOSED(LOOP%)::SALAMT - &
					LEFT_CLOSED(LOOP%)::DISAMT - &
					LEFT_CLOSED(LOOP%)::OTHCHG, "#######.##") + &
				FORMAT$(LEFT_CLOSED(LOOP%)::DISAMT, "######.## ") + &
				FORMAT$(LEFT_CLOSED(LOOP%)::OTHCHG, "#####.## ") + &
				FORMAT$(LEFT_CLOSED(LOOP%)::SALAMT, "#######.## ")

			CUS_SALAMT = CUS_SALAMT + LEFT_CLOSED(LOOP%)::SALAMT
			CUS_DISAMT = CUS_DISAMT + LEFT_CLOSED(LOOP%)::DISAMT
			CUS_OTHCHG = CUS_OTHCHG + LEFT_CLOSED(LOOP%)::OTHCHG

		ELSE
			TEXT$ = TEXT$ + SPACE$(49%)
		END IF

		!
		! Handle right column
		!
		IF LOOP% <= RIGHT_CLOSED%
		THEN
			TEXT$ = TEXT$ + &
				RIGHT_CLOSED(LOOP%)::RECNUM + " " + &
				RIGHT_CLOSED(LOOP%)::CHKNUM + " " + &
				PRNT_DATE(RIGHT_CLOSED(LOOP%)::TRADAT, 6%) + &
				FORMAT$(-RIGHT_CLOSED(LOOP%)::SALAMT, "#######.## ")

			CUS_CREDIT = CUS_CREDIT + RIGHT_CLOSED(LOOP%)::SALAMT
		ELSE
			TEXT$ = TEXT$ + SPACE$(35%)
		END IF

		!
		! Handle final total
		!
		IF LOOP% = ENDLOOP%
		THEN
			TEXT$ = TEXT$ + FORMAT$(INVTOTAL, "#######.##")
		END IF

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
		GOTO DumpInvoiceClosed1 IF UTL_REPORTX::STAT

	NEXT LOOP%

	!
	! Prepare for next invoice number
	!
	LEFT_CLOSED% = 0%
	RIGHT_CLOSED% = 0%
	THIS_INVOICE$ = AR_CLOSED::INVNUM + ""

 DumpInvoiceClosed1:
	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************

	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	RESUME HelpError

19999	END

20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"

	EXTERNAL LONG FUNCTION AR_MAIN_35CUSTOM

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE AR_MAIN_35CUSTOM.ID

		MAINT_GROUP = AR_MAIN_35CUSTOM(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
