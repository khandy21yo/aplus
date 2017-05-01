1	%TITLE "Accounts Payable Close File Maintenance"
	%SBTTL "AP_MAIN_37CLOSE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AP_MAIN_37CLOSE(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 2000 BY
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
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This option maintains the ^*Closed\* file.
	!	.lm -5
	!
	! Index:
	!	.x Close File Maintenance
	!	.x Maintenance>Close File
	!
	! Option:
	!
	!
	! Author:
	!
	!	07/05/2000 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_MAIN_37CLOSE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AP_MAIN_37CLOSE
	!	$ DELETE AP_MAIN_37CLOSE.OBJ;*
	!
	! Modification history:
	!
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%PAGE

	!
	! Include Statements
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	!
	! CDD and Maps
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AP.OPEN]AP_37CLOSE.HB"
	MAP (AP_37CLOSE)	AP_37CLOSE_CDD	AP_37CLOSE
	MAP (AP_37CLOSE_2)	AP_37CLOSE_CDD	AP_37CLOSE_OLD,	AP_37CLOSE2

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR)		AP_VENDOR_CDD	AP_VENDOR

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD	GL_CHART

	%INCLUDE "SOURCE:[AP.OPEN]AP_1099_TABLE.HB"
	MAP (AP_1099_TABLE)	AP_1099_TABLE_CDD	AP_1099_TABLE

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.HB"
	MAP (AP_CONTROL)	AP_CONTROL_CDD	AP_CONTROL

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_AP_37CLOSE) &
		AP_37CLOSE.CH%, &
		AP_37CLOSE.READONLY%, &
		YYYY_PP$ = 6%

	COM (CH_AP_CONTROL) &
		AP_CONTROL.CH%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION MAIN_WINDOW
	EXTERNAL LONG   FUNCTION FUNC_TESTENTRY

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!******************************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!******************************************************************
	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Accounts Payable Close Maintenance"
		SMG_WINDOW::NHELP = "AP_MAIN_37CLOSE"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 23%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 3%
		SMG_WINDOW::KNAME(0%) = "Vendor_tran"
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
		SMG_WINDOW::KNAME(1%) = "vendor_Inv"
			SMG_WINDOW::KFIELD(1%, 0%) = 2%
			SMG_WINDOW::KFIELD(1%, 1%) = 1%
			SMG_WINDOW::KFIELD(1%, 2%) = 4%
		SMG_WINDOW::KNAME(2%) = "Batch"
			SMG_WINDOW::KFIELD(2%, 0%) = 1%
			SMG_WINDOW::KFIELD(2%, 1%) = 22%

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Declare channels
		!
		IF AP_37CLOSE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AP_37CLOSE.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		%INCLUDE "SOURCE:[AP.OPEN]AP_37CLOSE.CRE"
		AP_37CLOSE.READONLY% = 0%

790		SMG_WINDOW::CHAN  = AP_37CLOSE.CH%
		RESET #AP_37CLOSE.CH%
		GET #AP_37CLOSE.CH%, REGARDLESS

	%PAGE

20100	!******************************************************************
	! Display the background
	!
	! This option is used to display the background information
	! on the screen.  It must first clear any junk on the screen,
	! and then write the background onto it.
	!******************************************************************
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	02, 04, "(01) Vendor Number", &
			04, 04, "(02) Trans. Number", &
			05, 04, "(03) Trans. Date", &
			07, 04, "(04) Invoice Number", &
			08, 04, "(05) Invoice Date", &
			09, 04, "(06) Invoice Amount", &
			11, 04, "(07) 1099 Code", &
			12, 04, "(08) 1099 Amount", &
			13, 04, "(09) Use Job Number", &
			14, 04, "(10) Use Amount", &
			16, 04, "(11) Discount Date", &
			17, 04, "(12) Discount Amount", &
			04, 41, "(13) Due Date", &
			05, 41, "(14) PO Number", &
			07, 41, "(15) AP   Account", &
			08, 41, "(16) Cash Account", &
			11, 41, "(17) Check Number", &
			12, 41, "(18) Check Date", &
			13, 41, "(19) Check Desc", &
			14, 41, "(20) Check Amount", &
			16, 41, "(21) Post Period", &
			17, 41, "(22) Batch #", &
			18, 41, "(23) Close Date", &
			0,  0, ""

		RESTORE

		READ XPOS%, YPOS%, XSTR$
		I% = 0%
		WHILE (XPOS% <> 0%)
			I% = I% + 1%
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				XSTR$, XPOS%, YPOS%) &
				IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%
			READ XPOS%, YPOS%, XSTR$
		NEXT

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

20200	!******************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display
	! data, set defaults, and return the data back according to
	! MFLAG.
	!******************************************************************
	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 Reenter2:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Vendor Number\*
	!	.b
	!	.lm +5
	!	The ^*Vendor Number\* field is used to assign an identification
	!	number of one (1) to ten (10) alphanumeric characters to be used
	!	for referencing a vendor.  It is recommended that all vendor numbers
	!	be the same length.
	!	.lm -5
	!
	! Index:
	!	.x Vendor Number
	!
	!--
			AP_37CLOSE::VENNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "02;24", TEMP$, &
				AP_37CLOSE::VENNUM, MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT
			CASE SMG$K_TRM_F14
				AP_37CLOSE::VENNUM = AP_VENDOR::VENNUM &
					IF MAIN_WINDOW(AP_MAIN_VENDOR.ID, "VX") = 1%
				GOTO Reenter2

			CASE SMG$K_TRM_F17
				ST% = MAIN_WINDOW(AP_MAIN_VENDOR.ID, &
					"M0" + AP_37CLOSE::VENNUM)
				GOTO Reenter2

			END SELECT

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Transaction Number\*
	!	.b
	!	.lm +5
	!	The ^*Transaction Number\* refers to the number of the transaction entered
	!	in the system.  The numbers are system assigned and will
	!	proceed in numerical order.
	!	.lm -5
	!
	! Index:
	!	.x Transaction Number>Close Maintenance
	!	.x Close Maintenance>Transaction Number
	!
	!--
			AP_37CLOSE::TRANKEY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "4;24", TEMP$, &
				AP_37CLOSE::TRANKEY, MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Transaction Date\*
	!	.b
	!	.lm +5
	!	The ^*Transaction Date\* refers to the date of the transaction.
	!	.lm -5
	!
	! Index:
	!	.x Transaction Date>Close Maintenance
	!	.x Close Maintenance>Transaction Date
	!
	!--
			AP_37CLOSE::TRANKEY_DATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "05;24", TEMP$, &
				AP_37CLOSE::TRANKEY_DATE, MFLAG, "8", &
				MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Invoice Number\*
	!	.b
	!	.lm +5
	!	A vendor's invoice number must be entered in the ^*Invoice Number\*
	!	field. A separate record should be added for each invoice received
	!	from a vendor. If a vendor does not furnish invoices, a user defined
	!	reference may be entered, for example, MAYSTMT,
	!	STMT09/87 or SC-Sep.
	!	.b
	!	This field will contain up to fifteen (15) alphanumeric
	!	characters.
	!	.lm -5
	!
	! Index:
	!	.x Invoice Number
	!	.x Number>Invoice
	!
	!--
			AP_37CLOSE::INVNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;24", TEMP$, &
				AP_37CLOSE::INVNUM, MFLAG, "'E", MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!	^* (05) Invoice Date\*
	!	.b
	!	.lm +5
	!	The ^*Invoice Date\* field contains the date which appears
	!	on the vendor's invoice in ^*MMDDYYYY\* format.
	!	.lm -5
	!
	! Index:
	!	.x Invoice Date>Close File Maintenance
	!
	!--
			AP_37CLOSE::INVDAT = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "8;29", TEMP$, &
				AP_37CLOSE::INVDAT, MFLAG, "8", MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Invoice Amount\*
	!	.b
	!	.lm +5
	!	The ^*Invoice Amount\* field enters the total
	!	gross dollar amount shown on a vendor's invoice.
	!	.lm -5
	!
	! Index:
	!	.x Invoice Amount>Close Maintenance
	!	.x Close Maintenance>Invoice Amount
	!
	!--
			AP_37CLOSE::INVAMT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "9;25", TEMP$, &
				AP_37CLOSE::INVAMT * 1.0, MFLAG, &
				"###,###,###.##", MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	^*(10) 1099 Amount\*
	!	.b
	!	.lm +5
	!	The ^*1099 Amount\* indicates the amount of the invoice or record to be
	!	included in the 1099.
	!	.lm -5
	!
	! Index:
	!	.x 1099 Amount>Close Maintenance
	!	.x Close Maintenance>1099 Amount
	!
	!--
			AP_37CLOSE::CODE_1099 = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "11;37", TEMP$, &
				AP_37CLOSE::CODE_1099, MFLAG, "'E", &
				MVALUE)

			SELECT SCOPE::SCOPE_EXIT
			CASE SMG$K_TRM_F14
				AP_37CLOSE::CODE_1099 = AP_1099_TABLE::CODE &
					IF MAIN_WINDOW(AP_MAIN_1099_TABLE.ID, "V0") = 1%
				GOTO Reenter2
			END SELECT

		CASE 8%

	!++
	! Abstract:FLD008
	!	^*(08) 1099 Amount\*
	!	.b
	!	.lm +5
	!	The ^*1099 Amount\* indicates the amount of the invoice or record to be
	!	included in the 1099 report.
	!	.lm -5
	!
	! Index:
	!	.x 1099 Amount>Close Maintenance
	!	.x Close Maintenance>1099 Amount
	!
	!--
			AP_37CLOSE::AMT_1099 = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "12;25", TEMP$, &
				AP_37CLOSE::AMT_1099 * 1.0, MFLAG, &
				"###,###,###.##", MVALUE)

		CASE 9%

	!++
	! Abstract:FLD009
	!	^*(09) Use Job Number\*
	!	.b
	!	.lm +5
	!	The ^*Use Job Number\* enters the work order or job
	!	number to which the Use tax should be charged.
	!	.lm -5
	!
	! Index:
	!	.x Use Job Number>Close  Maintenance
	!	.x Close Maintenance>Use Job Number
	!
	!--
			AP_37CLOSE::USE_JOB_NUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "13;29", TEMP$, &
				AP_37CLOSE::USE_JOB_NUM, MFLAG, "'E", &
				MVALUE)

		CASE 10%

	!++
	! Abstract:FLD010
	!	^*(10) Use Amount\*
	!	.b
	!	.lm +5
	!	The ^*Use Amount\* field enters the amount of Use
	!	tax that must be payed to allow for company use of the item within the
	!	state.
	!	.lm -5
	!
	! Index:
	!	.x Use Amount>Close Maintenance
	!	.x Close Maintenance>Use Amount
	!
	!--
			AP_37CLOSE::USE_AMT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "14;25", TEMP$, &
				AP_37CLOSE::USE_AMT * 1.0, MFLAG, &
				"###,###,###.##", MVALUE)

		CASE 11%

	!++
	! Abstract:FLD011
	!	^*(11) Discount Date\*
	!	.b
	!	.lm +5
	!	The ^*Discount Date\* is calculated and entered by the system
	!	according to the discount days or discount date terms which have
	!	been entered in the Vendor Master File record for the vendor.
	!	If both the discount days ^&and\& the discount date fields in the
	!	Vendor Master File are blank, the automatically entered discount
	!	date will be equal to the invoice date entered in the Purchases
	!	Journal record. To accept the Discount Date displayed on the screen,
	!	press ^*<Ent>\*. To override the system determined discount date, type
	!	the appropriate date in MMDDYYYY format and press ^*<Ent>\*.
	!	.lm -5
	!
	! Index:
	!	.x Discount Date>Close Maintenance
	!	.x Close Maintenance>Discount Date
	!
	!--
			AP_37CLOSE::DISCDAT = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "16;29", TEMP$, &
				AP_37CLOSE::DISCDAT, MFLAG, "8", MVALUE)

		CASE 12%

	!++
	! Abstract:FLD012
	!	^*(12) Discount Amount\*
	!	.b
	!	.lm +5
	!	The discount percentage, entered in the Vendor Master File
	!	for the subject vendor, will cause the system to calculate the
	!	discount amount and automatically enter that amount in this field.
	!	If the discount percentage in the Vendor Master File is zero, the
	!	calculated result will be zero. The automatically entered amount
	!	may be overridden by typing a different amount and pressing ^*<Ent>\*.
	!	.lm -5
	!
	! Index:
	!	.x Discount Amount>Close Maintenance
	!	.x Close Maintenance>Discount Amount
	!
	!--
			AP_37CLOSE::DISAMT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "17;25", TEMP$, &
				AP_37CLOSE::DISAMT * 1.0, MFLAG, &
				"###,###,###.##", MVALUE)

		CASE 13%

	!++
	! Abstract:FLD013
	!	^*(13) Due Date\*
	!	.b
	!	.lm +5
	!	The ^*Due Date\* is determined and entered automatically according
	!	to the due date or due days terms which have been entered in the
	!	Vendor Master File for the subject vendor. If both the due date
	!	^&and\& due days fields in the Vendor Master File are blank, the
	!	automatically entered date in the Due Date field will be equal to
	!	the invoice date entered in the Purchases Journal record. To accept
	!	the date displayed on the screen, press ^*<Ent>\*. To override the
	!	system determined date, type the appropriate date in MMDDYYYY format
	!	and press ^*<Ent>\*.
	!	.lm -5
	!
	! Index:
	!	.x Due Date>Close Maintenance
	!	.x Close Maintenance>Due Date
	!
	!--
			AP_37CLOSE::DUEDAT = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "4;59", TEMP$, &
				AP_37CLOSE::DUEDAT, MFLAG, "8", MVALUE)

		CASE 14%

	!++
	! Abstract:FLD014
	!	^*(14) Purchase Order Number\*
	!	.b
	!	.lm +5
	!	The ^*Purchase Order Number\* field enters the number
	!	of the ordering form used when ordering the merchandise.
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order Number>Close  Maintenance
	!	.x Close Maintenance>Purchase Order Number
	!
	!--
			AP_37CLOSE::PONUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "5;59", TEMP$, &
				AP_37CLOSE::PONUM, MFLAG OR 2%, "~R 'E", MVALUE)

		CASE 15%

	!++
	! Abstract:FLD015
	!	^*(15) Accounts Payable Account \*
	!	.b
	!	.lm +5
	!	During initialization procedures, the ^*Accounts Payable Account\* field of
	!	the Accounts Payable Closed Maintenance File enters the
	!	General Ledger Chart of Accounts number for Accounts Payable.
	!	.b
	!	The presence of the Accounts Payable account number in this
	!	field will cause the number to be defaulted in appropriate fields
	!	in other areas of the Accounts Payable system.
	!	.lm -5
	!
	! Index:
	!	.x Account>Close Maintenance
	!	.x Close Maintenance>Account
	!
	!--
			AP_37CLOSE::AP_ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;59", TEMP$, &
				AP_37CLOSE::AP_ACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
				THEN
					AP_37CLOSE::AP_ACCT = GL_CHART::ACCT
				END IF
				GOTO Reenter2
			END IF

		CASE 16%

	!++
	! Abstract:FLD016
	!	^*(16) Cash Account \*
	!	.b
	!	.lm +5
	!	During initialization procedures the General Ledger Chart
	!	of account number for the cash account, representing the bank
	!	from which funds are drawn in payment of accounts payable, should be
	!	entered in the ^*Cash Account\* field of the Accounts Payable
	!	Control File.
	!	.b
	!	The presence of the Cash account number in this field will
	!	cause the number to be defaulted in appropriate fields in other
	!	areas of the Accounts Payable system.
	!	.lm -5
	!
	! Index:
	!	.x Cash Account>Close Maintenance
	!	.x Close Maintenance>Cash Account
	!
	!--
			AP_37CLOSE::CASH_ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "8;59", TEMP$, &
				AP_37CLOSE::CASH_ACCT, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
				THEN
					AP_37CLOSE::CASH_ACCT = GL_CHART::ACCT
				END IF
				GOTO Reenter2
			END IF

		CASE 17%

	!++
	! Abstract:FLD017
	!	^*(17) Check Number\*
	!	.b
	!	.lm +5
	!	When an entry is made in the Purchases Journal for a vendor's
	!	charge for which payment has already been made with a hand written
	!	check, the number of the check which was used to make payment must
	!	be entered in this field.
	!	.b
	!	If payment has not been made relative to the vendor's charge,
	!	this field must be left blank.
	!	.lm -5
	!
	! Index:
	!	.x Close Maintenance>Check Number
	!	.x Check Number>Close Maintenance
	!
	!--
			AP_37CLOSE::CKNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "11;59", TEMP$, &
				AP_37CLOSE::CKNUM, MFLAG, "'E", MVALUE)

		CASE 18%

	!++
	! Abstract:FLD018
	!	^*(18) Check Date\*
	!	.b
	!	.lm +5
	!	If a ^*Check Number\* is entered in field (12) in a Purchases
	!	Journal record, the cursor will stop at the ^*Check Date\* field
	!	in order for the appropriate check date to be entered in MMDDYYYY
	!	format.
	!	.b
	!	If the Check _# field is blank the system will automatically
	!	skip the ^*Check Date\* field.
	!	.lm -5
	!
	! Index:
	!	.x Check Date>Close Maintenance
	!	.x Close Maintenance>Check Date
	!
	!--
			AP_37CLOSE::CKDAT = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "12;59", TEMP$, &
				AP_37CLOSE::CKDAT, MFLAG, "8", MVALUE)

		CASE 19%

	!++
	! Abstract:FLD019
	!	^*(19) Check Description\*
	!	.b
	!	.lm +5
	!	The ^*Check Description\* field describes the information entered on the
	!	stub of the check.
	!	.lm -5
	!
	! Index:
	!	.x Check Description>Close Maintenance
	!	.x Close Maintenance>Check Description
	!
	!--
			AP_37CLOSE::CKDESC = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "13;59", TEMP$, &
				AP_37CLOSE::CKDESC, MFLAG, "'E", MVALUE)

		CASE 20%

	!++
	! Abstract:FLD020
	!	^*(20) Check Amount\*
	!	.b
	!	.lm +5
	!	If a ^*check number\* has been entered in field (12) of a
	!	Purchases Journal record, the cursor will stop in the ^*Check
	!	Amount\* field in order for the dollar amount of the check to be
	!	entered.
	!	.b
	!	If the Check _# field is blank, the ^*Check Amount\* field will
	!	be automatically bypassed.
	!	.lm -5
	!
	! Index:
	!	.x Check Amount>Close Maintenance
	!	.x Close Maintenance>Check Amount
	!
	!--
			AP_37CLOSE::CKAMT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "14;59", TEMP$, &
				AP_37CLOSE::CKAMT * 1.0, MFLAG, &
				"###,###,###.##", MVALUE)

		CASE 21%

	!++
	! Abstract:FLD021
	!	^*(21) Post Period\*
	!	.b
	!	.lm +5
	!	The ^*Post Period\* field will indicate the period in which the
	!	transaction was posted.
	!	.lm -5
	!
	! Index:
	!	.x Post Period>Close Maintenance
	!	.x Close Maintenance>Post Period
	!
	!--
			AP_37CLOSE::UPDATED = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "16;59", TEMP$, &
				AP_37CLOSE::UPDATED, MFLAG, "8", &
				MVALUE)

		CASE 22%

	!++
	! Abstract:FLD022
	!
	! Index:
	!
	!--
			AP_37CLOSE::BATCH = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "17;59", TEMP$, &
				AP_37CLOSE::BATCH, MFLAG, "'E", MVALUE)

		CASE 23%

	!++
	! Abstract:FLD023
	!
	! Index:
	!
	!--
			AP_37CLOSE::CLOSEDATE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "18;59", TEMP$, &
				AP_37CLOSE::CLOSEDATE, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
21300	CASE OPT_TESTENTRY
		AP_MAIN_37CLOSE = 0%

		SELECT MLOOP
		CASE 1%
			AP_MAIN_37CLOSE = FUNC_TESTENTRY(SMG_WINDOW, &
				AP_37CLOSE::VENNUM, AP_VENDOR::VENNAM, &
				"AP", MLOOP, "VENNUM", &
				"Vendor # ", AP_MAIN_VENDOR.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AP_VENDOR::VENNAM, 2%, 41%,, SMG$M_BOLD)

		CASE 7%
			AP_MAIN_37CLOSE = FUNC_TESTENTRY(SMG_WINDOW, &
				AP_37CLOSE::CODE_1099, AP_1099_TABLE::DESCR, &
				"AP", MLOOP, "1099CD", &
				"1099 code", AP_MAIN_1099_TABLE.ID)

		CASE 15%, 16%
			!
			! Is the input defined?
			!
			TEMP$ = AP_37CLOSE::AP_ACCT
			TEMP$ = STRING$(LEN(TEMP$), 63%) IF TEMP$ = ""
			TEMP$ = AP_37CLOSE::CASH_ACCT IF MLOOP = 16%

			AP_MAIN_37CLOSE = FUNC_TESTENTRY(SMG_WINDOW, &
				TEMP$, TEMP$, &
				"AP", MLOOOP, "ACCT", &
				"Account number ", GL_MAIN_CHART.ID)

		END SELECT

	!
	! Set AP_37CLOSE_OLD value
	!
21500	CASE OPT_SETOLD
		AP_37CLOSE_OLD = AP_37CLOSE

	!
	! Restore AP_37CLOSE_OLD value
	!
	CASE OPT_RESETOLD
		AP_37CLOSE = AP_37CLOSE_OLD
		GOSUB GetTrans

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		AP_37CLOSE2 = AP_37CLOSE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		AP_37CLOSE = AP_37CLOSE2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP
		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Vendor #   Trans# " + &
				"Invoice Number  Invoice Date " + &
				"Invoice Amount Check Amount"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "013,020,036,049,064"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = AP_37CLOSE::VENNUM + " " + &
				AP_37CLOSE::TRANKEY + " " + &
				AP_37CLOSE::INVNUM + "  " + &
				PRNT_DATE(AP_37CLOSE::INVDAT, 8%) + "  " + &
				FORMAT$(AP_37CLOSE::INVAMT, &
					"###,###,###.##") + &
				" " + FORMAT$(AP_37CLOSE::CKAMT, &
					"###,###,###.##")

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #AP_37CLOSE.CH%, KEY #0% &
				GE AP_37CLOSE::VENNUM + AP_37CLOSE::TRANKEY, &
				REGARDLESS
		CASE 1%
			FIND #AP_37CLOSE.CH%, KEY #1% &
				GE AP_37CLOSE::VENNUM + AP_37CLOSE::INVNUM, &
				REGARDLESS
		CASE 2%
			FIND #AP_37CLOSE.CH%, KEY #2% &
				GE AP_37CLOSE::BATCH + "", &
				REGARDLESS
		END SELECT
	END SELECT

 ExitFunction:
	EXIT FUNCTION

 GetTrans:
28000	!*******************************************************************
	! This subroutine will assign an Transaction number from the control
	! file AP_CONTROL.  It will make sure that the number it is trying
	! to assign does not already exist.
	!*******************************************************************

28020	!
	! Read in the control record
	!
	WHEN ERROR IN
		GET #AP_CONTROL.CH%, RECORD 1%
	USE
		AP_37CLOSE::TRANKEY = "000000"
		CONTINUE 28070
	END WHEN

28060	!
	! We have a key to try now
	!
	TEMP = VAL(AP_CONTROL::LAST_TRANKEY) + 1.0

	IF TEMP > 1000000.0
	THEN
		AP_CONTROL::LAST_TRANKEY = "000000"
	ELSE
		AP_CONTROL::LAST_TRANKEY = FORMAT$(TEMP, "<0>#####")
	END IF

	AP_37CLOSE::TRANKEY = AP_CONTROL::LAST_TRANKEY

	UPDATE #AP_CONTROL.CH%

28070	RETURN

	%PAGE

29000	!
	! Trap Errors
	!
	RESUME ExitFunction

32767	END FUNCTION
