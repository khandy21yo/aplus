1	%TITLE "Accounts Payable Ledger File Maintenance"
	%SBTTL "AP_MAIN_OPEN_MAINT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AP_MAIN_OPEN_MAINT(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	! Abstract:HELP
	!	.p
	!	The ^*Accounts Payable Ledger File Maintenance\* option provides
	!	for editing or maintaining of records in either any
	!	Accounts Payable Ledger Files.
	!
	! Index:
	!	.x Ledger File Maintenance
	!	.x Maintain>Accounts Payable Ledger File
	!
	! Option:
	!
	!	AP_MAIN_OPEN_MAINT$CLOSE
	!	AP_MAIN_OPEN_MAINT$DIST
	!
	! Author:
	!
	!	08/02/88 - Lance Williams
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_MAIN_OPEN_MAINT/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AP_MAIN_OPEN_MAINT
	!	$ DELETE AP_MAIN_OPEN_MAINT.OBJ;*
	!
	! Modification history:
	!
	!	10/13/88 - Kevin Handy
	!		Added distribution journal maintenance.
	!
	!	06/07/89 - Aaron Redd
	!		Modified to show Vendor name after the
	!		user inputs the Vendor number.
	!
	!	09/06/89 - Kevin Handy
	!		Modified to allow modification to batch number
	!		field.
	!
	!	07/05/90 - Kevin Handy
	!		Modified to use formatted PO number.
	!
	!	08/27/90 - Kevin Handy
	!		Modified to allow find/view on all keys.
	!
	!	08/27/90 - Kevin Handy
	!		Right fill transaction number with zeroes.
	!
	!	10/29/90 - Kevin Handy
	!		Modified so exit key on vendor number will work.
	!
	!	04/05/91 - Frank F. Starman
	!		Fix bug to allow find and view by secondary keys.
	!
	!	03/05/92 - Dan Perkins
	!		Changed ENTR_3PO to ENTR_3STRING.
	!
	!	02/11/93 - Dan Perkins
	!		Changed "V0" to "VX" on chard of accounts to be
	!		able to list accounts starting at a particular
	!		account.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	07/03/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/09/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	06/08/99 - Kevin Handy
	!		Lose lines 760, 770 (Dead code)
	!
	!	11/06/99 - Kevin Handy
	!		Lose need for AP_CLOSE references.
	!
	!	03/14/2001 - Kevin Handy
	!		Moved menu options into MAST.
	!
	!	03/15/2003 - Kevin Handy
	!		Change Post Period from a date to a period field.
	!--

	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include Statements
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"

	!
	! CDD and Maps
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP (AP_OPEN)		AP_OPEN_CDD	AP_OPEN
	MAP (AP_OPEN2)	AP_OPEN_CDD	AP_OPEN_OLD,	AP_OPEN2

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
	COM (CH_AP_REG_MAINT) &
		AP_OPEN.CH%, &
		AP_OPEN.READONLY%

	COM (CH_AP_CONTROL) &
		AP_CONTROL.CH%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG   FUNCTION MAIN_WINDOW

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
		SMG_WINDOW::DESCR = "Accounts Payable Open Maintenance"
		SMG_WINDOW::NHELP = "AP_MAIN_OPEN_MAINT"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 130%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 22%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 3%
		SMG_WINDOW::KNAME(0%) = "Vendor_number"
		SMG_WINDOW::KFIELD(0%, 0%) = 2%
		SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KFIELD(0%, 2%) = 2%
		SMG_WINDOW::KNAME(1%) = "Invoice_number"
		SMG_WINDOW::KFIELD(1%, 0%) = 2%
		SMG_WINDOW::KFIELD(1%, 1%) = 1%
		SMG_WINDOW::KFIELD(1%, 2%) = 4%
		SMG_WINDOW::KNAME(2%) = "Batch_number"
		SMG_WINDOW::KFIELD(2%, 0%) = 1%
		SMG_WINDOW::KFIELD(2%, 1%) = 22%

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Declare channels
		!
		IF AP_OPEN.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AP_OPEN.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.CRE"
		AP_OPEN.READONLY% = 0%
		GOTO 790

 !760
		!
		! If unable to open for modify, try to open
		! with read access only.
		!
 !		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.OPN"
 !		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
 !		AP_OPEN.READONLY% = -1%
 !
 !		GOTO 790
 !
 !770
		!
		! File not able to open, so reset channel
		!
 !		CALL ASSG_FREECHANNEL(AP_OPEN.CH%)
 !
 !		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = AP_OPEN.CH%
		RESET #AP_OPEN.CH%
		GET #AP_OPEN.CH%, REGARDLESS

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
			13, 04, "(09) Use Tax Sub Acct", &
			14, 04, "(10) Use Tax Amount", &
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

 Reenter1:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Vendor Number\*
	!	.P
	!	The ^*Vendor Number\* field contains the identification number of the desired
	!	vendor.  The vendor must first be established in the ^*Vendor Master File\*.
	!	Pressing the ^*List Choices\* key displays a list of the valid vendor numbers.
	!
	!
	! Index:
	!	.x Vendor Number
	!
	!--
			AP_OPEN::VENNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "02;24", TEMP$, &
				AP_OPEN::VENNUM, MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14
				AP_OPEN::VENNUM = AP_VENDOR::VENNUM &
					IF MAIN_WINDOW(AP_MAIN_VENDOR.ID, "VX") = 1%
				GOTO Reenter1

			CASE SMG$K_TRM_F17
				ST% = MAIN_WINDOW(AP_MAIN_VENDOR.ID, &
					"M0" + AP_OPEN::VENNUM)
				GOTO Reenter1

			CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
				! BLANK CASE

			CASE ELSE
				IF MAIN_WINDOW(AP_MAIN_VENDOR.ID, "Q0" + AP_OPEN::VENNUM) <> 1%
				THEN
					AP_VENDOR::VENNAM = "????????????????????"
				END IF

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					LEFT(AP_VENDOR::VENNAM, 30%), &
					2%, 40%, , SMG$M_BOLD)
			END SELECT

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Transaction Number\*
	!	.p
	!	The ^*Transaction Number\* refers to the number of the transaction for the
	!	current situation. The numbers are assigned when the trade takes place and
	!	proceed in numerical order
	!
	! Index:
	!	.x Transaction Number>Open Maintenance
	!	.x Open Maintenance>Transaction Number
	!
	!--
			AP_OPEN::TRANKEY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "4;24", TEMP$, &
				AP_OPEN::TRANKEY, MFLAG, "~L0'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Transaction Date\*
	!	.p
	!	The ^*Transaction Date\* refers to the date that the transaction takes place.
	!
	! Index:
	!	.x Transaction Date>Open Maintenance
	!	.x Open Maintenance>Transaction Date
	!
	!--
			AP_OPEN::TRANKEY_DATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "05;24", TEMP$, &
				AP_OPEN::TRANKEY_DATE, MFLAG, "8", &
				MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Invoice Number\*
	!	.P
	!	A vendor's invoice number must be entered in the ^*Invoice Number\*
	!	field. A separate record should be added for each invoice received
	!	from a vendor. If a vendor does not furnish invoices, a user defined
	!	reference may be entered, for example, MAYSTMT,
	!	STMT09/87 or SC-Sep.
	!	.P
	!	This field will contain up to fifteen (15) alphanumeric
	!	characters.
	!
	! Index:
	!	.x Invoice Number
	!	.x Number>Invoice
	!
	!--
			AP_OPEN::INVNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;24", TEMP$, &
				AP_OPEN::INVNUM, MFLAG, "'E", MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!	^* (05) Invoice Date\*
	!	.P
	!	The ^*Invoice Date\* field contains the date which appears
	!	on the vendor's invoice in ^*MMDDYYYY\* format.
	!
	! Index:
	!	.x Invoice Date>Ledger File Maintenance
	!
	!--
			AP_OPEN::INVDAT = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "8;29", TEMP$, &
				AP_OPEN::INVDAT, MFLAG, "8", MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Invoice Amount\*
	!	.P
	!	The ^*Invoice Amount\* field enters the total
	!	gross dollar amount shown on a vendor's invoice.
	!
	! Index:
	!	.x Invoice Amount>Open Maintenance
	!	.x Open Maintenance>Invoice Amount
	!
	!--
			AP_OPEN::INVAMT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "9;25", TEMP$, &
				AP_OPEN::INVAMT * 1.0, MFLAG, &
				"###,###,###.##", MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	^*(07) 1099 Code\*
	!	.p
	!	If the 1099 flag for the subject vendor
	!	is set to ^*N\*, the system will automatically bypass this field. If the flag
	!	is set to ^*Y\*, the cursor will stop at the ^*1099 Code\* field, requiring
	!	that a code be entered.
	!	.p
	!	The code entered should be equal to a 1099 code that has already been added
	!	to the 1099 Table. If an invalid code is entered the system will prompt
	!	the user to re-enter a valid code or accept the invalid code, which must
	!	subsequently be validated by adding that code in the 1099 Table.
	!
	! Index:
	!	.x 1099 Code>Open Maintenance
	!	.x Open Maintenance>1099 Code
	!
	!--
			AP_OPEN::CODE_1099 = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "11;37", TEMP$, &
				AP_OPEN::CODE_1099, MFLAG, "'E", &
				MVALUE)

			SELECT SCOPE::SCOPE_EXIT
			CASE SMG$K_TRM_F14
				AP_OPEN::CODE_1099 = AP_1099_TABLE::CODE &
					IF MAIN_WINDOW(AP_MAIN_1099_TABLE.ID, "V0") = 1%
				GOTO Reenter1
			END SELECT

		CASE 8%

	!++
	! Abstract:FLD008
	!	^*(08) 1099 Amount\*
	!	.p
	!	The ^*1099 Amount\* indicates the amount of the invoice or record to be
	!	included in the 1099 report. Usually the Amount contains all that was in the
	!	record.
	!
	! Index:
	!	.x 1099 Amount>Open Maintenance
	!	.x Open Maintenance>1099 Amount
	!
	!--
			AP_OPEN::AMT_1099 = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "12;25", TEMP$, &
				AP_OPEN::AMT_1099 * 1.0, MFLAG, &
				"###,###,###.##", MVALUE)

		CASE 9%

	!++
	! Abstract:FLD009
	!	^*(09) Use Tax Sub Account\*
	!	.p
	!	The ^*Use Tax Sub Account\* enters of the work order or job
	!	number to which the Use tax should be charged.
	!
	! Index:
	!	.x Sub Account>Use Tax>Open Maintenance
	!	.x Open Maintenance>Sub Account>Use Tax
	!	.x Use Tax>Sub Account>Open Maintenance
	!
	!--
			AP_OPEN::USE_JOB_NUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "13;29", TEMP$, &
				AP_OPEN::USE_JOB_NUM, MFLAG, "'E", &
				MVALUE)

		CASE 10%

	!++
	! Abstract:FLD010
	!	^*(10) Use Tax Amount\*
	!	.p
	!	The ^*Use Tax Amount\* field enters the amount of Use
	!	tax that must be payed to allow for company use of the item within the
	!	state.
	!
	! Index:
	!	.x Use Tax Amount>Open Maintenance
	!	.x Open Maintenance>Use Tax Amount
	!
	!--
			AP_OPEN::USE_AMT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "14;25", TEMP$, &
				AP_OPEN::USE_AMT * 1.0, MFLAG, &
				"###,###,###.##", MVALUE)

		CASE 11%

	!++
	! Abstract:FLD011
	!	^*(11) Discount Date\*
	!	.P
	!	The ^*Discount Date\* is determined and entered automatically
	!	according to the discount days or discount date terms which have
	!	been entered in the Vendor Master File record for the subject vendor.
	!	If both the discount days ^&and\& the discount date fields in the
	!	Vendor Master File are blank, the automatically entered discount
	!	date will be equal to the invoice date entered in the Purchases
	!	Journal record. To accept the Discount Date displayed on the screen,
	!	press ^*<Ent>\*. To override the system determined discount date, type
	!	the appropriate date in MMDDYYYY format and press ^*<Ent>\*.
	!
	! Index:
	!	.x Discount Date>Open Maintenance
	!	.x Open Maintenance>Discount Date
	!
	!--
			AP_OPEN::DISCDAT = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "16;29", TEMP$, &
				AP_OPEN::DISCDAT, MFLAG, "8", MVALUE)

		CASE 12%

	!++
	! Abstract:FLD012
	!	^*(12) Discount Amount\*
	!	.P
	!	The discount percentage entered in the Vendor Master File
	!	for the subject vendor will cause the system to calculate the
	!	discount amount and automatically enter that amount in this field.
	!	If the discount percentage in the Vendor Master File is zero, the
	!	calculated result will be zero. The automatically entered amount
	!	may be overridden by typing a different amount and pressing ^*<Ent>\*.
	!
	! Index:
	!	.x Discount Amount>Open Maintenance
	!	.x Open Maintenance>Discount Amount
	!
	!--
			AP_OPEN::DISAMT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "17;25", TEMP$, &
				AP_OPEN::DISAMT * 1.0, MFLAG, &
				"###,###,###.##", MVALUE)

		CASE 13%

	!++
	! Abstract:FLD013
	!	^*(13) Due Date\*
	!	.P
	!	The ^*Due Date\* is determined and entered automatically according
	!	to the due date or due days terms which have been entered in the
	!	Vendor Master File for the subject vendor. If both the due date
	!	^&and\& due days fields in the Vendor Master File are blank, the
	!	automatically entered date in the Due Date field will be equal to
	!	the invoice date entered in the Purchases Journal record. To accept
	!	the date displayed on the screen, press ^*<Ent>\*. To override the
	!	system determined date, type the appropriate date in MMDDYYYY format
	!	and press ^*<Ent>\*.
	!
	! Index:
	!	.x Due Date>Open Maintenance
	!	.x Open Maintenance>Due Date
	!
	!--
			AP_OPEN::DUEDAT = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "4;59", TEMP$, &
				AP_OPEN::DUEDAT, MFLAG, "8", MVALUE)

		CASE 14%

	!++
	! Abstract:FLD014
	!	^*(14) Purchase Order Number\*
	!	.p
	!	The ^*Purchase Order Number\* field enters the number
	!	of the ordering form used when ordering the merchandise.
	!
	! Index:
	!	.x Purchase Order Number>Open Maintenance
	!	.x Open Maintenance>Purchase Order Number
	!
	!--
			AP_OPEN::PONUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "5;59", TEMP$, &
				AP_OPEN::PONUM, MFLAG OR 2%, "~R 'E", MVALUE)

		CASE 15%

	!++
	! Abstract:FLD015
	!	^*(15) Accounts Payable Account \*
	!	.p
	!	During initialization procedures, the ^*Accounts Payable Account\* field of
	!	the Accounts Payabe Open Maintenance provides for the entry of the General
	!	Ledger Chart of Accounts number for Accounts Payable.
	!	.p
	!	The presence of the Accounts Payable account number in this
	!	field will cause the number to be defaulted in appropriate fields
	!	in other areas of the Accounts Payable system.
	!
	! Index:
	!	.x Account>Open Maintenance
	!	.x Open Maintenance>Account
	!
	!--
			AP_OPEN::AP_ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;59", TEMP$, &
				AP_OPEN::AP_ACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
				THEN
					AP_OPEN::AP_ACCT = GL_CHART::ACCT
				END IF
				GOTO Reenter1
			END IF

		CASE 16%

	!++
	! Abstract:FLD016
	!	^*(16) Cash Account \*
	!	.p
	!	During initialization procedures the General Ledger Chart
	!	of account number for the cash account, representing the bank
	!	from which funds are drawn in payment of accounts payable, should be
	!	entered in the ^*Cash Account \* field of the Accounts Payable
	!	Open Maintenance File.
	!	.p
	!	The presence of the Cash account number in this field will
	!	cause the number to be defaulted in appropriate fields in other
	!	areas of the Accounts Payable system.
	!
	! Index:
	!	.x Cash Account>Open Maintenance
	!	.x Open Maintenance>Cash Account
	!
	!--
			AP_OPEN::CASH_ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "8;59", TEMP$, &
				AP_OPEN::CASH_ACCT, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
				THEN
					AP_OPEN::CASH_ACCT = GL_CHART::ACCT
				END IF
				GOTO Reenter1
			END IF

		CASE 17%

	!++
	! Abstract:FLD017
	!	^*(17) Check Number\*
	!	.P
	!	When an entry is made in the Purchases Journal for a vendor's
	!	charge for which payment has already been made with a hand written
	!	check, the number of the check which was used to make payment must
	!	be entered in this field.
	!	.P
	!	If payment has not been made relative to the vendor's charge,
	!	this field must be left blank.
	!
	! Index:
	!	.x Open Maintenance>Check Number
	!	.x Check Number>Open Maintenance
	!
	!--
			AP_OPEN::CKNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "11;59", TEMP$, &
				AP_OPEN::CKNUM, MFLAG, "'E", MVALUE)

		CASE 18%

	!++
	! Abstract:FLD018
	!	^*(18) Check Date\*
	!	.P
	!	If a ^*check number\* is entered in field (12) in a Purchases
	!	Journal record, the cursor will stop at the ^*Check Date\* field
	!	in order for the appropriate check date to be entered in MMDDYYYY
	!	format.
	!	.P
	!	If the Check _# field is blank the system will automatically
	!	skip the ^*Check Date\* field.
	!
	!
	! Index:
	!	.x Check Date>Open Maintenance
	!	.x Open Maintenance>Check Date
	!
	!--
			AP_OPEN::CKDAT = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "12;59", TEMP$, &
				AP_OPEN::CKDAT, MFLAG, "8", MVALUE)

		CASE 19%

	!++
	! Abstract:FLD019
	!	^*(19) Check Description\*
	!	.p
	!	The ^*Check Description\* field describes the information entered on the
	!	stub of the check.
	!
	!
	! Index:
	!	.x Check Description>Open Maintenance
	!	.x Open Maintenance>Check Description
	!
	!--
			AP_OPEN::CKDESC = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "13;59", TEMP$, &
				AP_OPEN::CKDESC, MFLAG, "'E", MVALUE)

		CASE 20%

	!++
	! Abstract:FLD020
	!	^*(20) Check Amount\*
	!	.P
	!	If a ^*check number\* has been entered in field (12) of a
	!	Purchases Journal record, the cursor will stop in the ^*Check
	!	Amount\* field in order for the dollar amount of the check to be
	!	entered.
	!	.P
	!	If the Check _# field is blank, the ^*Check Amount\* field will
	!	be automatically bypassed.
	!
	! Index:
	!	.x Check Amount>Open Maintenance
	!	.x Open Maintenance>Check Amount
	!
	!--
			AP_OPEN::CKAMT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "14;59", TEMP$, &
				AP_OPEN::CKAMT * 1.0, MFLAG, &
				"###,###,###.##", MVALUE)

		CASE 21%

	!++
	! Abstract:FLD021
	!	^*(21) Post Period\*
	!	.p
	!	The ^*Post Period\* enters the period in which the
	!	transaction will be posted. The date must be entered in the MMDDYYYY format.
	!
	! Index:
	!	.x Post Period>Open Maintenance
	!	.x Open Maintenance>Post Period
	!
	!--
			AP_OPEN::UPDATED = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "16;59", TEMP$, &
				AP_OPEN::UPDATED, MFLAG, "'E", MVALUE)

		CASE 22%

	!++
	! Abstract:FLD022
	!	^*(22) Batch Number\*
	!	.P
	!	The ^*Batch Number\* field determines which batch
	!	file will be printed. The ^*Batch Number\* field must contain a
	!	valid value, a batch number which contains data.
	!
	! Index:
	!	.x Batch Number>Open Maintenance
	!	.x Open Maintenance>Batch Number
	!
	!--
			AP_OPEN::BATCH = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "17;59", "", &
				AP_OPEN::BATCH, MFLAG, "'E", "")

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		AP_MAIN_OPEN_MAINT = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Is the input defined?
			!
			AP_MAIN_OPEN_MAINT = FUNC_TESTENTRY(SMG_WINDOW, &
				AP_OPEN::VENNUM, AP_VENDOR::VENNAM, &
				"AP", MLOOP, "VENNUM", &
				"Vendor number", AP_MAIN_VENDOR.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(AP_VENDOR::VENNAM, 30%), &
				2%, 40%, , SMG$M_BOLD)

		CASE 7%
			!
			! Is the input defined?
			!
			AP_MAIN_OPEN_MAINT = FUNC_TESTENTRY(SMG_WINDOW, &
				AP_OPEN::CODE_1099, AP_1099_TABLE::DESCR, &
				"AP", MLOOP, "1099CD", &
				"1099 code", AP_MAIN_1099_TABLE.ID)

		CASE 15%, 16%
			!
			! Is the input defined?
			!
			TEMP$ = AP_OPEN::AP_ACCT
			TEMP$ = STRING$(LEN(TEMP$), 63%) IF TEMP$ = ""
			TEMP$ = AP_OPEN::CASH_ACCT IF MLOOP = 16%

			AP_MAIN_OPEN_MAINT = FUNC_TESTENTRY(SMG_WINDOW, &
				TEMP$,TEMP$, &
				"AP", MLOOP, "ACCT", &
				"Account number ", GL_MAIN_CHART.ID)

		END SELECT

	!
	! Set AP_OPEN_OLD value
	!
20500	CASE OPT_SETOLD
		AP_OPEN_OLD = AP_OPEN

	!
	! Restore AP_OPEN_OLD value
	!
	CASE OPT_RESETOLD
		AP_OPEN = AP_OPEN_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		AP_OPEN2 = AP_OPEN

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		AP_OPEN = AP_OPEN2
		GOSUB GetTrans

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
				"Invoice Amount   Check Amount  " + &
				"Batch Number"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "013,020,036,049,064,079"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = AP_OPEN::VENNUM + " " + &
				AP_OPEN::TRANKEY + " " + &
				AP_OPEN::INVNUM + "  " + &
				PRNT_DATE(AP_OPEN::INVDAT, 8%) + "  " + &
				FORMAT$(AP_OPEN::INVAMT, "###,###,###.##") + &
				" " + FORMAT$(AP_OPEN::CKAMT, &
					"###,###,###.##") + "  " + &
				AP_OPEN::BATCH

		END SELECT
	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #AP_OPEN.CH%, KEY #0% &
				GE AP_OPEN::VENNUM + AP_OPEN::TRANKEY, &
				REGARDLESS
		CASE 1%
			FIND #AP_OPEN.CH%, KEY #1% &
				GE AP_OPEN::VENNUM + AP_OPEN::INVNUM, &
				REGARDLESS
		CASE 2%
			FIND #AP_OPEN.CH%, KEY #2% &
				GE AP_OPEN::BATCH + "", &
				REGARDLESS
		END SELECT

	END SELECT

	%PAGE

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
	GET #AP_CONTROL.CH%, RECORD 1%

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

	AP_OPEN::TRANKEY = AP_CONTROL::LAST_TRANKEY

	UPDATE #AP_CONTROL.CH%

	RETURN

	%PAGE

29000	!
	! Trap Errors
	!
	RESUME ExitFunction

32767	END FUNCTION
	!+-+-+
	!++
	! Abstract:DIST
	!	^*Distribution Maintenance\*
	!	.p
	!	The ^*Distribution Maintenance\* option
	!	edits the distribution of the current transaction.
	!
	! Index:
	!	.x Distribution>Open
	!
	!--
