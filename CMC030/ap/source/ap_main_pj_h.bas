1	%TITLE "Accounts Payable Purchase Journal Maintenance"
	%SBTTL "AP_MAIN_PJ_H"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AP_MAIN_PJ_H(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	The ^*Accounts Payable Purchases Journal Maintenance\* option provides access
	!	for entry of invoices, vender charges yet to be paid, and other related
	!	charges for which hand checks have already been written.
	!
	! Index:
	!	.x Purchase Journal Maintenance
	!	.x Maintenance>Purchase Journal
	!
	! Option:
	!
	!	AP_MAIN_PJ_H$LINE_ITEMS
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_MAIN_PJ_H/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AP_MAIN_PJ_H
	!	$ DELETE AP_MAIN_PJ_H.OBJ;*
	!
	! Author:
	!
	!	08/06/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	05/17/88 - Lance Williams
	!		Modified the header.
	!
	!	12/05/88 - Kevin Handy
	!		Modified to disable checking cash account number
	!		when check number is blank.
	!
	!	12/05/88 - Kevin Handy
	!		Modified to display the description of the AP
	!		account and the CASH account.
	!
	!	12/19/88 - Kevin Handy
	!		Modified to set due date and discount date back
	!		if the date does not really exist that month
	!		(eg. 11/31/88).
	!
	!	03/21/91 - Kevin Handy
	!		Modified so that VAL%() at 27000 would not crash
	!		program if it was supplied with letters instead
	!		of numbers.
	!
	!	08/01/91 - Frank F. Starman
	!		Set correctly due date, based on info from the vendor
	!		master file. Change test for valid due date. It didn't
	!		work for february.
	!
	!	09/30/91 - Kevin Handy
	!		Fixed key definitions so that it will ask for
	!		transatcion number instead of customer number.
	!
	!	10/24/91 - Kevin Handy
	!		Fixed defaulting of due date so that it would leave
	!		it alone if they do a hard or soft default on it.
	!
	!	02/12/93 - Dan Perkins
	!		Changed "V0" to "VX" on chard of accounts to be
	!		able to list accounts starting at a particular
	!		account.
	!
	!	03/11/93 - Kevin Handy
	!		Modified so that the F17 key would work on the
	!		vendor number field when the field was blank.
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/14/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		change RIGHT(NUM1$(...)) to FORMAT$().
	!
	!	04/23/96 - Kevin Handy
	!		Reformat source code.
	!
	!	04/23/96 - Kevin Handy
	!		Modifications to loose infinite loop in change.
	!		Return 255 in SCOPE_EXIT when field is not changable.
	!
	!	08/13/96 - Kevin Handy
	!		Remove extra '&' before a 'then'.
	!
	!	05/12/97 - Kevin Handy
	!		Reformat source code.
	!		Use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/09/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	05/20/99 - Kevin Handy
	!		Rewrote FNVAL to use WHEN ERROR IN so it won't
	!		disturb other error trapping.
	!
	!	06/08/99 - Kevin Handy
	!		Lose HelpError (Dead Code)
	!
	!	08/03/99 - Kevin Handy
	!		Added ability to load in PO lines automatically
	!		(Po_line option)
	!
	!	08/24/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/29/2000 - Kevin Handy
	!		Use A"x"B
	!
	!	02/19/2002 - Kevin Handy
	!		Create form routines
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Includes
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PO_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	!
	! CDD and Maps
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AP.OPEN]AP_PJH.HB"
	MAP (AP_PJH)		AP_PJH_CDD		AP_PJH
	MAP (AP_PJH_OLD)	AP_PJH_CDD		AP_PJH_OLD, AP_PJH2

	%INCLUDE "SOURCE:[AP.OPEN]AP_PJL.HB"
	MAP (AP_PJL)		AP_PJL_CDD		AP_PJL

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.HB"
	MAP (AP_CONTROL)	AP_CONTROL_CDD		AP_CONTROL

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR)		AP_VENDOR_CDD		AP_VENDOR

	%INCLUDE "SOURCE:[AP.OPEN]AP_1099_TABLE.HB"
	MAP (AP_1099_TABLE)	AP_1099_TABLE_CDD	AP_1099_TABLE

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP (AP_OPEN)		AP_OPEN_CDD		AP_OPEN

	%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.HB"
	MAP (AP_CLOSE)		AP_CLOSE_CDD		AP_CLOSE

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	MAP (PO_REG_LINE)	PO_REG_LINE_CDD		PO_REG_LINE
	DECLARE			PO_REG_LINE_CDD		PO_REG_LINE_READ

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.HB"
	DECLARE			PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE_READ

	!
	! Commons
	!
	COM (CH_AP_PJH) &
		AP_PJH.CH%, &
		AP_PJH.READONLY%, &
		BATCH_NO$ = 2%

	COM (CH_AP_CONTROL) &
		AP_CONTROL.CH%

	COM (CH_AP_VENDOR) &
		AP_VENDOR.CH%

	COM (CH_AP_OPEN) &
		AP_OPEN.CH%

	COM (CH_AP_CLOSE) &
		AP_CLOSE.CH%

	COM (CH_AP_PJL) &
		AP_PJL.CH%

	COM (READ_PO_REG_LINE) &
		PO_REG_LINE.CH%, &
		PO_REG_SUB_LINE.CH%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION MAIN_JOURNAL
	EXTERNAL LONG	FUNCTION PO_READ_REG_LINE
	EXTERNAL LONG   FUNCTION AP_OUTP_PJ

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
		SMG_WINDOW::DESCR = "Purchase Journal Maintenance"
		SMG_WINDOW::NHELP = "AP_MAIN_PJ_H"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 15%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Trans-number"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = -1%

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Declare channels
		!
		IF AP_PJH.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AP_PJH.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_PJH.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			AP_MAIN_PJ_H = ERR
			CONTINUE 770
		END WHEN

		AP_PJH.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_PJH.OPN"
		USE
			AP_MAIN_PJ_H = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		AP_PJH.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(AP_PJH.CH%)

		GOTO ExitProgram

790		SMG_WINDOW::CHAN  = AP_PJH.CH%
		WHEN ERROR IN
			RESET #AP_PJH.CH%
			GET #AP_PJH.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN
	%PAGE

	!
	! Modify the menu
	!
	CASE OPT_OPTLIST
		MVALUE = MVALUE + " Line_items Po_load invforM invform_direcT"

	!
	! Optional menu items
	!
5000	CASE OPT_MOREMENU
		SELECT SCOPE::PRG_ITEM

		!
		! Line option
		!
		CASE "Line_items"
	!++
	! Abstract:LINE_ITEMS
	!	^*Lines Items\*
	!	.p
	!	The ^*Line Items\* option in the COMMAND menu allows for entry and maintenance
	!	of the records by line.
	!
	! Index:
	!	.x Line Items
	!
	!--

			!
			! Make sure there is a header
			!
			T_SE% = SCOPE::SCOPE_EXIT
			ST% = MAIN_WINDOW(AP_MAIN_VENDOR.ID, &
				"Q0" + AP_PJH::VENNUM)
			SCOPE::SCOPE_EXIT = T_SE%

			AP_MAIN_PJ_H = MAIN_JOURNAL(AP_MAIN_PJ_L.ID, "")

		!
		! Line option
		!
		CASE "Po_load"
	!++
	! Abstract:PO_LOAD
	!	^*Po_load\*
	!	.lm +5
	!	.b
	!	Used to load in the remaining quantities from a Purchase Order into the
	!	line items of the current invoice.
	!	.lm -5
	!
	! Index:
	!
	!--
			GOSUB Po_load

		CASE "invforM"
	!++
	! Abstract:INVFORM
	!--
			V% = AP_OUTP_PJ(AP_PJH::TRANKEY, BATCH_NO$, 0%)

		CASE "invform_direcT"
	!++
	! Abstract:INVFORM_DIRECT
	!--
			V% = AP_OUTP_PJ(AP_PJH::TRANKEY, BATCH_NO$, 1%)

		END SELECT

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

		DATA	02, 03, "Trans #", &
			03, 03, "Trans Date", &
			05, 03, "(01) Vendor #", &
			11, 03, "(02) Invoice #", &
			12, 03, "(03) Inv Date", &
			13, 03, "(04) Inv Amt", &
			14, 03, "(05) Desc", &
			16, 03, "(06) A/P Acct", &
			02, 44, "(07) Due Date", &
			03, 44, "(08) Disc Date", &
			04, 44, "(09) Disc Amt", &
			06, 44, "(10) 1099 Code", &
			07, 44, "(11) 1099 Amt", &
			09, 44, "(12) Check   #", &
			10, 44, "(13) Check Date", &
			11, 44, "(14) Check Amt", &
			13, 44, "(15) Cash Acct", &
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

		TFLAG% = MFLAG
		TVALUE$ = MVALUE

		SELECT MLOOP

		CASE 10% TO 11%
			IF AP_VENDOR::FLG1099 <> "Y"
			THEN
				AP_PJH::CODE_1099 = ""
				AP_PJH::AMT_1099 = 0.0
				MFLAG = MFLAG OR 33%
				MFLAG = MFLAG AND NOT 64%
				MVALUE = ""
				SCOPE::SCOPE_EXIT = 255%
			END IF

		CASE 13% TO 15%
			IF AP_PJH::CKNUM = ""
			THEN
				AP_PJH::CKDAT = SPACE$(8%)
				AP_PJH::CKAMT = 0.0
				AP_PJH::CASH_ACCT = ""
				MFLAG = MFLAG OR 33%
				MFLAG = MFLAG AND NOT 64%
				MVALUE = ""
				SCOPE::SCOPE_EXIT = 255%
			ELSE
				IF EDIT$(TEMP$, -1%) = "ADD"
				THEN
					AP_PJH::CASH_ACCT = AP_CONTROL::CASH_ACCT
					AP_PJH::CKDAT = AP_PJH::INVDAT
				END IF
			END IF
		END SELECT

		TEMP.SCOPE% = SCOPE::SCOPE_EXIT

 ELoop:		SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE -1%
			AP_PJH::TRANKEY = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;14", TEMP$, AP_PJH::TRANKEY, MFLAG, &
				"'E", MVALUE)

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Vendor _#\*
	!	.P
	!	The ^*Vendor _#\* field contains the reference number or key used to identify
	!	a particular vendor. When a valid ^*Vendor _#\* is entered, the
	!	name and address of that vendor will be displayed.
	!	.P
	!	If the ^*Vendor _#\* entered does not exist in the Vendor Master
	!	file, the message, ^*"Undefined Vendor number - ADD to VENDOR file
	!	(Y/N) ? <Yes/No>: No"\*, will appear at the bottom of the screen. The
	!	default response is ^*No\*. Pressing ^*<Ent>\* or ^*<Ret>\* will cause
	!	the system to permit a re-try to enter the correct vendor number. A
	!	^*Yes\* response to the message will cause the system to access the
	!	Vendor Master File where a record can be added for the new vendor.
	!	After completing the Add function for the new vendor, press the
	!	^*<Exit>\* key to return to the purchases Journal routine.
	!	.P
	!	Function keys which can be used are:
	!	.LIST
	!	.LIST ELEMENT
	!	^*<List Choices>\* will list the vendor master file and allow
	!	the user to select a vendor.
	!	.LIST ELEMENT
	!	^*<F17>\* will allow editing of the vendor whose number
	!	is displayed on the screen.
	!	.END LIST
	!
	! Index:
	!	.x Vendor Number
	!
	!--
			AP_PJH::VENNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"5;18", TEMP$, AP_PJH::VENNUM, MFLAG, "'E", &
				MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				AP_PJH::VENNUM = AP_VENDOR::VENNUM &
					IF MAIN_WINDOW(AP_MAIN_VENDOR.ID, &
					"VX  ") = 1%
				GOTO Eloop
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				ST% = MAIN_WINDOW(AP_MAIN_VENDOR.ID, &
					"M0" + AP_PJH::VENNUM)
				GOTO Eloop
			END IF

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Invoice _#\*
	!	.P
	!	A vendor's invoice number must be entered in the ^*Invoice _#\*
	!	field. A separate record should be added for each invoice received
	!	from a vendor. If a vendor does not furnish invoices, a user defined
	!	reference may be entered, for example, MAYSTMT,
	!	STMT09/87 or SC-Sep.
	!	.P
	!	This field will contain up to fifteen (15) alphanumeric
	!	characters.
	!
	! Index:
	!	.x Invoice Number>Purchases Journal Maintenance
	!	.x Purchases Journal Maintenance>Invoice Number
	!
	!--
			AP_PJH::INVNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"11;25", TEMP$, AP_PJH::INVNUM, MFLAG, "'E", &
				MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^* (03) Invoice Date\*
	!	.P
	!	The ^*Invoice Date\* field contains the date which appears
	!	on the vendor's invoice in ^*MMDDYYYY\* format.
	!
	! Index:
	!	.x Invoice Date>Purchases Journal Maintenance
	!
	!--
			AP_PJH::INVDAT = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"12;30", TEMP$, AP_PJH::INVDAT, MFLAG, "'E", &
				MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Invoice Amount\*
	!	.P
	!	The ^*Invoice Amount\* field enters the total
	!	gross dollar amount shown on a vendor's invoice.
	!
	! Index:
	!	.x Invoice Amount>Purchases Journal Maintenance
	!
	!--
			AP_PJH::INVAMT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"13;28", TEMP$, AP_PJH::INVAMT, MFLAG, &
				"#,###,###.##", MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field enters a brief description
	!	of the goods, services furnished, or other meaningful information.
	!	.B
	!	This field may be bypassed.
	!	.b
	!	There are twenty (20) alphanumeric spaces available.
	!	.lm -5
	!
	! Index:
	!	.x Description>Purchases Journal Maintenance
	!	.x Purchases Journal Maintenance>Description
	!
	!--
			AP_PJH::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"14;20", TEMP$, AP_PJH::DESCR, MFLAG, "'E", &
				MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Accounts Payable Account\*
	!	.P
	!	The ^*Accounts Payable Account\* field must contain a General
	!	Ledger Chart of Accounts liability account which is to be credited
	!	when a vendor charge is recognized.
	!	.P
	!	The contents in this field will automatically default to the
	!	designated account number stored in the Account Payable Control
	!	record, but that account number may be overridden by entering
	!	a different account number.
	!
	! Index:
	!	.x Purchases Journal>Accounts Payable Account
	!
	!--
			AP_PJH::AP_ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"16;22", TEMP$, AP_PJH::AP_ACCT, MFLAG, &
				"'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				AP_PJH::AP_ACCT = GL_CHART::ACCT &
					IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
					"VX ") = 1%
				GOTO ELoop
			END IF

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(07) Due Date\*
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
	!	.x Due Date>Purchase Journal Maintenance
	!
	!--
			AP_PJH::DUEDAT = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"02;67", TEMP$, AP_PJH::DUEDAT, MFLAG, "'E", &
				MVALUE)

		CASE 8%
	!++
	! Abstract:FLD008
	!	^*(08) Discount Date\*
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
	!	.x Discount Date
	!
	!--
			AP_PJH::DISCDAT = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"03;67", TEMP$, AP_PJH::DISCDAT, MFLAG, "'E", &
				MVALUE)

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*(09) Discount Amount\*
	!	.P
	!	The discount percentage, entered in the Vendor Master File
	!	for the subject vendor, will cause the system to calculate the
	!	discount amount and automatically enter that amount in this field.
	!	If the discount percentage in the Vendor Master File is zero, the
	!	calculated result will be zero. The automatically entered amount
	!	may be overridden by typing a different amount and pressing ^*<Ent>\*.
	!
	! Index:
	!	.x Discount Amount
	!
	!--
			AP_PJH::DISCAMT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"04;65", TEMP$, AP_PJH::DISCAMT, MFLAG, &
				"#,###,###.##", MVALUE)

		CASE 10%
	!++
	! Abstract:FLD010
	!	^*(10) 1099 Code\*
	!	.P
	!	If the 1099 flag in the Accounts Payable Master File for the
	!	subject vendor is set to "N", the system will automatically bypass
	!	this field.  If the flag is set to "Y", the cursor will stop at the
	!	^*1099 Code\* field, requiring that a code be entered.
	!	.P
	!	The code entered should be equal to a 1099 code that has
	!	already been added to the 1099 Table.  If an invalid code is entered,
	!	the system will prompt the user to re-enter a valid code or accept
	!	the invalid code, which must subsequently be validated by adding that
	!	code in the 1099 Table.
	!
	! Index:
	!	.x 1099 Code
	!	.x Purchases Journal>1099 Code
	!
	!--
			AP_PJH::CODE_1099 = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"06;75", TEMP$, AP_PJH::CODE_1099, MFLAG, &
				"'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				AP_PJH::CODE_1099 = AP_1099_TABLE::CODE &
					IF MAIN_WINDOW(AP_MAIN_1099_TABLE.ID, &
					"V0 ") = 1%
				GOTO Eloop
			END IF

			SCOPE::SCOPE_EXIT = TEMP.SCOPE% IF (MFLAG AND 1%)

		CASE 11%
	!++
	! Abstract:FLD011
	!	^*(11) 1099 Amount\*
	!	.P
	!	The dollar amount of a vendor's charge which is subject to
	!	being reported on a Form 1099 is to be entered in the ^*1099 Amount\*
	!	field.
	!
	! Index:
	!	.x 1099 Amount
	!	.x Purchases Journal>1099 Amount
	!
	!--
			AP_PJH::AMT_1099 = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"07;65", TEMP$, AP_PJH::AMT_1099, MFLAG, &
				"#,###,###.##", MVALUE)
			SCOPE::SCOPE_EXIT = TEMP.SCOPE% IF (MFLAG AND 1%)

		CASE 12%
	!++
	! Abstract:FLD012
	!	^*(12) Check _#\*
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
	!	.x Purchases Journal>Check Number
	!
	!--
			AP_PJH::CKNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"09;71", TEMP$, AP_PJH::CKNUM, MFLAG, "'E", &
				MVALUE)

		CASE 13%
	!++
	! Abstract:FLD013
	!	^*(13) Check Date\*
	!	.P
	!	If a ^*check number\* is entered in field (12) in a Purchases
	!	Journal record, the cursor will stop at the ^*Check Date\* field
	!	in order for the appropriate check date to be entered in MMDDYYYY
	!	format.
	!	.P
	!	If the Check _# field is blank the system will automatically
	!	skip the ^*Check Date\* field.
	!
	! Index:
	!	.x Check Date
	!
	!--
			AP_PJH::CKDAT = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;67", TEMP$, AP_PJH::CKDAT, MFLAG, "'E", &
				MVALUE)
			SCOPE::SCOPE_EXIT = TEMP.SCOPE% IF (MFLAG AND 1%)

		CASE 14%
	!++
	! Abstract:FLD014
	!	^*(14) Check Amount\*
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
	!	.x Check Amount
	!	.x Purchases Journal>Check Amount
	!
	!--
			AP_PJH::CKAMT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"11;65", TEMP$, AP_PJH::CKAMT, MFLAG, &
				"#,###,###.##", MVALUE)
			SCOPE::SCOPE_EXIT = TEMP.SCOPE% IF (MFLAG AND 1%)

		CASE 15%
	!++
	! Abstract:FLD015
	!	^*(15) Cash Account\*
	!	.P
	!	The ^*Cash Account\* field must contain the General Ledger Chart
	!	of Accounts cash account number which represents the bank from which
	!	funds are drawn to pay the accounts payable.
	!	.P
	!	This field will automatically default to the
	!	designated account number stored in the Accounts Payable Control
	!	record.  This particular account number may be overridden by entering
	!	a different account number.
	!	.P
	!	If no check number is entered in field (12), the ^*Cash Account\*
	!	field will automatically be bypassed.
	!
	! Index:
	!	.x Purchases Journal>Cash Account
	!
	!--
			AP_PJH::CASH_ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"13;59", TEMP$, AP_PJH::CASH_ACCT, MFLAG, &
				"'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				AP_PJH::CASH_ACCT = GL_CHART::ACCT &
					IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
					"VX ") = 1%
				GOTO ELoop
			END IF
			SCOPE::SCOPE_EXIT = TEMP.SCOPE% IF (MFLAG AND 1%)

		END SELECT

		MFLAG = TFLAG%
		MVALUE = TVALUE$
		SCOPE::PRG_ITEM= TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		AP_MAIN_PJ_H = 0%

		SELECT MLOOP

		CASE 1%
			AP_MAIN_PJ_H, ST% = FUNC_TESTENTRY(SMG_WINDOW, &
				AP_PJH::VENNUM, AP_VENDOR::VENNAM, &
				"AP", MLOOP, "VEND", &
				"Vendor number", AP_MAIN_VENDOR.ID)

			IF ST% <> 0%
			THEN
				AP_VENDOR::ADD1 = &
					STRING$(LEN(AP_VENDOR::ADD1), A"?"B)

				AP_VENDOR::ADD2 = &
					STRING$(LEN(AP_VENDOR::ADD2), A"?"B)

				AP_VENDOR::CITY = &
					STRING$(LEN(AP_VENDOR::CITY), A"?"B)

				AP_VENDOR::STATE = &
					STRING$(LEN(AP_VENDOR::STATE), A"?"B)

				AP_VENDOR::ZIP = &
					STRING$(LEN(AP_VENDOR::ZIP), A"?"B)

				AP_VENDOR::COUNTRY = &
					STRING$(LEN(AP_VENDOR::COUNTRY), A"?"B)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AP_VENDOR::VENNAM, 6%, 3%,, SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AP_VENDOR::ADD1, 7%, 3%,, SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AP_VENDOR::ADD2, 8%, 3%,, SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS( &
				SMG_WINDOW::WNUMBER, &
				LEFT(TRM$(AP_VENDOR::CITY) + "  " + &
				AP_VENDOR::STATE + "  " + &
				AP_VENDOR::ZIP + "  " + &
				AP_VENDOR::COUNTRY + SPACE$(40%), 40%), &
				9%, 3%,, SMG$M_BOLD)

		CASE 2%		! Duplicate Invoice Number
			SCOPE::SCOPE_EXIT = T_SE%

			DUP_INV% = 0%
			IF AP_OPEN.CH% > 0%
			THEN
				GOSUB DupOpenFile
			END IF

			IF AP_CLOSE.CH% > 0% AND DUP_INV% = 0%
			THEN
				GOSUB DupCloseFile
			END IF

 DupInv:		IF DUP_INV%
			THEN
				INP$ = ENTR_3YESNO(SCOPE, &
					SMG_WINDOW::WNUMBER, "", &
					"Duplicate invoice number - " + &
					"ADD to PJ file (Y/N) ?", &
					"N", 4%, "!", "")

				SELECT SCOPE::SCOPE_EXIT
				!
				! Exit keys
				!
				CASE 3%, SMG$K_TRM_F8, SMG$K_TRM_F10, &
					SMG$K_TRM_CTRLZ

					CALL ENTR_3MESSAGE(SCOPE, &
						"Duplicate invoice Number", 1%)

				!
				! Good keys
				!
				CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

				!
				! Bad Keys
				!
				CASE ELSE
					CALL ENTR_3BADKEY(SCOPE, &
						SCOPE::SCOPE_EXIT)
					GOTO DupInv

				END SELECT

				IF INP$ <> "Y"
				THEN
					AP_MAIN_PJ_H = 1%
				END IF

			END IF

			SCOPE::SCOPE_EXIT = T_SE%

		CASE 3%	! Invoice Date

			T_SE% = SCOPE::SCOPE_EXIT
			GOSUB FieldCheck
			SCOPE::SCOPE_EXIT = T_SE%

		CASE 4%	! Invoice Amount

			T_SE% = SCOPE::SCOPE_EXIT

			ST% = MAIN_WINDOW(AP_MAIN_VENDOR.ID, &
				"Q0" + AP_PJH::VENNUM)

			IF ST% = 1%
			THEN
				AP_PJH::DISCAMT = FUNC_ROUND(AP_PJH::INVAMT * &
					(AP_VENDOR::DISCPER / 100.), 2%) &
					IF AP_VENDOR::DISCPER > 0%

				AP_PJH::DISCAMT = ENTR_3NUMBER(SCOPE, &
					SMG_WINDOW::WNUMBER, &
					"04;65", TEMP$, AP_PJH::DISCAMT, 1%, &
					"#,###,###.##", MVALUE)
			END IF

			SCOPE::SCOPE_EXIT = T_SE%

		CASE 6%

			T_SE% = SCOPE::SCOPE_EXIT

			AP_MAIN_PJ_H = 0%

			IF AP_PJH::AP_ACCT = ""
			THEN
				AP_MAIN_PJ_H = 1%
				EXIT FUNCTION
			END IF

			!
			! Is the input defined?
			!
			IF INSTR(1%, AP_PJH::AP_ACCT, "?") = 0%
			THEN
				AP_MAIN_PJ_H = FUNC_TESTENTRY(SMG_WINDOW, &
					AP_PJH::AP_ACCT, GL_CHART::DESCR, &
					"GL", MLOOP, "ACCT", &
					"Account number", GL_MAIN_CHART.ID)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					LEFT(GL_CHART::DESCR, 20%), 17%, 22%,, SMG$M_BOLD)
			ELSE
				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					SPACE$(20%), 17%, 22%,, SMG$M_BOLD)
			END IF

			SCOPE::SCOPE_EXIT = T_SE%

		CASE 10%
			!
			! Is the input defined?
			!
			IF AP_VENDOR::FLG1099 = "Y"
			THEN
				T_SE% = SCOPE::SCOPE_EXIT

				AP_MAIN_PJ_H = FUNC_TESTENTRY(SMG_WINDOW, &
					AP_PJH::CODE_1099, GL_CHART::DESCR, &
					"AP", MLOOP, "1099CD", &
					"1099 code", AP_MAIN_1099_TABLE.ID)

				SCOPE::SCOPE_EXIT = T_SE%

			END IF

		CASE 15%
			T_SE% = SCOPE::SCOPE_EXIT

			AP_MAIN_PJ_H = 0%

			IF (AP_PJH::CASH_ACCT = "")
			THEN
				IF (AP_PJH::CKNUM = "")
				THEN
					EXIT FUNCTION
				ELSE
					AP_MAIN_PJ_H = 1%
					EXIT FUNCTION
				END IF
			END IF

			!
			! Is the input defined?
			!
			IF INSTR(1%, AP_PJH::AP_ACCT, "?") = 0%
			THEN
				AR_MAIN_PJ_H = FUNC_TESTENTRY(SMG_WINDOW, &
					AP_PJH::CASH_ACCT, GL_CHART::DESCR, &
					"GL", MLOOP, "ACCT", &
					"Account number", GL_MAIN_CHART.ID)

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					LEFT(GL_CHART::DESCR, 20%), 14%, 59%,, SMG$M_BOLD)
			ELSE
				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					SPACE$(20%), 14%, 59%,, SMG$M_BOLD)
			END IF

			SCOPE::SCOPE_EXIT = T_SE%

		END SELECT

	!
	! Display additional information
	!
	CASE OPT_DISPLAY
		AP_PJH::TRANKEY = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
			"2;14", TEMP$, AP_PJH::TRANKEY, 1%, "'E", "")

		AP_PJH::TRANKEY_DATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
			"3;14", TEMP$, AP_PJH::TRANKEY_DATE, 1%, "'E", "")

		ST% = MAIN_WINDOW(AP_MAIN_VENDOR.ID, "Q0" + AP_PJH::VENNUM)

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			IF ST% <> 1%
			THEN
				AP_VENDOR::VENNAM = STRING$(40%, A"?"B)
				AP_VENDOR::ADD1 = STRING$(25%, A"?"B)
				AP_VENDOR::ADD2 = STRING$(21%, A"?"B)
				AP_VENDOR::CITY = STRING$(15%, A"?"B)
				AP_VENDOR::STATE = STRING$(2%, A"?"B)
				AP_VENDOR::ZIP = STRING$(10%, A"?"B)
				AP_VENDOR::COUNTRY = STRING$(8%, A"?"B)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AP_VENDOR::VENNAM, 6%, 3%,, SMG$M_BOLD)
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AP_VENDOR::ADD1, 7%, 3%,, SMG$M_BOLD)
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AP_VENDOR::ADD2, 8%, 3%,, SMG$M_BOLD)
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(TRM$(AP_VENDOR::CITY) + "  " + &
				AP_VENDOR::STATE + "  " + &
				AP_VENDOR::ZIP + "  " + &
				AP_VENDOR::COUNTRY + SPACE$(40%), 40%), &
				9%, 3%,, SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(7%) AND 2%) = 0%
		THEN
			IF INSTR(1%, AP_PJH::AP_ACCT, "?") = 0%
			THEN
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "Q0" + AP_PJH::AP_ACCT) <> 1%
				THEN
					GL_CHART::DESCR = "????????????????????"
				END IF

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					LEFT(GL_CHART::DESCR, 20%), 17%, 22%,, SMG$M_BOLD)
			ELSE
				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					SPACE$(20%), 17%, 22%,, SMG$M_BOLD)
			END IF
		END IF

		IF (SMG_WINDOW::HFLAG(15%) AND 2%) = 0%
		THEN
			IF (INSTR(1%, AP_PJH::CASH_ACCT, "?") = 0%) AND &
				(AP_PJH::CKNUM <> "      ")
			THEN
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
					"Q0" + AP_PJH::CASH_ACCT) <> 1%
				THEN
					GL_CHART::DESCR = "????????????????????"
				END IF

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					LEFT(GL_CHART::DESCR, 20%), 14%, 59%,, SMG$M_BOLD)
			ELSE
				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					SPACE$(20%), 14%, 59%,, SMG$M_BOLD)
			END IF
		END IF

	!
	! Set AP_PJH_OLD value
	!
20500	CASE OPT_SETOLD
		AP_PJH_OLD = AP_PJH

	!
	! Restore AP_PJH_OLD value
	!
	CASE OPT_RESETOLD
		AP_PJH = AP_PJH_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		AP_PJH2 = AP_PJH

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		AP_PJH2::VENNUM = AP_PJH::VENNUM
		AP_PJH2::INVDAT = AP_PJH::INVDAT
		AP_PJH = AP_PJH2

		GOSUB GetTrans

		AP_PJH::TRANKEY_DATE = DATE_TODAY

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Trans    Trans Date   Vendor #     " + &
				"Invoice Amount"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "010,023,036"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = AP_PJH::TRANKEY + "   " + &
				PRNT_DATE(AP_PJH::TRANKEY_DATE, 8%) + "   " + &
				AP_PJH::VENNUM + "     " + &
				FORMAT$(AP_PJH::INVAMT, "#,###,###.##")

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		FIND #AP_PJH.CH%, KEY #0% GE AP_PJH::TRANKEY + "", REGARDLESS

	!
	! Handle finishing various options specially
	!
	CASE OPT_AFTEROPT

		SELECT SCOPE::PRG_ITEM

		!
		! Add records
		!
		CASE "Add"
			!
			! Add line items also
			!

			AP_MAIN_PJ_H = MAIN_JOURNAL(AP_MAIN_PJ_L.ID, "A")

		!
		! Change records
		!
		CASE "Change"
			!
			! Change line items to match new header
			! if the key was changed.
			!
			! The original record must be the one in the
			! MAP for this to be able to work.  The new
			! key is passed through the QUERY$ variable.
			!
			IF AP_PJH_OLD::TRANKEY <> AP_PJH::TRANKEY
			THEN
				TEMP$ = AP_PJH::TRANKEY + ""
				AP_PJH = AP_PJH_OLD
				AP_MAIN_PJ_H = MAIN_JOURNAL(AP_MAIN_PJ_L.ID, &
					"C" + TEMP$)
			END IF

		!
		! Erase record
		!
		CASE "Erase"
			!
			! Erase any line items under the header
			!
			AP_MAIN_PJ_H = MAIN_JOURNAL(AP_MAIN_PJ_L.ID, "E")

		END SELECT

	END SELECT

 ExitProgram:
	EXIT FUNCTION

 FieldCheck:
27000	!*******************************************************************
	! This subroutine will check the DUE DATE, DISC DATE
	! against the coresponding Vendor file dates and the INVOICE DATE.
	!*******************************************************************
	SELECT MLOOP
	CASE 1%, 3%

		RETURN &
			IF AP_PJH::VENNUM = "" OR AP_PJH::INVDAT = ""

		IF AP_PJH::DUEDAT = ""
		THEN
			AP_PJH::DUEDAT = AP_PJH::INVDAT

			IF MAIN_WINDOW(AP_MAIN_VENDOR.ID, &
				"Q0" + AP_PJH::VENNUM) = 1%
			THEN

				IF AP_VENDOR::DUEDAYS <> 0%
				THEN
					!
					! Paid so many days after the invoice date.
					!
					AP_PJH::DUEDAT = DATE_INVDCODE(DATE_DAYCODE( &
						AP_PJH::INVDAT) + &
						AP_VENDOR::DUEDAYS)

				ELSE
					GOTO CheckDate &
						IF VAL%(AP_VENDOR::DUEDATE) = 0%
					!
					! Paid on a specific day of the month
					!
					NEW_MON% = &
						VAL%(MID$(AP_PJH::INVDAT, 5%, 2%)) + 1%
					NEW_YEAR% = VAL%(MID$(AP_PJH::INVDAT, 1%, 4%))
					IF NEW_MON% > 12%
					THEN
						NEW_MON% = 1%
						NEW_YEAR% = NEW_YEAR% + 1%
					END IF

					AP_PJH::DUEDAT = &
						FORMAT$(NEW_YEAR%, "<0>###") + &
						FORMAT$(NEW_MON%, "<0>#") + &
						FORMAT$(VAL%(AP_VENDOR::DUEDATE), "<0>#")

				END IF
			END IF

 CheckDate:
			!
			! Make sure the date exists this month
			!
			IF (AP_PJH::DUEDAT <> DATE_INVDCODE(DATE_DAYCODE( &
				AP_PJH::DUEDAT)))
			THEN
				AP_PJH::DUEDAT = LEFT(AP_PJH::DUEDAT, 6%) + &
					FORMAT$(VAL%(MID(AP_PJH::DUEDAT, 7%, 2%)) - 1%, "<0>#")
				GOTO CheckDate
			END IF

			!
			! Print the due date
			!
			AP_PJH::DUEDAT = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"02;67", TEMP$, AP_PJH::DUEDAT, 1%, "'E", &
				MVALUE)

			IF AP_VENDOR::DISDATE <> ""
			THEN
				!
				! Discount date on specific day of the month
				!
				V_DAY% = FNVAL%(AP_VENDOR::DISDATE)
				NEW_DAY%, I_DAY% = FNVAL%( &
					MID$(AP_PJH::INVDAT, 7%, 2%))
				NEW_MON% = FNVAL%( &
					MID$(AP_PJH::INVDAT, 5%, 2%))
				NEW_YEAR$ = LEFT(AP_PJH::INVDAT, 4%)

				SELECT I_DAY%
				CASE < V_DAY%
					NEW_DAY% = V_DAY%
				CASE ELSE
					NEW_MON% = NEW_MON% + 1%
					IF NEW_MON% > 12%
					THEN
						NEW_MON% = 1%
						NEW_YEAR$ = FORMAT$( &
							VAL%(NEW_YEAR$) + 1%, &
							"<0>###")
					END IF
					NEW_DAY% = V_DAY%
				END SELECT

				AP_PJH::DISCDAT = NEW_YEAR$ + &
					FORMAT$(NEW_MON%, "<0>#") + &
					FORMAT$(NEW_DAY%, "<0>#")
				!
				! Make sure the date exists this month
				!
				IF (AP_PJH::DISCDAT <> DATE_INVDCODE(DATE_DAYCODE( &
					AP_PJH::DISCDAT)))
				THEN
					AP_PJH::DISCDAT = DATE_INVDCODE(DATE_DAYCODE( &
						AP_PJH::DISCDAT) - 1%)
				END IF

			ELSE
				!
				! Discount date so many days after invoice date
				!
				IF AP_VENDOR::DISDAYS > 0%
				THEN
					AP_PJH::DISCDAT = DATE_INVDCODE(DATE_DAYCODE( &
						AP_PJH::INVDAT) + &
						AP_VENDOR::DISDAYS)
				ELSE
					AP_PJH::DISCDAT = AP_PJH::INVDAT
				END IF
			END IF

			!
			! Print discount date
			!
			AP_PJH::DISCDAT = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"03;67", TEMP$, AP_PJH::DISCDAT, 1%, "'E", &
				MVALUE)

		END IF

	END SELECT

	RETURN

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
		!
		! Invoice Record not found in ap open file
		!
		CONTINUE ExitProgram
	END WHEN

	AP_PJH::AP_ACCT = AP_CONTROL::AP_ACCT

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

	AP_PJH::TRANKEY = AP_CONTROL::LAST_TRANKEY

	UPDATE #AP_CONTROL.CH%

	RETURN

	%PAGE

 DupOpenFile:
28100	!**********************************************************************
	! Search for duplicate invoice in AP Open file
	!**********************************************************************
	WHEN ERROR IN
		FIND #AP_OPEN.CH%, &
			KEY #1% EQ AP_PJH::VENNUM + AP_PJH::INVNUM, &
			REGARDLESS
	USE
		CONTINUE 28190 IF ERR = 155% OR ERR = 9%
		EXIT HANDLER
	END WHEN

	DUP_INV% = -1%

28190	RETURN

 DupCloseFile:
28200	!**********************************************************************
	! Search for duplicate invoice in AP Close file
	!**********************************************************************
	WHEN ERROR IN
		FIND #AP_CLOSE.CH%, &
			KEY #1% EQ AP_PJH::VENNUM + AP_PJH::INVNUM, &
			REGARDLESS
	USE
		CONTINUE 28290 IF ERR = 155% OR ERR = 9%
		EXIT HANDLER
	END WHEN

	DUP_INV% = -1%

28290	RETURN

28300	!*******************************************************************
	! Load in all remaining lines from a specific PO into the APJ
	! line item file
	!*******************************************************************
 Po_load:

	!
	! Lets get a PO number to load in
	!
	PO_NUMBER$ = SPACE$(LEN(PO_REG_LINE::PO))
	LINE_COUNT% = 0%

	IF AP_PJL.CH% <= 0%
	THEN
		!
		! Open main file (existing) for modification
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_PJL.CRE"
		USE
			CONTINUE 28490
		END WHEN
		AP_PJL.READONLY% = 0%
	END IF

28310	PO_NUMBER$ = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
		"", TEMP$, PO_NUMBER$, 2%, "~R 'E", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE SMG$K_TRM_EXIT
		GOTO 28490

	CASE SMG$K_TRM_F14
		IF MAIN_WINDOW(PO_MAIN_REGLINE.ID, "V2" + AP_PJH::VENNUM) = 1%
		THEN
			PO_NUMBER$ = PO_REG_LINE::PO
				AP_PJL::PO_LINE = PO_REG_LINE::PO_LINE
		END IF
		GOTO 28310

	END SELECT

	!
	! We now need to find the last line allocated to this transaction
	!
28350	LAST_LINE$ = "0000"

	WHEN ERROR IN
		GET #AP_PJL.CH%, KEY #0% EQ AP_PJH::TRANKEY, REGARDLESS
	USE
		CONTINUE 28400
	END WHEN

28360	WHILE (AP_PJL::TRANKEY = AP_PJH::TRANKEY)

		LAST_LINE$ = AP_PJL::SLINE

		WHEN ERROR IN
			GET #AP_PJL.CH%, REGARDLESS
		USE
			CONTINUE 28400
		END WHEN
	NEXT

28400	!
	! Open PO_REG_LINE file
	!
	IF PO_REG_LINE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.OPN"
		USE
			CONTINUE 28480
		END WHEN
	END IF

	!
	! Find the first line of the PO
	!
	WHEN ERROR IN
		FIND #PO_REG_LINE.CH%, KEY #0% GE PO_NUMBER$, REGARDLESS
	USE
		CONTINUE 28480
	END WHEN

28410	WHEN ERROR IN
		GET #PO_REG_LINE.CH%, REGARDLESS
	USE
		CONTINUE 28480
	END WHEN

	GOTO 28480 IF (PO_REG_LINE::PO <> PO_NUMBER$)

	GOTO 28410 &
		IF PO_READ_REG_LINE(PO_REG_LINE::PO, &
		PO_REG_LINE::PO_LINE, "EQ", PO_REG_LINE_READ, &
		PO_REG_SUB_LINE_READ, QTY(), CUTOFF$) <> CMC$_NORMAL

	BALANCE = FUNC_ROUND(QTY(2%) - QTY(9%), 2%)

28440	IF (BALANCE <> 0.0)
	THEN
		RATE = QTY(8%)
		BALANCE_RATE = FUNC_ROUND(BALANCE * RATE, 2%)

		LAST_LINE$ = FORMAT$(VAL%(LAST_LINE$) + 1%, "<0>###")
		LINE_COUNT% = LINE_COUNT% + 1%

		AP_PJL::TRANKEY		= AP_PJH::TRANKEY
		AP_PJL::SLINE		= LAST_LINE$
		AP_PJL::PONUM		= PO_REG_LINE::PO
		AP_PJL::PO_LINE		= PO_REG_LINE::PO_LINE
		AP_PJL::ACCT		= PO_REG_SUB_LINE_READ::ACCOUNT
		AP_PJL::SUBACC		= PO_REG_SUB_LINE_READ::SUBACCT
		AP_PJL::OPERATION	= ""
		AP_PJL::UNITS		= BALANCE
		AP_PJL::AMOUNT		= BALANCE_RATE
		AP_PJL::DISCAMT		= 0.0
		AP_PJL::USE_TAX_FLAG	= ""

		PUT #AP_PJL.CH%
	END IF

	GOTO 28410

28480	CALL ENTR_3MESSAGE(SCOPE, &
		"Loaded " + NUM1$(LINE_COUNT%) + " PO Lines", 0%)

28490	RETURN

	%PAGE

28900	!*******************************************************************
	! FNVAL%() function - FNVAL with error trapping
	!*******************************************************************

	DEF FNVAL%(X$)

		WHEN ERROR IN
			RESULT% = VAL%(X$)
		USE
			RESULT% = 0%
		END WHEN

		FNVAL% = RESULT%
	FNEND

29000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	ON ERROR GO BACK

29900	!
	! Handle no header for Line function here
	!
	CALL ENTR_3MESSAGE(SCOPE, &
		"Sorry, but there is no current header item", 0%)
	GOTO 32767

32767	END FUNCTION
