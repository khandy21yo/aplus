1	%TITLE "Vendor 1099 Maintenance"
	%SBTTL "AP_MAIN_1099_MAINT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AP_MAIN_1099_MAINT(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	.b
	!	.lm +5
	!	The ^*Vendor 1099 Maintenance\* program maintains the
	!	1099 Vendor file.
	!	.lm -5
	!
	! Index:
	!	.x Vendor 1099 Maintenance
	!	.x Maintenance>Vendor 1099
	!
	! COMPILE:
	!
	!	$ BAS AP_SOURCE:AP_MAIN_1099_MAINT/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AP_MAIN_1099_MAINT
	!	$ DELETE AP_MAIN_1099_MAINT.OBJ;*
	!
	! Author:
	!
	!	07/29/87 - Lance Williams
	!
	! Modification history:
	!
	!	05/19/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/27/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/20/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/08/99 - Kevin Handy
	!		Removed lines 760, 770, which were never called
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

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AP.OPEN]AP_1099_YYYY.HB"
	MAP (AP_1099_YYYY)	AP_1099_YYYY_CDD	AP_1099_YYYY
	MAP (AP_1099_YYYY2)	AP_1099_YYYY_CDD	AP_1099_YYYY_OLD, &
			AP_1099_YYYY2

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR)		AP_VENDOR_CDD	AP_VENDOR

	%INCLUDE "SOURCE:[AP.OPEN]AP_1099_TABLE.HB"
	MAP (AP_1099_TABLE)	AP_1099_TABLE_CDD	AP_1099_TABLE

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_AP_1099_YYYY) &
		AP_1099_YYYY.CH%, &
		AP_1099_YYYY.READONLY%, &
		YEAR_1099$ = 4%


	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION MAIN_WINDOW
	EXTERNAL LONG   FUNCTION FUNC_TESTENTRY

	ON ERROR GOTO 29000

	%PAGE

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
		SMG_WINDOW::DESCR = "AP 1099 Register " + YEAR_1099$ + &
			" Maintenance"
		SMG_WINDOW::NHELP = "AP_MAIN_1099_MAINT"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 130%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 9%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "1099_code"
		SMG_WINDOW::KFIELD(0%, 0%) = 2%
		SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KFIELD(0%, 2%) = 2%

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Declare channels
		!
		IF AP_1099_YYYY.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AP_1099_YYYY.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		%INCLUDE "SOURCE:[AP.OPEN]AP_1099_YYYY.CRE"
		AP_1099_YYYY.READONLY% = 0%
		GOTO 790

 !760
		!
		! If unable to open for modify, try to open
		! with read access only.
		!
 !		%INCLUDE "SOURCE:[AP.OPEN]AP_1099_YYYY.OPN"
 !		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
 !		AP_1099_YYYY.READONLY% = -1%
 !
 !		GOTO 790

 !770
		!
		! File not able to open, so reset channel
		!
 !		CALL ASSG_FREECHANNEL(AP_1099_YYYY.CH%)
 !
 !		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = AP_1099_YYYY.CH%
		RESET #AP_1099_YYYY.CH%
		GET #AP_1099_YYYY.CH%, REGARDLESS

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

		DATA	02, 15, "(01) Vendor Number", &
			03, 15, "(02) 1099 Code", &
			04, 15, "(03) Trans Key", &
			05, 15, "(04) Invoice Number", &
			06, 15, "(05) Invoice Date", &
			07, 15, "(06) Check Number", &
			08, 15, "(07) Check Date", &
			09, 15, "(08) 1099 Amount", &
			10, 15, "(09) Check Amount", &
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
			AP_1099_YYYY::VENNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "02;35", TEMP$, &
				AP_1099_YYYY::VENNUM, MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT
			CASE SMG$K_TRM_F14
				AP_1099_YYYY::VENNUM = AP_VENDOR::VENNUM &
					IF MAIN_WINDOW(AP_MAIN_VENDOR.ID, "V0") = 1%
				GOTO Reenter1

			CASE SMG$K_TRM_F17
				ST% = MAIN_WINDOW(AP_MAIN_VENDOR.ID, "M0" + &
					AP_1099_YYYY::VENNUM) = 1%
				GOTO Reenter1

			END SELECT

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) 1099 Amount\*
	!	.b
	!	.lm +5
	!	The ^*1099 Amount\* indicates the amount of the invoice or record to be
	!	included in the 1099 report.
	!	.lm -5
	!
	! Index:
	!	.x 1099 Amount>Register Maintenance
	!	.x Register Maintenance>1099 Amount
	!
	!--
			AP_1099_YYYY::CODE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "03;35", TEMP$, &
				AP_1099_YYYY::CODE, MFLAG, "'E", &
				MVALUE)

			SELECT SCOPE::SCOPE_EXIT
			CASE SMG$K_TRM_F14
				AP_1099_YYYY::CODE = AP_1099_TABLE::CODE &
					IF MAIN_WINDOW(AP_MAIN_1099_TABLE.ID, "V0") = 1%
				GOTO Reenter1
			END SELECT

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Transaction Key\*
	!	.b
	!	.lm +5
	!	The ^*Transaction Key\* refers to the desired transaction when the user is
	!	referencing. The desired transaction is entered as the key and the referencing
	!	is then completed.
	!	.lm -5
	!
	! Index:
	!	.x Transaction Key>Register Maintenance
	!	.x Register Maintenance>Transaction Key
	!
	!--
			AP_1099_YYYY::TRANKEY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "04;35", TEMP$, &
				AP_1099_YYYY::TRANKEY, MFLAG, "'E", MVALUE)

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
	!	The field will accept up to fifteen (15) characters.
	!	.lm -5
	!
	! Index:
	!	.x Invoice Number>Register Maintenance
	!	.x Register Maintenance>Invoice Number
	!
	!--
			AP_1099_YYYY::INVNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "05;35", TEMP$, &
				AP_1099_YYYY::INVNUM, MFLAG, "'E", MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!	^* (05) Invoice Date\*
	!	.b
	!	.lm +5
	!	The ^*Invoice Date\* field contains the date which appears
	!	on the vendor's invoice.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Invoice Date
	!
	!--
			AP_1099_YYYY::INVDAT = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "06;35", TEMP$, &
				AP_1099_YYYY::INVDAT, MFLAG, "8", MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Check Number\*
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
	!	.x Check Number>Register Maintenance
	!	.x Register Maintenance>Check Number
	!
	!--
			AP_1099_YYYY::CKNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "07;35", TEMP$, &
				AP_1099_YYYY::CKNUM, MFLAG, "'E", MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	^*(07) Check Date\*
	!	.b
	!	.lm +5
	!	If a ^*Check Number\* is entered in field (12) in a Purchases
	!	Journal record, the cursor will stop at the ^*Check Date\* field
	!	in order for the appropriate check date to be entered in MMDDYYYY
	!	format.
	!	.b
	!	If the Check _# field is blank the system will automatically
	!	bypass the ^*Check Date\* field.
	!	.lm -5
	!
	! Index:
	!	.x Check Date>Register Maintenance
	!	.x Register Maintenance>Check Date
	!
	!--
			AP_1099_YYYY::CKDAT = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "08;35", TEMP$, &
				AP_1099_YYYY::CKDAT, MFLAG, "8", MVALUE)

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
	!	.x 1099 Amount>Register Maintenance
	!	.x Register Maintenance>1099 Amount
	!
	!--
			AP_1099_YYYY::AMT1099 = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "09;35", TEMP$, &
				AP_1099_YYYY::AMT1099 * 1.0, MFLAG, &
				"###,###,###.##", MVALUE)

		CASE 9%

	!++
	! Abstract:FLD009
	!	^*(09) Check Amount\*
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
	!	.x Check Amount>Register Maintenance
	!	.x Register Maintenance>Check Amount
	!
	!--
			AP_1099_YYYY::CKAMT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "10;35", TEMP$, &
				AP_1099_YYYY::CKAMT * 1.0, MFLAG, &
				"###,###,###.##", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		AP_MAIN_1099_MAINT = 0%

		SELECT MLOOP
		CASE 1%
			SELECT EDIT$(SCOPE::PRG_ITEM, 2% + 4% + 32%)
			CASE "BLANK"
				!
				! Don't allow blank vendor numbers
				!
				AP_MAIN_1099_MAINT = 2%

			CASE ELSE
				!
				! Don't allow blank vendor numbers
				!
				IF AP_1099_YYYY::VENNUM = ""
				THEN
					TEMP1$ = TRM$(SCOPE::PRG_ITEM)

					V% = MAIN_WINDOW(AP_MAIN_VENDOR.ID, "V0")
					AP_1099_YYYY::VENNUM = AP_VENDOR::VENNUM &
						IF SCOPE::SCOPE_EXIT = SMG$K_TRM_SELECT

					SCOPE::SCOPE_EXIT = 0%
					AP_MAIN_1099_MAINT = 1%
					SCOPE::PRG_ITEM = TEMP1$
				END IF

				!
				! Is the input defined?
				!
				AP_MAIN_1099_MAINT = FUNC_TESTENTRY(SMG_WINDOW, &
					AP_1099_YYYY::VENNUM, AP_VENDOR::VENNAM, &
					"AP", MLOOP, "VENNUM", &
					"Vendor number", AP_MAIN_VENDOR.ID)
				!
				! Print Vendor Name
				!
				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					LEFT(AP_VENDOR::VENNAM, 30%), 2%, 47%,, &
					SMG$M_BOLD)
			END SELECT

		CASE 2%
			!
			! Is the input defined?
			!
			AP_MAIN_1099_MAINT = FUNC_TESTENTRY(SMG_WINDOW, &
				AP_1099_YYYY::CODE, AP_1099_TABLE::DESCR, &
				"AP", MLOOP, "1099CD", &
				"1099 code", AP_MAIN_1099_TABLE.ID)

			!
			! Print 1099 description
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(AP_1099_TABLE::DESCR, 30%), 3%, 47%,, &
				SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY
		AP_MAIN_1099_MAINT = 0%

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			!
			! Is the input defined?
			!
			IF MAIN_WINDOW(AP_MAIN_VENDOR.ID, &
				"Q0" + AP_1099_YYYY::VENNUM) <> 1%
			THEN
				AP_VENDOR::VENNAM = "????????????????????"
			END IF
			!
			! Print Vendor Name
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(AP_VENDOR::VENNAM, 30%), 2%, 47%,, &
				SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			!
			! Is the input defined?
			!
			IF MAIN_WINDOW(AP_MAIN_1099_TABLE.ID, &
				"Q0" + AP_1099_YYYY::CODE) <> 1%
			THEN
				AP_1099_TABLE::DESCR = "????????????????????"
			END IF
			!
			! Print 1099 description
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(AP_1099_TABLE::DESCR, 30%), 3%, 47%,, &
				SMG$M_BOLD)
		END IF

	!
	! Set AP_1099_YYYY_OLD value
	!
20500	CASE OPT_SETOLD
		AP_1099_YYYY_OLD = AP_1099_YYYY

	!
	! Restore AP_1099_YYYY_OLD value
	!
	CASE OPT_RESETOLD
		AP_1099_YYYY = AP_1099_YYYY_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		AP_1099_YYYY2 = AP_1099_YYYY

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		AP_1099_YYYY = AP_1099_YYYY2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Vendor #   Code Trans# " + &
				"Invoice Number  Invoice Date " + &
				"Check# Check Date  1099 Amount " + &
				"  Check Amount"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "013,018,025,041,054,061,072,085,100"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = &
				AP_1099_YYYY::VENNUM + "  " + &
				AP_1099_YYYY::CODE + "  " + &
				AP_1099_YYYY::TRANKEY + " " + &
				AP_1099_YYYY::INVNUM + " " + &
				PRNT_DATE(AP_1099_YYYY::INVDAT, 8%) + "   " + &
				AP_1099_YYYY::CKNUM + " " + &
				PRNT_DATE(AP_1099_YYYY::CKDAT, 8%) + " " + &
				FORMAT$(AP_1099_YYYY::AMT1099, &
					"#,###,###.##") + "   " + &
				FORMAT$(AP_1099_YYYY::CKAMT, "#,###,###.##")
		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #AP_1099_YYYY.CH%, KEY #0% &
				GE AP_1099_YYYY::VENNUM + AP_1099_YYYY::CODE, &
				REGARDLESS
		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

29000	!
	! Trap Errors
	!
	RESUME ExitFunction

32767	END FUNCTION
