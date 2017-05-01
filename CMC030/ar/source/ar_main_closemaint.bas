1	%TITLE "AR Ledger Maintenance"
	%SBTTL "AR_MAIN_CLOSEMAINT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AR_MAIN_CLOSEMAINT(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	This program maintains the ledger file.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_MAIN_CLOSEMAINT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP AR_MAIN_CLOSEMAINT
	!	$ DELETE AR_MAIN_CLOSEMAINT.OBJ;*
	!
	!
	! Author:
	!
	!	02/23/88 - Aaron Redd
	!
	! Modification history:
	!
	!	05/16/88 - Aaron Redd
	!		Modified to allow R/O open of file if R/W open fails.
	!
	!	08/05/88 - Kevin Handy
	!		Fixed so that test for ARACCT occured at item 10,
	!		and not at item 08.
	!
	!	08/29/88 - Kevin Handy
	!		Fixed position of first five items so they show up
	!		in the proper positions.
	!
	!	04/15/92 - Dan Perkins
	!		Cut out extra code in connection with FUNC_
	!		TESTENTRY.  Use MLOOP in function arguement.
	!
	!	05/19/92 - Kevin Handy
	!		Added in more than one key.
	!
	!	05/20/92 - Kevin Handy
	!		Added check number and batch number to view.
	!
	!	02/10/93 - Dan Perkins
	!		Added fields for DUEDATE and DISCOUNTDATE.
	!		Added salesman key to OPT_FIND.
	!
	!	02/12/93 - Dan Perkins
	!		Changed "V0" to "VX" on chart of accounts to be
	!		able to list accounts starting at a particular
	!		account.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/14/96 - Kevin Handy
	!		Add two characters to Amount field in view.
	!		Reformat source code.
	!
	!	06/19/96 - Kevin Handy
	!		Added transaction type "11".
	!
	!	07/03/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/25/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP	(AR_CONTROL)	AR_CONTROL_CDD	AR_CONTROL

	%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.HB"
	MAP	(AR_CLOSED)	AR_CLOSED_CDD	AR_CLOSED
	MAP	(AR_CLOSED_OLD)	AR_CLOSED_CDD	AR_CLOSED_OLD, AR_CLOSED2

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD	GL_CHART

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)		AR_35CUSTOM_CDD	AR_35CUSTOM

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_AR_CLOSED) &
		AR_CLOSED.CH%, &
		AR_CLOSED.READONLY%

	COM (TT_AR_CLOSEMAINT) &
		ARTITLE$ = 20%, &
		ARTYPE$(9%) = 20%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG    FUNCTION MAIN_WINDOW

	ON ERROR GOTO 29000

	%PAGE

	SELECT MOPTION

	!*******************************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!
	!*******************************************************************
	CASE OPT_INIT

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "AR Close Ledger Maintenance"
		SMG_WINDOW::NHELP = "AR_MAIN_CLOSEMAINT"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 130%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 18%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = TRM$(AR_CONTROL::CTITLE)
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
		SMG_WINDOW::KNAME(1%) = "Salesman"
			SMG_WINDOW::KFIELD(1%, 0%) = 3%
			SMG_WINDOW::KFIELD(1%, 1%) = 17%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%
			SMG_WINDOW::KFIELD(1%, 3%) = 2%

		!
		! AR type
		!
		ARTITLE$ = "Type   Description"
		ARTYPE$(0%) = "8"
		ARTYPE$(1%) = "01   Receivable"
		ARTYPE$(2%) = "02   Cash Sale"
		ARTYPE$(3%) = "03   Debit Memo"
		ARTYPE$(4%) = "04   Service Charge"
		ARTYPE$(5%) = "08   Credit Memo"
		ARTYPE$(6%) = "09   ROA"
		ARTYPE$(7%) = "10   Cash Receipt"
		ARTYPE$(8%) = "11   Adjustment"

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF AR_CLOSED.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AR_CLOSED.READONLY%

			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			AR_MAIN_CLOSEMAINT = ERR
			CONTINUE 770
		END WHEN

		AR_CLOSED.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.OPN"
		USE
			AR_MAIN_CLOSEMAINT = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		AR_CLOSED.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(AR_CLOSED.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = AR_CLOSED.CH%
		WHEN ERROR IN
			RESET #AR_CLOSED.CH%
			GET #AR_CLOSED.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

20100	!*******************************************************************
	!
	! Display the background
	!
	! This option is used to display the background information on the
	! screen.  It must first clear any junk on the screen, and then
	! write the background onto it.
	!
	!*******************************************************************
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	1,  3, "(01)", &
			2,  3, "(02) Invoice", &
			3,  3, "(03) Type", &
			4,  3, "(04) Tran Date", &
			5,  3, "(05) Due Date", &
			6,  3, "(06) Disc Date", &
			7,  3, "(07) Amount", &
			8,  3, "(08) Discount", &
			9,  3, "(09) Other", &
			10, 3, "(10) Receipt No", &
			11, 3, "(11) Check No", &
			12, 3, "(12) Account", &
			13, 3, "(13) Matter #", &
			14, 3, "(14) Description", &
			15, 3, "(15) Batch", &
			16, 3, "(16) Update", &
			17, 3, "(17) Salesman", &
			18, 3, "(18) Close Date", &
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

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			LEFT(AR_CONTROL::CTITLE, 14%), 1%, 8%)

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

20200	!*******************************************************************
	!
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
	!
	!*******************************************************************
	CASE OPT_ENTRY

		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		SCOPE::SCOPE_EXIT = 0%

 E0loop:	SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Customer\*
	!	.b
	!	.lm +5
	!	The ^*Customer\* number refers to the number assigned to a customer as recorded
	!	in the Customer Master File.
	!	.b
	!	^*Note:\* It is ^*not\* recommended that Customer Numbers be changed,
	!	added or deleted in this option. Nevertheless, the ability to accomplish
	!	these kinds of edit functions is provided, but should be used in very unusual
	!	circumstances only.
	!	.lm -5
	!
	! Index:
	!	.x Customer>Close Ledger
	!	.x Close Ledger>Customer
	!
	!--
			AR_CLOSED::CUSNUM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"1;21", TEMP$, &
				AR_CLOSED::CUSNUM, MFLAG, "'E", MVALUE)

				IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
				THEN
					IF (MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, "V0  ") = 1%)
					THEN
						AR_CLOSED::CUSNUM = &
							AR_35CUSTOM::CUSNUM
					END IF
					GOTO E0Loop
				END IF

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Invoice\*
	!	.b
	!	.lm +5
	!	The ^*Invoice\* number refers to a reference number whether the record
	!	represents an invoice or credit memo.
	!	.b
	!	^*Note:\*  It is not recommended that Invoice Numbers by changed in this option.
	!	Nevertheless, the ability to accomplish this function is provided, but should
	!	be used in very unusual circumstances only.\*
	!	.lm -5
	!
	! Index:
	!	.x Invoice>Close Ledger
	!	.x Close Ledger>Invoice
	!
	!--
			AR_CLOSED::INVNUM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;21", TEMP$, &
				AR_CLOSED::INVNUM, MFLAG, "'E", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	.x Type>Closing Ledger
	!	^*(03) Type\*
	!	.b
	!	.lm +5
	!	The ^*Type\* field contains a code which indicates the type of transaction
	!	this screen represents.
	!	.b
	!	Valid types are:
	!	.table 3,25
	!	.te
	!	^*01\* - Invoice - Entered through Sales Journal.
	!	.te
	!	^*02\* - Cash Sale - Entered through Sales Journal.
	!	.te
	!	^*03\* - Debit Memo - Entered through Sales Journal.
	!	.te
	!	^*04\* - Service Charge - Created by SC calculation process.
	!	.te
	!	^*08\* - Credit Memo - Entered through Sales Journal.
	!	.te
	!	^*09\* - Cash Received on Account - Entered through CRJ.
	!	.te
	!	^*10\* - Cash Received - Cash received other.
	!	.end table
	!	^*Note:\* It is ^*not\* recommended that the type field be changed
	!	in this screen, even though the ability to accomplish this
	!	edit function is provided.\*
	!	.lm -5
	!
	! Index:
	!	.x Closing Ledger>Type
	!
	!--
			AR_CLOSED::TRATYP = ENTR_3STRINGLIST(SCOPE, SMG_WINDOW::WNUMBER, &
				"3;21", TEMP$, &
				AR_CLOSED::TRATYP, MFLAG, "'E", MVALUE, &
				ARTYPE$(), ARTITLE$, "005")

		CASE 4%
	!++
	! Abstract:FLD004
	!	.x Transaction Date>Close Ledger
	!	^*(04) Transaction Date\*
	!	.b
	!	.lm +5
	!	The ^*Transaction Date\* field represents the date of a
	!	particular transaction.
	!	.B
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.b
	!	The field will default to the system date. To accept the system
	!	date, press ^*Return\*. To override the default, enter the correct
	!	date and press ^*Return\*.
	!	.lm -5
	!
	! Index:
	!	.x Close Ledger>Transaction Date
	!
	!--
			AR_CLOSED::TRADAT = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;21", TEMP$, &
				AR_CLOSED::TRADAT, MFLAG, "'E", MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	.x Due Date>Close Ledger
	!	^*(05) Due Date\*
	!	.b
	!	.lm +5
	!	The ^*Due Date\* field represents the date of a
	!	particular transaction on which the amount is due.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Close Ledger>Due Date
	!
	!--
			AR_CLOSED::DUEDATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;21", TEMP$, &
				AR_CLOSED::DUEDATE, MFLAG, "'E", MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	.x Discount Date>Close Ledger
	!	^*(06) Discount Date\*
	!	.b
	!	.lm +5
	!	The ^*Discount Date\* field represents the date of a
	!	particular transaction on which a discount may be taken.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Close Ledger>Discount Date
	!
	!--
			AR_CLOSED::DISCOUNTDATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;21", TEMP$, &
				AR_CLOSED::DISCOUNTDATE, MFLAG, "'E", MVALUE)

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(07) Amount\*
	!	.b
	!	.lm +5
	!	The ^*Amount\* field displays the amount of the transaction whether it is
	!	a "Sale" or a "Receipt".
	!	.b
	!	^*Note:\* It is ^*not\* recommended that the amount field be changed in this
	!	screen even though the ability to do so exists.
	!	.lm -5
	!
	! Index:
	!	.x Amount>Close Ledger
	!	.x Close Ledger>Amount
	!
	!--
			AR_CLOSED::SALAMT = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"7;21", TEMP$, &
				AR_CLOSED::SALAMT, MFLAG, "######.##", MVALUE)

		CASE 8%
	!++
	! Abstract:FLD008
	!	^*(08) Discount\*
	!	.b
	!	.lm +5
	!	The ^*Discount\* field contains a discount amount if applicable.
	!	.b
	!	^*Note:\* It is ^*not\* recommended that the discount amount be
	!	changed in this screen, even though the ability to do so exists.
	!	.lm -5
	!
	! Index:
	!	.x Discount>Close Ledger
	!	.x Close Ledger>Discount
	!
	!--
			AR_CLOSED::DISAMT = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"8;21", TEMP$, &
				AR_CLOSED::DISAMT, MFLAG, "######.##", MVALUE)

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*(09) Other\*
	!	.b
	!	.lm +5
	!	The ^*Other\* field contains any other charges that pertain to a "Sale".
	!	This may include Sales Tax, Freight, etc.
	!	.b
	!	^*Note:\*  It is ^*not\* recommended that changes be made in this screen,
	!	even though the ability to do so exists.
	!	.lm -5
	!
	! Index:
	!	.x Other>Close Ledger
	!	.x Close Ledger>Other
	!
	!--
			AR_CLOSED::OTHCHG = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"9;21", TEMP$, &
				AR_CLOSED::OTHCHG, MFLAG, "######.##", MVALUE)

		CASE 10%
	!++
	! Abstract:FLD010
	!	^*(10) Receipt Number\*
	!	.b
	!	.lm +5
	!	The ^*Receipt Number\* field contains the receipt number when the transaction
	!	is a cash receipt type.
	!	.b
	!	^*Note:\* It is ^*not\* recommended that changes be made in this
	!	screen, even though the ability to do so exists.
	!	.lm -5
	!
	! Index:
	!	.x Receipt Number>Close Ledger
	!	.x Close Ledger>Receipt Number
	!
	!--
			AR_CLOSED::RECNUM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;21", TEMP$, &
				AR_CLOSED::RECNUM, MFLAG, "'E", MVALUE)

		CASE 11%
	!++
	! Abstract:FLD011
	!	^*(11) Check Number\*
	!	.b
	!	.lm +5
	!	The ^*Check Number\* field contains the customers check
	!	number when the type of transaction is a cash receipt.
	!	.lm -5
	!
	! Index:
	!
	!--
			AR_CLOSED::CHKNUM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"11;21", TEMP$, &
				AR_CLOSED::CHKNUM, MFLAG, "'E", MVALUE)

		CASE 12%
	!++
	! Abstract:FLD012
	!	^*(12) Account\*
	!	.b
	!	.lm +5
	!	The ^*Account\* number field contains the General Ledger Cash account number
	!	whenever the transaction is a cash sale, or the Accounts Receivable General
	!	Ledger account number if the transaction relates to an "on account" transaction.
	!	.b
	!	^*Note:\*  It is ^*not\* recommended that changes be made in this screen
	!	even though the ability to do so exists.
	!	.lm -5
	!
	! Index:
	!	.x Account>Close Ledger
	!	.x Close Ledger>Account
	!
	!--
			AR_CLOSED::ARACCT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"12;21", TEMP$, &
				AR_CLOSED::ARACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
				THEN
					AR_CLOSED::ARACCT = &
						GL_CHART::ACCT
				END IF
				GOTO E0Loop
			END IF

		CASE 13%
	!++
	! Abstract:FLD013
	!	^*(13) Matter _#\*
	!	.b
	!	.lm +5
	!	The ^*Matter _#\* field contains the identification number of different
	!	transactions for each customer. They are used especially for billing
	!	when the customer has more than one transaction with the organization.
	!	.b
	!	^*Note:\* It is ^*not\* recommended that changes be made in this screen even though the
	!	ability to do so exists.
	!	.lm -5
	!
	! Index:
	!	.x Matter _#>Close Ledger
	!	.x Close Ledger>Matter _#
	!
	!--
			AR_CLOSED::SUBACC = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"13;21", TEMP$, &
				AR_CLOSED::SUBACC, MFLAG, "~L0'E", MVALUE)

		CASE 14%
	!++
	! Abstract:FLD014
	!	^*(14) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field contains the description for a transaction if one
	!	were entered in the Receipts Journal or the Sales Journal.
	!	.lm -5
	!
	! Index:
	!	.x Description>Close Ledger
	!	.x Close Ledger>Description
	!
	!--
			AR_CLOSED::DESCR = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"14;21", TEMP$, &
				AR_CLOSED::DESCR, MFLAG, "'E", MVALUE)

		CASE 15%
	!++
	! Abstract:FLD015
	!	^*(15) Batch\*
	!	.b
	!	.lm +5
	!	The ^*Batch\* field refers to the posting batch number that was assigned
	!	during the posting process. The number contains up to six (6) characters.
	!	It is assigned by the posting process. The user has no control over this
	!	number.
	!	.b
	!	^*Note:\* It is ^*not\* recommended that changes be made in this
	!	screen even though the ability to do so exists.
	!	.lm -5
	!
	! Index:
	!	.x Batch>Close Ledger
	!	.x Close Ledger>Batch
	!
	!--
			AR_CLOSED::BATCH = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"15;21", TEMP$, &
				AR_CLOSED::BATCH, MFLAG, "'E", MVALUE)

		CASE 16%
	!++
	! Abstract:FLD016
	!	^*(16) Update\*
	!	.b
	!	.lm +5
	!	The ^*Update\* field contains the month, day, and year in which a transaction
	!	is updated.
	!	.b
	!	^*Note:\* It is ^*not\* recommended that changes be made in this
	!	field even though the ability to do so exists.
	!	.lm -5
	!
	! Index:
	!	.x Update>Close Ledger
	!	.x Close Ledger>Update
	!
	!--
			AR_CLOSED::UPDATED = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"16;21", TEMP$, &
				AR_CLOSED::UPDATED, MFLAG, "'E", MVALUE)

		CASE 17%
	!++
	! Abstract:FLD017
	!	^*(17) Salesman\*
	!	.b
	!	.lm +5
	!	The ^*Salesman\* field refers to the person who made and completed the sale
	!	of merchandise or service.
	!	.lm -5
	!
	! Index:
	!	.x Salesman>Close Ledger
	!	.x Close Ledger>Salesman
	!
	!--
			AR_CLOSED::SALNUM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"17;21", TEMP$, &
				AR_CLOSED::SALNUM, MFLAG, "'E", MVALUE)

		CASE 18%
	!++
	! Abstract:FLD018
	!	^*(18) Close Date\*
	!	.b
	!	.lm +5
	!	The ^*Close Date\* field refers to the date in which the account was closed.
	!	In order to be closed, the account balance must be zero.
	!	.lm -5
	!
	! Index:
	!	.x Close Date>Close Ledger
	!	.x Close Ledger>Close Date
	!
	!--
			AR_CLOSED::CLOSEDATE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"18;21", TEMP$, &
				AR_CLOSED::CLOSEDATE, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

20300	CASE OPT_TESTENTRY

		AR_MAIN_CLOSEMAINT = 0%

		SELECT MLOOP

		CASE 1%
			AR_MAIN_CLOSEMAINT = FUNC_TESTENTRY(SMG_WINDOW, &
				AR_CLOSED::CUSNUM, AR_35CUSTOM::CUSNAM, &
				"AR", MLOOP, "CUSNAM", &
				"Customer number", AR_MAIN_35CUSTOM.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(AR_35CUSTOM::CUSNAM, 30%), &
				1%, 45%, , SMG$M_BOLD)

		CASE 12%
			AR_MAIN_CLOSEMAINT = FUNC_TESTENTRY(SMG_WINDOW, &
				AR_CLOSED::ARACCT, GL_CHART::DESCR, &
				"GL", MLOOP, "ACCT", &
				"Account number", GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 30%), &
				12%, 45%, , SMG$M_BOLD)

		END SELECT


20500	CASE OPT_SETOLD
		AR_CLOSED_OLD = AR_CLOSED

	CASE OPT_RESETOLD
		AR_CLOSED = AR_CLOSED_OLD

	CASE OPT_SETDEFAULT
		AR_CLOSED2 = AR_CLOSED

	CASE OPT_RESETDEFAULT
		AR_CLOSED = AR_CLOSED2

	CASE OPT_VIEW
		SELECT MLOOP

		CASE 1%
			MVALUE = "  Cust #     Inv Num    Tp Date      " + &
				"   Amount Sales Tx Description       " + &
				"  Check     Batch"

		CASE 2%
			MVALUE = "013,022,025,036,047,056,075,083"

		CASE 3%
			MVALUE = AR_CLOSED::CUSNUM + " " + &
				AR_CLOSED::INVNUM + " " + &
				AR_CLOSED::TRATYP + " " + &
				PRNT_DATE(AR_CLOSED::TRADAT, 8%) + " " + &
				FORMAT$(AR_CLOSED::SALAMT, "#######.##") + " " + &
				FORMAT$(AR_CLOSED::OTHCHG, "#####.##") + " " + &
				LEFT(AR_CLOSED::DESCR, 20%) + " " + &
				AR_CLOSED::CHKNUM + &
				AR_CLOSED::BATCH

		END SELECT

	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #AR_CLOSED.CH%, KEY #0% GE AR_CLOSED::CUSNUM + &
				AR_CLOSED::INVNUM, REGARDLESS

		CASE 1%
			FIND #AR_CLOSED.CH%, KEY #1% GE AR_CLOSED::SALNUM + &
				AR_CLOSED::CUSNUM + &
				AR_CLOSED::INVNUM, REGARDLESS

		END SELECT

	END SELECT

	EXIT FUNCTION

29000	!
	! Error trapping
	!
	ON ERROR GO BACK

32767	END FUNCTION
