1	%TITLE "Journal Entry"
	%SBTTL "IC_MAIN_JOURNAL"
	%IDENT "V3.6a Calico"

	FUNCTION LONG IC_MAIN_JOURNAL(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	!	The ^*Journal Entry\* option
	!	maintains journal transactions.
	!	.b
	!	After selecting this option, a screen will appear for the selection
	!	of an existing batch number. If a new batch number is to be created,
	!	press ^*Return\* to bypass the existing batch number screen. A new screen
	!	will appear accommodating entry of a new batch number.
	!	.lm -5
	!
	! Index:
	!	.x Transaction Journal>Maintenance
	!	.x Transaction Journal>Add
	!	.x Transaction Journal>Erase
	!	.x Transaction Journal>Change
	!	.x Maintain>Transaction Journal
	!	.x Add>Transaction Journal
	!	.x Erase>Transaction Journal
	!	.x Change>Transaction Journal
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_MAIN_JOURNAL/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN IC_MAIN_JOURNAL
	!	$ DELETE IC_MAIN_JOURNAL.OBJ;*
	!
	! Author:
	!
	!	06/15/2000 - Kevin Handy
	!		Split out from IC_JOUR_JOURNAL
	!
	! Modification history:
	!
	!	06/15/2000 - Kevin Handy
	!		Make UOM actually display something, instead of
	!		a UOM$ variable that was never set up. (KBJ)
	!
	!	06/29/2000 - Kevin Handy
	!		Lose unused functions PD_MAIN_ACCOUNT, UTL_MAIN_ACCOUNT
	!
	!	08/22/2000 - Kevin Handy
	!		Lose definition of PD_MAIN_PRODUCT, never used.
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
	!
	!	07/02/2001 - Kevin Handy
	!		Don't include the option ic_main_journal$help,
	!		because that just makes a circular list.
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:IC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	%INCLUDE "SOURCE:[IC.OPEN]IC_JOURNAL.HB"
	MAP (IC_JOURNAL)	IC_JOURNAL_CDD		IC_JOURNAL
	MAP (IC_JOURNAL_OLD) IC_JOURNAL_CDD IC_JOURNAL_OLD, IC_JOURNAL2

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSTYPE.HB"
	MAP (UTL_TRANSTYPE)	UTL_TRANSTYPE_CDD	UTL_TRANSTYPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[PD.OPEN]PD_ACCOUNT.HB"
	MAP (PD_ACCOUNT)	PD_ACCOUNT_CDD		PD_ACCOUNT

	!
	! This common area must be mapped in both the main program and
	! in MAINT_GROUP.
	!
	COM (CH_IC_JOURNAL) &
		BATCH_NO$ = 2%

	COM (DSPL_ORDER_XYZ) &
		AVAILABLE, &
		LONG TEMP_DISPLAY

	COM (FRM) FRM$(14%)

	!
	! External functions
	!
	EXTERNAL REAL  FUNCTION PC_READ_COST

	EXTERNAL LONG  FUNCTION UTL_MAIN_LOCATION, &
		UTL_MAIN_TRANSTYPE, &
		GL_MAIN_CHART, &
		IC_WRIT_35BALANCE, &
		FUNC_TESTENTRY, &
		IC_DSPL_35BALANCE

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!
	! Initilization
	!
	CASE OPT_INIT

		!*****************************************************
		! Set up information
		!*****************************************************

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Inventory Transaction Journal " + &
			BATCH_NO$
		SMG_WINDOW::NHELP = "IC_MAIN_JOURNAL"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 14%

		SMG_WINDOW::NKEYS = 4%
		SMG_WINDOW::KNAME(0%) = "Product_num"
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
		SMG_WINDOW::KNAME(1%) = "primary_Ref"
			SMG_WINDOW::KFIELD(1%, 0%) = 3%
			SMG_WINDOW::KFIELD(1%, 2%) = 5%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%
			SMG_WINDOW::KFIELD(1%, 3%) = 2%
		SMG_WINDOW::KNAME(2%) = "Cross_ref"
			SMG_WINDOW::KFIELD(2%, 0%) = 3%
			SMG_WINDOW::KFIELD(2%, 2%) = 6%
			SMG_WINDOW::KFIELD(2%, 2%) = 1%
			SMG_WINDOW::KFIELD(2%, 3%) = 2%
		SMG_WINDOW::KNAME(3%) = "Subaccount"
			SMG_WINDOW::KFIELD(3%, 0%) = 3%
			SMG_WINDOW::KFIELD(3%, 2%) = 7%
			SMG_WINDOW::KFIELD(3%, 2%) = 1%
			SMG_WINDOW::KFIELD(3%, 3%) = 2%

		SMG_WINDOW::HVIEW = 130%
		SMG_WINDOW::VVIEW = 18%

		CALL READ_DEFAULTS(SMG_WINDOW)

20700		IF (IC_JOURNAL.CH% <= 0%)
		THEN
			!
			! Open main file (existing) for modification
			!
			WHEN ERROR IN
				%INCLUDE "SOURCE:[IC.OPEN]IC_JOURNAL.CRE"
			USE
				CALL ENTR_3MESSAGE(SCOPE, &
					"Unable to open IC_JOURNAL file " + &
					NUM1$(ERR), 0%)
				IC_MAIN_JOURNAL = 1%
				CONTINUE 27000
			END WHEN
		END IF

20710		SMG_WINDOW::CHAN  = IC_JOURNAL.CH%

		WHEN ERROR IN
			RESET #IC_JOURNAL.CH%
			GET #IC_JOURNAL.CH%, REGARDLESS
		USE
			CONTINUE 27000 IF ERR = 11%
			EXIT HANDLER
		END WHEN

	!
	! Display window background
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = &
			SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	02,04, "(01) Product #", &
			03,04, "(02) Location 1", &
			04,04, "(03) Location 2", &
			05,04, "(04) Date", &
			07,04, "(05) Prim Ref", &
			08,04, "(06) Cross Ref", &
			09,04, "(07) Subaccount", &
			10,04, "(08) Operator", &
			13,04, "(09) Trans Type A", &
			14,04, "(10) Quantity A", &
			15,04, "(11) Trans Type B", &
			16,04, "(12) Quantity B", &
			17,04, "(13) Unit Cost Amt", &
			18,04, "(14) Trans Account", &
			0, 0, ""

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

	!
	! Enter/Display/Default
	!
20800	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Product _#\*
	!	.b
	!	.lm +5
	!	The ^*Product _#\* field enters the number
	!	which references this particular product.
	!	.b
	!	Valid product _#'s may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Product Number>Transaction Journal
	!	.x Transaction Journal>Product Number
	!
	!--
			IC_JOURNAL::PRODUCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "02;23", TEMP$, &
				IC_JOURNAL::PRODUCT, MFLAG, "'E", &
				MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14
				PD_PRODUCT::PROD_TYPE = "??"
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX") = 1%
				THEN
					IC_JOURNAL::PRODUCT = &
						PD_PRODUCT::PRODUCT_NUM
				END IF
				GOTO Reenter

			CASE SMG$K_TRM_F17
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "M0") = 1%
				THEN
					IC_JOURNAL::PRODUCT = &
						PD_PRODUCT::PRODUCT_NUM
				END IF
				GOTO Reenter

			END SELECT

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Location 1\*
	!	.b
	!	.lm +5
	!	The ^*Location 1\* field enters a selected
	!	location code pertaining to this particular record.
	!	.b
	!	Valid location codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Location 1>Transaction Journal
	!	.x Transaction Journal>Location 1
	!
	!--
			IC_JOURNAL::LOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "03;23", TEMP$, &
				IC_JOURNAL::LOCATION, MFLAG, "'E", &
				MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "VX") = 1%
				THEN
					IC_JOURNAL::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO Reenter

			END SELECT

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Location 2\*
	!	.b
	!	.lm +5
	!	The ^*Location 2\* field enters a selected
	!	location code pertaining to this particular record.
	!	.b
	!	Valid location codes may be viewed by
	!	pressing ^*List Choices\*.
	!	.lm +5
	!
	! Index:
	!	.x Location 2>Transaction Journal
	!	.x Transaction Journal>Location 2
	!
	!--
			IC_JOURNAL::TOLOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "04;23", TEMP$, &
				IC_JOURNAL::TOLOCATION, MFLAG, "'E", &
				MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "VX") = 1%
				THEN
					IC_JOURNAL::TOLOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO Reenter

			END SELECT

		CASE 4%
	!++
	! Abstract:FLD004
	!	.x Date>Transaction Journal
	!	^*(04) Date\*
	!	.b
	!	.lm +5
	!	The ^*Date\* field enters the valid date for
	!	this particular record.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Transaction Journal>Date
	!
	!--
			IC_JOURNAL::TRANS_DATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "05;23", TEMP$, &
				IC_JOURNAL::TRANS_DATE, MFLAG, "'E", &
				MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Primary Reference\*
	!	.b
	!	.lm +5
	!	The ^*Primary Reference\* field contains the number which posting in the
	!	General Ledger will be completed under.  An example of a Primary
	!	Reference number is the invoice number.
	!	.lm -5
	!
	! Index:
	!	.x Primary Reference
	!
	!--
			IC_JOURNAL::PRIMARY_REF = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "07;23", TEMP$, &
				IC_JOURNAL::PRIMARY_REF, MFLAG, "'E", &
				MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Cross Reference\*
	!	.b
	!	.lm +5
	!	The ^*Cross Reference\* field refers to the individual who originated the
	!	transaction. Examples of a ^*Cross Reference\* are a customer or vendor
	!	number. This entry connects the transaction to the
	!	person responsible.
	!	.lm -5
	!
	! Index:
	!	.x Cross Reference
	!
	!--
			IC_JOURNAL::CROSS_REF = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "08;23", TEMP$, &
				IC_JOURNAL::CROSS_REF, MFLAG, "'E", &
				MVALUE)

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(07) Subaccount\*
	!	.b
	!	.lm +5
	!	The ^*Subaccount\* field creates a cross matrix for the
	!	subledger. Each entry refers to the specific job identification. Examples of
	!	a ^*Subaccount\* are job _# and work order.
	!	.lm -5
	!
	! Index:
	!	.x Subaccount
	!
	!--
			IC_JOURNAL::SUBACCOUNT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "09;23", TEMP$, &
				IC_JOURNAL::SUBACCOUNT, MFLAG, "'E", &
				MVALUE)

		CASE 8%
	!++
	! Abstract:FLD008
	!	^*(08) Operator\*
	!	.b
	!	.lm +5
	!	The ^*Operator\* field enters the code
	!	which will identify the employee responsible for this
	!	particular journal entry.
	!	.lm -5
	!
	! Index:
	!	.x Operator>Transaction Journal
	!	.x Transaction Journal>Operator
	!
	!--
			IC_JOURNAL::STATIONMAN = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "10;23", TEMP$, &
				IC_JOURNAL::STATIONMAN, MFLAG, "'E", &
				MVALUE)

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*(09) Transaction Type A\*
	!	.b
	!	.lm +5
	!	The ^*Transaction Type A\* field enters a valid
	!	transaction type code which has been established in the transaction
	!	type file.
	!	.b
	!	Valid codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Transaction Type A>Transaction Journal
	!	.x Transaction Journal>Transaction Type A
	!
	!--
			IC_JOURNAL::TYPE_A = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "13;23", TEMP$, &
				IC_JOURNAL::TYPE_A, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_TRANSTYPE.ID, "VX") = 1%
				THEN
					IC_JOURNAL::TYPE_A = &
						UTL_TRANSTYPE::CODE
				END IF
				GOTO Reenter
			END IF

		CASE 10%
	!++
	! Abstract:FLD010
	!	^*(10) Quantity A\*
	!	.b
	!	.lm +5
	!	The ^*Quantity A\* field refers to the number of items sold, exchanged,
	!	received, etc. in relation to the transaction type.
	!	.lm -5
	!
	! Index:
	!	.x Quantity
	!
	!--
			IC_JOURNAL::QUANTITY_A = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "14;24", TEMP$, &
				IC_JOURNAL::QUANTITY_A, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 11%
	!++
	! Abstract:FLD011
	!	^*(11) Transaction Type B\*
	!	.b
	!	.lm +5
	!	The ^*Transaction Type B\* field enters a valid
	!	transaction type code which has been established in the transaction
	!	type file.
	!	.b
	!	Valid type codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Transaction Type B>Transaction Journal
	!	.x Transaction Journal>Transaction Type B
	!
	!--
			IC_JOURNAL::TYPE_B = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "15;23", TEMP$, &
				IC_JOURNAL::TYPE_B, MFLAG, "'E", &
				MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14
				IF MAIN_WINDOW(UTL_MAIN_TRANSTYPE.ID, "VX") = 1%
				THEN
					IC_JOURNAL::TYPE_B = &
						UTL_TRANSTYPE::CODE
				END IF
				GOTO Reenter

			END SELECT

		CASE 12%
	!++
	! Abstract:FLD012
	!	^*(12) Quantity B\*
	!	.b
	!	.lm +5
	!	The ^*Quantity B\* field refers to the number of items sold, exchanged,
	!	received, etc., in relation to the transaction type.
	!	.lm -5
	!
	! Index:
	!	.x Quantity
	!
	!--
			IC_JOURNAL::QUANTITY_B = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "16;24", &
				TEMP$, IC_JOURNAL::QUANTITY_B, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 13%
	!++
	! Abstract:FLD013
	!	^*(13) Unit Cost Amount\*
	!	.b
	!	.lm +5
	!	The ^*Unit Cost Amount\* field enters the Price per
	!	Unit for the product.
	!	.lm -5
	!
	! Index:
	!
	!--
			IC_JOURNAL::COST = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "17;24", &
				TEMP$, IC_JOURNAL::COST, MFLAG, &
				"###,###.#####", MVALUE)

		CASE 14%
	!++
	! Abstract:FLD014
	!	^*(14) Transaction Account\*
	!	.b
	!	.lm +5
	!	The ^*Transaction Account\* enters the General Ledger
	!	Chart of Account number which this record has reference.
	!	.b
	!	Valid General Ledger Account Numbers may be viewed by pressing ^*List
	!	Choices\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			IC_JOURNAL::EXPACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "18;23", TEMP$, &
				IC_JOURNAL::EXPACCT, MFLAG, "'E", &
				MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%
				THEN
					IC_JOURNAL::EXPACCT = GL_CHART::ACCT
				END IF
				GOTO Reenter

			END SELECT

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20850	CASE OPT_TESTENTRY

		MAINT_GROUP = 0%

		SELECT MLOOP

		CASE 1%
			MAINT_GROUP = FUNC_TESTENTRY(SMG_WINDOW, &
				IC_JOURNAL::PRODUCT, &
				PD_PRODUCT::DESCRIPTION, &
				"IC", 1%, "PROG", &
				"Product Number", PD_MAIN_PRODUCT.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::DESCRIPTION, 2%, 38%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::UOM, 14%, 38%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::UOM, 16%, 38%, , SMG$M_BOLD)


		CASE 2%
			IF MVALUE = "ADD"
			THEN
				IF TEMP_DISPLAY
				THEN
					SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(TEMP_DISPLAY, &
						SCOPE::SMG_PBID)
					TEMP_DISPLAY = 0%
				END IF

				SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY( &
					1%, 15%, TEMP_DISPLAY)
			END IF

			MAINT_GROUP = FUNC_TESTENTRY(SMG_WINDOW, &
				IC_JOURNAL::LOCATION, &
				UTL_LOCATION::LOCNAME, &
				"IC", 2%, "PROG", &
				"Location", UTL_MAIN_LOCATION.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, 3%, 38%, , SMG$M_BOLD)

			IF MVALUE = "ADD"
			THEN
				SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY( &
					TEMP_DISPLAY, SCOPE::SMG_PBID, &
					10%, 16%)

				V% = IC_DSPL_35BALANCE(IC_JOURNAL::PRODUCT, &
					IC_JOURNAL::LOCATION, &
					AVAILABLE, "2;38", 64% + 512%)
			END IF

		CASE 3%
			IF IC_JOURNAL::TOLOCATION = ""
			THEN
				UTL_LOCATION::LOCNAME = ""
			ELSE
				MAINT_GROUP = FUNC_TESTENTRY(SMG_WINDOW, &
					IC_JOURNAL::TOLOCATION, &
					UTL_LOCATION::LOCNAME, &
					"IC", 3%, "PROG", &
					"Location", UTL_MAIN_LOCATION.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, 4%, 38%, , SMG$M_BOLD)

		CASE 4%
			IF MVALUE = "ADD" AND SMG_WINDOW::HFLAG(13%) = 0%
			THEN
				IC_JOURNAL::COST = &
					PC_READ_COST(IC_JOURNAL::PRODUCT, &
					IC_JOURNAL::LOCATION, &
					IC_JOURNAL::TRANS_DATE, "")
			END IF

		CASE 9%
			IF MVALUE = "ADD"
			THEN
				SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(TEMP_DISPLAY, &
					SCOPE::SMG_PBID)
				TEMP_DISPLAY = 0%
			END IF

			MAINT_GROUP = FUNC_TESTENTRY(SMG_WINDOW, &
				IC_JOURNAL::TYPE_A, &
				UTL_TRANSTYPE::DESCRIPTION, &
				"IC", 9%, "PROG", &
				"Transaction Code", UTL_MAIN_TRANSTYPE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_TRANSTYPE::DESCRIPTION, 13%, 38%, , SMG$M_BOLD)

		CASE 11%
			IF IC_JOURNAL::TYPE_B = ""
			THEN
				UTL_TRANSTYPE::DESCRIPTION = ""
			ELSE
				MAINT_GROUP = FUNC_TESTENTRY(SMG_WINDOW, &
					IC_JOURNAL::TYPE_B, &
					UTL_TRANSTYPE::DESCRIPTION, &
					"IC", 11%, "PROG", &
					"Transaction Code", UTL_MAIN_TRANSTYPE.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_TRANSTYPE::DESCRIPTION, 15%, 38%, , SMG$M_BOLD)

		CASE 14%
			IF IC_JOURNAL::EXPACCT = ""
			THEN
				GL_CHART::DESCR = ""
			ELSE
				MAINT_GROUP = FUNC_TESTENTRY(SMG_WINDOW, &
					IC_JOURNAL::EXPACCT, &
					GL_CHART::DESCR, &
					"IC", 14%, "PROG", &
					"Account Number", GL_MAIN_CHART.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 18%, 38%, , SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY
		IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, &
			"Q0" + IC_JOURNAL::PRODUCT) <> 1%
		THEN
			PD_PRODUCT::DESCRIPTION = &
				STRING$(LEN(PD_PRODUCT::DESCRIPTION), A"?"B)
			PD_PRODUCT::UOM = &
				STRING$(LEN(PD_PRODUCT::UOM), A"?"B)
		END IF


		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::DESCRIPTION, 2%, 38%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			UTL_LOCATION::LOCNAME = &
				STRING$(LEN(UTL_LOCATION::LOCNAME), A"?"B) &
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, &
				"Q0" + IC_JOURNAL::LOCATION) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, 3%, 38%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(3%) AND 2%) = 0%
		THEN
			IF IC_JOURNAL::TOLOCATION = ""
			THEN
				UTL_LOCATION::LOCNAME = ""
			ELSE
				UTL_LOCATION::LOCNAME = &
					STRING$(LEN(UTL_LOCATION::LOCNAME), A"?"B) &
					IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, &
					"Q0" + IC_JOURNAL::TOLOCATION) <> 1%
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, 4%, 38%, , SMG$M_BOLD)
		END IF

		!
		! Display Unit of Measure
		!
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			PD_PRODUCT::UOM, 14%, 38%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			PD_PRODUCT::UOM, 16%, 38%, , SMG$M_BOLD)

		IF (SMG_WINDOW::HFLAG(9%) AND 2%) = 0%
		THEN
			UTL_TRANSTYPE::DESCRIPTION = &
				STRING$(LEN(UTL_TRANSTYPE::DESCRIPTION), A"?"B) &
				IF MAIN_WINDOW(UTL_MAIN_TRANSTYPE.ID, &
				"Q0" + IC_JOURNAL::TYPE_A) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_TRANSTYPE::DESCRIPTION, 13%, 38%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(11%) AND 2%) = 0%
		THEN
			IF IC_JOURNAL::TYPE_B = ""
			THEN
				UTL_TRANSTYPE::DESCRIPTION = ""
			ELSE
				UTL_TRANSTYPE::DESCRIPTION = &
					STRING$(LEN(UTL_TRANSTYPE::DESCRIPTION), A"?"B) &
					IF MAIN_WINDOW(UTL_MAIN_TRANSTYPE.ID, &
					"Q0" + IC_JOURNAL::TYPE_B) <> 1%
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_TRANSTYPE::DESCRIPTION, 15%, 38%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(14%) AND 2%) = 0%
		THEN
			IF IC_JOURNAL::EXPACCT = ""
			THEN
				GL_CHART::DESCR = ""
			ELSE
				GL_CHART::DESCR = &
					STRING$(LEN(GL_CHART::DESCR), A"?"B) &
					IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
					"Q0" + IC_JOURNAL::EXPACCT) <> 1%
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 18%, 38%, , SMG$M_BOLD)
		END IF

	!
	! Set IC_JOURNAL_OLD value
	!
20900	CASE OPT_SETOLD
		IC_JOURNAL_OLD = IC_JOURNAL

	!
	! Restore IC_JOURNAL_OLD value
	!
	CASE OPT_RESETOLD
		IC_JOURNAL = IC_JOURNAL_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		IC_JOURNAL2 = IC_JOURNAL

		IF MFLAG = 1%
		THEN
			SELECT MLOOP
			CASE 0%
				FRM$(10%) = "#,###,###.##"
				FRM$(12%) = "#,###,###.##"
			CASE ELSE
				FRM$(MLOOP) = MVALUE
			END SELECT
		END IF

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		IC_JOURNAL = IC_JOURNAL2

		IF IC_JOURNAL::TRANS_DATE = ""
		THEN
			IC_JOURNAL::TRANS_DATE = DATE_TODAY
		END IF

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Product#       Loc   Date    "        + &
				" Referenc CrossRef   "                   + &
				"SubAccoun     Price A         Qty B    " + &
				"     Qty ExpAccount"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "017,023,032,041,052,062,072,075,086,089,100"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = IC_JOURNAL::PRODUCT + " " + &
				IC_JOURNAL::LOCATION + "  " + &
				IC_JOURNAL::TRANS_DATE + " " + &
				IC_JOURNAL::PRIMARY_REF + " " + &
				IC_JOURNAL::CROSS_REF + " " + &
				IC_JOURNAL::SUBACCOUNT + &
				FORMAT$(IC_JOURNAL::COST, "######.##") + " " + &
				IC_JOURNAL::TYPE_A + " " + &
				FORMAT$(IC_JOURNAL::QUANTITY_A, "######.###") + " "  + &
				IC_JOURNAL::TYPE_B + " " + &
				FORMAT$(IC_JOURNAL::QUANTITY_B, "######.###") + " " + &
				IC_JOURNAL::EXPACCT &

			END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE IC_JOURNAL::PRODUCT + &
				IC_JOURNAL::LOCATION, &
				REGARDLESS

		END SELECT

	CASE OPT_AFTEROPT

		SELECT MVALUE

		CASE "Add"

			V% = IC_WRIT_35BALANCE(IC_JOURNAL::PRODUCT, &
				IC_JOURNAL::LOCATION, &
				IC_JOURNAL::TYPE_A, &
				IC_JOURNAL::QUANTITY_A)

			V% = IC_WRIT_35BALANCE(IC_JOURNAL::PRODUCT, &
				IC_JOURNAL::LOCATION, &
				IC_JOURNAL::TYPE_B, &
				IC_JOURNAL::QUANTITY_B) &
				IF IC_JOURNAL::TYPE_B <> ""

			V% = IC_WRIT_35BALANCE(IC_JOURNAL::PRODUCT, &
				IC_JOURNAL::TOLOCATION, &
				IC_JOURNAL::TYPE_A, &
				-IC_JOURNAL::QUANTITY_A) &
				IF IC_JOURNAL::TOLOCATION <> ""

		CASE "Change", "Blank", "Initialize"

			V% = IC_WRIT_35BALANCE(IC_JOURNAL_OLD::PRODUCT, &
				IC_JOURNAL_OLD::LOCATION, &
				IC_JOURNAL_OLD::TYPE_A, &
				-IC_JOURNAL_OLD::QUANTITY_A)

			V% = IC_WRIT_35BALANCE(IC_JOURNAL::PRODUCT, &
				IC_JOURNAL::LOCATION, &
				IC_JOURNAL::TYPE_A, &
				IC_JOURNAL::QUANTITY_A)

			V% = IC_WRIT_35BALANCE(IC_JOURNAL_OLD::PRODUCT, &
				IC_JOURNAL_OLD::LOCATION, &
				IC_JOURNAL_OLD::TYPE_B, &
				-IC_JOURNAL_OLD::QUANTITY_B) &
				IF IC_JOURNAL_OLD::TYPE_B <> ""

			V% = IC_WRIT_35BALANCE(IC_JOURNAL::PRODUCT, &
				IC_JOURNAL::LOCATION, &
				IC_JOURNAL::TYPE_B, &
				IC_JOURNAL::QUANTITY_B) &
				IF IC_JOURNAL::TYPE_B <> ""

			V% = IC_WRIT_35BALANCE(IC_JOURNAL_OLD::PRODUCT, &
				IC_JOURNAL_OLD::TOLOCATION, &
				IC_JOURNAL_OLD::TYPE_A, &
				IC_JOURNAL_OLD::QUANTITY_A) &
				IF IC_JOURNAL_OLD::TOLOCATION <> ""

			V% = IC_WRIT_35BALANCE(IC_JOURNAL::PRODUCT, &
				IC_JOURNAL::TOLOCATION, &
				IC_JOURNAL::TYPE_A, &
				-IC_JOURNAL::QUANTITY_A) &
				IF IC_JOURNAL::TOLOCATION <> ""

		CASE "Erase"

			V% = IC_WRIT_35BALANCE(IC_JOURNAL::PRODUCT, &
				IC_JOURNAL::LOCATION, &
				IC_JOURNAL::TYPE_A, &
				-IC_JOURNAL::QUANTITY_A)

			V% = IC_WRIT_35BALANCE (IC_JOURNAL::PRODUCT, &
				IC_JOURNAL::LOCATION, &
				IC_JOURNAL::TYPE_B, &
				-IC_JOURNAL::QUANTITY_B) &
				IF IC_JOURNAL::TYPE_B <> ""

			V% = IC_WRIT_35BALANCE (IC_JOURNAL::PRODUCT, &
				IC_JOURNAL::TOLOCATION, &
				IC_JOURNAL::TYPE_A, &
				IC_JOURNAL::QUANTITY_A) &
				IF IC_JOURNAL::TOLOCATION <> ""

		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
