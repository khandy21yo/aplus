1	%TITLE "Product Operator Labor Maintenance"
	%SBTTL "BM_MAIN_PRODOPER"
	%IDENT "V3.6a Calico"

	FUNCTION LONG BM_MAIN_PRODOPER(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1992 BY
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
	!	The ^*Structure\* program maintains the Product structure file in
	!	the Bill of Materials System.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS BM_SOURCE:BM_MAIN_PRODOPER/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP BM_MAIN_PRODOPER
	!	$ DELETE BM_MAIN_PRODOPER.OBJ;*
	!
	! Author:
	!
	!	06/23/92 - Dan Perkins
	!		Modified from BM_MAIN_STRUCTURE.
	!
	! Modification history:
	!
	!	06/25/92 - Frank F. Starman
	!		Check for operation.
	!
	!	08/05/92 - Dan Perkins
	!		Rearranged field order.  Use function
	!		PR_READ_OPERATION to compute hours or amount.
	!
	!	08/14/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/12/92 - Dan Perkins
	!		Added CASE 2 to OPT_SUBWIND so that VIEW would
	!		work properly.
	!
	!	11/17/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/02/94 - Kevin Handy
	!		Added field for BM_PRODOPER::THISHOURS
	!
	!	03/18/94 - Kevin Handy
	!		Changed field titles.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/15/95 - Kevin Handy
	!		Reformat source. Lose unecessary externals.
	!
	!	03/15/96 - Kevin Handy
	!		Reformat source code.
	!
	!	10/17/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/09/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	10/20/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.HB"
	MAP (BM_PRODOPER)	BM_PRODOPER_CDD		BM_PRODOPER
	MAP (BM_PRODOPER_OLD)	BM_PRODOPER_CDD		BM_PRODOPER_OLD, &
							BM_PRODOPER2, &
							BM_PRODOPER3

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[PR.OPEN]PR_OPER.HB"
	MAP (PR_OPER)		PR_OPER_CDD		PR_OPER
	DECLARE			PR_OPER_CDD		PR_OPER_READ

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_BM_PRODOPER) &
		BM_PRODOPER.CH%, &
		BM_PRODOPER.READONLY%

	COM (TT_BM_MAIN_PRODOPER) &
		STITLE$      = 30%, &
		SSTAT$(6%)   = 30%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG    FUNCTION PR_READ_OPERATION

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!
	CASE OPT_INIT

	!******************************************************************
	! Set up information
	!******************************************************************

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Product Labor Operations"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::NHELP = "BM_MAIN_PRODOPER"
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 11%
		SMG_WINDOW::HPOS  = 3%
		SMG_WINDOW::VPOS  = 8%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 7%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "Item"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KNAME(1%) = "Operation"
			SMG_WINDOW::KFIELD(1%, 0%) = 2%
			SMG_WINDOW::KFIELD(1%, 1%) = 2%
			SMG_WINDOW::KFIELD(1%, 2%) = 3%

		SMG_WINDOW::HVIEW = 76%
		SMG_WINDOW::VVIEW = 11%
		SMG_WINDOW::VVPOS = 8%
		SMG_WINDOW::VHPOS = 3%

		STITLE$ = "Status   Description"
		SSTAT$(0%) = "3"
		SSTAT$(1%) = "A      Active"
		SSTAT$(2%) = "I      Inactive"
		SSTAT$(3%) = "O      Obsolete"

		COM (BM_MAIN_PRODOPER_FRM) FRM$(7%)

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF BM_PRODOPER.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF BM_PRODOPER.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			BM_MAIN_PRODOPER = ERR
			CONTINUE 770
		END WHEN

		BM_PRODOPER.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.OPN"
		USE
			BM_MAIN_PRODOPER = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		BM_PRODOPER.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(BM_PRODOPER.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = BM_PRODOPER.CH%

		WHEN ERROR IN
			RESET #BM_PRODOPER.CH%
			GET #BM_PRODOPER.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display the background
	!
	! This option is used to display the background information on the
	! screen.  It must first clear any junk on the screen, and then
	! write the background onto it.
	!
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	03,10, "(01) Item #", &
			04,10, "(02) Operation", &
			05,10, "(03) Eff Date", &
			06,10, "(04) Status", &
			07,10, "(05) Total Hours", &
			08,10, "(06) Total Amount", &
			09,10, "(07) Level Hours", &
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
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
	!
20200	CASE OPT_ENTRY

		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 Reenter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Item _#\*
	!	.b
	!	.lm +5
	!	The ^*Item _#\* field indicates the sequence
	!	number for components which go into a product. When adding a
	!	component relative to a product, the value in the Item _# field
	!	will automatically increment. The Item _#'s may be changed.
	!	.b
	!	Duplicate item numbers for the same product are not allowed.
	!	.lm -5
	!
	! Index:
	!
	!--
			IF TEMP$ = "Add"
			THEN
				GOSUB 28500 IF (MFLAG AND 1%) = 0%
			END IF

			ITEMNUM = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "3;28", TEMP$, &
				VAL%(EDIT$(BM_PRODOPER::ITEMNUM, -1%)), &
				MFLAG, "<0>###", MVALUE)

			BM_PRODOPER::ITEMNUM = FORMAT$(ITEMNUM, "<0>###")

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Operation\*
	!	.b
	!	.lm +5
	!	The ^*Operation\* field enters the designation
	!	for an operation in reference to a particular product where
	!	the component is first used.
	!	.lm -5
	!
	! Index:
	!
	!--
			BM_PRODOPER::OPERATION = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"04;28", TEMP$, BM_PRODOPER::OPERATION, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(PR_MAIN_OPER.ID, "V0") = 1%)
				THEN
					BM_PRODOPER::OPERATION = &
						PR_OPER::OPER
				END IF
				GOTO Reenter
			END IF

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F17)
			THEN
				IF (MAIN_WINDOW(PR_MAIN_OPER.ID, "M") = 1%)
				THEN
					BM_PRODOPER::OPERATION = &
						PR_OPER::OPER
				END IF
				GOTO Reenter
			END IF

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Effective Date\*
	!	.b
	!	.lm +5
	!	The ^*Effective Date\* field enters the date of
	!	the operation, and required hours become effective
	!	for the product involved.
	!	.b
	!	The date will default to the system date but may be overridden
	!	by entering the correct date and pressing ^*Return\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			BM_PRODOPER::EFFDATE = &
				ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;28", TEMP$, BM_PRODOPER::EFFDATE, MFLAG, &
				"'E", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Activity Status\*
	!	.b
	!	.lm +5
	!	The ^*Activity Status\* field must contain one of the
	!	following codes.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*A\*	- Active
	!	.te
	!	^*I\*	- Inactive
	!	.te
	!	^*C\*	- Obsolete
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--
			BM_PRODOPER::STAT = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"6;29", TEMP$, BM_PRODOPER::STAT, &
				MFLAG, "!", MVALUE, &
				SSTAT$(), STITLE$, "008"), -1%)

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Hours\*
	!	.b
	!	.lm +5
	!	The ^*Hours\* field enters the number of hours
	!	necessary to construct the product from the lowest component
	!	level.
	!	.b
	!	The field will accommodate a figure as large as 99,999,99.  It can be
	!	changed using the /SET option to increase the size of the field.
	!	.lm -5
	!
	! Index:
	!
	!--
			BM_PRODOPER::HOURS = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"07;28", TEMP$, BM_PRODOPER::HOURS, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Labor Amount\*
	!	.b
	!	.lm +5
	!	The ^*Labor Amount\* field enters the dollar
	!	amount necessary to construct the product from the lowest component
	!	level.
	!	.b
	!	The default length of this field is 99,999,99. It can be
	!	changed using the /SET option to increase the length of the field.
	!	.lm -5
	!
	! Index:
	!
	!--
			IF PR_READ_OPERATION(BM_PRODOPER::OPERATION, &
				"", PR_OPER_READ) = CMC$_NORMAL
			THEN
				LABOR_AMOUNT = BM_PRODOPER::HOURS * &
					PR_OPER_READ::HOUR_RATE
			ELSE
				LABOR_AMOUNT = 0.0
			END IF

			LABOR_AMOUNT = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"08;28", TEMP$, LABOR_AMOUNT, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

			IF LABOR_AMOUNT <> 0.0
			THEN
				BM_PRODOPER::HOURS = &
					LABOR_AMOUNT / PR_OPER_READ::HOUR_RATE &
					IF PR_OPER_READ::HOUR_RATE <> 0.0
			END IF

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(07) Hours on this level\*
	!	.b
	!	.lm +5
	!	This field enters the number of hours
	!	necessary to construct the product at this component
	!	level.
	!	.b
	!	The field will accommodate a figure as large as 99,999,99.  It can be
	!	changed using the /SET option to increase the size of the field.
	!	.lm -5
	!
	! Index:
	!
	!--
			BM_PRODOPER::THISHOURS = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"09;28", TEMP$, BM_PRODOPER::THISHOURS, MFLAG, &
				TRM$(FRM$(MLOOP)), MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		BM_MAIN_STRUCTURE = 0%

		SELECT MLOOP

		CASE 1%
			IF BM_PRODOPER::ITEMNUM = ""
			THEN
				BM_MAIN_STRUCTURE = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #SMG_WINDOW::CHAN, &
							KEY #0% EQ BM_PRODOPER::PRODUCT + &
							BM_PRODOPER::ITEMNUM, &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					BM_MAIN_STRUCTURE = 2%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Record Already Exists", 0%)
				END IF
			END IF

		CASE 2%
			!
			! Is the input defined?
			!
			BM_MAIN_PRODOPER = FUNC_TESTENTRY(SMG_WINDOW, &
				BM_PRODOPER::OPERATION, &
				PR_OPER::OPER, &
				"BM", MLOOP, "OPER", &
				"Operation", PR_MAIN_OPER.ID)

		CASE 6%
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				FORMAT$(BM_PRODOPER::HOURS,TRM$(FRM$(5%))), &
				7%, 26%, , SMG$M_BOLD)

		END SELECT

	!
	! Set BM_PRODOPER_OLD value
	!
20500	CASE OPT_SETOLD
		BM_PRODOPER_OLD = BM_PRODOPER

	!
	! Restore BM_PRODOPER_OLD value
	!
	CASE OPT_RESETOLD
		BM_PRODOPER = BM_PRODOPER_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		BM_PRODOPER2 = BM_PRODOPER

		IF MFLAG = 1%
		THEN
			SELECT MLOOP
			CASE 0%
				FRM$(5%) = "##,###.##"
				FRM$(6%) = "##,###.##"
				FRM$(7%) = "##,###.##"
			CASE ELSE
				FRM$(MLOOP) = MVALUE
			END SELECT
		END IF

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		BM_PRODOPER = BM_PRODOPER2
		BM_PRODOPER::PRODUCT = MVALUE
		BM_PRODOPER::EFFDATE = DATE_TODAY IF BM_PRODOPER::EFFDATE = ""

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Item Operation " + &
				"    Hours EffDate  Status "

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "007,017,027,036"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = BM_PRODOPER::ITEMNUM + " " + &
				BM_PRODOPER::OPERATION + " " + &
				FORMAT$(BM_PRODOPER::HOURS, "###,###.##") + " " + &
				PRNT_DATE(BM_PRODOPER::EFFDATE, 6%) + " " + &
				BM_PRODOPER::STAT
		END SELECT

	!
	! Find
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE BM_PRODOPER::PRODUCT + &
				BM_PRODOPER::ITEMNUM, REGARDLESS

		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE BM_PRODOPER::PRODUCT + &
				BM_PRODOPER::OPERATION + &
				BM_PRODOPER::EFFDATE, REGARDLESS

		END SELECT

	CASE OPT_SUBWIND

		SELECT MLOOP

		!
		! Find first record (if there is any)
		!
		CASE 1%
			!
			! Set init value
			!
			SMG_WINDOW::CURREC = -1%

			!
27110			!
			! Search for first record
			!
			WHEN ERROR IN
				FIND #SMG_WINDOW::CHAN, &
					KEY #0% EQ MVALUE, &
					REGARDLESS

				!
				! Get a record
				!
				GET #SMG_WINDOW::CHAN
			USE
				CONTINUE 28000
			END WHEN

			SMG_WINDOW::CURREC = 0%

		!
		! Find starting record (if there is any)
		!
		CASE 2%
			!
			! Set init value
			!
			SMG_WINDOW::CURREC = -1%

27115			!
			! Search for starting record
			!
			SELECT MFLAG

			CASE 0%
				WHEN ERROR IN
					FIND #SMG_WINDOW::CHAN, &
						KEY #0% GE MVALUE + &
						BM_PRODOPER::ITEMNUM, &
						REGARDLESS
				USE
					CONTINUE 28000
				END WHEN

			CASE 1%
				WHEN ERROR IN
					FIND #SMG_WINDOW::CHAN, KEY #1% GE MVALUE + &
						BM_PRODOPER::OPERATION + &
						BM_PRODOPER::EFFDATE, REGARDLESS
				USE
					CONTINUE 28000
				END WHEN

			END SELECT

			!
			! Get a record
			!
			SMG_WINDOW::CURREC = 0%

		!
		! Check if still right key
		!
		CASE 3%
			SMG_WINDOW::CURREC = -1%

			IF BM_PRODOPER::PRODUCT = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Set the new key
		!
		CASE 6%
			BM_PRODOPER::PRODUCT = MVALUE

		END SELECT

	END SELECT

28000	EXIT FUNCTION

28500	!
	! Find the next item number
	!
	ITEM% = 0%
	WORK_ITEMNUM$ = ""
	WORK_PRODUCT$ = BM_PRODOPER::PRODUCT
	BM_PRODOPER3 = BM_PRODOPER

	WHEN ERROR IN
		FIND #BM_PRODOPER.CH%, &
			KEY #0% EQ BM_PRODOPER::PRODUCT + "", &
			REGARDLESS
	USE
		CONTINUE 28530 IF ERR = 155%
		EXIT HANDLER
	END WHEN

28510	WHILE ITEM% < 9999% - 5%

		ITEM% = ITEM% + 5%

		WHEN ERROR IN
			FIND  #BM_PRODOPER.CH%, KEY #0% GE &
				WORK_PRODUCT$ + FORMAT$(ITEM%, "<0>###"), &
				REGARDLESS

			GET #BM_PRODOPER.CH%, REGARDLESS
		USE
			CONTINUE 28515 IF ERR = 155%
			EXIT HANDLER
		END WHEN

		GOTO 28515 IF WORK_PRODUCT$ <> BM_PRODOPER::PRODUCT
	NEXT

28515	ITEM% = ITEM% - 5%

	FIND #BM_PRODOPER.CH%, &
		KEY #0% GE WORK_PRODUCT$ + FORMAT$(ITEM%, "<0>###"), &
		REGARDLESS

	GET #BM_PRODOPER.CH%, REGARDLESS

28520	IF WORK_PRODUCT$ = BM_PRODOPER::PRODUCT
	THEN
		WORK_ITEMNUM$ = BM_PRODOPER::ITEMNUM

		WHEN ERROR IN
			GET #BM_PRODOPER.CH%, REGARDLESS
		USE
			CONTINUE 28530 IF ERR = 11%
			EXIT HANDLER
		END WHEN

		GOTO 28520
	END IF

28530	BM_PRODOPER = BM_PRODOPER3
	BM_PRODOPER::ITEMNUM = FORMAT$(VAL%(WORK_ITEMNUM$) + 1%, "<0>###")

	RETURN

29000	!****************************************************************
	! Trap errors
	!****************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
