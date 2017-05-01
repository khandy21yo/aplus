1	%TITLE "Accounts Payable 1099 Table Maintenance"
	%SBTTL "AP_MAIN_1099_TABLE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AP_MAIN_1099_TABLE(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	Each record in the file (table) contains the following fields:
	!	.table 3,25
	!	.te
	!	Code
	!	.tE
	!	Description
	!	.tE
	!	Base Amount
	!	.tE
	!	Form Number
	!	.tE
	!	Form Location
	!	.End table
	!	.lm -5
	!
	! Index:
	!	.x 1099 Table
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_MAIN_1099_TABLE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AP_MAIN_1099_TABLE
	!	$ DELETE AP_MAIN_1099_TABLE.OBJ;*
	!
	! Author:
	!
	!	08/03/87 - B. Craig Larsen
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
	!	08/22/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/09/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	11/09/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!***************************************************************
	! Set up compiling options
	!***************************************************************
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!***************************************************************
	! Include files
	!***************************************************************
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	!***************************************************************
	! CDD inclusions and MAPs
	!***************************************************************
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AP.OPEN]AP_1099_TABLE.HB"
	MAP (AP_1099_TABLE) AP_1099_TABLE_CDD AP_1099_TABLE
	MAP (AP_1099_TABLE_1) AP_1099_TABLE_CDD AP_1099_TABLE_OLD, &
		AP_1099_TABLE2

	!***************************************************************
	! Common areas
	!
	!	These areas store information that is re-used between
	! calls to these functions.
	!***************************************************************
	COM (CH_AP_1099_TABLE) &
		AP_1099_TABLE.CH%, &
		AP_1099_TABLE.READONLY%

	!***************************************************************
	! Declare some arrays
	!***************************************************************
	EC$(0%) = "9"
	EC$(1%) = "1  1099-A"
	EC$(2%) = "2  1099-B"
	EC$(3%) = "3  1099-DIV"
	EC$(4%) = "4  1099-G"
	EC$(5%) = "5  1099-INT"
	EC$(6%) = "6  1099-MISC"
	EC$(7%) = "7  1099-OID"
	EC$(8%) = "8  1099-PATR"
	EC$(9%) = "9  1099-R"

	!***************************************************************
	! Set up error trapping
	!***************************************************************
	ON ERROR GOTO 29000

	%PAGE

	SELECT MOPTION

	!*******************************************************************
	! Initialization
	!
	!	This option is used to initialize the window structure,
	!	set up the default values for add, and open all files
	!	necessary that have not already been opened.
	!*******************************************************************

	CASE OPT_INIT
		!***************************************************************
		!Define window
		!***************************************************************
		SMG_WINDOW::DESCR = "Accounts Payable 1099 Table Maintenance"
		SMG_WINDOW::NHELP = "AP_MAIN_1099_TABLE"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 5%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Code"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

			!*****************************************************
			!Load in defaults
			!*****************************************************
			CALL READ_DEFAULTS(SMG_WINDOW)

700		IF AP_1099_TABLE.CH% > 0%
		THEN
			!*****************************************************
			! Already open, set flag to read-only if
			! was that way from last time.
			!*****************************************************
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AP_1099_TABLE.READONLY%
			GOTO 790
		END IF

		!***************************************************************
		!Open main file (existing) for modification
		!***************************************************************
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_1099_TABLE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			AP_MAIN_1099_TABLE = ERR
			CONTINUE 770
		END WHEN

		AP_1099_TABLE.READONLY% = 0%
		GOTO 790

760		!***************************************************************
		!If unable to open for modify, try to open
		!with read access only.
		!***************************************************************

		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_1099_TABLE.OPN"
		USE
			AP_MAIN_1099_TABLE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		AP_1099_TABLE.READONLY% = -1%

		GOTO 790

770		!***************************************************************
		!File not able to open, so reset channel
		!***************************************************************
		CALL ASSG_FREECHANNEL(AP_1099_TABLE.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = AP_1099_TABLE.CH%

		WHEN ERROR IN
			RESET #AP_1099_TABLE.CH%
			GET #AP_1099_TABLE.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	%PAGE

20100	!*********************************************************************
	! Display the background
	!
	! This option is used to display the background information
	! on the screen.  It must first clear any junk on the screen,
	! and then write the background onto it.
	!*********************************************************************
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	05, 04, "(01) Code", &
			06, 04, "(02) Description", &
			08, 04, "(03) Base Amount", &
			09, 04, "(04) Form Number", &
			10, 04, "(05) Form Location", &
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

20200	!*********************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display
	! data, set defaults, and return the data back according to
	! MFLAG.
	!*********************************************************************
	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	.x Code>Table Maintenance
	!	^*(01) Code\*
	!	.b
	!	.lm +5
	!	The ^*Code\* field
	!	establishs a two (2) character alphanumeric code in the 1099 Table
	!	which has reference to a particular type of payment; i.e., rent,
	!	interest, dividends, miscellaneous, etc.
	!	.lm -5
	!
	! Index:
	!
	!--

			AP_1099_TABLE::CODE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "05;24", TEMP$, &
				AP_1099_TABLE::CODE, MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field is a twenty (20)
	!	character alphanumeric field which enters
	!	information describing a particular type of payment, i.e. rent,
	!	interest, dividends, miscellaneous, etc.
	!	.lm -5
	!
	! Index:
	!	.x Description>Table Maintenance
	!	.x Table Maintenance>Description
	!
	!--

			AP_1099_TABLE::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;24", TEMP$, &
				AP_1099_TABLE::DESCR, MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Base Amounts\*
	!	.b
	!	.lm +5
	!	The Internal Revenue Service requires that Form 1099 be
	!	furnished to individuals for various types of payments made
	!	if the payments in a given calendar year equal or exceed
	!	a pre-established dollar volume or "base amount". Established
	!	base amounts vary depending upon the type of payments.
	!	.b
	!	The ^*Base Amount\* field in the 1099 Table
	!	records the appropriate base amount for each 1099 payment type.
	!	.b
	!	Fourteen (14) spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Base Amount>Table Maintenance
	!	.x Table Maintenance>Base Amount
	!
	!--

			AP_1099_TABLE::BASEAMT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "8;24", TEMP$, &
				AP_1099_TABLE::BASEAMT * 1.0, MFLAG, &
				"###,###,###.##", MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Form Number\*
	!	.b
	!	.lm +5
	!	The various types of payments subject to reporting on Forms
	!	1099 are required to be reported on specific forms.
	!	.b
	!	The ^*Form Number\* field
	!	specifies which Form the 1099 program uses for the related type
	!	of payment. To make the form number selection, with the cursor
	!	on field (04), press the ^*List Choices\* key. Use the
	!	^*up arrow\* or the ^*down arrow\* key to move the pointer to the
	!	form number to be selected, then press ^*Select\*.
	!	.lm -5
	!
	! Index:
	!	.x Form Number>Table Maintenance
	!	.x Table Maintenance>Form Number
	!
	!--

 Loop4:			AP_1099_TABLE::FRMNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "9;24", TEMP$, &
				AP_1099_TABLE::FRMNUM, MFLAG, "'", MVALUE)

			SELECT SCOPE::SCOPE_EXIT
			CASE SMG$K_TRM_F14
				X% = ENTR_3CHOICE(SCOPE, "", "", EC$(), "", 0%, &
					"Form Numbers", "", 0%)
				AP_1099_TABLE::FRMNUM = NUM1$(X%) &
					IF X% > 0%
				GOTO Loop4
			END SELECT

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Form Location\*
	!	.b
	!	.lm +5
	!	The ^*Form Location\* field
	!	determines the box number on the Form 1099 where the payment
	!	type is to be printed.
	!	.lm -5
	!
	! Index:
	!	.x Form 1099>Form Location
	!
	!--

			AP_1099_TABLE::FRMLOC = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "10;24", TEMP$, &
				AP_1099_TABLE::FRMLOC, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

20300	!*********************************************************************
	!Test values
	!*********************************************************************
	CASE OPT_TESTENTRY

		AP_MAIN_1099_TABLE = 0%

		SELECT MLOOP

		CASE 1%
			IF AP_1099_TABLE::CODE = ""
			THEN
				AP_MAIN_1099_TABLE = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					GET #AP_1099_TABLE.CH%, &
						KEY #0% EQ AP_1099_TABLE::CODE + "", &
						REGARDLESS

					AP_MAIN_1099_TABLE = 2%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Record Already Exists", 1%)
				END IF
			END IF

		CASE 4%
			GOOD% = 0%
			GOOD% = -1% IF AP_1099_TABLE::FRMNUM = LEFT(EC$(I%), 1%) &
				FOR I% = 1% TO VAL%(EC$(0%))

			IF GOOD% = 0%
			THEN
				X% = ENTR_3CHOICE(SCOPE, "", "", EC$(), "", 0%, &
					"Form Numbers", "", 0%)
				IF X% > 0%
				THEN
					AP_1099_TABLE::FRMNUM = &
						ENTR_3STRING(SCOPE, &
						SMG_WINDOW::WNUMBER, "9;24", TEMP$, &
						AP_1099_TABLE::FRMNUM, 33%, "'", &
						NUM1$(X%))
				END IF
			END IF
		CASE 5%
			IF INSTR(1%," 01 02 03 04 05 06 07 08 09 10 " + &
				"11 12 13 14 ", " " + &
				AP_1099_TABLE::FRMLOC + " " &
				) = 0%
			THEN
				CALL ENTR_3MESSAGE(SCOPE, &
					"Form location must be between 01 and 14", 1%)
				AP_MAIN_1099_TABLE = 1%
			END IF

		END SELECT


	CASE OPT_DISPLAY

	!***************************************************************
	!Set AP_1099_TABLE_OLD value
	!***************************************************************
20500	CASE OPT_SETOLD

		AP_1099_TABLE_OLD = AP_1099_TABLE

	!***************************************************************
	!Restore AP_1099_TABLE_OLD value
	!***************************************************************
	CASE OPT_RESETOLD

		AP_1099_TABLE = AP_1099_TABLE_OLD

	!***************************************************************
	!Set default value
	!***************************************************************
	CASE OPT_SETDEFAULT

		AP_1099_TABLE2 = AP_1099_TABLE

	!***************************************************************
	!Restore default value
	!***************************************************************
	CASE OPT_RESETDEFAULT

		AP_1099_TABLE = AP_1099_TABLE2

	!***************************************************************
	!View header
	!***************************************************************
	CASE OPT_VIEW

		SELECT MLOOP

		!***************************************************************
		!Title (One line only)
		!***************************************************************
		CASE 1%
			MVALUE = "  Code   Description            " + &
				"   Base Amount   Form #   Form Loc"

		!***************************************************************
		!Positions of lines
		!***************************************************************
		CASE 2%
			MVALUE = "008,031,048,057"

		!***************************************************************
		!Convert current record into text
		!***************************************************************
		CASE 3%
			MVALUE = " " + AP_1099_TABLE::CODE + "    " + &
				AP_1099_TABLE::DESCR + "   " + &
				FORMAT$(AP_1099_TABLE::BASEAMT, &
					"###,###,###.##") + "     " + &
				AP_1099_TABLE::FRMNUM + "         " + &
				AP_1099_TABLE::FRMLOC

		END SELECT

	!***************************************************************
	!Find
	!***************************************************************
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #AP_1099_TABLE.CH%, &
				KEY #0% GE AP_1099_TABLE::CODE + "", &
				REGARDLESS
		END SELECT
	END SELECT

 ExitFunction:
	EXIT FUNCTION

29000	!***************************************************************
	!Trap Errors
	!***************************************************************

	RESUME ExitFunction

32767	END FUNCTION
