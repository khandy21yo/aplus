1	%TITLE "Maintenance for Account Mask"
	%SBTTL "GL_MAIN_MASK"
	%IDENT "V3.6a Calico"

	FUNCTION LONG GL_MAIN_MASK(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1987,1988 BY
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
	! ID:1035
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This process is used to create a list of account objects. An
	!	account object is the core of an account where the department,
	!	location, or division have been stripped out of the account number.
	!	.B
	!	Example: The account number 5021-0010 represents salaries
	!	(5021) in location (0010). In this case the account object
	!	would be 5021-.
	!	.b
	!	After the account objects have been created, an account
	!	mask can be added. This account mask is used to represent
	!	the other parts of an account number, i.e. the location, division,
	!	etc. The mask would look like ?????-0010. The account object
	!	would be substitute where the question marks are to create
	!	a new account number of 5021-0010.
	!	.b
	!	This process is used to create a new group of accounts as new
	!	divisions, departments, or locations are added to the business.
	!	.lm -5
	!
	! Index:
	!	.x Mask>Maintenance
	!	.x Maintain>Mask
	!	.x Add>Mask
	!	.x Change>Mask
	!	.x Erase>Mask
	!	.x Mask>Add
	!	.x Mask>Change
	!	.x Mask>Erase
	!	.x Object>Maintenance
	!	.x Maintain>Object
	!	.x Add>Object
	!	.x Change>Object
	!	.x Erase>Object
	!	.x Object>Add
	!	.x Object>Change
	!	.x Object>Erase
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_MAIN_MASK/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP GL_MAIN_MASK
	!	$ DELETE GL_MAIN_MASK.OBJ;*
	!
	! Author:
	!
	!	06/24/88 - Aaron Redd
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/23/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	03/09/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	11/10/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[GL.OPEN]GL_MASK.HB"
	MAP	(GL_MASK)	GL_MASK_CDD	GL_MASK
	MAP	(GL_MASK2)	GL_MASK_CDD	GL_MASK_OLD,	GL_MASK2

	%INCLUDE "SOURCE:[GL.OPEN]GL_OBJECT.HB"
	MAP	(GL_OBJECT)	GL_OBJECT_CDD	GL_OBJECT

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD	GL_CHART

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_GL_MASK) &
		GL_MASK.CH%, &
		GL_MASK.READONLY%
	COM (CH_GL_OBJECT) &
		GL_OBJECT.CH%, &
		GL_OBJECT.READONLY%
	COM (CH_GL_CHART) &
		GL_CHART.CH%, &
		GL_CHART.READONLY%

	!
	! Declare some variables
	!
	DECLARE LONG XPOS, YPOS

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 29000

	SELECT MOPTION

	!**************************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!**************************************************************
	CASE OPT_INIT

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Account Mask Maintenance"
		SMG_WINDOW::NHELP = "GL_MAIN_MASK"
		SMG_WINDOW::CHAN  = GL_MASK.CH%
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 76%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 5%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Mask"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF GL_MASK.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF GL_MASK.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_MASK.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			GL_MAIN_MASK = ERR
			CONTINUE 770
		END WHEN

		GL_MASK.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_MASK.OPN"
		USE
			GL_MAIN_MASK = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		GL_MASK.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(GL_MASK.CH%)

		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = GL_MASK.CH%
		WHEN ERROR IN
			RESET #GL_MASK.CH%
			GET #GL_MASK.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

20100	!**************************************************************
	! Display the background
	!
	! This option is used to display the background information
	! on the screen.  It must first clear any junk on the screen,
	! and then write the background onto it.
	!**************************************************************
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	5, 10, "(01) Account Mask", &
			7, 10, "(02) Description Mask", &
			9, 10, "(03) Cash Flow Mask", &
			11, 10, "(04) Work Capital Mask", &
			13, 10, "(05) Financial Type Mask", &
			0,  0, ""

		RESTORE

		READ XPOS, YPOS, XSTR$
		I% = 0%
		WHILE (XPOS <> 0%)
			I% = I% + 1%
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				XSTR$, XPOS, YPOS) &
				IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%
			READ XPOS, YPOS, XSTR$
		NEXT

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

20150	!**************************************************************
	! Add to the options menu
	!**************************************************************
	CASE OPT_OPTLIST
		MVALUE = MVALUE + " cOpy obJect"

	!**************************************************************
	! Window option
	!**************************************************************
	CASE OPT_MOREMENU

		SELECT MVALUE
		!
		! Copy
		!
		CASE "cOpy"
			GOSUB CopyOption

			!
			! Repaint screen
			!
			GL_MAIN_MASK = 8%

		!
		! Line option
		!
		CASE "obJect"
			!
			! Add into object file
			!
			GL_MAIN_MASK = MAIN_WINDOW(GL_MAIN_OBJECT.ID, "")

		END SELECT

20200	!**************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display
	! data, set defaults, and return the data back according to
	! MFLAG.
	!**************************************************************
	CASE OPT_ENTRY
		TEMP$ = TRM$(SCOPE::PRG_ITEM)

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	.x Account Mask>Mask
	!	^*(01) Account Mask\*
	!	.b
	!	.lm +5
	!	The ^*Account Mask\* is the overlay mask for a chart
	!	of account number. The mask must be the same length
	!	as the chart of account number. The mask will have question
	!	marks where the object number will be overlaid to create
	!	a new account number.
	!	.B
	!	Example: A mask of ?????0001 combined with an object
	!	of 1010- would create an account number of 1010-0001.
	!	.b
	!	Eighteen spaces are available for the entry.
	!	.b
	!	This field cannot be set to null. A value must be entered
	!	when a record is being added.
	!	.lm -5
	!
	! Index:
	!	.x Mask>Account Mask
	!
	!--

			GL_MASK::ACCT_MASK = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"5;38", TEMP$, GL_MASK::ACCT_MASK, &
				MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	.x Description
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field is used to enter an account
	!	description which will be appended to the end of the
	!	object description.
	!	.B
	!	Example: If the mask description were SAMPLE STORE and
	!	the object description were SALARIES, then the description
	!	in the chart of accounts would be SALARIES - SAMPLE STORE.
	!	.b
	!	Forty spaces are available for the entry.
	!	.b
	!	This field may be left blank.
	!	.lm -5
	!
	! Index:
	!	.x Mask>Description
	!
	!--

			GL_MASK::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"7;38", TEMP$, GL_MASK::DESCR, MFLAG, &
				"'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	.x Cash Flow>Mask
	!	^*(03) Cash Flow Mask\*
	!	.b
	!	.lm +5
	!	The ^*Cash Flow Mask\* field enters an
	!	overlay mask for a chart of account cash flow code. The mask
	!	must be the same length as the chart of account cash flow code.
	!	The mask will have question marks where the object cash flow will
	!	be overlaid to create a new cash flow code.
	!	.B
	!	Example: A mask of ???1 combined with an object of CH2 would
	!	create a code of CH21.  Often, the Cash Flow Mask would be
	!	"????", in order to duplicate the code for an object account
	!	whenever created.
	!	.b
	!	Four spaces are available for the entry.
	!	.b
	!	This field may be left blank.
	!	.lm -5
	!
	! Index:
	!	.x Mask>Cash Flow
	!
	!--

			GL_MASK::CASH_FLOW = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"9;38", TEMP$, GL_MASK::CASH_FLOW, &
				MFLAG, "'E", MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	.x Working Capital>Mask
	!	^*(04) Working Capital Mask\*
	!	.b
	!	.lm +5
	!	The ^*Working Capital Mask\* field enters an
	!	overlay mask for a chart of account working capital code. The mask
	!	must be the same length as the chart of account working capital code.
	!	The mask will have question marks where the object Working Capital
	!	will be overlaid to create a new working capital code.
	!	.B
	!	Example: A mask of ???1 combined with an object of WRK would
	!	create a code of WRK1. Often, the Working Capital Mask would
	!	be "????", which would result in the code being duplicated
	!	for an object account whenever created.
	!	.b
	!	Four spaces are available for the entry.
	!	.b
	!	This field may be left blank.
	!	.lm -5
	!
	! Index:
	!	.x Mask>Working Capital
	!
	!--

			GL_MASK::WORK_CAPT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"11;38", TEMP$, GL_MASK::WORK_CAPT, &
				MFLAG, "'E", MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!	.x Financial Type>Mask
	!	^*(05) Financial Type Mask\*
	!	.b
	!	.lm +5
	!	The ^*Financial Type Mask\* field enters an
	!	overlay mask for a chart of account financial type. The mask must
	!	be the same length as the chart of account financial type. The mask
	!	will have question marks where the object financial type will be
	!	overlaid to create a new financial type code.
	!	.B
	!	Example: A mask of ?????23411 combined with an object of AC010
	!	would create a type code of AC01023411.
	!	.b
	!	Ten spaces are available for this entry.
	!	.b
	!	This field may be left blank.
	!	.lm -5
	!
	! Index:
	!	.x Mask>Financial Type
	!
	!--

			GL_MASK::FIN_TYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"13;38", TEMP$, GL_MASK::FIN_TYPE, &
				MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

20300	!**************************************************************
	! Test values
	!**************************************************************
	CASE OPT_TESTENTRY
		GL_MAIN_MASK = 0%

		SELECT MLOOP

		CASE 1%
			IF GL_MASK::ACCT_MASK = ""
			THEN
				!
				! Don't allow blank account masks
				!
				GL_MAIN_MASK = 1%
			ELSE
				!
				! Don't allow duplicates
				!
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #GL_MASK.CH%, &
							KEY #0% EQ GL_MASK::ACCT_MASK + "", &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					GL_MAIN_MASK = 2%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Record Already Exists", 1%)
				END IF
			END IF

		END SELECT

20500	!**************************************************************
	! Set GL_MASK_OLD value
	!**************************************************************
	CASE OPT_SETOLD
		GL_MASK_OLD = GL_MASK

	!**************************************************************
	! Restore GL_MASK_OLD value
	!**************************************************************
	CASE OPT_RESETOLD
		GL_MASK = GL_MASK_OLD

	!**************************************************************
	! Set default value
	!**************************************************************
	CASE OPT_SETDEFAULT
		GL_MASK2 = GL_MASK

	!**************************************************************
	! Restore default value
	!**************************************************************
	CASE OPT_RESETDEFAULT
		GL_MASK = GL_MASK2

	!***********************************************************
	! View header
	!***********************************************************
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Account            Description"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "021"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = GL_MASK::ACCT_MASK + " " + &
				GL_MASK::DESCR

		END SELECT

	!**************************************************************
	! Find
	!**************************************************************
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			!
			! FIND according to Primary Key
			!	(Account Mask)
			!
			FIND #GL_MASK.CH%, KEY #0% GE GL_MASK::ACCT_MASK + "", &
				REGARDLESS
		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

	%PAGE

 CopyOption:
28000	!**************************************************************
	! Subroutine to handle copy option
	!**************************************************************
	!
	! Check to see if there is a current mask record
	!
	IF EDIT$(GL_MASK::ACCT_MASK, -1%) = ""
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"A mask record must exist to use copy.  Aborting. . .", 0%)
		GOTO 28090
	END IF

	!
	! Ask if this is what you really want to do
	!
	INP$ = ENTR_3YESNO(SCOPE, SMG_WINDOW::WNUMBER, "", &
		"Confirm copying to chart of accounts - then press <Do>", &
		"N", 0%, "!", "")

	IF EDIT$(INP$, -1%) <> "Y"
	THEN
		GOTO 28090
	END IF

	!
	! Reset object file
	!
	WHEN ERROR IN
		RESET #GL_OBJECT.CH%
	USE
		CALL ENTR_3MESSAGE(SCOPE, &
			"Object file is empty.  Aborting. . . ", 0%)
		CONTINUE 28090
	END WHEN

28010	!
	! Get object record
	!
	WHEN ERROR IN
		GET #GL_OBJECT.CH%
	USE
		CONTINUE 28090
	END WHEN

28020	!
	! Create chart of account record
	!

	!
	! Use the account mask to create a new account number
	!
	TEMP_ACCT$ = ""
	OBJ_TEST% = 0%
	OBJ_LOOP% = LEN(EDIT$(GL_OBJECT::OBJ_MASK, -1%))

	FOR LOOP% = 1% TO LEN(GL_MASK::ACCT_MASK)
		TEST$ = MID(GL_MASK::ACCT_MASK, LOOP%, 1%)
		!
		! If mask then substitute with Object account mask
		!
		IF TEST$ = "?"
		THEN
			OBJ_TEST% = OBJ_TEST% + 1%
			TEST$ = MID(GL_OBJECT::OBJ_MASK, OBJ_TEST%, 1%)
		END IF
		!
		! Now add to temp account
		!
		TEMP_ACCT$ = TEMP_ACCT$ + TEST$

	NEXT LOOP%

	GL_CHART::ACCT		= TEMP_ACCT$

	!
	! Create description
	!
	IF EDIT$(GL_MASK::DESCR, -1%) <> ""
	THEN
		GL_CHART::DESCR		= EDIT$(GL_OBJECT::DESCR, 140%) + &
			" - " + GL_MASK::DESCR
	ELSE
		GL_CHART::DESCR		= GL_OBJECT::DESCR
	END IF

	GL_CHART::ACCTYPE	= GL_OBJECT::ACCT_TYPE
	GL_CHART::SUMMARY	= GL_OBJECT::SUMM_FLAG

	IF EDIT$(GL_MASK::CASH_FLOW, -1%) = ""
	THEN
		GL_CHART::FLOW = GL_OBJECT::CASH_FLOW
	ELSE
		!
		! Use the cash flow mask to create a new cash flow code
		!
		TEMP_CASH$ = ""
		OBJ_TEST% = 0%
		OBJ_LOOP% = LEN(EDIT$(GL_OBJECT::CASH_FLOW, -1%))

		FOR LOOP% = 1% TO LEN(GL_MASK::CASH_FLOW)
			TEST$ = MID(GL_MASK::CASH_FLOW, LOOP%, 1%)
			!
			! If mask then substitute with Object mask
			!
			IF TEST$ = "?"
			THEN
				OBJ_TEST% = OBJ_TEST% + 1%
				TEST$ = MID(GL_OBJECT::CASH_FLOW, OBJ_TEST%, 1%)
			END IF
			!
			! Now add to temp variable
			!
			TEMP_CASH$ = TEMP_CASH$ + TEST$

		NEXT LOOP%

		GL_CHART::FLOW		= TEMP_CASH$
	END IF

	IF EDIT$(GL_MASK::WORK_CAPT, -1%) = ""
	THEN
		GL_CHART::WORK = GL_OBJECT::WORK_CAPT
	ELSE
		!
		! Use the work capital flow mask to create a new cash flow code
		!
		TEMP_WORK$ = ""
		OBJ_TEST% = 0%
		OBJ_LOOP% = LEN(EDIT$(GL_OBJECT::WORK_CAPT, -1%))

		FOR LOOP% = 1% TO LEN(GL_MASK::WORK_CAPT)
			TEST$ = MID(GL_MASK::WORK_CAPT, LOOP%, 1%)
			!
			! If mask then substitute with Object mask
			!
			IF TEST$ = "?"
			THEN
				OBJ_TEST% = OBJ_TEST% + 1%
				TEST$ = MID(GL_OBJECT::WORK_CAPT, OBJ_TEST%, 1%)
			END IF
			!
			! Now add to temp variable
			!
			TEMP_WORK$ = TEMP_WORK$ + TEST$

		NEXT LOOP%

		GL_CHART::WORK = TEMP_WORK$
	END IF

	IF EDIT$(GL_MASK::FIN_TYPE, -1%) = ""
	THEN
		GL_CHART::FINTYPE = GL_OBJECT::FIN_TYPE
	ELSE
		!
		! Use the work capital flow mask to create a new cash flow code
		!
		TEMP_FIN$ = ""
		OBJ_TEST% = 0%
		OBJ_LOOP% = LEN(EDIT$(GL_OBJECT::FIN_TYPE, -1%))

		FOR LOOP% = 1% TO LEN(GL_MASK::FIN_TYPE)
			TEST$ = MID(GL_MASK::FIN_TYPE, LOOP%, 1%)
			!
			! If mask then substitute with Object mask
			!
			IF TEST$ = "?"
			THEN
				OBJ_TEST% = OBJ_TEST% + 1%
				TEST$ = MID(GL_OBJECT::FIN_TYPE, OBJ_TEST%, 1%)
			END IF
			!
			! Now add to temp variable
			!
			TEMP_FIN$ = TEMP_FIN$ + TEST$

		NEXT LOOP%

		GL_CHART::FINTYPE	= TEMP_FIN$
	END IF

	GL_CHART::DOLLAR(I%) = 0.0 FOR I% = 0% TO 20%
	GL_CHART::UNIT(I%) = 0.0 FOR I% = 0% TO 20%
	GL_CHART::HOUR(I%) = 0.0 FOR I% = 0% TO 20%
	GL_CHART::CPERIOD = 0%
	GL_CHART::RUNDOL = 0.0
	GL_CHART::RUNHOUR = 0.0
	GL_CHART::RUNUNIT = 0.0
	GL_CHART::CURDOL = 0.0
	GL_CHART::CURHOUR = 0.0
	GL_CHART::CURUNIT = 0.0
	GL_CHART::BATCH = ""

	WHEN ERROR IN
		PUT #GL_CHART.CH%
	USE
		CONTINUE 28010 IF ERR = 134%
		EXIT HANDLER
	END WHEN

	CALL ENTR_3MESSAGE(SCOPE, "Copying " + GL_CHART::ACCT, 1%)

	!
	! Go get the next record
	!
	GOTO 28010

28090	!
	! Finished copy process
	!
	CALL ENTR_3MESSAGE(SCOPE, "", 1%)

	RETURN

	%PAGE

29000	!**************************************************************
	! Trap errors
	!**************************************************************

	ON ERROR GO BACK

32767	!**************************************************************
	! End of GL_MAIN_MASK function
	!**************************************************************
	END FUNCTION
	!+-+-+
	!++
	! Abstract:OBJECT
	!	^*OBJECT\*
	!	.b
	!	.lm +5
	!	^*Object\* allows for entry, maintenance, and copying of the mask and account
	!	object files. This process is often used to create a list of account objects
	!	to which account masks may be added. New groups of accounts may be created
	!	through this process as new divisions are added to the Chart of Accounts.
	!	.lm -5
	!
	! Index:
	!	.x Object
	!
	!--
