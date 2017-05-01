1	%TITLE "Transaction Type Description"
	%SBTTL "UTL_MAIN_TRANSTYPE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG UTL_MAIN_TRANSTYPE(CDD_WINDOW_CDD SMG_WINDOW, &
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
	! ID:0196
	!
	! Abstract:HELP
	!	.p
	! Index:
	!	.x Table>Transaction>Type
	!	.x Transaction>Type>Table
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_MAIN_TRANSTYPE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP UTL_MAIN_TRANSTYPE
	!	$ DELETE UTL_MAIN_TRANSTYPE.OBJ;*
	!
	! Author:
	!
	!	03/01/88 - Frank F. Starman
	!
	! Modification history:
	!
	!	05/30/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	12/18/90 - Frank F. Starman
	!		Transaction type is hard coded. There are only
	!		two fields.
	!
	!	12/19/90 - Val James Allen
	!		Modified to force entry of class code (01,02,03)
	!		depending upon the type (ie: SA=01, SO=02, PO=03 etc.)
	!		into the third field.
	!
	!	09/13/91 - Frank F. Starman
	!		Automatically add records in the file if trans type is
	!		missing.
	!
	!	01/06/94 - Kevin Handy
	!		Modified input of code to allow anything, instead
	!		of trying to use a lookup list that has "+" or
	!		"-" appended to the front for who knows why.
	!
	!	01/06/94 - Kevin Handy
	!		Modified to list all four fields in the
	!		file. Also disables goofy menu, that allowed
	!		changing anything but not adding new records.
	!
	!	01/18/95 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/30/96 - Kevin Handy
	!		Reformat source code
	!
	!	06/06/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	12/04/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSTYPE.HB"
	MAP (UTL_TRANSTYPE)		UTL_TRANSTYPE_CDD	UTL_TRANSTYPE
	MAP (UTL_TRANSTYPE_OLD) UTL_TRANSTYPE_CDD UTL_TRANSTYPE_OLD, &
		UTL_TRANSTYPE2

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_UTL_TRANSTYPE) &
		UTL_TRANSTYPE.CH%, &
		UTL_TRANSTYPE.READONLY%


	COM (TABLE_IC_TRANSTYPE) &
		TRANSTITLE$     = 30%, &
		TRANSTYPE$(20%) = 30%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION UTL_WRIT_TRANSTYPE

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!
	! Initilization
	!
	CASE OPT_INIT

		!*******************************************************
		! Set up information
		!*******************************************************

		!
		! Define SMG_WINDOW
		!
		SMG_WINDOW::DESCR = "Transaction Type Description"
		SMG_WINDOW::NHELP = "UTL_MAIN_TRANSTYPE"
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 4%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Trans_code"
		SMG_WINDOW::KFIELD(0%, 0%) = 1%
		SMG_WINDOW::KFIELD(0%, 1%) = 1%

		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%


		!
		! Category
		!
		TRANSTITLE$ = "Code Description"

		IF INSTR(1%, " QV", MVALUE) <= 1%
		THEN
			!
			! Load in defaults
			!
			CALL READ_DEFAULTS(SMG_WINDOW)
		END IF

700		!
		! Declare channels
		!
		IF UTL_TRANSTYPE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF UTL_TRANSTYPE.READONLY%
			GOTO 790
		END IF

		! Add records in the file
		V% = UTL_WRIT_TRANSTYPE(TRANSTYPE$())

		IF (UTL_TRANSTYPE.CH% <= 0%)
		THEN
			!
			! Open main file (existing) for modification
			!
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSTYPE.CRE"
		END IF

750		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSTYPE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			UTL_MAIN_TRANSTYPE = ERR
			CONTINUE 770
		END WHEN

		UTL_TRANSTYPE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSTYPE.OPN"
		USE
			UTL_MAIN_TRANSTYPE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		UTL_TRANSTYPE.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(UTL_TRANSTYPE.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = UTL_TRANSTYPE.CH%
		WHEN ERROR IN
			RESET #UTL_TRANSTYPE.CH%
			GET #UTL_TRANSTYPE.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display SMG_WINDOW background
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)


		DATA	08,  05, "(01) Trans Type", &
			09,  05, "(02) Description", &
			10,  05, "(03) Class", &
			11,  05, "(04) Sign", &
			0,   0, ""

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
20200	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Transaction Type\*
	!	.p
	!	The ^*Transaction Type\* field provides
	!	a code which will reference the type of transaction defined.
	!	.p
	!	This field will accommodate two (2) alphanumeric characters.
	!	.note
	!	This field cannot be changed.
	!	.end note
	!
	! Index:
	!	.x Transaction Type
	!--

			UTL_TRANSTYPE::CODE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"8;26", TEMP$, UTL_TRANSTYPE::CODE, MFLAG, &
				"'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.p
	!	The ^*Description\* field
	!	enters a description for a transaction type. Examples for inventory
	!	type transactions could include: Purchase Orders, Receivers, Issues, Sales,
	!	Waste, etc.
	!	.p
	!	This field will accommodate up to twenty (20) alphanumeric characters.
	!
	! Index:
	!	.x Transaction Type>Description
	!
	!--


			UTL_TRANSTYPE::DESCRIPTION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "09;26", TEMP$, &
				UTL_TRANSTYPE::DESCRIPTION, MFLAG, "'E", &
				MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Class\*
	!	.lm +5
	!	.b
	!	Possible values are:
	!	.br
	!	^*01\* On Hand
	!	.br
	!	^*02\* On Order
	!	.br
	!	^*03\* Allocated
	!	.lm -5
	!
	! Index:
	!	.x Transaction Type>Class
	!
	!--


			UTL_TRANSTYPE::CLASS = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"10;26", TEMP$, &
				UTL_TRANSTYPE::CLASS, MFLAG, "'E", &
				MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Sign\*
	!	.lm +5
	!	.b
	!	Specifies the sign to use when posting.
	!	.b
	!	Possible values are:
	!	.br
	!	*+ Positive
	!	.br
	!	*- Negative
	!	.lm -5
	!
	! Index:
	!	.x Transaction Type>Sign
	!
	!--


			UTL_TRANSTYPE::TRANSSIGN = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"11;26", TEMP$, &
				UTL_TRANSTYPE::TRANSSIGN, MFLAG, "'E", &
				MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Set UTL_TRANSTYPE_OLD value
	!
20500	CASE OPT_SETOLD
		UTL_TRANSTYPE_OLD = UTL_TRANSTYPE

	!
	! Restore UTL_TRANSTYPE_OLD value
	!
	CASE OPT_RESETOLD
		UTL_TRANSTYPE = UTL_TRANSTYPE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		UTL_TRANSTYPE2 = UTL_TRANSTYPE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		UTL_TRANSTYPE = UTL_TRANSTYPE2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Type Description          "

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "007,030,034"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = UTL_TRANSTYPE::CODE + "   " + &
				UTL_TRANSTYPE::DESCRIPTION + "   " + &
				UTL_TRANSTYPE::CLASS + "   " + &
				UTL_TRANSTYPE::TRANSSIGN

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE UTL_TRANSTYPE::CODE + "", &
				REGARDLESS
		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION

