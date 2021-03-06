1	%TITLE "Product Label Description"
	%SBTTL "PD_MAIN_LABEL"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PD_MAIN_LABEL(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	!	The ^*Product Label Description\* option
	!	accesses the routine where every label associated with all
	!	products is maintained.
	!	.p
	!	If a like product is purchased with more than one label, there
	!	must be a record in the Product Description file for each label.
	!
	! Index:
	!	.x Product Label
	!	.x Product Label
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PD_SOURCE:PD_MAIN_LABEL/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PD_MAIN_LABEL
	!	$ DELETE PD_MAIN_LABEL.OBJ;*
	!
	! Author:
	!
	!	07/22/87 - Frank F. Starman
	!
	! Modification history:
	!
	!	05/23/88 - Aaron Redd
	!		Modified to allow R/O open of file if R/W open fails.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/20/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/28/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	12/05/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PD.OPEN]PD_LABEL.HB"
	MAP	(PD_LABEL)	PD_LABEL_CDD	PD_LABEL
	MAP	(PD_LABEL_OLD)	PD_LABEL_CDD	PD_LABEL_OLD, PD_LABEL2

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PD_LABEL) &
		PD_LABEL.CH%, &
		PD_LABEL.READONLY%

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

		!*************************************************************
		! Set up information
		!*************************************************************

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Product Label Description"
		SMG_WINDOW::NHELP = "PD_MAIN_LABEL"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 2%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Code"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PD_LABEL.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PD_LABEL.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PD.OPEN]PD_LABEL.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PD_MAIN_LABEL = ERR
			CONTINUE 770
		END WHEN

		PD_LABEL.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PD.OPEN]PD_LABEL.OPN"
		USE
			PD_MAIN_LABEL = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PD_LABEL.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PD_LABEL.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PD_LABEL.CH%
		WHEN ERROR IN
			RESET #PD_LABEL.CH%
			GET #PD_LABEL.CH%, REGARDLESS
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


		DATA	8,  20, "(01) Code", &
			10,  20, "(02) Description", &
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
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
	!
20200	CASE OPT_ENTRY

		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Code\*
	!	.p
	!	The ^*Code\* field
	!	enters a code assigned to a specific label or supplier of a
	!	product.
	!	.p
	!	This field will accommodate up to four (4) alphanumeric characters.
	!	.p
	!	Duplicates are not allowed.
	!
	! Index:
	!	.x Code>Label
	!	.x Label>Code
	!
	!--


			PD_LABEL::CODE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"08;38", TEMP$, &
				PD_LABEL::CODE, MFLAG, "'E", &
				MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.p
	!	The ^*Description\* field
	!	enters a description of up to twenty (20) alphanumeric
	!	characters describing a specific label or product source which was
	!	entered in field (1).
	!
	! Index:
	!	.x Product Label>Description
	!
	!--

			PD_LABEL::DESCRIPTION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "10;38", TEMP$, &
				PD_LABEL::DESCRIPTION, MFLAG, "'E", &
				MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

20300	CASE OPT_TESTENTRY
		PD_MAIN_LABEL = 0%

		SELECT MLOOP

		CASE 1%
			IF PD_LABEL::CODE = ""
			THEN
				PD_MAIN_LABEL = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #SMG_WINDOW::CHAN, &
							KEY #0% EQ PD_LABEL::CODE + "", &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					PD_MAIN_LABEL = 2%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Record Already Exists", 0%)
				END IF
			END IF
		END SELECT

	!
	! Set PD_LABEL_OLD value
	!
20500	CASE OPT_SETOLD
		PD_LABEL_OLD = PD_LABEL

	!
	! Restore PD_LABEL_OLD value
	!
	CASE OPT_RESETOLD
		PD_LABEL = PD_LABEL_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PD_LABEL2 = PD_LABEL

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PD_LABEL = PD_LABEL2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Code Description"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "007"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = PD_LABEL::CODE + " " + &
				PD_LABEL::DESCRIPTION
		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE PD_LABEL::CODE + "", &
				REGARDLESS
		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!***************************************************************
	! Trap errors
	!***************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
