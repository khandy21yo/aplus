1	%TITLE "County Definition Table"
	%SBTTL "UTL_MAIN_COUNTY"
	%IDENT "V3.6a Calico"

	FUNCTION LONG UTL_MAIN_COUNTY(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1989 BY
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
	! ID:0130
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*County Definition Table\* function contains all of the routines necessary
	!	to maintain the Utility System's County (Township, etc.)
	!	Definition Table file.
	!	.lm -5
	!
	! Index:
	!	.x County Definition Table
	!	.x Table>County Definition
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_MAIN_COUNTY/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN UTL_MAIN_COUNTY
	!	$ DELETE UTL_MAIN_COUNTY.OBJ;*
	!
	! Author:
	!
	!	02/03/88 - Aaron Redd
	!
	! Modification history:
	!
	!	05/24/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	08/01/89 - Aaron Redd
	!		Modified to more correctly follow the format of
	!		the usual MAIN function.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/28/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND problem
	!
	!	11/29/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Included files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	!
	! CDD inclusions and related memory MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTY.HB"
	MAP	(UTL_COUNTY)	UTL_COUNTY_CDD	UTL_COUNTY
	MAP	(UTL_COUNTY_2)	UTL_COUNTY_CDD	UTL_COUNTY_OLD, UTL_COUNTY2

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_UTL_COUNTY) &
		UTL_COUNTY.CH%, &
		UTL_COUNTY.READONLY%

	%PAGE

	!
	! Set up error trapping
	!
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
		SMG_WINDOW::DESCR = "County/Township/etc. Definition"
		SMG_WINDOW::CURREC= -2%
		SMG_WINDOW::NHELP = "UTL_MAIN_COUNTY"
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 6%
		SMG_WINDOW::HPOS  = 4%
		SMG_WINDOW::VPOS  = 12%
		SMG_WINDOW::HVIEW = 76%
		SMG_WINDOW::VVIEW = 6%
		SMG_WINDOW::VHPOS = 4%
		SMG_WINDOW::VVPOS = 12%
		SMG_WINDOW::NITEMS= 2%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "county_Code"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KNAME(1%) = "county_Descr"
			SMG_WINDOW::KFIELD(1%, 0%) = 1%
			SMG_WINDOW::KFIELD(1%, 1%) = 2%

		!
		! Load in defaults for county description
		!
		CALL READ_DEFAULTS(SMG_WINDOW) IF &
			(INSTR(1%, " QV", MVALUE) <= 1%)

700		!
		! Declare channels
		!
		IF UTL_COUNTY.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF UTL_COUNTY.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTY.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			UTL_MAIN_COUNTY = ERR
			CONTINUE 770
		END WHEN

		UTL_COUNTY.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTY.OPN"
		USE
			UTL_MAIN_COUNTY = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		UTL_COUNTY.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(UTL_COUNTY.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = UTL_COUNTY.CH%
		WHEN ERROR IN
			RESET #UTL_COUNTY.CH%
			GET #UTL_COUNTY.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

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

		DATA	3,  5, "(01) County Code", &
			4,  5, "(02) Description", &
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

	%PAGE

20200	!******************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display
	! data, set defaults, and return the data back according to
	! MFLAG.
	!******************************************************************
	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) County\*
	!	.b
	!	.lm +5
	!	The ^*County\* field
	!	enters a user defined code
	!	representing a specific county.
	!	.b
	!	This field will accommodate two (2) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x County>Code
	!	.x Code>County
	!
	!--

			UTL_COUNTY::COUNTY = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"3;25", TEMP$, UTL_COUNTY::COUNTY, &
				MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field
	!	enters the name of a specific county represented by the code in
	!	field (01).
	!	.b
	!	This field will accommodate up to forty (40) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x County>Description
	!	.x Description>County
	!
	!--

			UTL_COUNTY::DESCR = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;25", TEMP$, UTL_COUNTY::DESCR, &
				MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		UTL_MAIN_COUNTY = 0%

	!
	! Set UTL_COUNTY_OLD value
	!
20500	CASE OPT_SETOLD
		UTL_COUNTY_OLD = UTL_COUNTY

	!
	! Restore UTL_COUNTY_OLD value
	!
	CASE OPT_RESETOLD
		UTL_COUNTY = UTL_COUNTY_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		UTL_COUNTY2 = UTL_COUNTY

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		UTL_COUNTY = UTL_COUNTY2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is
		! to be called.
		!
		UTL_COUNTY::COUNTRY = LEFT(MVALUE, 2%)
		UTL_COUNTY::STATE = RIGHT(MVALUE, 3%)

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #UTL_COUNTY.CH%, &
				KEY #0% GE (UTL_COUNTY::COUNTRY + &
				UTL_COUNTY::STATE + UTL_COUNTY::COUNTY), &
				REGARDLESS

		CASE 1%
			FIND #UTL_COUNTY.CH%, &
				KEY #1% GE UTL_COUNTY::DESCR + "", &
				REGARDLESS

		END SELECT

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP
			!
			! Title (One line only)
			!
			CASE 1%
				MVALUE = "  County Description"

			!
			! Positions of lines
			!
			CASE 2%
				MVALUE = "009"

			!
			! Convert current record into text
			!
			CASE 3%
				MVALUE = UTL_COUNTY::COUNTY + "     " + &
					UTL_COUNTY::DESCR

		END SELECT

	!
	! Handle array of records
	!
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

27110			!
			! Search for first record
			!
			WHEN ERROR IN
				FIND #SMG_WINDOW::CHAN, KEY #0% EQ MVALUE, REGARDLESS

				!
				! Get a record
				!
				GET #SMG_WINDOW::CHAN
				SMG_WINDOW::CURREC = 0%
			USE
				CONTINUE 28000 IF ERR = 155%
				EXIT HANDLER
			END WHEN

		!
		! Check if still right key
		!
		CASE 3%
			SMG_WINDOW::CURREC = -1%
			IF (UTL_COUNTY::COUNTRY+UTL_COUNTY::STATE) = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			UTL_COUNTY::COUNTRY = LEFT(MVALUE, 2%)
			UTL_COUNTY::STATE = RIGHT(MVALUE, 3%)

		END SELECT

	END SELECT

28000	EXIT FUNCTION

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
