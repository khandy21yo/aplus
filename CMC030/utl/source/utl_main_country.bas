1	%TITLE "Country Description Table"
	%SBTTL "UTL_MAIN_COUNTRY"
	%IDENT "V3.6a Calico"

	FUNCTION LONG UTL_MAIN_COUNTRY(CDD_WINDOW_CDD SMG_WINDOW, &
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
	! ID:0125
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Country, State and County\* option enters
	!	codes for each Country, State and County.
	!	.lm -5
	!
	! Index:
	!	.x Table>Country
	!	.x Table>State
	!	.x Table>County
	!	.x Country>Table
	!	.x State>Table
	!	.x County>Table
	!
	! Option:
	!
	! COMPILE:
	!
	!	$ BAS UTL_SOURCE:UTL_MAIN_COUNTRY/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN UTL_MAIN_COUNTRY
	!	$ DELETE UTL_MAIN_COUNTRY.OBJ;*
	!
	! Author:
	!
	!	02/03/88 - Aaron Redd
	!
	! Modification history:
	!
	!	05/23/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	08/01/89 - Aaron Redd
	!		Rewrote to be a simple maintenance program, instead
	!		of a journal (MAIN_WINDOW vs. MAIN_JOURNAL)
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
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE
	!
	!	12/01/2000 - Kevin Handy
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

	!
	! CDD Inclusions and memory MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTRY.HB"
	MAP	(UTL_COUNTRY)	UTL_COUNTRY_CDD	UTL_COUNTRY
	MAP	(UTL_COUNTRY_2)	UTL_COUNTRY_CDD	UTL_COUNTRY_OLD, UTL_COUNTRY2

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_UTL_COUNTRY) &
		UTL_COUNTRY.CH%, &
		UTL_COUNTRY.READONLY%

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
		SMG_WINDOW::DESCR = "Country Descr Maintenance"
		SMG_WINDOW::NHELP = "UTL_MAIN_COUNTRY"
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 76%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 3%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::VHPOS = 3%
		SMG_WINDOW::VVPOS = 2%
		SMG_WINDOW::NITEMS= 2%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "country_Code"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KNAME(1%) = "country_Descr"
			SMG_WINDOW::KFIELD(1%, 0%) = 1%
			SMG_WINDOW::KFIELD(1%, 1%) = 2%

		!
		! Load in defaults for country description
		!
		CALL READ_DEFAULTS(SMG_WINDOW) IF &
			(INSTR(1%, " QV", MVALUE) <= 1%)

700		!
		! Declare channels
		!
		IF UTL_COUNTRY.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF UTL_COUNTRY.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTRY.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			UTL_MAIN_COUNTRY = ERR
			CONTINUE 770
		END WHEN

		UTL_COUNTRY.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTRY.OPN"
		USE
			UTL_MAIN_COUNTRY = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		UTL_COUNTRY.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(UTL_COUNTRY.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = UTL_COUNTRY.CH%
		WHEN ERROR IN
			RESET #UTL_COUNTRY.CH%
			GET #UTL_COUNTRY.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

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

		DATA	3,  5, "(01) Country Code", &
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
	!	^*(01) Country\*
	!	.b
	!	.lm +5
	!	The ^*Country\* field
	!	contains the ANSI code for a particular country.
	!	.b
	!	The field will accommodate a two (2) character code.
	!	.b
	!	Country codes are to be the standard ANSI codes.  Deviations from or
	!	modifications to the ANSI codes provided should not be considered.
	!	.lm -5
	!
	! Index:
	!	.x ANSI Code>Country
	!	.x Code>Country
	!	.x Country>ANSI Code
	!
	!--

			UTL_COUNTRY::COUNTRY = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"3;25", TEMP$, UTL_COUNTRY::COUNTRY, &
				MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field
	!	contains the name of country represented by the code in field (01).
	!	.b
	!	The field will accommodate forty (40) characters.
	!	.lm -5
	!
	! Index:
	!	.x Country>Name
	!
	!--

			UTL_COUNTRY::DESCR = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;25", TEMP$, UTL_COUNTRY::DESCR, MFLAG, &
				"'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		UTL_MAIN_COUNTRY = 0%

	!
	! Set UTL_COUNTRY_OLD value
	!
20500	CASE OPT_SETOLD
		UTL_COUNTRY_OLD = UTL_COUNTRY

	!
	! Restore UTL_COUNTRY_OLD value
	!
	CASE OPT_RESETOLD
		UTL_COUNTRY = UTL_COUNTRY_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		UTL_COUNTRY2 = UTL_COUNTRY

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		UTL_COUNTRY = UTL_COUNTRY2

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #UTL_COUNTRY.CH%, &
				KEY #0% GE UTL_COUNTRY::COUNTRY + "", &
				REGARDLESS

		CASE 1%
			FIND #UTL_COUNTRY.CH%, &
				KEY #1% GE UTL_COUNTRY::DESCR + "", &
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
				MVALUE = "  Country Description"

			!
			! Positions of lines
			!
			CASE 2%
				MVALUE = "010"

			!
			! Convert current record into text
			!
			CASE 3%
				MVALUE = UTL_COUNTRY::COUNTRY + "      " + &
					UTL_COUNTRY::DESCR

		END SELECT

	END SELECT

28000	EXIT FUNCTION

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
	!+-+-+
	! More Menu option hidden in mast.
	!++
	! Abstract:STATE
	!	^*State\*
	!	.b
	!	.lm +5
	!	The ^*State\* option
	!	maintains the State codes and definitions.
	!	.lm -5
	!
	! Index:
	!
	!--
	!+-+-+
	! More Menu option hidden in mast.
	!++
	! Abstract:REGION
	!	^*Region\*
	!	.b
	!	.lm +5
	!	The ^*Region\* option
	!	maintains the Region codes and descriptions.
	!	.lm -5
	!
	! Index:
	!
	!--
