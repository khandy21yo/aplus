1	%TITLE "Maintain Customer Licenses"
	%SBTTL "SS_MAIN_LICENSE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG SS_MAIN_LICENSE(CDD_WINDOW_CDD SMG_WINDOW, &
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
	! Abstract:HELP
	!	.p
	!	This function contains all of the routines needed
	!	to maintain the Support System's Customer License file.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS SS_SOURCE:SS_MAIN_LICENSE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN SS_MAIN_LICENSE
	!	$ DELETE SS_MAIN_LICENSE.OBJ;*
	!
	! Author:
	!
	!	08/03/89 - Aaron Redd
	!
	! Modification history:
	!
	!	11/30/92 - Dan Perkins
	!		Added CASE 2 to OPT_SUBWIND so that VIEW would
	!		work properly.
	!
	!	12/01/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	04/20/95 - Kevin Handy
	!		Fix format parameter to entr_3phone.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/24/2000 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:SS_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	!
	! CDD inclusions and related memory MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[SS.OPEN]SS_LICENSE.HB"
	MAP	(SS_LICENSE)		SS_LICENSE_CDD	SS_LICENSE
	MAP	(SS_LICENSE_OLD)	SS_LICENSE_CDD	SS_LICENSE_OLD, SS_LICENSE2

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_SS_LICENSE) &
		SS_LICENSE.CH%, &
		SS_LICENSE.READONLY%

	%PAGE

	!
	! Set up error trapping
	!
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
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Customer License Maintenance"
		SMG_WINDOW::NHELP = "SS_MAIN_LICENSE"
		SMG_WINDOW::HSIZE = 74%
		SMG_WINDOW::VSIZE = 11%
		SMG_WINDOW::HPOS  = 3%
		SMG_WINDOW::VPOS  = 6%
		SMG_WINDOW::HVIEW = 74%
		SMG_WINDOW::VVIEW = 11%
		SMG_WINDOW::VHPOS = 3%
		SMG_WINDOW::VVPOS = 6%
		SMG_WINDOW::NITEMS= 9%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "License_num"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		!
		! Load in defaults for Customer Licensing
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF SS_LICENSE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF SS_LICENSE.READONLY%
			GOTO 790
		END IF
		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[SS.OPEN]SS_LICENSE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			SS_MAIN_LICENSE = ERR
			CONTINUE 770
		END WHEN

		SS_LICENSE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[SS.OPEN]SS_LICENSE.OPN"
		USE
			SS_MAIN_LICENSE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		SS_LICENSE.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(SS_LICENSE.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = SS_LICENSE.CH%
		WHEN ERROR IN
			RESET #SS_LICENSE.CH%
			GET #SS_LICENSE.CH%, REGARDLESS
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

		DATA	2,  5, "(01) License Number", &
			3,  5, "(02) Expiration Date", &
			4,  5, "(03) Make Number", &
			5,  5, "(04) Model Number", &
			6,  5, "(05) Serial Number", &
			7,  5, "(06) Operating System", &
			8,  5, "(07) Version", &
			9,  5, "(08) Phone Number", &
			10, 5, "(09) Phone Extension", &
			0,  0, ""

		RESTORE

		READ XPOS%, YPOS%, XSTR$
		I% = 0%

		WHILE XPOS% <> 0%
			I% = I% + 1%
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				XSTR$, XPOS%, YPOS%) &
				IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%

			READ XPOS%, YPOS%, XSTR$
		NEXT

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	%PAGE

	!
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
	!
20200	CASE OPT_ENTRY

		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
			SS_LICENSE::LICENSE_NUM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;30", TEMP$, SS_LICENSE::LICENSE_NUM, &
				MFLAG, "'E", MVALUE)

		CASE 2%
			SS_LICENSE::EXPIR_DATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"3;30", TEMP$, SS_LICENSE::EXPIR_DATE, &
				MFLAG, "'E", MVALUE)

		CASE 3%
			SS_LICENSE::MAKE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;30", TEMP$, SS_LICENSE::MAKE, &
				MFLAG, "'E", MVALUE)

		CASE 4%
			SS_LICENSE::MODEL = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;30", TEMP$, SS_LICENSE::MODEL, &
				MFLAG, "'E", MVALUE)

		CASE 5%
			SS_LICENSE::SERIAL_NUM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;30", TEMP$, SS_LICENSE::SERIAL_NUM, &
				MFLAG, "'E", MVALUE)

		CASE 6%
			SS_LICENSE::OPER_SYS = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"7;30", TEMP$, SS_LICENSE::OPER_SYS, &
				MFLAG, "'E", MVALUE)

		CASE 7%
			SS_LICENSE::VERSION = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"8;30", TEMP$, SS_LICENSE::VERSION, &
				MFLAG, "'E", MVALUE)

		CASE 8%
			SS_LICENSE::PHONE_NUM = ENTR_3PHONE(SCOPE, SMG_WINDOW::WNUMBER, &
				"9;30", TEMP$, SS_LICENSE::PHONE_NUM, &
				MFLAG, 0%, MVALUE)

		CASE 9%
			SS_LICENSE::PHONE_EXT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;30", TEMP$, SS_LICENSE::PHONE_EXT, &
				MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		SS_MAIN_LICENSE = 0%

		SELECT MLOOP
			CASE 1%
				SS_MAIN_LICENSE = 1% IF &
					SS_LICENSE::LICENSE_NUM = ""
		END SELECT

	!
	! Set SS_LICENSE_OLD value
	!
20500	CASE OPT_SETOLD
		SS_LICENSE_OLD = SS_LICENSE

	!
	! Restore SS_LICENSE_OLD value
	!
	CASE OPT_RESETOLD
		SS_LICENSE = SS_LICENSE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		SS_LICENSE2 = SS_LICENSE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		SS_LICENSE = SS_LICENSE2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		SS_LICENSE::CUSNUM = MVALUE

	!
	! Find
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SS_LICENSE.CH%, &
				KEY #0% GE SS_LICENSE::CUSNUM + &
				SS_LICENSE::LICENSE_NUM, REGARDLESS

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
			MVALUE = "  LicenseNum ExpirDat Make       " + &
				"Model      OperSys Version PhoneNum"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "013,022,033,044,052,060"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = SS_LICENSE::LICENSE_NUM              + " "  + &
				PRNT_DATE(SS_LICENSE::EXPIR_DATE, 0%) + " "  + &
				SS_LICENSE::MAKE                      + " "  + &
				SS_LICENSE::MODEL                     + " "  + &
				SS_LICENSE::OPER_SYS                  + "  " + &
				SS_LICENSE::VERSION                   + "  " + &
				PRNT_PHONE(SS_LICENSE::PHONE_NUM, 0%)

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
					FIND #SS_LICENSE.CH%, &
						KEY #0% GE MVALUE + &
						SS_LICENSE::LICENSE_NUM, REGARDLESS
				USE
					CONTINUE 28000 IF ERR = 155%
					EXIT HANDLER
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

			IF SS_LICENSE::CUSNUM = MVALUE
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			SS_LICENSE::CUSNUM = MVALUE

		END SELECT

	END SELECT

28000	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
