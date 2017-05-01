1	%TITLE "Sales Group File Maintenance"
	%SBTTL "SA_MAIN_SALEGROUP"
	%IDENT "V3.6a Calico"

	FUNCTION LONG SA_MAIN_SALEGROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1996 BY
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
	! Software Solutions, Inc
	!
	! Software Solutions, Inc assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS SA_SOURCE:SA_MAIN_SALEGROUP/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN SA_MAIN_SALEGROUP
	!	$ DELETE SA_MAIN_SALEGROUP.OBJ;*
	!
	! Author:
	!
	!	06/27/96 - Kevin Handy
	!
	! Modification history:
	!
	!	09/13/96 - Kevin Handy
	!		Lose unecessary error traps.
	!
	!	10/03/96 - Kevin Handy
	!		Add lookup into salesman file.
	!		Added CH_SA_SALEGROUP common area
	!
	!	10/29/96 - Kevin Handy
	!		Don't allow salesgroup number to be same as a
	!		salesman number.
	!
	!	11/04/96 - Kevin Handy
	!		Fix several typo's
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
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

	%INCLUDE "FUNC_INCLUDE:SA_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[SA.OPEN]SA_SALEGROUP.HB"
	MAP (SA_SALEGROUP)	SA_SALEGROUP_CDD	SA_SALEGROUP
	MAP (SA_SALEGROUP_OLD)	SA_SALEGROUP_CDD	SA_SALEGROUP_OLD
	MAP (SA_SALEGROUP_DEF)	SA_SALEGROUP_CDD	SA_SALEGROUP_DEF

	%INCLUDE "SOURCE:[SA.OPEN]SA_SALESMAN.HB"
	MAP (SB_SUBACCOUNT)	SA_SALESMAN_CDD		SA_SALESMAN

	COM (CH_SA_SALEGROUP) &
		SA_SALEGROUP.CH%, &
		SA_SALEGROUP.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION FUNC_TESTENTRY

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

		!************************************************************
		! Set up information
		!************************************************************

		!
		! Define SMG_WINDOW
		!
		SMG_WINDOW::DESCR = "Broker and Salesman Groups Maintenance"
		SMG_WINDOW::NHELP = "SA_MAIN_SALEGROUP"
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 2%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "Group"
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
		SMG_WINDOW::KNAME(1%) = "Type"
			SMG_WINDOW::KFIELD(1%, 0%) = 2%
			SMG_WINDOW::KFIELD(1%, 1%) = 2%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%

		SMG_WINDOW::HVIEW = 76%
		SMG_WINDOW::VVIEW = 18%

		!
		! Read Defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF SA_SALEGROUP.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if it was
			! that way from the last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF SA_SALEGROUP.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[SA.OPEN]SA_SALEGROUP.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			SA_MAIN_SALEGROUP = ERR
			CONTINUE 770
		END WHEN

		SA_SALEGROUP.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[SA.OPEN]SA_SALEGROUP.OPN"
		USE
			SA_MAIN_SALEGROUP = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		SA_SALEGROUP.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(SA_SALEGROUP.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = SA_SALEGROUP.CH%
		WHEN ERROR IN
			RESET #SA_SALEGROUP.CH%
			GET #SA_SALEGROUP.CH%, REGARDLESS
		USE
			CONTINUE 32767 IF ERR = 11%
			EXIT HANDLER
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


		DATA	01,05, "(01) Group Number", &
			02,05, "(02) Salesman", &
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

 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Group\*
	!
	! Index:
	!	.x Group>Salesman Group
	!	.x Salesman Group>Group
	!
	!--

			SA_SALEGROUP::SALGROUP = &
				ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"1;32",TEMP$, SA_SALEGROUP::SALGROUP, &
				MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Salesman\*
	!
	! Index:
	!	.x Salesman>Salesman Group
	!	.x Salesman Group>Salesman
	!
	!--

			SA_SALEGROUP::SALESMAN = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;32",	TEMP$, SA_SALEGROUP::SALESMAN, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(SA_MAIN_SALESMAN.ID, "V0 ") = 1%
				THEN
					SA_SALEGROUP::SALESMAN = SA_SALESMAN::TTYPE
				END IF
				GOTO ReEnter
			END IF

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		SA_MAIN_SALEGROUP = 0%

		SELECT MLOOP

		CASE 1%
			ST% = FUNC_TESTENTRY(SMG_WINDOW, &
				SA_SALEGROUP::SALESMAN, SA_SALESMAN::SALESMAN, &
				"SA", MLOOP, "SALMAN", &
				"Sales group", SA_MAIN_SALESMAN.ID)

			IF (ST% = 0%)
			THEN
				SA_MAIN_SALEGROUP = 1%

				CALL ENTR_3MESSAGE(SCOPE, &
					"Number already assigned to salesman", &
					0%)
			END IF

		END SELECT

	!
	! Set SA_SALEGROUP_OLD value
	!
	CASE OPT_SETOLD
		SA_SALEGROUP_OLD = SA_SALEGROUP

	!
	! Restore SA_SALEGROUP_OLD value
	!
	CASE OPT_RESETOLD
		SA_SALEGROUP = SA_SALEGROUP_OLD
	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		SA_SALEGROUP_DEF = SA_SALEGROUP
	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		SA_SALEGROUP = SA_SALEGROUP_DEF

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE ="  Group      Salesman               " + &
				"                                        "

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "013"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = SA_SALEGROUP::SALGROUP + " " + &
				SA_SALEGROUP::SALESMAN

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE SA_SALEGROUP::SALGROUP + &
				SA_SALEGROUP::SALESMAN, &
				REGARDLESS

		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE SA_SALEGROUP::SALESMAN + &
				SA_SALEGROUP::SALGROUP, &
				REGARDLESS

		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
