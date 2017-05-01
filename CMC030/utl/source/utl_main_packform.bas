1	%TITLE "Packaging Form"
	%SBTTL "UTL_MAIN_PACKFORM"
	%IDENT "V3.6a Calico"

	FUNCTION LONG UTL_MAIN_PACKFORM(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	! ID:0175
	!
	! Abstract:HELP
	!	.p
	!	The ^*Packaging Form\* option sets up
	!	codes and descriptions for different packaging forms. Examples are:
	!	bale, bag, container, roll, etc.
	!
	! Index:
	!	.x Packaging>Form
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS SOURCE:[UTL.SOURCE]UTL_MAIN_PACKFORM/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN UTL_MAIN_PACKFORM
	!	$ DELETE UTL_MAIN_PACKFORM.OBJ;*
	!
	! Author:
	!
	!	10/30/87 - Frantisek Starman
	!
	! Modification history:
	!
	!	05/30/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/30/96 - Kevin Handy
	!		Reformat source code
	!
	!	06/07/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	11/29/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_PACKFORM.HB"
	MAP (UTL_PACKFORM)		UTL_PACKFORM_CDD	UTL_PACKFORM
	MAP (UTL_PACKFORM_OLD) UTL_PACKFORM_CDD UTL_PACKFORM_OLD, UTL_PACKFORM2

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_UTL_PACKFORM) &
		UTL_PACKFORM.CH%, &
		UTL_PACKFORM.READONLY%

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
		SMG_WINDOW::DESCR = "Packaging form description"
		SMG_WINDOW::NHELP = "UTL_MAIN_PACKFORM"
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
		IF UTL_PACKFORM.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF UTL_PACKFORM.READONLY%
			GOTO 790
		END IF

		IF (UTL_PACKFORM.CH% <= 0%)
		THEN
			!
			! Open main file (existing) for modification
			!
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_PACKFORM.CRE"
		END IF

750		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_PACKFORM.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			UTL_MAIN_PACKFORM = ERR
			CONTINUE 770
		END WHEN

		UTL_PACKFORM.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_PACKFORM.OPN"
		USE
			UTL_MAIN_PACKFORM = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		UTL_PACKFORM.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(UTL_PACKFORM.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = UTL_PACKFORM.CH%
		WHEN ERROR IN
			RESET #UTL_PACKFORM.CH%
			GET #UTL_PACKFORM.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display SMG_WINDOW background
	!
	CASE OPT_BACKGROUND

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
20200	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Code\*
	!	.p
	!	The ^*Code\* field contains a specific
	!	standard ANSI code which identifies a particular packaging form.
	!	.p
	!	Since the codes provided with the system are internationally recognized ANSI
	!	codes, deviation from or modifications to the codes provided should not be
	!	considered.
	!	.p
	!	This field will accommodate three (3) alphanumeric characters.
	!
	! Index:
	!	.x ANSI Codes>Packaging Form
	!	.x Packaging>Form>ASNI Code
	!	.x Packaging>Form>Codes
	!	.x Codes>Packaging>Form
	!
	!--


			UTL_PACKFORM::CODE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "08;38", TEMP$, &
				UTL_PACKFORM::CODE, MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.p
	!	The ^*Description\* field
	!	describes a packaging form.
	!	.p
	!	This field will accommodate twenty (20) alphanumeric characters.
	!
	! Index:
	!	.x Packaging>Form>Description
	!
	!--

			UTL_PACKFORM::DESCRIPTION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "10;38", TEMP$, &
				UTL_PACKFORM::DESCRIPTION, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		UTL_MAIN_PACKFORM = 0%

		SELECT MLOOP

		CASE 1%
			IF UTL_PACKFORM::CODE = ""
			THEN
				UTL_MAIN_PACKFORM = 1%
			END IF

		END SELECT

	!
	! Set UTL_PACKFORM_OLD value
	!
20500	CASE OPT_SETOLD
		UTL_PACKFORM_OLD = UTL_PACKFORM

	!
	! Restore UTL_PACKFORM_OLD value
	!
	CASE OPT_RESETOLD
		UTL_PACKFORM = UTL_PACKFORM_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		UTL_PACKFORM2 = UTL_PACKFORM

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		UTL_PACKFORM = UTL_PACKFORM2

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
			MVALUE = UTL_PACKFORM::CODE + "  " + &
				UTL_PACKFORM::DESCRIPTION
		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE UTL_PACKFORM::CODE + "", &
				REGARDLESS
		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
