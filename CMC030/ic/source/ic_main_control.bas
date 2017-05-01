1	%TITLE "Inventory Control File"
	%SBTTL "IC_MAIN_CONTROL"
	%IDENT "V3.6a Calico"

	FUNCTION LONG IC_MAIN_CONTROL(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	.b
	!	.lm +5
	!	The "Era Code", "Period", and "Status Flag" are established in the
	!	^*Inventory Control File\*.  After initialization, changes to this file
	!	should not be considered.
	!	.lm -5
	!
	! Index:
	!	.x Controlling File>Utility
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_MAIN_CONTROL/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP IC_MAIN_CONTROL
	!	$ DELETE IC_MAIN_CONTROL.OBJ;*
	!
	! Author:
	!
	!	08/11/87 - Frank F. Starman
	!
	! Modification history:
	!
	!	02/21/88 - Frank F. Starman
	!		Made a new layout with era code
	!
	!	05/31/88 - Aaron Redd
	!		Modified to allow R/O open of file if R/W open fails.
	!
	!	01/11/91 - Frank F. Starman
	!		Change meaning of the Period field. Now the Period
	!		is the current one.
	!
	!	04/06/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/23/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/18/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/01/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:IC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP (IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL
	MAP (IC_CONTROL_OLD)	IC_CONTROL_CDD		IC_CONTROL_OLD, IC_CONTROL2

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_ERA.HB"
	MAP (UTL_ERA)		UTL_ERA_CDD		UTL_ERA

	COM (TT_IC_CONTROL) &
		CLOSETITLE$ = 20%, &
		CLOSETYPE$(5%) = 20%

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_IC_CONTROL) &
		IC_CONTROL.CH%, &
		IC_CONTROL.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_WINDOW
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY

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
		SMG_WINDOW::DESCR = "IC Control file"
		SMG_WINDOW::NHELP = "IC_MAIN_CONTROL"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 128% !Relative file
		SMG_WINDOW::NITEMS= 3%

		SMG_WINDOW::NKEYS = 0%

		CLOSETITLE$ = "Type   Description"
		CLOSETYPE$(0%) = "5"
		CLOSETYPE$(1%) = "0    No status"
		CLOSETYPE$(2%) = "1    Closing"
		CLOSETYPE$(3%) = "2    Resetting"
		CLOSETYPE$(4%) = "3    Posting"
		CLOSETYPE$(5%) = "4    Resynchronizing"

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF IC_CONTROL.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF IC_CONTROL.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			IC_MAIN_CONTROL = ERR
			CONTINUE 770
		END WHEN

		IC_CONTROL.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.OPN"
		USE
			IC_MAIN_CONTROL = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		IC_CONTROL.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(IC_CONTROL.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = IC_CONTROL.CH%
		GOSUB 28000

	!
	! Select function
	!
	CASE OPT_OPTLIST

		MVALUE = "Change Blank Help eXit"

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


		DATA	06,05, "(01) Era Code", &
			07,05, "(02) Period", &
			08,05, "(03) Status Flag", &
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
	!	^*(01) Era Code\*
	!	.b
	!	.lm +5
	!	The ^*Era Code\* field enters a user defined
	!	code which will identify a particular era (period) of time.  An
	!	"Era" is defined in this system as a measurement of a period of time.
	!	.b
	!	Valid Era codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Era Code
	!
	!--
			IC_CONTROL::ERA = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"06;36", TEMP$, &
				IC_CONTROL::ERA, MFLAG, "'E",MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_ERA.ID, "V0  ") = 1%
				THEN
					IC_CONTROL::ERA = &
						UTL_ERA::ERA
				END IF
				GOTO Reenter
			END IF

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Period\*
	!	.b
	!	.lm +5
	!	The ^*Period\* field contains the
	!	inventory accounting period which is currently open.
	!	This is the period that will be used when the ledger
	!	is printed.
	!	.b
	!	When the Inventory system is initialized, this
	!	field should be the period in which the system
	!	is initialized.
	!	.b
	!	^*Note:\* After completing the initialization procedures,
	!	this field should never again be edited.
	!	.lm -5
	!
	! Index:
	!	.x Inventory Period
	!
	!--
			IC_CONTROL::PERIOD = ENTR_PERIOD(SMG_WINDOW::WNUMBER, &
				"07;36", TEMP$, IC_CONTROL::PERIOD, &
				MFLAG, IC_CONTROL::ERA, MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Status Flag\*
	!	.b
	!	.lm +5
	!	The ^*Status Flag\* field is always
	!	system generated.
	!	.b
	!	Valid flags are:
	!	.table 3,25
	!	.te
	!	^*0\*	No Status
	!	.te
	!	^*1\*	Closing
	!	.te
	!	^*2\*	Resetting
	!	.te
	!	^*3\*	Posting
	!	.te
	!	*4	Resync
	!	.te
	!	*5	Closing/Zeroing Balances
	!	.end table
	!	Valid flags may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Status>Flag
	!
	!--

			IC_CONTROL::CONTROLFLAG = EDIT$(ENTR_3STRINGLIST(SCOPE, SMG_WINDOW::WNUMBER, &
				"08;36", TEMP$, IC_CONTROL::CONTROLFLAG, &
				MFLAG, "'", MVALUE, CLOSETYPE$(), &
				CLOSETITLE$, "005"), -1%)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		IC_MAIN_CONTROL = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Is the input defined?
			!
			IC_MAIN_CONTROL = FUNC_TESTENTRY(SMG_WINDOW, &
				IC_CONTROL::ERA, UTL_ERA::DESCRIPTION, &
				"IC", MLOOP, "PROD", &
				"Era", UTL_MAIN_ERA.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_ERA::DESCRIPTION, 06%, 43%, , SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			DESCRIPTION$ = STRING$(20%, 63%)
			DESCRIPTION$ = UTL_ERA::DESCRIPTION &
				IF MAIN_WINDOW(UTL_MAIN_ERA.ID, &
				"Q0" + IC_CONTROL::ERA) = 1%
			!
			! Display name (ALSO IN TESTENTRY)
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				DESCRIPTION$, 06%, 43%, , SMG$M_BOLD)
		END IF

	!
	! Set IC_CONTROL_OLD value
	!
20500	CASE OPT_SETOLD
		IC_CONTROL_OLD = IC_CONTROL

	!
	! Restore IC_CONTROL_OLD value
	!
	CASE OPT_RESETOLD
		IC_CONTROL = IC_CONTROL_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		IC_CONTROL2 = IC_CONTROL

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		IC_CONTROL = IC_CONTROL2

	END SELECT

27000	EXIT FUNCTION

28000	!
	! Get control record
	!
	WHEN ERROR IN
		GET #IC_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		CONTINUE 28030
	END WHEN

	GOTO 28040

28030	!
	! Load in defaults for control file
	!
	IC_CONTROL::ERA = "??"
	IC_CONTROL::PERIOD = "??????"
	IC_CONTROL::CONTROLFLAG = "0"

	WHEN ERROR IN
		PUT #IC_CONTROL.CH%
	USE
		CALL ENTR_3MESSAGE(SCOPE, "Unable to add a record", 0%)
		CONTINUE 32767
	END WHEN

28040	RETURN

29000	!***************************************************************
	! Trap errors
	!***************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
