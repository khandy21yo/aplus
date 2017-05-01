1	%TITLE "Maintain Purchase Order Control Record"
	%SBTTL "PO_MAIN_CONTROL"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PO_MAIN_CONTROL(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Maintain Purchase Order Control Record\*
	!	establishes several different records
	!	and dates that once are established need not be
	!	accessed again.
	!	.lm -5
	!
	! Index:
	!	.x Maintain>Control Record
	!	.x Control Record>Maintain
	!	.x Maintain>Control
	!	.x Control>Maintain
	!
	! Option:
	!
	! Author:
	!
	!	07/23/88 - J. Shad Rydalch
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_MAIN_CONTROL/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PO_MAIN_CONTROL
	!	$ DELETE PO_MAIN_CONTROL.OBJ;*
	!
	! Modification history:
	!
	!	02/01/92 - Frank F. Starman
	!		Remove UTL_TRANSTYPE
	!
	!	04/09/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/18/94 - Kevin Handy
	!		Added LOAD_FORMULA field.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/20/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/11/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PO.OPEN]PO_CONTROL.HB"
	MAP (PO_CONTROL)	PO_CONTROL_CDD	PO_CONTROL
	MAP (PO_CONTROL2)	PO_CONTROL_CDD	PO_CONTROL_OLD, PO_CONTROL2

	!
	! This common area must be mapped in both the main program and
	! in MAINT_GROUP.
	!
	COM (CH_PO_CONTROL) &
		PO_CONTROL.CH%, &
		PO_CONTROL.READONLY%

	ON ERROR GOTO 29000

	%PAGE

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
		SMG_WINDOW::DESCR = "Control File Maintenance"
		SMG_WINDOW::NHELP = "PO_MAIN_CONTROL"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 2%
		SMG_WINDOW::FLAGS = 128%
		! Relative file

		SMG_WINDOW::NKEYS = 0%

700		!
		! Declare channels
		!
		IF PO_CONTROL.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PO_CONTROL.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_CONTROL.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PO_MAIN_CONTROL = ERR
			CONTINUE 770
		END WHEN

		PO_CONTROL.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PO.OPEN]PO_CONTROL.OPN"
		USE
			PO_MAIN_CONTROL = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PO_CONTROL.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PO_CONTROL.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PO_CONTROL.CH%
		GOSUB 28000

	CASE OPT_OPTLIST
		MVALUE = "Change Blank Help eXit"

	!
	! Display the background
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	5, 5, "(01) Last PO", &
			7, 5, "(02) Load Formula", &
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

	!
	! Enter/Display/Default
	!
20200	CASE OPT_ENTRY
		TEMP$ = TRM$(SCOPE::PRG_ITEM)

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 Reenter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Purchase Order Number\*
	!	.b
	!	.lm +5
	!	The ^*PO Number\* field records the last PO number
	!	used prior to initialization of the system.  The system will then
	!	automatically increment and record the number of the PO's.
	!	.lm -5
	!
	! Index:
	!	.x PO Number>Control
	!	.x Purchase Order>Number>Control
	!	.x Control>Purchase Order Number
	!	.x Control>PO Number
	!
	!--
			PO_CONTROL::LAST_PO = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;24",TEMP$, &
				PO_CONTROL::LAST_PO, &
				MFLAG OR 2%, "~R 'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Load Formula\*
	!	.b
	!	.lm +5
	!	This field is used by the \*Load PO's\* program.
	!	It tells what factors to consider when calculating the quanity to
	!	order.
	!	Each of ten possible factors considered are assigned a single character
	!	position on the string.
	!	.b
	!	The factors available are:
	!	.b
	!	.table 3,28
	!	.te
	!	A	Allocated
	!	.te
	!	H	On Hand
	!	.te
	!	O	On Order
	!	.te
	!	S	Safety Stock
	!	.TE
	!	1	Current Quarter (Qtr1)
	!	.te
	!	2	Last Quarter (Qtr2)
	!	.te
	!	3	(Qtr3)
	!	.te
	!	4	(Qtr4)
	!	.te
	!	5	(Qtr5)
	!	.end table
	!	.b
	!	The default formula is "^*HAOS\*".
	!	.lm -5
	!
	! Index:
	!
	!--
			PO_CONTROL::LOAD_FORMULA = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"7;24",TEMP$, &
				PO_CONTROL::LOAD_FORMULA, &
				MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		!
		! Set PO_CONTROL_OLD value
		!
20500		CASE OPT_SETOLD
			PO_CONTROL_OLD = PO_CONTROL

		!
		! Restore PO_CONTROL_OLD value
		!
		CASE OPT_RESETOLD
			PO_CONTROL = PO_CONTROL_OLD

		!
		! Set default value
		!
		CASE OPT_SETDEFAULT
			PO_CONTROL2 = PO_CONTROL

		!
		! Restore default value
		!
		CASE OPT_RESETDEFAULT
			PO_CONTROL = PO_CONTROL2

	END SELECT

	EXIT FUNCTION

28000	!
	! Get period record
	!
	WHEN ERROR IN
		GET #PO_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		CONTINUE 28030
	END WHEN

	GOTO 28040

28030	!
	! Load in defaults for period file
	!
	PO_CONTROL::LAST_PO = SPACE$(LEN(PO_CONTROL::LAST_PO))
	PO_CONTROL::LOAD_FORMULA = "HAOS"
	WHEN ERROR IN
		PUT #PO_CONTROL.CH%, RECORD 1%
	USE
		CALL ENTR_3MESSAGE(SCOPE, "Unable to add period record", 0%)
		CONTINUE 32767
	END WHEN

28040	RETURN

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
