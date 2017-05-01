1	%TITLE "Manufacturing Orders Make Master Maintenance"
	%SBTTL "MO_MAIN_MAKE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG MO_MAIN_MAKE(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1991 BY
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
	!	Information concerning a particular Make and its associated Models is entered
	!	and maintained in the ^*Make Master Maintenance\* file.
	!	.b
	!	The ^*Make Master\* screen contains the following fields:
	!	.lm 15
	!	.b
	!	.LIST 0,"*"
	!	.LE
	!	(01) Make
	!	.le
	!	(02) Year
	!	.le
	!	(03) Type
	!	.le
	!	(04) Size
	!	.le
	!	(05) Class
	!	.le
	!	(06) Cut Tubing
	!	.le
	!	(07) Front Slant
	!	.le
	!	(08) Overall Length
	!	.le
	!	(09) Narrow Front Flag
	!	.le
	!	(10) Narrow Back Flag
	!	.els
	!	.lm -10
	!
	! Index:
	!	.x Maintenance>Make
	!	.x Make>Maintenance
	!
	! Option:
	!
	! Author:
	!	02/27/91 - Val James Allen
	!
	! Compile:
	!
	!	$ BAS MO_SOURCE:MO_MAIN_MAKE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN MO_MAIN_MAKE
	!	$ DELETE MO_MAIN_MAKE.OBJ;*
	!
	! Modification history:
	!
	!	08/07/91 - Craig Tanner
	!		Changed OPT_AFTEROPT, as it was not adding records to
	!		line file.
	!
	!	08/19/91 - Dan Perkins
	!		Added F17 key feature.
	!
	!	06/12/91 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/26/96 - Kevin Handy
	!		Reformat source code.
	!		Change SPACE$(...,ASCII(" ")) to "" in
	!		several places.
	!
	!	08/28/97 - Kevin Handy
	!		Lose unecessary external definitions.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	10/31/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MO_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[MO.OPEN]MO_MAKE.HB"
	MAP (MO_MAKE)		MO_MAKE_CDD		MO_MAKE
	MAP (MO_MAKE_OLD)	MO_MAKE_CDD		MO_MAKE_OLD, MO_MAKE2
	DECLARE			MO_MAKE_CDD		MO_MAKE_HOLD

	%INCLUDE "SOURCE:[MO.OPEN]MO_MAKECLASS.HB"
	MAP (MO_MAKECLASS)	MO_MAKECLASS_CDD	MO_MAKECLASS

	%INCLUDE "SOURCE:[MO.OPEN]MO_MAKETYPE.HB"
	MAP (MO_MAKETYPE)	MO_MAKETYPE_CDD		MO_MAKETYPE

	%INCLUDE "SOURCE:[MO.OPEN]MO_MAKESIZE.HB"
	MAP (MO_MAKESIZE)	MO_MAKESIZE_CDD		MO_MAKESIZE

	!
	! Common Statements
	!
	COM (CH_MO_MAKE) &
		MO_MAKE.CH%, &
		MO_MAKE.READONLY%

	COM (CH_MO_MAKELINE) &
		MO_MAKELINE.CH%, &
		MO_MAKELINE.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!
	! Initilization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!
	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "MO Make Master Maintenance"
		SMG_WINDOW::NHELP = "MO_MAIN_MAKE"
		SMG_WINDOW::CHAN  = MO_MAKE.CH%
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 11%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 5%
		SMG_WINDOW::KNAME(0%) = "Make"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		SMG_WINDOW::KNAME(1%) = "Year"
			SMG_WINDOW::KFIELD(1%, 0%) = 2%
			SMG_WINDOW::KFIELD(1%, 1%) = 3%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%

		SMG_WINDOW::KNAME(2%) = "Type"
			SMG_WINDOW::KFIELD(2%, 0%) = 2%
			SMG_WINDOW::KFIELD(2%, 1%) = 4%
			SMG_WINDOW::KFIELD(2%, 2%) = 1%

		SMG_WINDOW::KNAME(3%) = "Size"
			SMG_WINDOW::KFIELD(3%, 0%) = 2%
			SMG_WINDOW::KFIELD(3%, 1%) = 5%
			SMG_WINDOW::KFIELD(3%, 2%) = 1%

		SMG_WINDOW::KNAME(1%) = "Class"
			SMG_WINDOW::KFIELD(4%, 0%) = 2%
			SMG_WINDOW::KFIELD(4%, 1%) = 6%
			SMG_WINDOW::KFIELD(4%, 2%) = 1%

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
		IF MO_MAKE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF MO_MAKE.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_MAKE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			MO_MAIN_MAKE = ERR
			CONTINUE 770
		END WHEN

		MO_MAKE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_MAKE.OPN"
		USE
			MO_MAIN_MAKE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		MO_MAKE.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		CALL ASSG_FREECHANNEL(MO_MAKE.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = MO_MAKE.CH%

		WHEN ERROR IN
			RESET #MO_MAKE.CH%
			GET #MO_MAKE.CH%, REGARDLESS
		USE
			CONTINUE 32767 IF ERR = 11%
			EXIT HANDLER
		END WHEN

	!
	! Display the background
	!
	! This option is used to display the background information
	! on the screen.  It must first clear any junk on the screen,
	! and then write the background onto it.
	!
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	02, 05, "(01) Make", &
			03, 05, "(02) Description", &
			04, 05, "(03) Make Year", &
			05, 05, "(04) Make Type", &
			06, 05, "(05) Make Size", &
			07, 05, "(06) Make Class", &
			08, 05, "(07) Cut Tubing", &
			09, 05, "(08) Front Slant", &
			10, 05, "(09) Overall Length", &
			11, 05, "(10) Narrow Front", &
			12, 05, "(11) Narrow Back", &
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
	! This option is used to enter the data from the user, display
	! data, set defaults, and return the data back according
	! to MFLAG.
	!
20200	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 Reenter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Make\*
	!	.b
	!	.lm +5
	!	The ^*Make\* field enters the Make of the Dealer's
	!	Model (ie: MAZDA, FORD, DODGE, etc.)
	!	.b
	!	An entry is required in this field. The field will
	!	accommodate ten (10) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Make
	!
	!--
			MO_MAKE::MAKE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;25", TEMP$, MO_MAKE::MAKE, MFLAG, &
				"'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field enters the description
	!	of the Make entered in field (01).
	!	.b
	!	The field will accommodate forty (40) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Description
	!
	!--
			MO_MAKE::DESCR = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"3;25", TEMP$, MO_MAKE::DESCR, MFLAG, &
				"'E", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Make Year\*
	!	.b
	!	.lm +5
	!	The ^*Make Year\* field enters the year of the Make entered
	!	in field (01).
	!	.b
	!	The format for entry is YYYY.
	!	.lm -5
	!
	! Index:
	!	.x Make Year
	!
	!--
			MO_MAKE::YEAR = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;25", TEMP$, MO_MAKE::YEAR, MFLAG, &
				"'E", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Make Type\*
	!	.b
	!	.lm +5
	!	The ^*Make Type\* field enters the type
	!	of the Make entered in field (01).
	!	.b
	!	This field will accommodate two (2) alphanumeric characters.
	!	Pressing ^*List Choices\* while the cursor is located at this
	!	field, will display a list of valid Make types.  Additional types may
	!	be added by pressing the ^*F17\* key.
	!	.lm -5
	!
	! Index:
	!	.x Make Type
	!
	!--
			MO_MAKE::MTYPE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;25", TEMP$, MO_MAKE::MTYPE, MFLAG, &
				"'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF (MAIN_WINDOW(MO_MAIN_MAKETYPE.ID, "VX") = 1%)
				THEN
					MO_MAKE::MTYPE = MO_MAKETYPE::MTYPE
				END IF
				GOTO ReEnter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(MO_MAIN_MAKETYPE.ID, "M")
				MO_MAKE::MTYPE = MO_MAKE::MTYPE
				GOTO ReEnter
			END IF

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Make Size\*
	!	.b
	!	.lm +5
	!	The ^*Make Size\* field enters the size
	!	of the Make.
	!	.b
	!	Pressing ^*List Choices\* while the cursor is located at this
	!	field will display a list of valid Make sizes.  Additional size codes may be
	!	added by pressing ^*F17\*.
	!	.lm -5
	!
	! Index:
	!	.x Make Size
	!
	!--
			MO_MAKE::MSIZE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;25", TEMP$, MO_MAKE::MSIZE, MFLAG, &
				"'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF (MAIN_WINDOW(MO_MAIN_MAKESIZE.ID, "VX") = 1%)
				THEN
					MO_MAKE::MSIZE = MO_MAKESIZE::MSIZE
				END IF
				GOTO ReEnter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(MO_MAIN_MAKESIZE.ID, "M")
				MO_MAKE::MSIZE = MO_MAKESIZE::MSIZE
				GOTO ReEnter
			END IF

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Make Class\*
	!	.b
	!	.lm +5
	!	The ^*Make Class\* field enters the class
	!	of the Make.  The field will accommodate four (4) alphanumeric characters.
	!	.b
	!	Pressing ^*List Choices\* while the cursor is located at this
	!	field will display a list of valid Make classes.  Additional class codes may be
	!	added by pressing ^*F17\*.
	!	.lm -5
	!
	! Index:
	!	.x Make Class
	!
	!--
			MO_MAKE::CLASS = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"7;25", TEMP$, MO_MAKE::CLASS, MFLAG, &
				"'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF (MAIN_WINDOW(MO_MAIN_MAKECLASS.ID, "VX") = 1%)
				THEN
					MO_MAKE::CLASS = MO_MAKECLASS::CLASS
				END IF
				GOTO ReEnter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(MO_MAIN_MAKECLASS.ID, "M")
				MO_MAKE::CLASS = MO_MAKECLASS::CLASS
				GOTO ReEnter
			END IF

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(07) Cut Tubing\*
	!	.b
	!	.lm +5
	!	The ^*Cut Tubing\* field enters the cut tubing
	!	length (in inches - ie: xxx x/x) of the Make.  Eight (08) spaces are available
	!	for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Cut Tubing
	!
	!--
		MO_MAKE::TUBING = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
			"8;25", TEMP$, MO_MAKE::TUBING, MFLAG, &
			"'E", MVALUE)

		CASE 8%
	!++
	! Abstract:FLD008
	!	^*(08) Front Slant\*
	!	.b
	!	.lm +5
	!	The ^*Front Slant\* field enters the Front Slant
	!	of the Make (in degrees - ie: 3.5, 5.56 etc.).  Eight (08) spaces are available
	!	for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Front Slant
	!
	!--
			MO_MAKE::SLANT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"9;25", TEMP$, MO_MAKE::SLANT, MFLAG, &
				"'E", MVALUE)

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*(09) Overall Length\*
	!	.b
	!	.lm +5
	!	The ^*Overall Lenth\* field enters the overall
	!	length (in inches - ie: xxx x/x) of the cab of the Make. This
	!	field will accommodate eight (08) characters.
	!	.lm -5
	!
	! Index:
	!	.x Overall Length
	!
	!--
			MO_MAKE::OVERALL = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;25", TEMP$, MO_MAKE::OVERALL, MFLAG, &
				"'E", MVALUE)

		CASE 10%
	!++
	! Abstract:FLD010
	!	^*(10) Narrow Front\*
	!	.b
	!	.lm +5
	!	The ^*Narrow Front\* field enters the flag to
	!	indicate if the front of the bed of the cab is narrow.
	!	.b
	!	The only valid entries are Y (Yes) or N (No).
	!	.lm -5
	!
	! Index:
	!	.x Narrow Front
	!
	!--
			MO_MAKE::NFRONT = ENTR_3YESNO(SCOPE, SMG_WINDOW::WNUMBER, &
				"11;25", TEMP$, MO_MAKE::NFRONT, MFLAG, &
				"'E", MVALUE)

		CASE 11%
	!++
	! Abstract:FLD011
	!	^*(11) Narrow Back\*
	!	.b
	!	.lm +5
	!	The ^*Narrow Back\* field enters the flag to
	!	indicate if the back of the bed of the cab is narrow.
	!	.b
	!	The only valid entries are Y (Yes) or N (No).
	!	.lm -5
	!
	! Index:
	!	.x Narrow Back
	!
	!--
			MO_MAKE::NBACK = ENTR_3YESNO(SCOPE, SMG_WINDOW::WNUMBER, &
				"12;25", TEMP$, MO_MAKE::NBACK, MFLAG, &
				"'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		MO_MAIN_MAKE = 0%

		SELECT MLOOP

		CASE 1%
			IF MO_MAKE::MAKE = ""
			THEN
				MO_MAIN_MAKE = 1%
			END IF

		CASE 3%
			IF MO_MAKE::YEAR = ""
			THEN
				MO_MAIN_MAKE = 1%
			END IF

		CASE 4%
			IF MO_MAKE::MTYPE <> ""
			THEN
				MO_MAIN_MAKE = FUNC_TESTENTRY(SMG_WINDOW, &
					MO_MAKE::MTYPE, &
					MO_MAKETYPE::DESCR, &
					"MO", MLOOP, "PROG", &
					"Make Type", MO_MAIN_MAKETYPE.ID)
			ELSE
				MO_MAKETYPE::DESCR = ""
 !					STRING$(LEN(MO_MAKETYPE::DESCR), A" "B)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MO_MAKETYPE::DESCR, 5%, 35%,, SMG$M_BOLD)

		CASE 5%
			IF MO_MAKE::MSIZE <> ""
			THEN
				MO_MAIN_MAKE = FUNC_TESTENTRY(SMG_WINDOW, &
					MO_MAKE::MSIZE, &
					MO_MAKESIZE::DESCR, &
					"MO", MLOOP, "PROG", &
					"Make Size", MO_MAIN_MAKESIZE.ID)
			ELSE
				MO_MAKESIZE::DESCR = ""
 !					STRING$(LEN(MO_MAKESIZE::DESCR), A" "B)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MO_MAKESIZE::DESCR, 6%, 35%,, SMG$M_BOLD)

			SELECT MVALUE

			CASE "ADD"
				WHEN ERROR IN
					GET #MO_MAKE.CH%, &
						KEY #0% EQ MO_MAKE::MAKE + &
							MO_MAKE::YEAR + &
							MO_MAKE::MTYPE + &
							MO_MAKE::MSIZE, &
						REGARDLESS
				USE
					CONTINUE ExitProgram IF ERR = 155%
					EXIT HANDLER
				END WHEN

				MO_MAIN_MAKE = 2%
				CALL ENTR_3MESSAGE(SCOPE, "Record Already Exists", 1%)

			END SELECT

		CASE 6%
			IF MO_MAKE::CLASS <> ""
			THEN
				MO_MAIN_MAKE = FUNC_TESTENTRY(SMG_WINDOW, &
					MO_MAKE::CLASS, &
					MO_MAKECLASS::DESCR, &
					"MO", MLOOP, "PROG", &
					"Make Class",MO_MAIN_MAKECLASS.ID)
			ELSE
				MO_MAKECLASS::DESCR = ""
 !					STRING$(LEN(MO_MAKECLASS::DESCR), A" "B)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MO_MAKECLASS::DESCR, 7%, 35%,, SMG$M_BOLD)

		END SELECT

	!
	! Test option
	!
	CASE OPT_TESTOPT
		MO_MAIN_MAKE = 0%

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(4%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(MO_MAIN_MAKETYPE.ID, "Q0" + MO_MAKE::MTYPE) <> 1%
			THEN
				MO_MAKETYPE::DESCR = &
					STRING$(LEN(MO_MAKETYPE::DESCR), A"?"B)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MO_MAKETYPE::DESCR, 5%, 35%,, SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(5%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(MO_MAIN_MAKESIZE.ID, "Q0" + MO_MAKE::MSIZE) <> 1%
			THEN
				MO_MAKESIZE::DESCR = &
					STRING$(LEN(MO_MAKESIZE::DESCR), A"?"B)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MO_MAKESIZE::DESCR, 6%, 35%,, SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(6%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(MO_MAIN_MAKECLASS.ID, "Q0" + MO_MAKE::CLASS) <> 1%
			THEN
				MO_MAKECLASS::DESCR = &
					STRING$(LEN(MO_MAKECLASS::DESCR), A"?"B)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MO_MAKECLASS::DESCR, 7%, 35%,, SMG$M_BOLD)
		END IF

	!
	! Set MO_MAKE_OLD value
	!
20500	CASE OPT_SETOLD
		MO_MAKE_OLD = MO_MAKE

	!
	! Restore MO_MAKE_OLD value
	!
	CASE OPT_RESETOLD
		MO_MAKE = MO_MAKE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		MO_MAKE2 = MO_MAKE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		MO_MAKE = MO_MAKE2

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Make       Year Type Size Class Description"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "013,018,023,028,034"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = MO_MAKE::MAKE + " " + &
				MO_MAKE::YEAR  + " " + &
				MO_MAKE::MTYPE + "   " + &
				MO_MAKE::MSIZE + " " + &
				MO_MAKE::CLASS + "  " + &
				MO_MAKE::DESCR

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #MO_MAKE.CH%, &
				KEY #0% GE MO_MAKE::MAKE + "", &
				REGARDLESS

		CASE 1%
			FIND #MO_MAKE.CH%, KEY #1% &
				GE MO_MAKE::YEAR + MO_MAKE::MAKE, REGARDLESS

		CASE 2%
			FIND #MO_MAKE.CH%, KEY #2% &
				GE MO_MAKE::MTYPE + MO_MAKE::MAKE, REGARDLESS

		CASE 3%
			FIND #MO_MAKE.CH%, KEY #3% &
				GE MO_MAKE::MSIZE + MO_MAKE::MAKE, REGARDLESS

		CASE 4%
			FIND #MO_MAKE.CH%, KEY #4% &
				GE MO_MAKE::CLASS + MO_MAKE::MAKE, REGARDLESS

		END SELECT

	END SELECT

 ExitProgram:
	EXIT FUNCTION

29000	!
	! Trap Errors
	!
	ON ERROR GO BACK

29900	!
	! Handle no header for Line function here
	!
	CALL ENTR_3MESSAGE(SCOPE, "Sorry, but there is no current header item", 0%)

32767	END FUNCTION
	!+-+-+
	!++
	! Abstract:MODELS
	!	^*Manufacturing Order Make Master Model Attachments\*
	!	.b
	!	.lm +5
	!	The ^*Manufacturing Order Make Master Model Attachments\* program
	!	maintains the Model(s) attached to specific records of the Make Master.
	!	.lm -5
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Abstract:MODEL
	!	^*Model\*
	!	.b
	!	.lm +5
	!	The ^*Model\* portion
	!	maintains attachment of Models to a particular Make. The
	!	Model window is displayed by accessing the "Model" function.
	!	.b
	!	The ^*Model\* window contains the following fields:
	!	.lm 15
	!	.b
	!	.LIST 0,"*"
	!	.LE
	!	(01) Model
	!	.le
	!	Model Description
	!	.els
	!
	! Index:
	!	.x Maintenance>Make
	!	.x Make>Maintenance
	!
	!--
