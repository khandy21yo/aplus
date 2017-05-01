1	%TITLE "Set File Maintenance"
	%SBTTL "UTL_MAIN_SET"
	%IDENT "V3.6a Calico"

	FUNCTION LONG UTL_MAIN_SET(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	! ID:0189
	!
	! Abstract:HELP
	!	.p
	!	The primary purpose of the set routine is to allow the
	!	user to set defaults for fields in a program.  For example, to
	!	default a field in the GL chart program, the following would
	!	be entered.
	!	.lm +10
	!	.b
	!	.list 0,"*"
	!	.le
	!	(01) Program
	!	.le
	!	(02) Item######^*<field number>\*
	!	.le
	!	(03) System####^*<system id>\*
	!	.le
	!	(04) Hard/Soft#^*H\* or ^*S\*
	!	.le
	!	(05) Data######^*<default value>\*
	!	.le
	!	(06) Format####^*<format>\*
	!	.els
	!
	! Index:
	!	.x Set Default and Allow Entries>Table
	!	.x Table>Set Default and Allow Entries
	!	.x Default>Set>Table
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_MAIN_SET/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP UTL_MAIN_SET
	!	$ DELETE UTL_MAIN_SET.OBJ;*
	!
	! Author:
	!
	!	08/18/87 - Kevin Handy
	!
	! Modification history:
	!
	!	03/06/91 - Frank F. Starman
	!		Add Allow field and clean codes.
	!
	!	01/31/92 - Frank F. Starman
	!		Add a format field.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/30/96 - Kevin Handy
	!		Reformat source code
	!
	!	02/11/97 - Kevin Handy
	!		Modify view to display lines, format data
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/01/98 - Kevin Handy
	!		Make the item number part of the search entry
	!
	!	03/04/99 - Kevin Handy
	!		Change field 'PROGRAM' to 'PROGRAMNAME'
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

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_SET.HB"
	MAP (UTL_SET)	UTL_SET_CDD	UTL_SET
	MAP (UTL_SET_OLD)	UTL_SET_CDD	UTL_SET_OLD, UTL_SET2

	COM (CH_UTL_SET) &
		UTL_SET.CH%

	COM (TT_UTL_SET) &
		DEFTITLE$ = 20%, &
		DEFTYPE$(3%) = 20%

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Set File Maintenance"
		SMG_WINDOW::NHELP = "UTL_MAIN_SET"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 7%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Entity"
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%

		DEFTITLE$ = "Type   Description"
		DEFTYPE$(0%) = "3"
		DEFTYPE$(1%) = "F    Blank Field"
		DEFTYPE$(2%) = "H    Hard Default"
		DEFTYPE$(3%) = "S    Soft Default"

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

20075		SMG_WINDOW::CHAN  = UTL_SET.CH%

		WHEN ERROR IN
			RESET #UTL_SET.CH%
			GET #UTL_SET.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display the background
	!
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	4, 3, "(01) Entity", &
			5, 3, "(02) Item", &
			6, 3, "(03) System", &
			7, 3, "(04) Def Type", &
			8, 3, "(05) Allow", &
			9, 3, "(06) Data", &
			10, 3, "(07) Format", &
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
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!
	!	^*(01) Entity\*
	!	.p
	!	The ^*Entity\* field
	!	.p
	!	The field will accommodate up to thirty nine (39) alphanumeric
	!	characters.
	!
	! Index:
	!--

			UTL_SET::PROGRAMNAME = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;18", TEMP$, &
				UTL_SET::PROGRAMNAME, MFLAG, "'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!
	!	^*(02) Item\*
	!	.p
	!	The ^*Item\* field
	!	.p
	!	The field will accommodate up to six (6) alphanumeric
	!	characters.
	!
	! Index:
	!--
			UTL_SET::ITEM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;18", TEMP$, &
				UTL_SET::ITEM, MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!
	!	^*(03) System\*
	!	.p
	!	The ^*System\* field
	!	.p
	!	The field will accommodate up to two (2) alphanumeric
	!	characters.
	!
	! Index:
	!--

			UTL_SET::SYSTEM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;18", TEMP$, &
				UTL_SET::SYSTEM, MFLAG, "'E", MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Default Type\*
	!	.p
	!	The ^*Default Type\* field
	!
	! Index:
	!
	!--

			UTL_SET::HARD = EDIT$(ENTR_3STRINGLIST(SCOPE, SMG_WINDOW::WNUMBER, &
					"07;18", TEMP$, UTL_SET::HARD, &
					MFLAG, "'", MVALUE, DEFTYPE$(), &
					DEFTITLE$, "005"), -1%)

		CASE 5%

	!++
	! Abstract:FLD005
	!
	!	^*(05) Allow Undefined\*
	!	.p
	!	The ^*Allow Undefined\* field
	!
	! Index:
	!--

			UTL_SET::ALLOWUND = ENTR_3YESNO(SCOPE, SMG_WINDOW::WNUMBER, &
				"8;18", TEMP$, UTL_SET::ALLOWUND, MFLAG, "'", MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!
	!	^*(06) Data\*
	!	.p
	!	The ^*Data\* field
	!	.p
	!	The field will accommodate up to thirty (30) alphanumeric
	!	characters.
	!
	! Index:
	!--

			UTL_SET::SDATA = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"9;18", TEMP$, &
				UTL_SET::SDATA, MFLAG, "'E", MVALUE)

		CASE 7%
	!++
	! Abstract:FLD007
	!
	!	^*(07) Data Format\*
	!	.p
	!	The ^*Data Format\* field will accommodate up to thirty (30) alphanumeric
	!	characters.
	!
	! Index:
	!--

			UTL_SET::FDATA = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;18", TEMP$, &
				UTL_SET::FDATA, MFLAG, "'E", MVALUE)


		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		UTL_MAIN_SET = 0%

		SELECT MLOOP

		CASE 1%
			UTL_MAIN_SET = 1% IF UTL_SET::PROGRAMNAME = ""

		END SELECT

	!
	! Set UTL_SET_OLD value
	!
20500	CASE OPT_SETOLD
		UTL_SET_OLD = UTL_SET

	!
	! Restore UTL_SET_OLD value
	!
	CASE OPT_RESETOLD
		UTL_SET = UTL_SET_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		UTL_SET2 = UTL_SET

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		UTL_SET = UTL_SET2
		UTL_SET::UNUSED = ""
	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Entity          Item   System H A Data"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "018,025,032,034,036,057"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = LEFT(UTL_SET::PROGRAMNAME, 15%) + " " + &
				UTL_SET::ITEM + " " + &
				UTL_SET::SYSTEM + "     " + &
				UTL_SET::HARD + " " + &
				UTL_SET::ALLOWUND + " " + &
				LEFT(UTL_SET::SDATA, 20%) + " " + &
				UTL_SET::FDATA

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		FIND #UTL_SET.CH%, &
			KEY #0% GE UTL_SET::PROGRAMNAME + UTL_SET::ITEM, &
			REGARDLESS

	END SELECT

	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
