1	%TITLE "String Print File Maintenance"
	%SBTTL "UTL_MAIN_STRING_PRINT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG UTL_MAIN_STRING_PRINT(CDD_WINDOW_CDD SMG_WINDOW, &
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
	! ID:0193
	!
	! Abstract:HELP
	!	.p
	!	The ^*String Print File Maintenance\* file maintains
	!	a variety of report string instructions which cause reports to be printed
	!	in in a predetermined sequence.
	!
	! Index:
	!	.x Maintain>String
	!	.x String>Maintain
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_MAIN_STRING_PRINT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP UTL_MAIN_STRING_PRINT
	!	$ DELETE UTL_MAIN_STRING_PRINT.OBJ;*
	!
	!
	! Author:
	!
	!	01/13/89 - Kevin Handy
	!
	! Modification history:
	!
	!	05/13/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	06/16/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/30/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/09/99 - Kevin Handy
	!		Lose chunk of code in Elp001: (Dead Code)
	!
	!	11/30/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Includes
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	!
	! Maps and CDD
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_STRING_PRINT.HB"
	MAP (UTL_STRING_PRINT)		UTL_STRING_PRINT_CDD	UTL_STRING_PRINT
	MAP (UTL_STRING_PRINT_OLD)	UTL_STRING_PRINT_CDD	UTL_STRING_PRINT_OLD, UTL_STRING_PRINT2

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.HB"
	MAP (UTL_REPORT)		UTL_REPORT_CDD		UTL_REPORT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_SYSTEM.HB"
	MAP (UTL_SYSTEM)		UTL_SYSTEM_CDD		UTL_SYSTEM

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_UTL_STRING_PRINT) &
		UTL_STRING_PRINT.CH%, &
		UTL_STRING_PRINT.READONLY%

	COM (TT_UTL_MAIN_STRING_PRINT) &
		ECTITLE$ = 32%, &
		EC$(6%) = 32%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	MAIN_WINDOW
	EXTERNAL LONG	FUNCTION	FUNC_TESTENTRY

	ON ERROR GOTO 29000

	%PAGE

	SELECT MOPTION

	!***************************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!***************************************************************
	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Print String Maintenance"
		SMG_WINDOW::NHELP = "UTL_MAIN_STRING_PRINT"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 16%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "String"
			SMG_WINDOW::KFIELD(0%, 0%) = 3%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
			SMG_WINDOW::KFIELD(0%, 3%) = 3%

		!
		! List of types
		!
		ECTITLE$ = "Flag Description"
		EC$(0%) = "3"
		EC$(1%) = "I    Ignore field"
		EC$(2%) = "S    Set (Force) field"
		EC$(3%) = "Q    Query for field"

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Get info required for main file
		!
		IF UTL_STRING_PRINT.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF UTL_STRING_PRINT.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_STRING_PRINT.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			UTL_MAIN_STRING_PRINT = ERR
			CONTINUE 770
		END WHEN

		UTL_STRING_PRINT.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_STRING_PRINT.OPN"
		USE
			UTL_MAIN_STRING_PRINT = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		UTL_STRING_PRINT.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(UTL_STRING_PRINT.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = UTL_STRING_PRINT.CH%

		WHEN ERROR IN
			RESET #UTL_STRING_PRINT.CH%
			GET #UTL_STRING_PRINT.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	%PAGE

20100	!***************************************************************
	! Display the background
	!
	! This option is used to display the background information
	! on the screen.  It must first clear any junk on the screen,
	! and then write the background onto it.
	!***************************************************************
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	1,  1, "(01) System", &
			2,  1, "(02) Grouping", &
			3,  1, "(03) Sequence", &
			4,  1, "(04) Report Number", &
			5,  1, "(05) Title", &
			6,  1, "(06) Output to", &
			9,  1, "(07) 01", &
			10,  1, "(08) 02", &
			11,  1, "(09) 03", &
			12,  1, "(10) 04", &
			13,  1, "(11) 05", &
			14,  1, "(12) 06", &
			15,  1, "(13) 07", &
			16,  1, "(14) 08", &
			17,  1, "(15) 09", &
			18,  1, "(16) 10", &
			8,  1, "     Item Flag Code   Text                 Report Descr", &
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

20200	!***************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display
	! data, set defaults, and return the data back according to
	! MFLAG.
	!***************************************************************
	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 ReEnter:	SCOPE::SCOPE_EXIT = 0%

 Eloop:		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!
	!	^*(01) System\*
	!	.p
	!	The ^*System\* field is used to specify which system the string of
	!	reports belongs to, ie. ^*GL\*, ^*PR\*, ^*AP\*, ^*AR\*.
	!
	! Index:
	!	.x String print>System
	!	.x System>String print
	!--

			UTL_STRING_PRINT::SYSTEM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"1;22", TEMP$, &
				UTL_STRING_PRINT::SYSTEM, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(UTL_MAIN_SYSTEM.ID, "V0  ") = 1%)
				THEN
					UTL_STRING_PRINT::SYSTEM = &
						UTL_SYSTEM::SYSTEM
				END IF
				GOTO Reenter
			END IF

		CASE 2%

	!++
	! Abstract:FLD002
	!
	!	^*(02) Grouping\*
	!	.p
	!	The ^*Grouping\* field is used to break report strings into smaller
	!	groups.  For example, you may have a section ^*LEDGER\* for ledgers,
	!	^*WRKSHT\* for worksheets, etc.
	!
	! Index:
	!	.x String print>Grouping
	!	.x Grouping>String print
	!--

			UTL_STRING_PRINT::GROUPING = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;22", TEMP$, &
				UTL_STRING_PRINT::GROUPING, MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Sequence\*
	!	.p
	!	The ^*Sequence\* field assigns a unique identifier in
	!	a print string group.  The list of reports will show up in
	!	alphabetical order in the group according to this field.
	!	.note
	!	It is recommended that the field contains some meaning if
	!	possible, such as store number when the report is for a
	!	particular store.
	!	.end note
	!
	! Index:
	!	.x Sequence>Print string
	!	.x Print string>Sequence
	!
	!--

			UTL_STRING_PRINT::REPSEQ = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"3;22", TEMP$, &
				UTL_STRING_PRINT::REPSEQ, MFLAG, "'E", MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!
	!	^*(04) Report number\*
	!	.p
	!	The ^*Report number\* field is used to specify which report is to be
	!	printed when this record is selected.
	!
	! Index:
	!	.x String print>Report number
	!	.x Report number>String print
	!--

			UTL_STRING_PRINT::REPNUM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;22", TEMP$, &
				UTL_STRING_PRINT::REPNUM, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(UTL_MAIN_PRINT.ID, "VX") = 1%)
				THEN
					UTL_STRING_PRINT::REPNUM = &
						UTL_REPORT::REPNUM
				END IF
				GOTO ELoop
			END IF

		CASE 5%

	!++
	! Abstract:FLD005
	!
	!	^*(05) Title\*
	!	.p
	!	The ^*Title\* field is displayed for this record in the string print
	!	program.
	!
	! Index:
	!	.x Title>String print
	!	.x String print>Title
	!--

			UTL_STRING_PRINT::TITLES = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;22", TEMP$, &
				UTL_STRING_PRINT::TITLES, MFLAG, "'E", MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Output to\*
	!	.p
	!	The ^*Output to\* field defines where the report
	!	program can be found
	!
	! Index:
	!	.x Device
	!
	!--

			UTL_STRING_PRINT::OUTDEV = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;22", TEMP$, &
				UTL_STRING_PRINT::OUTDEV, MFLAG, "'E", MVALUE)

		CASE 7% TO 16%

	!++
	! Abstract:FLD007
	!	^*(07)-(16) Line\*
	!	.P
	!	The ^*flags\* section allows the following flags:
	!	.b
	!	.list 0,"*"
	!	.le
	!	S - Force the value in the settings to this value.
	!	Anything that was originally there is overwritten.
	!	.le
	!	Q - Query for this value. This field will cause the string
	!	print program to pause and ask for something in this field.
	!	.le
	!	I - Ignore this field. This causes the string print to use
	!	whatever happens to be in the settings field at the
	!	time the report is run.
	!	.els
	!	.p
	!	The ^*Codes\* section is used to assign a code for those items
	!	being queried.
	!	This field is used to identify what items need to be asked only
	!	once during the string print, such as GL Period.
	!	.p
	!	The ^*Text\* Field is used for two purposes.
	!	.list
	!	.le
	!	When the flag is set to "Q", this field is used to give the
	!	title of the field when the string print program queries
	!	for that field.
	!	.le
	!	When the flag is set to "S", this is the data to be forced into
	!	that field.
	!	.els
	!
	! Index:
	!	.x Line
	!
	!--

		SCOPE::PRG_ITEM = "FLD007"

			LINE$ = NUM1$(MLOOP + 2%)
			SCOPE::SCOPE_EXIT = 0%

 Elp001:
			UTL_STRING_PRINT::FLAGS(MLOOP - 7%) = &
				EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, LINE$ + ";11", &
				TEMP$ + " (I/S/Q)", &
				UTL_STRING_PRINT::FLAGS(MLOOP - 7%), MFLAG, &
				"'", MVALUE, EC$(), ECTITLE$, "007"), -1%)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_DOWN, 0%, 10%, 12%, 13%, SMG$K_TRM_DO
				GOTO Elp002

			CASE ELSE
				GOTO Elp004

			END SELECT

 !			IF (MFLAG AND 1%) = 0%
 !			THEN
 !				SELECT UTL_STRING_PRINT::FLAGS(MLOOP - 1%)
 !
 !				CASE "S"
 !					UTL_STRING_PRINT::CODES(MLOOP - 7%) = ""
 !					UTL_STRING_PRINT::DESCRS(MLOOP - 7%) = ""
 !
 !				CASE "I"
 !					UTL_STRING_PRINT::CODES(MLOOP - 7%) = ""
 !					UTL_STRING_PRINT::DESCRS(MLOOP - 7%) = ""
 !
 !				CASE "Q"
					! UTL_STRING_PRINT::CODES(MLOOP - 6%) = ""
 !					UTL_STRING_PRINT::DESCRS(MLOOP - 6%) = &
 !						UTL_REPORT::DESCR(MLOOP - 6%)
 !
 !				END SELECT
 !			END IF

 Elp002:
			UTL_STRING_PRINT::CODES(MLOOP - 7%) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				LINE$ + ";16", TEMP$, &
				UTL_STRING_PRINT::CODES(MLOOP - 7%), &
				MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO Elp001

			CASE SMG$K_TRM_DOWN, 0%, 10%, 12%, 13%, SMG$K_TRM_DO
				GOTO Elp003

			CASE ELSE
				GOTO Elp004

			END SELECT

 Elp003:
			UTL_STRING_PRINT::DESCRS(MLOOP - 7%) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				LINE$ + ";23", TEMP$, &
				UTL_STRING_PRINT::DESCRS(MLOOP-7%), MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO Elp001

			END SELECT

			UTL_REPORT::DESCR(MLOOP - 7%) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				LINE$ + ";44", TEMP$, &
				UTL_REPORT::DESCR(MLOOP - 7%), 1%, "'E", MVALUE)
 Elp004:

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		UTL_MAIN_STRING_PRINT = 0%

		SELECT MLOOP

		CASE 1%
			IF UTL_STRING_PRINT::SYSTEM = ""
			THEN
				UTL_MAIN_STRING_PRINT = 1%
			END IF

		CASE 2%
			IF UTL_STRING_PRINT::GROUPING = ""
			THEN
				UTL_MAIN_STRING_PRINT = 1%
			END IF

		CASE 3%
			IF UTL_STRING_PRINT::REPSEQ = ""
			THEN
				UTL_MAIN_STRING_PRINT = 1%
			END IF

		CASE 4%
			!
			! Is the input defined?
			!
			UTL_MAIN_STRING_PRINT = FUNC_TESTENTRY(SMG_WINDOW, &
				UTL_STRING_PRINT::REPNUM, &
				UTL_REPORT::REPDES, &
				"UTL", MLOOP, "PRG", &
				"Report", UTL_MAIN_PRINT.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(UTL_REPORT::REPDES, 30%), &
				4%, 51%, , SMG$M_BOLD)

			!
			! Force in some defaults
			!
			UTL_STRING_PRINT::TITLES = UTL_REPORT::REPDES &
				IF UTL_STRING_PRINT::TITLES = ""

			FOR LOOP% = 0% TO 9%
				IF UTL_REPORT::DESCR(LOOP%) = ""
				THEN
					UTL_STRING_PRINT::FLAGS(LOOP%) = "I"
					UTL_STRING_PRINT::CODES(LOOP%) = ""
					UTL_STRING_PRINT::DESCRS(LOOP%) = ""
				ELSE
					UTL_STRING_PRINT::FLAGS(LOOP%) = "S"
					UTL_STRING_PRINT::CODES(LOOP%) = ""
					UTL_STRING_PRINT::DESCRS(LOOP%) = &
						UTL_REPORT::OPTDEF(LOOP%)
				END IF

			NEXT LOOP%

			!
			! Display forced settings
			!
			FOR LOOP% = 7% TO 16%
				LINE$ = NUM1$(LOOP% + 2%)

				JUNK$ = ENTR_3STRING(SCOPE, &
					SMG_WINDOW::WNUMBER, LINE$ + ";11", &
					TEMP$, UTL_STRING_PRINT::FLAGS(LOOP% - 7%), 1%, &
					"'", MVALUE)

				JUNK$ = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
					LINE$ + ";16", TEMP$, &
					UTL_STRING_PRINT::CODES(LOOP% - 7%), 1%, "'E", MVALUE)

				JUNK$ = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
					LINE$ + ";23", TEMP$, &
					UTL_STRING_PRINT::DESCRS(LOOP% - 7%), 1%, "'E", MVALUE)

				JUNK$ = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
					LINE$ + ";44", TEMP$, &
					UTL_REPORT::DESCR(LOOP% - 7%), 1%, "'E", MVALUE)
			NEXT LOOP%

		END SELECT

	!
	! Display additional information
	!
	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(4%) AND 2%) = 0%
		THEN
			UTL_MAIN_STRING_PRINT = 0%

			!
			! Is the input defined?
			!
			IF MAIN_WINDOW(UTL_MAIN_PRINT.ID, &
				"Q0" + UTL_STRING_PRINT::REPNUM) <> 1%
			THEN
				UTL_REPORT::REPDES = "????????????????????"
			END IF

			!
			! Print chart description
			!
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(UTL_REPORT::REPDES, 30%), &
				4%, 51%, , SMG$M_BOLD)
		END IF

		!
		! Display forced settings
		!
		FOR LOOP% = 7% TO 16%
			LINE$ = NUM1$(LOOP% + 2%)

			JUNK$ = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, LINE$ + ";11", &
				TEMP$, UTL_STRING_PRINT::FLAGS(LOOP% - 7%), &
				1%, "'", MVALUE)

			JUNK$ = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				LINE$ + ";16", TEMP$, &
				UTL_STRING_PRINT::CODES(LOOP% - 7%), 1%, &
				"'E", MVALUE)

			JUNK$ = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				LINE$ + ";23", TEMP$, &
				UTL_STRING_PRINT::DESCRS(LOOP% - 7%), &
				1%, "'E", MVALUE)

			JUNK$ = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				LINE$ + ";44", TEMP$, &
				UTL_REPORT::DESCR(LOOP% - 7%), 1%, "'E", MVALUE)
		NEXT LOOP%

	!
	! Set UTL_STRING_PRINT_OLD value
	!
20500	CASE OPT_SETOLD
		UTL_STRING_PRINT_OLD = UTL_STRING_PRINT

	!
	! Restore UTL_STRING_PRINT_OLD value
	!
	CASE OPT_RESETOLD
		UTL_STRING_PRINT = UTL_STRING_PRINT_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		UTL_STRING_PRINT2 = UTL_STRING_PRINT

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		UTL_STRING_PRINT = UTL_STRING_PRINT2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  System Groups RepSeq Title"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "009,016,023"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = &
				UTL_STRING_PRINT::SYSTEM + " " + &
				UTL_STRING_PRINT::GROUPING + " " + &
				UTL_STRING_PRINT::REPSEQ + " " + &
				UTL_STRING_PRINT::TITLES

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #UTL_STRING_PRINT.CH%, &
				KEY #0% GE UTL_STRING_PRINT::SYSTEM + &
					UTL_STRING_PRINT::GROUPING + &
					UTL_STRING_PRINT::REPSEQ, &
				REGARDLESS

		END SELECT

	END SELECT

	EXIT FUNCTION

29000	!***************************************************************
	! Trap errors
	!***************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
