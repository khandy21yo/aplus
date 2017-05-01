1	%TITLE "Financial Statement Layout Maintenance"
	%SBTTL "GL_MAIN_LAYOUT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG GL_MAIN_LAYOUT(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	! ID:1030
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The purpose of the ^*Layout File\* is to define the following
	!	information relative to each financial statement:
	!	.b
	!	.lm +5
	!	Define the key for each statement.
	!	.br
	!	Provide a brief description of a statement.
	!	.br
	!	Determine the exact title to be printed on a
	!	statement.
	!	.br
	!	Establish the file name by which a statement
	!	will be identified in the Command File.
	!	.br
	!	Identify the type of statement.
	!	.br
	!	Designate various input codes.
	!	.lm -5
	!
	! Index:
	!	.x Financial Statement>Layout File
	!	.x Layout File>Financial Statement
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_MAIN_LAYOUT/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN GL_MAIN_LAYOUT
	!	$ DELETE GL_MAIN_LAYOUT.OBJ;*
	!
	! Author:
	!
	!	03/02/87 - Kevin Handy
	!
	! Modification history:
	!
	!	05/13/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	06/23/88 - Aaron Redd
	!		Split into two modules (_MAST_ and _MAIN_) in order
	!		to meet standardization requirements.
	!
	!	10/10/88 - Kevin Handy
	!		Added four more lines of title (A-D).
	!
	!	05/20/91 - Frank F. Starman
	!		Add option OPT_DISPLAY.
	!
	!	06/18/92 - Kevin Handy
	!		Modified to GOTO 20420 on all errors occuring at
	!		20400, instead of just error 5.  It was crashing
	!		with no way to fix.
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/22/93 - Kevin Handy
	!		Modified input of command file name so that it
	!		will be upper case always.
	!
	!	12/14/93 - Kevin Handy
	!		Added option "ediT" to drop into editor for command
	!		file instead of forcing the user to switch to another
	!		program to edit it.
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico Coding standards.
	!		Lose extra parameter in call to ASSG_FREECHANNEL.
	!
	!	05/02/95 - Kevin Handy
	!		Added read of device name in OPT_DISPLAY section,
	!		so would pull up descriptions across directories.
	!
	!	01/29/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to SPACE$(...) in
	!		several places.
	!
	!	10/18/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/23/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/19/97 - Kevin Handy
	!		Add a whole bunch of spaces to the code so I
	!		could read it.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	03/09/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	11/10/2000 - Kevin Handy
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
	! External functions
	!
	EXTERNAL LONG    FUNCTION EDT$EDIT

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[GL.OPEN]GL_FINSTA.HB"
	MAP	(GL_FINSTA)	GL_FINSTA_CDD	GL_FINSTA
	MAP	(GL_FINSTA_OLD)	GL_FINSTA_CDD	GL_FINSTA_OLD, GL_FINSTA2

	!
	! Common areas
	!
	! These areas store information that is re-used between calls
	! to these functions.
	!
	COM (CH_GL_FINSTA) &
		GL_FINSTA.CH%, &
		GL_FINSTA.READONLY%

	COM (TT_GL_FINSTA) &
		STATYPE$ = 25%, &
		STATYPE$(3%) = 25%, &
		STACMD$  = 25%, &
		STACMD$(1000%) = 36%

	!
	! Declare some variables
	!
	DECLARE LONG XPOS, YPOS

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
		SMG_WINDOW::DESCR = "Financial Statement Layout Maintenance"
		SMG_WINDOW::NHELP = "GL_MAIN_LAYOUT"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 17%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Prompt"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

		!
		! Define statement types
		!
		STATYPE$ = "TYPE  STATEMENT"
		STATYPE$(0%) = "3"
		STATYPE$(1%) = "O     Operational"
		STATYPE$(2%) = "C     Cash Flow"
		STATYPE$(3%) = "W     Working Capital"

		!
		! Look up device
		!
		CALL READ_DEVICE("GL_FINCMD", GL_FINCMD.DEV$, STAT%)

		!
		! Define statement command files
		!
		STACMD$ = "COMMAND FILES"

		!
		! Do a directory of all command files
		!
		CALL FIND_FILE( GL_FINCMD.DEV$ + "*.FS", STACMD$(), &
			16%, "", "")

700		!
		! Declare channels
		!
		IF GL_FINSTA.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF GL_FINSTA.READONLY%
			GOTO 790
		END IF

		IF GL_FINSTA.CH%
		THEN
			GL_MAIN_LAYOUT = 1%
			GOTO ExitFunction
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_FINSTA.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			GL_MAIN_LAYOUT = ERR
			CONTINUE 770
		END WHEN

		GL_FINSTA.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_FINSTA.OPN"
		USE
			GL_MAIN_LAYOUT = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		GL_FINSTA.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(GL_FINSTA.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = GL_FINSTA.CH%
		WHEN ERROR IN
			RESET #GL_FINSTA.CH%
			GET #GL_FINSTA.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Modify the menu
	!
	CASE OPT_OPTLIST
		MVALUE = MVALUE + " ediT"

	!
	! Optional menu items
	!
5000	CASE OPT_MOREMENU

		SELECT SCOPE::PRG_ITEM

	!++
	! Abstract:EDIT
	!	^*Contact\*
	!	.b
	!	.lm +5
	!	^*Contact\* contains information concerning the person the company
	!	will reach regarding questions or problems with that particular vendor.
	!	.lm -5
	!
	! Index:
	!	.x Contact>Customer Address Maintenance
	!	.x Customer Address Maintenance>Contact
	!
	!--
		!
		! Line option
		!
		CASE "ediT"
			TEMPFILE$ = TRM$(GL_FINSTA::CMDFIL)

			IF TEMPFILE$ <> ""
			THEN
				!
				! Append a ".fs" to file name
				!
				IF INSTR(1%, TEMPFILE$, ".FS") = 0%
				THEN
					TEMPFILE$ = TEMPFILE$ + ".FS"
				END IF

				!
				! Append device name to file
				!
				CALL READ_DEVICE("GL_FINCMD", &
					GL_FINCMD.DEV$, STAT%)
				TEMPFILE$ = GL_FINCMD$.DEV$ + TEMPFILE$

				!
				! Call editor
				!
				CALL ENTR_3MESSAGE(SCOPE, &
					"Entering the editor 'EDT' " + &
					TEMPFILE$, 1%)

				ST% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 0%)
				ST% = EDT$EDIT(TEMPFILE$,,,,,,,)
				SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)
				SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)

				CALL ENTR_3MESSAGE(SCOPE, "", 1%)

			END IF

		END SELECT

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

		DATA	2, 10, "(01) Prompt", &
			3, 10, "(02) Description", &
			4, 10, "(03) Report Title", &
			5, 10, "(04) Title A", &
			6, 10, "(05) Title B", &
			7, 10, "(06) Title C", &
			8, 10, "(07) Title D", &
			9, 10, "(08) Command File", &
			10, 10, "(09) Type", &
			11, 10, "(10) Input 1", &
			12, 10, "(11) Input 2", &
			13, 10, "(12) Input 3", &
			14, 10, "(13) Input 4", &
			15, 10, "(14) Input 5", &
			16, 10, "(15) Input 6", &
			17, 10, "(16) Input 7", &
			18, 10, "(17) Input 8", &
			0,  0, ""

		RESTORE

		READ XPOS, YPOS, XSTR$
		I% = 0%
		WHILE (XPOS <> 0%)
			I% = I% + 1%
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				XSTR$, XPOS, YPOS) &
				IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%
			READ XPOS, YPOS, XSTR$
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
		TEMP$ = TRM$(SCOPE::PRG_ITEM)

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	.x Financial Statement>Layout>Prompt
	!	^*(01) Prompt\*
	!	.b
	!	.lm +5
	!	This is a key field.  The Prompt for a balance sheet
	!	could be ^*BAL\* or ^*1000\*, while the Prompt for an income
	!	statement could be ^*INC\* or ^*2000\*. For a consolidated
	!	income statement and several subsidiary income statements,
	!	the Prompts might be ^*INC0\*, ^*INC1\*, ^*INC2\* and ^*INC3\* or
	!	^*2000\*, ^*2010\*, ^*2020\*, and ^*2030\*.
	!	.b
	!	Four spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Layout>Prompt>Financial Statement
	!
	!--

			GL_FINSTA::PROMPT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"2;28", TEMP$, &
				GL_FINSTA::PROMPT, MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	.x Description>Financial Statement
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	This field will contain a Financial Statement Description.
	!	.b
	!	The description could be simply ^*INCOME STATEMENT\*, or more
	!	explicitly, ^*INC STATEMENT W/ PREVIOUS YR COMPAR\*.
	!	.b
	!	Thirty spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Layout File>Financial Statement>Description
	!	.x Financial Statement>Description
	!
	!--

			GL_FINSTA::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"3;28", TEMP$, &
				GL_FINSTA::DESCR, MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	.x Financial Statement>Title
	!	^*(03) Report Title\*
	!	.b
	!	.lm +5
	!	The data entered in this field determines the title which
	!	will be printed on the hard copy financial statement.
	!	.b
	!	Example: ^*COMPARATIVE STATEMENT OF INCOME - CONSOLIDATED\*
	!	.b
	!	The field will accommodate 50 characters.
	!	.lm -5
	!
	! Index:
	!	.x Title>Financial Statement
	!	.x Financial Statement>Report Title
	!	.x Report Title>Financial Statement
	!
	!--

			GL_FINSTA::REPTITLE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"4;28", TEMP$, &
				GL_FINSTA::REPTITLE, MFLAG, "'E", MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	.x Financial Statement>Title>A
	!	^*(04) Title A\*
	!	.b
	!	.lm +5
	!	The data entered in this field determines one title which
	!	will be printed on the hard copy financial statement.
	!	.b
	!	Example: ^*COMPARATIVE STATEMENT OF INCOME BETWEEN PERIODS\*
	!	.b
	!	Fifty spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Title>Financial Statement
	!
	!--

			GL_FINSTA::REPTITLEA = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"5;28", TEMP$, &
				GL_FINSTA::REPTITLEA, MFLAG, "'E", MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!	.x Financial Statement>Title>B
	!	^*(05) Title B\*
	!	.b
	!	.lm +5
	!	The data entered in this field determines one title which
	!	will be printed on the hard copy financial statement.
	!	.b
	!	Example: ^*PERIODS COMPARED- ONE AND TEN\*
	!	.b
	!	Fifty spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Title>Financial Statement
	!
	!--

			GL_FINSTA::REPTITLEB = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"6;28", TEMP$, &
				GL_FINSTA::REPTITLEB, MFLAG, "'E", MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	.x Financial Statement>Title>C
	!	^*(06) Title C\*
	!	.b
	!	.lm +5
	!	The data entered in this field determines one title which
	!	will be printed on the hard copy financial statement.
	!	.b
	!	Example: ^*ISSUED FOR FINANCIAL BOARD MEETING\*
	!	.b
	!	Fifty spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Title>Financial Statement
	!
	!--

			GL_FINSTA::REPTITLEC = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"7;28", TEMP$, &
				GL_FINSTA::REPTITLEC, MFLAG, "'E", MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	.x Financial Statement>Title>D
	!	^*(07) Title D\*
	!	.b
	!	.lm +5
	!	The data entered in this field determines one title which
	!	will be printed on the hard copy financial statements.
	!	.b
	!	Example: ^*INFORMATION FOR STOCKHOLDERS\*
	!	.b
	!	Fifty spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Title>Financial Statement
	!
	!--

			GL_FINSTA::REPTITLED = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"8;28", TEMP$, &
				GL_FINSTA::REPTITLEC, MFLAG, "'E", MVALUE)

		CASE 8%

	!++
	! Abstract:FLD008
	!	.x Financial Statement>Command File
	!	^*(08) Command File\*
	!	.b
	!	.lm +5
	!	This field is used to define which financial command file will be
	!	used to print this financial report.
	!	.b
	!	.lm +5
	!	Example: A file name for a balance sheet could be
	!	designated as ^*BS\*; income statement, ^*INCOME\*;
	!	sales and cost of sales, ^*SALES\*; expense schedule,
	!	^*SCHD\*; and schedule of dollar and unit sales, ^*SCHGP\*.
	!	.b
	!	.lm -5
	!	Thirty spaces are available for the entry.
	!	.b
	!	Pressing ^*List Choices\* will display a list of valid command
	!	file names.
	!	.lm -5
	!
	! Index:
	!	.x Command File>Financial Statement
	!	.x Financial Statement>File>Command
	!	.x File>Command>Financial Statement
	!
	!--

			GL_FINSTA::CMDFIL = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"9;28", TEMP$, GL_FINSTA::CMDFIL, &
				MFLAG OR 16%, "'E", MVALUE, &
				STACMD$(), STACMD$, ""), 32%)

		CASE 9%

	!++
	! Abstract:FLD009
	!	.x Type>Financial Statement
	!	^*(09) Type\*
	!	.b
	!	.lm +5
	!	The ^*Type\* field must be entered with
	!	a valid code.
	!	.b
	!	Valid codes are listed below:
	!	.TABLE 3,25
	!	.te
	!	^*C\* - Cash flow statement
	!	.TE
	!	^*W\* - Working capital
	!	.TE
	!	^*O\* - All other statements
	!	.END TABLE
	!	If an invalid character is typed, a message will appear at the bottom of the
	!	screen indicating so.
	!	.b
	!	This type indicates to the system which financial code to
	!	use from the chart of accounts. For example, if the 'O' type
	!	is selected, the system will use the ^*Bal/inc Code\* from
	!	the chart of accounts. If the "W" type is used, the system
	!	will use the ^*Work Capital\* code. If the "C" type is used,
	!	the system will use the ^*Cash Flow\* code.
	!	.lm -5
	!
	! Index:
	!	.x Financial Statement>Type
	!
	!--

			GL_FINSTA::FINTYPE = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"10;28", TEMP$, GL_FINSTA::FINTYPE, &
				MFLAG, "'", MVALUE, STATYPE$(), &
				STATYPE$, "005"), 32%)

		CASE 10% TO 17%

	!++
	! Abstract:FLD010
	!	^*(10)-(17) Input 1-8\*
	!	.b
	!	.lm +5
	!	The ^*Input\* fields are used to define conditions for selection of certain
	!	accounts such as locations, groups, departments, divisions, etc., for the
	!	creation of individual and combined financial statements.
	!	.b
	!	.lm 10
	!	Example: If the code numbers were the following:
	!	.b
	!	.lm 15
	!	11900
	!	.br
	!	12900
	!	.br
	!	14900
	!	.br
	!	15909
	!	.br
	!	16909
	!	.lm -5
	!	.b
	!	The Inputs were the following:
	!	.lm 15
	!	.b
	!	Input 1 ??
	!	.br
	!	Input 2 _*_*9
	!	.lm -5
	!	.b
	!	In this case the search would extract all accounts coded with a number
	!	ending in a 9, as the other numbers are not relevant.
	!
	! Index:
	!	.x Input>Financial Statement
	!	.x Financial Statement>Input
	!
	!--

			GL_FINSTA::FINCMD(MLOOP - 9%) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				NUM1$(MLOOP + 1%) + ";28", TEMP$, &
				GL_FINSTA::FINCMD(MLOOP - 9%), &
				MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

20300	!******************************************************************
	! Test values
	!******************************************************************
	CASE OPT_TESTENTRY
		GL_MAIN_LAYOUT = 0%

		SELECT MLOOP

		CASE 1%
			IF GL_FINSTA::PROMPT = ""
			THEN
				!
				! Don't allow blank Prompts
				!
				GL_MAIN_LAYOUT = 1%
			ELSE
				!
				! Don't allow duplicates
				!
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #GL_FINSTA.CH%, &
							KEY #0% EQ GL_FINSTA::PROMPT + "", &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					GL_MAIN_LAYOUT = 2%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Record Already Exists", 1%)
				END IF
			END IF

		END SELECT

20400	CASE OPT_DISPLAY
		LOOP% = 0%
		CALL ASSG_CHANNEL(FS.CH%, STAT%)

		IF INSTR(1%, GL_FINSTA::CMDFIL, ".")
		THEN
			FILE.FS$ = GL_FINSTA::CMDFIL
		ELSE
			FILE.FS$ = GL_FINSTA::CMDFIL + ".FS"
		END IF

		CALL READ_DEVICE("GL_FINCMD", GL_FINCMD.DEV$, STAT%)

		WHEN ERROR IN
			OPEN GL_FINCMD.DEV$ + FILE.FS$ FOR INPUT AS FILE FS.CH%, &
				RECORDSIZE 132%, &
				ACCESS READ, &
				ALLOW MODIFY
		USE
			CONTINUE 20420
		END WHEN

20410		WHEN ERROR IN
			LINPUT #FS.CH%, TEXT$
		USE
			CONTINUE 20420 IF ERR = 11%
			EXIT HANDLER
		END WHEN

		GOTO 20420 IF INSTR(1%, TEXT$, "INPUT") = 0%
		FIRST% = INSTR(1%, TEXT$, '"')
		SECOND% = INSTR(FIRST% + 1%, TEXT$, '"')

		LOOP% = LOOP% + 1%
		IF (SMG_WINDOW::HFLAG(9% + LOOP%) AND 2%) = 0%
		THEN
			TEXT$ = MID(TEXT$, FIRST% + 1%, &
				SECOND% - FIRST% - 1%) + &
				SPACE$(40%)
			! Display description for input
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				TEXT$, 10% + LOOP%, 49%, , SMG$M_BOLD)
		END IF
		GOTO 20410

20420		FOR I% = LOOP% + 1% TO 8%
			! Display blank description for input
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				SPACE$(40%), &
				10% + I%, 49%, , SMG$M_BOLD)
		NEXT I%

		CLOSE FS.CH%
		CALL ASSG_FREECHANNEL(FS.CH%)

20500	!******************************************************************
	! Set GL_FINSTA_OLD value
	!******************************************************************
	CASE OPT_SETOLD
		GL_FINSTA_OLD = GL_FINSTA

	!******************************************************************
	! Restore GL_FINSTA_OLD value
	!******************************************************************
	CASE OPT_RESETOLD
		GL_FINSTA = GL_FINSTA_OLD

	!******************************************************************
	! Set default value
	!******************************************************************
	CASE OPT_SETDEFAULT
		GL_FINSTA2 = GL_FINSTA

	!******************************************************************
	! Restore default value
	!******************************************************************
	CASE OPT_RESETDEFAULT
		GL_FINSTA = GL_FINSTA2

	!******************************************************************
	! View header
	!******************************************************************
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Prompt Description         " + &
				"           Type Title"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "009,040,045"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = &
				GL_FINSTA::PROMPT + " " + &
				GL_FINSTA::DESCR + " " + &
				GL_FINSTA::FINTYPE + "    " + &
				GL_FINSTA::REPTITLE

		END SELECT

	!******************************************************************
	! Find
	!******************************************************************
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			!
			! FIND according to Primary Key
			!	(Prompt)
			!
			FIND #GL_FINSTA.CH%, KEY #0% GE GL_FINSTA::PROMPT + "", &
				REGARDLESS
		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

	%PAGE

29000	!******************************************************************
	! Trap errors
	!******************************************************************

	ON ERROR GO BACK

32767	!******************************************************************
	! End of GL_MAIN_LAYOUT function
	!******************************************************************
	END FUNCTION
