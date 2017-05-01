1	%TITLE "File Structures Description"
	%SBTTL "TK_MAIN_FILE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG TK_MAIN_FILE(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1987 BY
	!
	! Computer Management Center
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be used and
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies therof may not be provided or otherwise made
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
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_MAIN_FILE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP TK_MAIN_FILE
	!	$ DELETE TK_MAIN_FILE.OBJ;*
	!
	! Author:
	!
	!	01/27/88 - Frank F. Starman
	!
	! Modification history:
	!
	!	06/11/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/29/96 - Kevin Handy
	!		Reformat source code
	!
	!	06/04/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/27/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[TK.OPEN]TK_FILE.HB"
	MAP (TK_FILE)		TK_FILE_CDD	TK_FILE
	MAP (TK_FILE_OLD)	TK_FILE_CDD	TK_FILE_OLD, TK_FILE2

	MAP (CH_TK_FILE) &
		TK_FILE.CH%

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!
	! Initilization
	!
	CASE OPT_INIT

		!*************************************************************
		! Set up information
		!*************************************************************

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Record structures description"
		SMG_WINDOW::NHELP = "TK_MAIN_FILE"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 11%

		SMG_WINDOW::NKEYS = 3%
		SMG_WINDOW::KNAME(0%) = "Structure_name"
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
		SMG_WINDOW::KNAME(1%) = "Database"
			SMG_WINDOW::KFIELD(1%, 0%) = 3%
			SMG_WINDOW::KFIELD(1%, 1%) = 4%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%
			SMG_WINDOW::KFIELD(1%, 3%) = 2%
		SMG_WINDOW::KNAME(2%) = "data_Type"
			SMG_WINDOW::KFIELD(2%, 0%) = 2%
			SMG_WINDOW::KFIELD(2%, 1%) = 6%
			SMG_WINDOW::KFIELD(2%, 2%) = 7%

		SMG_WINDOW::HVIEW = 130%
		SMG_WINDOW::VVIEW = 18%

		CALL READ_DEFAULTS(SMG_WINDOW)

700		IF (TK_FILE.CH% <= 0%)
		THEN
			!
			! Open main file (existing) for modification
			!
			WHEN ERROR IN
				%INCLUDE "SOURCE:[TK.OPEN]TK_FILE.CRE"
			USE
				CONTINUE 27000
			END WHEN
		END IF

710		SMG_WINDOW::CHAN  = TK_FILE.CH%

		WHEN ERROR IN
			RESET #TK_FILE.CH%
			GET #TK_FILE.CH%, REGARDLESS
		USE
			CONTINUE 27000 IF ERR = 11%
			EXIT HANDLER
		END WHEN

	!
	! Display window background
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)


		DATA	05, 05, "(01) Structure Name", &
			06, 05, "(02) Sequence", &
			07, 05, "(03) Field Name", &
			08, 05, "(04) Description", &
			09, 05, "(05) Database", &
			10, 05, "(06) Classifier", &
			11, 05, "(07) Data Array", &
			12, 05, "(08) Data Type", &
			13, 05, "(09) Data Size", &
			15, 05, "(09) Date", &
			16, 05, "(10) Time", &
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
20200	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")
 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

			TK_FILE::STRUCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "05;25", TEMP$, &
				TK_FILE::STRUCT, MFLAG, "'E", &
				MVALUE)

		CASE 2%

			TK_FILE::SEQUENCE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "06;25", TEMP$, &
				TK_FILE::SEQUENCE, MFLAG, "~L0'E", &
				MVALUE)

		CASE 3%

			TK_FILE::FLDNAME = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "07;25", TEMP$, &
				TK_FILE::FLDNAME, MFLAG, "'E", MVALUE)

		CASE 4%

			TK_FILE::DESCRIPTION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "08;25", TEMP$, &
				LEFT(TK_FILE::DESCRIPTION, 55%), MFLAG, "'E", &
				MVALUE)

		CASE 5%

			TK_FILE::DATABASE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "09;25", TEMP$, &
				TK_FILE::DATABASE, MFLAG, "'E", MVALUE)

		CASE 6%

			TK_FILE::CLASSIFIER = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "10;25", TEMP$, &
				TK_FILE::CLASSIFIER, MFLAG, "'E", MVALUE)

		CASE 7%

			TK_FILE::DATAARRAY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "11;25", TEMP$, &
				TK_FILE::DATAARRAY, MFLAG, "'E", &
				MVALUE)

		CASE 8%

			TK_FILE::DATETYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "12;25", TEMP$, &
				TK_FILE::DATETYPE, MFLAG, "'E", &
				MVALUE)

		CASE 9%

			TK_FILE::DATASIZE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "13;25", TEMP$, &
				TK_FILE::DATASIZE * 1., MFLAG, "#####", &
				MVALUE)

		CASE 10%

			TK_FILE::CDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "15;25", TEMP$, &
				TK_FILE::CDATE, MFLAG, "", &
				MVALUE)

		CASE 11%

			TK_FILE::CTIME= ENTR_3TIME(SCOPE, &
				SMG_WINDOW::WNUMBER, "16;25", TEMP$, &
				TK_FILE::CTIME, MFLAG, "", &
				MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		TK_MAIN_FILE = 0%

		SELECT MLOOP

		CASE 1%
			IF TK_FILE::STRUCT = ""
			THEN
				TK_MAIN_FILE = 1%
			END IF

		CASE 2%
			IF TK_FILE::SEQUENCE = ""
			THEN
				TK_MAIN_FILE = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #SMG_WINDOW::CHAN, &
							KEY #0% EQ  TK_FILE::STRUCT + &
							TK_FILE::SEQUENCE, &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					TK_MAIN_FILE = 2%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Record Already Exists", 0%)
				END IF
			END IF

		END SELECT

	!
	! Set TK_FILE_OLD value
	!
20500	CASE OPT_SETOLD
		TK_FILE_OLD = TK_FILE

	!
	! Restore TK_FILE_OLD value
	!
	CASE OPT_RESETOLD
		TK_FILE = TK_FILE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		TK_FILE2 = TK_FILE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		TK_FILE = TK_FILE2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Struct Name            Seq  Field Name" + &
				"           Description          DB Classi" + &
				"fier           Array Type           Size"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "025,030,051,072,075,096,102,117"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = LEFT(TK_FILE::STRUCT, 20%) + "   " + &
				TK_FILE::SEQUENCE + "  " + &
				LEFT(TK_FILE::FLDNAME, 20%) + " " + &
				LEFT(TK_FILE::DESCRIPTION, 20%) + " " + &
				TK_FILE::DATABASE + " " + &
				TK_FILE::CLASSIFIER + "   " + &
				TK_FILE::DATAARRAY + "   " + &
				LEFT(TK_FILE::DATETYPE, 14%) + " " + &
				FORMAT$(TK_FILE::DATASIZE, "####")

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE TK_FILE::STRUCT + TK_FILE::SEQUENCE, &
				REGARDLESS
		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE TK_FILE::DATABASE + &
				TK_FILE::STRUCT + &
				TK_FILE::SEQUENCE, &
				REGARDLESS
		CASE 2%
			FIND #SMG_WINDOW::CHAN, &
				KEY #2% GE TK_FILE::DATETYPE + &
				TK_FILE::STRUCT, &
				REGARDLESS
		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
