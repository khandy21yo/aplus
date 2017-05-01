1	%TITLE "Maintain Operations Table"
	%SBTTL "PR_MAIN_OPER"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_OPER(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	The ^*Maintain Operations Table\*
	!	accesses the file where valid operations are
	!	established, including related effective dates and industrial
	!	standards for both piece and hourly rates.
	!	.lm -5
	!
	! Index:
	!	.x Operation>Table
	!	.x Tables>Operation
	!
	! Option:
	!
	! Author:
	!
	!	11/24/87 - B. Craig Larsen
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_OPER/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PR_MAIN_OPER
	!	$ DELETE PR_MAIN_OPER.OBJ;*
	!
	! Modification history:
	!
	!	11/12/90 - Craig Tanner
	!		Split in to to modules, PR_MAST_OPER and PR_MAIN_OPER.
	!
	!	06/25/92 - Frank F. Starman
	!		Change title.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/09/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/29/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/14/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Map Statements and CDD inclusions
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PR.OPEN]PR_OPER.HB"
	MAP	(PR_OPER)	PR_OPER_CDD	PR_OPER
	MAP	(PR_OPER2)	PR_OPER_CDD	PR_OPER_OLD, PR_OPER2

	!
	! Functions
	!
	EXTERNAL LONG   FUNCTION PR_FUNC_COPYOPER

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	!
	COM (CH_PR_OPER) &
		PR_OPER.CH%, &
		PR_OPER.READONLY%

	%PAGE


	!
	! Set up error handling
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
		SMG_WINDOW::DESCR = "Operations Table Maintenance"
		SMG_WINDOW::NHELP = "PR_MAIN_OPER"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 5%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Operation"
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

20100		!
		! Declare channels
		!
		IF PR_OPER.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_OPER.READONLY%
			GOTO 20190
		END IF


		!
		! Open main file (existing) for modification
		!
20150		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_OPER.CRE"
		USE
			CONTINUE 20160 IF ERR = 10%
			PR_MAIN_OPER = ERR
			CONTINUE 20170
		END WHEN

		PR_OPER.READONLY% = 0%
		GOTO 20190

20160		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_OPER.OPN"
		USE
			PR_MAIN_OPER = ERR
			CONTINUE 20170
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_OPER.READONLY% = -1%

		GOTO 20190

20170		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PR_OPER.CH%)

		EXIT FUNCTION

20190		SMG_WINDOW::CHAN  = PR_OPER.CH%
		WHEN ERROR IN
			RESET #PR_OPER.CH%
			GET #PR_OPER.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display the background
	!
	! This option is used to display the background information
	! on the screen.  It must first clear any junk on the screen,
	! and then write the background onto it.
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	05, 05, "(01) Operation", &
			06, 05, "(02) Effective Date", &
			10, 05, "(03) Unit Rate", &
			11, 05, "(04) Hourly Rate", &
			12, 05, "(05) Unit/Hour", &
			08, 05, "Industrial Standards", &
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
	! Add to the list of options
	!
	CASE OPT_OPTLIST
		MVALUE = MVALUE + " cOpy"

	!
	! Window option
	!
	CASE OPT_MOREMENU

		SELECT MVALUE

	!++
	! Abstract:COPY
	!	^*Copy\*
	!	.b
	!	.lm +5
	!	The ^*Copy\* option creates new records in the operations file
	!	from existing records for a new period.  It will ask for the
	!	new starting date, the new hourly rate, from and to operation,
	!	and will then create new operation records from that information.
	!	.lm -5
	!
	! Index:
	!	.x Copy>Operations
	!	.x Operations>Copy
	!
	!--
		CASE "cOpy"
			CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)
			JUNK% = PR_FUNC_COPYOPER(PR_OPER.CH%)
		END SELECT



	!
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user,
	! display data, set defaults, and return the data back
	! according to MFLAG.
	!
20200	CASE OPT_ENTRY

		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 Reenter1:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Operation\*
	!	.b
	!	.lm +5
	!	The ^*Operation\* field
	!	enters an operation name or code up to eight (8) alphanumeric
	!	characters in length.  The field is a key.  Duplicates are allowed
	!	in order to have more than one record for the same operation, but with
	!	different effective dates and different industrial standards for
	!	pieces per hour and/or hourly rates.
	!	.lm -5
	!
	! Index:
	!	.x Operation
	!	.x Operation>Key
	!
	!--

			PR_OPER::OPER = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "05;32", TEMP$, &
				PR_OPER::OPER, MFLAG, "'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Effective Date\*
	!	.b
	!	.lm +5
	!	The ^*Effective Date\* field
	!	enters a date in MMDDYY format indicating when an
	!	industrial standard pieces per hour and/or hourly rate became or
	!	will become effective.
	!	.lm -5
	!
	! Index:
	!	.x Operation>Effective Date
	!
	!--
			PR_OPER::EFFDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "06;32", TEMP$, &
				PR_OPER::EFFDATE, MFLAG, &
				"8", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Unit Rate\*
	!	.b
	!	.lm +5
	!	The ^*Unit Rate\* field
	!	enters the industrial standard unit rate for the specific
	!	subject operation as of a specified effective date.
	!	.lm -5
	!
	! Index:
	!	.x Operation>Pieces Per Hour
	!
	!--
			TEMP = 0.0
			TEMP = PR_OPER::HOUR_RATE/PR_OPER::PIECE_RATE &
				IF PR_OPER::PIECE_RATE <> 0.0

			PR_OPER::PIECE_RATE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "10;26", TEMP$, &
				PR_OPER::PIECE_RATE * 1.0, MFLAG, &
				"#,###,###.######", MVALUE)

			TEMP1 = 0.0
			TEMP1 = PR_OPER::HOUR_RATE / PR_OPER::PIECE_RATE &
				IF PR_OPER::PIECE_RATE <> 0.0

			IF TEMP <> TEMP1
			THEN
				PR_OPER::HOUR_RATE = &
					PR_OPER::PIECE_RATE * TEMP

				SMG_STATUS% = SMG$PUT_CHARS( &
					SMG_WINDOW::WNUMBER, &
					FORMAT$(PR_OPER::HOUR_RATE, &
					"###,###,###.######"), &
					11%, 24%, , SMG$M_BOLD)
			END IF

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Hourly Rate\*
	!	.b
	!	.lm +5
	!	The ^*Hourly Rate\* field
	!	enters the industrial standard hourly rate for the specific
	!	subject operation as of a specified effective date.
	!	.lm -5
	!
	! Index:
	!	.x Operation>Hourly Rate
	!
	!--
			TEMP = 0.0
			TEMP = PR_OPER::HOUR_RATE/PR_OPER::PIECE_RATE &
				IF PR_OPER::PIECE_RATE <> 0.0

			PR_OPER::HOUR_RATE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "11;24", TEMP$, &
				PR_OPER::HOUR_RATE * 1.0, MFLAG, &
				"###,###,###.######", MVALUE)

			TEMP1 = 0.0
			TEMP1 = PR_OPER::HOUR_RATE / PR_OPER::PIECE_RATE &
				IF PR_OPER::PIECE_RATE <> 0.0

			IF TEMP <> TEMP1
			THEN
				PR_OPER::PIECE_RATE = &
				PR_OPER::HOUR_RATE / TEMP

				SMG_STATUS% = SMG$PUT_CHARS( &
					SMG_WINDOW::WNUMBER, &
					FORMAT$(PR_OPER::PIECE_RATE, &
					"###,###,###.######"), &
					10%, 24%, , SMG$M_BOLD)
			END IF

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Unit/Hourly Rate\*
	!	.b
	!	.lm +5
	!	This field contains the unit (03) divided by the hourly rate (04)
	!	to get an approximate unit rate.  This field is used in the ^*Copy\*
	!	function to recompute the new Units based on the new hourly rate.
	!	.lm -5
	!
	! Index:
	!	.x Oper>Unit/Hour
	!	.x Unit/Hour>Oper
	!
	!--
			TEMP = 0.0
			TEMP = PR_OPER::HOUR_RATE / PR_OPER::PIECE_RATE &
				IF PR_OPER::PIECE_RATE <> 0.0

			TEMP1 = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "12;24", TEMP$, &
				TEMP * 1.0, MFLAG, &
				"###,###,###.######", MVALUE)

			IF TEMP <> TEMP1
			THEN
				PR_OPER::HOUR_RATE = PR_OPER::PIECE_RATE * TEMP1

				SMG_STATUS% = SMG$PUT_CHARS( &
					SMG_WINDOW::WNUMBER, &
					FORMAT$(PR_OPER::HOUR_RATE, &
					"###,###,###.######"), &
					11%, 24%, , SMG$M_BOLD)

			END IF
		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

		!
		! Test values
		!
20300	CASE OPT_TESTENTRY
		PR_MAIN_OPER = 0%

		SELECT MLOOP

		CASE 1%
			IF PR_OPER::OPER = ""
			THEN
				PR_MAIN_OPER = 1%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Blank Operation # not allowed", 1%)
			END IF

		CASE 2%
			IF PR_OPER::EFFDATE = ""
			THEN
				PR_MAIN_OPER = 1%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Blank Effective date not allowed", 1%)
			END IF
		END SELECT

	CASE OPT_DISPLAY

		!
		! Set PR_OPER_OLD value
		!
20500	CASE OPT_SETOLD
		PR_OPER_OLD = PR_OPER

		!
		! Restore PR_OPER_OLD value
		!
	CASE OPT_RESETOLD
		PR_OPER = PR_OPER_OLD

		!
		! Set default value
		!
	CASE OPT_SETDEFAULT
		PR_OPER2 = PR_OPER

		!
		! Restore default value
		!
	CASE OPT_RESETDEFAULT
		PR_OPER = PR_OPER2

		!
		! View header
		!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Operation    " + &
				"Effective Date      " + &
				"  Unit Rate        Hour Rate" + &
				"   Units/Hour"
		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "014,030,047,064"

		!
		! Convert current record into text
		!
		CASE 3%
			TEMP = 0.0
			TEMP = PR_OPER::HOUR_RATE/PR_OPER::PIECE_RATE &
				IF PR_OPER::PIECE_RATE <> 0.0
			MVALUE = &
				PR_OPER::OPER + "       " + &
				PRNT_DATE(PR_OPER::EFFDATE, 8%) + &
				"       " + &
				FORMAT$(PR_OPER::PIECE_RATE, &
				"###,###.####") + "     " + &
				FORMAT$(PR_OPER::HOUR_RATE, &
				"###,###.####") + " " + &
				FORMAT$(TEMP, "###,###.####")
		END SELECT
	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #PR_OPER.CH%, KEY #0% &
				GE PR_OPER::OPER + PR_OPER::EFFDATE, &
				REGARDLESS
		END SELECT

	END SELECT

29000	!
	! Trap Errors
	!
	RESUME ExitFunction

 ExitFunction:
32767	END FUNCTION
