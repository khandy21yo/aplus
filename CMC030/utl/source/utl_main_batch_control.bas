1	%TITLE "Batch Control File Maintenance"
	%SBTTL "UTL_MAIN_BATCH_CONTROL"
	%IDENT "V3.6a Calico"

	FUNCTION LONG UTL_MAIN_BATCH_CONTROL(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	! ID:0117
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	When the posting routine is initiated, the system creates a
	!	Batch Control Record. In the event of a system failure, the
	!	Batch Control Record enables the system to continue the
	!	posting procedure from the point of interruption.
	!	.lm +5
	!	.b
	!	^*CAUTION: Do not add or edit any Batch
	!	Control Record.  Doing so could result in undesirable consequences\*
	!	.lm -5
	!
	! Index:
	!	.x Controlling File
	!	.x Batch Control>File Maintenance
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_MAIN_BATCH_CONTROL/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN UTL_MAIN_BATCH_CONTROL
	!	$ DELETE UTL_MAIN_BATCH_CONTROL.OBJ;*
	!
	! Author:
	!
	!	08/07/87 - Robert Peterson
	!
	! Modification history:
	!
	!	08/22/91 - Kevin Handy
	!		Modified the batch number (field 3) so that you can
	!		only type in six characters.  Any more cause
	!		the format in ASSG_BATCH to come out to something
	!		line "%.1e+8", which causes errors in creating the
	!		work files.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/05/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/04/99 - Kevin Handy
	!		Change field 'PROGRAM' to 'PROGRAMNAME'
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	12/05/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_BATCH_CONTROL.HB"
	MAP (UTL_BATCH_CONTROL)	UTL_BATCH_CONTROL_CDD	UTL_BATCH_CONTROL
	MAP (UTL_BATCH_CONTROL_BLAH)	UTL_BATCH_CONTROL_CDD &
		UTL_BATCH_CONTROL_OLD, UTL_BATCH_CONTROL2

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_UTL_BATCH_CONTROL) &
		UTL_BATCH_CONTROL.CH%, &
		UTL_BATCH_CONTROL.READONLY%


	!
	! Declare data types
	!
	DECLARE LONG XPOS, YPOS

	%PAGE

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define UTL_BATCH_CONTROL
		!
		SMG_WINDOW::DESCR = "Batch Control File Maintenance"
		SMG_WINDOW::NHELP = "UTL_MAIN_BATCH_CONTROL"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 9%
		SMG_WINDOW::FLAGS = 0%
		! 1 - Allow window command

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Batch "
		SMG_WINDOW::KFIELD(0%, 0%) = 1%
		SMG_WINDOW::KFIELD(0%, 1%) = 1%

20010		GOTO 20020 IF UTL_BATCH_CONTROL.CH% > 0%

		CALL READ_DEFAULTS(SMG_WINDOW)

		%INCLUDE "SOURCE:[UTL.OPEN]UTL_BATCH_CONTROL.CRE"

20020		SMG_WINDOW::CHAN  = UTL_BATCH_CONTROL.CH%

		WHEN ERROR IN
			RESET	#UTL_BATCH_CONTROL.CH%
			GET	#UTL_BATCH_CONTROL.CH%, REGARDLESS
 !			UNLOCK	#UTL_BATCH_CONTROL.CH%
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display the background
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE( &
			SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	2,  10, "(01) Batch", &
			3,  10, "(02) Process Name", &
			4,  10, "(03) User Batch Number", &
			5,  10, "(04) Date Start", &
			6,  10, "(05) Time Start", &
			7,  10, "(06) Post Status", &
			8,  10, "(07) Description", &
			9,  10, "(08) Period 1", &
			10,  10, "(09) Period 2", &
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

	!
	! Enter/Display/Default
	!
	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View  "

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		SELECT MLOOP
		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Batch\*
	!	.b
	!	.lm +5
	!	The ^*Batch\* field contains the number
	!	which the system automatically assigns to a specific posting execution.
	!	.lm -5
	!
	! Index:
	!
	!--

			UTL_BATCH_CONTROL::BATCH = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;35", TEMP$, &
				UTL_BATCH_CONTROL::BATCH, MFLAG, "'E", &
				MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Process Name\*
	!	.b
	!	.lm +5
	!	The ^*Process Name\* field contains the
	!	program name of the process which has been or is being executed.
	!	.lm -5
	!
	! Index:
	!
	!--

			UTL_BATCH_CONTROL::PROGRAMNAME = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "3;35", TEMP$, &
				UTL_BATCH_CONTROL::PROGRAMNAME, MFLAG, &
				"'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) User Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*User Batch Number\* field contains the
	!	user assigned batch number during the entry of a particular journal.
	!	.lm -5
	!
	! Index:
	!
	!--
			UTL_BATCH_CONTROL::BFILE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "4;35", TEMP$, &
				LEFT(UTL_BATCH_CONTROL::BFILE, 6%), MFLAG, &
				"'LLLLL", MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Date Start\*
	!	.b
	!	.lm +5
	!	The ^*Date Start\* field contains the date
	!	when a specific journal posting procedure was initiated.
	!	.lm -5
	!
	! Index:
	!
	!--

			UTL_BATCH_CONTROL::DSTART = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "5;35", TEMP$, &
				UTL_BATCH_CONTROL::DSTART, MFLAG, &
				"'E", MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Time Start\*
	!	.b
	!	.lm +5
	!	The ^*Time Start\* field contains the time
	!	of day when a specific posting procedure was initiated.
	!	.lm -5
	!
	! Index:
	!
	!--

			UTL_BATCH_CONTROL::TSTART = ENTR_3TIME(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;35", TEMP$, &
				UTL_BATCH_CONTROL::TSTART, MFLAG, "", &
				MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Post Status\*
	!	.b
	!	.lm +5
	!	The ^*Post Status\* field contains a code
	!	which indicates whether a specific journal posting process completed
	!	execution or is in process.
	!	.lm -5
	!
	! Index:
	!
	!--
			UTL_BATCH_CONTROL::USTATUS = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;35", TEMP$, &
				UTL_BATCH_CONTROL::USTATUS, MFLAG, &
				"'E", MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	^*(07) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field contains a verbal
	!	description of the process executed or in the process of execution.
	!	.lm -5
	!
	! Index:
	!
	!--

			UTL_BATCH_CONTROL::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "8;35", TEMP$, &
				UTL_BATCH_CONTROL::DESCR, MFLAG, "'E", &
				MVALUE)

		CASE 8%
	!++
	! Abstract:FLD008
	!	^*(08) Period 1\*
	!	.b
	!	.lm +5
	!	The ^*Period 1\* field contains
	!	the accounting period where a journal file is posting.
	!	.lm -5
	!
	! Index:
	!
	!--

			UTL_BATCH_CONTROL::UTLFILE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "9;35", TEMP$, &
				UTL_BATCH_CONTROL::UTLFILE, MFLAG, &
				"'E", MVALUE)

		CASE 9%

	!++
	! Abstract:FLD009
	!	^*(09) Period 2\*
	!	.b
	!	.lm +5
	!	The ^*Period 2\* field contains
	!	the accounting period where a journal file is posting. This field is
	!	used only if a posted journal needs to be transmitted to the General
	!	Ledger and the Inventory Ledger.
	!	.lm -5
	!
	! Index:
	!
	!--

			UTL_BATCH_CONTROL::U1FILE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "10;35", TEMP$, &
				UTL_BATCH_CONTROL::U1FILE, MFLAG, &
				"'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test value
	!
	CASE OPT_TESTENTRY

		UTL_MAIN_BATCH_CONTROL = 0%

		SELECT MLOOP

		CASE 1%
			IF UTL_BATCH_CONTROL::BATCH = ""
			THEN
				UTL_MAIN_BATCH_CONTROL = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					GET #UTL_BATCH_CONTROL.CH%, &
						KEY #0% EQ UTL_BATCH_CONTROL::BATCH + "", &
						REGARDLESS

					UTL_MAIN_BATCH_CONTROL = 2%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Record Already Exists", 1%)
				END IF
			END IF
		END SELECT

	!
	! Test option
	!
	CASE OPT_TESTOPT
		TESTOPT%, UTL_MAIN_BATCH_CONTROL = 0%

		IF EDIT$(MVALUE, -1%) = "ERASE"
		THEN
			TESTOPT%, UTL_MAIN_BATCH_CONTROL = 1% &
				IF UTL_BATCH_CONTROL::BATCH = "000000"

			CALL ENTR_3MESSAGE(SCOPE, "Batch 000000 cannot " + &
				"be Erased", 0%) IF TESTOPT% <> 0%
		END IF

	!
	! Display additional information
	!
	CASE OPT_DISPLAY
		UTL_MAIN_BATCH_CONTROL = 0%

	!
	! Set OLD value
	!
	CASE OPT_SETOLD
		UTL_BATCH_CONTROL_OLD = UTL_BATCH_CONTROL

	!
	! Restore UTL_BATCH_CONTROL_OLD value
	!
	CASE OPT_RESETOLD
		UTL_BATCH_CONTROL = UTL_BATCH_CONTROL_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		UTL_BATCH_CONTROL2 = UTL_BATCH_CONTROL

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		UTL_BATCH_CONTROL = UTL_BATCH_CONTROL2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		CASE 1%
			MVALUE = " Batch    Program             " + &
				"                 BFile    Date Start " + &
				"Time Start"

		CASE 2%
			MVALUE = "010,047,056,067"

		CASE 3%
			MVALUE = UTL_BATCH_CONTROL::BATCH + "  " + &
			LEFT(UTL_BATCH_CONTROL::PROGRAMNAME, 36%)  + " " + &
			UTL_BATCH_CONTROL::BFILE + " " + &
			PRNT_DATE(UTL_BATCH_CONTROL::DSTART, 8%) + " " + &
			PRNT_TIME(UTL_BATCH_CONTROL::TSTART, 6%)

		END SELECT
	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #UTL_BATCH_CONTROL.CH%, KEY #0% GE &
				UTL_BATCH_CONTROL::BATCH, REGARDLESS

		END SELECT

	END SELECT

	EXIT FUNCTION

32767	END FUNCTION
