1	%TITLE "Maintain Cross Reference Table"
	%SBTTL "PR_MAST_JWOD_CROSS_REF"
	%IDENT "V3.6a Calico"

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
	!	.p
	!	The ^*Cross Reference Table\* is used to define what items will
	!	appear on the JWOD report. Only subaccounts listed in the
	!	table will appear on the report.
	!	.p
	!	There are two flags for reporting. '*J' to show the item as JWOD,
	!	and '*N' to show the item as non-JWOD. If there is no record in the
	!	file for a particular subaccount, it will not appear on the report.
	!
	! Index:
	!	.x Cross Reference Table>Maintain
	!	.x Maintain>Cross Reference Table
	!
	! Option:
	!
	!	PR_MAIN_JWOD_CROSS_REF$HELP
	!
	! Author:
	!
	!	11/24/87 - B. Craig Larsen
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAST_JWOD_CROSS_REF
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_MAST_JWOD_CROSS_REF, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_MAST_JWOD_CROSS_REF.OBJ;*
	!
	! Modification history:
	!
	!	06/01/88 - Aaron Redd
	!		Modified to allow R/O open of file if R/W open fails.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/10/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/29/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/16/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW

	%PAGE

400	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(3850%, "")

 ExitProgram:
	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE


	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PR.OPEN]PR_JWOD_CROSS_REF.HB"
	MAP (PR_JWOD_CROSS_REF)	PR_JWOD_CROSS_REF_CDD	PR_JWOD_CROSS_REF
	MAP (PR_JWOD_CROSS_REF2) PR_JWOD_CROSS_REF_CDD PR_JWOD_CROSS_REF_OLD, &
		PR_JWOD_CROSS_REF2

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	! This common area must be mapped in both the main program and
	! in MAINT_GROUP.
	!
	COM (CH_PR_JWOD_CROSS_REF) &
		PR_JWOD_CROSS_REF.CH%, &
		PR_JWOD_CROSS_REF.READONLY%

	%PAGE

	ON ERROR GOTO 29000

	EC$(0%) = "2"
	EC$(1%) = "J - JWOD     "
	EC$(2%) = "N - Non JWOD "

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE 3850%	! Open window

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
			SMG_WINDOW::DESCR = "PR JWOD Cross Reference Maintenance"
			SMG_WINDOW::NHELP = "PR_MAST_JWOD_CROSS_REF"
			SMG_WINDOW::HSIZE = 78%
			SMG_WINDOW::VSIZE = 18%
			SMG_WINDOW::HVIEW = 78%
			SMG_WINDOW::VVIEW = 18%
			SMG_WINDOW::HPOS  = 2%
			SMG_WINDOW::VPOS  = 2%
			SMG_WINDOW::NITEMS= 2%
			SMG_WINDOW::FLAGS = 0%

			SMG_WINDOW::NKEYS = 1%
			SMG_WINDOW::KNAME(0%) = "Sub_account"
				SMG_WINDOW::KFIELD(0%, 0%) = 1%
				SMG_WINDOW::KFIELD(0%, 1%) = 1%

			!
			! Load in defaults
			!
			CALL READ_DEFAULTS(SMG_WINDOW) &
				IF INSTR(1%, " QV", MVALUE) <= 1%

20100			!
			! Declare channels
			!
			IF PR_JWOD_CROSS_REF.CH% > 0%
			THEN
				!
				! Already open, set flag to read-only if was
				! that way from last time.
				!
				SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
					IF PR_JWOD_CROSS_REF.READONLY%
				GOTO 20190
			END IF

			!
			! Open main file (existing) for modification
			!
20150			WHEN ERROR IN
				%INCLUDE "SOURCE:[PR.OPEN]PR_JWOD_CROSS_REF.CRE"
			USE
				CONTINUE 20160 IF ERR = 10%
				MAINT_GROUP = ERR
				CONTINUE 20170
			END WHEN

			PR_JWOD_CROSS_REF.READONLY% = 0%
			GOTO 20190

20160			!
			! If unable to open for modify, try to open with read
			! access only.
			!
			WHEN ERROR IN
				%INCLUDE "SOURCE:[PR.OPEN]PR_JWOD_CROSS_REF.OPN"
			USE
				MAINT_GROUP = ERR
				CONTINUE 20170
			END WHEN

			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
			PR_JWOD_CROSS_REF.READONLY% = -1%

			GOTO 20190

20170			!
			! File not open, so reset channel
			!
			CALL ASSG_FREECHANNEL(PR_JWOD_CROSS_REF.CH%)

			EXIT FUNCTION

20190			SMG_WINDOW::CHAN  = PR_JWOD_CROSS_REF.CH%
			WHEN ERROR IN
				RESET #PR_JWOD_CROSS_REF.CH%
				GET #PR_JWOD_CROSS_REF.CH%, REGARDLESS
			USE
				CONTINUE 32767
			END WHEN

		!
		! Display the background
		!
		! This option is used to display the background information
		! on the screen.  It must first clear any junk on the screen,
		! and the write the background onto it.
		!
		CASE OPT_BACKGROUND

			SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

			SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

			DATA	05, 05, "(01) Sub Account", &
				06, 05, "(02) Flag", &
				0,  0, ""

			RESTORE

			READ XPOS%, YPOS%, XSTR$

			WHILE (XPOS% <> 0%)
				SMG_STATUS% = SMG$PUT_CHARS( &
					SMG_WINDOW::WNUMBER,XSTR$, XPOS%, YPOS%)
				READ XPOS%, YPOS%, XSTR$
			NEXT

			SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		!
		! Enter/Display/Default
		!
		! This option is used to enter the data from the user, display data,
		! set defaults, and return the data back according to MFLAG.
		!
		CASE OPT_ENTRY

			TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
			TEMP$ = "View starting at" IF TEMP$ = "View"

			SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 Reenter1:		SCOPE::SCOPE_EXIT = 0%

			SELECT MLOOP

			CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) SubAccount\*
	!	.p
	!	The ^*Subaccount\* field enters the job number of work
	!	order number of the current item or project.
	!
	! Index:
	!	.x Subaccount
	!
	!--
				PR_JWOD_CROSS_REF::SUBACCT = ENTR_3STRING(SCOPE, &
					SMG_WINDOW::WNUMBER, "05;30", TEMP$, &
					PR_JWOD_CROSS_REF::SUBACCT, MFLAG, &
					"'E", MVALUE)

			CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Flag\*
	!	.p
	!	The ^*Flag\* field is a code indicating whether
	!	the item is a JWOD item or not.  A ^*J\* indicates that the item is a JWOD
	!	item and a ^*N\* indicates that the item is not JWOD.
	!
	! Index:
	!	.x Flag
	!
	!--
 Loop3:				PR_JWOD_CROSS_REF::FLAG = ENTR_3STRING(SCOPE, &
					SMG_WINDOW::WNUMBER, "06;39", TEMP$, &
					PR_JWOD_CROSS_REF::FLAG, MFLAG, &
					"!", MVALUE)

				SELECT SCOPE::SCOPE_EXIT
				CASE SMG$K_TRM_F14
					X% = ENTR_3CHOICE(SCOPE, "", "", EC$(), "", 128%, &
						"List of Flags", "", 0%)
					PR_JWOD_CROSS_REF::FLAG = LEFT$(EC$(X%), 1%) &
						IF X% > 0%
					GOTO Loop3
				END SELECT

			END SELECT

			SCOPE::PRG_ITEM = TEMP1$

		!
		! Test values
		!
20300		CASE OPT_TESTENTRY
			MAINT_GROUP = 0%

		SELECT MLOOP
		CASE 2%
			IF PR_JWOD_CROSS_REF::FLAG = ""
			THEN
				MAINT_GROUP = 1%
			ELSE
				GOOD% = 0%
				GOOD% = -1% IF PR_JWOD_CROSS_REF::FLAG = &
					LEFT(EC$(I%), 1%) &
					FOR I% = 1% TO VAL%(EC$(0%))

				IF GOOD% = 0%
				THEN
					X% = ENTR_3CHOICE(SCOPE, "", "", EC$(), "", 128%, &
						"List of Flags", "", 0%)
					PR_JWOD_CROSS_REF::FLAG = LEFT(EC$(X%), 1%) &
						IF X% > 0%
					MAINT_GROUP = 1%
				END IF
			END IF

		END SELECT

		CASE OPT_DISPLAY

		!
		! Set PR_JWOD_CROSS_REF_OLD value
		!
20500		CASE OPT_SETOLD
			PR_JWOD_CROSS_REF_OLD = PR_JWOD_CROSS_REF

		!
		! Restore PR_JWOD_CROSS_REF_OLD value
		!
		CASE OPT_RESETOLD
			PR_JWOD_CROSS_REF = PR_JWOD_CROSS_REF_OLD

		!
		! Set default value
		!
		CASE OPT_SETDEFAULT
			PR_JWOD_CROSS_REF2 = PR_JWOD_CROSS_REF

		!
		! Restore default value
		!
		CASE OPT_RESETDEFAULT
			PR_JWOD_CROSS_REF = PR_JWOD_CROSS_REF2

		!
		! View header
		!
		CASE OPT_VIEW
			SELECT MLOOP

			!
			! Title (One line only)
			!
			CASE 1%
				MVALUE = "  Sub Account     Flag"

			!
			! Positions of lines
			!
			CASE 2%
				MVALUE = "016"

			!
			! Convert current record into text
			!
			CASE 3%
				MVALUE = &
					PR_JWOD_CROSS_REF::SUBACCT + &
					"       " + &
					PR_JWOD_CROSS_REF::FLAG
			END SELECT
		!
		! Find
		!
		CASE OPT_FIND
			SELECT MLOOP
			CASE 0%
				FIND #PR_JWOD_CROSS_REF.CH%, KEY #0% &
					GE PR_JWOD_CROSS_REF::SUBACCT, &
					REGARDLESS
			END SELECT

		END SELECT
	END SELECT

 ExitFunction:
	EXIT FUNCTION

29000	!
	! Trap Errors
	!
	FILENAME$ = ""
	RESUME ExitFunction

32767	END FUNCTION
