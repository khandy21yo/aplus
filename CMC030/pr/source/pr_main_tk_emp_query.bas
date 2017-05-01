1	%TITLE "Employee Master File Maintenance"
	%SBTTL "PR_MAIN_TK_EMP_QUERY"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_TK_EMP_QUERY(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	.p
	!	This program maintains the Payroll Employee Master file
	!	for the time keeper.
	!
	! Index:
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_TK_EMP_QUERY/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PR_MAIN_TK_EMP_QUERY
	!	$ DELETE PR_MAIN_TK_EMP_QUERY.OBJ;*
	!
	! Author:
	!
	!	12/03/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	06/01/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/27/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/28/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	11/28/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER
	MAP (PR_EMP_MASTER2) PR_EMP_MASTER_CDD PR_EMP_MASTER_OLD, &
		PR_EMP_MASTER2

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	! This common area must be mapped in some of the MAIN programs,
	! PR_MAST_EMPLOYEE.BAS, and PR_MAST_WC_WORK.BAS.
	!
	COM (CH_PR_EMPLOYEE) &
		PR_EMP_MASTER.CH%, &
		PR_EMP_MASTER.READONLY%

	!
	! Declare data types
	!
	DECLARE LONG XPOS, YPOS

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Payroll Time Keeper"
		SMG_WINDOW::NHELP = "PR_MAIN_TK_EMP_QUERY"
		SMG_WINDOW::CHAN  = PR_EMP_MASTER.CH%
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 4%

		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 4%
		SMG_WINDOW::KNAME(0%) = "Employee-number"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		SMG_WINDOW::KNAME(1%) = "Name"
			SMG_WINDOW::KFIELD(1%, 0%) = 1%
			SMG_WINDOW::KFIELD(1%, 1%) = 2%

		SMG_WINDOW::KNAME(2%) = "Alpha_sort"
			SMG_WINDOW::KFIELD(2%, 0%) = 1%
			SMG_WINDOW::KFIELD(2%, 1%) = 3%

		SMG_WINDOW::KNAME(3%) = "Soc-sec-num"
			SMG_WINDOW::KFIELD(3%, 0%) = 1%
			SMG_WINDOW::KFIELD(3%, 1%) = 4%

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
		IF PR_EMP_MASTER.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_EMP_MASTER.READONLY%
			GOTO 790
		END IF

750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PR_MAIN_TK_EMP_QUERY = ERR
			CONTINUE 770
		END WHEN

		PR_EMP_MASTER.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
		USE
			PR_MAIN_TK_EMP_QUERY = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_EMP_MASTER.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PR_EMP_MASTER.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PR_EMP_MASTER.CH%
		WHEN ERROR IN
			RESET #PR_EMP_MASTER.CH%
			GET #PR_EMP_MASTER.CH%, REGARDLESS
			UNLOCK	#PR_EMP_MASTER.CH%
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display the background
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	1,  1, "(01) Emp #", &
			2,  1, "(02) Name", &
			3,  1, "(03) Sort", &
			1, 40, "(04) Soc Sec Num", &
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
	! More options
	!
	CASE OPT_OPTLIST
		MVALUE = "Find Next Restore View Help eXit"

	!
	! Enter/Display/Default
	!
20200	CASE OPT_ENTRY
		TEMP$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View "

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		SCOPE::SCOPE_EXIT = 0%

 E0Loop:	SELECT MLOOP

		CASE 1%
			PR_EMP_MASTER::EMPNUM  = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "1;15", TEMP$, &
				PR_EMP_MASTER::EMPNUM, MFLAG, "'E", MVALUE)

		CASE 2%

			PR_EMP_MASTER::EMPNAME = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;15", TEMP$, &
				PR_EMP_MASTER::EMPNAME, MFLAG, &
				"'E", MVALUE)

		CASE 3%
			PR_EMP_MASTER::SORT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "3;15", TEMP$, &
				PR_EMP_MASTER::SORT, MFLAG, &
				"'E", MVALUE)

		CASE 4%
			PR_EMP_MASTER::SSN = ENTR_3SSN(SCOPE, &
				SMG_WINDOW::WNUMBER, "1;58", TEMP$, &
				PR_EMP_MASTER::SSN, MFLAG, &
				"'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

20300	CASE OPT_TESTENTRY

		PR_MAIN_TK_EMP_QUERY = 0%

20500	CASE OPT_SETOLD
		PR_EMP_MASTER_OLD = PR_EMP_MASTER

	CASE OPT_RESETOLD
		PR_EMP_MASTER = PR_EMP_MASTER_OLD

	CASE OPT_SETDEFAULT
		PR_EMP_MASTER2 = PR_EMP_MASTER

	CASE OPT_RESETDEFAULT
		PR_EMP_MASTER = PR_EMP_MASTER2

	CASE OPT_VIEW
		SELECT MLOOP

		CASE 1%
			MVALUE = " Emp number Name                 " + &
				"Sort           SSN"

		CASE 2%
			MVALUE = "012,033,048"

		CASE 3%
			MVALUE = PR_EMP_MASTER::EMPNUM + " " + &
				LEFT(PR_EMP_MASTER::EMPNAME, 20%) + " " + &
				LEFT(PR_EMP_MASTER::SORT, 14%) + " " + &
				LEFT(PR_EMP_MASTER::SSN, 13%)
		END SELECT

	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #PR_EMP_MASTER.CH%, &
				KEY #0% GE PR_EMP_MASTER::EMPNUM + "", &
				REGARDLESS

		CASE 1%
			FIND #PR_EMP_MASTER.CH%, &
				KEY #1% GE PR_EMP_MASTER::EMPNAME + "", &
				REGARDLESS

		CASE 2%
			FIND #PR_EMP_MASTER.CH%, &
				KEY #2% GE PR_EMP_MASTER::SORT + "", &
				REGARDLESS

		CASE 3%
			FIND #PR_EMP_MASTER.CH%, &
				KEY #3% GE PR_EMP_MASTER::SSN + "", &
				REGARDLESS

		END SELECT

	END SELECT

	EXIT FUNCTION

29000	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
