1	%TITLE "PR Control File Maintenance"
	%SBTTL "PR_MAIN_CONTROL"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_CONTROL(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	^*Payroll Update Control File\*
	!	.b
	!	.lm +5
	!	The ^*Payroll Update Control File\* indicates the current status of
	!	payroll processing as related to the following matters:
	!	.table 3,25
	!	.te
	!	Last Year Purged (from files)
	!	.te
	!	Last Payroll Posted (to the General
	!	Ledger)
	!	.te
	!	Last Payroll Updated/Reversed (in
	!	the Payroll Ledger)
	!	.te
	!	Update Counter
	!	.te
	!	Update Flag
	!	.end table
	!	^*Note:\* All data in the Payroll Update Control File is system
	!	generated and is ^*not\* intended to be entered or
	!	edited by the user.
	!
	! Index:
	!	.x Update Control File>Payroll
	!	.x Payroll>Update Control File
	!	.x Utility>Payroll Update Control File
	!
	! Option:
	!
	! Author:
	!
	!	12/01/88 - J. Shad Rydalch
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_CONTROL
	!	$ LIB FUNC_LIB:CMCFUN/REP PR_MAIN_CONTROL
	!	$ DELETE PR_MAIN_CONTROL.OBJ;*
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/22/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	01/06/2000 - Kevin Handy
	!		Fill out remaining fields on initial file
	!		creation.
	!
	!	12/12/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PR.OPEN]PR_CONTROL.HB"
	MAP (PR_CONTROL)	PR_CONTROL_CDD	PR_CONTROL
	MAP (PR_CONTROL2)	PR_CONTROL_CDD	PR_CONTROL_OLD, PR_CONTROL2

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PR_CONTROL)	PR_CONTROL.CH%, &
		CLOSETITLE$ = 20%, &
		CLOSETYPE$(4%) = 20%, &
		APPLYTITLE$ = 30%, &
		APPLYTYPE$(2%) = 30%, &
		PR_CONTROL.READONLY%

	!
	! Declare data types
	!
	DECLARE LONG XPOS, YPOS

	%PAGE

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

		!******************************************************************
		! Set up information
		!******************************************************************

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Payroll Control File Maintenance"
		SMG_WINDOW::NHELP = "PR_MAIN_CONTROL"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 6%
		SMG_WINDOW::FLAGS = 128%	! Relative file

		SMG_WINDOW::NKEYS = 0%

		!
		! List of types
		!
		CLOSETITLE$ = "Type Description"
		CLOSETYPE$(0%) = "3"
		CLOSETYPE$(1%) = "0    No status"
		CLOSETYPE$(2%) = "1    Updating"
		CLOSETYPE$(3%) = "2    Reversing"

		!
		! List of apply types
		!
		APPLYTITLE$ = "Type Description"
		APPLYTYPE$(0%) = "2"
		APPLYTYPE$(1%) = "D    Apply OH By Department"
		APPLYTYPE$(2%) = "S    Apply OH By SubAcct"

700		!
		! Declare channels
		!
		IF PR_CONTROL.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_CONTROL.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_CONTROL.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PR_MAIN_CONTROL = ERR
			CONTINUE 770
		END WHEN

		PR_CONTROL.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_CONTROL.OPN"
		USE
			PR_MAIN_CONTROL = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_CONTROL.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PR_CONTROL.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PR_CONTROL.CH%
		GOSUB 28000

	!
	! Select function
	!
	CASE OPT_OPTLIST
		MVALUE = "Change Blank Help eXit"

	!
	! Display the background
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	6, 6,	"(01) Last Year Purged", &
			8, 6,	"(02) Last Payroll Posted", &
			10, 6,	"(03) Last Payroll Updated/Reversed", &
			12, 6,	"(04) Update Counter", &
			14, 6,	"(05) Update Flag", &
			16, 6,	"(06) Apply Flag", &
			0,  0,	""

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
20200	CASE OPT_ENTRY
		TEMP$ = TRM$(SCOPE::PRG_ITEM)

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 Reenter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	!
	! Abstract:FLD001
	!	^*(01) Last Year Purged\*
	!	.b
	!	.lm +5
	!	The ^*Last Year Purged\* field
	!	indicates the last calendar year for which the Purge routine was
	!	executed.
	!	.b
	!	^*Note\* The value in this field is system generated for
	!	informational purposes. It is ^*not\* intended
	!	that any data be entered by the user.
	!	.lm -5
	!
	! Index:
	!	.x Last Year Purged
	!	.x Update Status>Last Year Purged
	!
	!--
			PR_CONTROL::YEAR = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;44", TEMP$, &
				PR_CONTROL::YEAR, MFLAG, "'E", MVALUE)

		CASE 2%
	!++
	!
	! Abstract:FLD002
	!	^*(02) Last Payroll Posted\*
	!	.b
	!	.lm +5
	!	The ^*Last Payroll Posted\* field
	!	indicates the date of the payroll folder file
	!	which was most recently posted to the General Ledger.
	!	.b
	!	^*Note:\* The value of this field is system generated for
	!	informational purposes. It is ^*not\* intended that
	!	the data be entered by the user.
	!	.lm -5
	!
	! Index:
	!	.x Last Payroll Posted
	!	.x Utility>Update Status>Last Payroll Posted
	!	.x Update>Status>Last Payroll Posted
	!
	!--
			PR_CONTROL::POST_DATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"8;44", TEMP$, &
				PR_CONTROL::POST_DATE, &
				MFLAG, "8", MVALUE)

		CASE 3%
	!++
	!
	! Abstract:FLD003
	!	^*(03) Last Payroll Updated/Reversed\*
	!	.b
	!	.lm +5
	!	The ^*Last Payroll Updated/Reversed\* field
	!	indicates the date of the Payroll Folder
	!	File which has been most recently updated or reversed.
	!	.b
	!	^*Note:\* The value of this field is system generated for
	!	informational purposes only. It is ^*not\* intended
	!	that data be entered by the user.
	!	.lm -5
	!
	! Index:
	!	.x Last Payroll Updated/Reversed
	!	.x Update Status>Last Payroll Updated/Reversed
	!	.x Utility>Update Status
	!
	!--
			PR_CONTROL::UR_DATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"10;44", TEMP$, &
				PR_CONTROL::UR_DATE, &
				MFLAG, "8", MVALUE)

		CASE 4%
	!++
	!
	! Abstract:FLD004
	!	^*(04) Update Counter\*
	!	.b
	!	.lm +5
	!	The ^*Update Counter\* field
	!	indicates the system generated update counter flag which
	!	is assigned as a payroll update routine is initiated. The purpose of
	!	the update counter is to aid in the recovery of a payroll update in
	!	the event of system crash such as might be experienced during a power
	!	failure.
	!	.b
	!	^*Note:\* It is ^~not\~ intended that data in this field be
	!	entered by the user.
	!	.lm -5
	!
	! Index:
	!	.x Update Counter
	!	.x Update Status>Update Counter
	!
	!--
			PR_CONTROL::UR_COUNT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"12;44", TEMP$, &
				PR_CONTROL::UR_COUNT * 1.0, &
				MFLAG, "#####", MVALUE)

		CASE 5%
	!++
	!
	! Abstract:FLD005
	!	^*(05) Update Flag\*
	!	.b
	!	.lm +5
	!	The ^*Update Flag\* indicates the status
	!	of any payroll updating process. Valid flags are:
	!	.table 3,25
	!	.te
	!	^*0\*	No status
	!	.te
	!	^*1\*	Updating
	!	.te
	!	^*2\*	Reversing
	!	.end table
	!	^*Note:\* The value in this field is system generated, and is
	!	^~not\~ intended for user entry.
	!
	! Index:
	!	.x Flag>Update
	!	.x Update>Flag
	!	.x Payroll>Update Status>Update Flag
	!
	!--
			PR_CONTROL::CLOSEFLAG = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"14;44", TEMP$, &
				PR_CONTROL::CLOSEFLAG, &
				MFLAG, "'", MVALUE, &
				CLOSETYPE$(), &
				CLOSETITLE$, "005"), -1%)

		CASE 6%
	!++
	!
	! Abstract:FLD006
	!	^*(06) Apply Flag\*
	!	.b
	!	.lm +5
	!	The ^*Apply Flag\* field
	!	defines a flag which will control the method in which overhead
	!	is handled.
	!	.b
	!	Valid flags are:
	!	.table 3,25
	!	.te
	!	^*D\*	Apply OH by Department
	!	.te
	!	^*S\*	Apply OH by SubAccount
	!	.end table
	!	The effect of applying overhead by department is that actual
	!	payroll taxes and workmens compensation expenses will be allocated
	!	to designated departmental accounts, depending upon the default
	!	account numbers recorded in each employee's master file.
	!	.b
	!	The effect of applying overhead by subaccount is that average
	!	percentages or amounts, based on direct labor dollars or hours, is
	!	applied to the jobs or processes in process. The percentages or
	!	amounts may vary depending upon the operation and General Ledger
	!
	! Index:
	!	.x Apply Flag>Update Control File
	!	.x Update Control File>Apply Flag
	!
	!--
			PR_CONTROL::OH_APPLY_FLAG = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"16;44", TEMP$, &
				PR_CONTROL::OH_APPLY_FLAG, &
				MFLAG, "'", MVALUE, &
				APPLYTYPE$(), &
				APPLYTITLE$, "005"), -1%)

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		PR_MAIN_CONTROL = 0%

	!
	! Set PR_CONTROL_OLD value
	!
20500	CASE OPT_SETOLD
		PR_CONTROL_OLD = PR_CONTROL

	!
	! Restore PR_CONTROL_OLD value
	!
	CASE OPT_RESETOLD
		PR_CONTROL = PR_CONTROL_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PR_CONTROL2 = PR_CONTROL

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PR_CONTROL = PR_CONTROL2

	!
	! Display additional info
	!
	CASE OPT_DISPLAY

	END SELECT

	EXIT FUNCTION

28000	!
	! Get period record
	!
	WHEN ERROR IN
		GET #PR_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		CONTINUE 28030
	END WHEN

	GOTO 28040

28030	!
	! Load in defaults for period file
	!
	PR_CONTROL::YEAR = DATE_TODAY
	PR_CONTROL::CLOSEFLAG = "0"

	PR_CONTROL::POST_DATE = ""
	PR_CONTROL::UR_DATE = ""
	PR_CONTROL::UR_COUNT = 0%
	PR_CONTROL::OH_APPLY_FLAG = "D"

	WHEN ERROR IN
		PUT #PR_CONTROL.CH%, RECORD 1%
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
