1	%TITLE "Check/Deposit Control Maintenance"
	%SBTTL "CK_MAIN_CONTROL"
	%IDENT "V3.6a Calico"

	FUNCTION LONG CK_MAIN_CONTROL(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1987,1988 BY
	!
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be used and
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies thereof may not be provided or otherwise mCKe
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
	!	The ^*Check/Deposit Control Maintenance\* option
	!	establishs the current fiscal year and the General Ledger or accounting
	!	period which identifies the accounting period immediately preceeding
	!	the period in which the Check Reconciliation System is initialized. The
	!	record also contains a status flag field which indicates whether or not
	!	a purging process is being executed.
	!	^*
	!	.note
	!	Once this record has been initialized, it
	!	normally is not necessary to access it again.
	!	The record should not be subsequently edited
	!	even though the ability to do so exists.\*
	!	.lm -5
	!	.en
	!
	! Index:
	!	.x Check/Deposit Maintenance>Control
	!	.x Control>Check/Deposit Maintenance
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS CK_SOURCE:CK_MAIN_CONTROL/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN CK_MAIN_CONTROL
	!	$ DELETE CK_MAIN_CONTROL.OBJ;*
	!
	! Author:
	!
	!	12/09/87 - Lance Williams
	!
	! Modification history:
	!
	!	04/28/88 - Kevin Handy
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	03/24/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/17/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/20/2000 - Kevin Handy
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
	%INCLUDE "FUNC_INCLUDE:CK_WINDOW.INC"

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[CK.OPEN]CK_CONTROL.HB"
	MAP (CK_CONTROL)	CK_CONTROL_CDD	CK_CONTROL
	MAP (CK_CONTROL_OLD)	CK_CONTROL_CDD	CK_CONTROL_OLD, CK_CONTROL2

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_CK_CONTROL) &
		CK_CONTROL.CH%, &
		CK_CONTROL.READONLY%

	COM (TT_CK_CONTROL) &
		CLOSETITLE$ = 20%, &
		CLOSETYPE$(2%) = 20%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_WINDOW

	ON ERROR GOTO 29000

	%PAGE

	SELECT MOPTION
	!*****************************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!*****************************************************************
	CASE OPT_INIT

		!******************************************************************
		! Set up information
		!******************************************************************

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Check to GL Control File"
		SMG_WINDOW::NHELP = "CK_MAIN_CONTROL"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 128% !Relative file
		SMG_WINDOW::NITEMS= 3%

		SMG_WINDOW::NKEYS = 0%

		!
		! List of types
		!
		CLOSETITLE$ = "Type   Description"
		CLOSETYPE$(0%) = "2"
		CLOSETYPE$(1%) = "0    No status"
		CLOSETYPE$(2%) = "1    Purging"

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF CK_CONTROL.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF CK_CONTROL.READONLY%
			EXIT FUNCTION
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[CK.OPEN]CK_CONTROL.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			CK_MAIN_CONTROL = ERR
			CONTINUE 770
		END WHEN

		CK_CONTROL.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[CK.OPEN]CK_CONTROL.OPN"
		USE
			CK_MAIN_CONTROL = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		CK_CONTROL.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(CK_CONTROL.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = CK_CONTROL.CH%
		GOSUB 28000

	!
	! Select function
	!
	CASE OPT_OPTLIST

		MVALUE = "Change Blank Help eXit Account "

	!
	! Handle Additional options
	!
	CASE OPT_MOREMENU
		SELECT EDIT$(MVALUE, -1%)

		!
		! Units transaction entry
		!
		CASE "ACCOUNT"
			CK_MAIN_CONTROL = MAIN_WINDOW(CK_MAIN_CONTROLACC.ID, "")

	!++
	! Abstract:ACCOUNT
	!	^*Account\*
	!	.b
	!	.lm +5
	!	The ^*Account\* function
	!	enters the General Ledger account numbers which identify each cash account
	!	as well as establishing a code and the check number parameters for each account.
	!	.lm -5
	!
	! Index:
	!
	!--

		END SELECT

20100	!********************************************************************
	! Display the background
	!
	! This option is used to display the background information on the
	! screen.  It must first clear any junk on the screen, and then
	! write the background onto it.
	!********************************************************************

	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	05,10, "(01) Year", &
			06,10, "(02) GL Period", &
			07,10, "(03) Status Flag", &
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

20200	!*****************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
	!*****************************************************************
	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")
 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Year\*
	!	.b
	!	.lm +5
	!	The ^*Year\* field enters the fiscal year in which the
	!	check reconciliation system is installed. Upon the completion of a fiscal
	!	year, this field will automatically increment to the next year.
	!	^*
	!	.note
	!	Changes to this field after initialization should
	!	not be considered, even though the ability to do
	!	so exists.\*
	!	.en
	!
	! Index:
	!	.x Fiscal Year>Control
	!	.x Year>Control
	!	.x Control>Fiscal Year
	!
	!--
			CK_CONTROL::YEAR = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"05;30", TEMP$, &
				CK_CONTROL::YEAR, MFLAG, "'E", &
				MVALUE)


		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) GL Period\*
	!	.b
	!	.lm +5
	!	The ^*GL Period\* option enters a General Ledger Period
	!	which identifies the accounting period immediately preceeding the one in which
	!	the Check Reconciliation System is initialized.  When the "Purge" option is
	!	executed, this field automatically increments to the
	!	next accounting period.
	!	^*
	!	.note
	!	Changes to this field after initialization should
	!	not be considered, even though the ability to do so
	!	exists.
	!	.en
	!
	! Index:
	!	.x General Ledger>Period>Control
	!	.x Control>General Ledger Period
	!
	!--
			CK_CONTROL::PERIOD = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"06;30", TEMP$, 1.0 * CK_CONTROL::PERIOD, &
				MFLAG, "##", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Status Flag\*
	!	.b
	!	.lm +5
	!	The ^*Status Flag\* field is system generated and indicates the status of the
	!	purging process.
	!	.b
	!	Valid values are:^*
	!	.b
	!	.lm 15
	!	0 = No Status
	!	.br
	!	1 = Purging in Process\*
	!	.lm -5
	!	.b
	!	Pressing ^*<List Choices>\*, while the cursor is located
	!	at this field, will display the valid status flag types
	!	and descriptions.
	!	.b
	!	^*
	!	.note
	!	There is never a need to change the value in
	!	this field.\*
	!	.en
	!
	! Index:
	!	.x Control>Status Flag
	!	.x Status Flag>Control
	!
	!--
			CK_CONTROL::FLAG = EDIT$(ENTR_3STRINGLIST(SCOPE, SMG_WINDOW::WNUMBER, &
				"07;30", TEMP$,CK_CONTROL::FLAG, &
				MFLAG, "'", MVALUE, CLOSETYPE$(), &
				CLOSETITLE$, "005"), -1%)

		END SELECT


		SCOPE::PRG_ITEM = TEMP1$


	! Set CK_CONTROL_OLD value
	!
20500	CASE OPT_SETOLD
		CK_CONTROL_OLD = CK_CONTROL

	!
	! Restore CK_CONTROL_OLD value
	!
	CASE OPT_RESETOLD
		CK_CONTROL = CK_CONTROL_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		CK_CONTROL2 = CK_CONTROL

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		CK_CONTROL = CK_CONTROL2

	END SELECT

	EXIT FUNCTION

28000	!
	! Get control record
	!
	WHEN ERROR IN
		GET #CK_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		CONTINUE 28030
	END WHEN

	GOTO 28040

28030	!
	! Load in defaults for control file
	!
	CK_CONTROL::YEAR = " "
	CK_CONTROL::PERIOD    = 0.0
	CK_CONTROL::FLAG = "0"

	WHEN ERROR IN
		PUT #CK_CONTROL.CH%
	USE
		CALL ENTR_3MESSAGE(SCOPE, "Unable to add a record",0%)
		CONTINUE 32767
	END WHEN

28040	RETURN

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
