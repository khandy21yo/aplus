1	%TITLE "Maintenance Job Cost Controlling File"
	%SBTTL "JC_MAIN_CONTROL"
	%IDENT "V3.6a Calico"

	FUNCTION LONG JC_MAIN_CONTROL(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	To initialize and show the status of the system at the present time access
	!	the ^*Maintenance Job Cost Controlling File\*.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS JC_SOURCE:JC_MAIN_CONTROL/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN JC_MAIN_CONTROL
	!	$ DELETE JC_MAIN_CONTROL.OBJ;*
	!
	! Author:
	!
	!	03/19/89 - Frank Starman
	!
	! Modification history:
	!
	!	04/27/92 - Kevin Handy
	!		Clean up (check)
	!
	!	11/20/92 - Dan Perkins
	!		Added CASE 2 to OPT_SUBWIND so that VIEW would
	!		work properly.
	!
	!	04/01/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/20/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/19/98 - Kevin Handy
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

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.HB"
	MAP (SB_CONTROL)	SB_CONTROL_CDD		SB_CONTROL
	MAP (SB_CONTROL_OLD)	SB_CONTROL_CDD		SB_CONTROL_OLD
	MAP (SB_CONTROL_DEF)	SB_CONTROL_CDD		SB_CONTROL_DEF

	COM (TT_BA_CONTROL) &
		CLOSETITLE$ = 20%, &
		CLOSEFLAG$(5%) = 20%

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_SB_CONTROL) &
		SB_CONTROL.CH%, &
		SB_CONTROL.READONLY%

	!
	! Default System
	!
	DEF_SYSTEM$ = "JC"

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
		! Define SMG_WINDOW
		!
		SMG_WINDOW::DESCR = "Job Cost Controlling File"
		SMG_WINDOW::NHELP = "JC_MAIN_CONTROL"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 2%

		SMG_WINDOW::NKEYS = 1%

		CLOSETITLE$ = "Flag   Description"
		CLOSEFLAG$(0%) = "5"
		CLOSEFLAG$(1%) = "0    No status"
		CLOSEFLAG$(2%) = "1    Closing"
		CLOSEFLAG$(3%) = "2    Resetting"
		CLOSEFLAG$(4%) = "3    Resynchronize"
		CLOSEFLAG$(5%) = "4    Purge"

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF SB_CONTROL.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if it was
			! that way from the last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF SB_CONTROL.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			JC_MAIN_CONTROL = ERR
			CONTINUE 770
		END WHEN

		SB_CONTROL.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.OPN"
		USE
			JC_MAIN_CONTROL = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		SB_CONTROL.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(SB_CONTROL.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = SB_CONTROL.CH%
		WHEN ERROR IN
			RESET #SB_CONTROL.CH%
		USE
			CONTINUE 32767
		END WHEN

		GOSUB 28500

	!
	! Select function
	!
	CASE OPT_OPTLIST

		MVALUE = "Change Blank Help eXit"

	!
	! Display the background
	!
	! This option is used to display the background information on the
	! screen.  It must first clear any junk on the screen, and then
	! write the background onto it.
	!
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)


		DATA	03,03, "(01) Current Period", &
			04,03, "(02) Status Flag", &
			06,03, "     Date", &
			07,03, "     Time", &
			08,03, "     Batch #", &
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
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
	!
20200	CASE OPT_ENTRY

		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Current Period\*
	!	.b
	!	.lm +5
	!	The ^*Current Period\* field contains the period
	!	indicating where the system is at the present time.
	!	.lm -5
	!
	! Index:
	!	.x Current Period
	!
	!--
			SB_CONTROL::PERIOD = ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"03;36", TEMP$, SB_CONTROL::PERIOD, &
				MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Status Flag\*
	!	.b
	!	.lm +5
	!	The ^*Status Flag\* field assigns a flag for the major processes indicating
	!	their activity at the present time. This enables the user to locate the
	!	current processes being conducted.
	!	.B
	!	Current valid values are:
	!	.b
	!	.lm +5
	!	*0 Normal
	!	.br
	!	*1 Close in process
	!	.lm -5
	!	.lm -5
	!
	! Index:
	!	.x Status Flag
	!
	!--
			SB_CONTROL::CONTROLFLAG = EDIT$(ENTR_3STRINGLIST(SCOPE, SMG_WINDOW::WNUMBER, &
				"04;36", TEMP$, SB_CONTROL::CONTROLFLAG, &
				MFLAG, "'", MVALUE, CLOSEFLAG$(), &
				CLOSETITLE$, "005"), -1%)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		JC_MAIN_CONTROL = 0%

		SELECT MLOOP

		CASE 1%

			IF XLATE(SB_CONTROL::PERIOD, STRING$(48%, 0%) + &
				"0123456789") <> SB_CONTROL::PERIOD
			THEN
				JC_MAIN_CONTROL = 1%

				CALL ENTR_3MESSAGE(SCOPE, &
					"Bad Period(Must be a Number)", 1%)
			END IF

		END SELECT

	CASE OPT_DISPLAY

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			PRNT_DATE(SB_CONTROL::CDATE, 8%), 6%, 36%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			PRNT_TIME(SB_CONTROL::CTIME, 1%), 7%, 36%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			SB_CONTROL::BATCH, 8%, 36%, , SMG$M_BOLD)

	!
	! Set SB_CONTROL_OLD value
	!
20500	CASE OPT_SETOLD
		SB_CONTROL_OLD = SB_CONTROL

	!
	! Restore SB_CONTROL_OLD value
	!
	CASE OPT_RESETOLD
		SB_CONTROL = SB_CONTROL_OLD

	!
	! Find
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE SB_CONTROL::SYSTEM + "", &
				REGARDLESS
		END SELECT

	CASE OPT_SUBWIND

		SELECT MLOOP

		!
		! Find first record (if there is any)
		!
		CASE 1%

			!
			! Set init value
			!
			SMG_WINDOW::CURREC = -1%

27110			!
			! Search for first record
			!
			WHEN ERROR IN
				FIND #SMG_WINDOW::CHAN, &
					KEY #0% EQ DEF_SYSTEM$, &
					REGARDLESS

				!
				! Get a record
				!
				GET #SMG_WINDOW::CHAN
				SMG_WINDOW::CURREC = 0%
			USE
				CONTINUE 28000 IF ERR = 155%
				EXIT HANDLER
			END WHEN

		!
		! Find starting record (if there is any)
		!
		CASE 2%
			!
			! Set init value
			!
			SMG_WINDOW::CURREC = -1%

27115			!
			! Search for starting record
			!
			SELECT MFLAG

			CASE 0%
				WHEN ERROR IN
					FIND #SMG_WINDOW::CHAN, &
						KEY #0% GE SB_CONTROL::SYSTEM + "", &
						REGARDLESS
				USE
					CONTINUE 28000 IF ERR = 155%
					EXIT HANDLER
				END WHEN
			END SELECT

			!
			! Get a record
			!
			SMG_WINDOW::CURREC = 0%

		!
		! Check if still right key
		!
		CASE 3%
			SMG_WINDOW::CURREC = -1%
			SMG_WINDOW::CURREC = 0% &
				IF SB_CONTROL::SYSTEM = DEF_SYSTEM$


		END SELECT

	END SELECT

28000	EXIT FUNCTION

28500	!
	! Get control record
	!
	WHEN ERROR IN
		GET #SB_CONTROL.CH%, KEY #0% EQ DEF_SYSTEM$, REGARDLESS
	USE
		CONTINUE 28530
	END WHEN

	GOTO 28540

28530	!
	! Load in defaults for control file
	!
	SB_CONTROL::SYSTEM = DEF_SYSTEM$
	SB_CONTROL::PERIOD = ""
	SB_CONTROL::CONTROLFLAG = "0"
	SB_CONTROL::SUBJECT = "J"
	SB_CONTROL::CDATE = DATE_TODAY
	SB_CONTROL::CTIME = TIME_NOW
	SB_CONTROL::BATCH = "000000"

	PUT #SB_CONTROL.CH%

28540	RETURN

29000	!***************************************************************
	! Trap errors
	!***************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
	!+-+-+
	!++
	! Abstract:ACCOUNT
	!	^*Accounts\*
	!	.b
	!	.lm +5
	!	The ^*Accounts\* option enters the General Ledger Accounts
	!	to which the Job Costing system relates.  These accounts must first be
	!	established in the General Ledger Chart of Accounts.
	!	.lm -5
	!
	! Index:
	!	.x Accounts
	!
	!--
