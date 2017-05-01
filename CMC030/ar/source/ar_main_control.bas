1	%TITLE "Maintain Accounts Receivable Control Record"
	%SBTTL "AR_MAIN_CONTROL"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AR_MAIN_CONTROL(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	The ^*Maintain Accounts Receivable Control Record\* is provided to establish
	!	several different records and dates that once are established need not be
	!	accessed again.
	!	.lm -5
	!
	! Index:
	!	.x Maintain>Control Record
	!	.x Control Record>Maintain
	!	.x Maintain>Control
	!	.x Control>Maintain
	!
	! Option:
	!
	! Author:
	!
	!	11/28/88 - J. Shad Rydalch
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_MAIN_CONTROL/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP AR_MAIN_CONTROL
	!	$ DELETE AR_MAIN_CONTROL.OBJ;*
	!
	! Modification history:
	!
	!	04/15/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/21/92 - Kevin Handy
	!		Clean up (check)
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/25/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/13/96 - Kevin Handy
	!		Lose extra '&' before 'end if'
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
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
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD	AR_CONTROL
	MAP (AR_CONTROL2)	AR_CONTROL_CDD	AR_CONTROL_OLD, AR_CONTROL2

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD	GL_CHART

	!
	! This common area must be mapped in both the main program and
	! in MAINT_GROUP.
	!
	COM (CH_AR_CONTROL) &
		AR_CONTROL.CH%, &
		AR_CONTROL.READONLY%

	COM (TT_AR_MAIN_CONTROL) &
		CONTROLTITLE$ = 25%, &
		CONTROLTYPE$(4%) = 25%, &
		ECTITLE$ = 32%, &
		EC$(2%) = 32%


	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION MAIN_JOURNAL
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY

	ON ERROR GOTO 29000

	%PAGE

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
		SMG_WINDOW::DESCR = "AR Control File Maintenance"
		SMG_WINDOW::NHELP = "AR_MAIN_CONTROL"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 17%
		SMG_WINDOW::FLAGS = 128%
		! Relative file

		SMG_WINDOW::NKEYS = 0%

		!
		! Control type
		!
		CONTROLTITLE$ = "Type   Description"
		CONTROLTYPE$(0%) = "4"
		CONTROLTYPE$(1%) = "0    Normal State"
		CONTROLTYPE$(2%) = "1    Close in process"
		CONTROLTYPE$(3%) = "2    Reset in process"
		CONTROLTYPE$(4%) = "3    Purge in process"

		!
		! List of types
		!
		ECTITLE$ = "Method Description"
		EC$(0%) = "2"
		EC$(1%) = "O    Open Billing"
		EC$(2%) = "B    Balance Forward Billing"


700		!
		! Declare channels
		!
		IF AR_CONTROL.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AR_CONTROL.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			AR_MAIN_CONTROL = ERR
			CONTINUE 770
		END WHEN

		AR_CONTROL.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.OPN"
		USE
			AR_MAIN_CONTROL = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		AR_CONTROL.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(AR_CONTROL.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = AR_CONTROL.CH%
		GOSUB 28000

	!
	! Handle the option list
	!
	CASE OPT_OPTLIST
		MVALUE = "Change Blank Help eXit accounTs"

	CASE OPT_MOREMENU

		SELECT SCOPE::PRG_ITEM

		CASE "accounTs"
	!++
	! Abstract:ACCOUNTS
	!	^*accounTs\*
	!	.B
	!	.LM +5
	!	The ^*accounTs\* option enters the Accounts Receivable
	!	accounts that will be needed to complete Accounts Receivable transactions.
	!	.lm -5
	!
	! Index:
	!	.x Accounts>AR
	!	.x AR>Accounts
	!
	!--

			V% = MAIN_JOURNAL(AR_MAIN_CONTROL_ACCT.ID, "")

		END SELECT

	!
	! Display the background
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	1, 1, "(01) AR Account", &
			3, 1, "(02) Retention", &
			5, 1, "(03) Last Period", &
			7, 1, "(04) Year", &
			9, 1, "(05) Control Flag", &
			11,1, "(06) Title", &
			13,1, "(07) Method", &
			4, 41, "First  (08)", &
			4, 58, "(09)", &
			5, 41, "Second (10)", &
			5, 58, "(11)", &
			6, 41, "Third  (12)", &
			6, 58, "(13)", &
			7, 41, "Fourth (14)", &
			7, 58, "(15)", &
			8, 41, "Fifth  (16)", &
			8, 58, "(17)", &
			2, 41, "Period", &
			2, 52, "Days", &
			2, 63, "Title", &
			3, 41, "------", &
			3, 52, "----", &
			3, 63, "----------------", &
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
	! Enter/Display/Default
	!
20200	CASE OPT_ENTRY
		TEMP$ = TRM$(SCOPE::PRG_ITEM)

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 Reenter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Accounts Receivable Account\*
	!	.b
	!	.lm +5
	!	During initialization procedures, the General Ledger Chart of
	!	Accounts number for Accounts Receivable is to be entered in the
	!	^*Accounts Receivable Account Number\* field of the Accounts Receivable Control
	!	File.
	!	.b
	!	The presence of the Accounts Receivable account number in this
	!	field will cause the number to be defaulted in appropriate fields
	!	in other areas of the Accounts Receivable System.
	!	.b
	!	Valid account numbers may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x AR Account>Control
	!	.x Control>AR Account
	!
	!--
			AR_CONTROL::AR_ACCT = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"1;19", TEMP$, &
				AR_CONTROL::AR_ACCT, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX  ") = 1%)
				THEN
					AR_CONTROL::AR_ACCT = &
						GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

		CASE 2%
	!++
	! Abstract:FLD002
	!	.x Control>Retention
	!	^*(02) Retention\*
	!	.b
	!	.lm +5
	!	The ^*Retention\* field in the Accounts Receivable System
	!	enters a value indicating the number of accounting
	!	periods for which accounts receivable historical information will
	!	be retained.
	!	.b
	!	Theoretically, the system would accommodate retaining historical
	!	information for literally tens of thousands of periods. Disk storage
	!	space could be a limiting factor.
	!	.b
	!	From a practical point of view, it is suggested that Accounts
	!	Receivable historical information be retained from fifteen (15)
	!	to twenty-six (26) periods.
	!	.lm -5
	!
	! Index:
	!	.x Retention>Control
	!
	!--
			AR_CONTROL::RETAIN = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"3;19", TEMP$, &
				AR_CONTROL::RETAIN * 1.0, &
				MFLAG, "##", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Last Period Closed\*
	!	.b
	!	.lm +5
	!	The ^*Last Period Closed\* field contains the number of the
	!	accounting period in which the Accounts Receivable was closed.
	!	.b
	!	When the Accounts Receivable system is initialized,
	!	this field should be one period prior to the period in which
	!	the system is initialized. For example, if the system were
	!	being initialized in the fifth period of the fiscal year, this
	!	field should be set to "4".
	!	.b
	!	^*Note:\* After completing the initialization
	!	procedures, this field should ^*not\* be edited.
	!	.lm -5
	!
	! Index:
	!	.x Last Period Closed>Control
	!	.x Control>Last Period Closed
	!
	!--
			AR_CONTROL::LASTPERCLOSE = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;19", TEMP$, &
				AR_CONTROL::LASTPERCLOSE * 1.0, &
				MFLAG, "##", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	.x Control>Year
	!	^*(04) Year\*
	!	.b
	!	.lm +5
	!	The ^*Year\* field indicates the year in which the
	!	Accounts Receivable System is put into operation.
	!	.b
	!	The format for entry is YYYY.
	!	.b
	!	This field will automatically increment to the next year when
	!	the last month of the year has been closed.
	!	.lm -5
	!
	! Index:
	!	.x Year>Control
	!
	!--
			AR_CONTROL::YEAR = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"7;19", TEMP$, &
				AR_CONTROL::YEAR, &
				MFLAG, "'E", MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	.x Control>Control Flag
	!	^*(05) Control Flag\*
	!	.b
	!	.lm +5
	!	The ^*Control Flag\* field is always system
	!	generated.
	!	.b
	!	Valid flags are:
	!	.table 3,25
	!	.te
	!	^*0\* - Normal State
	!	.te
	!	^*1\* - Close in Process
	!	.te
	!	^*2\* - Reset in Process
	!	.te
	!	^*3\* - Purge in Process
	!	.end table
	!	^*Note:\* There is never any need to initialize
	!	or change a value in this field.
	!	.b
	!	Valid flags may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Control Flag>Control
	!
	!--
			AR_CONTROL::CLOSEFLAG = &
				ENTR_3STRINGLIST(SCOPE, SMG_WINDOW::WNUMBER, &
				"9;19", TEMP$, &
				AR_CONTROL::CLOSEFLAG, &
				MFLAG, "'", MVALUE, CONTROLTYPE$(), &
				CONTROLTITLE$, "005")

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Title\*
	!	.b
	!	.lm +5
	!	The ^*Title\* field is used to determine how the Name/Address
	!	file will be titled, "Customer number", "Patient number", "Client
	!	number", or some other designation. Related reports and screens
	!	will be titled accordingly.
	!	.lm -5
	!
	! Index:
	!	.x Title>Control
	!	.x Control>Title
	!
	!--
			AR_CONTROL::CTITLE = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"11;19", TEMP$, &
				AR_CONTROL::CTITLE, &
				MFLAG, "'E", MVALUE)

		CASE 7%
	!++
	! Abstract:FLD007
	!	.x Method>Control
	!	^*(07) Method\*
	!	.b
	!	.lm +5
	!	The ^*Method\* field defines the default
	!	accounting method for customers.
	!	.b
	!	Valid values are:
	!	.table 3,25
	!	.te
	!	^*O\* - Open item
	!	.te
	!	^*B\* - Balance forward
	!	.end table
	!	Valid methods may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Method>Control
	!	.x Method>Control
	!	.x Method>Control
	!
	!--
			AR_CONTROL::METHOD = ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, "13;19", &
				TEMP$ + " (O/B)", AR_CONTROL::METHOD, MFLAG, &
				"'", MVALUE, EC$(), ECTITLE$, "007")

		CASE 8%
		SCOPE::PRG_ITEM = "FLD008PDT"
	!++
	! Abstract:FLD008PDT
	!	^*(08) - (17) Period, Days and Title\*
	!	.b
	!	.lm +5
	!	The ^*Period, Days and Title\* fields
	!	establishes the intervals (number of days) and related aging
	!	description as pertains to aging schedules of Account Receivable.
	!	.b
	!	The example below shows the most common aging interval:
	!	.table 3,25
	!	.te
	!	^*Period######Days####Title\*
	!	.tE
	!	First  (08) ^*30\* (09) ^*Current\*
	!	.tE
	!	Second (10) ^*30\* (11) ^*31 to 60 days\*
	!	.tE
	!	Third  (12) ^*30\* (13) ^*61 to 90 days\*
	!	.tE
	!	Fourth (14) ^*30\* (15) ^*91 to 120 days\*
	!	.tE
	!	Fifth  (16) ^*30\* (17) ^*121 and over\*
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.X Period>Control
	!	.x Title>Control
	!	.x Days>Control
	!	.x Intervals>Control
	!	.X Control>Period
	!	.x Control>Title
	!	.x Control>Days
	!	.x Control>Intervals
	!
	!--
			AR_CONTROL::AGEPER(0) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;53", TEMP$, &
				AR_CONTROL::AGEPER(0) * 1.0, &
				MFLAG, "##", MVALUE)

		CASE 9%
			SCOPE::PRG_ITEM = "FLD008PDT"
			AR_CONTROL::AGENAM(0) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;63", TEMP$, &
				AR_CONTROL::AGENAM(0), &
				MFLAG, "'E", MVALUE)

		CASE 10%
			SCOPE::PRG_ITEM = "FLD008PDT"
			AR_CONTROL::AGEPER(1) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;53", TEMP$, &
				AR_CONTROL::AGEPER(1) * 1.0, &
				MFLAG, "##", MVALUE)

		CASE 11%
			SCOPE::PRG_ITEM = "FLD008PDT"
			AR_CONTROL::AGENAM(1) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;63", TEMP$, &
				AR_CONTROL::AGENAM(1), &
				MFLAG, "'E", MVALUE)

		CASE 12%
			SCOPE::PRG_ITEM = "FLD008PDT"
			AR_CONTROL::AGEPER(2) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;53", TEMP$, &
				AR_CONTROL::AGEPER(2) * 1.0, &
				MFLAG, "##", MVALUE)

		CASE 13%
			SCOPE::PRG_ITEM = "FLD008PDT"
			AR_CONTROL::AGENAM(2) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;63", TEMP$, &
				AR_CONTROL::AGENAM(2), &
				MFLAG, "'E", MVALUE)

		CASE 14%
			SCOPE::PRG_ITEM = "FLD008PDT"
			AR_CONTROL::AGEPER(3) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"7;53", TEMP$, &
				AR_CONTROL::AGEPER(3) * 1.0, &
				MFLAG, "##", MVALUE)

		CASE 15%
			SCOPE::PRG_ITEM = "FLD008PDT"
			AR_CONTROL::AGENAM(3)= &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"7;63", TEMP$, &
				AR_CONTROL::AGENAM(3), &
				MFLAG, "'E", MVALUE)

		CASE 16%
			SCOPE::PRG_ITEM = "FLD008PDT"
			AR_CONTROL::AGEPER(4) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"8;53", TEMP$, &
				AR_CONTROL::AGEPER(4) * 1.0, &
				MFLAG, "##", MVALUE)

		CASE 17%
			SCOPE::PRG_ITEM = "FLD008PDT"
			AR_CONTROL::AGENAM(4) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"8;63", TEMP$, &
				AR_CONTROL::AGENAM(4), &
				MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		AR_MAIN_CONTROL = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Is the input defined?
			!
			AR_MAIN_CONTROL = FUNC_TESTENTRY(SMG_WINDOW, &
				AR_CONTROL::AR_ACCT, GL_CHART::DESCR, &
				"AR", MLOOP, "ACCT", &
				"Account number", GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 1%, 40%,, SMG$M_BOLD)

		END SELECT


		!
		! Set AR_CONTROL_OLD value
		!
20500		CASE OPT_SETOLD
			AR_CONTROL_OLD = AR_CONTROL

		!
		! Restore AR_CONTROL_OLD value
		!
		CASE OPT_RESETOLD
			AR_CONTROL = AR_CONTROL_OLD

		!
		! Set default value
		!
		CASE OPT_SETDEFAULT
			AR_CONTROL2 = AR_CONTROL

		!
		! Restore default value
		!
		CASE OPT_RESETDEFAULT
			AR_CONTROL = AR_CONTROL2

		!
		! Display additional info
		!
		CASE OPT_DISPLAY
			!
			! Is the input defined?
			!
			IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
			THEN
				TEMP$ = STRING$(40%, A"?"B)
				TEMP$ = GL_CHART::DESCR &
					IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
					"Q0" + AR_CONTROL::AR_ACCT) = 1%
				SMG_STATUS% = SMG$PUT_CHARS( &
					SMG_WINDOW::WNUMBER, &
					TEMP$, 1%, 40%,, SMG$M_BOLD)
			END IF

	END SELECT

	EXIT FUNCTION

28000	!
	! Get period record
	!
	WHEN ERROR IN
		GET #AR_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		CONTINUE 28030
	END WHEN

	GOTO 28040

28030	!
	! Load in defaults for period file
	!
	AR_CONTROL::AR_ACCT = ""
	AR_CONTROL::RETAIN = 0%
	AR_CONTROL::LASTPERCLOSE = 0%
	AR_CONTROL::YEAR = ""
	AR_CONTROL::CLOSEFLAG = "0"
	AR_CONTROL::CTITLE = "Customer"
	AR_CONTROL::AGEPER(0) = 30%
	AR_CONTROL::AGENAM(0) = "Current"
	AR_CONTROL::AGEPER(1) = 30%
	AR_CONTROL::AGENAM(1) = "31 to 60 days"
	AR_CONTROL::AGEPER(2) = 30%
	AR_CONTROL::AGENAM(2) = "61 to 90 days"
	AR_CONTROL::AGEPER(3) = 30%
	AR_CONTROL::AGENAM(3) = "91 to 120 days"
	AR_CONTROL::AGEPER(4) = 30%
	AR_CONTROL::AGENAM(4) = "121 and over"
	AR_CONTROL::METHOD = "O"

	WHEN ERROR IN
		PUT #AR_CONTROL.CH%, RECORD 1%
	USE
		CALL ENTR_3MESSAGE(SCOPE, "Unable to add period record", 0%)
		CONTINUE 32767
	END WHEN


28040	RETURN

29000	!****************************************************************
	! Trap errors
	!****************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
