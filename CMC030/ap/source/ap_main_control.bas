1	%TITLE "Maintain Accounts Payable Control Record"
	%SBTTL "AP_MAIN_CONTROL"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AP_MAIN_CONTROL(CDD_WINDOW_CDD SMG_WINDOW, &
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
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	When the Accounts Payable system is initialized, the ^*Maintain
	!	Accounts Payable Control Record\* option
	!	enters necessary information needed to maintain the control record.
	!	.b
	!	After the initialization procedures have been completed, it
	!	should never again be necessary to maintain or edit any field in
	!	the Accounts Payable Control Record. Any subsequent changes in
	!	the fields in the Control Record are system
	!	generated.
	!	.lm -5
	!
	! Index:
	!	.x Control Record
	!
	! Option:
	!
	!
	! Author:
	!
	!	08/02/88 - Lance Williams
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_MAIN_CONTROL/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AP_MAIN_CONTROL
	!	$ DELETE AP_MAIN_CONTROL.OBJ;*
	!
	! Modification history:
	!
	!	10/03/88 - Kevin Handy
	!		Fixed problem where program would die if
	!		control record did not already exist (added
	!		GOSUB 28000).
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	11/02/95 - Kevin Handy
	!		Added "accounTs" option, and hooked in function.
	!
	!	11/02/95 - Kevin Handy
	!		Reformatted lines so things looked correct.
	!
	!	08/13/96 - Kevin Handy
	!		Removed extra '&' before 'end if'.
	!
	!	08/14/96 - Kevin Handy
	!		Removed extra '&' before two more 'end if's.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/09/2000 - Kevin Handy
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
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"

	!
	! CDD and Maps
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.HB"
	MAP (AP_CONTROL)	AP_CONTROL_CDD	AP_CONTROL
	MAP (AP_CONTROL2)	AP_CONTROL_CDD	AP_CONTROL_OLD, AP_CONTROL2

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD	GL_CHART

	!
	! This common area must be mapped in both the main program and
	! in MAINT_GROUP.
	!
	COM (CH_AP_CONTROL) &
		AP_CONTROL.CH%, &
		AP_CONTROL.READONLY%, &
		CLOSETITLE$ = 20%, &
		CLOSETYPE$(4%) = 20%

	!
	! External functions
	!
	EXTERNAL LONG		FUNCTION MAIN_WINDOW
	EXTERNAL LONG		FUNCTION MAIN_JOURNAL
	EXTERNAL LONG		FUNCTION FUNC_TESTENTRY

	!
	! Declare data types
	!
	DECLARE LONG XPOS, YPOS

	!
	! Set up error trapping
	!
	ON ERROR GOTO 29000

	%PAGE

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
		SMG_WINDOW::DESCR = "Accounts Payable Control File Maintenance"
		SMG_WINDOW::NHELP = "AP_MAIN_CONTROL"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 10%
		SMG_WINDOW::FLAGS = 128%	! Relative file

		SMG_WINDOW::NKEYS = 0%

		!
		! Closing type
		!
		CLOSETITLE$ = "Type   Description"
		CLOSETYPE$(0%) = "3"
		CLOSETYPE$(1%) = "0    No status"
		CLOSETYPE$(2%) = "1    Closing"
		CLOSETYPE$(3%) = "2    Resetting"
		CLOSETYPE$(4%) = "3    Purging"

		!
		! Get info required for main file
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Declare channels
		!
		IF AP_CONTROL.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AP_CONTROL.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			AP_MAIN_CONTROL = ERR
			CONTINUE 770
		END WHEN

		AP_CONTROL.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.OPN"
		USE
			AP_MAIN_CONTROL = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		AP_CONTROL.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(AP_CONTROL.CH%)
		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = AP_CONTROL.CH%
		GOSUB 28000

	%PAGE


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
	!	The ^*accounTs\* option enters the Accounts Payable
	!	accounts that will be needed to complete Accounts Payable transactions.
	!	.lm -5
	!
	! Index:
	!	.x Accounts>AR
	!	.x AR>Accounts
	!
	!--

			V% = MAIN_JOURNAL(AP_MAIN_CONTROL_ACCT.ID, "")

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

		DATA	2, 7, "(01) Last Transaction Number", &
			4, 7, "(02) Last Check Number", &
			6, 7, "(03) AP Account Number", &
			8, 7, "(04) Discount Lost Account Number", &
			10,7, "(05) Cash Account Number", &
			12,7, "(06) # of periods in retention cycle", &
			13,7, "(07) Retain 1099 History Only", &
			14,7, "(08) Current fiscal year", &
			15,7, "(09) Last period closed", &
			16,7, "(10) Close Flag", &
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

 Reenter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Last Transaction Number\*
	!	.b
	!	.lm +5
	!	An Accounts Payable transaction number is system created each
	!	time a record is added in a Purchases Journal. When the Accounts
	!	Payable system is initialized, this field can be set to any number.
	!	The transaction numbers will increment, beginning with the set
	!	number + 1. The ^*Last Transaction Number\* in the Accounts Payable
	!	Control File will always record the last transaction number created.
	!	.note
	!	^*Except during the initialization procedures, this number
	!	should never be changed\*.
	!	.end note
	!	.lm -5
	!
	! Index:
	!	.x Last Transaction Number
	!	.x Transaction Number>Last
	!
	!--
			AP_CONTROL::LAST_TRANKEY= &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;44", TEMP$, &
				AP_CONTROL::LAST_TRANKEY, &
				MFLAG, "~L0'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Last Check Number\*
	!	.b
	!	.lm +5
	!	The ^*Last Check Number\* field
	!	records the number of the last check printed during the most
	!	recent execution of the check printing routine.
	!	.b
	!	There should never be any reason to change this field.
	!	.lm -5
	!
	! Index:
	!	.x Last Check Number
	!	.x Number>Last Check
	!
	!--
			AP_CONTROL::LAST_CKNUM = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;44", TEMP$, &
				AP_CONTROL::LAST_CKNUM, &
				MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Accounts Payable Account Number\*
	!	.b
	!	.lm +5
	!	During initialization procedures, the General Ledger Chart
	!	of Accounts number for Accounts Payable is to be entered in the
	!	^*Accounts Payable Account Number\* field of the Accounts Payable Control File.
	!	.b
	!	The presence of the Accounts Payable account number in this
	!	field will cause the number to be defaulted in appropriate fields
	!	in other areas of the Accounts Payable system.
	!	.lm -5
	!
	! Index:
	!	.x Accounts Payable Account Number
	!	.x Account Number>Accounts Payable
	!
	!--
			AP_CONTROL::AP_ACCT = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;44", TEMP$, &
				AP_CONTROL::AP_ACCT, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX  ") = 1%)
				THEN
					AP_CONTROL::AP_ACCT = &
						GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Discount Lost Account Number\*
	!	.b
	!	.lm +5
	!	During initialization procedures, the General Ledger Chart of
	!	Account number for Discounts Lost should be entered in the ^*Discount
	!	Lost Account Number\* field of the Accounts Payable Control File.
	!	.b
	!	The presence of the Discount Lost account number in this field
	!	will cause the number to be defaulted in appropriate fields in other
	!	areas of the Accounts Payable system.
	!	.lm -5
	!
	! Index:
	!	.x Discount Lost Account Number
	!	.x Account Number>Discount Lost
	!
	!--
			AP_CONTROL::DISCLOST_ACCT = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
					"8;44", TEMP$, &
					AP_CONTROL::DISCLOST_ACCT, &
					MFLAG, "'E", MVALUE)

				IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
				THEN
					IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX  ") = 1%)
					THEN
						AP_CONTROL::DISCLOST_ACCT = &
							GL_CHART::ACCT
					END IF
				GOTO Reenter
			END IF

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Cash Account Number\*
	!	.b
	!	.lm +5
	!	During initialization procedures, the General Ledger Chart
	!	of account number for the cash account, representing the bank
	!	from which funds are drawn in payment of accounts payable, ^*must\* be
	!	entered in the ^*Cash Account Number\* field of the Accounts Payable
	!	Control File.
	!	.b
	!	The presence of the Cash account number in this field will
	!	cause the number to be defaulted in appropriate fields in other
	!	areas of the Accounts Payable system.
	!	.lm -5
	!
	! Index:
	!	.x Cash Account Number>Control Record
	!
	!--
			AP_CONTROL::CASH_ACCT = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;44", TEMP$, &
				AP_CONTROL::CASH_ACCT, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX  ") = 1%)
				THEN
					AP_CONTROL::CASH_ACCT = &
						GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Number of periods in retention cycle\*
	!	.b
	!	.lm +5
	!	The ^*Number of periods in retention cycle\* field
	!	enters a value
	!	indicating the number of accounting periods for which accounts
	!	payable historical information will be retained.
	!	.b
	!	Theoretically, the system would accommodate retaining
	!	historical information for literally tens of thousands of periods.
	!	Disk storage space can be a limiting factor.
	!	.b
	!	From a practical point of view, it is suggested that Accounts
	!	Payable historical information be retained from fifteen (15) to
	!	twenty-six (26) periods.
	!	.b
	!	If the retention cycle is to be for twenty (20) periods, the
	!	value "20" would be entered in this field.
	!	.lm -5
	!
	! Index:
	!	.x Retention Cycle>Periods>Control Record
	!	.x Periods>Retention Cycle>Control Record
	!	.x Control Record>Retention Cycle>Periods
	!
	!--
			AP_CONTROL::RETAIN = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"12;44", TEMP$, &
				AP_CONTROL::RETAIN * 1.0, MFLAG, "##", MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	^*(07) Retain 1099 Only\*
	!	.b
	!	.lm +5
	!	The ^*Retain 1099 Only\* field is used to control the amount of
	!	detail that is saved in the AP closed file when a close process
	!	is executed.  The retain 1099 only flag, if set to ^*Y\* will save
	!	only data for vendors that have the 1099 flag set to ^*Y\*.
	!	.lm -5
	!
	! Index:
	!	.x Retain 1099 Only>Control
	!	.x Control>Retain 1099 Only
	!
	!--
			AP_CONTROL::RETAIN_1099_ONLY = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "13;44", TEMP$, &
				AP_CONTROL::RETAIN_1099_ONLY, &
				MFLAG, "'E", MVALUE)

		CASE 8%

	!++
	! Abstract:FLD008
	!	.x Current Year>Control
	!	^*(08) Current Fiscal Year\*
	!	.b
	!	.lm +5
	!	The ^*Current Fiscal Year\* field
	!	enters, during the initialization of the
	!	system, the current fiscal year.  Thereafter, the system will
	!	automatically maintain the value in the field.
	!	.lm -5
	!
	! Index:
	!	.x Control>Current Year
	!
	!--
			AP_CONTROL::YEAR = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"14;44", TEMP$, &
				AP_CONTROL::YEAR, MFLAG, "'E", MVALUE)

		CASE 9%

	!++
	! Abstract:FLD009
	!	^*(09) Last Period Closed\*
	!	.b
	!	.lm +5
	!	The ^*Last period closed\* field
	!	contains the number of the accounting period in which the
	!	accounts payable was closed.
	!	.b
	!	When the Accounts Payable system is initialized, the contents
	!	of this field should be one period prior to the period in which
	!	the system is initialized.  For example, if the system were being
	!	initialized in the fifth period of the fiscal year, this field
	!	should be set to "4".
	!	^*
	!	.note
	!	^*After completing the initialization procedures, this
	!	field should ^&never\& again be edited.
	!	.end note
	!
	! Index:
	!	.x Last Period Closed>Control
	!	.x Control>Last Period Closed
	!
	!--
			AP_CONTROL::LASTPERCLOSE = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"15;44", TEMP$, &
				AP_CONTROL::LASTPERCLOSE * 1.0, MFLAG, "##", MVALUE)

		CASE 10%

	!++
	! Abstract:FLD010
	!	^*(10) Close Flag\*
	!	.b
	!	.lm +5
	!	The ^*Close Flag\* field
	!	is always system generated.
	!	.b
	!	The flag types and associated descriptions are:
	!	.b 1
	!	.lm 15
	!	^&Type\& #^&Description\&
	!	.b
	!	##^*0\* ##No status
	!	.br
	!	##^*1\* ##Closing
	!	.br
	!	##^*2\* ##Resetting
	!	.br
	!	##^*3\* ##Purging
	!	.lm -5
	!	.b
	!	^*There is ^&never\& any need to initialize or change a value in
	!	this field.
	!
	! Index:
	!	.x Close Flag>Control
	!	.x Control>Close Flag
	!
	!--
			AP_CONTROL::CLOSEFLAG = EDIT$(ENTR_3STRINGLIST(SCOPE, SMG_WINDOW::WNUMBER, &
				"16;45", TEMP$, AP_CONTROL::CLOSEFLAG, &
				MFLAG, "'", MVALUE, CLOSETYPE$(), &
				CLOSETITLE$, "005"), -1%)

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

		!
		! Test values
		!
20300		CASE OPT_TESTENTRY
			AP_MAIN_CONTROL = 0%

			SELECT MLOOP

			CASE 3%, 4%, 5%
				!
				! Is the input defined?
				!
				TEMP$ = AP_CONTROL::AP_ACCT
				TEMP$ = AP_CONTROL::CASH_ACCT IF MLOOP = 5%
				TEMP$ = STRING$(LEN(TEMP$), 63%) IF TEMP$ = ""
				TEMP$ = AP_CONTROL::DISCLOST_ACCT IF MLOOP = 4%

				AP_MAIN_CONTROL = FUNC_TESTENTRY(SMG_WINDOW, &
					TEMP$, GL_CHART::DESCR, &
					"AP", MLOOP, "ACCT", &
					"Account number", GL_MAIN_CHART.ID)

				IF MLOOP = 3%
				THEN
					SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
						GL_CHART::DESCR, 7%, 40%,, SMG$M_BOLD)
				END IF

				IF MLOOP = 4%
				THEN
					SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
						GL_CHART::DESCR, 9%, 40%,, SMG$M_BOLD)
				END IF

				IF MLOOP = 5%
				THEN
					SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
						GL_CHART::DESCR, 11%, 40%,, SMG$M_BOLD)
				END IF

			END SELECT

		!
		! Set AP_CONTROL_OLD value
		!
20500		CASE OPT_SETOLD
			AP_CONTROL_OLD = AP_CONTROL

		!
		! Restore AP_CONTROL_OLD value
		!
		CASE OPT_RESETOLD
			AP_CONTROL = AP_CONTROL_OLD

		!
		! Set default value
		!
		CASE OPT_SETDEFAULT
			AP_CONTROL2 = AP_CONTROL

		!
		! Restore default value
		!
		CASE OPT_RESETDEFAULT
			AP_CONTROL = AP_CONTROL2

		END SELECT

 ExitFunction:
	EXIT FUNCTION

28000	!
	! Get period record
	!
	WHEN ERROR IN
		GET #AP_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		CONTINUE 28030
	END WHEN

	GOTO 28040

28030	!
	! Load in defaults for period file
	!
	AP_CONTROL::LAST_TRANKEY = "000000"
	AP_CONTROL::LAST_CKNUM = "000000"
	AP_CONTROL::CLOSEFLAG = "0"
	AP_CONTROL::LASTPERCLOSE = 0%
	AP_CONTROL::YEAR = DATE_TODAY
	AP_CONTROL::RETAIN = 18%
	AP_CONTROL::RETAIN_1099_ONLY = "N"

	WHEN ERROR IN
		PUT #AP_CONTROL.CH%, RECORD 1%
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
