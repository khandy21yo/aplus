1	%TITLE "WIP Controlling File Maintenance"
	%SBTTL "WP_MAIN_CONTROL"
	%IDENT "V3.6a Calico"

	FUNCTION LONG WP_MAIN_CONTROL(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1991 BY
	!
	! Computer Management Center
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
	!	The ^*Controlling File\* screen is used to display and allow
	!	change to the last order number and entry of the last purge date.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_MAIN_CONTROL/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN WP_MAIN_CONTROL
	!	$ DELETE WP_MAIN_CONTROL.OBJ;*
	!
	! Author:
	!
	!	05/28/91 - Val James "kook" Allen
	!
	! Modification history:
	!
	!	07/24/91 - Craig Tanner
	!		Added feild 2, Requisition number, to file.
	!
	!	07/16/92 - Dan Perkins
	!		Drop leading zeros on Order Number.
	!		RSET and drop zeros from Requisiton Number.
	!
	!	07/21/92 - Dan Perkins
	!		Added second page to accomodate added account fields.
	!
	!	06/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/29/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to "" in
	!		several places.
	!
	!	08/28/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/09/99 - Kevin Handy
	!		Lose lines 760, 770 (Dead Code)
	!
	!	11/06/2000 - Kevin Handy
	!		Use A"x"B
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include Files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[WP.OPEN]WP_CONTROL.HB"
	MAP (WP_CONTROL)	WP_CONTROL_CDD		WP_CONTROL
	MAP (WP_CONTROL_OLD)	WP_CONTROL_CDD		WP_CONTROL_OLD, &
							WP_CONTROL2

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	! Common Statements
	!
	COM (CH_WP_CONTROL) &
		WP_CONTROL.CH%, &
		WP_CONTROL.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_WINDOW
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!
	! Initilization
	!
	CASE OPT_INIT

		!******************************************************************
		! Set up information
		!******************************************************************

		!
		! Define window
		!
		SMG_WINDOW::DESCR  = "WIP Controlling File"
		SMG_WINDOW::NHELP  = "WP_MAIN_CONTROL"
		SMG_WINDOW::CHAN   = WP_CONTROL.CH%
		SMG_WINDOW::HSIZE  = 78%
		SMG_WINDOW::VSIZE  = 18%
		SMG_WINDOW::HPOS   = 2%
		SMG_WINDOW::VPOS   = 2%
		SMG_WINDOW::FLAGS  = 128% + 4%!Relative file
		SMG_WINDOW::NITEMS = 12%

		SMG_WINDOW::NKEYS  = 0%

		SMG_WINDOW::LWIDTH  = 78%
		SMG_WINDOW::LHEIGHT = 18%
		SMG_WINDOW::LHPOS   = 2%
		SMG_WINDOW::LVPOS   = 2%
		SMG_WINDOW::LLAST   = 1%
		SMG_WINDOW::LTITLE(0%) = "First Page"
		SMG_WINDOW::LPAGE(0%)  = 4%
		SMG_WINDOW::LTITLE(1%) = "Last Page"
		SMG_WINDOW::LPAGE(1%)  = 12%

		CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Declare channels
		!
		IF WP_CONTROL.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF WP_CONTROL.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		%INCLUDE "SOURCE:[WP.OPEN]WP_CONTROL.CRE"
		WP_CONTROL.READONLY% = 0%
		GOTO 790

 !760
		!
		! If unable to open for modify, try to open
		! with read access only.
		!
 !		%INCLUDE "SOURCE:[WP.OPEN]WP_CONTROL.OPN"
 !		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
 !		WP_CONTROL.READONLY% = -1%
 !
 !		GOTO 790

770		!
		! File not able to open, so reset channel
		!
 !		CALL ASSG_FREECHANNEL(WP_CONTROL.CH%)
 !		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = WP_CONTROL.CH%
		GOSUB 28000

	!
	! Display window background
	!
	CASE OPT_BACKGROUND

		SELECT MLOOP
		!
		! Main screen
		!
		CASE 0%

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)


		DATA	04,  09, "(01) Last Order Number", &
			05,  09, "(02) Last Requisition Number", &
			06,  09, "(03) Last Close/Purge Date", &
			07,  09, "(04) Activity Status Flag", &
			0,   0, ""

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
		! 2nd page
		!
		CASE 1%
			SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::LWINDOW)

			SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::LWINDOW)

			DATA	5,  3, "(05) Material Usage Acct", &
				6,  3, "(06) Labor Rate Acct", &
				7,  3, "(07) Labor Efficiency Acct", &
				8,  3, "(08) Burden Acct", &
				11, 3, "(09) Material Usage Acct", &
				12, 3, "(10) Labor Rate Acct", &
				13, 3, "(11) Labor Efficiency Acct", &
				14, 3, "(12) Burdern Acct", &
				4,  1, "INVENTORY VARIANCE ACCOUNTS", &
				10, 1, "EQUIPMENT VARIANCE ACCOUNTS", &
				0,  0, ""

			RESTORE

			XPOS% = -1%

			READ XPOS%, YPOS%, XSTR$ UNTIL XPOS% = 0%
			READ XPOS%, YPOS%, XSTR$

		I% = SMG_WINDOW::LPAGE(0%)

		WHILE (XPOS% <> 0%)
			I% = I% + 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				XSTR$, XPOS%, YPOS%) &
				IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%

			READ XPOS%, YPOS%, XSTR$
		NEXT

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::LWINDOW)

		END SELECT

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
	!++
	! Abstract:FLD001
	!	^*(01) Last Order Number\*
	!	.b
	!	.lm +5
	!	The ^*Last Order Number\* field contains the next Order number which will be
	!	assigned during the journal entry.
	!	.b
	!	An entry is required in this field. The field will
	!	accommodate ten (10) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Order Number
	!	.x Last Order Number
	!
	!--
			WP_CONTROL::ORDNUM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"04;46", TEMP$, WP_CONTROL::ORDNUM, &
				MFLAG, "'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Last Requisition Number\*
	!	.b
	!	.lm +5
	!	The ^*Last Requisition Number\* field contains the next Requisition number
	!	which will be assigned during the journal entry.
	!	.b
	!	An entry is required in this field. The field will
	!	accommodate ten (10) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Requisition Number
	!	.x Last Requisition Number
	!
	!--
			WP_CONTROL::REQNUM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"05;46", TEMP$, WP_CONTROL::REQNUM, &
				MFLAG OR 2%, "~R 'E", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Last Closed/Purge Date\*
	!	.b
	!	.lm +5
	!	The ^*Last Closed/Purge Date\* field contains the date in which
	!	the orders were closed and purged.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Last Purge Date
	!	.x Last Closed Date
	!
	!--
			WP_CONTROL::PURGDATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;46", TEMP$, WP_CONTROL::PURGDATE, &
				MFLAG, "'E", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Activity Status Flag\*
	!	.b
	!	.lm +5
	!	The ^*Activity Status Flag\* field contains the flag which
	!	indicates that the processes are normal (flag = 0) or that
	!	the Close/purge process is underway (flag = 1).
	!	No Posting/updating of the Order register file will be allowed
	!	if the status flag is other than 0.
	!	.lm -5
	!
	! Index:
	!	.x Activity Status Flag
	!
	!--
			WP_CONTROL::STATUS_FLAG = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"07;46", TEMP$, WP_CONTROL::STATUS_FLAG, &
				MFLAG, "'E", MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Inventory Material Usage Variance Account\*
	!	.b
	!	.lm +5
	!	The ^*Inventory Material Usage Variance Account\* field
	!	enters the General Ledger Account number for the
	!	Inventory Material Usage Variance Account.
	!	.b
	!	Valid Account Numbers may be viewed by pressing ^*List Choices\*.
	!	.b
	!	This field will accept up to eighteen (18) alphanummeric
	!	characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			WP_CONTROL::INVMATUVAR = ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"05;30", TEMP$, WP_CONTROL::INVMATUVAR, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%
				THEN
					WP_CONTROL::INVMATUVAR = &
						GL_CHART::ACCT
				END IF
				GOTO ReEnter
			END IF

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Inventory Labor Rate Variance Account\*
	!	.b
	!	.lm +5
	!	The ^*Inventory Labor Rate Variance Account\* field
	!	enters the General Ledger Account number for the
	!	Inventory Labor Rate Variance Account.
	!	.b
	!	Valid Account Numbers may be viewed by pressing ^*List Choices\*.
	!	.b
	!	This field will accept up to eighteen (18) alphanummeric
	!	characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			WP_CONTROL::INVLABRVAR = ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"06;30", TEMP$, WP_CONTROL::INVLABRVAR, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%
				THEN
					WP_CONTROL::INVLABRVAR = &
						GL_CHART::ACCT
				END IF
				GOTO ReEnter
			END IF

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(07) Inventory Labor Efficiency Variance Account\*
	!	.b
	!	.lm +5
	!	The ^*Inventory Labor Efficiency Variance Account\* field
	!	enters the General Ledger Account number for the
	!	Inventory Labor Efficiency Variance Account.
	!	.b
	!	Valid Account Numbers may be viewed by pressing ^*List Choices\*.
	!	.b
	!	This field will accept up to eighteen (18) alphanummeric
	!	characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			WP_CONTROL::INVLABEVAR = ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"07;30", TEMP$, WP_CONTROL::INVLABEVAR, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%
				THEN
					WP_CONTROL::INVLABEVAR = &
						GL_CHART::ACCT
				END IF
				GOTO ReEnter
			END IF

		CASE 8%
	!++
	! Abstract:FLD008
	!	^*(08) Inventory Burden Variance Account\*
	!	.b
	!	.lm +5
	!	The ^*Inventory Burden Variance Account\* field
	!	enters the General Ledger Account number for the
	!	Inventory Burden Variance Account.
	!	.b
	!	Valid Account Numbers may be viewed by pressing ^*List Choices\*.
	!	.b
	!	This field will accept up to eighteen (18) alphanummeric
	!	characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			WP_CONTROL::INVBURVAR = ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"08;30", TEMP$, WP_CONTROL::INVBURVAR, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%
				THEN
					WP_CONTROL::INVBURVAR = &
						GL_CHART::ACCT
				END IF
				GOTO ReEnter
			END IF

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*(09) Equipment Material Usage Variance Account\*
	!	.b
	!	.lm +5
	!	The ^*Equipment Material Usage Variance Account\* field
	!	enters the General Ledger Account number for the
	!	Equipment Material Usage Variance Account.
	!	.b
	!	Valid Account Numbers may be viewed by pressing ^*List Choices\*.
	!	.b
	!	This field will accept up to eighteen (18) alphanumeric
	!	characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			WP_CONTROL::EQMATUVAR = ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"11;30", TEMP$, WP_CONTROL::EQMATUVAR, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%
				THEN
					WP_CONTROL::EQMATUVAR = &
						GL_CHART::ACCT
				END IF
				GOTO ReEnter
			END IF

		CASE 10%
	!++
	! Abstract:FLD010
	!	^*(10) Equipment Labor Rate Variance Account\*
	!	.b
	!	.lm +5
	!	The ^*Equipment Labor Rate Variance Account\* field
	!	enters the General Ledger Account number for the
	!	Equipment Labor Rate Variance Account.
	!	.b
	!	Valid Account Numbers may be viewed by pressing ^*List Choices\*.
	!	.b
	!	This field will accept up to eighteen (18) alphanumeric
	!	characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			WP_CONTROL::EQLABRVAR = ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"12;30", TEMP$, WP_CONTROL::EQLABRVAR, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%
				THEN
					WP_CONTROL::EQLABRVAR = &
						GL_CHART::ACCT
				END IF
				GOTO ReEnter
			END IF

		CASE 11%
	!++
	! Abstract:FLD011
	!	^*(11) Equipment Labor Efficiency Variance Account\*
	!	.b
	!	.lm +5
	!	The ^*Equipment Labor Efficiency Variance Account\* field
	!	enters the General Ledger Account number for the
	!	Equipment Labor Efficiency Variance Account.
	!	.b
	!	Valid Account Numbers may be viewed by pressing ^*List Choices\*.
	!	.b
	!	This field will accept up to eighteen (18) alphanumeric
	!	characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			WP_CONTROL::EQLABEVAR = ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"13;30", TEMP$, WP_CONTROL::EQLABEVAR, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%
				THEN
					WP_CONTROL::EQLABEVAR = &
						GL_CHART::ACCT
				END IF
				GOTO ReEnter
			END IF

		CASE 12%
	!++
	! Abstract:FLD012
	!	^*(12) Equipment Burden Variance Account\*
	!	.b
	!	.lm +5
	!	The ^*Equipment Burden Variance Account\* field
	!	enters the General Ledger Account number for the
	!	Equipment Burden Variance Account.
	!	.b
	!	Valid Account Numbers may be viewed by pressing ^*List Choices\*.
	!	.b
	!	This field will accept up to eighteen (18) alphanumeric
	!	characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			WP_CONTROL::EQBURVAR = ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"14;30", TEMP$, WP_CONTROL::EQBURVAR, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%
				THEN
					WP_CONTROL::EQBURVAR = &
						GL_CHART::ACCT
				END IF
				GOTO ReEnter
			END IF

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		WP_MAIN_CONTROL = 0%

		SELECT MLOOP

		CASE 5%
			IF WP_CONTROL::INVMATUVAR = ""
			THEN
				GL_CHART::DESCR = SPACE$(LEN(GL_CHART::DESCR))
			ELSE
				WP_MAIN_CONTROL = FUNC_TESTENTRY(SMG_WINDOW, &
					WP_CONTROL::INVMATUVAR, &
					GL_CHART::ACCT, &
					"GL", MLOOP, "PROG", &
					"Material Usage Acct", &
					GL_MAIN_CHART.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				GL_CHART::DESCR, 5%, 50%,, SMG$M_BOLD)

		CASE 6%
			IF WP_CONTROL::INVLABRVAR = ""
			THEN
				GL_CHART::DESCR = SPACE$(LEN(GL_CHART::DESCR))
			ELSE
				WP_MAIN_CONTROL = FUNC_TESTENTRY(SMG_WINDOW, &
					WP_CONTROL::INVLABRVAR, &
					GL_CHART::ACCT, &
					"GL", MLOOP, "PROG", &
					"Labor Rate Acct", &
					GL_MAIN_CHART.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				GL_CHART::DESCR, 6%, 50%,, SMG$M_BOLD)

		CASE 7%
			IF WP_CONTROL::INVLABEVAR = ""
			THEN
				GL_CHART::DESCR = SPACE$(LEN(GL_CHART::DESCR))
			ELSE
				WP_MAIN_CONTROL = FUNC_TESTENTRY(SMG_WINDOW, &
					WP_CONTROL::INVLABEVAR, &
					GL_CHART::ACCT, &
					"GL", MLOOP, "PROG", &
					"Labor Efficiency Acct", &
					GL_MAIN_CHART.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				GL_CHART::DESCR, 7%, 50%,, SMG$M_BOLD)

		CASE 8%
			IF WP_CONTROL::INVBURVAR = ""
			THEN
				GL_CHART::DESCR = SPACE$(LEN(GL_CHART::DESCR))
			ELSE
				WP_MAIN_CONTROL = FUNC_TESTENTRY(SMG_WINDOW, &
					WP_CONTROL::INVBURVAR, &
					GL_CHART::ACCT, &
					"GL", MLOOP, "PROG", &
					"Burden Acct", &
					GL_MAIN_CHART.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				GL_CHART::DESCR, 8%, 50%,, SMG$M_BOLD)

		CASE 9%
			IF WP_CONTROL::EQMATUVAR = ""
			THEN
				GL_CHART::DESCR = SPACE$(LEN(GL_CHART::DESCR))
			ELSE
				WP_MAIN_CONTROL = FUNC_TESTENTRY(SMG_WINDOW, &
					WP_CONTROL::EQMATUVAR, &
					GL_CHART::ACCT, &
					"GL", MLOOP, "PROG", &
					"Eq Mat Usage Var Acct", &
					GL_MAIN_CHART.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				GL_CHART::DESCR, 11%, 50%,, SMG$M_BOLD)

		CASE 10%
			IF WP_CONTROL::EQLABRVAR = ""
			THEN
				GL_CHART::DESCR = SPACE$(LEN(GL_CHART::DESCR))
			ELSE
				WP_MAIN_CONTROL = FUNC_TESTENTRY(SMG_WINDOW, &
					WP_CONTROL::EQLABRVAR, &
					GL_CHART::ACCT, &
					"GL", MLOOP, "PROG", &
					"Labor Rate Acct", &
					GL_MAIN_CHART.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				GL_CHART::DESCR, 12%, 50%,, SMG$M_BOLD)

		CASE 11%
			IF WP_CONTROL::EQLABEVAR = ""
			THEN
				GL_CHART::DESCR = SPACE$(LEN(GL_CHART::DESCR))
			ELSE
				WP_MAIN_CONTROL = FUNC_TESTENTRY(SMG_WINDOW, &
					WP_CONTROL::EQLABEVAR, &
					GL_CHART::ACCT, &
					"GL", MLOOP, "PROG", &
					"Lab Efficiency Acct", &
					GL_MAIN_CHART.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				GL_CHART::DESCR, 13%, 50%,, SMG$M_BOLD)

		CASE 12%
			IF WP_CONTROL::EQBURVAR = ""
			THEN
				GL_CHART::DESCR = SPACE$(LEN(GL_CHART::DESCR))
			ELSE
				WP_MAIN_CONTROL = FUNC_TESTENTRY(SMG_WINDOW, &
					WP_CONTROL::EQBURVAR, &
					GL_CHART::ACCT, &
					"GL", MLOOP, "PROG", &
					"Burden Acct", &
					GL_MAIN_CHART.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				GL_CHART::DESCR, 14%, 50%,, SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(5%) AND 2%) = 0%
		THEN
			IF WP_CONTROL::INVMATUVAR = ""
			THEN
				GL_CHART::DESCR = ""
			ELSE
				GL_CHART::DESCR = &
					STRING$(LEN(GL_CHART::DESCR), A"?"B) &
					IF  MAIN_WINDOW(GL_MAIN_CHART.ID, &
						"Q0" + WP_CONTROL::INVMATUVAR) <> 1%
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				GL_CHART::DESCR, 5%, 50%,, SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(6%) AND 2%) = 0%
		THEN
			IF WP_CONTROL::INVLABRVAR = ""
			THEN
				GL_CHART::DESCR = ""
			ELSE
				GL_CHART::DESCR = &
					STRING$(LEN(GL_CHART::DESCR), A"?"B) &
					IF  MAIN_WINDOW(GL_MAIN_CHART.ID, &
						"Q0" + WP_CONTROL::INVLABRVAR) <> 1%
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				GL_CHART::DESCR, 6%, 50%,, SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(7%) AND 2%) = 0%
		THEN
			IF WP_CONTROL::INVLABEVAR = ""
			THEN
				GL_CHART::DESCR = ""
			ELSE
				GL_CHART::DESCR = &
					STRING$(LEN(GL_CHART::DESCR), A"?"B) &
					IF  MAIN_WINDOW(GL_MAIN_CHART.ID, &
						"Q0" + WP_CONTROL::INVLABEVAR) <> 1%
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				GL_CHART::DESCR, 7%, 50%,, SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(8%) AND 2%) = 0%
		THEN
			IF WP_CONTROL::INVBURVAR = ""
			THEN
				GL_CHART::DESCR = ""
			ELSE
				GL_CHART::DESCR = &
					STRING$(LEN(GL_CHART::DESCR), A"?"B) &
					IF  MAIN_WINDOW(GL_MAIN_CHART.ID, &
						"Q0" + WP_CONTROL::INVBURVAR) <> 1%
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				GL_CHART::DESCR, 8%, 50%,, SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(9%) AND 2%) = 0%
		THEN
			IF WP_CONTROL::EQMATUVAR = ""
			THEN
				GL_CHART::DESCR = ""
			ELSE
				GL_CHART::DESCR = &
					STRING$(LEN(GL_CHART::DESCR), A"?"B) &
					IF  MAIN_WINDOW(GL_MAIN_CHART.ID, &
						"Q0" + WP_CONTROL::EQMATUVAR) <> 1%
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				GL_CHART::DESCR, 11%, 50%,, SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(10%) AND 2%) = 0%
		THEN
			IF WP_CONTROL::EQLABRVAR = ""
			THEN
				GL_CHART::DESCR = ""
			ELSE
				GL_CHART::DESCR = &
					STRING$(LEN(GL_CHART::DESCR), A"?"B) &
					IF  MAIN_WINDOW(GL_MAIN_CHART.ID, &
						"Q0" + WP_CONTROL::EQLABRVAR) <> 1%
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				GL_CHART::DESCR, 12%, 50%,, SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(11%) AND 2%) = 0%
		THEN
			IF WP_CONTROL::EQLABEVAR = ""
			THEN
				GL_CHART::DESCR = ""
			ELSE
				GL_CHART::DESCR = &
					STRING$(LEN(GL_CHART::DESCR), A"?"B) &
					IF  MAIN_WINDOW(GL_MAIN_CHART.ID, &
						"Q0" + WP_CONTROL::EQLABEVAR) <> 1%
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				GL_CHART::DESCR, 13%, 50%,, SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(12%) AND 2%) = 0%
		THEN
			IF WP_CONTROL::EQBURVAR = ""
			THEN
				GL_CHART::DESCR = ""
			ELSE
				GL_CHART::DESCR = &
					STRING$(LEN(GL_CHART::DESCR), A"?"B) &
					IF  MAIN_WINDOW(GL_MAIN_CHART.ID, &
						"Q0" + WP_CONTROL::EQBURVAR) <> 1%
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				GL_CHART::DESCR, 14%, 50%,, SMG$M_BOLD)
		END IF

	!
	! Set WP_CONTROL_OLD value
	!
20500	CASE OPT_SETOLD
		WP_CONTROL_OLD = WP_CONTROL

	!
	! Restore WP_CONTROL_OLD value
	!
	CASE OPT_RESETOLD
		WP_CONTROL = WP_CONTROL_OLD
		WP_CONTROL::INVMATPVAR = ""
		WP_CONTROL::EQMATPVAR  = ""

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		WP_CONTROL2 = WP_CONTROL

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		WP_CONTROL = WP_CONTROL2

	END SELECT

	!
	! Exit the Function
	!
 ExitFunction:
	EXIT FUNCTION

28000	!
	! Get control record
	!
	WHEN ERROR IN
		GET #WP_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		CONTINUE 28030
	END WHEN

	GOTO 28040

28030	!
	! Load in defaults for control file
	!
	WP_CONTROL::ORDNUM	= STRING$(LEN(WP_CONTROL::ORDNUM), A"0"B)
	WP_CONTROL::REQNUM	= STRING$(LEN(WP_CONTROL::REQNUM), A"0"B)
	WP_CONTROL::PURGDATE	= SPACE$(LEN(WP_CONTROL::PURGDATE))
	WP_CONTROL::STATUS_FLAG = "0"

	WHEN ERROR IN
		PUT #WP_CONTROL.CH%
	USE
		CALL ENTR_3MESSAGE(SCOPE, "Unable to add a record", 0%)
		CONTINUE 32767
	END WHEN

28040	RETURN

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
