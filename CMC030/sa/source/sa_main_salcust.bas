1	%TITLE "Customer Sales Maintenance"
	%SBTTL "SA_MAIN_SALCUST"
	%IDENT "V3.6a Calico"

	FUNCTION LONG SA_MAIN_SALCUST(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1987, 1988 BY
	!
	! Computer Management Center
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be used and
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies therof may not be provided or otherwise made
	! available to any other person.  No title to and ownership of
	! the software is hereby transferred.
	!
	! The information in this software is subject to change without
	! notice and should not be construed as a commitment by
	! Computer Management Center.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	! Abstract:HELP
	!	.p
	!	The ^*Customer Sales Maintenance\* option
	!	accesses the file where customer sales are
	!	stored and either view, modify, or erase the orders and their
	!	descriptons to the users specifications.
	!
	! Index:
	!	.x Sales Entry>Maintenance
	!	.x Maintenance>Sales Entry
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS SA_SOURCE:SA_MAIN_SALCUST/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP SA_MAIN_SALCUST
	!	$ DELETE SA_MAIN_SALCUST.OBJ;*
	!
	!
	! Author:
	!
	!	06/14/90 - Lance Williams
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/29/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/05/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include scope.com
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:SA_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	!
	! Include Main Window
	!
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[SA.OPEN]SA_SALCUST.HB"
	MAP (SA_SALCUST)	SA_SALCUST_CDD		SA_SALCUST
	MAP (SA_SALCUST_OLD)	SA_SALCUST_CDD		SA_SALCUST_OLD, SA_SALCUST2

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_SA_SALCUST) &
		SA_SALCUST.CH%, &
		SA_SALCUST.READONLY%

	COM (SA_SALCUST_COM) YYYY$ = 4%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG   FUNCTION MAIN_WINDOW

	%PAGE

	ON ERROR GOTO 29000

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
		SMG_WINDOW::DESCR = "Customer Sales Balance in " + &
			YYYY$
		SMG_WINDOW::NHELP = "SA_MAIN_SALCUST"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 4%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Customer Number"
			SMG_WINDOW::KFIELD(0%, 0%) = 3%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
			SMG_WINDOW::KFIELD(0%, 3%) = 3%

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare order type channels
		!
		IF SA_SALCUST.CH% > 0%
		THEN
			!
			! If SA_SALCUST is already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF SA_SALCUST.READONLY%
			GOTO 790
		END IF

		!
		! Open SA_SALCUST (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[SA.OPEN]SA_SALCUST.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			SA_MAIN_SALCUST  = ERR
			CONTINUE 770
		END WHEN

		SA_SALCUST.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open SA_SALCUST for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[SA.OPEN]SA_SALCUST.OPN"
		USE
			SA_MAIN_SALCUST = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		SA_SALCUST.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open SA_SALCUST, so reset channel
		!
		CALL ASSG_FREECHANNEL(SA_SALCUST.CH%)

		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = SA_SALCUST.CH%
		WHEN ERROR IN
			RESET #SA_SALCUST.CH%
			GET #SA_SALCUST.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	%PAGE

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

		DATA	2,  3, "(01) Customer #", &
			3,  3, "(02) Period ", &
			4,  3, "(03) GL Account ", &
			5,  3, "(04) Beg Amount ", &
			7,  3, "     Post Date", &
			8,  3, "     Post Time", &
			9,  3, "     Batch Number", &
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

20200	!******************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display
	! data, set defaults, and return the data back according to
	! MFLAG.
	!******************************************************************
	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 Reenter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Customer Number\*
	!	.b
	!	.lm +5
	!	The ^*Customer Number\* field enters a user defined
	!	string which will identify a customer number.
	!	.b
	!	The field provides ten (10) spaces for entry.
	!	.lm -5
	!
	! Index:
	!
	!--

			SA_SALCUST::CUSNUM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;22", TEMP$, &
				SA_SALCUST::CUSNUM, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, "V0") = 1%
				THEN
					SA_SALCUST::CUSNUM = &
						AR_35CUSTOM::CUSNUM
				END IF
				GOTO Reenter
			END IF


		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Period\*
	!	.p
	!	The ^*Period\* field enters the period
	!	for this particular invoice.
	!	.p
	!	This field will accommodate six (6) alphanumeric characters.
	!
	! Index:
	!
	!--

			SA_SALCUST::PERIOD = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"3;22", TEMP$, &
				SA_SALCUST::PERIOD, MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) General Ledger Account\*
	!	.p
	!	.p
	!	The ^*General Ledger Account\* field enters the General
	!	Ledger account number.
	!	.p
	!	The field will accommodate eighteen (18) alphanumeric characters.
	!
	! Index:
	!
	!--

			SA_SALCUST::ACCOUNT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;22", TEMP$, &
				SA_SALCUST::ACCOUNT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX0") = 1%
				THEN
					SA_SALCUST::ACCOUNT = &
						GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Beginning Amount\*
	!	.p
	!	The ^*Beginning Amount\* field enters
	!	the beginning balance sale amount for the customer.
	!
	! Index:
	!	.x Beginning Amount
	!
	!--

			SA_SALCUST::AMOUNT = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;22", TEMP$, &
				SA_SALCUST::AMOUNT, MFLAG, "#,###,###.##", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

20300	CASE OPT_TESTENTRY
		SA_MAIN_SALCUST = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Display the descriptions for customer name
			!
			SA_MAIN_SALCUST = FUNC_TESTENTRY(SMG_WINDOW, &
				SA_SALCUST::CUSNUM, &
				AR_35CUSTOM::CUSNUM, &
				"SA", MLOOP, "PROG", &
				"Customer", AR_MAIN_35CUSTOM.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AR_35CUSTOM::CUSNAM, &
				2%, 35%, , SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			AR_35CUSTOM::CUSNAM = &
				STRING$(LEN(AR_35CUSTOM::CUSNAM), A"?"B) &
				IF MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, &
				"Q0" + SA_SALCUST::CUSNUM) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AR_35CUSTOM::CUSNAM, &
				2%, 35%, , SMG$M_BOLD)
		END IF

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			PRNT_DATE(SA_SALCUST::POSTDATE, 8%), &
			07%, 22%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			PRNT_TIME(SA_SALCUST::POSTTIME, 0%), &
			08%, 22%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			SA_SALCUST::BATCH, &
			09%, 22%, , SMG$M_BOLD)

	!
	! Set SA_SALCUST_OLD value
	!
20500	CASE OPT_SETOLD
		SA_SALCUST_OLD = SA_SALCUST

	!
	! Restore SA_SALCUST_OLD value
	!
	CASE OPT_RESETOLD
		SA_SALCUST = SA_SALCUST_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		SA_SALCUST2 = SA_SALCUST

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		SA_SALCUST = SA_SALCUST2
		SA_SALCUST::POSTDATE = DATE_TODAY
		SA_SALCUST::POSTTIME = TIME_NOW
		SA_SALCUST::BATCH = "000000"

	!
	! View the SA_SALCUST Record.
	!
	CASE OPT_VIEW
		SELECT MLOOP

		CASE 1%
			MVALUE = "Customer#    Period" + &
				"              GLAccount" + &
				"                              Amount"

		CASE 2%
			MVALUE = "013,033,049"

		CASE 3%
			MVALUE = &
				SA_SALCUST::CUSNUM + " " + &
				SA_SALCUST::PERIOD + "             " + &
				SA_SALCUST::ACCOUNT + "                " + &
				FORMAT$(SA_SALCUST::AMOUNT, "#,###,###.##")

		END SELECT

	!
	! Find the SA_SALCUST Record.
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #SA_SALCUST.CH%, KEY #0% GE SA_SALCUST::CUSNUM + &
				SA_SALCUST::PERIOD + SA_SALCUST::ACCOUNT, &
				REGARDLESS

		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

29000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Resume to display untrapped error
	!
	ON ERROR GO BACK

	!
	! End of Function
	!
32767	END FUNCTION
