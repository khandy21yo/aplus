1	%TITLE "Cash In and Out of the Cash Register"
	%SBTTL "PS_MAIN_CASHINOUT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PS_MAIN_CASHINOUT(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	The ^*Cash In and Out Maintenance\*
	!	enters a cash transaction, in or out of the
	!	cash register, which is not related to the sale.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PS_SOURCE:PS_MAIN_CASHINOUT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PS_MAIN_CASHINOUT
	!	$ DELETE PS_MAIN_CASHINOUT.OBJ;*
	!
	! Author:
	!
	!	11/02/91 - Frank F. Starman
	!
	! Modification history:
	!
	!	04/15/92 - Frank F. Starman
	!		Added the OPERATOR field.
	!
	!	04/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	11/14/95 - Kevin Handy
	!		Format source closer to 80 columns.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	01/08/98 - Kevin Handy
	!		Added deposit number field
	!
	!	01/11/98 - Kevin Handy
	!		Added account number field
	!
	!	11/20/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include scope.com
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "FUNC_INCLUDE:PS_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PS.OPEN]PS_CASHINOUT.HB"
	MAP	(PS_CASHINOUT)	PS_CASHINOUT_CDD	PS_CASHINOUT
	MAP	(PS_CASHINOUT_OLD) PS_CASHINOUT_CDD	PS_CASHINOUT_OLD, &
		PS_CASHINOUT2

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (BATCH_NO) &
		BATCH_NO$ = 7%

	COM (CH_PS_CASHINOUT) &
		PS_CASHINOUT.CH%, &
		PS_CASHINOUT.READONLY%

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

		!**************************************************************
		! Set up information
		!**************************************************************
		!
		! Define SMG_WINDOW
		!
		SMG_WINDOW::DESCR	= "Cash In and Out"
		SMG_WINDOW::NHELP	= "PS_MAIN_CASHINOUT"
		SMG_WINDOW::HSIZE	= 58%
		SMG_WINDOW::VSIZE	= 9%
		SMG_WINDOW::HPOS	= 10%
		SMG_WINDOW::VPOS	= 10%
		SMG_WINDOW::FLAGS	= 0%
		SMG_WINDOW::NITEMS	= 7%
		SMG_WINDOW::HVIEW	= 58%
		SMG_WINDOW::VVIEW	= 7%
		SMG_WINDOW::VHPOS	= 10%
		SMG_WINDOW::VVPOS	= 10%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Date"
			SMG_WINDOW::KFIELD(0%, 0%)	= 2%
			SMG_WINDOW::KFIELD(0%, 1%)	= 1%
			SMG_WINDOW::KFIELD(0%, 2%)	= 2%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, "QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PS_CASHINOUT.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PS_CASHINOUT.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PS.OPEN]PS_CASHINOUT.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			EXIT HANDLER
		END WHEN

		PS_CASHINOUT.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PS.OPEN]PS_CASHINOUT.OPN"
		USE
			CONTINUE 770 IF ERR = 10%
			EXIT HANDLER
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PS_CASHINOUT.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PS_CASHINOUT.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PS_CASHINOUT.CH%
		WHEN ERROR IN
			RESET #PS_CASHINOUT.CH%
			GET #PS_CASHINOUT.CH%, REGARDLESS
		USE
			CONTINUE 32767 IF ERR = 11%
			EXIT HANDLER
		END WHEN

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

		DATA	02,03, "(01) Trans Date", &
			03,03, "(02) Trans Time", &
			04,03, "(03) Cash Amount", &
			05,03, "(04) Written By", &
			06,03, "(05) Notes", &
			07,03, "(06) Deposit", &
			08,03, "(07) Account", &
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

	!*********************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
	!*********************************************************************
20200	CASE OPT_ENTRY

		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")
 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	.x Transaction Date
	!	^*(01) Transaction Date\*
	!	.b
	!	.lm +5
	!	The ^*Transaction Date\* field enters the date
	!	of the cash transaction.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!
	!--
			PS_CASHINOUT::CASHDATE = &
				ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;25",TEMP$, PS_CASHINOUT::CASHDATE, MFLAG, &
				"'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	.x Transaction Time
	!	^*(02) Transaction Time\*
	!	.b
	!	.lm +5
	!	The ^*Transaction Time\* field enters the time
	!	of the cash transaction.
	!	.b
	!	The format for entry is HHMMSS.
	!	.lm -5
	!
	! Index:
	!
	!--
			PS_CASHINOUT::CASHTIME = &
				ENTR_3TIME(SCOPE, SMG_WINDOW::WNUMBER, &
				"3;25",TEMP$, PS_CASHINOUT::CASHTIME, MFLAG, &
				"'E", MVALUE)


		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Cash Amount\*
	!	.b
	!	.lm +5
	!	The ^*Cash Amount\* field enters the amount of
	!	the cash transaction.
	!	.lm -5
	!
	! Index:
	!	.x Cash Amount
	!
	!--
			PS_CASHINOUT::AMOUNT = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"04;25",TEMP$, PS_CASHINOUT::AMOUNT, MFLAG, &
				"#,###,###.##", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Written by\*
	!	.b
	!	.lm +5
	!	The ^*Written by\* field enters the identification of
	!	the person responsible for making the cash transaction.
	!	.b
	!	The field will accept ten (10) characters.
	!	.lm -5
	!
	! Index:
	!	.x Written by>Cash In and Out
	!
	!--
			PS_CASHINOUT::OPERATOR = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;25", TEMP$, PS_CASHINOUT::OPERATOR, MFLAG, &
				"'E", MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Notes\*
	!	.B
	!	.LM +5
	!	The ^*Notes\* field enters free formatted notes
	!	relative to the cash transaction.
	!	.b
	!	The field will accept up to forty (40) characters.
	!	.LM -5
	!
	! Index:
	!	.x Notes>Cash In and Out
	!
	!--
			PS_CASHINOUT::NOTES = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;25", TEMP$, PS_CASHINOUT::NOTES, MFLAG, &
				"'E", MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Deposit\*
	!	.B
	!	.LM +5
	!	The ^*Deposit\* field specifies the bank deposit number
	!	that will be used to post the cash transactions.
	!	.LM -5
	!
	! Index:
	!	.x Deposit>Cash In and Out
	!
	!--
			PS_CASHINOUT::DEPOSIT = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"7;25", TEMP$, PS_CASHINOUT::DEPOSIT, MFLAG, &
				"'E", MVALUE)

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*(07) Account\*
	!	.B
	!	.LM +5
	!	.LM -5
	!
	! Index:
	!	.x Account>Cash In and Out
	!
	!--
			PS_CASHINOUT::ACCOUNT = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"8;25", TEMP$, PS_CASHINOUT::ACCOUNT, MFLAG, &
				"'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test the data
	!
	CASE OPT_TESTENTRY
		PS_MAIN_CASHINOUT = 0%

	!
	! Set PS_CASHINOUT_OLD value
	!
	CASE OPT_SETOLD
		PS_CASHINOUT_OLD = PS_CASHINOUT

	!
	! Restore PS_CASHINOUT_OLD value
	!
	CASE OPT_RESETOLD
		PS_CASHINOUT = PS_CASHINOUT_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PS_CASHINOUT2 = PS_CASHINOUT

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PS_CASHINOUT = PS_CASHINOUT2
		PS_CASHINOUT::CASHDATE = DATE_TODAY
		PS_CASHINOUT::CASHTIME = TIME_NOW

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Date       Time             Amount Notes"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "013,024,037"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = PRNT_DATE(PS_CASHINOUT::CASHDATE, 8%) + " " + &
				PRNT_TIME(PS_CASHINOUT::CASHTIME, 6%) + " " + &
				FORMAT$(PS_CASHINOUT::AMOUNT, &
				"#,###,###.##") + " " + &
				PS_CASHINOUT::NOTES


		END SELECT

	!
	! Find the next record
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE PS_CASHINOUT::CASHDATE + &
					PS_CASHINOUT::CASHTIME, &
				REGARDLESS

		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION


29000	!******************************************************************
	! Trap errors
	!******************************************************************

	!
	! Resume to display untrapped error
	!
	ON ERROR GO BACK

32767	END FUNCTION
