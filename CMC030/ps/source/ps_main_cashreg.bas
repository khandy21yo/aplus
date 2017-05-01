1	%TITLE "Cash Register Description Table"
	%SBTTL "PS_MAIN_CASHREG"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PS_MAIN_CASHREG(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	The ^*Cash Register Description Maintenance\*
	!	enters a cash register set up.
	!	.b
	!	.b
	!	.b
	!	.b
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PS_SOURCE:PS_MAIN_CASHREG/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PS_MAIN_CASHREG
	!	$ DELETE PS_MAIN_CASHREG.OBJ;*
	!
	! Author:
	!
	!	11/02/91 - Frank F. Starman
	!
	! Modification history:
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
	!		Added 'Petty Cash' account field.
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	11/03/2000 - Kevin Handy
	!		Use A"x"B
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
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PS.OPEN]PS_CASHREG.HB"
	MAP	(PS_CASHREG)	PS_CASHREG_CDD	PS_CASHREG
	MAP	(PS_CASHREG_OLD) PS_CASHREG_CDD	PS_CASHREG_OLD, PS_CASHREG2

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP	(UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PS_CASHREG) &
		PS_CASHREG.CH%, &
		PS_CASHREG.READONLY%

	!
	! External functions
	!
	EXTERNAL	LONG    FUNCTION MAIN_WINDOW
	EXTERNAL	LONG	FUNCTION FUNC_TESTENTRY

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
		SMG_WINDOW::DESCR	= "Cash Register Description Table"
		SMG_WINDOW::NHELP	= "PS_MAIN_CASHREG"
		SMG_WINDOW::HSIZE	= 78%
		SMG_WINDOW::VSIZE	= 18%
		SMG_WINDOW::HPOS	= 2%
		SMG_WINDOW::VPOS	= 2%
		SMG_WINDOW::FLAGS	= 0%
		SMG_WINDOW::NITEMS	= 6%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "Cash_register"
			SMG_WINDOW::KFIELD(0%, 0%)	= 1%
			SMG_WINDOW::KFIELD(0%, 1%)	= 1%
		SMG_WINDOW::KNAME(1%) = "Location"
			SMG_WINDOW::KFIELD(1%, 0%)	= 2%
			SMG_WINDOW::KFIELD(1%, 1%)	= 3%
			SMG_WINDOW::KFIELD(1%, 2%)	= 1%

		IF MVALUE = "M"
		THEN
			SMG_WINDOW::HSIZE	= 55%
			SMG_WINDOW::VSIZE	= 8%
			SMG_WINDOW::HPOS	= 7%
			SMG_WINDOW::VPOS	= 7%
			SMG_WINDOW::HVIEW	= 55%
			SMG_WINDOW::VVIEW	= 8%
			SMG_WINDOW::VHPOS	= 7%
			SMG_WINDOW::VVPOS	= 7%
		END IF

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, "QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PS_CASHREG.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PS_CASHREG.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PS.OPEN]PS_CASHREG.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			EXIT HANDLER
		END WHEN

		PS_CASHREG.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PS.OPEN]PS_CASHREG.OPN"
		USE
			CONTINUE 770 IF ERR = 10%
			EXIT HANDLER
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PS_CASHREG.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PS_CASHREG.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PS_CASHREG.CH%
	WHEN ERROR IN
			RESET #PS_CASHREG.CH%
			GET #PS_CASHREG.CH%, REGARDLESS
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

		DATA	02,03, "(01) Cash Register#", &
			03,03, "(02) Description", &
			04,03, "(03) Location ", &
			05,03, "(04) Notes", &
			07,03, "(05) Last Inv#", &
			08,03, "(06) Petty Cash Acct", &
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
	!	^*(01) Cash Register _#\*
	!	.b
	!	.lm +5
	!	The ^*Cash Register Number\* field enters the cash register
	!	ID number.
	!	.lm -5
	!
	! Index:
	!	.x Cash Register Number
	!
	!--

			PS_CASHREG::CASHREG= ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "02;25", TEMP$, &
				PS_CASHREG::CASHREG, MFLAG, "'E", MVALUE)


		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field enters a description
	!	for the cash register number which was entered in field (01).
	!	.lm -5
	!
	! Index:
	!	.x Cash Register Description
	!
	!--
			PS_CASHREG::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "03;25", TEMP$, &
				PS_CASHREG::DESCR, MFLAG, "'E", &
				MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Location\*
	!	.b
	!	.lm +5
	!	The ^*Location\* field enters the location
	!	of the cash register identified in field (01).
	!	.lm -5
	!
	! Index:
	!	.x Location
	!
	!--
			PS_CASHREG::LOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "04;25", TEMP$, &
				PS_CASHREG::LOCATION, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "VX") = 1%)
				THEN
					PS_CASHREG::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO Reenter
			END IF

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Notes\*
	!	.b
	!	.lm +5
	!	The ^*Notes\* field enters any free formatted notes
	!	relative to the cash register. The notes entered in this field can be
	!	printed on the invoice form.
	!	.b
	!	Eighty (80) spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Notes>Cash Register
	!
	!--
 FirstNote:
			PS_CASHREG::NOTES(0%) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;25", TEMP$, PS_CASHREG::NOTES(0%), MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO ExitFunction

			END SELECT

			PS_CASHREG::NOTES(1%) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"6;25", TEMP$, PS_CASHREG::NOTES(1%), MFLAG, &
				"'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO FirstNote

			END SELECT

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Last Invoice Number\*
	!	.b
	!	.lm +5
	!	The ^*Last Invoice Number\* field enters the last
	!	invoice number printed for the cash register entered in field (01).
	!	.lm -5
	!
	! Index:
	!	.x Last Invoice Number
	!
	!--
			PS_CASHREG::LAST_INVNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "07;25", TEMP$, &
				PS_CASHREG::LAST_INVNUM, MFLAG, "'E", &
				MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Petty Cash Account\*
	!	.b
	!	.lm +5
	!	The ^*Petty Cash Account\* specifies what GL
	!	account number is used for posting the cash in/out
	!	information.
	!	.lm -5
	!
	! Index:
	!	.x Petty Cash Account
	!
	!--
			PS_CASHREG::PETTYCASH = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "08;25", TEMP$, &
				PS_CASHREG::PETTYCASH, MFLAG, "'E", &
				MVALUE)

	END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test the data
	!
20300	CASE OPT_TESTENTRY
		PS_MAIN_CASHREG = 0%

		SELECT MLOOP

		CASE 3%
			!
			! Test UTL_LOCATION
			!
			PS_MAIN_CASHREG = FUNC_TESTENTRY(SMG_WINDOW, &
				PS_CASHREG::LOCATION, &
				UTL_LOCATION::LOCNAME, &
				"OE", MLOOP, "PROG", &
				"Location", &
				UTL_MAIN_LOCATION.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, 4%, 30%, , SMG$M_BOLD)


		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(3%) AND 2%) = 0%
		THEN
			UTL_LOCATION::LOCNAME = STRING$(LEN( &
				UTL_LOCATION::LOCNAME), A"?"B) &
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "Q0" + &
					PS_CASHREG::LOCATION) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, 4%, 30%, , SMG$M_BOLD)
		END IF


	!
	! Set PS_CASHREG_OLD value
	!
20500	CASE OPT_SETOLD
		PS_CASHREG_OLD = PS_CASHREG

	!
	! Restore PS_CASHREG_OLD value
	!
	CASE OPT_RESETOLD
		PS_CASHREG = PS_CASHREG_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PS_CASHREG2 = PS_CASHREG

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PS_CASHREG = PS_CASHREG2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  CashReg Description          Location  "

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "010,031"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = PS_CASHREG::CASHREG+ "    " + &
				PS_CASHREG::DESCR + " " + &
				PS_CASHREG::LOCATION


		END SELECT

	!
	! Find the next record
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE PS_CASHREG::CASHREG + "", &
				REGARDLESS


		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE PS_CASHREG::LOCATION + &
				PS_CASHREG::CASHREG, &
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
