1	%TITLE "Credit Codes"
	%SBTTL "OE_MAIN_CREASON"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OE_MAIN_CREASON(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	The ^*Credit Reason\* table defines the reasons for the
	!	issuing of Credit Memos and the General Ledger Account
	!	to be credited.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_MAIN_CREASON/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP OE_MAIN_CREASON
	!	$ DELETE OE_MAIN_CREASON.OBJ;*
	!
	! Author:
	!
	!	08/29/91 - JEFF BEARD
	!
	! Modification history:
	!
	!	09/16/91 - Dan Perkins
	!		Modified error trapping
	!
	!
	!	10/24/91 - Kevin Handy
	!		Modified to add the "COM (CH_OE_CREASON)" statement
	!		so that it doesn't re-open the file every call.
	!
	!	04/10/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/15/92 - Frank F. Starman
	!		Remove account number.
	!
	!	06/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/14/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Add 0% as last param to call to ENTR_3MESSAGE.
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
	!	11/30/2000 - Kevin Handy
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
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[OE.OPEN]OE_CREASON.HB"
	MAP (OE_CREASON)	OE_CREASON_CDD		OE_CREASON
	MAP (OE_CREASON_OLD)	OE_CREASON_CDD		OE_CREASON_OLD, &
							OE_CREASON2

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_OE_CREASON) &
		OE_CREASON.CH%, &
		OE_CREASON.READONLY%

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necesUTLry that have not already been opened.
	!
	CASE OPT_INIT

		!******************************************************************
		! Set up information
		!******************************************************************
		!
		! Define SMG_WINDOW
		!
		SMG_WINDOW::DESCR	= "Reason Codes Description"
		SMG_WINDOW::NHELP	= "OE_MAIN_CREASON"
		SMG_WINDOW::HSIZE	= 78%
		SMG_WINDOW::VSIZE	= 18%
		SMG_WINDOW::HPOS	= 2%
		SMG_WINDOW::VPOS	= 2%
		SMG_WINDOW::FLAGS	= 0%
		SMG_WINDOW::NITEMS	= 3%
		SMG_WINDOW::HVIEW	= 78%
		SMG_WINDOW::VVIEW	= 18%
		SMG_WINDOW::VHPOS	= 2%
		SMG_WINDOW::VVPOS	= 2%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Reason"
			SMG_WINDOW::KFIELD(0%, 0%)	= 1%
			SMG_WINDOW::KFIELD(0%, 1%)	= 1%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, "QV", MVALUE) <= 1%


700		!
		! Declare order type channels
		!
		IF OE_CREASON.CH% > 0%
		THEN
			!
			! If OE_CREASON is already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF OE_CREASON.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_CREASON.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			EXIT HANDLER
		END WHEN

		OE_CREASON.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_CREASON.OPN"
		USE
			CONTINUE 770 IF ERR = 5%
			EXIT HANDLER
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		OE_CREASON.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(OE_CREASON.CH%)
		GOTO 32767

790		SMG_WINDOW::CHAN  = OE_CREASON.CH%
		WHEN ERROR IN
			RESET #OE_CREASON.CH%
			GET #OE_CREASON.CH%, REGARDLESS
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

		DATA	07,03, "(01) Reason Code", &
			09,03, "(02) Description", &
			11,03, "(03) Taxable", &
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
	!	^*(01) Credit Reason\*
	!	.b
	!	.lm +5
	!	The ^*Credit Reason\* field enters a selected
	!	code which will identify the reason for the credit.
	!	.b
	!	Example:  ^*DM\* - Damaged Merchandise
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_CREASON::CREASON = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "07;23", TEMP$, &
				OE_CREASON::CREASON, MFLAG, "'E", &
				MVALUE)


			CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field enters a description
	!	for the Reason Code entered in field (01).
	!	.b
	!	Forty spaces are available for the description.
	!	.b
	!	Example:  DM - ^*Damaged Merchandise\*
	!	.lm -5
	!
	! Index:
	!
	!--

			OE_CREASON::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "09;23", TEMP$, &
				OE_CREASON::DESCR, MFLAG, "'E", &
				MVALUE)

			CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Taxable\*
	!	.b
	!	.lm +5
	!	Determines if the item is removed before sales tax
	!	is calculated, or not.
	!	.list 0,"*"
	!	.le
	!	*Y Pull out before sales tax is calculated.
	!	.le
	!	*N Don't pull out before sales tax.
	!	.els
	!	.lm -5
	!
	! Index:
	!
	!--

			OE_CREASON::TAXABLE = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, "11;23", TEMP$, &
				OE_CREASON::TAXABLE, MFLAG, "'E", &
				MVALUE)


		END SELECT

		SCOPE::PRG_ITEM = TEMP1$



	!
	! Test the data
	!
20300	CASE OPT_TESTENTRY
		OE_MAIN_CREASON = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Test OE_CREASON
			!
			IF OE_CREASON::CREASON = ""
			THEN
				OE_MAIN_CREASON = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #SMG_WINDOW::CHAN, &
							KEY #0% EQ &
							OE_CREASON::CREASON + "", &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					OE_MAIN_CREASON = 2%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Record Already Exists", 0%)
				END IF
			END IF

		END SELECT

	!
	! Set OE_CREASON_OLD value
	!
20500	CASE OPT_SETOLD
		OE_CREASON_OLD = OE_CREASON

	!
	! Restore OE_CREASON_OLD value
	!
	CASE OPT_RESETOLD
		OE_CREASON = OE_CREASON_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		OE_CREASON2 = OE_CREASON

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		OE_CREASON = OE_CREASON2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = " Reason Description                 " + &
				"             Taxable"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "008,049"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = OE_CREASON::CREASON + "    " + &
				OE_CREASON::DESCR + " " + &
				OE_CREASON::TAXABLE

		END SELECT

	!
	! Find the next record
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE OE_CREASON::CREASON + ""

		END SELECT

	END SELECT

28000	GOTO 32767


29000	!******************************************************************
	! Trap errors
	!******************************************************************
	!
	! Resume to display untrapped error
	!
	ON ERROR GO BACK

32767	END FUNCTION
