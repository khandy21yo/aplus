1	%TITLE "Pacific Pride Control File"
	%SBTTL "PP_MAIN_CONTROL"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PP_MAIN_CONTROL(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1992 BY
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
	!	The ^*Pacific Pride Control File\* maintains
	!	the System Customer Number, the AR General
	!	Ledger Account Number, the Host Number and the Discount Days.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PP_SOURCE:PP_MAIN_CONTROL/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PP_MAIN_CONTROL
	!	$ DELETE PP_MAIN_CONTROL.OBJ;*
	!
	! Author:
	!
	!	12/18/92 - Dan Perkins
	!
	! Modification history:
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/05/99 - Kevin Handy
	!		Fix error trapping for 750 to go to 760
	!		instead of 790.
	!
	!	05/29/2000 - Kevin Handy
	!		Lose unreachable line 770
	!
	!	11/02/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
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

	%INCLUDE "SOURCE:[PP.OPEN]PP_CONTROL.HB"
	MAP (PP_CONTROL)	PP_CONTROL_CDD		PP_CONTROL
	MAP (PP_CONTROL_OLD)	PP_CONTROL_CDD		PP_CONTROL_OLD, &
							PP_CONTROL2

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	!
	! Common Statements
	!
	COM (CH_PP_CONTROL) &
		PP_CONTROL.CH%, &
		PP_CONTROL.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_WINDOW
	EXTERNAL LONG    FUNCTION GL_MAIN_CHART
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
		SMG_WINDOW::DESCR  = "Pacific Pride Control File"
		SMG_WINDOW::NHELP  = "PP_MAIN_CONTROL"
		SMG_WINDOW::HSIZE  = 78%
		SMG_WINDOW::VSIZE  = 18%
		SMG_WINDOW::HPOS   = 2%
		SMG_WINDOW::VPOS   = 2%
		SMG_WINDOW::FLAGS  = 128% !Relative file
		SMG_WINDOW::NITEMS = 6%

		SMG_WINDOW::NKEYS = 0%

		CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Declare channels
		!
		IF PP_CONTROL.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PP_CONTROL.READONLY%

			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PP.OPEN]PP_CONTROL.CRE"
		USE
			CONTINUE 760 IF ERR = 7% OR ERR = 16%
			EXIT HANDLER
		END WHEN

		PP_CONTROL.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PP.OPEN]PP_CONTROL.OPN"
		USE
			CONTINUE 790 IF ERR = 7%
			EXIT HANDLER
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PP_CONTROL.READONLY% = -1%
		GOTO 790

790		SMG_WINDOW::CHAN = PP_CONTROL.CH%
		GOSUB 28000

	!
	! Select function
	!
	CASE OPT_OPTLIST
		MVALUE = "Change Blank Help eXit "

	!
	! Display window background
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	03,  02, "(01) System Customer #", &
			04,  02, "(02) AR Account #", &
			05,  02, "(03) Host #", &
			06,  02, "(04) Discount Days", &
			07,  02, "(05) Last Invoice Date", &
			08,  02, "(06) Last Invoice #", &
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
	!	.x System Customer Number
	!	^*(01) System Customer Number\*
	!	.b
	!	.lm +5
	!	The ^*System Customer Number\* field contains the System
	!	Customer Number which will be unique to the system.
	!	.b
	!	The field will accept 10 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!	.x System Customer Number
	!
	!--
			PP_CONTROL::CUSNUM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"03;26", TEMP$, PP_CONTROL::CUSNUM, &
				MFLAG, "'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	.x AR Account Number
	!	^*(02) AR Account Number\*
	!	.b
	!	.lm +5
	!	The ^*AR Account Number\* field contains the General Ledger
	!	account number to which transactions from the Pacific Pride
	!	system will be assigned.  The account must first have been defined in the
	!	general ledger chart of accounts in order to be a valid account.
	!	.b
	!	Valid account numbers may be viewed by pressing ^*List Choices, F14\*.
	!	.b
	!	The field will accept 18 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!	.x AR Account Number
	!
	!--
			PP_CONTROL::AR_ACCOUNT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"04;26", TEMP$, PP_CONTROL::AR_ACCOUNT, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%
				THEN
					PP_CONTROL::AR_ACCOUNT = GL_CHART::ACCT
				END IF
				GOTO ReEnter
			END IF

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Host Number\*
	!	.b
	!	.lm +5
	!	The ^*Host Number\* field contains the specific number
	!	assigned to the given host in the Pacific Pride System.
	!	.b
	!	The field will accept 3 alpha-numeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Host Number
	!
	!--
			PP_CONTROL::HOST_NUM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"05;26", TEMP$, PP_CONTROL::HOST_NUM, &
				MFLAG, "'E", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	.x Discount Days
	!	^*(04) Discount\*
	!	.b
	!	.lm +5
	!	The ^*Discount Days\* contains the number of days
	!	for which a discount may be taken.
	!	.b
	!	The field will accept 2 characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_CONTROL::DIS_DAYS = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"06;26", TEMP$, PP_CONTROL::DIS_DAYS, &
				MFLAG, "'E", MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	.x Last Invoice Date
	!	^*(04) Last Invoice Date\*
	!	.b
	!	.lm +5
	!	The ^*Last Invoice Date\* contains the date the last
	!	invoices were printed.
	!	.b
	!	The field will accept 8 characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_CONTROL::INVDATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"7;26", TEMP$, PP_CONTROL::INVDATE, &
				MFLAG, "'E", MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	.x Last Invoice Number
	!	^*(04) Last Invoice Number\*
	!	.b
	!	.lm +5
	!	The ^*Last Invoice Number\* contains the last invoice number
	!	that was printed on an invoice.
	!	.b
	!	The field will accept 8 characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			PP_CONTROL::LAST_INV = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"08;26", TEMP$, PP_CONTROL::LAST_INV, &
				MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
	CASE OPT_TESTENTRY

		OE_MAIN_CONTROL = 0%

		SELECT MLOOP

		CASE 2%
			!
			! Test AR Account Number
			!
			PP_MAIN_CONTROL = FUNC_TESTENTRY(SMG_WINDOW, &
				PP_CONTROL::AR_ACCOUNT, &
				GL_CHART::DESCR, &
				"PP", MLOOP, "PROG", &
				"AR Account", GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 4%, 46%, , SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY

		!
		! Display the descriptions
		!
		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			GL_CHART::DESCR = &
				STRING$(LEN(GL_CHART::DESCR), A"?"B) &
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "Q0" + &
				PP_CONTROL::AR_ACCOUNT) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 4%, 46%, , SMG$M_BOLD)

		END IF

	!
	! Set PP_CONTROL_OLD value
	!
	CASE OPT_SETOLD
		PP_CONTROL_OLD = PP_CONTROL

	!
	! Restore PP_CONTROL_OLD value
	!
	CASE OPT_RESETOLD
		PP_CONTROL = PP_CONTROL_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PP_CONTROL2 = PP_CONTROL

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PP_CONTROL = PP_CONTROL2

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
		GET #PP_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		CONTINUE 28030 IF ERR = 155%
		EXIT HANDLER
	END WHEN

	GOTO 28040

28030	!
	! Load in defaults for control file
	!
	PP_CONTROL::CUSNUM     = SPACE$(LEN(PP_CONTROL::CUSNUM))
	PP_CONTROL::AR_ACCOUNT = SPACE$(LEN(PP_CONTROL::AR_ACCOUNT))
	PP_CONTROL::HOST_NUM   = SPACE$(LEN(PP_CONTROL::HOST_NUM))
	PP_CONTROL::DIS_DAYS   = SPACE$(LEN(PP_CONTROL::DIS_DAYS))
	PP_CONTROL::INVDATE    = SPACE$(LEN(PP_CONTROL::INVDATE))
	PP_CONTROL::LAST_INV   = SPACE$(LEN(PP_CONTROL::LAST_INV))

	WHEN ERROR IN
		PUT #PP_CONTROL.CH%
	USE
		CONTINUE 32767 IF ERR = 5%
		EXIT HANDLER
	END WHEN

28040	RETURN

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
