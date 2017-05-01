1	%TITLE "Closing WIP Journal"
	%SBTTL "WP_MAIN_CLOSEWIP"
	%IDENT "V3.6a Calico"

	FUNCTION LONG WP_MAIN_CLOSEWIP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	!	The ^*Work In Process Buyoff Line Journal\* enters the
	!	number of products completed and cancelled.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_MAIN_CLOSEWIP/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN WP_MAIN_CLOSEWIP
	!	$ DELETE WP_MAIN_CLOSEWIP.OBJ;*
	!
	! Author:
	!
	!	07/27/92 - Frank F.Starman
	!
	! Modification history:
	!
	!	08/31/92 - Kevin Handy
	!		Clean up (check)
	!
	!	09/22/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/10/92 - Dan Perkins
	!		Added CASE 2 to OPT_SUBWIND so that VIEW would
	!		work properly.
	!
	!	11/17/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/31/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/21/98 - Kevin Handy
	!		Increase batch number from 2 to 8 characters
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	06/15/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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
	! Include Statements
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:SB_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:WP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[WP.OPEN]WP_CLOSELINE.HB"
	MAP (WP_CLOSELINE)	WP_CLOSELINE_CDD	WP_CLOSELINE
	MAP (WP_CLOSELINE_OLD)	WP_CLOSELINE_CDD	WP_CLOSELINE_OLD, &
							WP_CLOSELINE_DEF
	COM (WP_CLOSELINE_TOTAL) WP_CLOSELINE_CDD	WP_CLOSELINE_TOTAL

	%INCLUDE "SOURCE:[WP.OPEN]WP_CLOSEJOUR.HB"
	MAP (WP_CLOSEJOUR)	WP_CLOSEJOUR_CDD	WP_CLOSEJOUR

	%INCLUDE "SOURCE:[WP.OPEN]WP_CONTROL.HB"
	MAP (WP_CONTROL)	WP_CONTROL_CDD		WP_CONTROL

	%INCLUDE "SOURCE:[BM.OPEN]BM_CONTROL.HB"
	MAP (BM_CONTROL)	BM_CONTROL_CDD		BM_CONTROL

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	!
	! Common Statements
	!
	COM (CH_WP_CLOSELINE) &
		WP_CLOSELINE.CH%, &
		WP_CLOSELINE.READONLY%, &
		WP_CLOSELINE.TOTALONLY%

	COM (BATCH_WP_CLOSEJOUR) &
		BATCH_NO$ = 8%

	COM (CH_WP_CONTROL) &
		WP_CONTROL.CH%

	COM (CH_BM_CONTROL_READ) BM_CONTROL.CH%

	COM (TT_WP_MAIN_CLOSEWIP) &
		ACCTITLE$ = 20%, &
		ACCTYPE$(7%) = 40%
	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR  = "Closing WIP Journal"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::NHELP  = "WP_MAIN_CLOSEWIP"
		SMG_WINDOW::HSIZE  =  76%
		SMG_WINDOW::VSIZE  =   4%
		SMG_WINDOW::HPOS   =   3%
		SMG_WINDOW::VPOS   =   15%
		SMG_WINDOW::NITEMS =   3%
		SMG_WINDOW::FLAGS  =   0%
		SMG_WINDOW::HVIEW  =  76%
		SMG_WINDOW::VVIEW  =   5%
		SMG_WINDOW::VHPOS  =   3%
		SMG_WINDOW::VVPOS  =   15%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Var_class"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

		ACCTITLE$ = "Group    Description"
		ACCTYPE$(0%) = "7"
		ACCTYPE$(1%) = "BILL     Billings"
		ACCTYPE$(2%) = "BOFF     BuyOff"
		ACCTYPE$(3%) = "BURD     Burden"
		ACCTYPE$(4%) = "DLAB     Direct Labor"
		ACCTYPE$(5%) = "ILAB     Indirect Labor"
		ACCTYPE$(6%) = "PMAT     Parts Material"
		ACCTYPE$(7%) = "RMAT     Raw Material"

700		!
		! Declare channels
		!
		IF WP_CLOSELINE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF WP_CLOSELINE.READONLY%

			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_CLOSELINE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			WP_MAIN_CLOSEWIP = ERR
			CONTINUE 770
		END WHEN

		WP_CLOSELINE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_CLOSELINE.OPN"
		USE
			WP_MAIN_CLOSEWIP = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		WP_CLOSELINE.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(WP_CLOSELINE.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN = WP_CLOSELINE.CH%

		WHEN ERROR IN
			RESET #WP_CLOSELINE.CH%
			GET #WP_CLOSELINE.CH%, REGARDLESS
		USE
			CONTINUE 32767 IF ERR = 11%
			EXIT HANDLER
		END WHEN

	!******************************************************************
	! Display the background
	!
	! This option is used to display the background information on the
	! screen.  It must first clear any junk on the screen, and then
	! write the background onto it.
	!******************************************************************
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)


		DATA	01,03, "(01) Acct Class", &
			02,03, "(02) Account #", &
			03,03, "(03) Amount", &
			04,48, "Total", &
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

 Reentry:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Account Class\*
	!	.b
	!	.lm +5
	!	The General Ledger ^*Account Class\* field provides
	!	information on the state or classification of the account.
	!	.b
	!	This field will accommodate four (4) alpha numeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			WP_CLOSELINE::VCLASS = EDIT$(ENTR_3STRINGLIST(SCOPE, SMG_WINDOW::WNUMBER, &
				"1;27", TEMP$, &
				WP_CLOSELINE::VCLASS, MFLAG, "'E", MVALUE, &
				ACCTYPE$(), ACCTITLE$, "008"), -1%)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) General Ledger Account\*
	!	.b
	!	.lm +5
	!	The ^*General Ledger Account\* field enters the General
	!	Ledger Finished Goods Inventory
	!	account number to which this line will be debited.
	!	.b
	!	Pressing ^*List Choices\* will provide a list of valid
	!	General Ledger account numbers.
	!	.lm -5
	!
	! Index:
	!
	!--
			WP_CLOSELINE::VACCT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;27", TEMP$, &
				WP_CLOSELINE::VACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
				THEN
					WP_CLOSELINE::VACCT = GL_CHART::ACCT
				END IF
				GOTO Reentry
			END IF

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Amount\*
	!	.b
	!	.lm +5
	!	The ^*Amount\* field enteres the amount.
	!	.b
	!	The field may contain a figure as large as 9,999,999.99
	!	.lm -5
	!
	! Index:
	!	.x Amount
	!
	!--
			WP_CLOSELINE::VAMOUNT= ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"03;27",TEMP$, WP_CLOSELINE::VAMOUNT, MFLAG, &
				"#,###,###.##", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		WP_MAIN_CLOSEWIP = 0%

		SELECT MLOOP

		CASE 2%
			WP_MAIN_CLOSEWIP = FUNC_TESTENTRY(SMG_WINDOW, &
				WP_CLOSELINE::VACCT, &
				GL_CHART::DESCR, &
				"WP", MLOOP, "PRG", &
				"GL Account", GL_MAIN_CHART.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 30%), &
				2%, 48%,, SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			IF (MAIN_WINDOW(GL_MAIN_CHART.ID, &
				"Q0" + WP_CLOSELINE::VACCT) <> 1%)
			THEN
				GL_CHART::DESCR = &
					STRING$(LEN(GL_CHART::DESCR), A"?"B)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				GL_CHART::DESCR, 2%, 48%,, SMG$M_BOLD)

		END IF

		GOSUB 28100

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			FORMAT$(AMOUNT, "#,###,###.##"), 4%, 55%,, SMG$M_BOLD)

	!
	! Set WP_CLOSELINE_OLD value
	!
20500	CASE OPT_SETOLD
		WP_CLOSELINE_OLD = WP_CLOSELINE

	!
	! Restore WP_CLOSELINE_OLD value
	!
	CASE OPT_RESETOLD
		WP_CLOSELINE = WP_CLOSELINE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		WP_CLOSELINE_DEF = WP_CLOSELINE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		WP_CLOSELINE = WP_CLOSELINE_DEF

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		WP_CLOSELINE::JOB  = MVALUE
		WP_CLOSELINE::LFLAG  = "A"

		IF MFLAG = 1%
		THEN
			GOSUB 28100
			WP_CLOSELINE::VAMOUNT = - AMOUNT &
				IF WP_CLOSELINE::VAMOUNT = 0.0
		END IF


	!
	! Find the next record
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE WP_CLOSELINE::JOB + &
				WP_CLOSELINE::LFLAG + &
				WP_CLOSELINE::VCLASS, REGARDLESS

		END SELECT

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  VType WIPAcct#                 Amount"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "008,027,040"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = WP_CLOSELINE::VCLASS + "  " + &
				WP_CLOSELINE::VACCT + " "  + &
				FORMAT$(WP_CLOSELINE::VAMOUNT, "#,###,###.##")

		END SELECT

	!
	! Handle array of records
	!
	CASE OPT_SUBWIND

		SELECT MLOOP

		!
		! Find first record (if there is any)
		!
		CASE 1%

			! Set init value
			!
			SMG_WINDOW::CURREC = -1%

27110			!
			! Search for first record
			!
			WHEN ERROR IN
				GET #SMG_WINDOW::CHAN, &
					KEY #0% EQ MVALUE + "A", &
					REGARDLESS
			USE
				CONTINUE 28000 IF ERR = 155%
				EXIT HANDLER
			END WHEN

			SMG_WINDOW::CURREC = 0%

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
					FIND #SMG_WINDOW::CHAN, KEY #0% GE MVALUE + &
						WP_CLOSELINE::LFLAG + &
						WP_CLOSELINE::VCLASS, REGARDLESS
				USE
					CONTINUE 28000
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

			IF WP_CLOSELINE::JOB + WP_CLOSELINE::LFLAG = &
				MVALUE + "A"
			THEN
				SMG_WINDOW::CURREC = 0%
			END IF

		!
		! Change key
		!
		CASE 6%
			WP_CLOSELINE::JOB  = MVALUE

		END SELECT

	END SELECT

 ExitFunction:
28000	EXIT FUNCTION

28100	!
	! Search for first record
	!
	IF WP_CLOSELINE.TOTALONLY% = 0%
	THEN
		WP_CLOSELINE.CH% = 0%
		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_CLOSELINE.OPN"
		USE
			CONTINUE 28120 IF RR = 131%
			EXIT HANDLER
		END WHEN

		WP_CLOSELINE.TOTALONLY% = WP_CLOSELINE.CH%
		WP_CLOSELINE.CH% = SMG_WINDOW::CHAN
	END IF

	AMOUNT = 0.0
	WP_CLOSELINE_TOTAL = WP_CLOSELINE
	WHEN ERROR IN
		FIND #WP_CLOSELINE.TOTALONLY%, &
			KEY #0% EQ WP_CLOSEJOUR::JOB + "", &
			REGARDLESS
	USE
		CONTINUE 28120 IF ERR = 155% OR ERR = 131%
		EXIT HANDLER
	END WHEN

	!
	! Get a record
	!
28110	WHEN ERROR IN
		GET #WP_CLOSELINE.TOTALONLY%, REGARDLESS
	USE
		CONTINUE 28120 IF ERR = 11%
		EXIT HANDLER
	END WHEN

	GOTO 28120 IF WP_CLOSEJOUR::JOB <> WP_CLOSELINE::JOB
	AMOUNT = AMOUNT + WP_CLOSELINE::VAMOUNT
	GOTO 28110

28120	WP_CLOSELINE = WP_CLOSELINE_TOTAL
	RETURN

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
	!+-+-+
	!++
	! Abstract:FLD007
	!	^*(07) General Ledger Account\*
	!	.b
	!	.lm +5
	!	The ^*General Ledger Account\* field enters the General
	!	Ledger Finished Goods Inventory
	!	account number to which this line will be debited.
	!	.b
	!	Pressing ^*List Choices\* will provide a list of valid
	!	General Ledger account numbers.
	!	.lm -5
	!
	! Index:
	!	.x Finished Goods>Account
	!	.x Account>Finished Goods
	!
	!--
