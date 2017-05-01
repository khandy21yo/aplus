1	%TITLE "Uuse Journal Line Item Maintenance"
	%SBTTL "GL_MAIN_USERJOUR"
	%IDENT "V3.6a Calico"

	FUNCTION LONG GL_MAIN_USERJOUR(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1998 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the
	! use or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Line__items\* portion of this screen is used to indicate how
	!	the transaction is to be allocated to various accounts and sub-codes.
	!	.b
	!	This portion of the screen will scroll, allowing as many as forty
	!	(40) line item distributions to be made.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_MAIN_USERJOUR/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP GL_MAIN_USERJOUR
	!	$ DELETE GL_MAIN_USERJOUR.OBJ;*
	!
	! Author:
	!
	!	11/19/98 - Kevin Handy
	!
	! Modification history:
	!
	!	12/08/98 - Kevin Handy
	!		Display customer/vendor name on entry
	!
	!	03/08/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	12/17/2000 - Kevin Handy
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
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[GL.OPEN]GL_USERJOUR.HB"
	MAP (GL_USERJOUR)	GL_USERJOUR_CDD		GL_USERJOUR
	MAP (GL_USERJOUR_OLD)	GL_USERJOUR_CDD		GL_USERJOUR_OLD, GL_USERJOUR2

	%INCLUDE "SOURCE:[GL.OPEN]GL_USERHEAD.HB"
	MAP (GL_USERHEAD)	GL_USERHEAD_CDD		GL_USERHEAD

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	DECLARE SB_SUBACCOUNT_CDD SB_SUBACCOUNT_READ

	%INCLUDE "SOURCE:[GL.OPEN]GL_USERDEF.HB"
	MAP	(GL_USERDEF)	GL_USERDEF_CDD	GL_USERDEF

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR)		AP_VENDOR_CDD		AP_VENDOR

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
		STRING	JLINE = 4	! Line number
		REAL	AMOUNT		! Amount for record
		REAL	QTY		! Quanity for record
		REAL	SIGN		! Sign for Amount
	END RECORD

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (TT_GL_USERJOY) RARRAY_RECORD RARRAY(300%)

	COM (CH_GL_USERHEAD) &
		GL_USERHEAD.CH%

	COM (CH_GL_USERJOUR) &
		GL_USERJOUR.CH%, &
		GL_USERJOUR.READONLY%

	COM (TT_GL_USERJOUR) &
		BATCH_NO$ = 2%

	COM (CH_GL_USERDEF) &
		GL_USERDEF.CH%, &
		GL_USERDEF.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY

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

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Line items: " + BATCH_NO$
		SMG_WINDOW::NHELP = "GL_MAIN_USERJOUR"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 14%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 6%
		SMG_WINDOW::NITEMS= 7%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 3%
		SMG_WINDOW::BOTLIN = 13%

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF GL_USERJOUR.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF GL_USERJOUR.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_USERJOUR.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			GL_MAIN_USERJOUR = ERR
			CONTINUE 770
		END WHEN

		GL_USERJOUR.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_USERJOUR.OPN"
		USE
			GL_MAIN_USERJOUR = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		GL_USERJOUR.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(GL_USERJOUR.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = GL_USERJOUR.CH%
		WHEN ERROR IN
			RESET #GL_USERJOUR.CH%
			GET #GL_USERJOUR.CH%, REGARDLESS
		USE
			CONTINUE 32767
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

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  (01)    (02)              (03)      " + &
			"   (04)       (05)     (06)    (07)     ", &
			1%, 1%, , SMG$M_REVERSE)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  Line  Description          Account  " + &
			"    Dollars   Units    XRef     Invoice ", &
			2%, 1%, , SMG$M_REVERSE)

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Extra display stuff
	!
	CASE OPT_DISPLAY

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		!
		! Generate totals
		!
		AMOUNT = 0.0
		QTY = 0.0

		FOR I% = 1% TO SMG_WINDOW::TOTREC

			AMOUNT = AMOUNT + RARRAY(I%)::AMOUNT * RARRAY(I%)::SIGN
			QTY    = QTY    + RARRAY(I%)::QTY

		NEXT I%

		!
		! Display totals
		!
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"Number of lines" + &
			FORMAT$(SMG_WINDOW::TOTREC, "####") + &
			"                            " + &
			FORMAT$(AMOUNT, "#####.##") + &
			FORMAT$(QTY, " #####.##") + &
			"              ", &
			SMG_WINDOW::VSIZE, 1%, , SMG$M_REVERSE)

		!
		! Paint lines on screen
		!
		FOR I% = 1% TO 6%

			A% = VAL%(MID("007,028,041,050,059,069", I% * 4% - 3%, 3%))

			SMG_STATUS% = SMG$DRAW_LINE(SMG_WINDOW::WNUMBER, &
				1%, A%, SMG_WINDOW::BOTLIN, A%)

		NEXT I%

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

		XLINE$ = NUM1$(SMG_WINDOW::CURLIN)

		TEMP1% = SCOPE::SCOPE_EXIT

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Line _#\*
	!	.b
	!	.lm +5
	!	The ^*Line Number\* field refers back to the definition
	!	file, and must be defined in there to be allowed as an
	!	entry in the journal.
	!	.lm -5
	!
	! Index:
	!	.x Line Number>User Journal
	!
	!--
			GL_USERJOUR::JLINE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";3", TEMP$, &
				GL_USERJOUR::JLINE, MFLAG, "'LLL", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_USERDEF.ID, &
					"V0" + GL_USERJOUR::JCODE) = 1%)
				THEN
					GL_USERJOUR::JLINE = &
						GL_USERDEF::JLINE
				END IF
				GOTO E0Loop
			END IF
		CASE 2%
	!++
	! Abstract:FLD002
	!	.x Description>User Journal
	!	^*(02) Description	40 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field describes what the line contains.
	!	It will be pulled from the definition file, unless it is
	!	blank there.
	!	.lm -5
	!
	! Index:
	!
	!--
			GOSUB NeedDefine
			IF GL_USERDEF::DESCRIPTION = "" OR (MFLAG AND 32%) <> 0%
			THEN
				TEMP% = MFLAG
			ELSE
				TEMP% = MFLAG% OR 1%
			END IF

			GL_USERJOUR::DESCRIPTION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";8", TEMP$, &
				GL_USERJOUR::DESCRIPTION, TEMP%, &
				"'LLLLLLLLLLLLLLLLLLL", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	.x Account>User Journal
	!	^*(03) G/L Account\*
	!	.b
	!	.lm +5
	!	The ^*G/L Account\* field defines what account this entry should
	!	be posted to.
	!	It will be pulled from the definition file unless it is blank there.
	!	.lm -5
	!
	! Index:
	!
	!--
			GOSUB NeedDefine

			IF GL_USERDEF::ACCOUNT = "" OR (MFLAG AND 32%) <> 0%
			THEN
				TEMP% = MFLAG
			ELSE
				TEMP% = MFLAG% OR 1%
			END IF

			GL_USERJOUR::ACCOUNT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";29", TEMP$, &
				GL_USERJOUR::ACCOUNT, TEMP%, &
				"'LLLLLLLLLLL", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX  ") = 1%)
				THEN
					GL_USERJOUR::ACCOUNT = GL_CHART::ACCT
				END IF
				GOTO E0Loop
			END IF

		CASE 4%
	!++
	! Abstract:FLD004
	!	.x Dollars>User Journal
	!	^*(04) Dollars\*
	!	.b
	!	.lm +5
	!	The ^*Dollars\* field contains the dollar amount that will
	!	be posted to the specified account for this line item.
	!	.lm -5
	!
	! Index:
	!
	!--
			GL_USERJOUR::DOLLARS = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";42", TEMP$, &
				GL_USERJOUR::DOLLARS, MFLAG, &
				"#####.##", MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	.x Units>User Journal
	!	^*(05) Units\*
	!	.b
	!	.lm +5
	!	The ^*Units\* field contains the units
	!	that will be posted for the specified line.
	!	.b
	!	This field will only be available when the definition
	!	says that it is useful.
	!	.lm -5
	!
	! Index:
	!
	!--
			GOSUB NeedDefine
			IF GL_USERDEF::UNITFLAG = "Y" OR (MFLAG AND 32%) <> 0%
			THEN
				TEMP% = MFLAG
				GL_USERJOUR::UNITS = ENTR_3NUMBER(SCOPE, &
					SMG_WINDOW::WNUMBER, &
					XLINE$ + ";51", TEMP$, &
					GL_USERJOUR::UNITS, TEMP%, &
					"#####.##", MVALUE)
			END IF

		CASE 6%
	!++
	! Abstract:FLD006
	!	.x XRef>User Journal
	!	^*(06) Cross Reference\*
	!	.b
	!	.lm +5
	!	The ^*Cross Reference\* field contains the
	!	customer/vendor/employee number (as defined by the definition
	!	file for the specific line).
	!	.b
	!	This field is only available when the definition for the line
	!	allows it.
	!	.lm -5
	!
	! Index:
	!	.x Cross Reference>User Journal
	!
	!--
			GOSUB NeedDefine
			IF ((GL_USERDEF::ARPFLAG = "") OR &
				(TRM$(GL_USERDEF::XREF) <> "")) AND (MFLAG AND 32%) = 0%
			THEN
				TEMP% = MFLAG OR 1%
			ELSE
				TEMP% = MFLAG
			END IF

			GL_USERJOUR::XREF = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";60", TEMP$, &
				GL_USERJOUR::XREF, TEMP%, "'LLLLLLLL", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				SELECT GL_USERDEF::ARPFLAG
				CASE "AR"
					IF (MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, &
						"VX  ") = 1%)
					THEN
						GL_USERJOUR::XREF = &
							AR_35CUSTOM::CUSNUM
					END IF
				CASE "AP"
					IF (MAIN_WINDOW(AP_MAIN_VENDOR.ID, &
						"VX  ") = 1%)
					THEN
						GL_USERJOUR::XREF = &
							AP_VENDOR::VENNUM
					END IF
				END SELECT
				GOTO E0Loop
			END IF

			IF (TEMP% AND 1%) = 0%
			THEN
				SELECT GL_USERDEF::ARPFLAG
				CASE "AR"
					V% = FUNC_TESTENTRY(SMG_WINDOW, &
						GL_USERJOUR::XREF, AR_35CUSTOM::CUSNAM, &
						"AR", MLOOP, "CUSTOM", &
						"Customer Number", AR_MAIN_35CUSTOM.ID)
					CALL ENTR_3MESSAGE(SCOPE, &
						AR_35CUSTOM::CUSNUM + " " + &
						AR_35CUSTOM::CUSNAM, 1%)

				CASE "AP"
					V% = FUNC_TESTENTRY(SMG_WINDOW, &
						GL_USERJOUR::XREF, AP_VENDOR::VENNAM, &
						"AP", MLOOP, "VENDOR", &
						"Vendor number", AP_MAIN_VENDOR.ID)
					CALL ENTR_3MESSAGE(SCOPE, &
						AP_VENDOR::VENNUM + " " + &
						AP_VENDOR::VENNAM, 1%)
				END SELECT
			END IF

		CASE 7%
	!++
	! Abstract:FLD007
	!	.b
	!	.lm +5
	!	.lm -5
	!
	! Index:
	!
	!--
			GOSUB NeedDefine
			IF (GL_USERDEF::ARPFLAG = "")
			THEN
				TEMP% = MFLAG OR 1%
			ELSE
				TEMP% = MFLAG
			END IF

			GL_USERJOUR::INVNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";70", TEMP$, &
				GL_USERJOUR::INVNUM, TEMP%, "'LLLLLLLLL", MVALUE)


		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		GL_MAIN_USERJOUR = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Pull off items defined by the header
			!
			GOSUB NeedDefine
			IF GL_USERDEF::JCODE = GL_USERJOUR::JCODE AND &
				GL_USERDEF::JLINE = GL_USERJOUR::JLINE
			THEN
				GL_USERJOUR::DESCRIPTION = GL_USERDEF::DESCRIPTION
				GL_USERJOUR::ACCOUNT = GL_USERDEF::ACCOUNT
				GL_USERJOUR::UNITS = 0.0
				GL_USERJOUR::XREF = GL_USERDEF::XREF

				!
				! Repaint everything
				!
				V% = GL_MAIN_USERJOUR(SMG_WINDOW, &
					OPT_ENTRY, LOOP%, 1%, "") &
					FOR LOOP% = 1% TO SMG_WINDOW::NITEMS
			ELSE
				!
				! Undefined line
				!
				GL_MAIN_USERJOUR = 1%
			END IF

		CASE 3%
			!
			! Is the input defined?
			!
			GOSUB NeedDefine
			IF GL_USERDEF::ACCOUNT = ""
			THEN
				GL_MAIN_USERJOUR = FUNC_TESTENTRY(SMG_WINDOW, &
					GL_USERJOUR::ACCOUNT, GL_CHART::DESCR, &
					"AR", MLOOP, "ACCT", &
					"Account number", GL_MAIN_CHART.ID)
			END IF

		CASE 6%
			IF (GL_USERDEF::ARPFLAG <> "  ") AND &
				(GL_USERDEF::XREF = "")
			THEN
				SELECT GL_USERDEF::ARPFLAG
				CASE "AR"
					GL_MAIN_USERJOUR = FUNC_TESTENTRY(SMG_WINDOW, &
						GL_USERJOUR::XREF, AR_35CUSTOM::CUSNAM, &
						"AR", MLOOP, "CUSTOM", &
						"Customer Number", AR_MAIN_35CUSTOM.ID)

				CASE "AP"
					GL_MAIN_USERJOUR = FUNC_TESTENTRY(SMG_WINDOW, &
						GL_USERJOUR::XREF, AP_VENDOR::VENNAM, &
						"AP", MLOOP, "VENDOR", &
						"Vendor number", AP_MAIN_VENDOR.ID)
				END SELECT
			END IF

		END SELECT

	!
	! Set GL_USERJOUR_OLD value
	!
20500	CASE OPT_SETOLD
		GL_USERJOUR_OLD = GL_USERJOUR

	!
	! Restore GL_USERJOUR_OLD value
	!
	CASE OPT_RESETOLD
		GL_USERJOUR = GL_USERJOUR_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		GL_USERJOUR2 = GL_USERJOUR

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		GL_USERJOUR = GL_USERJOUR2

		GL_USERJOUR::JCODE = GL_USERHEAD::JCODE

20600		!
		! Try for the next line number
		!
		IF SMG_WINDOW::TOTREC = 0%
		THEN
			TEMP$ = "    "
		ELSE
			TEMP$ = RARRAY(SMG_WINDOW::TOTREC)::JLINE
		END IF

		WHEN ERROR IN
			GET #GL_USERDEF.CH%, &
				KEY #0 GT GL_USERJOUR::JCODE + TEMP$, &
				REGARDLESS
		USE
			CONTINUE 20690
		END WHEN

		!
		! If we got it, great
		!
		IF (GL_USERDEF::JCODE = GL_USERJOUR::JCODE)
		THEN
			GL_USERJOUR::JLINE = GL_USERDEF::JLINE
			EXIT FUNCTION
		END IF

20690		!
		! If we don't get it, fake up something
		!	(probibly a useless something, but something)
		!
		IF SMG_WINDOW::TOTREC = 0%
		THEN
			GL_USERJOUR::JLINE = "0001"
		ELSE
			GL_USERJOUR::JLINE = &
				RARRAY(SMG_WINDOW::TOTREC)::JLINE
			V% = FUNC_INCREMENT(GL_USERJOUR::JLINE)
		END IF

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #GL_USERJOUR.CH%, &
				KEY #0% GE GL_USERJOUR::JCODE + "", &
				REGARDLESS
		END SELECT

	!
	! Handle array of records
	!
27000	CASE OPT_ARRAY

		!
		! Select sub-option of array
		!
		SELECT MLOOP

		!
		! Load array with line items
		!
		CASE 1%

			!
			! Empty array
			!
			SMG_WINDOW::TOTREC = 0%

27110			!
			! Search for first record
			!
			WHEN ERROR IN
				FIND #SMG_WINDOW::CHAN, &
					KEY #0% GE GL_USERHEAD::JCODE + "", &
					REGARDLESS
			USE
				CONTINUE 28000
			END WHEN

27120			!
			! Get a record
			!
			WHEN ERROR IN
				GET #SMG_WINDOW::CHAN
			USE
				CONTINUE 28000 IF ERR = 11%
				EXIT HANDLER
			END WHEN

			IF GL_USERJOUR::JCODE = GL_USERHEAD::JCODE
			THEN
				GOSUB NeedDefine

				!
				! Add information to array
				!
				SMG_WINDOW::TOTREC = SMG_WINDOW::TOTREC + 1%
				RARRAY(SMG_WINDOW::TOTREC)::LINRFA = &
					GETRFA(SMG_WINDOW::CHAN)
				RARRAY(SMG_WINDOW::TOTREC)::JLINE = &
					GL_USERJOUR::JLINE
				RARRAY(SMG_WINDOW::TOTREC)::AMOUNT = &
					GL_USERJOUR::DOLLARS
				RARRAY(SMG_WINDOW::TOTREC)::QTY = &
					GL_USERJOUR::UNITS
				IF GL_USERDEF::SIGNED = "-"
				THEN
					RARRAY(SMG_WINDOW::TOTREC)::SIGN = -1.0
				ELSE
					RARRAY(SMG_WINDOW::TOTREC)::SIGN = 1.0
				END IF

				GOTO 27120
			END IF

		!
		! Remove one element of the array
		!
		CASE 2%
			!
			! Remove item pointed to by Mflag
			!
			FOR I% = MFLAG TO SMG_WINDOW::TOTREC - 1%
				RARRAY(I%) = RARRAY(I% + 1%)
			NEXT I%

		!
		! Set array item to current record
		!
		CASE 3%
			GOSUB NeedDefine

			RARRAY(MFLAG)::LINRFA = GETRFA(SMG_WINDOW::CHAN)
			RARRAY(MFLAG)::JLINE = GL_USERJOUR::JLINE
			RARRAY(MFLAG)::AMOUNT = GL_USERJOUR::DOLLARS
			RARRAY(MFLAG)::QTY = GL_USERJOUR::UNITS
			IF GL_USERDEF::SIGNED = "-"
			THEN
				RARRAY(MFLAG)::SIGN = -1.0
			ELSE
				RARRAY(MFLAG)::SIGN = 1.0
			END IF

		!
		! Load in current record, locked
		!
		CASE 4%
27200			GET #SMG_WINDOW::CHAN, RFA RARRAY(MFLAG)::LINRFA

		!
		! Load in current record, unlocked
		!
		CASE 5%
			GET #SMG_WINDOW::CHAN, RFA RARRAY(MFLAG)::LINRFA, &
				REGARDLESS

		!
		! Change the current record's key to match header.  The
		! new key is probibly passes through MVALUE, unless some
		! other means is devised.
		!
		CASE 6%
			GL_USERJOUR::JCODE = RIGHT(MVALUE, 2%)

		END SELECT
	END SELECT

28000	EXIT FUNCTION

 NeedDefine:
28100	!*******************************************************************
	! Dig through the userdef file for the proper definition for
	! this line
	!*******************************************************************

	IF GL_USERDEF::JCODE = GL_USERJOUR::JCODE AND &
		GL_USERDEF::JLINE = GL_USERJOUR::JLINE
	THEN
		RETURN
	END IF

	WHEN ERROR IN
		GET #GL_USERDEF.CH%, &
			KEY #0% EQ GL_USERJOUR::JCODE + GL_USERJOUR::JLINE, &
			REGARDLESS
	USE
		CONTINUE 28190
	END WHEN

28190	RETURN

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
