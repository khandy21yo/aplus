1	%TITLE "Sales Journal Header Maintenance"
	%SBTTL "GL_MAIN_TRIJOUR"
	%IDENT "V3.6a Calico"

	FUNCTION LONG GL_MAIN_TRIJOUR(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1995 BY
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
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Maintain Sales Journal\* option
	!	maintains regular sales and service charge journals.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_MAIN_TRIJOUR/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP GL_MAIN_TRIJOUR
	!	$ DELETE GL_MAIN_TRIJOUR.OBJ;*
	!
	! Author:
	!
	!	06/06/95 - Kevin Handy
	!
	! Modification history:
	!
	!	06/15/95 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/20/95 - Kevin Handy
	!		Lose /NOWARN on compile.
	!
	!	02/26/96 - Kevin Handy
	!		Modifications to the way titles work.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/09/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	12/12/2000 - Kevin Handy
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
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	!
	! Maps
	!
	%INCLUDE "SOURCE:[GL.OPEN]GL_TRICONTROL.HB"
	MAP (GL_TRICONTROL)	GL_TRICONTROL_CDD		GL_TRICONTROL

	%INCLUDE "SOURCE:[GL.OPEN]GL_TRIJOUR.HB"
	MAP (GL_TRIJOUR)	GL_TRIJOUR_CDD		GL_TRIJOUR
	MAP (GL_TRIJOUR_OLD)	GL_TRIJOUR_CDD		GL_TRIJOUR_OLD, GL_TRIJOUR2

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_GL_TRIJOUR) &
		GL_TRIJOUR.CH%, &
		GL_TRIJOUR.READONLY%

	COM (CH_GL_TRICONTROL) &
		GL_TRICONTROL.CH%, &
		GL_TRICONTROL.READONLY%

	COM (TT_GL_TRIJOUR) &
		BATCH_NO$ = 2%

	!
	! External functions
	!

	!
	! Declare some variables
	!
	DECLARE RFA TEMP_RFA

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
		SMG_WINDOW::DESCR = "Sales Journal " + BATCH_NO$ + &
			" Maintenance"
		SMG_WINDOW::NHELP = "GL_MAIN_TRIJOUR"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 36%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Location"
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%

		!
		! Load in defaults
		!
		GL_TRIJOUR::ACCOUNT(SETLOOP%) = "" &
			FOR SETLOOP% = 0% TO 20%
		GL_TRIJOUR::DESCRIPTION(SETLOOP%) = "" &
			FOR SETLOOP% = 0% TO 20%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF GL_TRIJOUR.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF GL_TRIJOUR.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_TRIJOUR.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			GL_MAIN_TRIJOUR = ERR
			CONTINUE 770
		END WHEN

		GL_TRIJOUR.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_TRIJOUR.OPN"
		USE
			GL_MAIN_TRIJOUR = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		GL_TRIJOUR.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(GL_TRIJOUR.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = GL_TRIJOUR.CH%
		WHEN ERROR IN
			RESET #GL_TRIJOUR.CH%
			GET #GL_TRIJOUR.CH%, REGARDLESS
		USE
			CONTINUE 30000
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

		DATA	1,  1, "(01) Location", &
			1, 40, "(02) Date", &
			2,  1, "(03)", &
			3,  1, "(04)", &
			3, 40, "(05)", &
			4,  1, "------------------------------------------------------------------------------", &
			5,  1, "Cash Paid Outs", &
			6,  1, "(06)", &
			7,  1, "(09)", &
			8,  1, "(12)", &
			9,  1, "(15)", &
			10, 1, "(18)", &
			11, 1, "(21)", &
			12, 1, "(24)", &
			13, 1, "(27)", &
			14, 1, "(30)", &
			15, 1, "------------------------------------------------------------------------------", &
			16, 1, "(33)", &
			17, 1, "(34)", &
			18, 1, "(35)", &
			16,40, "(36)", &
			18,40, "     Balance", &
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
	! Extra display stuff
	!
	CASE OPT_DISPLAY

		!
		! Is the input defined?
		!
		GOSUB SeekTriControl
		GOSUB ShowTriControl

		!
		! Generate totals
		!
		AMOUNT = 0.0

		FOR I% = 0% TO 20%

			SELECT GL_TRICONTROL::FLAG(I%)

			CASE " ", "+"
				AMOUNT = AMOUNT + GL_TRIJOUR::AMOUNT(I%)

			CASE "-"
				AMOUNT = AMOUNT - GL_TRIJOUR::AMOUNT(I%)

			END SELECT

		NEXT I%

		!
		! Display totals
		!
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			FORMAT$(AMOUNT, " ########.##"), &
			18%, 55%)


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

		TEMP1% = SCOPE::SCOPE_EXIT

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	.ts 55
	!	^*(01) Invoice	8 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Invoice\* field enters the number or
	!	reference for a particular document.
	!	.lm -5
	!
	! Index:
	!	.x Sales Journal>Invoice Number
	!	.x Invoice>Number in Sales Journal
	!
	!--

			GL_TRIJOUR::LOCATION = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"1;16", TEMP$, &
				GL_TRIJOUR::LOCATION, MFLAG, "'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Customer\*
	!	.b
	!	.lm +5
	!	The ^*Customer\* field is provided to enter a customer code which
	!	will identify a particular customer.  This number should have previously
	!	been entered in the Customer Master Name and Address screen.
	!	.b
	!	If the code entered is valid, the customers name and
	!	address information will appear.
	!	.b
	!	Pressing ^*List Choices\* will cause a list of valid customer codes
	!	to be displayed.
	!	.b
	!	By pressing the ^*F17\* key, the system will display the screen where
	!	additional customer codes may be entered.  After the screen has been
	!	completed the system will automatically return to the
	!	Sales Journal maintenance routine.
	!	.lm -5
	!
	! Index:
	!	.x Sales Journal>Customer Number
	!	.x Customer>Customer Number in Sales Journal
	!
	!--
			GL_TRIJOUR::TRANDATE = &
				ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"1;56", TEMP$, &
				GL_TRIJOUR::TRANDATE, MFLAG, &
				"8", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	.ts 55
	!	^*(03) Description	26 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field enters a
	!	description in reference to the transaction represented by an
	!	invoice.
	!	.lm -5
	!
	! Index:
	!	.x Sales Journal>Description
	!	.x Description>Sales Journal
	!
	!--
			GL_TRIJOUR::AMOUNT(1%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;18", TEMP$, &
				GL_TRIJOUR::AMOUNT(1%), &
				MFLAG, "#######.##", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	.ts 55
	!	^*(04) Transaction Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*Transaction Date\* field is provided to enter the
	!	date for a particular transaction.
	!	.b
	!	This field requires an entry and will not default to the
	!	current date.
	!	.lm -5
	!
	! Index:
	!	.x Transaction Date>Sales Journal
	!	.x Sales Journal>Transaction Date
	!
	!--
			GL_TRIJOUR::AMOUNT(2%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"3;18", TEMP$, &
				GL_TRIJOUR::AMOUNT(2%), &
				MFLAG, "#######.##", MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	.ts 55
	!	^*(05) Accounts Receivable/Cash	20 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Accounts Receivable/Cash\* field is provided to enter the appropriate
	!	"Accounts Receivable" or "Cash" account number established in the General Ledger
	!	Chart of Accounts.  The selection is dependent upon whether a sale is
	!	a charge sale or a cash sale to be recorded as a transaction in
	!	a customer's file.
	!	.b
	!	Pressing ^*List Choices\* will cause a list of General Ledger Chart
	!	of Account numbers to be displayed.
	!	.lm -5
	!
	! Index:
	!	.x Accounts Receivable Account>Sales Journal
	!	.x Cash Account>Sales Journal
	!	.x Sales Journal>Accounts Receivable Account
	!	.x Sales Journal>Cash Account
	!
	!--
			GL_TRIJOUR::AMOUNT(3%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"3;58", TEMP$, &
				GL_TRIJOUR::AMOUNT(3%), &
				MFLAG, "#######.##", MVALUE)

		CASE 6%, 9%, 12%, 15%, 18%, 21%, 24%, 27%, 30%

	!++
	! Abstract:FLD006
	!
	! Index:
	!
	!--
			SCOPE::PRG_ITEM = "FLD06"

			ITEM% = MLOOP / 3% + 2%

			IF GL_TRICONTROL::DESCRIPTION(ITEM%) = ""
			THEN
				M1FLAG% = MFLAG
			ELSE
				M1FLAG% = MFLAG OR 1%
				SCOPE::SCOPE_EXIT = TEMP1%
			END IF

			GL_TRIJOUR::DESCRIPTION(ITEM%) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				NUM1$(ITEM% + 2%) + ";6", &
				TEMP$, &
				GL_TRIJOUR::DESCRIPTION(ITEM%), &
				M1FLAG%, "'LLLLLLLLLLLLLLLLLLL", MVALUE)

		CASE 7%, 10%, 13%, 16%, 19%, 22%, 25%, 28%, 31%

	!++
	! Abstract:FLD007
	!
	! Index:
	!
	!--
			SCOPE::PRG_ITEM = "FLD07"

			ITEM% = MLOOP / 3% + 2%

			GL_TRIJOUR::AMOUNT(ITEM%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				NUM1$(ITEM% + 2%) + ";30", &
				TEMP$, &
				GL_TRIJOUR::AMOUNT(ITEM%), &
				MFLAG, "#######.##", MVALUE)

		CASE 8%, 11%, 14%, 17%, 20%, 23%, 26%, 29%, 32%

	!++
	! Abstract:FLD008
	!
	! Index:
	!
	!--
			SCOPE::PRG_ITEM = "FLD08"

			ITEM% = MLOOP / 3% + 2%

			IF GL_TRICONTROL::ACCOUNT(ITEM%) = ""
			THEN
				M1FLAG% = MFLAG
			ELSE
				M1FLAG% = MFLAG OR 1%
				SCOPE::SCOPE_EXIT = TEMP1%
			END IF

			GL_TRIJOUR::ACCOUNT(ITEM%) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				NUM1$(ITEM% + 2%) + ";45", &
				TEMP$, &
				GL_TRIJOUR::ACCOUNT(ITEM%), &
				M1FLAG%, "'E", MVALUE)

		CASE 33%
	!++
	! Abstract:FLD033
	!	.ts 55
	!	^*(07) Receipt _#	8 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Receipt _#\* field is provided to enter the number of a
	!	receipt if issued for a cash sale. When cash sales are accounted for
	!	using cash register closing totals, a receipt number would not
	!	necessarily be applicable. This field may be bypassed.
	!	.b
	!	^*Note:\* During an add function, this field is bypassed unless the
	!	transaction type is either a Cash Sale (02) or a Credit Memo (08).
	!	.lm -5
	!
	! Index:
	!	.x Receipt #>Sales Journal
	!	.x Sales Journal>Receipt #
	!
	!--
			GL_TRIJOUR::AMOUNT(14%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"16;18", TEMP$, &
				GL_TRIJOUR::AMOUNT(14%), &
				MFLAG, "#######.##", MVALUE)

		CASE 34%
	!++
	! Abstract:FLD034
	!	.ts 55
	!	^*(08) Check _#	6 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Check _#\* field is provided to enter the number of a check
	!	issued by a customer in the event of a cash sale.  If a check was
	!	not received for the sale, or if the record represents a summary of
	!	cash sales such as a register total, this field would appropriately
	!	be left blank.
	!	.b
	!	During an Add function, this field is bypassed unless the
	!	transaction type represents either a Cash Sale or a Credit Memo.
	!	.lm -5
	!
	! Index:
	!	.x Check #>Sales Journal
	!	.x Sales Journal>Check #
	!
	!--
			GL_TRIJOUR::AMOUNT(15%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"17;18", TEMP$, &
				GL_TRIJOUR::AMOUNT(15%), &
				MFLAG, "#######.##", MVALUE)

		CASE 35%
	!++
	! Abstract:FLD035
	!	.ts 55
	!	^*(09) Deposit _#	6 Characters\*
	!	.b
	!	.lm +5
	!	The ^*Deposit _#\* field enters the number
	!	of the bank deposit in which a cash receipt is included. If CMC's
	!	Check Reconciliation System is used, the deposit _# field ^&must\& contain
	!	a valid deposit number. The use of this field is optional.
	!	If the Check Reconciliation system is used, it is important that
	!	deposit numbers assigned be outside the parameters established for
	!	check numbers.
	!	.b
	!	^*Note:\* During an Add function, this field is bypassed unless the
	!	transaction type is either a Cash Sale or a Credit Memo.
	!	.lm -5
	!
	! Index:
	!	.x Deposit #>Sales Journal
	!	.x Sales Journal>Deposit #
	!
	!--
			GL_TRIJOUR::AMOUNT(16%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"18;18", TEMP$, &
				GL_TRIJOUR::AMOUNT(16%), &
				MFLAG, "#######.##", MVALUE)

		CASE 36%
	!++
	! Abstract:FLD036
	!	.x Sales Journal>Amount
	!	^*(10) Amount\*
	!	.b
	!	.lm +5
	!	The ^*Amount\* field in the Sales Journal is provided to enter the
	!	total invoice amount, including any freight, sales taxes, etc.
	!	.b
	!	^*Note:\* Sales tax will be a separate line item in the distribution.
	!	.lm -5
	!
	! Index:
	!	.x Amount>Sales Journal
	!
	!--
			GL_TRIJOUR::AMOUNT(17%) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"16;64", TEMP$, &
				GL_TRIJOUR::AMOUNT(17%), &
				MFLAG, "#######.##", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		GL_MAIN_TRIJOUR = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Is the input defined?
			!
			GOSUB SeekTriControl

			IF FOUND_CONTROL% = 0%
			THEN
				GL_MAIN_TRIJOUR = 1%
			ELSE
				GOSUB SetTriControl
			END IF

			GOSUB ShowTriControl

		END SELECT

	!
	! Display additional information
	!
	CASE OPT_DISPLAY

		GL_MAIN_TRIJOUR = 0%

	!
	! Set GL_TRIJOUR_OLD value
	!
20500	CASE OPT_SETOLD
		GL_TRIJOUR_OLD = GL_TRIJOUR

	!
	! Restore GL_TRIJOUR_OLD value
	!
	CASE OPT_RESETOLD
		GL_TRIJOUR = GL_TRIJOUR_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		GL_TRIJOUR2 = GL_TRIJOUR

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		GL_TRIJOUR = GL_TRIJOUR2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = " Location  Date"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "011"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = GL_TRIJOUR::LOCATION + " " + &
				PRNT_DATE(GL_TRIJOUR::TRANDATE, 8%)

		END SELECT
	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #GL_TRIJOUR.CH%, &
				KEY #0% GE GL_TRIJOUR::LOCATION + &
				GL_TRIJOUR::TRANDATE, &
				REGARDLESS
		END SELECT

	END SELECT

	EXIT FUNCTION

25000	!***************************************************************
	! Look for control record for this location
	!***************************************************************

 SeekTriControl:

	FOUND_CONTROL% = 0%

	IF GL_TRICONTROL::LOCATION = GL_TRIJOUR::LOCATION
	THEN
		FOUND_CONTROL% = -1%
		RETURN
	END IF

	WHEN ERROR IN
		GET #GL_TRICONTROL.CH%, &
			KEY #0% EQ GL_TRIJOUR::LOCATION + "", REGARDLESS
	USE
		CONTINUE 25090
	END WHEN

	FOUND_CONTROL% = -1%

25090	RETURN

25100	!***************************************************************
	! Set this record to match the control record
	!***************************************************************

 SetTriControl:

	GL_TRIJOUR::ACCOUNT(SETLOOP%) = GL_TRICONTROL::ACCOUNT(SETLOOP%) &
		IF GL_TRICONTROL::ACCOUNT(SETLOOP%) <> "" &
		FOR SETLOOP% = 0% TO 20%

	GL_TRIJOUR::DESCRIPTION(SETLOOP%) = GL_TRICONTROL::DESCRIPTION(SETLOOP%) &
		IF GL_TRICONTROL::DESCRIPTION(SETLOOP%) <> "" &
		FOR SETLOOP% = 0% TO 20%


	RETURN

	!*******************************************************************
	! Display Title information on screen
	!*******************************************************************

 ShowTriControl:

	!
	! Repaint screen
	!
	JUNK% = GL_MAIN_TRIJOUR(SMG_WINDOW, OPT_ENTRY, SETLOOP%, 1%, "") &
		FOR SETLOOP% = 1% TO SMG_WINDOW::NITEMS

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
		LEFT(GL_TRIJOUR::DESCRIPTION(0%), 12%), 2%, 6%) &
		IF (SMG_WINDOW::HFLAG(3%) AND 2%) = 0%

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
		LEFT(GL_TRIJOUR::DESCRIPTION(1%), 12%), 3%, 6%) &
		IF (SMG_WINDOW::HFLAG(4%) AND 2%) = 0%

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
		LEFT(GL_TRIJOUR::DESCRIPTION(2%), 12%), 3%, 45%) &
		IF (SMG_WINDOW::HFLAG(5%) AND 2%) = 0%

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
		LEFT(GL_TRIJOUR::DESCRIPTION(13%), 12%), 16%, 6%) &
		IF (SMG_WINDOW::HFLAG(33%) AND 2%) = 0%

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
		LEFT(GL_TRIJOUR::DESCRIPTION(14%), 12%), 17%, 6%) &
		IF (SMG_WINDOW::HFLAG(34%) AND 2%) = 0%

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
		LEFT(GL_TRIJOUR::DESCRIPTION(15%), 12%), 18%, 6%) &
		IF (SMG_WINDOW::HFLAG(35%) AND 2%) = 0%

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
		LEFT(GL_TRIJOUR::DESCRIPTION(16%), 12%), 16%, 45%) &
		IF (SMG_WINDOW::HFLAG(36%) AND 2%) = 0%

	RETURN

29000	!***************************************************************
	! Trap errors
	!***************************************************************

	ON ERROR GO BACK

30000	END FUNCTION
