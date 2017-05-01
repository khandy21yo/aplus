1	%TITLE "GENERAL JOURNAL MAINTENANCE"
	%SBTTL "GL_MAIN_GJ_LINE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG GL_MAIN_GJ_LINE(CDD_WINDOW_CDD SMG_WINDOW, &
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
	! ID:1020
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program maintains the General Ledger Journal.
	!	.LM -5
	!
	! Index:
	!	.x General Ledger Journal
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_MAIN_GJ_LINE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN GL_MAIN_GJ_LINE
	!	$ DELETE GL_MAIN_GJ_LINE.OBJ;*
	!
	! Author:
	!
	!	04/30/87 - Kevin Handy
	!
	! Modification history:
	!
	!	05/10/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	06/23/88 - Aaron Redd
	!		Split into two modules (_MAST_ and _MAIN_) in order
	!		to meet standardization requirements.
	!
	!	03/02/93 - Dan Perkins
	!		Changed "V0" to "VX" on chart of accounts to be
	!		able to list accounts starting at a particular
	!		account.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/18/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/23/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/25/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	03/09/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	11/10/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	08/06/2001 - Kevin Handy
	!		Increase number of digits allowed for entry. (kbj)
	!
	!	08/10/2001 - Kevin Handy
	!		Adjust position of journal total
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG	FUNCTION MAIN_WINDOW

	%PAGE

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD	GL_CHART

	%INCLUDE "SOURCE:[GL.OPEN]GL_GJ_LINE.HB"
	MAP	(GL_GJ_LINE)	GL_GJ_LINE_CDD	GL_GJ_LINE
	MAP	(GL_GJ_LINE2)	GL_GJ_LINE_CDD	GL_GJ_LINE_OLD, &
					GL_GJ_LINE2, GL_GJ_LINE3

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_GL_GJ_LINE) &
		GL_GJ_LINE.CH%, &
		GL_GJ_LINE.READONLY%
	COM (TT_GL_GJ_LINE) &
		JRL_TYPE$ = 1%, &
		GL_GJ_LINE.JOURNAL$ = 20%

	!
	! Declare some variables
	!
	DECLARE	LONG	XPOS, YPOS
	DECLARE	RFA	GL_GJ_LINE_RFA

	%PAGE

	!
	! Set up error trapping
	!
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
		SMG_WINDOW::DESCR = TRM$(GL_GJ_LINE.JOURNAL$) + &
			" Journal Maintenance"
		SMG_WINDOW::NHELP = "GL_MAIN_GJ_LINE"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 14%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Journal"
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF GL_GJ_LINE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF GL_GJ_LINE.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_GJ_LINE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			GL_MAIN_GJ_LINE = ERR
			CONTINUE 770
		END WHEN

		GL_GJ_LINE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_GJ_LINE.OPN"
		USE
			GL_MAIN_GJ_LINE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		GL_GJ_LINE.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(GL_GJ_LINE.CH%)
		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = GL_GJ_LINE.CH%

		WHEN ERROR IN
			RESET #GL_GJ_LINE.CH%
			GET #GL_GJ_LINE.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

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

		DATA	3, 20, "(01) Journal #", &
			4, 20, "(02) Item #", &
			6, 20, "(03) Account #", &
			7, 20, "(04) Source", &
			8, 20, "(05) Description", &
			9, 20, "(06) Tran Date", &
			10, 20, "(07) Amount", &
			11, 20, "(08) Ckno", &
			12, 20, "(09) Xrefno", &
			13, 20, "(10) TranKey", &
			14, 20, "(11) SubAcc", &
			15, 20, "(12) Operation", &
			16, 20, "(13) Units", &
			17, 20, "(14) Hours", &
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

20150	!******************************************************************
	! Add to the option menu
	!******************************************************************
	CASE OPT_OPTLIST
		MVALUE = MVALUE + " Journal_total"

	!******************************************************************
	! Window option
	!******************************************************************
	CASE OPT_MOREMENU

		SELECT MVALUE

		!
		! Journal total
		!
		CASE "Journal_total"
	!++
	!
	! Abstract:JOURNAL_TOTAL
	!	^*Journal Total\*
	!	.b
	!	.lm +5
	!	Use this command menu function to determine if this specific
	!	journal is in balance.  Access this function after the final
	!	journal item has been entered.  If the screen indicates a balance
	!	other than zero, the posting routine
	!	will be aborted.
	!	.lm -5
	!
	! Index:
	!	.x Journal Total>General Journal
	!	.x General Journal>Journal Total
	!
	!--
			GOSUB JournalTotal

		END SELECT

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

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	.x General Journal>Journal Number
	!	^*(01) Journal _#\*
	!	.B
	!	.LM +5
	!	The Journal _# field is used to assign a journal identification
	!	number for all items which will be entered.
	!	.b
	!	The field will accommodate 6 characters.
	!	.LM -5
	!
	! Index:
	!	.x Journal Number>General Journal
	!
	!--
			GL_GJ_LINE::JOURNAL = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "3;38", TEMP$, &
				GL_GJ_LINE::JOURNAL, MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	.x Journal Item>General Journal
	!	^*(02) Item _#\*
	!	.B
	!	.lm +5
	!	This field is automatically system assigned.
	!	.b
	!	^*No entry is required\*.
	!	.lm -5
	!
	! Index:
	!	.x General Journal>Journal Item
	!	.x Item>General Journal
	!	.x General Journal>Item
	!
	!--
			MFLAG = MFLAG OR 1% &
				UNLESS (TEMP$ = "Find") OR (TEMP$ = "VIEW")

			GL_GJ_LINE::ITEMNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"4;38", TEMP$, &
				GL_GJ_LINE::ITEMNUM, MFLAG, &
				"~L0'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	.x Account>General Journal
	!	^*(03) Account _#\*
	!	.b
	!	.lm +5
	!	Enter the general ledger chart of accounts number.  The
	!	account number must be valid.  Pressing List Choices
	!	will display a list of valid numbers.
	!	.b
	!	The field will accommodate 18 characters.
	!	.b
	!	This field requires an entry.
	!	.lm -5
	!
	! Index:
	!	.x General Journal>Account
	!	.x Account>General Journal
	!	.x General Journal>Account
	!
	!--
			GL_GJ_LINE::ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;38", TEMP$, &
				GL_GJ_LINE::ACCT, MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%)
				THEN
					GL_GJ_LINE::ACCT = GL_CHART::ACCT
				END IF
				GOTO E0Loop

			CASE SMG$K_TRM_F17

				V% = MAIN_WINDOW(GL_MAIN_CHART.ID, "M")
				GL_GJ_LINE::ACCT = GL_CHART::ACCT
				GOTO E0Loop

			END SELECT

			IF MAIN_WINDOW(GL_MAIN_CHART.ID, &
				"Q0" + GL_GJ_LINE::ACCT) <> 1%
			THEN
				GL_CHART::DESCR = &
					STRING$(LEN(GL_CHART::DESCR), 63%)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(GL_CHART::DESCR, 20%), &
				6%, 57%, , SMG$M_BOLD)

		CASE 4%

	!++
	! Abstract:FLD004
	!	.x General Journal>Source Code
	!	^*(04) Source\*
	!	.b
	!	.lm +5
	!	The ^*Source\* will identify the origin of an entry.
	!	.b
	!	Example:
	!	.table 5,25
	!	.te
	!	^*CD\* #- Cash Disbursements Journal
	!	.te
	!	^*CRJ\* - Cash Receipts Journal
	!	.end table
	!	The field will accept up to 4 characters.
	!	.lm -5
	!
	! Index:
	!	.x Source Code>General Journal
	!
	!--
			GL_GJ_LINE::SOURCE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;38", TEMP$, &
				GL_GJ_LINE::SOURCE, MFLAG, "'E", MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!	.x General Journal>Description
	!	^*(05) Description\*
	!	.b
	!	.lm +5
	!	The description is provided to enter a brief description for the journal entry.
	!	.b
	!	The field will accept up to 30 characters.
	!	.lm -5
	!
	! Index:
	!	.x Description>General Journal
	!
	!--
			GL_GJ_LINE::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "8;38", TEMP$, &
				GL_GJ_LINE::DESCR, MFLAG, "'E", MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	.FIELD
	!	^*(06) Transaction Date\*
	!	.b
	!	Used to identify the date of the transaction.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.b
	!	Example:
	!	.b
	!	.lm +5
	!	01011992 - will appear as 01/01/92
	!	.b
	!	010192 ##- will also appear as 01/01/92
	!
	! Index:
	!	.x General Journal>Transaction Date
	!	.x Transaction Date>General Journal
	!	.x Date>Transaction>General Journal
	!
	!--
			GL_GJ_LINE::TRANDAT = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "9;38", TEMP$, &
				GL_GJ_LINE::TRANDAT, MFLAG, "'E", MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	.FIELD
	!	^*(07) Amount\*
	!	.b
	!	Enter the dollar amount of the transaction.
	!	.TABLE 3,25
	!	.TE
	!	^*-\*	represents a credit
	!	.TE
	!	^*+\*	represents a debit
	!	.END TABLE
	!	After entering (07) dollar amount, subsequent fields may be
	!	skipped over by pressing PF2.
	!
	! Index:
	!	.x General Journal>Amount
	!	.x Amount>General Journal
	!
	!--
			GL_GJ_LINE::AMOUNT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "10;38", TEMP$, &
				GL_GJ_LINE::AMOUNT, MFLAG, &
				"###,###,###,###.##", MVALUE)

		CASE 8%

	!++
	! Abstract:FLD008
	!	.x General Journal>Check Number
	!	^*(08) Ckno (Check number)\*
	!	.b
	!	.lm +5
	!	If this entry relates to a cash disbursement or cash
	!	receipt, a check number or deposit number is entered here.
	!	.b
	!	The field will accept 6 characters.
	!	.lm -5
	!
	! Index:
	!	.x Check Number>General Journal
	!
	!--
			GL_GJ_LINE::CKNO = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "11;38", TEMP$, &
				GL_GJ_LINE::CKNO, MFLAG, "'E", MVALUE)

		CASE 9%

	!++
	! Abstract:FLD009
	!	.FIELD
	!	^*(09) Xrefno (Cross Reference Number)\*
	!	.b
	!	Used to identify a transaction.
	!	.Example
	!	.EE
	!	Employee _#
	!	.EE
	!	Customer
	!	.EE
	!	Vendor _#
	!	.EE
	!	etc.
	!	.end example
	!	The field will accept 10 characters.
	!	.b
	!	This field may be left blank.
	!	.b
	!	^*Note: Entry in this field will have no effect on the
	!	payroll files, accounts receivable, or accounts payable
	!	subsidiary ledgers.\*
	!
	! Index:
	!	.x Cross Reference>General Journal
	!	.x General Journal>Cross Reference
	!	.x Xref>General Journal
	!	.x General Journal>Xref
	!
	!--
			GL_GJ_LINE::XREFNO = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "12;38", TEMP$, &
				GL_GJ_LINE::XREFNO, MFLAG, "'E", MVALUE)

		CASE 10%

	!++
	! Abstract:FLD010
	!	.x Transaction Key>General Journal
	!	^*(10) TranKey (Transaction Key)\*
	!	.B
	!	.lm +5
	!	A transaction key number is generated when a Purchase Journal
	!	transaction is entered and is used to identify a particular
	!	entry.
	!	.b
	!	The field will accommodate 6 characters.
	!	.b
	!	^*Note: An entry in this field has no effect upon the Accounts Payable
	!	Subsidiary ledgers.\*
	!	.lm -5
	!
	! Index:
	!	.x General Journal>Transaction Key
	!
	!--
			GL_GJ_LINE::TRANKEY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "13;38", TEMP$, &
				GL_GJ_LINE::TRANKEY, MFLAG, "'E", MVALUE)

		CASE 11%

	!++
	! Abstract:FLD011
	!	.x General Journal>Sub Account
	!	^*(11) SubAcc (Sub Account)\*
	!	.B
	!	.lm +5
	!	Used to identify the transaction.
	!	.b
	!	Example:
	!	.table 3,25
	!	.te
	!	^*Work order
	!	.tE
	!	Job number
	!	.tE
	!	Asset _#\*
	!	.end table
	!	The field will accept 10 characters.
	!	.b
	!	This field may be left blank.
	!	.lm -5
	!
	! Index:
	!	.x Sub Account>General Journal
	!	.x Job Number>General Journal
	!	.x General Journal>Job Number
	!	.x Work Order>General Journal
	!	.x General Journal>Work Order
	!	.x Asset Number>General Journal
	!	.x General Journal>Asset Number
	!
	!--
			GL_GJ_LINE::SUBACC = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "14;38", TEMP$, &
				GL_GJ_LINE::SUBACC, MFLAG, "'E", MVALUE)

		CASE 12%

	!++
	! Abstract:FLD012
	!	.x General Journal>Operation Code
	!	^*(12) Operation\*
	!	.B
	!	.lm +5
	!	Enter a code which identifies the work area or operation
	!	description.  This field is used in conjunction with the
	!	sub-account number.
	!	.b
	!	The field will accept 8 characters.
	!	.b
	!	This field may be left blank.
	!	.lm -5
	!
	! Index:
	!	.x Operation Code>General Journal
	!	.x Work Area>General Journal
	!	.x General Journal>Work Area
	!
	!--
			GL_GJ_LINE::OPERATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "15;38", TEMP$, &
				GL_GJ_LINE::OPERATION, MFLAG, "'E", MVALUE)

		CASE 13%

	!++
	! Abstract:FLD013
	!	.FIELD
	!	^*(13) Units\*
	!	.b
	!	The ^*Units\* field is used to record the number of units related to the
	!	transaction.  The system will automatically insert a decimal point
	!	two positions from the right if no decimal is entered.
	!	.TABLE 3,25
	!	.TE
	!	^*-\*	represents a credit
	!	.TE
	!	^*+\*	represents a debit
	!	.END TABLE
	!	This field may be left blank.
	!
	! Index:
	!	.x General Journal>Unit
	!	.x Unit>General Journal
	!
	!--
			GL_GJ_LINE::UNITS = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "16;38", TEMP$, &
				GL_GJ_LINE::UNITS, MFLAG, "###,###.##", MVALUE)

		CASE 14%

	!++
	! Abstract:FLD014
	!	.FIELD
	!	^*(14) Hours\*
	!	.B
	!	Used to enter the number of hours related to the transaction.
	!	.b
	!	The system will automatically insert the decimal point two positions
	!	from the right if no decimal is entered.
	!	.table 3,25
	!	.te
	!	^*-\*	represents a credit
	!	.te
	!	^*+\*	represents a debit
	!	.end table
	!	This field may be left blank.
	!
	! Index:
	!	.x General Journal>Hour
	!	.x Hour>General Journal
	!
	!--
			GL_GJ_LINE::HOURS = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "17;38", TEMP$, &
				GL_GJ_LINE::HOURS, MFLAG, "###,###.##", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

20300	!******************************************************************
	! Test values
	!******************************************************************
	CASE OPT_TESTENTRY
		GL_MAIN_GJ_LINE = 0%

		SELECT MLOOP

		!
		! Don't allow blank Journal numbers
		!
		CASE 1%
			IF GL_GJ_LINE::JOURNAL = ""
			THEN
				GL_MAIN_GJ_LINE = 1%
			END IF

		!
		! Is the chart number defined?
		!
		CASE 3%
			GL_MAIN_GJ_LINE = FUNC_TESTENTRY( SMG_WINDOW, &
				GL_GJ_LINE::ACCT, GL_CHART::DESCR, &
				"GL", MLOOP, "ACCT", &
				"Account number", GL_MAIN_CHART.ID)

		END SELECT

	!******************************************************************
	! Special stuff to do just before function completes
	!******************************************************************
	CASE OPT_TESTOPT

		SELECT SCOPE::PRG_ITEM

		CASE "Add"
			!
			! Assign a line number to this item
			!
			GOSUB 28000

			JUNK$ = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;38", TEMP$, &
				GL_GJ_LINE::ITEMNUM, 1%, &
				"~L0'E", MVALUE)

		END SELECT

20500	!******************************************************************
	! Set GL_GJ_LINE_OLD value
	!******************************************************************
	CASE OPT_SETOLD
		GL_GJ_LINE_OLD = GL_GJ_LINE

	!******************************************************************
	! Restore GL_GJ_LINE_OLD value
	!******************************************************************
	CASE OPT_RESETOLD
		GL_GJ_LINE = GL_GJ_LINE_OLD

	!******************************************************************
	! Set default value
	!******************************************************************
	CASE OPT_SETDEFAULT
		GL_GJ_LINE2 = GL_GJ_LINE

	!******************************************************************
	! Restore default value
	!******************************************************************
	CASE OPT_RESETDEFAULT

		!
		! Do not replace any hard defaults
		!
		GL_GJ_LINE2::JOURNAL = GL_GJ_LINE::JOURNAL &
			UNLESS SMG_WINDOW::HFLAG(1%)
		GL_GJ_LINE2::SOURCE = GL_GJ_LINE::SOURCE &
			UNLESS SMG_WINDOW::HFLAG(4%)
		GL_GJ_LINE2::DESCR = GL_GJ_LINE::DESCR &
			UNLESS SMG_WINDOW::HFLAG(5%)
		GL_GJ_LINE2::TRANDAT = GL_GJ_LINE::TRANDAT &
			UNLESS SMG_WINDOW::HFLAG(6%)
		GL_GJ_LINE = GL_GJ_LINE2

	!******************************************************************
	! View data in the file
	!******************************************************************
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Jrnl # Item Account            " + &
				"Description           Date   " + &
				"          Amount"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "009,014,033,055,066"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = GL_GJ_LINE::JOURNAL + " " + &
				GL_GJ_LINE::ITEMNUM + " " + &
				GL_GJ_LINE::ACCT + " " + &
				LEFT(GL_GJ_LINE::DESCR, 21%) + " " + &
				PRNT_DATE(GL_GJ_LINE::TRANDAT, 8%) + " " + &
				FORMAT$(GL_GJ_LINE::AMOUNT, "#,###,###.##")

		END SELECT

	!******************************************************************
	! Find
	!******************************************************************
	CASE OPT_FIND
		SELECT MLOOP

		!
		! FIND according to the primary key
		!	(Journal number + Item number)
		!
		CASE 0%
			FIND #GL_GJ_LINE.CH%, KEY #0% GE &
				GL_GJ_LINE::JOURNAL + GL_GJ_LINE::ITEMNUM, &
				REGARDLESS

		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

	%PAGE

28000	!******************************************************************
	! Subroutine to find the next item number
	!******************************************************************
	GL_GJ_LINE3 = GL_GJ_LINE
	WORK_ITEMNUM$ = ""
	WORK_JOURNAL$ = GL_GJ_LINE::JOURNAL

	WHEN ERROR IN
		FIND #GL_GJ_LINE.CH%, &
			KEY #0% GE GL_GJ_LINE::JOURNAL + "", REGARDLESS

		GET #GL_GJ_LINE.CH%, REGARDLESS
	USE
		CONTINUE 28030 IF ERR = 155%
		EXIT HANDLER
	END WHEN

28010	ITEM% = ITEM% + 35%
	WHEN ERROR IN
		GET #GL_GJ_LINE.CH%, KEY #0% GE &
			WORK_JOURNAL$ + FORMAT$(ITEM%, "<0>###"), REGARDLESS
	USE
		CONTINUE 28015 IF ERR = 155%
		EXIT HANDLER
	END WHEN

	GOTO 28015 IF WORK_JOURNAL$ <> GL_GJ_LINE::JOURNAL

	GOTO 28010 IF ITEM% < 9900%

28015	ITEM% = ITEM% - 35%
	FIND  #GL_GJ_LINE.CH%, KEY #0% GE &
		WORK_JOURNAL$ + FORMAT$(ITEM%, "<0>###"), REGARDLESS

	GET #GL_GJ_LINE.CH%, REGARDLESS

28020	IF WORK_JOURNAL$ = GL_GJ_LINE::JOURNAL
	THEN
		WORK_ITEMNUM$ = GL_GJ_LINE::ITEMNUM

		WHEN ERROR IN
			GET #GL_GJ_LINE.CH%, REGARDLESS
		USE
			CONTINUE 28030 IF ERR = 11%
			EXIT HANDLER
		END WHEN

		GOTO 28020
	END IF

28030	GL_GJ_LINE = GL_GJ_LINE3
	GL_GJ_LINE::ITEMNUM = FORMAT$(VAL%(WORK_ITEMNUM$) + 1%, "<0>###")

	RETURN

	%PAGE

 JournalTotal:
28100	!******************************************************************
	! Subroutine to recalculate running total
	!******************************************************************
	GL_GJ_LINE3 = GL_GJ_LINE
	RUNNING_TOTAL = 0.0
	WORK_JOURNAL$ = GL_GJ_LINE::JOURNAL

	GL_GJ_LINE_RFA = GETRFA(GL_GJ_LINE.CH%)

	WHEN ERROR IN
		FIND #GL_GJ_LINE.CH%, &
			KEY #0% GE GL_GJ_LINE::JOURNAL + "", REGARDLESS

		GET #GL_GJ_LINE.CH%, REGARDLESS
	USE
		CONTINUE 28120
	END WHEN

28110	IF WORK_JOURNAL$ = GL_GJ_LINE::JOURNAL
	THEN
		RUNNING_TOTAL = RUNNING_TOTAL + GL_GJ_LINE::AMOUNT
		WHEN ERROR IN
			GET #GL_GJ_LINE.CH%, REGARDLESS
		USE
			CONTINUE 28120
		END WHEN

		GOTO 28110
	END IF

28120	WHEN ERROR IN
		GET #GL_GJ_LINE.CH%, RFA GL_GJ_LINE_RFA, REGARDLESS
	USE
		CONTINUE 28130
	END WHEN

28130	GL_GJ_LINE = GL_GJ_LINE3

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
		"Jrnl Total" + &
		FORMAT$(RUNNING_TOTAL, "###########.##"), &
		18%, 53%, , SMG$M_BOLD)

	RETURN

	%PAGE

29000	!******************************************************************
	! Trap errors
	!******************************************************************

	ON ERROR GO BACK

32767	!******************************************************************
	! End of GL_MAIN_GJ_LINE function
	!******************************************************************
	END FUNCTION
