1	%TITLE "Manually Cancel Cleared Items"
	%SBTTL "CK_MAIN_MANUAL_CANCEL"
	%IDENT "V3.6a Calico"

	FUNCTION LONG CK_MAIN_MANUAL_CANCEL(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	The ^*Manually Cancel Cleared Items\* option enters
	!	or cancel the items (checks, deposits or any adjustments) which have cleared
	!	a bank account.
	!	.lm -5
	!
	! Index:
	!	.x Enter>Cleared Checks
	!	.x Enter>Deposits Credited
	!	.x Cancel>Items Cleared by Bank
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS CK_SOURCE:CK_MAIN_MANUAL_CANCEL/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP CK_MAIN_MANUAL_CANCEL
	!	$ DELETE CK_MAIN_MANUAL_CANCEL.OBJ;*
	!
	! Author:
	!
	!	07/22/87 - Kevin Handy
	!
	! Modification history:
	!
	!	05/05/88 - Aaron Redd
	!		Modified to allow R/O open of file if R/W open fails.
	!
	!	07/15/91 - Kevin Handy
	!		Removed second line 29000.
	!		Removed error trapping for 27110 and 27120 which
	!		don't exist.
	!
	!	03/02/93 - Dan Perkins
	!		Changed "V0" to "VX" on chart of accounts to be
	!		able to list accounts starting at a particular
	!		account.
	!
	!	04/12/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Include GL_WINDOW.INC file.
	!
	!	10/17/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/01/97 - Kevin Handy
	!		Clean up source code.
	!		Use integer for #key
	!
	!	06/12/98 - Kevin Handy
	!		Fix bug with pressing list-choises on the
	!		bank account field.
	!		Don't need GL_WINDOW.INC.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/20/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	05/08/2001 - Kevin Handy
	!		Use FUNC_INCREMENT instead of SUM$(, "1")
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
	%INCLUDE "FUNC_INCLUDE:CK_WINDOW.INC"

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[CK.OPEN]CK_CKMNT.HB"
	MAP (CK_CKMNT)		CK_CKMNT_CDD	CK_CKMNT
	MAP (CK_CKMNT_OLD)	CK_CKMNT_CDD	CK_CKMNT_OLD, CK_CKMNT2, &
		CK_CKMNT3

	%INCLUDE "SOURCE:[CK.OPEN]CK_CONTROLACC.HB"
	MAP (CK_CONTROLACC)	CK_CONTROLACC_CDD	CK_CONTROLACC

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
		REAL	AMOUNT		! Amount for record
		STRING	BANK_ACCT = 6%	! Bank account number
		STRING	CKNUM = 6%	! Check number
		STRING	CKDAT = 8%	! Check date
	END RECORD

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (TT_CK_CKMNT) RARRAY_RECORD RARRAY(3000%)

	COM (TT_CK_SPEC_CANCELL) &
		ETYPE$ = 1%, &
		BANK_ACCT$ = 6%, &
		CKDAT$ = 8%, &
		CKNUM$ = 6%

	COM (CH_CK_CKMNT) &
		CK_CKMNT.CH%, &
		CK_CKMNT.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_WINDOW

	!
	! Declare Variables
	!
	DECLARE RFA CK_CKMNT_RFA

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!*********************************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!
	!*********************************************************************
	CASE OPT_INIT
		!
		! Define window
		!
		SELECT ETYPE$
		CASE "C"
			SMG_WINDOW::DESCR = "Enter Cancelled Checks"
		CASE "D"
			SMG_WINDOW::DESCR = "Enter Cancelled Deposits"
		CASE "A"
			SMG_WINDOW::DESCR = "Enter Cancelled Adjustments"
		END SELECT
		SMG_WINDOW::NHELP = "CK_MAIN_MANUAL_CANCEL"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 4%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 3%
		SMG_WINDOW::BOTLIN = 17%

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

		BANK_ACCT$ = ""
		CKDAT$ = ""
		CKNUM$ = ""

700		!
		! Declare channels
		!
		IF CK_CKMNT.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF CK_CKMNT.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[CK.OPEN]CK_CKMNT.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			CK_MAIN_MANUAL_CANCEL = ERR
			CONTINUE 770
		END WHEN

		CK_CKMNT.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[CK.OPEN]CK_CKMNT.OPN"
		USE
			CK_MAIN_MANUAL_CANCEL = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		CK_CKMNT.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(CK_CKMNT.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = CK_CKMNT.CH%
		WHEN ERROR IN
			RESET #CK_CKMNT.CH%
			GET #CK_CKMNT.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN


	!*********************************************************************
	!
	! Display the background
	!
	! This option is used to display the background information on the
	! screen.  It must first clear any junk on the screen, and then
	! write the background onto it.
	!
	!*********************************************************************
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"   (01)      (02)       (03)              (04)" + &
			SPACE$(32%), 1%, 1%, , SMG$M_REVERSE)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"Bank Code    Date    Ck/Dep#/Adj         Amount" + &
			SPACE$(32%), 2%, 1%, , SMG$M_REVERSE)

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

		FOR I% = 1% TO SMG_WINDOW::TOTREC

			AMOUNT = AMOUNT + RARRAY(I%)::AMOUNT

		NEXT I%

		!
		! Display totals
		!
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"Number of lines" + &
			FORMAT$(SMG_WINDOW::TOTREC, "###") + &
			"             " + &
			FORMAT$(AMOUNT, "#,###,###,###.##") + SPACE$(32%), &
			SMG_WINDOW::VSIZE, 1%, , SMG$M_REVERSE)

		!
		! Paint lines on screen
		!
		FOR I% = 1% TO 3%

			A% = VAL%(MID("010,021,033", I% * 4% - 3%, 3%))

			SMG_STATUS% = SMG$DRAW_LINE(SMG_WINDOW::WNUMBER, &
				1%, A%, SMG_WINDOW::BOTLIN, A%)

		NEXT I%

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!*********************************************************************
	!
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
	!
	!*********************************************************************
20200	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		XLINE$ = NUM1$(SMG_WINDOW::CURLIN)

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Bank Code\*
	!	.b
	!	.lm +5
	!	The ^*Bank Code\* field enters the bank account code
	!	(as defined in the Check Reconciliation utility file) to which the checks
	!	or deposits to be entered have reference.
	!	.b
	!	The field will accommodate up to a six (6) character alphanumeric code.
	!	.b
	!	Pressing ^*<List Choices>\*, while the cursor is located at this field,
	!	will cause a list of valid Bank Codes to be displayed.
	!	.b
	!	Since a series of outstanding checks would most likely be entered using the
	!	same bank code, it is suggested that the default function in the Command
	!	Menu could be used for this field. Entering the data would be facilitated by
	!	setting up a hard default with the appropriate bank code. The hard default
	!	value would need to be changed before entering data relating to a different
	!	bank account. However, if a hard default is not established the field will
	!	soft default to the value of the previous entry. That is, by pressing
	!	^*<Ent>\*, the value in the previous line item will be duplicated.
	!
	! Index:
	!	.x Bank Account Code
	!
	!--
			CK_CKMNT::BANK_ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";3", TEMP$, &
				CK_CKMNT::BANK_ACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(CK_MAIN_CONTROLACC.ID, "VX") = 1%)
				THEN
					CK_CKMNT::BANK_ACCT = &
						CK_CONTROLACC::BANK_ACCT
				END IF
				GOTO E0Loop
			END IF

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Date\*
	!	.b
	!	.lm +5
	!	The ^*Date\* field enters a date related to a check
	!	cleared, deposit credited or bank adjustment recorded. To simplify data
	!	entry, it is suggested that the date of the bank statement or the date of the
	!	last day of the accounting period be used.
	!	.b
	!	If either of the above suggested dates are used, data entry would be
	!	facilitated by using the default function in the Command Menu and entering
	!	a hard default value in this field. However, by pressing ^*<Ent>\*, the
	!	value in the previous line item will be repeated.
	!	.b
	!	The format for entering the date is MMDDYYYY, MMDDYY or MMDD.
	!	.lm -5
	!
	! Index:
	!	.x Date>Manually Enter Cancelled Checks
	!	.x Manually Enter Cancelled Checks>Date
	!
	!--
			CK_CKMNT::CKDAT = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";11", TEMP$, &
				CK_CKMNT::CKDAT, MFLAG, &
				"'E", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Ck/Dep/Adj\*
	!	.b
	!	.lm +5
	!	The ^*Check/Deposit/Adjustment\* field enters a check
	!	number, deposit number, or adjustment number which has cleared or been
	!	credited by or charged by the bank. Each time a line item is entered, this
	!	field will increment by one. The value in the field may be changed by
	!	entering the correct number or the value may be accepted by pressing
	!	^*<Enter>\*, ^*<Return>\* or ^*<Do>\*.
	!	.b
	!	The field will accommodate up to six (6) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Ck/Dep/Adj>Manually Enter Cancelled Checks
	!	.x Manually Enter Cancelled Checks>Ck/Dep/Adj
	!
	!--
			CK_CKMNT::CKNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";25", TEMP$, &
				CK_CKMNT::CKNUM, MFLAG OR 2%, "'E", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Amount\*
	!	.b
	!	.lm +5
	!	The ^*Amount\* field will automatically display the amount of the item
	!	represented in field (3) as recorded in the General Ledger. The amount
	!	displayed can be accepted by pressing ^*<Enter>\*, ^*<Return>\* or ^*<Do>\*.
	!	If the amount displayed does not agree with the data as recorded by the
	!	bank, the amount recorded by the bank should be entered.
	!	.b
	!	The field will accommodate an amount as large as 99,999,999.99.
	!	.lm -5
	!
	! Index:
	!	.x Amount>Manually Clear Cancelled Checks
	!	.x Manually Clear Cancelled Checks>Amount
	!
	!--
			IF EDIT$(TEMP$, -1%) = "ADD" AND (MFLAG AND 1%) = 0%
			THEN
				GOSUB AmountLookup
			END IF

			CK_CKMNT::CKAMT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";34", TEMP$, &
				CK_CKMNT::CKAMT, MFLAG, "###,###,###.##", &
				MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		CK_MAIN_MANUAL_CANCEL = 0%

		SELECT MLOOP

		CASE 1%
			IF (CK_CKMNT::BANK_ACCT = "")
			THEN
				CK_MAIN_MANUAL_CANCEL = 1%
			END IF

		CASE 2%
			IF (CK_CKMNT::CKDAT = "")
			THEN
				CK_MAIN_MANUAL_CANCEL = 1%
			END IF

		CASE 3%
			IF (CK_CKMNT::CKNUM = "")
			THEN
				CK_MAIN_MANUAL_CANCEL = 1%
			END IF

		END SELECT

	!
	! Set CK_CKMNT_OLD value
	!
20500	CASE OPT_SETOLD
		CK_CKMNT_OLD = CK_CKMNT

	!
	! Restore CK_CKMNT_OLD value
	!
	CASE OPT_RESETOLD
		CK_CKMNT = CK_CKMNT_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		CK_CKMNT2 = CK_CKMNT

	!
	! Restore default value
	!
20600	CASE OPT_RESETDEFAULT
		!
		! Remember previous values
		!
		IF (SMG_WINDOW::TOTREC <> 0%)
		THEN
			BANK_ACCT$ = RARRAY(SMG_WINDOW::TOTREC)::BANK_ACCT
			CKNUM$ = RARRAY(SMG_WINDOW::TOTREC)::CKNUM
			CKDAT$ = RARRAY(SMG_WINDOW::TOTREC)::CKDAT
		END IF

		!
		! Pull in defaults
		!
		CK_CKMNT = CK_CKMNT2

		!
		! Some informatione must be hardcoded
		!
		CK_CKMNT::ETYPE = ETYPE$
		CK_CKMNT::STYPE = "B"
		CK_CKMNT::GLDATE = ""

		!
		! Some other values set if they start out blank.
		! NOTE: The check number set must be the last thing done
		! in this section in case an error occurs in the
		! processing.
		!
		CK_CKMNT::BANK_ACCT = BANK_ACCT$ &
			IF (CK_CKMNT::BANK_ACCT = "")
		CK_CKMNT::CKDAT = CKDAT$ &
			IF (CK_CKMNT::CKDAT = "")
 !		RSET CK_CKMNT::CKNUM = SUM$(CKNUM$, "1")
		RSET CK_CKMNT::CKNUM = CKNUM$
		JUNK% = FUNC_INCREMENT(CK_CKMNT::CKNUM)

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			WHEN ERROR IN
				FIND #CK_CKMNT.CH%, &
					KEY #0% GE CK_CKMNT::BANK_ACCT + &
					CK_CKMNT::CKNUM + &
					CK_CKMNT::ETYPE, &
					REGARDLESS
			USE
				CK_CKMNT::CKNUM = ""
				CONTINUE 28000
			END WHEN

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
		! Load array with line items.
		!
		! NOTE: This program does not pull up any existing records
		! from the file.  It only works within the current batch
		! being entered.
		!
		CASE 1%

			!
			! Empty array
			!
			SMG_WINDOW::TOTREC = 0%

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
			RARRAY(MFLAG)::LINRFA = GETRFA(SMG_WINDOW::CHAN)
			RARRAY(MFLAG)::AMOUNT = CK_CKMNT::CKAMT
			RARRAY(MFLAG)::BANK_ACCT = CK_CKMNT::BANK_ACCT
			RARRAY(MFLAG)::CKNUM = CK_CKMNT::CKNUM
			RARRAY(MFLAG)::CKDAT = CK_CKMNT::CKDAT

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

		END SELECT
	END SELECT

28000	EXIT FUNCTION

	%PAGE

 AmountLookup:
28200	!*****************************************************************
	! Look up amount of the check and return that as a default value
	!*****************************************************************
	CK_CKMNT3 = CK_CKMNT

	TEST$ = CK_CKMNT::BANK_ACCT + CK_CKMNT::CKNUM
	TEST_ETYPE$ = CK_CKMNT::ETYPE

	CKAMT = 0.0
	TEST_CHECK$ = TRM$(CK_CKMNT::CKNUM)

	CK_CKMNT_RFA = GETRFA(CK_CKMNT.CH%)

	WHEN ERROR IN
		FIND #CK_CKMNT.CH%, &
			KEY #0% GE CK_CKMNT::BANK_ACCT + &
			CK_CKMNT::CKNUM + &
			CK_CKMNT::ETYPE, &
			REGARDLESS
	USE
		CONTINUE 28280
	END WHEN

28220	WHEN ERROR IN
		GET #CK_CKMNT.CH%, REGARDLESS
	USE
		CONTINUE 28280
	END WHEN

	GOTO 28280 IF CK_CKMNT::BANK_ACCT + CK_CKMNT::CKNUM <> TEST$

	IF CK_CKMNT::ETYPE = TEST_ETYPE$
	THEN
		IF CK_CKMNT::STYPE <> "B"
		THEN
			CKAMT = CKAMT + CK_CKMNT::CKAMT
		ELSE
			CKAMT = CKAMT - CK_CKMNT::CKAMT
		END IF
	END IF

	!
	! Check to see this this check has already been cleared
	!
	IF CK_CKMNT::STYPE = "B"
	THEN
		IF CANCEL_TEST% = 0%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, TEST_CHECK$ + &
				" has already been cleared", 1%)
			CANCEL_TEST% = -1%
		END IF
	END IF

	GOTO 28220

28280	WHEN ERROR IN
		GET #CK_CKMNT.CH%, RFA CK_CKMNT_RFA, REGARDLESS
	USE
		CONTINUE 28290
	END WHEN

28290	CK_CKMNT = CK_CKMNT3

	CK_CKMNT::CKAMT = CKAMT

	RETURN

	%Page

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
