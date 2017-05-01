1	%TITLE "File Reconciliation Maintenance"
	%SBTTL "CK_MAIN_CKMNT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG CK_MAIN_CKMNT(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1987,1988 BY
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
	!	The ^*Maintain Reconciliation File\* option creates
	!	records for the outstanding checks and deposits in transit for each
	!	bank account relative to the accounting period previous to the period in
	!	which the system is installed.
	!	.note
	!	^*This maintenance option is not intended for
	!	data entry on a regular basis even though the
	!	ability to do so exists.\*
	!	.en
	!
	! Index:
	!	.x Maintenance>Reconciliation File
	!	.x Check Reconciliation File>Maintenance
	!	.x Initialize>Outstanding Checks
	!	.x Initialize>Deposits in Transit
	!	.x Outstanding Checks>Initialize
	!	.x Deposits in Transit>Initialize
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS CK_SOURCE:CK_MAIN_CKMNT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP CK_MAIN_CKMNT
	!	$ DELETE CK_MAIN_CKMNT.OBJ;*
	!
	! Author:
	!
	!	03/24/88 - Lance Williams
	!
	! Modification history:
	!
	!	04/28/88 - Kevin Handy
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/17/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/01/97 - Kevin Handy
	!		Clean up source code.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/08/99 - Kevin Handy
	!		Lose lines 760, 770 (Dead Code)
	!
	!	10/20/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[CK.OPEN]CK_CKMNT.HB"
	MAP (CK_CKMNT)		CK_CKMNT_CDD	CK_CKMNT
	MAP (CK_CKMNT_OLD)	CK_CKMNT_CDD	CK_CKMNT_OLD, CK_CKMNT2

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_CK_MAIN_CKMNT) &
		CK_CKMNT.CH%, &
		CK_CKMNT.READONLY%

	COM (TT_CK_CKMNT) &
		CLOSETITLE$ = 20%, &
		CLOSETYPE$(2%) = 20%, &
		CLOSETITLE1$ = 20%, &
		CLOSETYPE2$(3%) = 20%

	ON ERROR GOTO 29000

	%PAGE

	SELECT MOPTION
	!******************************************************************
	! Initialization
	!
	! This option is used to initialize the SMG_WINDOW structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!******************************************************************
	CASE OPT_INIT

		!******************************************************************
		! Set up information
		!******************************************************************

		!
		! Define SMG_WINDOW
		!
		SMG_WINDOW::DESCR = "Reconciliation File Maintenance Structure"
		SMG_WINDOW::NHELP = "CK_MAIN_CKMNT"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 7%

		SMG_WINDOW::NKEYS = 1%
			SMG_WINDOW::KNAME(0%) = "Maintenance_name"
			SMG_WINDOW::KFIELD(0%, 0%) = 3%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
			SMG_WINDOW::KFIELD(0%, 3%) = 3%

		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%

		CLOSETITLE$ = "Type   Description"
		CLOSETYPE$(0%) = "2"
		CLOSETYPE$(1%) = "G    General Ledger"
		CLOSETYPE$(2%) = "B    Bank"

		CLOSETITLE1$ = "Type   Description"
		CLOSETYPE2$(0%) = "3"
		CLOSETYPE2$(1%) = "A    Adjustment"
		CLOSETYPE2$(2%) = "C    Check"
		CLOSETYPE2$(3%) = "D    Deposit"


		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF CK_CKMNT.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF CK_CKMNT.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
		%INCLUDE "SOURCE:[CK.OPEN]CK_CKMNT.CRE"
		CK_CKMNT.READONLY% = 0%
		GOTO 790

 !760
		!
		! If unable to open for modify, try to open
		! with read access only.
		!
 !		%INCLUDE "SOURCE:[CK.OPEN]CK_CKMNT.OPN"
 !		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
 !		CK_CKMNT.READONLY% = -1%
 !
 !		GOTO 790
 !
 !770
		!
		! File not able to open, so reset channel
		!
 !		CALL ASSG_5FREECHANNEL(CK_CKMNT.CH%)
 !
 !		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = CK_CKMNT.CH%
		WHEN ERROR IN
			RESET #CK_CKMNT.CH%
			GET #CK_CKMNT.CH%, REGARDLESS

		USE
			CONTINUE 32767
		END WHEN

	!*******************************************************************
	! Display the background
	!
	! This option is used to display the background information on the
	! screen.  It must first clear any junk on the screen, and then
	! write the background onto it.
	!*******************************************************************
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)


		DATA	5,10, "(01) Bank Code", &
			6,10, "(02) C/D/A Number", &
			7,10, "(03) Type Flag", &
			8,10, "(04) GL or Bank", &
			9,10, "(05) Date", &
			10,10, "(06) Amount", &
			11,10, "(07) GL Period", &
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

	!********************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
	!********************************************************************
20200	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")
 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Bank Code\*
	!	.b
	!	.lm +5
	!	The ^*Bank Code\* field enters a bank code as
	!	defined in the Control file.
	!	.b
	!	Pressing ^*<List Choices>\*, while the cursor is located at this field,
	!	will cause a list of valid codes to be displayed.
	!	.lm -5
	!
	! Index:
	!	.x Bank Code>Reconciliation File Maintenance Structure
	!	.x Reconciliation File Maintenance Structure>Bank Code
	!
	!--

			CK_CKMNT::BANK_ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "5;30", TEMP$, &
				CK_CKMNT::BANK_ACCT, MFLAG, "'E", MVALUE)


		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) C/D/A Number\*
	!	.b
	!	.lm +5
	!	The ^*C/D/A Number\* field enters the number of an
	!	outstanding check, deposit in transit, or adjustment to which a record
	!	has reference.
	!	.b
	!	The field will accommodate an entry of six (6) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x C/D/A Number>Reconciliation File Maintenance Structure
	!	.x Reconciliation File Maintenance Structure>C/D/A Number
	!
	!--


			CK_CKMNT::CKNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;30", TEMP$, &
				CK_CKMNT::CKNUM, MFLAG OR 2%, "'E", MVALUE)


		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Type Flag\*
	!	.b
	!	.lm +5
	!	The ^*Type Flag\* field indicates the type of transaction
	!	to which the entry has reference. Valid flags are:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	^*C\* = Check
	!	.le
	!	^*D\* = Deposit
	!	.le
	!	^*A\* = Adjustment
	!	.els
	!	.lm -5
	!	.b
	!	An entry is required in this field. Pressing ^*<List Choices>\*,
	!	while the cursor is located at this field, will provide a list of
	!	valid type flags.
	!	.lm -5
	!
	! Index:
	!	.x Type Flag>Reconciliation File Maintenance Structure
	!	.x Reconciliation File Maintenance Structure>Type Flag
	!
	!--

			CK_CKMNT::ETYPE = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;30", TEMP$, &
				CK_CKMNT::ETYPE, MFLAG, "'", &
				MVALUE, &
				CLOSETYPE2$(), CLOSETITLE1$, "005"), -1%)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) GL or Bank\*
	!	.b
	!	.lm +5
	!	The ^*GL or Bank\* field indicates whether a record
	!	refers to a record as recorded in the General Ledger or a record reflected
	!	in the bank statement.
	!	.lm -5
	!
	! Index:
	!	.x GL>Reconciliation File Maintenance
	!	.x Bank>Reconciliation File Maintenance
	!	.x Reconciliation File Maintenance>GL
	!	.x Reconciliation File Maintenance>Bank
	!
	!--

			CK_CKMNT::STYPE = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, "8;30", TEMP$, &
				CK_CKMNT::STYPE, MFLAG, "'", &
				MVALUE, &
				CLOSETYPE$(), CLOSETITLE$, "005"), -1%)

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Date\*
	!	.b
	!	.lm +5
	!	The ^*Date\* field indicates the date of a check, deposit, or adjustment
	!	transaction.
	!	.b
	!	The format for this field is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Date>Reconciliation File Maintenance Structure
	!	.x Reconciliation File Maintenance Structure>Date
	!
	!--

			CK_CKMNT::CKDAT = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "9;30", TEMP$, &
				CK_CKMNT::CKDAT, MFLAG, "'E", &
				MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Amount\*
	!	.b
	!	.lm +5
	!	The ^*Amount\* field enters the dollar amount of a
	!	check, deposit, or adjustment.
	!	.b
	!	The field will accept an amount as large as a plus or minus $99,999,999.99.
	!	.lm -5
	!
	! Index:
	!	.x Amount>Reconciliation File Maintenance Structure
	!	.x Reconciliation File Maintenance Structure>Amount
	!
	!--

			CK_CKMNT::CKAMT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "10;30", TEMP$, &
				CK_CKMNT::CKAMT, MFLAG, "##,###,###.##", &
				MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	^*(07) GL Period\*
	!	.b
	!	.lm +5
	!	The ^*GL Period\* field enters the
	!	General Ledger accounting period to which a record has
	!	reference.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!	.x GL Period>Reconciliation File Maintenance Structure
	!	.x Reconciliation File Maintenance Structure>GL Period
	!
	!--

			CK_CKMNT::GLDATE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "11;30", TEMP$, &
				CK_CKMNT::GLDATE, MFLAG, "'E", &
				MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$
	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		CK_MAIN_CKMNT = 0%

		SELECT MLOOP

		CASE 1%
			IF CK_CKMNT::BANK_ACCT = ""
			THEN
				CK_MAIN_CKMNT = 1%
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
	CASE OPT_RESETDEFAULT
		CK_CKMNT = CK_CKMNT2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  BankAct  C/D/A# Flag G/B  " + &
				"  Date           Amount GLDate"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "011,018,023,027,039,052"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = CK_CKMNT::BANK_ACCT + "   " + &
				CK_CKMNT::CKNUM + " " + &
				CK_CKMNT::ETYPE + "    " + &
				CK_CKMNT::STYPE + "    " + &
				PRNT_DATE(CK_CKMNT::CKDAT, 8%) + "" + &
				FORMAT$(CK_CKMNT::CKAMT, &
					"##,###,###.##") + " " + &
				CK_CKMNT::GLDATE
		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE (CK_CKMNT::BANK_ACCT + &
					CK_CKMNT::CKNUM + &
					CK_CKMNT::ETYPE), &
				REGARDLESS
		END SELECT

	END SELECT

	EXIT FUNCTION

29000	!********************************************************************
	! Trap errors
	!********************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
