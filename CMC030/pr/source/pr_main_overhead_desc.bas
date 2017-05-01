1	%TITLE "PR Overhead Description Maintenance"
	%SBTTL "PR_MAIN_OVERHEAD_DESC"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_OVERHEAD_DESC(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	!	The ^*Maintain Overhead Table\* program maintains the Payroll Overhead
	!	Description file.
	!	.lm -5
	!
	! Index:
	!	.x Maintain Overhead Table
	!	.x Tables>Maintain Overhead
	!
	! Option:
	!
	!	PR_MAIN_OVERHEAD_DESC$DEFINITION
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_OVERHEAD_DESC/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PR_MAIN_OVERHEAD_DESC
	!	$ DELETE PR_MAIN_OVERHEAD_DESC.OBJ;*
	!
	! Author:
	!
	!	12/04/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	06/01/88 - Aaron Redd
	!		Modified to allow R/O open of file if R/W open fails.
	!
	!	07/14/91 - Kevin Handy
	!		Changed line 20050 to 20150 to keep it in the
	!		correct order.
	!
	!	04/22/02 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/29/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/22/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/29/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	11/20/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PR.OPEN]PR_OVERHEAD_DESC.HB"
	MAP	(PR_OVERHEAD_DESC)	PR_OVERHEAD_DESC_CDD	PR_OVERHEAD_DESC
	MAP (PR_OVERHEAD_DESC2) PR_OVERHEAD_DESC_CDD PR_OVERHEAD_DESC_OLD, &
		PR_OVERHEAD_DESC2

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)		GL_CHART_CDD		GL_CHART

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PR_OVERHEAD_DESC) &
		PR_OVERHEAD_DESC.CH%, &
		PR_OVERHEAD_DESC.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION MAIN_JOURNAL
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY

	!
	! Declare data types
	!
	DECLARE LONG XPOS, YPOS
	DECLARE RFA TEMP_RFA

	%PAGE

	ON ERROR GOTO 29000

	ECTITLE$ = "Type Description"
	EC$(0%) = "2"
	EC$(1%) = "1    Per Hours"
	EC$(2%) = "2    % of Wages"

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
		SMG_WINDOW::DESCR = "Overhead Description Maintenance"
		SMG_WINDOW::NHELP = "PR_MAIN_OVERHEAD_DESC"
		SMG_WINDOW::CHAN  = PR_OVERHEAD_DESC.CH%
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 130%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 7%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Overhead_key"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PR_OVERHEAD_DESC.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_OVERHEAD_DESC.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_OVERHEAD_DESC.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PR_MAIN_OVERHEAD_DESC = ERR
			CONTINUE 770
		END WHEN

		PR_OVERHEAD_DESC.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_OVERHEAD_DESC.OPN"
		USE
			PR_MAIN_OVERHEAD_DESC = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_OVERHEAD_DESC.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PR_OVERHEAD_DESC.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PR_OVERHEAD_DESC.CH%
		WHEN ERROR IN
			RESET #PR_OVERHEAD_DESC.CH%
			GET #PR_OVERHEAD_DESC.CH%, REGARDLESS
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

		DATA	2,  5, "(01) OH Key", &
			2, 30, "(02) Description", &
			4,  5, "(03) Rate", &
			5,  5, "(04) Basis", &
			6,  5, "(05) Premium  Ex Acct (Dr)", &
			7,  5, "(06) OH Applied Acct (Cr)", &
			8,  5, "(07) OH Expense Acct (Dr)", &
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

	!
	! Optional menu items
	!
	CASE OPT_OPTLIST
		MVALUE = MVALUE + " definiTion"

	!
	! Handle additional options
	!
20150	CASE OPT_MOREMENU

		!
		! Make sure there is a header
		!
		TEMP_RFA = GETRFA(PR_OVERHEAD_DESC.CH%)

		SELECT EDIT$(MVALUE, -1%)

		!
		! Definitions
		!
		CASE "DEFINITION"
			PR_MAIN_OVERHEAD_DESC = &
				MAIN_JOURNAL(PR_MAIN_OVERHEAD_DEF.ID, "")

	!++
	! Abstract:DEFINITION
	!	^*Definition\*
	!	.b
	!	.lm +5
	!	The ^*Definition\* function
	!	accesses the Overhead Definition screen.
	!	.b
	!	The Overhead Definition screen enters
	!	General Ledger accounts and operation codes and/or combinations
	!	thereof which relate to a specific Overhead Key.
	!	.lm -5
	!
	! Index:
	!	.x Definition>Function>Overhead Table
	!	.x Overhead>Table>Function Definition
	!	.x Overhead>Table>Definition Function
	!
	!--
		END SELECT

	!
	! Handle finishing various options specially
	!
	CASE OPT_AFTEROPT

		SELECT SCOPE::PRG_ITEM

		!
		! Add records
		!
		CASE "Add"
			!
			! Add line items also
			!
			PR_MAIN_OVERHEAD_DESC = &
				MAIN_JOURNAL(PR_MAIN_OVERHEAD_DEF.ID, "A")


		!
		! Change records
		!
		CASE "Change", "Blank", "Initialize"
			!
			! Change line items to match new header
			! if the key was changed.
			!
			! The original record must be the one in the
			! MAP for this to be able to work.  The new
			! key is passed through the QUERY$ variable.
			!
			IF PR_OVERHEAD_DESC_OLD::OVH_KEY <> &
				PR_OVERHEAD_DESC::OVH_KEY
			THEN
				TEMP$ = PR_OVERHEAD_DESC::OVH_KEY + ""
				PR_OVERHEAD_DESC= PR_OVERHEAD_DESC_OLD
				PR_MAIN_OVERHEAD_DESC = &
					MAIN_JOURNAL(PR_MAIN_OVERHEAD_DEF.ID, &
					"C" + TEMP$)
			END IF

		!
		! Erase record
		!
		CASE "Erase"
			!
			! Erase any line items under the header
			!
			PR_MAIN_OVERHEAD_DESC = &
				MAIN_JOURNAL(PR_MAIN_OVERHEAD_DEF.ID, "E")

		END SELECT


	!
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
	!
20200	CASE OPT_ENTRY

		TEMP$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View "

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Overhead  Key\*
	!	.b
	!	.lm +5
	!	The ^*Overhead Key\* field
	!	enters a key code of up to six (6) alphanumeric user defined
	!	characters. Duplicates are not allowed.
	!	.lm -5
	!
	! Index:
	!	.x Overhead>Key
	!	.x Key>Overhead
	!
	!--

			PR_OVERHEAD_DESC::OVH_KEY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;17", TEMP$, &
				PR_OVERHEAD_DESC::OVH_KEY, MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field
	!	enters a user defined description of up to thirty (30)
	!	alphanumeric characters which relates to a unique overhead key.
	!	.lm -5
	!
	! Index:
	!	.x Overhead>Key Description
	!	.x Description>Overhead
	!
	!--

			PR_OVERHEAD_DESC::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;47", TEMP$, &
				PR_OVERHEAD_DESC::DESCR, MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Rate\*
	!	.b
	!	.lm +5
	!	The ^*Rate\* field
	!	enters a specific overhead rate related to an overhead key. The
	!	rate can be expressed as a dollar amount or as a percentage. Which
	!	expression applies is determined by the contents in field (04) Basis.
	!	Either expression can be carried to two (2) positions to the right
	!	of the decimal point.
	!	.b
	!	If the value is expressed as a dollar amount, the resulting
	!	overhead calculation is obtained by multiplying the direct labor
	!	hours times the rate.
	!	.b
	!	If the value is expressed as a percentage, the resulting overhead
	!	calculation is obtained by multiplying the direct labor dollar costs
	!	times the rate.
	!	.lm -5
	!
	! Index:
	!	.x Rate>Overhead
	!	.x Overhead>Rate
	!
	!--

			PR_OVERHEAD_DESC::RATE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "4;40", TEMP$, &
				PR_OVERHEAD_DESC::RATE, MFLAG, "#,###.##", &
				MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Basis\*
	!	.b
	!	.lm +5
	!	The ^*Basis\* field
	!	determines whether the overhead to be applied
	!	in reference to a specific overhead key will be calculated on the
	!	basis of ^&x\& number of dollars per direct labor hour or an ^&x\& percent
	!	of the direct labor dollar cost.
	!	.b
	!	Valid codes are:
	!	.LIST "*"
	!	.LE
	!	^*1\* = Dollars per Direct Labor Hour
	!	.LE
	!	^*2\* = Percent of Direct Labor Dollars
	!	.ELS
	!
	! Index:
	!	.x Basis>Overhead Table
	!	.x Overhead>Basis
	!
	!--

			PR_OVERHEAD_DESC::BASIS = &
				EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, "5;32", TEMP$, &
				PR_OVERHEAD_DESC::BASIS, MFLAG, "'", MVALUE, &
				EC$(), ECTITLE$, "005"), -1%)

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Premium Expense Account (Dr)\*
	!	.b
	!	.lm +5
	!	The ^*Premium Expense Account (Dr)\* field
	!	enters a General Ledger Account to
	!	which the cover time premium labor costs, relative to a specific
	!	Overhead Key, will be posted.
	!	.lm -5
	!
	! Index:
	!	.x Premium>Labor Cost>Account
	!	.x Overtime>Expense Account
	!	.x Overhead>Table>Premium Expense Account
	!
	!--

			PR_OVERHEAD_DESC::PREM_ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;32", TEMP$, &
				PR_OVERHEAD_DESC::PREM_ACCT, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX ") = 1%)
				THEN
					PR_OVERHEAD_DESC::PREM_ACCT = &
						GL_CHART::ACCT
					GOTO E0Loop
				END IF
			END IF

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Overhead Applied Account (Cr)\*
	!	.b
	!	.lm +5
	!	The ^*Overhead Applied Account (Cr)\* field
	!	enters a General Ledger Account which
	!	will be credited with the result of overhead calculations relative
	!	to the specific Overhead Key.
	!	.lm -5
	!
	! Index:
	!	.x Overhead>Applied Account
	!	.x Applied Overhead Account
	!	.x Overhead>Table>Overhead Applied Account
	!
	!--

			PR_OVERHEAD_DESC::OVRHD_ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;32", TEMP$, &
				PR_OVERHEAD_DESC::OVRHD_ACCT, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX ") = 1%)
				THEN
					PR_OVERHEAD_DESC::OVRHD_ACCT = &
						GL_CHART::ACCT
					GOTO E0Loop
				END IF
			END IF

		CASE 7%

	!++
	! Abstract:FLD007
	!	^*(07) Overhead Expense Account (Dr)
	!	.b
	!	.lm +5
	!	The ^*Overhead Expense Account (Dr)\* field
	!	enters a General Ledger account or General
	!	Ledger "account mask" which will be charged (debited) with the result
	!	of overhead calculations relative to the specific Overhead Key.
	!	.b
	!	An "account mask" is an account "number" in which question marks
	!	are inserted in some or all positions in the field. If a mask contains
	!	a question mark in all positions, it is equal to the defined account(s)
	!	which it is masking.
	!	.b
	!	If one or more positions in the field contain a character other
	!	than a question mark, it is equal to the defined account(s) which it is
	!	masking only in respect to the positions which contain question marks.
	!	.b
	!	The chart below illustrates the masking concept:
	!	.b
	!	.lm +5
	!	^&Defined Accounts\&####^&Account Mask\&####^&Masked Account\&
	!	.break
	!	####70010-01############5??20-??####50020-01
	!	.break
	!	####70010-02#############"##########50020-02
	!	.break
	!	####75010-01#############"##########55020-01
	!	.break
	!	####75010-03#############"##########55020-03
	!	.break
	!	####86145-03#############"##########56120-03
	!	.break
	!	####86145-04#############"##########56120-04
	!	.lm -5
	!
	! Index:
	!	.x Mask>Overhead Expence Account
	!	.x Overhead>Expense Account
	!	.x Overhead>Table>Expense Account
	!
	!--

			PR_OVERHEAD_DESC::EX_ACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "8;32", TEMP$, &
				PR_OVERHEAD_DESC::EX_ACCT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX ") = 1%)
				THEN
					PR_OVERHEAD_DESC::EX_ACCT = &
						GL_CHART::ACCT
					GOTO E0Loop
				END IF
			END IF

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		PR_MAIN_OVERHEAD_DESC = 0%

		SELECT MLOOP

		CASE 5%, 6%, 7%
			TEMP$ = PR_OVERHEAD_DESC::PREM_ACCT
			TEMP$ = PR_OVERHEAD_DESC::OVRHD_ACCT IF MLOOP = 6%
			TEMP$ = PR_OVERHEAD_DESC::EX_ACCT IF MLOOP = 7%

			IF EDIT$(TEMP$, -1%) <> "" AND &
				INSTR(1%, TEMP$, "?") = 0%
			THEN
				!
				! Is the input defined?
				!
				PR_MAIN_OVERHEAD_DESC = &
					FUNC_TESTENTRY(SMG_WINDOW, &
					TEMP$, &
					GL_CHART::DESCR, &
					"PR", MLOOP, "PRG", &
					"Account", GL_MAIN_CHART.ID)
			END IF

		END SELECT

	!
	! Set PR_OVERHEAD_DESC_OLD value
	!
20500	CASE OPT_SETOLD
		PR_OVERHEAD_DESC_OLD = PR_OVERHEAD_DESC

	!
	! Restore PR_OVERHEAD_DESC_OLD value
	!
	CASE OPT_RESETOLD
		PR_OVERHEAD_DESC = PR_OVERHEAD_DESC_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PR_OVERHEAD_DESC2 = PR_OVERHEAD_DESC

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PR_OVERHEAD_DESC = PR_OVERHEAD_DESC2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = &
				"  Key        Description                  Rate   Basis   " + &
				"Premium Account       Overhead  Account      Expense Account    "
		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "011,036,048,056,079,102"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = PR_OVERHEAD_DESC::OVH_KEY + "     " + &
				LEFT(PR_OVERHEAD_DESC::DESCR, 20%) + "     " + &
				FORMAT$(PR_OVERHEAD_DESC::RATE, "#,###.##") &
				+ "     " + &
				PR_OVERHEAD_DESC::BASIS + "     " + &
				PR_OVERHEAD_DESC::PREM_ACCT + "     " + &
				PR_OVERHEAD_DESC::OVRHD_ACCT + "     " + &
				PR_OVERHEAD_DESC::EX_ACCT

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #PR_OVERHEAD_DESC.CH%, &
				KEY #0% GE PR_OVERHEAD_DESC::OVH_KEY + "", &
				REGARDLESS

		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

29900	!
	! Handle no header for Line function here
	!
	CALL ENTR_3MESSAGE(SCOPE, "Sorry, but there is no current record", 0%)

32767	END FUNCTION
