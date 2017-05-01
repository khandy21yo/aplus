1	%TITLE "Cash Distribution Register"
	%SBTTL "CK_RPRT_REG"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1988 BY
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
	! ID:CK001
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Cash Distribution Register\* report includes all transactions relating to
	!	a selected cash account which have been posted to a General Ledger file.
	!	.b
	!	In addition to being a Cash Register, this report includes information
	!	relating to bank deposits and cash account adjustments.
	!	The report is sorted
	!	by reference number, i.e. check _#, deposit _#, and adjustment _#.  Any
	!	missing numbers are noted.
	!	.b
	!	A Cash Distribution Register for any accounting period may be printed at any
	!	time, even though the General Ledger file may be closed.
	!	^*
	!	.note
	!	.b
	!	^*It is imperative that check numbers, deposit numbers, and
	!	adjustment numbers each have a unique sequence or block of numbers.
	!	.en
	!
	! Index:
	!	.x Report>Cash Distribution
	!	.x Cash Distribution>Report
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS CK_SOURCE:CK_RPRT_REG.BAS/LINE
	!	$ LINK/EXE=CK_EXE:*.EXE CK_RPRT_REG, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE CK_RPRT_REG.OBJ;*
	!
	! Author:
	!
	!	04/09/88 - Lance Williams
	!
	! Modification history:
	!
	!	08/08/89 - Kevin Handy
	!		Modified title to show period being printed,
	!		and the bank code.
	!
	!	08/09/89 - Kevin Handy and B. Craig Larsen
	!		Added ability to not print distribution.
	!
	!	08/09/89 - Kevin Handy
	!		Added net amount on right hand side.
	!
	!	08/09/89 - Kevin Handy
	!		Added flag on net to show error if check
	!		amount doesn't match distribution.
	!
	!	11/16/89 - Frank F. Starman
	!		Read distribution from AP distrubution file.
	!		Print summary page by account number.
	!		Report name is now "Cash" instead "Check"
	!		Reconcilation Register.Have option to print
	!		only checks, receipts or adjustments.
	!
	!	04/18/90 - Kevin Handy
	!		Increased the size of the arrays used so that
	!		DWI could get past error.
	!
	!	08/24/90 - J. Shad Rydalch
	!		Added option to sort by Category. Category comes
	!		from gl_chartex.
	!
	!	03/24/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/17/93 - Kevin Handy
	!		Remove commented out code.
	!		Use FUNC_NUMBERADD instead of weird SUM$ stuff.
	!		Added subtotals after each check/deposit number.
	!
	!	08/18/93 - Kevin Handy
	!		Added lots of comments.
	!		Modified code to make it more readable.
	!		Moved "Net" to total line, from 1st line.
	!
	!	08/18/93 - Kevin Handy
	!		Fixed to check numeric value of previous check
	!		against this one to see if numbers are the same.
	!		(Trying to lose excess "Missing Document" errors
	!		on HMI using deposit #'s of 0701IF, 0701BO, ...)
	!
	!	08/18/93 - Kevin Handy
	!		Modified test to determine A,C,D status to match
	!		how the read program works.
	!
	!	08/18/93 - Kevin Handy
	!		Added "SJ" to list of things that are deposits.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	11/10/95 - Kevin Handy
	!		Remove references to CHARTEX, which nobody was
	!		using, and should be replaced by the SB_ACCOUNT
	!		master file anyway.
	!
	!	01/26/96 - Kevin Handy
	!		Lose lots of commented out code.
	!
	!	07/17/96 - Kevin Handy
	!		Reformat source code.
	!
	!	09/03/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	07/24/97 - Kevin Handy
	!		Increase MAXCK from 400 to 800.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/13/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE	UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[CK.OPEN]CK_CONTROLACC.HB"
	MAP	(CK_CONTROLACC)	CK_CONTROLACC_CDD	CK_CONTROLACC

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP	(GL_YYYY_PP)	GL_YYYY_PP_CDD	GL_YYYY_PP

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_DIST.HB"
	MAP	(AP_OPEN_DIST)	AP_OPEN_DIST_CDD AP_OPEN_DIST

	%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE_DIST.HB"
	MAP	(AP_CLOSE_DIST)	AP_CLOSE_DIST_CDD AP_CLOSE_DIST

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	DECLARE	GL_CHART_CDD	GL_CHART_EXAM

	RECORD CK_TEMP_STRUCT
		STRING CHECK = 6%
		STRING TRANKEY = 6%
		STRING SOURCE = 6%
		STRING CDATE = 8%
		STRING CDESCR = 30%
		REAL   AMOUNT
		STRING ACCOUNT = 18%
	END RECORD

	MAP (CK_TEMP) CK_TEMP_STRUCT CK_TEMP

	RECORD GL_TEMP_STRUCT
		STRING ACCOUNT = 18%
		REAL CREDIT
		REAL DEBIT
	END RECORD

	MAP (GL_TEMP) GL_TEMP_STRUCT GL_TEMP

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION GL_EXAM_CHART

	!
	! Declare variables
	!
	DECLARE INTEGER CONSTANT MAXCK = 800
	DIM CKACC$(MAXCK), CKDATE$(MAXCK), DESCR$(MAXCK), &
		ACC$(MAXCK), CKDEPD(MAXCK), &
		CKDEPC(MAXCK), ACCD(MAXCK), ACCC(MAXCK)

	%PAGE

	!
	! Set error trapping
	!
	ON ERROR GOTO 19000

	!
	! Build up an XLATE string for cleaning up numbers
	!
	XLSTR$ = STRING$(256%, 0%)

	X$ = "0123456789"
	X% = A"0"B
	XLSTR$ = LEFT(XLSTR$, X%) + X$ + &
		RIGHT(XLSTR$, X% + LEN(X$) + 1%)

	!
	! Create default from-to
	!
	FROM_ITEM$ = SPACE$(LEN(GL_YYYY_PP::CKNO))
	TO_ITEM$ = SPACE$(LEN(GL_YYYY_PP::CKNO))

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	PERIOD$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) GL Period\*
	!	.b
	!	.lm +5
	!	The ^*GL Period\* field enters the accounting period
	!	for which the report will print.
	!	.b
	!	The value of this field must be a designation for a valid accounting period.
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!	.x GL Period>Cash Distribution Register
	!	.x Cash Distribution Register>GL Period
	!
	!--

	BANK_CODE$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) Bank Code\*
	!	.b
	!	.lm +5
	!	The ^*Bank Code\* field enters the code, as defined in
	!	the Check Reconciliation Control file, which relates to the cash account
	!	which is to be printed.
	!	.lm -5
	!
	! Index:
	!	.x Bank Code>Cash Distribution Register
	!	.x Cash Distribution Register>Bank Code
	!
	!--

	RSET FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) From Check\*
	!	.b
	!	.lm +5
	!	The ^*From Check\* field causes the report to begin with
	!	a selected check or deposit number.
	!	.b
	!	A blank setting will cause the report to begin with the first number in the
	!	file.
	!	.lm -5
	!
	! Index:
	!	.x From Check>Cash Distribution Register
	!	.x Cash Distribution Register>From Check
	!
	!--

	RSET TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) To Check\*
	!	.b
	!	.lm +5
	!	The ^*To Check\* field causes the report to end with
	!	a selected check or deposit number.
	!	.b
	!	A blank setting in this field will cause the report to end with the last
	!	number in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Check>Cash Distribution Register
	!	.x Cash Distribution Register>To Check
	!
	!--

	DIST_YN$ = LEFT(EDIT$(UTL_REPORTX::OPTDEF(4%), -1%), 1%)

	!++
	! Abstract:FLD05
	!	^*(05) Distribution (Y/N)
	!	.b
	!	.lm +5
	!	The ^*Distribution (Y/N)\* field
	!	chooses whether or not a General Ledger
	!	account distribution will be printed on the report.
	!	.b
	!	The account distribution displays the General Ledger number(s) and
	!	related dollar amounts which were recorded when the original purchases
	!	journal records were entered, regardless of the General Ledger accounting
	!	period to which the purchases journal was posted.
	!	.lm -5
	!
	! Index:
	!	.x Distribution>Account
	!
	!--

	ONLY_TYPE$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 132%)
	!++
	! Abstract:FLD06
	!	^*(06) Type (A,C,D,_*)\*
	!	.b
	!	.lm +5
	!	The ^*Type\* field exercises the option to include
	!	all transaction types, i.e. adjustments, checks and deposits in a single
	!	report or to select only one of the three types to be included.
	!	.b
	!	Valid options are:
	!	.table 3,25
	!	.te
	!	^*A\* = Adjustments
	!	.te
	!	^*C\* = Checks
	!	.te
	!	^*D\* = Deposits
	!	.te
	!	^*_*\* = All three types
	!	.end table
	!
	! Index:
	!	.x Type
	!
	!--

	BANK_CODE$ = "*" IF BANK_CODE$ = ""
	ONLY_TYPE$ = "*" IF ONLY_TYPE$ = ""

	YYYY_PP$ = LEFT(PERIOD$, 4%) + "_" + RIGHT(PERIOD$, 5%)


300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_DIST.OPN"
	USE
		CONTINUE 310 IF ERR = 5%
		FILENAME$ = "AP_OPEN_DIST"
		CONTINUE HelpError
	END WHEN

310	!
	! Open dist file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE_DIST.OPN"
	USE
		CONTINUE 320 IF ERR = 5%
		FILENAME$ = "AP_CLOSE_DIST"
		CONTINUE HelpError
	END WHEN

320	!
	! Open Control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[CK.OPEN]CK_CONTROLACC.OPN"
	USE
		FILENAME$ = "CK_CONTROL"
		CONTINUE HelpError
	END WHEN

330	!
	! Open GL Period file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"
	USE
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

350	!
	! Open temporary file
	!
	CALL ENTR_3MESSAGE(SCOPE, "Creating temporary file.", 1% + 16%)

	!
	! Look up device
	!
	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)
	CALL ASSG_CHANNEL(GL_TEMP.CH%, STAT%)

	OPEN UTL_WORK.DEV$ + "GL_TEMP.TMP" FOR OUTPUT &
		AS FILE #GL_TEMP.CH%, ORGANIZATION INDEXED FIXED, &
		MAP GL_TEMP, &
		PRIMARY KEY (GL_TEMP::ACCOUNT), &
		TEMPORARY, &
		BUFFER 32%, &
		ACCESS MODIFY, ALLOW NONE


355	CALL ASSG_CHANNEL(CK_TEMP.CH%, STAT%)

	OPEN UTL_WORK.DEV$ + "CK_TEMP.TMP" FOR OUTPUT &
		AS FILE #CK_TEMP.CH%, ORGANIZATION INDEXED FIXED, &
		MAP CK_TEMP, &
		PRIMARY KEY (CK_TEMP::CHECK, CK_TEMP::TRANKEY) DUPLICATES, &
		TEMPORARY, &
		BUFFER 32%, &
		ACCESS MODIFY, ALLOW NONE

360	!*******************************************************************
	! Create list of cash accounts
	!*******************************************************************

	CASH_ACCT$ = ""

	WHEN ERROR IN
		RESET #CK_CONTROLACC.CH%
	USE
		CONTINUE 400
	END WHEN

365	WHEN ERROR IN
		GET #CK_CONTROLACC.CH%, REGARDLESS
	USE
		CONTINUE 400 IF ERR = 155% OR ERR = 11%
		FILENAME$ = "CK_CONTROLACC"
		CONTINUE HelpError
	END WHEN

	IF COMP_STRING(CK_CONTROLACC::BANK_ACCT, BANK_CODE$)
	THEN
		CASH_ACCT$ = CASH_ACCT$ + CK_CONTROLACC::ACCOUNT + ","
	END IF

	GOTO 365

400	!*******************************************************************
	! Start putting stuff into CK_TEMP file
	!*******************************************************************

	RESET #GL_YYYY_PP.CH%

 GetFrom_GL:
401	WHEN ERROR IN
		GET #GL_YYYY_PP.CH%, REGARDLESS
	USE
		CONTINUE ReportTitle
	END WHEN

	!
	! Handle from-to here
	!
	RSET CK_TEMP::CHECK = EDIT$(GL_YYYY_PP::CKNO, 8% + 128%)

	IF INSTR(1%, CASH_ACCT$, GL_YYYY_PP::ACCT) = 0%
	THEN
		GOTO GetFrom_GL IF GL_YYYY_PP::CKNO = ""
	END IF

	GOTO GetFrom_GL &
		IF (CK_TEMP::CHECK < FROM_ITEM$) OR &
		CK_TEMP::CHECK > TO_ITEM$ AND TO_ITEM$ <> ""

	CK_TEMP::TRANKEY	= GL_YYYY_PP::TRANKEY
	CK_TEMP::SOURCE		= GL_YYYY_PP::SOURCE
	CK_TEMP::CDATE		= GL_YYYY_PP::TRANDAT
	CK_TEMP::CDESCR		= GL_YYYY_PP::DESCR
	CK_TEMP::AMOUNT		= GL_YYYY_PP::AMOUNT
	CK_TEMP::ACCOUNT	= GL_YYYY_PP::ACCT

	PUT #CK_TEMP.CH%

	GOTO GetFrom_GL

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Cash Register"
	TITLE$(2%) = "For the Period " + TRM$(PERIOD$) + &
		", Bank Code " + BANK_CODE$ + &
		" and Type " + ONLY_TYPE$
	TITLE$(3%) = "Check Reconcilation System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	IF DIST_YN$ = "N"
	THEN
		TITLE$(5%) = "Ck/Dp#   Description                    " + &
			"Date             Debit      Credit           Net"
	ELSE
		TITLE$(5%) = "Ck/Dp#   Description                    " + &
			"Date             Debit      Credit   Acct# " + &
			"                   Debit      Credit        Net"
	END IF

	TITLE$(6%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	DR_TOTAL, CR_TOTAL = 0.0

	WHEN ERROR IN
		RESET #CK_TEMP.CH%
		GET #CK_TEMP.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "CK_TEMP"
		CONTINUE HelpError
	END WHEN

17010	CK_NUM$ = CK_TEMP::CHECK
	ACC_COUNT%, CK_COUNT% = 0%
	TEST_TRANKEY$ = STRING$(LEN(CK_TEMP::TRANKEY) + 1%, A"?"B)

	WHILE CK_TEMP::CHECK = CK_NUM$

		SELECT TRM$(CK_TEMP::SOURCE)

		CASE "CD", "PJ", "PR"
			SOURCE_STRING$ = "C"

		CASE "CR", "CRJ", "SJ"
			SOURCE_STRING$ = "D"

		CASE ELSE
			GOSUB HardTest

		END SELECT

		GOTO NextTemp &
			IF COMP_STRING(SOURCE_STRING$, ONLY_TYPE$) = 0%

		IF CK_TEMP::AMOUNT <= 0.0
		THEN
			CREDIT = CK_TEMP::AMOUNT
			DEBIT = 0.0
		ELSE
			CREDIT = 0.0
			DEBIT = CK_TEMP::AMOUNT
		END IF

17040		IF INSTR(1%, CASH_ACCT$, CK_TEMP::ACCOUNT)
		THEN
			CK_COUNT% = CK_COUNT% + 1%
			CKACC$(CK_COUNT%) = CK_TEMP::ACCOUNT
			CKDATE$(CK_COUNT%) = CK_TEMP::CDATE
			DESCR$(CK_COUNT%) = CK_TEMP::CDESCR
			CKDEPC(CK_COUNT%) = CREDIT
			CKDEPD(CK_COUNT%) = DEBIT

			CR_TOTAL = FUNC_ROUND(CR_TOTAL + CREDIT, 2%)
			DR_TOTAL = FUNC_ROUND(DR_TOTAL + DEBIT, 2%)

		ELSE
			!
			! Check, if there is transaction key
			!
			IF SOURCE_STRING$ = "C" AND TRM$(CK_TEMP::TRANKEY) <> ""
			THEN
				GOTO NextTemp &
					IF CK_TEMP::TRANKEY = TEST_TRANKEY$
				TEST_TRANKEY$ = CK_TEMP::TRANKEY
				GOTO 17070
			END IF

			!
			! Probably not AP, we dont need distribution
			!
			ACC_COUNT% = ACC_COUNT% + 1%
			ACC$(ACC_COUNT%) = CK_TEMP::ACCOUNT

			IF ACC_COUNT%>CK_COUNT%
			THEN
				CKDATE$(ACC_COUNT%) = CK_TEMP::CDATE
				DESCR$(ACC_COUNT%) = CK_TEMP::CDESCR
			END IF

			ACCC(ACC_COUNT%) = CREDIT
			ACCD(ACC_COUNT%) = DEBIT
		END IF

		GOTO NextTemp

17070		!
		! Go for detail in distribution
		!
		WHEN ERROR IN
			FIND #AP_OPEN_DIST.CH%, &
				KEY #0% EQ CK_TEMP::TRANKEY, &
				REGARDLESS
		USE
			CONTINUE 17080 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "AP_OPEN_DIST"
			CONTINUE HelpError
		END WHEN

 NextOpen:
		WHEN ERROR IN
			GET #AP_OPEN_DIST.CH%, REGARDLESS
		USE
			CONTINUE 17100 IF ERR = 11%
			FILENAME$ = "AP_OPEN_DIST"
			CONTINUE HelpError
		END WHEN

		GOTO NextTemp IF CK_TEMP::TRANKEY <> AP_OPEN_DIST::TRANKEY

		IF AP_OPEN_DIST::AMOUNT - AP_OPEN_DIST::DISCAMT <= 0.0
		THEN
			CREDIT = AP_OPEN_DIST::AMOUNT - AP_OPEN_DIST::DISCAMT
			DEBIT = 0.0
		ELSE
			CREDIT = 0.0
			DEBIT = AP_OPEN_DIST::AMOUNT - AP_OPEN_DIST::DISCAMT
		END IF

		ACC_COUNT% = ACC_COUNT% + 1%
		ACC$(ACC_COUNT%) = AP_OPEN_DIST::ACCT

		IF ACC_COUNT%>CK_COUNT%
		THEN
			CKDATE$(ACC_COUNT%) = CK_TEMP::CDATE
			DESCR$(ACC_COUNT%) = CK_TEMP::CDESCR
		END IF

		ACCC(ACC_COUNT%) = CREDIT
		ACCD(ACC_COUNT%) = DEBIT

		GOTO NextOpen

17080		WHEN ERROR IN
			FIND #AP_CLOSE_DIST.CH%, &
				KEY #0% EQ CK_TEMP::TRANKEY, &
				REGARDLESS
		USE
			CONTINUE 17100 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "AP_CLOSE_DIST"
			CONTINUE HelpError
		END WHEN

 NextClose:
		WHEN ERROR IN
			GET #AP_CLOSE_DIST.CH%, REGARDLESS
		USE
			CONTINUE 17100 IF ERR = 11% OR ERR = 9%
			FILENAME$ = "AP_CLOSE_DIST"
			CONTINUE HelpError
		END WHEN

		GOTO NextTemp IF CK_TEMP::TRANKEY <> AP_CLOSE_DIST::TRANKEY

		IF AP_CLOSE_DIST::AMOUNT - AP_CLOSE_DIST::DISCAMT <= 0.0
		THEN
			CREDIT = AP_CLOSE_DIST::AMOUNT - AP_CLOSE_DIST::DISCAMT
			DEBIT = 0.0
		ELSE
			CREDIT = 0.0
			DEBIT = AP_CLOSE_DIST::AMOUNT - AP_CLOSE_DIST::DISCAMT
		END IF

		ACC_COUNT% = ACC_COUNT% + 1%
		ACC$(ACC_COUNT%) = AP_CLOSE_DIST::ACCT

		IF ACC_COUNT%>CK_COUNT%
		THEN
			CKDATE$(ACC_COUNT%) = CK_TEMP::CDATE
			DESCR$(ACC_COUNT%) = CK_TEMP::CDESCR
		END IF

		ACCC(ACC_COUNT%) = CREDIT
		ACCD(ACC_COUNT%) = DEBIT

		GOTO NextClose
 NextTemp:
17100		WHEN ERROR IN
			GET #CK_TEMP.CH%, REGARDLESS
		USE
			CONTINUE ExitTotal IF ERR = 11%
			FILENAME$ = "CK_TEMP"
			CONTINUE HelpError
		END WHEN
	NEXT

 GoPrint:
	!
	! If there is not an bank amount to print then
	! skip printing this record
	!
	IF CK_COUNT% OR ACC_COUNT%
	THEN
		GOSUB PrintCheck
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	!
	! Try for next record
	!
	GOTO 17010

 ExitTotal:
17400	!*******************************************************************
	! Handle end of report
	!*******************************************************************

	!
	! Dump out any remaining distribution
	!
	IF CK_COUNT% OR ACC_COUNT%
	THEN
		GOSUB PrintCheck
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	!
	! Don't print totals if no lines has been printed
	!
	GOTO ExitProgram IF PRINT_FLAG% = 0%

	!
	! Now print the totals
	!
	TEXT$ = LEFT("Total" + SPACE$(51%), 51%) + &
		FORMAT$(DR_TOTAL, "########.##") + " " + &
		FORMAT$(-CR_TOTAL, "########.##")

	IF DIST_YN$ = "Y"
	THEN
		TEXT$ = TEXT$ + &
			SPACE$(22%) + &
			FORMAT$(DIST_DB, "########.## ") + &
			FORMAT$(-DIST_CR, "########.##")
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TOTAL = FUNC_ROUND(DR_TOTAL + CR_TOTAL, 2%)

	IF TOTAL < 0.0
	THEN
		TEXT$ = LEFT("Change" + SPACE$(63%), 63%) + &
			FORMAT$(-TOTAL, "########.##")
	ELSE
		TEXT$ = LEFT("Change" + SPACE$(51%), 51%) + &
			FORMAT$(TOTAL, "########.##")
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)

	GOTO ExitProgram IF DIST_YN$ <> "Y"
	!
	! Print account summary page
	!
	TITLE$(5%) = "Account            Catg Description" + SPACE$(29%) + &
		"Debit         Credit"
	LIN% = 999%
	DEBIT_TOTAL, CREDIT_TOTAL, TOTAL = 0.0

17405	!
	! Get the (next) GL Chart record from the temporary file
	!
17410	WHEN ERROR IN
		GET #GL_TEMP.CH%
	USE
		CONTINUE 17420 IF ERR = 11%
		FILENAME$ = "GL_TEMP"
		CONTINUE HelpError
	END WHEN

	V% = GL_EXAM_CHART(GL_TEMP::ACCOUNT, GL_CHART_EXAM)

	TEXT$ = GL_TEMP::ACCOUNT + " " + &
		LEFT(GL_CHART_EXAM::DESCR, 30%) + " " + &
		FORMAT$(GL_TEMP::DEBIT, "<%>##,###,###.## ") + &
		FORMAT$(-GL_TEMP::CREDIT, "<%>##,###,###.## ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, LIN%)

	DEBIT_TOTAL = FUNC_ROUND(DEBIT_TOTAL + GL_TEMP::DEBIT, 2%)
	CREDIT_TOTAL = FUNC_ROUND(CREDIT_TOTAL + GL_TEMP::CREDIT, 2%)

	LIN% = 0%

	GOTO 17410

	!
	! Print out totals
	!
17420	IF NET_TOTAL <> 0.0
	THEN
		IF NET_TOTAL > 0.0
		THEN
			GL_TEMP::CREDIT = 0.0
			GL_TEMP::DEBIT = NET_TOTAL
		ELSE
			GL_TEMP::CREDIT = NET_TOTAL
			GL_TEMP::DEBIT = 0.0
		END IF

		TEXT$ = STRING$(LEN(GL_TEMP::ACCOUNT), A"?"B) + "      " + &
			"Undistributed Checks           " + &
			FORMAT$(GL_TEMP::DEBIT, "<%>##,###,###.## ") + &
			FORMAT$(-GL_TEMP::CREDIT, "<%>##,###,###.## ")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		DEBIT_TOTAL = FUNC_ROUND(DEBIT_TOTAL + GL_TEMP::DEBIT, 2%)
		CREDIT_TOTAL = FUNC_ROUND(CREDIT_TOTAL + GL_TEMP::CREDIT, 2%)
	END IF

	BAL_MESS$ = ""
	BAL_MESS$ = " Out of Balance" &
		IF FUNC_ROUND(DEBIT_TOTAL + CREDIT_TOTAL, 2%) <> 0.0
	TEXT$ = "Total" + SPACE$(50%) + &
		FORMAT$(DEBIT_TOTAL, "###,###,###.## ") + &
		FORMAT$(-CREDIT_TOTAL, "###,###,###.## ") + &
		BAL_MESS$

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)

 ExitProgram:

	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Exit to next program or menu
	!
	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

	%PAGE

 PrintCheck:
	!*****************************************************************
	! Print each check and each deposit
	!*****************************************************************
	GOTO PrintCheck1 &
		IF CK_COUNT% = 0% AND BANK_CODE$ <> "*"

	!
	! Determine number of lines that will be printed
	!
	IF (CK_COUNT% > ACC_COUNT%) OR (DIST_YN$ = "N")
	THEN
		TEST_LOOP% = CK_COUNT%
	ELSE
		TEST_LOOP% = ACC_COUNT%
	END IF

	GOTO PrintCheck1 IF TEST_LOOP% = 0%

	!
	! Calculate grand totals for both sides
	!
	TEST_TOTAL = 0.0
	TEST_TOTAL = TEST_TOTAL + CKDEPD(LOOP%) + CKDEPC(LOOP%) &
		FOR LOOP% = 1% TO CK_COUNT%

	TEST_TOTAL1 = 0.0
	TEST_TOTAL = TEST_TOTAL + ACCD(LOOP%) + ACCC(LOOP%) &
		FOR LOOP% = 1% TO ACC_COUNT%

	!
	! Flag if totals don't match
	!
	IF FUNC_ROUND(TEST_TOTAL - TEST_TOTAL1, 2%) <> 0.0
	THEN
		NET_TOTAL = NET_TOTAL + FUNC_ROUND(TEST_TOTAL1 - TEST_TOTAL, 2%)
		TEST_TOTAL$ = "*"
	ELSE
		TEST_TOTAL$ = " "
	END IF

18000	!
	! Figure out what the previous number should have been
	!
	LAST_NUM$ = EDIT$(CK_NUM$, -1%)
	ST% = FUNC_NUMBERADD(LAST_NUM$, -1%)

18010	!
	! Print message if this document number isn't one more than
	! the previous one was, or has the same numeric value as
	! the last one did.
	!
	IF NEXT_NUM$ <> ""
	THEN
		IF (NEXT_NUM$ <> EDIT$(CK_NUM$, -1%)) AND &
			(THIS_NUM$ <> XLATE(CK_NUM$, XLSTR$))
		THEN
			TEXT$ = "         Missing Document(s) " + &
				NEXT_NUM$ + " - " + &
				LAST_NUM$
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

			GOTO PrintCheck1 IF UTL_REPORTX::STAT
		END IF
	END IF

18020	!
	! Guess next document number
	!
	THIS_NUM$ = XLATE(CK_NUM$, XLSTR$)
	NEXT_NUM$ = EDIT$(CK_NUM$, -1%)
	ST% = FUNC_NUMBERADD(NEXT_NUM$, 1%)

18030	!
	! Zero totals
	!
	CKDD_TOTAL = 0.0
	CKDC_TOTAL = 0.0
	ACCD_TOTAL = 0.0
	ACCC_TOTAL = 0.0

	!
	! Print out all documents
	!
	FOR LOOP% = 1% TO TEST_LOOP%

		!
		! Set up description title
		!
		TEXT$ = LEFT(CK_NUM$ + SPACE$(6%), 6%) + "   " + &
			LEFT(DESCR$(LOOP%) + SPACE$(30%), 30%) + " " + &
			PRNT_DATE(CKDATE$(LOOP%), 8%) + " "

		!
		! Skip out if there isn't anything for left side
		!
		IF LOOP% > CK_COUNT%
		THEN
			TEXT$ = TEXT$ + SPACE$(26%)
			GOTO Dist
		END IF

		!
		! Slap on left side amounts
		!
		TEXT$ = TEXT$ + &
			FORMAT$(CKDEPD(LOOP%), "<%>#######.##") + " " + &
			FORMAT$(-CKDEPC(LOOP%), "<%>#######.##") + "   "

		CKDD_TOTAL = CKDD_TOTAL + CKDEPD(LOOP%)
		CKDC_TOTAL = CKDC_TOTAL + CKDEPC(LOOP%)

18040		!
		! Summarize by account number
		!
		WHEN ERROR IN
			GET #GL_TEMP.CH%, KEY #0% EQ CKACC$(LOOP%)
		USE
			CONTINUE 18050 IF ERR = 155%
			FILENAME$ = "GL_TEMP"
			CONTINUE HelpError
		END WHEN

		GL_TEMP::CREDIT = GL_TEMP::CREDIT + CKDEPC(LOOP%)
		GL_TEMP::DEBIT = GL_TEMP::DEBIT + CKDEPD(LOOP%)

		WHEN ERROR IN
			UPDATE #GL_TEMP.CH%
		USE
			CONTINUE 18050 IF ERR = 155%
			FILENAME$ = "GL_TEMP"
			CONTINUE HelpError
		END WHEN

		GOTO Dist

18050		GL_TEMP::ACCOUNT = CKACC$(LOOP%)
		GL_TEMP::CREDIT = CKDEPC(LOOP%)
		GL_TEMP::DEBIT = CKDEPD(LOOP%)

18070		WHEN ERROR IN
			PUT #GL_TEMP.CH%
		USE
			FILENAME$ = "GL_TEMP"
			CONTINUE HelpError
		END WHEN

		!**************************************************
		! Start on right side
		!**************************************************
 Dist:
		GOTO EndDist IF DIST_YN$ = "N"

		!
		! Skip if no distribution
		!
		IF ACC_COUNT% < LOOP%
		THEN
			TEXT$ = TEXT$ + SPACE$(42%)
			GOTO EndDist
		END IF

		TEXT$ = TEXT$ + &
			ACC$(LOOP%) + " " + &
			FORMAT$(ACCD(LOOP%), "<%>#######.##") + " " + &
			FORMAT$(-ACCC(LOOP%), "<%>#######.##")

		ACCD_TOTAL = ACCD_TOTAL + ACCD(LOOP%)
		ACCC_TOTAL = ACCC_TOTAL + ACCC(LOOP%)

18140		!
		! Summarize by account
		!
		WHEN ERROR IN
			GET #GL_TEMP.CH%, KEY #0% EQ ACC$(LOOP%)
		USE
			CONTINUE 18150 IF ERR = 155%
			FILENAME$ = "GL_TEMP"
			CONTINUE HelpError
		END WHEN

		GL_TEMP::CREDIT = GL_TEMP::CREDIT + ACCC(LOOP%)
		GL_TEMP::DEBIT = GL_TEMP::DEBIT + ACCD(LOOP%)

		WHEN ERROR IN
			UPDATE #GL_TEMP.CH%
		USE
			CONTINUE 18150 IF ERR = 155%
			FILENAME$ = "GL_TEMP"
			CONTINUE HelpError
		END WHEN

		GOTO EndDist

18150		GL_TEMP::ACCOUNT = ACC$(LOOP%)
		GL_TEMP::CREDIT = ACCC(LOOP%)
		GL_TEMP::DEBIT = ACCD(LOOP%)

18170		WHEN ERROR IN
			PUT #GL_TEMP.CH%
		USE
			FILENAME$ = "GL_TEMP"
			CONTINUE HelpError
		END WHEN

 EndDist:
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		PRINT_FLAG% = -1%
		GOTO PrintCheck1 &
			IF UTL_REPORTX::STAT

		!
		! Calculate totals
		!
		DIST_DB = DIST_DB + ACCD(LOOP%)
		DIST_CR = DIST_CR - ACCC(LOOP%)

	NEXT LOOP%

	!
	! Print total for this check/deposit
	!
	TEXT$ = "      " + "   " + &
		"Total                         " + " " + &
		"          " + " " + &
		FORMAT$(CKDD_TOTAL, "########.##") + " " + &
		FORMAT$(-CKDC_TOTAL, "########.##") + "   "

	IF DIST_YN$ = "Y"
	THEN
		TEXT$ = TEXT$ + &
			"                  " + " " + &
			FORMAT$(ACCD_TOTAL, "########.##") + " " + &
			FORMAT$(-ACCC_TOTAL, "########.##")
	END IF

	TEXT$ = TEXT$ + &
		FORMAT$(TEST_TOTAL, "########.##") + &
		TEST_TOTAL$

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -2%)

 PrintCheck1:
	RETURN

	%Page

 HardTest:
18200	!*******************************************************************
	! Check current record to decide what type it is, since simple
	! checks have faild to determine it.
	!*******************************************************************

	RESET #CK_CONTROLACC.CH%

	SOURCE_STRING$ = "A"

18210	WHEN ERROR IN
		GET #CK_CONTROLACC.CH%, REGARDLESS
	USE
		CONTINUE 18290
	END WHEN

	GOTO 18210 IF GL_YYYY_PP::ACCT <> CK_CONTROLACC::ACCOUNT

	IF (CK_CONTROLACC::STARTCK <= CK_TEMP::CHECK) AND &
		(CK_CONTROLACC::ENDCK >= CK_TEMP::CHECK)
	THEN
		SOURCE_STRING$ = "C"
	ELSE
		SOURCE_STRING$ = "A"
	END IF

18290	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
