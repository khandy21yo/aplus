1	%TITLE "IG to GL Comparison Report"
	%SBTTL "IC_RPRT_AUDT_GL"
	%IDENT "V3.6a Calico"

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
	! use or reliability of its software on equipment which is
	! not supported by Software Solutions, Inc.
	!
	!++
	! ID:GLBTCH
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Transmittal by Batch Report\* lists the transmittal information for
	!	each batch, which has been posted to a specified General
	!	Ledger file.
	!	.b
	!	The following columns are included:
	!	.table 30
	!	.te
	!	Account Number
	!	.te
	!	Description
	!	.te
	!	Debit Amount
	!	.te
	!	Credit Amount
	!	.te
	!	Units Amount
	!	.end table
	!	.LM -5
	!
	! Index:
	!	.x Audit>GL
	!	.x Audit>Compare
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_RPRT_AUDT_GL/LINE
	!	$ LINK/EXECUTABLE=IC_EXE: IC_RPRT_AUDT_GL, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_RPRT_AUDT_GL.OBJ;*
	!
	! Author:
	!
	!	04/16/98 - Kevin Handy
	!
	! Modification history:
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	05/24/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Fix several mismatched line #'s in error trapping
	!		Add several REGARDLESS clauses
	!
	!	08/10/2000 - Kevin Handy
	!		Don't skip PJ entries
	!
	!	05/15/2001 - Kevin Handy
	!		Added "use cost" option.
	!
	!	07/15/2002 - Kevin Handy
	!		Added Sortby (xref, batch) option
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!******************************************************************
	! External modules needed
	!******************************************************************

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!******************************************************************
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs)
	!******************************************************************

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD	GL_CHART

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP	(GL_YYYY_PP)	GL_YYYY_PP_CDD	GL_YYYY_PP

	%INCLUDE "SOURCE:[PD.OPEN]PD_ACCOUNT.HB"
	MAP	(PD_ACCOUNT)	PD_ACCOUNT_CDD	PD_ACCOUNT
	DECLARE PD_ACCOUNT_CDD		PD_ACCOUNT_EXAM

	%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.HB"
	MAP	(IC_TRANSACTION) IC_TRANSACTION_CDD IC_TRANSACTION

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSTYPE.HB"
	MAP	(UTL_TRANSTYPE)	UTL_TRANSTYPE_CDD	UTL_TRANSTYPE

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	!
	! Define record for the array(s)
	!
	RECORD TOTAL_RECORD
		STRING	ACCT = 18
		DOUBLE	DEBIT
		DOUBLE	CREDIT
		DOUBLE	UNITS
		DOUBLE	ICDEBIT
		DOUBLE	ICCREDIT
		DOUBLE	ICUNITS
	END RECORD

	!
	! Declare constants
	!
	DECLARE	LONG	CONSTANT MAX_BATCH = 200
	DECLARE LONG	CONSTANT MAX_GRAND = 200

	!
	! Dimension arrays
	!
	DIM TOTAL_RECORD BATCH_TOTAL(MAX_BATCH)
	DIM TOTAL_RECORD GRAND_TOTAL(MAX_GRAND)

	!
	! Declare some variables
	!
	DECLARE STRING CURRENT_YEAR, CURRENT_PERIOD
	DECLARE LONG TOTAL_BATCH, TOTAL_GRAND

	!
	! External functions
	!
	EXTERNAL	LONG	FUNCTION PD_READ_ACCOUNT
	EXTERNAL	LONG	FUNCTION PD_EXAM_PRODUCT
	EXTERNAL	REAL	FUNCTION PC_READ_COST

	%PAGE

	!******************************************************************
	! Take care of anything else before starting the report
	!******************************************************************

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Initialization:
	!******************************************************************
	! Get ready to begin
	!******************************************************************

	!
	! Initialize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = LEFT(EDIT$(UTL_REPORTX::OPTDEF(0%), 32%), 10%)

	!++
	! Abstract:FLD01
	!	^*(01) From Batch _#\*
	!	.b
	!	.lm +5
	!	The ^*From Batch _#\* setting selects a batch
	!	number from which the report is to begin printing.
	!	.b
	!	If the report is to begin with the first batch _# in the file, this
	!	field should be left blank.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = LEFT(TRM$(UTL_REPORTX::OPTDEF(1%)), 6%)

	!++
	! Abstract:FLD02
	!	^*(02) To Batch _#\*
	!	.b
	!	.lm +5
	!	The ^*To Batch _#\* setting selects a batch
	!	number with which the report is to end printing.
	!	.b
	!	If the report is to end with the last batch _# in the file, this
	!	field should be left blank.
	!	.lm -5
	!
	! Index:
	!
	!--

	YYYYPP$ = TRM$(UTL_REPORTX::OPTDEF(2%))
	YYYY_PP$ = LEFT(UTL_REPORTX::OPTDEF(2%), 4%) + "_" + &
		RIGHT(UTL_REPORTX::OPTDEF(2%), 5%)
	CURRENT_YEAR = LEFT(YYYY_PP$, 4%)
	CURRENT_PERIOD = TRM$(RIGHT(YYYY_PP$, 6%))

	!++
	! Abstract:FLD03
	!	^*(03) Period\*
	!	.b
	!	.lm +5
	!	^*Period\* refers to the accounting period that will be considered when
	!	running the report.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!
	!--

	WILDCARD$ = TRM$(UTL_REPORTX::OPTDEF(3%))
	WILDCARD$ = "*" IF WILDCARD$ = ""

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard Account\*
	!	.b
	!	.lm +5
	!	Selectes which GL accounts will appear.
	!	.lm -5
	!
	! Index:
	!
	!--

	USECOST$ = LEFT$(UTL_REPORTX::OPTDEF(4%), 1%)

	!++
	! Abstract:FLD05
	!	^*(05) Use Cost\*
	!	.b
	!	.lm +5
	!	Specifies if it should use the cost from the product
	!	cost (*Y) or if it should use the cost stored in the
	!	transaction file (*N)
	!	.lm -5
	!
	! Index:
	!
	!--

	SORTBY$ = LEFT$(UTL_REPORTX::OPTDEF(5%), 1%)

	IF (SORTBY$ = "X")
	THEN
		! Cross reference
		GL_SORTBY% = 2%
		IC_SORTBY% = 3%
	ELSE
		! Batch number
		GL_SORTBY% = 4%
		IC_SORTBY% = 1%
		FROM_ITEM$ = LEFT(FROM_ITEM$, 6%)
	END IF

	!++
	! Abstract:FLD06
	!	^*(06) Sort By\*
	!	.b
	!	.lm +5
	!	Specifies the sort order for the report.
	!	Options are *Batch or *Xref.
	!	.lm -5
	!
	! Index:
	!
	!--


	TOTAL_BATCH = 0%
	TOTAL_GRAND = 0%

300	!
	! Chart of accounts
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	!
	! Get the current period file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"
	USE
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

320	!
	! Generate list of valid accounts for inventory
	!
	INV_ACCT$ = ""
	%INCLUDE "SOURCE:[PD.OPEN]PD_ACCOUNT.OPN"

	RESET #PD_ACCOUNT.CH%

322	WHEN ERROR IN
		GET #PD_ACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE 328
	END WHEN

	ONE_ACCOUNT$ = PD_ACCOUNT::INVACCT

	IF INSTR(1%, INV_ACCT$ + ",", "," + ONE_ACCOUNT$ + ",") = 0% AND &
		COMP_STRING(ONE_ACCOUNT$, WILDCARD$)
	THEN
		INV_ACCT$ = INV_ACCT$ + "," + ONE_ACCOUNT$
	END IF

	GOTO 322

328	CLOSE PD_ACCOUNT.CH%

330	!
	! IC Transdaction File
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.OPN"
	USE
		FILENAME$ = "IC_TRANSACTION"
		CONTINUE HelpError
	END WHEN

340	!
	! Figure out the transaction types
	!
	TRANCODE$ = ""

	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSTYPE.OPN"
		RESET #UTL_TRANSTYPE.CH%
	USE
		FILENAME$ = "UTL_TRANSTYPE"
		CONTINUE HelpError
	END WHEN

342	WHEN ERROR IN
		GET #UTL_TRANSTYPE.CH%, REGARDLESS
	USE
		CONTINUE 348
	END WHEN

	IF (UTL_TRANSTYPE::CLASS = "01")
	THEN
		TRANCODE$ = TRANCODE$ + UTL_TRANSTYPE::CODE + ","
	END IF

	GOTO 342

348	CLOSE #UTL_TRANSTYPE.CH%

	%PAGE

 ReportTitle:
	!
	! Titles
	!
	TITLE$(1%) = "IC to GL Audit Listing"
	TITLE$(2%) = "Period " + CURRENT_PERIOD + " of Year " + CURRENT_YEAR
	TITLE$(3%) = ""

	!
	! Headers
	!
	TITLE$(4%) = "                                       " + &
		"  --------------General Ledger--------------" + &
		" ----------------Inventory-----------------"
	TITLE$(5%) = "Account #           Description        " + &
		"          Debit          Credit        Units" + &
		"         Debit          Credit        Units"
	TITLE$(6%) = ""

	!
	! Layouts for some of the printed lines
	!
	LYT_LINE$ = "$Account:018,$Descr:050,VDebit:072,VCredit:098," + &
		"VUnits:113"

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	THIS_BATCH$ = FROM_ITEM$

17010	GOSUB 17100			! GL Side
	GOSUB 17200			! IC Side
	GOSUB BatchTotal		! Print summary

17020	!
	! Figure out the next batch/xref to process
	!
	GL_YYYY_PP::BTHNUM = "ZZZZZZ"
	GL_YYYY_PP::XREFNO = "ZZZZZZ"

	WHEN ERROR IN
		GET #GL_YYYY_PP.CH%, &
			KEY #GL_SORTBY% GT THIS_BATCH$, &
			REGARDLESS
	USE
		CONTINUE 17050
	END WHEN

17030	IC_TRANSACTION::BATCH = "ZZZZZZ"
	IC_TRANSACTION::CROSS_REF = "ZZZZZZ"

	WHEN ERROR IN
		GET #IC_TRANSACTION.CH%, &
			KEY #IC_SORTBY% GT THIS_BATCH$, &
			REGARDLESS
	USE
		CONTINUE 17050
	END WHEN

17050	IF SORTBY$ = "X"
	THEN
		IF GL_YYYY_PP::XREFNO < IC_TRANSACTION::CROSS_REF
		THEN
			THIS_BATCH$ = GL_YYYY_PP::XREFNO
		ELSE
			THIS_BATCH$ = IC_TRANSACTION::CROSS_REF
		END IF
	ELSE
		IF GL_YYYY_PP::BTHNUM < IC_TRANSACTION::BATCH
		THEN
			THIS_BATCH$ = GL_YYYY_PP::BTHNUM
		ELSE
			THIS_BATCH$ = IC_TRANSACTION::BATCH
		END IF
	END IF

	IF THIS_BATCH$ <> "ZZZZZZ"
	THEN
		GOTO 17010
	ELSE
		GOTO ExitTotal
	END IF

17100	!******************************************************************
	! Main report loop starts here
	!******************************************************************

	WHEN ERROR IN
		FIND #GL_YYYY_PP.CH%, &
			KEY #GL_SORTBY% EQ THIS_BATCH$, &
			REGARDLESS
	USE
		CONTINUE 17190 IF ERR = 155%
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17110	!
	! Get next record
	!
	WHEN ERROR IN
		GET #GL_YYYY_PP.CH%, REGARDLESS
	USE
		CONTINUE 17190 IF ERR = 11%
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	IF SORTBY$ = "X"
	THEN
		GOTO 17190 IF (GL_YYYY_PP::XREFNO <> THIS_BATCH$)
	ELSE
		GOTO 17190 IF (GL_YYYY_PP::BTHNUM <> THIS_BATCH$)
	END IF

	GOTO 17110 IF INSTR(1%, INV_ACCT$, GL_YYYY_PP::ACCT) = 0%

	!
	! Search BATCH_TOTAL balance list for currently existing account
	!

	!
	! First a binary search to speed up the sequential search.
	! Will cut the search range down to a maximum of 4 items to look at.
	! Then is followed by a sequential search of the localized range.
	!
	SFROM% = 1%
	STO% = TOTAL_BATCH

	WHILE STO% - SFROM% > 4%
		SMID% = (SFROM% + STO%) / 2%
		IF (BATCH_TOTAL(SMID%)::ACCT <= GL_YYYY_PP::ACCT)
		THEN
			SFROM% = SMID%
		ELSE
			STO% = SMID%
		END IF
	NEXT

	!
	! Now the sequential search on the localized range.
	!
	GOTO GotAccount &
		IF (BATCH_TOTAL(I%)::ACCT = GL_YYYY_PP::ACCT) &
		FOR I% = SFROM% TO STO%

	!
	! Item not found, create it
	!
	I%, TOTAL_BATCH = TOTAL_BATCH + 1%

	WHILE (I% > 1%) AND (BATCH_TOTAL(I% - 1%)::ACCT > GL_YYYY_PP::ACCT)
		BATCH_TOTAL(I%) = BATCH_TOTAL(I% - 1%)
		I% = I% - 1%
	NEXT

	BATCH_TOTAL(I%)::ACCT = GL_YYYY_PP::ACCT
	BATCH_TOTAL(I%)::CREDIT = 0.0
	BATCH_TOTAL(I%)::DEBIT = 0.0
	BATCH_TOTAL(I%)::UNITS = 0.0
	BATCH_TOTAL(I%)::ICCREDIT = 0.0
	BATCH_TOTAL(I%)::ICDEBIT = 0.0
	BATCH_TOTAL(I%)::ICUNITS = 0.0

 GotAccount:
	!
	! Add credit/debit amounts
	!
	IF GL_YYYY_PP::AMOUNT > 0.0
	THEN
		BATCH_TOTAL(I%)::DEBIT = BATCH_TOTAL(I%)::DEBIT + &
			GL_YYYY_PP::AMOUNT
	ELSE
		BATCH_TOTAL(I%)::CREDIT = BATCH_TOTAL(I%)::CREDIT + &
			GL_YYYY_PP::AMOUNT
	END IF

	BATCH_TOTAL(I%)::UNITS = BATCH_TOTAL(I%)::UNITS + GL_YYYY_PP::UNITS

	!
	! Try for next record
	!
	GOTO 17110

17190	RETURN

	%PAGE

 GetNextICRec:
17200	!******************************************************************
	! Main report loop starts here
	!******************************************************************

	WHEN ERROR IN
		FIND #IC_TRANSACTION.CH%, &
			KEY #IC_SORTBY% EQ THIS_BATCH$, &
			REGARDLESS
	USE
		CONTINUE 17290 IF ERR = 155%
		FILENAME$ = "IC_TRANSACTION_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

17210	!
	! Get next record
	!
	WHEN ERROR IN
		GET #IC_TRANSACTION.CH%, REGARDLESS
	USE
		CONTINUE 17290 IF ERR = 11%
		FILENAME$ = "IC_TRANSACTION_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Check current record, Skip out if we can
	!
	IF SORTBY$ = "X"
	THEN
		GOTO 17290 IF (IC_TRANSACTION::CROSS_REF <> THIS_BATCH$)
	ELSE
		GOTO 17290 IF (IC_TRANSACTION::BATCH <> THIS_BATCH$)
	END IF

	GOTO 17210 IF INSTR(1%, TRANCODE$, IC_TRANSACTION::TYPE_A) = 0% AND &
		INSTR(1%, TRANCODE$, IC_TRANSACTION::TYPE_B) = 0%

	!
	! Get the inventory account number
	!
	EXIT_STATUS = PD_EXAM_PRODUCT(IC_TRANSACTION::PRODUCT, PD_PRODUCT_EXAM)
	IF EXIT_STATUS = CMC$_NORMAL
	THEN
		EXIT_STATUS = PD_READ_ACCOUNT(IC_TRANSACTION::LOCATION, &
			PD_PRODUCT_EXAM::PROD_TYPE, &
			PD_ACCOUNT_EXAM)
	END IF

	SELECT EXIT_STATUS
	!
	! Account number defined
	!
	CASE CMC$_NORMAL
		IC_ACCOUNT$ = PD_ACCOUNT_EXAM::INVACCT

	CASE ELSE
		IC_ACCOUNT$ = "??????????????????"
	END SELECT

	GOTO 17210 IF INSTR(1%, INV_ACCT$, IC_ACCOUNT$) = 0%


	!
	! Search BATCH_TOTAL balance list for currently existing account
	!

	!
	! First a binary search to speed up the sequential search.
	! Will cut the search range down to a maximum of 4 items to look at.
	! Then is followed by a sequential search of the localized range.
	!
	SFROM% = 1%
	STO% = TOTAL_BATCH

	WHILE STO% - SFROM% > 4%
		SMID% = (SFROM% + STO%) / 2%
		IF (BATCH_TOTAL(SMID%)::ACCT <= IC_ACCOUNT$)
		THEN
			SFROM% = SMID%
		ELSE
			STO% = SMID%
		END IF
	NEXT

	!
	! Now the sequential search on the localized range.
	!
	GOTO GotICAccount &
		IF (BATCH_TOTAL(I%)::ACCT = IC_ACCOUNT$) &
		FOR I% = SFROM% TO STO%

	!
	! Item not found, create it
	!
	I%, TOTAL_BATCH = TOTAL_BATCH + 1%

	WHILE (I% > 1%) AND (BATCH_TOTAL(I% - 1%)::ACCT > GL_YYYY_PP::ACCT)
		BATCH_TOTAL(I%) = BATCH_TOTAL(I% - 1%)
		I% = I% - 1%
	NEXT

	BATCH_TOTAL(I%)::ACCT = IC_ACCOUNT$
	BATCH_TOTAL(I%)::CREDIT = 0.0
	BATCH_TOTAL(I%)::DEBIT = 0.0
	BATCH_TOTAL(I%)::UNITS = 0.0
	BATCH_TOTAL(I%)::ICCREDIT = 0.0
	BATCH_TOTAL(I%)::ICDEBIT = 0.0
	BATCH_TOTAL(I%)::ICUNITS = 0.0

 GotICAccount:
	!
	! Add credit/debit amounts
	!
	ISIT% = INSTR(1%, TRANCODE$, IC_TRANSACTION::TYPE_A)
	IF (ISIT% <> 0%) AND (IC_TRANSACTION::QUANTITY_A <> 0.0)
	THEN
		IF IC_TRANSACTION::QUANTITY_A < 0.0
		THEN
			VSIGN = -1.0
		ELSE
			VSIGN = 1.0
		END IF

		IF (USECOST$ = "Y")
		THEN
			IC_AMOUNT = PC_READ_COST(IC_TRANSACTION::PRODUCT, &
				IC_TRANSACTION::LOCATION, &
				IC_TRANSACTION::TRANS_DATE, EFFDATE$) * &
				IC_TRANSACTION::QUANTITY_A
		ELSE
			IC_AMOUNT = ABS(IC_TRANSACTION::COST) * VSIGN
		END IF

		IF IC_AMOUNT > 0.0
		THEN
			BATCH_TOTAL(I%)::ICDEBIT = BATCH_TOTAL(I%)::ICDEBIT + &
				IC_AMOUNT
		ELSE
			BATCH_TOTAL(I%)::ICCREDIT = &
				BATCH_TOTAL(I%)::ICCREDIT + &
				IC_AMOUNT
		END IF

		BATCH_TOTAL(I%)::ICUNITS = BATCH_TOTAL(I%)::ICUNITS + &
			IC_TRANSACTION::QUANTITY_A
	END IF

	ISIT% = INSTR(1%, TRANCODE$, IC_TRANSACTION::TYPE_B)
	IF (ISIT% <> 0%) AND (IC_TRANSACTION::QUANTITY_B <> 0.0)
	THEN

		IF IC_TRANSACTION::QUANTITY_B < 0.0
		THEN
			VSIGN = -1.0
		ELSE
			VSIGN = 1.0
		END IF

		IF (USECOST$ = "Y")
		THEN
			IC_AMOUNT = PC_READ_COST(IC_TRANSACTION::PRODUCT, &
				IC_TRANSACTION::LOCATION, &
				IC_TRANSACTION::TRANS_DATE, EFFDATE$) * &
				IC_TRANSACTION::QUANTITY_B
		ELSE
			IC_AMOUNT = ABS(IC_TRANSACTION::COST) * VSIGN
		END IF

		IF IC_AMOUNT > 0.0
		THEN
			BATCH_TOTAL(I%)::ICDEBIT = BATCH_TOTAL(I%)::ICDEBIT + &
				IC_AMOUNT
		ELSE
			BATCH_TOTAL(I%)::ICCREDIT = BATCH_TOTAL(I%)::ICCREDIT + &
				IC_AMOUNT
		END IF

		BATCH_TOTAL(I%)::ICUNITS = BATCH_TOTAL(I%)::ICUNITS + &
			IC_TRANSACTION::QUANTITY_B
	END IF

	!
	! Try for next record
	!
	GOTO 17210

17290	RETURN

	%PAGE

17900	!******************************************************************
	! Handle totals and other items before EXITing
	!******************************************************************

 ExitTotal:
	!
	! Total up last batch number
	!
	GOSUB BatchTotal

	!
	! Print out grand total title
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 1000%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "Grand Totals", 0%)

	!
	! Print all totals for this batch
	!
	TOTAL_DEBIT = 0.0
	TOTAL_CREDIT = 0.0
	TOTAL_UNITS = 0.0
	TOTAL_ICDEBIT = 0.0
	TOTAL_ICCREDIT = 0.0
	TOTAL_ICUNITS = 0.0

	FOR I% = 1% TO TOTAL_GRAND

		GOTO 17930 IF GRAND_TOTAL(I%)::DEBIT = 0.0 AND &
			GRAND_TOTAL(I%)::CREDIT = 0.0 AND &
			GRAND_TOTAL(I%)::UNITS = 0.0 AND &
			GRAND_TOTAL(I%)::ICDEBIT = 0.0 AND &
			GRAND_TOTAL(I%)::ICCREDIT = 0.0 AND &
			GRAND_TOTAL(I%)::ICUNITS = 0.0

17910		GL_CHART::DESCR = STRING$(LEN(GL_CHART::DESCR), 63%)

		WHEN ERROR IN
			GET #GL_CHART.CH%, &
				KEY #0% EQ GRAND_TOTAL(I%)::ACCT, &
				REGARDLESS
		USE
			CONTINUE 17920 IF ERR = 155%
			FILENAME$ = "GL_CHART"
			CONTINUE HelpError
		END WHEN

17920		TEXT$ = LEFT(GRAND_TOTAL(I%)::ACCT + SPACE$(18%), 18%) + &
			"  " + &
			LEFT(GL_CHART::DESCR, 20%) + " " + &
			FORMAT$(GRAND_TOTAL(I%)::DEBIT, &
				"<%>##,###,###.## ") + &
			FORMAT$(-GRAND_TOTAL(I%)::CREDIT, &
				"<%>##,###,###.## ") + &
			FORMAT$(GRAND_TOTAL(I%)::UNITS, &
				"#,###,###.## ") + &
			FORMAT$(GRAND_TOTAL(I%)::ICDEBIT, &
				"<%>##,###,###.## ") + &
			FORMAT$(-GRAND_TOTAL(I%)::ICCREDIT, &
				"<%>##,###,###.## ") + &
			FORMAT$(GRAND_TOTAL(I%)::ICUNITS, &
				"#,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TOTAL_DEBIT = TOTAL_DEBIT + GRAND_TOTAL(I%)::DEBIT
		TOTAL_CREDIT = TOTAL_CREDIT + GRAND_TOTAL(I%)::CREDIT
		TOTAL_UNITS = TOTAL_UNITS + GRAND_TOTAL(I%)::UNITS
		TOTAL_ICDEBIT = TOTAL_ICDEBIT + GRAND_TOTAL(I%)::ICDEBIT
		TOTAL_ICCREDIT = TOTAL_ICCREDIT + GRAND_TOTAL(I%)::ICCREDIT
		TOTAL_ICUNITS = TOTAL_ICUNITS + GRAND_TOTAL(I%)::ICUNITS

17930	NEXT I%

	TEXT$ = "                    " + &
		"Grand Totals         " + &
		FORMAT$(TOTAL_DEBIT, "<%>##,###,###.## ") + &
		FORMAT$(-TOTAL_CREDIT, "<%>##,###,###.## ") + &
		FORMAT$(TOTAL_UNITS, "#,###,###.## ") + &
		FORMAT$(TOTAL_ICDEBIT, "<%>##,###,###.## ") + &
		FORMAT$(-TOTAL_ICCREDIT, "<%>##,###,###.## ") + &
		FORMAT$(TOTAL_ICUNITS, "#,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)



 ExitProgram:
	!
	! Finish up the report
	!
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

 BatchTotal:
18000	!******************************************************************
	! Subroutine to print out totals for one batch
	!******************************************************************

	GOTO 18040 IF TOTAL_BATCH = 0%

	!
	! Calculate totals
	!
	TOTAL_DEBIT = 0.0
	TOTAL_CREDIT = 0.0
	TOTAL_UNITS = 0.0
	TOTAL_ICDEBIT = 0.0
	TOTAL_ICCREDIT = 0.0
	TOTAL_ICUNITS = 0.0

	FOR I% = 1% TO TOTAL_BATCH

		TOTAL_DEBIT = TOTAL_DEBIT + BATCH_TOTAL(I%)::DEBIT
		TOTAL_CREDIT = TOTAL_CREDIT + BATCH_TOTAL(I%)::CREDIT
		TOTAL_UNITS = TOTAL_UNITS + BATCH_TOTAL(I%)::UNITS
		TOTAL_ICDEBIT = TOTAL_ICDEBIT + BATCH_TOTAL(I%)::ICDEBIT
		TOTAL_ICCREDIT = TOTAL_ICCREDIT + BATCH_TOTAL(I%)::ICCREDIT
		TOTAL_ICUNITS = TOTAL_ICUNITS + BATCH_TOTAL(I%)::ICUNITS

	NEXT I%

	!
	! Print title for batch
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 8%)

	IF SORTBY$ = "X"
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
			"CROSS REF:  " + THIS_BATCH$, 3%)
	ELSE
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
			"BATCH NUMBER:  " + THIS_BATCH$, 3%)
	END IF

	TEST_FLAG$ = "      "

	!
	! Print all totals for this batch
	!
	FOR I% = 1% TO TOTAL_BATCH

18010		WHEN ERROR IN
			GET #GL_CHART.CH%, &
				KEY #0% EQ BATCH_TOTAL(I%)::ACCT, &
				REGARDLESS
		USE
			GL_CHART::DESCR = "**********"
			CONTINUE 18020
		END WHEN

18020		IF ABS(BATCH_TOTAL(I%)::DEBIT - &
			BATCH_TOTAL(I%)::ICDEBIT) > .001 OR &
			ABS(BATCH_TOTAL(I%)::CREDIT - &
			BATCH_TOTAL(I%)::ICCREDIT) > .001
		THEN
			TEST_FLAG$ = "******"
			ONE_FLAG$ = "**"
		ELSE
			ONE_FLAG$ = ""
		END IF

		TEXT$ = BATCH_TOTAL(I%)::ACCT + "  " + &
			LEFT(GL_CHART::DESCR, 20%) + " " + &
			FORMAT$(BATCH_TOTAL(I%)::DEBIT, &
				"<%>##,###,###.## ") + &
			FORMAT$(-BATCH_TOTAL(I%)::CREDIT, &
				"<%>##,###,###.## ") + &
			FORMAT$(BATCH_TOTAL(I%)::UNITS, &
				"#,###,###.## ") + &
			FORMAT$(BATCH_TOTAL(I%)::ICDEBIT, &
				"<%>##,###,###.## ") + &
			FORMAT$(-BATCH_TOTAL(I%)::ICCREDIT, &
				"<%>##,###,###.## ") + &
			FORMAT$(BATCH_TOTAL(I%)::ICUNITS, &
				"#,###,###.## ") + &
			ONE_FLAG$

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, &
			TITLE$(), TEXT$, 1%)

	NEXT I%

	!
	! Print title for batch
	!
	TEXT$ = TEST_FLAG$ + "              " + &
		"Batch Totals         " + &
		FORMAT$(TOTAL_DEBIT, &
			"<%>##,###,###.## ") + &
		FORMAT$(-TOTAL_CREDIT, &
			"<%>##,###,###.## ") + &
		FORMAT$(TOTAL_UNITS, &
			"#,###,###.## ") + &
		FORMAT$(TOTAL_ICDEBIT, &
			"<%>##,###,###.## ") + &
		FORMAT$(-TOTAL_ICCREDIT, &
			"<%>##,###,###.## ") + &
		FORMAT$(TOTAL_ICUNITS, &
			"#,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

18030	!
	! Insert batch totals into grand totals
	!
	GRAND_POINTER% = 1%

	FOR I% = 1% TO TOTAL_BATCH

		!
		! Search for place to add it to in the grand totals
		!
		GRAND_POINTER% = GRAND_POINTER% + 1% &
			UNTIL (GRAND_TOTAL(GRAND_POINTER%)::ACCT >= &
			BATCH_TOTAL(I%)::ACCT) OR &
			GRAND_POINTER% > TOTAL_GRAND

		!
		! Create new one if necessary
		!
		IF (GRAND_POINTER% > TOTAL_GRAND) OR &
			(GRAND_TOTAL(GRAND_POINTER%)::ACCT <> &
			BATCH_TOTAL(I%)::ACCT)
		THEN
			!
			! Make room for new one
			!
			TOTAL_GRAND = TOTAL_GRAND + 1%
			GRAND_TOTAL(J%) = GRAND_TOTAL(J% - 1%) &
				FOR J% = TOTAL_GRAND TO GRAND_POINTER% + 1% &
					STEP -1%

			!
			! And insert it
			!
			GRAND_TOTAL(GRAND_POINTER%) = BATCH_TOTAL(I%)
		ELSE
			!
			! Add to the totals
			!
			J% = GRAND_POINTER%
			GRAND_TOTAL(J%)::DEBIT = GRAND_TOTAL(J%)::DEBIT + &
				BATCH_TOTAL(I%)::DEBIT
			GRAND_TOTAL(J%)::CREDIT = GRAND_TOTAL(J%)::CREDIT + &
				BATCH_TOTAL(I%)::CREDIT
			GRAND_TOTAL(J%)::UNITS = GRAND_TOTAL(J%)::UNITS + &
				BATCH_TOTAL(I%)::UNITS
			GRAND_TOTAL(J%)::ICDEBIT = GRAND_TOTAL(J%)::ICDEBIT + &
				BATCH_TOTAL(I%)::ICDEBIT
			GRAND_TOTAL(J%)::ICCREDIT = GRAND_TOTAL(J%)::ICCREDIT + &
				BATCH_TOTAL(I%)::ICCREDIT
			GRAND_TOTAL(J%)::ICUNITS = GRAND_TOTAL(J%)::ICUNITS + &
				BATCH_TOTAL(I%)::ICUNITS
		END IF

	NEXT I%

18040	!
	! Initialize for new batch group
	!
	TOTAL_BATCH = 0%

	RETURN

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
	! Handle untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32767	!******************************************************************
	! End of report IC_RPRT_AUDT_GL
	!******************************************************************
	END
