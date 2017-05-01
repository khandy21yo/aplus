1	%TITLE "Post Journal"
	%SBTTL "OE_POST_CREDIT"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1990 BY
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
	! ID:OE0012
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Post Journal\* option transfers
	!	information from the Credit Memo Journal file to the Accounts
	!	Receivable Register, Inventory transaction file (when a product is
	!	restocked) and the General Ledger.
	!	.lm -5
	!
	! Index:
	!	.x Post>Credit Memo Journal
	!	.x Credit Memo Journal>Post
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_POST_CREDIT/LINE
	!	$ LINK/EXE=OE_EXE: OE_POST_CREDIT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_POST_CREDIT.OBJ;*
	!
	! Author:
	!
	!	10/25/91 - Frank F. Starman
	!
	! Modification history:
	!
	!	11/15/91 - Dan Perkins
	!		Modified to carry OE_CREASON description
	!		through to AR_OPEN description.
	!
	!	11/22/91 - Dan Perkins
	!		Changed FUNC_ROUND to round money to
	!		two decimal places.
	!
	!	12/18/91 - Kevin Handy
	!		Removed unused map for POST_TO_GL.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	02/16/92 - Frank F. Starman
	!		Added OE_REASONACCT
	!
	!	04/27/92 - Kevin Handy
	!		Clean up (check)
	!
	!	06/08/92 - Dan Perkins
	!		Kill Journal files before completing posting, if
	!		posting gets that far.
	!
	!	02/05/93 - Kevin Handy
	!		Added AR_OPEN::DUEDATE and AR_OPEN::DISCOUNTDATE.
	!
	!	04/05/93 - Kevin Handy
	!		Clean up (check)
	!
	!	04/12/93 - Dan Perkins
	!		Added code to enable this program to print the
	!		Journal if desired.
	!
	!	04/21/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/05/93 - Dan Perkins
	!		Added code to print the rest of the header
	!		info like handling, freight, and memo total.
	!		Moved tax section after line section since the
	!		tax is figured on the line totals.  This is because
	!		the header sales tax field is a percentage and not
	!		a dollar amount.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/27/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to SPACE$(...) in
	!		several places.
	!
	!	06/10/96 - Kevin Handy
	!		Don't include sa_salesman from cdd, since it is
	!		never used.
	!		Reformat source code.
	!
	!	09/15/97 - Kevin Handy
	!		Use OE_CREDITJOUR::MEMONUM instead of
	!		OE_CREDITLINE::MEMONUM in total line so we don't
	!		get goofy stuff printed there.
	!		Changed to print two decimals on quanity instead
	!		of just whole units.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/13/98 - Kevin Handy
	!		Add TAXFLAG parameter to OE_READ_SALESTAX
	!
	!	08/09/2000 - Kevin Handy
	!		Match sign of UNITS in GL to AMOUNT.
	!		Use WHEN ERROR IN
	!
	!	09/18/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include constants and and some functions
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[OE.OPEN]OE_CREDITJOUR.HB"
	MAP (OE_CREDITJOUR)	OE_CREDITJOUR_CDD	OE_CREDITJOUR

	%INCLUDE "SOURCE:[OE.OPEN]OE_CREDITLINE.HB"
	MAP (OE_CREDITLINE)	OE_CREDITLINE_CDD	OE_CREDITLINE

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN_DIST.HB"
	%INCLUDE "SOURCE:[AR.OPEN]AR_SALTAXLED.HB"

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"

	%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.HB"

	%INCLUDE "SOURCE:[OE.OPEN]OE_ACCOUNT.HB"
	%INCLUDE "SOURCE:[OE.OPEN]OE_PROMO.HB"
	%INCLUDE "SOURCE:[OE.OPEN]OE_REASONACCT.HB"
	%INCLUDE "SOURCE:[OE.OPEN]OE_SALESTAX.HB"

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	%INCLUDE "SOURCE:[PD.OPEN]PD_ACCOUNT.HB"

	%INCLUDE "SOURCE:[SA.OPEN]SA_COMMACCT.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	AR_EXAM_CUSTOM
	EXTERNAL LONG	FUNCTION	AR_TRAN_POSTAR
	EXTERNAL LONG	FUNCTION	ASSG_POSTBATCH

	EXTERNAL LONG	FUNCTION	GL_TRAN_POSTGL
	EXTERNAL LONG	FUNCTION	GL_EXAM_CHART

	EXTERNAL LONG	FUNCTION	IC_TRAN_POST

	EXTERNAL LONG	FUNCTION	OE_READ_ACCOUNT
	EXTERNAL LONG	FUNCTION	OE_READ_PROMO
	EXTERNAL LONG	FUNCTION	OE_READ_REASONACCT
	EXTERNAL LONG	FUNCTION	OE_READ_SALESTAX
	EXTERNAL LONG	FUNCTION	OUTP_UNDEFCODES

	EXTERNAL LONG	FUNCTION	PD_EXAM_PRODUCT
	EXTERNAL LONG	FUNCTION	PD_READ_ACCOUNT

	EXTERNAL LONG	FUNCTION	UTL_EXAM_LOCATION

	!
	! Declare internal variables
	!
	DECLARE AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM
	DECLARE AR_OPEN_CDD		AR_OPEN
	DECLARE AR_OPEN_DIST_CDD	AR_OPEN_DIST
	DECLARE AR_SALTAXLED_CDD	AR_SALTAXLED

	DECLARE	IC_TRANSACTION_CDD	IC_TRANSACTION

	DECLARE GL_CHART_CDD		GL_CHART_EXAM
	DECLARE	GL_YYYY_PP_CDD		GL_YYYY_PP

	DECLARE OE_ACCOUNT_CDD		OE_ACCOUNT_READ
	DECLARE OE_PROMO_CDD		OE_PROMO_READ
	DECLARE	OE_REASONACCT_CDD	OE_REASONACCT_READ
	DECLARE OE_SALESTAX_CDD		OE_SALESTAX_READ

	DECLARE PD_PRODUCT_CDD		PD_PRODUCT_EXAM
	DECLARE PD_ACCOUNT_CDD		PD_ACCOUNT_READ

	DECLARE	UTL_LOCATION_CDD	UTL_LOCATION_EXAM
	DECLARE	UTL_REPORTX_CDD		UTL_REPORTX

	DECLARE	LONG			EXIT_STATUS
	DECLARE	LONG			INTR_STATUS
	DECLARE LONG			PRNT_SUMMARY
	DECLARE LONG			CHECK_PERIOD

	DECLARE	STRING			ICPERIOD
	DECLARE	STRING			IC.INTER.PERIOD
	DECLARE	STRING			BATCH_NUMBER
	DECLARE STRING			CHECK_DATE
	DECLARE	STRING			POSTDATE
	DECLARE	STRING			POSTTIME
	DECLARE	STRING			TEXT
	DECLARE	STRING			TITLE(10%)

	%PAGE

	!**************************************************************
	! Get some stuff done before we start
	!**************************************************************

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Title
	!
	TITLE(1%) = "CREDIT  JOURNAL  POSTING  PROTOCOL"
	TITLE(2%) = "Order Entry System"
	TITLE(3%) = ""

	!
	! Heading
	!
	TITLE(4%) = "."

	%PAGE

	!**************************************************************
	! Process `from user' input
	!**************************************************************

	!
	! Set user defined fields
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	.x Batch Number>Print Journal
	!	^*(01) Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field enters a
	!	particular batch to be posted.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Print Journal>Batch Number
	!
	! Required:
	!--

	ICPERIOD = EDIT$(UTL_REPORTX::OPTDEF(1%), -1%)

	!++
	! Abstract:FLD02
	!	.X Post>Accounting Period
	!	^*(02) Accounting Period\*
	!	.b
	!	.lm +5
	!	The ^*Accounting Period\* field enters the
	!	accounting period into which this batch
	!	will be posted.
	!	.b
	!	The format for entry is YYYYPP.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	CHECK_DATE = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	.x Check Dates
	!	^*(04) Check Date\*
	!	.b
	!	.lm +5
	!	The ^*Check Date\* field checks all dates of the
	!	entries in the journal to insure they are within a particular date range
	!	(accounting period) for posting.
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes
	!	.te
	!	^*N\* - No
	!	.end table
	!	By entering "N" for No, the dates will not be checked and all entries
	!	will be posted. A "Y" for Yes will cause all dates to be checked. If "Y"
	!	is entered and dates are found that are not within the date range for
	!	posting, the posting will be aborted.
	!	.lm -5
	!
	! Index:
	!
	!--

	CHECK_PERIOD = SUBOPT_NOOPT
	CHECK_PERIOD = SUBOPT_CHECK IF CHECK_DATE = "Y"

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	.x Sort by
	!	^*(06) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines the order
	!	in which the report will print.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*M\* - Memo Number
	!	.te
	!	^*N\* - Customer Number
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(6%), 132%)

	!++
	! Abstract:FLD07
	!	^*(07) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field enters an item with which the
	!	report will begin printing.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x FromItem
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(7%), 132%)

	!++
	! Abstract:FLD08
	!	^*(08) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field enters an item with which the
	!	report will end printing.
	!	.b
	!	A blank field will cause the report to end with the last
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(8%), -1%)

	!++
	! Abstract:FLD09
	!	^*(09) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects
	!	designated items to be printed by entering a "wildcard"
	!	for Wildcarding
	!	Technique.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

	%PAGE

300	!
	! Open Credit header file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_CREDITJOUR.UPD"
	USE
		FILENAME$ = "OE_CREDITJOUR"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Credit Journal line items file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_CREDITLINE.UPD"
	USE
		FILENAME$ = "OE_CREDITLINE"
		CONTINUE HelpError
	END WHEN

	SELECT SORTBY$

	CASE "M"
		K_NUM% = 0%
		TITLE$(1%) = " CREDIT JOURNAL " + BATCH_NO$ + &
			" BY MEMO NUMBER"

		!
		! Routine to load left justified spaces into FROM_ITEM
		! and TO_ITEM if any memo numbers are entered as ranges
		!
		FROM_ITEM$ = SPACE$(LEN(OE_CREDITJOUR::MEMONUM) - &
			LEN(FROM_ITEM$)) + &
			FROM_ITEM$ IF FROM_ITEM$ <> ""

		TO_ITEM$ = SPACE$(LEN(OE_CREDITJOUR::MEMONUM) - &
			LEN(TO_ITEM$)) + &
			TO_ITEM$ IF TO_ITEM$ <> ""

	CASE "N"
		K_NUM% = 1%
		TITLE$(1%) = " CREDIT JOURNAL " + BATCH_NO$ + &
			" BY CUSTOMER NUMBER"

	END SELECT

	TITLE$(2%) = " Order-Invoice System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	IF SORTBY$ <> ""
	THEN
		TITLE$(4%) = "Memo#     MemoDate     OrderDate    SalType" + &
			"  CusNumber   CustomerName       " + &
			"                  Operator     Salesman"

		TITLE$(5%) = "           Cr Product#       Description          " + &
			"         Cost     CrdQty      InvQty     " + &
			"UnitPrice     Promo  Disc%       ExtPrice"

		TITLE$(6%) = "                Discount      SalesTax      " + &
			"Handling       Freight      Misc Chg  MiscAcct" + &
			SPACE$(30%) + "Credit Total"

		TITLE$(7%) = "."

		!
		! Set values to zero
		!
		CTR% = 0%

		BATCH_NUMBER = "OO"
		GOTO StartReport
	END IF

	%PAGE

	!******************************************************************
	!	1) See if the posting process has been interrupted
	!	2) If not interrupted, go on
	!	3) If interrupted, delete the superceded ledger records
	!		and then continue
	!******************************************************************

	!******************************************************************
	! Check if posting process has been interrupted
	!******************************************************************

	!
	! Open up batch control file and check if interrupted
	!
	INTR_STATUS = ASSG_POSTBATCH(OPT_RESTART, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "OE_CREDITJOUR", BATCH_NO$, IC.INTER.PERIOD, "")

	SELECT INTR_STATUS
	!
	! Success; keep going
	!
	CASE CMC$_NORMAL

	!
	! Process was interrupted
	!
	CASE CMC$_WARNING

		IF TRM$(IC.INTER.PERIOD) <> ""
		THEN
			GOTO Aborted IF IC_TRAN_POST(OPT_RESTART, SUBOPT_NOOPT, &
				BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", &
				IC.INTER.PERIOD) <> CMC$_NORMAL

			GOTO Aborted IF GL_TRAN_POSTGL(OPT_RESTART, SUBOPT_NOOPT, &
				BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", "", &
				IC.INTER.PERIOD) <> CMC$_NORMAL

			GOTO Aborted IF &
				AR_TRAN_POSTAR(OPT_RESTART, SUBOPT_NOOPT, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				"", "", "", IC.INTER.PERIOD) <> CMC$_NORMAL

		END IF

	!
	! Abort post process
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	%PAGE

 AssignBatch:
	!******************************************************************
	!	1) Assign a batch number
	!	2) Make sure no legitimate records in the ledger already
	!		have this batch number; if any records do have
	!		this newly assigned number, go back to (1) and
	!		get a new one.
	!******************************************************************
	!
	! Open up batch control file and get a batch number
	!
	GOTO Aborted IF ASSG_POSTBATCH(OPT_ASSIGN, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "OE_CREDITJOUR", &
		BATCH_NO$, ICPERIOD, "") <> CMC$_NORMAL

 StartReport:
	EXIT_STATUS = IC_TRAN_POST(OPT_CHECK, SUBOPT_NOOPT, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", ICPERIOD)

	SELECT EXIT_STATUS

	!
	! Success
	!
	CASE CMC$_NORMAL

	!
	! Found batch number, go for new one
	!
	CASE CMC$_WARNING
		GOTO AssignBatch

	!
	! Something else wrong
	!
	CASE ELSE
		GOTO Aborted
	END SELECT

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_CHECK, SUBOPT_NOOPT, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", ICPERIOD)

	SELECT EXIT_STATUS

	!
	! Success
	!
	CASE CMC$_NORMAL

	!
	! Found batch number, go for new one
	!
	CASE CMC$_WARNING
		GOTO AssignBatch

	!
	! Something else wrong
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	EXIT_STATUS = AR_TRAN_POSTAR(OPT_CHECK, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "", "", "", ICPERIOD)

	SELECT EXIT_STATUS

	!
	! Success; keep going
	!
	CASE CMC$_NORMAL

	!
	! Found batch number, go for new one
	!
	CASE CMC$_WARNING
		GOTO AssignBatch

	!
	! Something's definitely wrong here
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	%PAGE

 Transmittel:
	!******************************************************************
	! Create transmittal
	!******************************************************************
	IF SORTBY$ = ""
	THEN
		GOTO Aborted IF ASSG_POSTBATCH(OPT_ADDREC, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "OE_CREDITJOUR", &
			BATCH_NO$, "", "") <> CMC$_NORMAL
	END IF

	!
	! If from item is blank then reset Header file
	! else try to find the first record
	!
	IF FROM_ITEM$ = ""
	THEN
		RESET #OE_CREDITJOUR.CH%, KEY #K_NUM%
	ELSE
		FIND #OE_CREDITJOUR.CH%, KEY #K_NUM% GE FROM_ITEM$, REGARDLESS
	END IF

	POSTDATE = DATE_TODAY
	POSTTIME = TIME_NOW

 ReadHeader:
3000	OE_CREDITLINE::PRODUCT = ""

	!
	! Read in one record from the header file
	!
	WHEN ERROR IN
		GET #OE_CREDITJOUR.CH%
	USE
		!
		! Locked block
		!
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE Confirm
	END WHEN

	!
	! Check current record if should be printed
	!
	SELECT SORTBY$

	CASE "M"
		GOTO Confirm IF (OE_CREDITJOUR::MEMONUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO ReadHeader IF COMP_ARRAY(EDIT$( &
			OE_CREDITJOUR::MEMONUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "N"
		GOTO Confirm IF (OE_CREDITJOUR::CUSNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO ReadHeader IF COMP_ARRAY(EDIT$( &
			OE_CREDITJOUR::CUSNUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

	!
	! Read customer name
	!
	EXIT_STATUS = AR_EXAM_CUSTOM(OE_CREDITJOUR::CUSNUM, AR_35CUSTOM_EXAM)

	SELECT EXIT_STATUS

	!
	! Success; go on
	!
	CASE CMC$_NORMAL

	!
	! Undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		TEXT$ = CONV_STRING(OE_CREDITJOUR::MEMONUM, CMC$_LEFT) + &
			" (CustNumber)* " + &
			OE_CREDITJOUR::CUSNUM

		!
		! Keep undefined codes
		!
		GOTO Aborted IF OUTP_UNDEFCODES(OPT_ADDREC, &
			TITLE(), UTL_REPORTX, TEXT$) <> CMC$_NORMAL

	!
	! Untrapped error
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! Check location
	!
	EXIT_STATUS = UTL_EXAM_LOCATION(OE_CREDITJOUR::LOCATION, &
		UTL_LOCATION_EXAM)

	SELECT EXIT_STATUS

	!
	! Success; go on
	!
	CASE CMC$_NORMAL

	!
	! Undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		TEXT$ = CONV_STRING(OE_CREDITJOUR::MEMONUM, CMC$_LEFT) + &
			" (Location)* " + &
			OE_CREDITJOUR::LOCATION

		!
		! Keep undefined codes
		!
		GOTO Aborted IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), &
			UTL_REPORTX, TEXT$) <> CMC$_NORMAL

	!
	! Untrapped error
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	IF SORTBY$ <> ""
	THEN
		!
		! Print out one line
		!
		TEXT$ = CONV_STRING(OE_CREDITJOUR::MEMONUM, CMC$_LEFT) + "  " + &
			PRNT_DATE(OE_CREDITJOUR::MEMODATE, 8%) + "   " + &
			PRNT_DATE(OE_CREDITJOUR::ORDDATE, 8%) + "   " + &
			OE_CREDITJOUR::ORDTYPE + "       " + &
			OE_CREDITJOUR::CUSNUM + "  " + &
			LEFT$(AR_35CUSTOM_EXAM::CUSNAM, 35%) + "  " + &
			OE_CREDITJOUR::OPERATOR + "   " + &
			OE_CREDITJOUR::SALESMAN

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	!
	! Get some account information
	!
	V% = OE_READ_ACCOUNT(AR_35CUSTOM_EXAM::TTYPE, OE_CREDITJOUR::ORDTYPE, &
		OE_CREDITJOUR::LOCATION, OE_ACCOUNT_READ)

	!
	! Begin GL post function for Header information
	!
	! Generate a GL record to pass through to the post function
	!
	GL_YYYY_PP::SOURCE	= "CR"
	GL_YYYY_PP::REFNO	= OE_CREDITJOUR::MEMONUM
	GL_YYYY_PP::TRANDAT	= OE_CREDITJOUR::MEMODATE
	GL_YYYY_PP::DESCR	= AR_35CUSTOM_EXAM::CUSNAM
	GL_YYYY_PP::XREFNO	= OE_CREDITJOUR::CUSNUM
	GL_YYYY_PP::CKNO	= ""
	GL_YYYY_PP::TRANKEY	= ""
	GL_YYYY_PP::SUBACC	= OE_CREDITJOUR::SALESMAN
	GL_YYYY_PP::OPERATION	= ""
	GL_YYYY_PP::HOURS	= 0.0
	GL_YYYY_PP::UPDSTA	= ""
	GL_YYYY_PP::POSDAT	= POSTDATE
	GL_YYYY_PP::POSTIM	= POSTTIME
	GL_YYYY_PP::BTHNUM	= BATCH_NUMBER

	!
	!
	! Check Invoice Handling
	!
 Handling:
	GOTO Misc IF OE_CREDITJOUR::HANDLING = 0.0

	!
	! Is Handling account number defined?
	!
	EXIT_STATUS = GL_EXAM_CHART(OE_ACCOUNT_READ::HANDLING, GL_CHART_EXAM)

	SELECT EXIT_STATUS

	!
	! Account number defined
	!
	CASE CMC$_NORMAL

	!
	! Number undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		TEXT$ = CONV_STRING(OE_CREDITJOUR::MEMONUM, CMC$_LEFT) + &
			" (Handling Acct)* " + &
			OE_ACCOUNT_READ::HANDLING

		!
		! Keep undefined codes
		!
		GOTO Aborted IF OUTP_UNDEFCODES(OPT_ADDREC, &
			TITLE(), UTL_REPORTX, TEXT$) <> CMC$_NORMAL

	!
	! Weird happenin's
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	GL_YYYY_PP::ACCT	= OE_ACCOUNT_READ::HANDLING
	GL_YYYY_PP::AMOUNT	= FUNC_ROUND(OE_CREDITJOUR::HANDLING, 2%)
	GL_YYYY_PP::UNITS	= 0.0
	GL_YYYY_PP::DESCR	= AR_35CUSTOM_EXAM::CUSNAM

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, ICPERIOD)

	!
	! Check Invoice Miscellaneous
	!
 Misc:
	GOTO Freight IF OE_CREDITJOUR::MISC = 0.0

	!
	! Is Miscellaneous account number defined?
	!
	EXIT_STATUS = GL_EXAM_CHART(OE_CREDITJOUR::MISCACCT, GL_CHART_EXAM)

	SELECT EXIT_STATUS

	!
	! Account number defined
	!
	CASE CMC$_NORMAL

	!
	! Number undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		TEXT$ = CONV_STRING(OE_CREDITJOUR::MEMONUM, CMC$_LEFT) + &
			" (Misc Acct)* " + &
			OE_CREDITJOUR::MISCACCT

		!
		! Keep undefined codes
		!
		GOTO Aborted IF OUTP_UNDEFCODES(OPT_ADDREC, &
			TITLE(), UTL_REPORTX, TEXT$) <> CMC$_NORMAL

	!
	! Weird happenin's
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	GL_YYYY_PP::ACCT	= OE_CREDITJOUR::MISCACCT
	GL_YYYY_PP::AMOUNT	= FUNC_ROUND(OE_CREDITJOUR::MISC, 2%)
	GL_YYYY_PP::UNITS	= 0.0
	GL_YYYY_PP::DESCR	= AR_35CUSTOM_EXAM::CUSNAM

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, ICPERIOD)

	!
	! Check Invoice Freight
	!
 Freight:
	GOTO Discount IF OE_CREDITJOUR::FREIGHT = 0.0

	!
	! Is Freight account number defined?
	!
	EXIT_STATUS = GL_EXAM_CHART(OE_ACCOUNT_READ::FRACCT, GL_CHART_EXAM)

	SELECT EXIT_STATUS

	!
	! Account number defined
	!
	CASE CMC$_NORMAL

	!
	! Number undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		TEXT$ = CONV_STRING(OE_CREDITJOUR::MEMONUM, CMC$_LEFT) + &
			" (Freight Acct)* " + &
			OE_ACCOUNT_READ::FRACCT

		!
		! Keep undefined codes
		!
		GOTO Aborted IF OUTP_UNDEFCODES(OPT_ADDREC, &
			TITLE(), UTL_REPORTX, TEXT$) <> CMC$_NORMAL

	!
	! Weird happenin's
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	GL_YYYY_PP::ACCT	= OE_ACCOUNT_READ::FRACCT
	GL_YYYY_PP::AMOUNT	= FUNC_ROUND(OE_CREDITJOUR::FREIGHT, 2%)
	GL_YYYY_PP::UNITS	= 0.0
	GL_YYYY_PP::DESCR	= AR_35CUSTOM_EXAM::CUSNAM

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, ICPERIOD)

	!
	! Check Invoice Discount
	!
 Discount:
	ORDER_DISC = OE_CREDITJOUR::DISC

	GOTO ProcessLines IF ORDER_DISC = 0.0

	!
	! Is Discount account number defined?
	!
	EXIT_STATUS = GL_EXAM_CHART(OE_ACCOUNT_READ::DISACCT, GL_CHART_EXAM)

	SELECT EXIT_STATUS

	!
	! Account number defined
	!
	CASE CMC$_NORMAL

	!
	! Number undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		TEXT$ = CONV_STRING(OE_CREDITJOUR::MEMONUM, CMC$_LEFT) + &
			" (Discount Acct)* " + &
			OE_ACCOUNT_READ::DISACCT

		!
		! Keep undefined codes
		!
		GOTO Aborted IF OUTP_UNDEFCODES(OPT_ADDREC, &
			TITLE(), UTL_REPORTX, TEXT$) <> CMC$_NORMAL

	!
	! Weird happenin's
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	GL_YYYY_PP::ACCT	= OE_ACCOUNT_READ::DISACCT
	GL_YYYY_PP::AMOUNT	= FUNC_ROUND(-ORDER_DISC, 2%)
	GL_YYYY_PP::UNITS	= 0.0
	GL_YYYY_PP::DESCR	= AR_35CUSTOM_EXAM::CUSNAM

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, ICPERIOD)

 ProcessLines:
	!
	! Find the first line item for the header
	!
	LTOTAL = 0.0

3100	WHEN ERROR IN
		FIND #OE_CREDITLINE.CH%, KEY #0% EQ OE_CREDITJOUR::MEMONUM
	USE
		!
		! Locked block
		!
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE ProcessHeader IF ERR = 155%
		FILENAME$ = "OE_CREDITLINE"
		CONTINUE HelpError
	END WHEN

 LineItem:
	!
	! Get the (next) line item
	!
3200	WHEN ERROR IN
		GET #OE_CREDITLINE.CH%
	USE
		!
		! Locked block
		!
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE ProcessHeader IF ERR = 11%
		FILENAME$ = "OE_CREDITLINE"
		CONTINUE HelpError
	END WHEN

	!
	! Finish up the header if we're done with the line items
	!
	GOTO ProcessHeader IF OE_CREDITLINE::MEMONUM <> OE_CREDITJOUR::MEMONUM

	!
	! Test product number
	!
	EXIT_STATUS = PD_EXAM_PRODUCT(OE_CREDITLINE::PRODUCT, PD_PRODUCT_EXAM)

	SELECT EXIT_STATUS

	!
	! Code found; go on
	!
	CASE CMC$_NORMAL

	!
	! Product number undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		TEXT$ = CONV_STRING(OE_CREDITJOUR::MEMONUM, CMC$_LEFT) + " " + &
			" (ProdNum)* " + &
			OE_CREDITLINE::PRODUCT

		!
		! Keep undefined codes
		!
		GOTO Aborted IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), &
			UTL_REPORTX, TEXT$) <> CMC$_NORMAL

	!
	! Something's going wrong
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	EXTPRICE = FUNC_ROUND(OE_CREDITLINE::CREDQTY * OE_CREDITLINE::PRICE, 2%)

	PROMO = FUNC_ROUND(OE_CREDITLINE::CREDQTY * OE_CREDITLINE::PROMO, 2%)

	DISCOUNT = FUNC_ROUND(OE_CREDITLINE::CREDQTY * &
		(OE_CREDITLINE::PRICE - OE_CREDITLINE::PROMO) * &
		OE_CREDITLINE::DISCOUNT/100, 2%)

	LTOTAL = LTOTAL + EXTPRICE - DISCOUNT - PROMO

	COST = FUNC_ROUND(OE_CREDITLINE::COST * OE_CREDITLINE::INVQTY, 2%)

	IF SORTBY$ <> ""
	THEN
		TEXT$ = CONV_STRING(OE_CREDITLINE::MEMONUM, CMC$_LEFT) + "   " + &
			OE_CREDITLINE::REASON + " " + &
			OE_CREDITLINE::PRODUCT + " " + &
			LEFT$(PD_PRODUCT_EXAM::DESCRIPTION, 20%) + " " + &
			FORMAT$(OE_CREDITLINE::COST, "#,###,###.###") + &
			FORMAT$(OE_CREDITLINE::CREDQTY, "###,##,#.##") + &
			FORMAT$(OE_CREDITLINE::INVQTY, "####,##,#.##") + " " + &
			FORMAT$(OE_CREDITLINE::PRICE, "#,###,###.###") + " " + &
			FORMAT$(OE_CREDITLINE::PROMO, "##,###.##") + " " + &
			FORMAT$(OE_CREDITLINE::DISCOUNT, "###.##") + "  " + &
			FORMAT$(EXTPRICE, "##,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	V% = OE_READ_REASONACCT(OE_CREDITLINE::REASON, &
		OE_CREDITJOUR::LOCATION, OE_REASONACCT_READ)

	EXIT_STATUS = GL_EXAM_CHART(OE_REASONACCT_READ::ACCOUNT, GL_CHART_EXAM)

	SELECT EXIT_STATUS

	!
	! Account number defined
	!
	CASE CMC$_NORMAL

	!
	! Number undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		TEXT$ = CONV_STRING(OE_CREDITJOUR::MEMONUM, CMC$_LEFT) + " " + &
			OE_CREDITLINE::PRODUCT + &
			" (Reason Acct)* " + &
			OE_REASONACCT_READ::ACCOUNT

		!
		! Keep undefined codes
		!
		GOTO Aborted IF OUTP_UNDEFCODES(OPT_ADDREC, &
			TITLE(), UTL_REPORTX, TEXT$) <> CMC$_NORMAL

	!
	! Weird happenin's
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	IC_TRANSACTION::PRODUCT		= OE_CREDITLINE::PRODUCT
	IC_TRANSACTION::LOCATION	= OE_CREDITJOUR::LOCATION
	IC_TRANSACTION::TRANS_DATE	= OE_CREDITJOUR::MEMODATE
	IC_TRANSACTION::PRIMARY_REF	= OE_CREDITLINE::MEMONUM
	IC_TRANSACTION::CROSS_REF	= OE_CREDITJOUR::CUSNUM
	IC_TRANSACTION::SUBACCOUNT	= OE_CREDITJOUR::SALESMAN
	IC_TRANSACTION::LOT		= ""
	IC_TRANSACTION::STATIONMAN	= OE_CREDITJOUR::OPERATOR
	IC_TRANSACTION::TRANSACCT	= OE_REASONACCT_READ::ACCOUNT
	IC_TRANSACTION::POSTDATE	= POSTDATE
	IC_TRANSACTION::POSTTIME	= POSTTIME
	IC_TRANSACTION::BATCH		= BATCH_NUMBER

	IC_TRANSACTION::COST = &
		FUNC_ROUND(OE_CREDITLINE::COST * OE_CREDITLINE::CREDQTY, 2%)

	IC_TRANSACTION::PRICE = &
		FUNC_ROUND(OE_CREDITLINE::PRICE * OE_CREDITLINE::CREDQTY, 2%)

	IC_TRANSACTION::TYPE_A		= OE_CREDITLINE::REASON
	IC_TRANSACTION::QUANTITY_A	= OE_CREDITLINE::CREDQTY
	IC_TRANSACTION::TYPE_B		= ""
	IC_TRANSACTION::QUANTITY_B	= 0.0

	!
	! Post shipping to inventory transaction file
	!
	EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

	!
	! Read inventory GL accounts
	!
	EXIT_STATUS = PD_READ_ACCOUNT(OE_CREDITJOUR::LOCATION, &
		PD_PRODUCT_EXAM::PROD_TYPE, PD_ACCOUNT_READ)

	GL_YYYY_PP::ACCT	= OE_REASONACCT_READ::ACCOUNT
	GL_YYYY_PP::AMOUNT	= FUNC_ROUND(EXTPRICE, 2%)
	GL_YYYY_PP::UNITS	= OE_CREDITLINE::CREDQTY

	GL_YYYY_PP::DESCR	= OE_CREDITLINE::PRODUCT + &
		OE_CREDITJOUR::LOCATION

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, ICPERIOD)

	!
	! Skip if nothing goes into inventory
	!
	GOTO ProdDisc IF OE_CREDITLINE::INVQTY = 0.0

	IC_TRANSACTION::TRANSACCT	= ""
	IC_TRANSACTION::COST		= &
		FUNC_ROUND(OE_CREDITLINE::COST * OE_CREDITLINE::INVQTY, 2%)

	IC_TRANSACTION::PRICE		= &
		FUNC_ROUND(OE_CREDITLINE::PRICE * OE_CREDITLINE::INVQTY, 2%)

	IC_TRANSACTION::TYPE_A		= "RT"
	IC_TRANSACTION::QUANTITY_A	= OE_CREDITLINE::INVQTY

	!
	! Post shipping to inventory transaction file
	!
	EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

	!
	! Is inventory account number defined?
	!
	EXIT_STATUS = GL_EXAM_CHART(PD_ACCOUNT_READ::INVACCT, GL_CHART_EXAM)

	SELECT EXIT_STATUS

	!
	! Account number defined
	!
	CASE CMC$_NORMAL

	!
	! Number undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		TEXT$ = CONV_STRING(OE_CREDITJOUR::MEMONUM, CMC$_LEFT) + " " + &
			OE_CREDITLINE::PRODUCT + &
			" (Inventory Acct)* " + &
			PD_ACCOUNT_READ::INVACCT

		!
		! Keep undefined codes
		!
		GOTO Aborted IF OUTP_UNDEFCODES(OPT_ADDREC, &
			TITLE(), UTL_REPORTX, TEXT$) <> CMC$_NORMAL

	!
	! Weird happenin's
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	GL_YYYY_PP::ACCT	= PD_ACCOUNT_READ::INVACCT
	GL_YYYY_PP::AMOUNT	= FUNC_ROUND(COST, 2%)
	GL_YYYY_PP::UNITS	= OE_CREDITLINE::INVQTY

	GL_YYYY_PP::DESCR	= OE_CREDITLINE::PRODUCT + &
		OE_CREDITJOUR::LOCATION

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, ICPERIOD)

	!
	! Is COS account number defined?
	!
	EXIT_STATUS = GL_EXAM_CHART(PD_ACCOUNT_READ::COSACCT, GL_CHART_EXAM)

	SELECT EXIT_STATUS

	!
	! Account number defined
	!
	CASE CMC$_NORMAL

	!
	! Number undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		TEXT$ = CONV_STRING(OE_CREDITJOUR::MEMONUM, CMC$_LEFT) + " " + &
			OE_CREDITLINE::PRODUCT + &
			" (Cost Acct)* " + &
			PD_ACCOUNT_READ::COSACCT

		!
		! Keep undefined codes
		!
		GOTO Aborted IF OUTP_UNDEFCODES(OPT_ADDREC, &
			TITLE(), UTL_REPORTX, TEXT$) <> CMC$_NORMAL

	!
	! Weird happenin's
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	GL_YYYY_PP::ACCT	= PD_ACCOUNT_READ::COSACCT
	GL_YYYY_PP::AMOUNT	= FUNC_ROUND(-COST, 2%)
	GL_YYYY_PP::UNITS	= -OE_CREDITLINE::INVQTY

	GL_YYYY_PP::DESCR	= OE_CREDITLINE::PRODUCT + &
		OE_CREDITJOUR::LOCATION

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, ICPERIOD)

 ProdDisc:
	!
	! Check the Discount
	!
	IF DISCOUNT <> 0.0
	THEN
		!
		! Is discount account number defined?
		!
		EXIT_STATUS = GL_EXAM_CHART(PD_ACCOUNT_READ::DISCACCT, &
			GL_CHART_EXAM)

		SELECT EXIT_STATUS

		!
		! Account number defined
		!
		CASE CMC$_NORMAL

		!
		! Undefined Account Number
		!
		CASE CMC$_UNDEFINED
			TEXT$ = CONV_STRING(OE_CREDITJOUR::MEMONUM, CMC$_LEFT) + " " + &
				OE_CREDITLINE::PRODUCT + &
				" (Discount Acct)* " + &
				PD_ACCOUNT_READ::DISCACCT

			!
			! Keep undefined codes
			!
			GOTO Aborted IF OUTP_UNDEFCODES(OPT_ADDREC, &
				TITLE(), UTL_REPORTX, TEXT$) <> CMC$_NORMAL

		!
		! Weird happenin's
		!
		CASE ELSE
			GOTO Aborted

		END SELECT

		GL_YYYY_PP::ACCT	= PD_ACCOUNT_READ::DISCACCT
		GL_YYYY_PP::AMOUNT	= FUNC_ROUND(-DISCOUNT, 2%)
		GL_YYYY_PP::UNITS	= -OE_CREDITLINE::CREDQTY

		GL_YYYY_PP::DESCR	= OE_CREDITLINE::PRODUCT + &
			OE_CREDITJOUR::LOCATION

		EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, GL_YYYY_PP, &
			GL_CHART_EXAM, ICPERIOD)

	END IF

	!
	! Check Product Promo
	!
	IF PROMO <> 0.0
	THEN
		V% = OE_READ_PROMO(OE_CREDITLINE::PRODUCT, &
			OE_CREDITJOUR::MEMODATE, &
			OE_CREDITJOUR::CUSNUM, OE_PROMO_READ, 0.0, 0.0)

		!
		! Is promo account number defined?
		!
		EXIT_STATUS = GL_EXAM_CHART(OE_PROMO_READ::ACCOUNT, &
			GL_CHART_EXAM)

		SELECT EXIT_STATUS

		!
		! Account number defined
		!
		CASE CMC$_NORMAL

		!
		! Number undefined; set flag and go on
		!
		CASE CMC$_UNDEFINED
			TEXT$ = CONV_STRING(OE_CREDITJOUR::MEMONUM, CMC$_LEFT) + " " + &
				OE_CREDITLINE::PRODUCT + &
				" (Promo Acct)* " + &
				OE_PROMO_READ::ACCOUNT

			!
			! Keep undefined codes
			!
			GOTO Aborted IF OUTP_UNDEFCODES(OPT_ADDREC, &
				TITLE(), UTL_REPORTX, TEXT$) <> CMC$_NORMAL

		!
		! Weird happenin's
		!
		CASE ELSE
			GOTO Aborted

		END SELECT

		GL_YYYY_PP::ACCT	= OE_PROMO_READ::ACCOUNT
		GL_YYYY_PP::AMOUNT	= FUNC_ROUND(-PROMO, 2%)
		GL_YYYY_PP::UNITS	= -OE_CREDITLINE::CREDQTY

		GL_YYYY_PP::DESCR	= OE_CREDITLINE::PRODUCT + &
			OE_CREDITJOUR::LOCATION

		EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, GL_YYYY_PP, &
			GL_CHART_EXAM, ICPERIOD)

	END IF

	GOTO LineItem

 ProcessHeader:
	!
	! Check Invoice Sales Tax
	!
	GOTO OrderTotal IF OE_CREDITJOUR::SALESTAX = 0.0

	V% = OE_READ_SALESTAX(AR_35CUSTOM_EXAM::TAXCODE, &
		"1", OE_SALESTAX_READ)

	GOTO CityTax IF OE_SALESTAX_READ::STATETAX = 0.0

	STATETAX = FUNC_ROUND(LTOTAL * OE_SALESTAX_READ::STATETAX / 100.0, 2%)

	GOTO CityTax IF STATETAX = 0.0

	!
	! Is Sales Tax State account number defined?
	!
	EXIT_STATUS = GL_EXAM_CHART(OE_SALESTAX_READ::STATEACC, GL_CHART_EXAM)

	SELECT EXIT_STATUS

	!
	! Account number defined
	!
	CASE CMC$_NORMAL

	!
	! Number undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		TEXT$ = CONV_STRING(OE_CREDITJOUR::MEMONUM, CMC$_LEFT) + &
			" (State Tax Acct)* " + &
			OE_SALESTAX_READ::STATEACC

		!
		! Keep undefined codes
		!
		GOTO Aborted IF OUTP_UNDEFCODES(OPT_ADDREC, &
			TITLE(), UTL_REPORTX, TEXT$) <> CMC$_NORMAL

	!
	! Weird happenin's
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	GL_YYYY_PP::ACCT	= OE_SALESTAX_READ::STATEACC
	GL_YYYY_PP::AMOUNT	= FUNC_ROUND(STATETAX, 2%)
	GL_YYYY_PP::UNITS	= 0.0
	GL_YYYY_PP::DESCR	= AR_35CUSTOM_EXAM::CUSNAM

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, ICPERIOD)

 CityTax:
	GOTO CountyTax IF OE_SALESTAX_READ::CITYTAX = 0.0

	CITYTAX = FUNC_ROUND(LTOTAL * OE_SALESTAX_READ::CITYTAX / 100.0, 2%)

	GOTO CountyTax IF CITYTAX = 0.0

	!
	! Is City Tax account number defined?
	!
	EXIT_STATUS = GL_EXAM_CHART(OE_SALESTAX_READ::CITYACC, GL_CHART_EXAM)

	SELECT EXIT_STATUS

	!
	! Account number defined
	!
	CASE CMC$_NORMAL

	!
	! Number undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		TEXT$ = CONV_STRING(OE_CREDITJOUR::MEMONUM, CMC$_LEFT) + &
			" (City Tax Acct)* " + &
			OE_SALESTAX_READ::CITYACC

		!
		! Keep undefined codes
		!
		GOTO Aborted IF OUTP_UNDEFCODES(OPT_ADDREC, &
			TITLE(), UTL_REPORTX, TEXT$) <> CMC$_NORMAL

	!
	! Weird happenin's
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	GL_YYYY_PP::ACCT	= OE_SALESTAX_READ::CITYACC
	GL_YYYY_PP::AMOUNT	= FUNC_ROUND(CITYTAX, 2%)
	GL_YYYY_PP::UNITS	= 0.0
	GL_YYYY_PP::DESCR	= AR_35CUSTOM_EXAM::CUSNAM

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, ICPERIOD)

 CountyTax:
	GOTO OrderTotal IF OE_SALESTAX_READ::COUNTYTAX = 0.0

	COUNTYTAX = FUNC_ROUND(LTOTAL * OE_SALESTAX_READ::COUNTYTAX / 100.0, 2%)

	GOTO OrderTotal IF COUNTYTAX = 0.0

	!
	! Is County Tax account number defined?
	!
	EXIT_STATUS = GL_EXAM_CHART(OE_SALESTAX_READ::COUNTYACC, GL_CHART_EXAM)

	SELECT EXIT_STATUS

	!
	! Account number defined
	!
	CASE CMC$_NORMAL

	!
	! Number undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		TEXT$ = CONV_STRING(OE_CREDITJOUR::MEMONUM, CMC$_LEFT) + &
			" (County Tax Acct)* " + &
			OE_SALESTAX_READ::COUNTYACC

		!
		! Keep undefined codes
		!
		GOTO Aborted IF OUTP_UNDEFCODES(OPT_ADDREC, &
			TITLE(), UTL_REPORTX, TEXT$) <> CMC$_NORMAL

	!
	! Weird happenin's
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	GL_YYYY_PP::ACCT	= OE_SALESTAX_READ::COUNTYACC
	GL_YYYY_PP::AMOUNT	= FUNC_ROUND(COUNTYTAX, 2%)
	GL_YYYY_PP::UNITS	= 0.0
	GL_YYYY_PP::DESCR	= AR_35CUSTOM_EXAM::CUSNAM

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, ICPERIOD)

 OrderTotal:
	TAX_TOT = FUNC_ROUND(LTOTAL * OE_CREDITJOUR::SALESTAX / 100.0, 2%)

	ORDER_TOT = LTOTAL + OE_CREDITJOUR::FREIGHT + OE_CREDITJOUR::MISC + &
		OE_CREDITJOUR::HANDLING + TAX_TOT - ORDER_DISC

	EXIT_STATUS = GL_EXAM_CHART(OE_ACCOUNT_READ::ACCOUNT, GL_CHART_EXAM)

	SELECT EXIT_STATUS

	!
	! Account number defined
	!
	CASE CMC$_NORMAL

	!
	! Number undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		TEXT$ = CONV_STRING(OE_CREDITJOUR::MEMONUM, CMC$_LEFT) + &
			" (Order Acct)* " + &
			OE_ACCOUNT_READ::ACCOUNT

		!
		! Keep undefined codes
		!
		GOTO Aborted IF OUTP_UNDEFCODES(OPT_ADDREC, &
			TITLE(), UTL_REPORTX, TEXT$) <> CMC$_NORMAL

	!
	! Weird happenin's
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	GL_YYYY_PP::ACCT	= OE_ACCOUNT_READ::ACCOUNT
	GL_YYYY_PP::AMOUNT	= FUNC_ROUND(-ORDER_TOT, 2%)
	GL_YYYY_PP::UNITS	= 0.0
	GL_YYYY_PP::DESCR	= GL_CHART_EXAM::DESCR

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, ICPERIOD)

	!
	! Generate a AR_OPEN_DIST record to pass through to the
	! post function
	!
	AR_OPEN_DIST::INVNUM	= OE_CREDITJOUR::MEMONUM
	AR_OPEN_DIST::CUSNUM	= OE_CREDITJOUR::CUSNUM
	AR_OPEN_DIST::SLINE	= ""
	AR_OPEN_DIST::ACCT	= ""
	AR_OPEN_DIST::SUBACCT	= OE_CREDITJOUR::SALESMAN
	AR_OPEN_DIST::AMOUNT	= ORDER_TOT
	AR_OPEN_DIST::QTY	= 0.0
	AR_OPEN_DIST::LTYPE	= "S"
	AR_OPEN_DIST::TAXTYP	= "1"
	AR_OPEN_DIST::DESCR	= ""
	AR_OPEN_DIST::BATCH	= BATCH_NUMBER
	AR_OPEN_DIST::STAFF_NUM	= ""
	AR_OPEN_DIST::POST_DATE	= POSTDATE
	AR_OPEN_DIST::POST_TIME	= POSTTIME

	!
	! Call the post function
	!
	GOTO Aborted IF AR_TRAN_POSTAR(OPT_ADDREC, SUBOPT_LINEITEM, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		AR_OPEN, AR_OPEN_DIST, AR_SALTAXLED, ICPERIOD) <> CMC$_NORMAL

	!
	! Post to sales tax file if correct type
	!
	IF OE_CREDITJOUR::SALESTAX <> 0.0
	THEN
		AR_SALTAXLED::TAXTYP = "1"
		AR_SALTAXLED::CUSNUM = OE_CREDITJOUR::CUSNUM
		AR_SALTAXLED::INVNUM = OE_CREDITJOUR::MEMONUM
		AR_SALTAXLED::AMOUNT = ORDER_TOT
		AR_SALTAXLED::BATCH  = BATCH_NUMBER
		AR_SALTAXLED::TRADAT = OE_CREDITJOUR::MEMODATE

		GOTO Aborted IF AR_TRAN_POSTAR(OPT_ADDREC, SUBOPT_LEDGER, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, AR_OPEN, &
			AR_OPEN_DIST, AR_SALTAXLED, ICPERIOD) <> CMC$_NORMAL
	END IF

	!
	! Generate a AR record to pass through to the post function
	!
	AR_OPEN::CUSNUM		= OE_CREDITJOUR::CUSNUM
	AR_OPEN::INVNUM		= OE_CREDITJOUR::MEMONUM
	AR_OPEN::TRATYP		= "08"
	AR_OPEN::TRADAT		= OE_CREDITJOUR::MEMODATE
	AR_OPEN::SALAMT		= -ORDER_TOT
	AR_OPEN::DISAMT		= 0.0
	AR_OPEN::OTHCHG		= 0.0
	AR_OPEN::RECNUM		= ""
	AR_OPEN::CHKNUM		= ""
	AR_OPEN::ARACCT		= OE_ACCOUNT_READ::ACCOUNT
	AR_OPEN::SUBACC		= ""
	AR_OPEN::SALNUM		= OE_CREDITJOUR::SALESMAN
	AR_OPEN::DESCR		= LEFT(OE_CREDITJOUR::NOTES(0%), 25%)
	AR_OPEN::BATCH		= BATCH_NUMBER
	AR_OPEN::UPDATED	= ICPERIOD + "00"
	AR_OPEN::CLOSEDATE	= ""
	AR_OPEN::DUEDATE	= ""
	AR_OPEN::DISCOUNTDATE	= ""

	!
	! Call the post function
	!
	GOTO Aborted IF AR_TRAN_POSTAR(OPT_ADDREC, SUBOPT_REGISTER, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		AR_OPEN, AR_OPEN_DIST, AR_SALTAXLED, ICPERIOD) <> CMC$_NORMAL

	IF SORTBY$ <> ""
	THEN
		TEXT$ = CONV_STRING(OE_CREDITJOUR::MEMONUM, CMC$_LEFT) + "      "    + &
			FORMAT$(OE_CREDITJOUR::DISC, " ##,###.##") + "     " + &
			FORMAT$(TAX_TOT, "##,###.##") + "     " + &
			FORMAT$(OE_CREDITJOUR::HANDLING, "##,###.##") + "     "     + &
			FORMAT$(OE_CREDITJOUR::FREIGHT, "##,###.##") + "     "     + &
			FORMAT$(OE_CREDITJOUR::MISC, "##,###.##") + "  " + &
			OE_CREDITJOUR::MISCACCT + SPACE$(19%) + &
			FORMAT$(ORDER_TOT, "##,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	END IF

	GOTO ReadHeader

 Confirm:
	!******************************************************************
	! Confirm posting
	!******************************************************************

	!
	! Almost done if just report
	!
	!IF SORTBY$ <> ""
	!THEN
	!	GOSUB SubTotal IF CTR%
	!END IF

	EXIT_STATUS = IC_TRAN_POST(OPT_CONFIRM, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "", ICPERIOD)

	EXIT_STATUS = EXIT_STATUS AND GL_TRAN_POSTGL(OPT_CONFIRM, &
		SUBOPT_NOOPT, BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		"", "", ICPERIOD)

	EXIT_STATUS = EXIT_STATUS AND OUTP_UNDEFCODES(OPT_CONFIRM, TITLE(), &
		UTL_REPORTX, "")

	!
	! Almost done if just report
	!
	IF SORTBY$ <> ""
	THEN
		PRNT_SUMMARY = SUBOPT_FINAL
		GOTO ExitProgram
	END IF

	GOTO Aborted IF EXIT_STATUS <> CMC$_NORMAL

	%PAGE

	!******************************************************************
	!	1) Since everything's correct and we have the user's
	!		permission, transfer all data from the temporary
	!		files into the ledger
	!******************************************************************
	!
	! Begin posting
	!

	! Post to the inventory ledger
	!
	GOTO Interrupt IF IC_TRAN_POST(OPT_POSTFILE, SUBOPT_LEDGER, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "", ICPERIOD) <> CMC$_NORMAL

	GOTO Interrupt IF GL_TRAN_POSTGL(OPT_POSTFILE, SUBOPT_DETAIL, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "", "", &
		ICPERIOD) <> CMC$_NORMAL

	!
	! Post the AR Sales Journal header
	!
	GOTO Interrupt IF AR_TRAN_POSTAR(OPT_POSTFILE, SUBOPT_REGISTER, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "", "", "", &
		ICPERIOD) <> CMC$_NORMAL

	!
	! Continue by posting the Sales Journal line items
	!
	GOTO Interrupt IF AR_TRAN_POSTAR(OPT_POSTFILE, SUBOPT_LINEITEM, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "", "", "", &
		ICPERIOD) <> CMC$_NORMAL

	!
	! Post any Sales Tax items
	!
	GOTO Interrupt IF AR_TRAN_POSTAR(OPT_POSTFILE, SUBOPT_LEDGER, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "", "", "", &
		ICPERIOD) <> CMC$_NORMAL

	%PAGE

	!
	! Remove files
	!
5000	CLOSE OE_CREDITJOUR.CH%
	CLOSE OE_CREDITLINE.CH%

5010 !	WHEN ERROR IN
 !		KILL OE_CREDITJOUR.DEV$ + "OE_CREDITJOUR_" + BATCH_NO$ + ".JRL" &
 !			FOR I% = 1% TO 10%
 !	USE
 !		CONTINUE 5020
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(OE_CREDITJOUR.DEV$ + &
		"OE_CREDITJOUR_" + BATCH_NO$ + ".JRL;*")

5020 !	WHEN ERROR IN
 !		KILL OE_CREDITLINE.DEV$ + "OE_CREDITLINE_" + BATCH_NO$ + ".JRL" &
 !			FOR I% = 1% TO 10%
 !	USE
 !		CONTINUE Complete
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(OE_CREDITLINE.DEV$ + &
		"OE_CREDITLINE_" + BATCH_NO$ + ".JRL;*")

 Complete:
	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "OE_CREDITJOUR", BATCH_NO$, "", "")

	PRNT_SUMMARY = SUBOPT_FINAL

 ExitProgram:
	!******************************************************************
	! Exit normally
	!******************************************************************
	!
	! Print credit and debit transmittals
	!
	EXIT_STATUS = GL_TRAN_POSTGL(OPT_SUMMARY, PRNT_SUMMARY, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", ICPERIOD)

	!
	! Print undefined codes (if any)
	!
	EXIT_STATUS = OUTP_UNDEFCODES(OPT_SUMMARY, TITLE(), UTL_REPORTX, TEXT)

	!
	! Finish up the transmittal
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

 Aborted:
	!******************************************************************
	! Abort process
	!******************************************************************
	TEXT$ = OE_CREDITJOUR::MEMONUM + " " + OE_CREDITLINE::PRODUCT

	EXIT_STATUS = OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), UTL_REPORTX, TEXT$)

	IF INTR_STATUS <> CMC$_WARNING
	THEN
		EXIT_STATUS = ASSG_POSTBATCH(OPT_ABORT,	BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "OE_CREDITJOUR", &
			BATCH_NO$, "", "")

		GOTO ExitProgram
	END IF

 Interrupt:
	!******************************************************************
	! Interrupt process
	!******************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_INTERRUPT, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "OE_CREDITJOUR", BATCH_NO$, "", "")

	GOTO ExitProgram

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_PRINTMESS(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR), UTL_REPORTX, TITLE(), 0%)

	!UTL_REPORTX::STAT = -1%
	GOTO Aborted

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	!
	! Trap untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32000	!******************************************************************
	! End of posting program OE_POST_CREDIT
	!******************************************************************
	END
	!+-+-+
	!++
	! Abstract:FLD03
	!	.X Post>General Ledger Period
	!	^*(03) General Ledger Period\*
	!	.b
	!	.lm +5
	!	The ^*General Ledger Period\* field enters the
	!	General Ledger accounting period into which this batch
	!	will be posted.
	!	.B
	!	The format for entry is YYYYPP.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.X General Ledger Period>Post
	!	.X Post>Period
	!	.X Period>Post
	!
	!--
