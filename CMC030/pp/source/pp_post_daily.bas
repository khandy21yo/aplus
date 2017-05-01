1	%TITLE "Post Pacific Pride Daily Transaction Journal"
	%SBTTL "PP_POST_DAILY"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1993 BY
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
	! ID:???
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Post Daily Transaction Journal\* process transfers
	!	information from the Order Entry Journal to the Order Entry Register
	!	and allocate the
	!	inventory by posting to the Inventory transaction file.
	!	.b
	!	After the posting is completed, the system will return to the Order Entry
	!	Journal menu screen.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PP_SOURCE:PP_POST_DAILY/LINE
	!	$ LINK/EXE=PP_EXE: PP_POST_DAILY, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PP_POST_DAILY.OBJ;*
	!
	! Author:
	!
	!	01/13/93 - Dan Perkins
	!
	! Modification history:
	!
	!	03/03/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/08/93 - Kevin Handy
	!		Read invoice number from comtrol file instead of
	!		asking the user for it.
	!
	!	04/13/93 - Kevin Handy
	!		Wasn't posting transaction date properly.
	!
	!	04/13/93 - Kevin Handy
	!		On foreign sales and foreign purchases, it was
	!		posting price instead of (price * quanity).
	!
	!	04/15/93 - Kevin Handy
	!		Modified so foreign sales post transfer cost
	!		instead of sell price.
	!
	!	04/15/93 - Kevin Handy
	!		Modified to put card numbers in the line item
	!		descriptions.
	!
	!	04/23/93 - Kevin Handy
	!		Modified to post Foreign sales transfer costs to
	!		6??.?? and 130.00.
	!
	!	04/23/93 - Kevin Handy
	!		Modified to send customer number instead of
	!		system number for foreign purchases.
	!
	!	04/26/93 - Kevin Handy
	!		Modified so that it actually updates last invoice
	!		number in the control file.
	!
	!	05/17/93 - Kevin Handy
	!		Modified foreign purchases to include taxes in
	!		the header/1st line.
	!
	!	 05/20/93 - Kevin Handy
	!		Modified to post foreign purchase transfer cost to
	!		PP customer instead of to regular customer (oops).
	!
	!	06/11/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	09/28/93 - Kevin Handy
	!		Modified so the foreign sale transfer cost goes
	!		to 6??.?? instead of 5??.??
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	10/21/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/30/97 - Kevin Handy
	!		Lose commented out code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/01/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	09/18/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!--

	!
	! Comment stolen from RSTS/E version.
	!
	!==============================================================
	! Local Sale
	!
	! Debit		AR	130.00	$10.00	Selling Price
	! Credit	Sales	5??.??	$10.00	Selling Price
	!
	! If Lia Account Exists
	! then
	!	Debit	COS	6??.??	$ 1.60($.16/gal)Tax Amount
	!	Credit	LiaAcct	???.??	$ 1.60		Tax Amount
	!
	!==============================================================
	! Foreign Sale
	!
	! Debit		IFPPSI  130.00  $ 8.40  Transfer Cost
	! Credit	Sales   5??.??  $ 8.40  Transfer Cost
	!
	!==============================================================
	! Foreign Purchase
	!
	! Debit AR	130.00  $10.00(10gal)   Selling Price(incl tax)
	! Credit	Sales   5??.??  $10.00  Selling Price(incl tax)
	!
	! Debit		COS     6??.??  $ 8.00  Transfer Cost
	! Credit	IFPPSI  130.00  $ 8.00  Transfer Cost
	!
	! Credit	LiaAcct ???.??  $ 1.60($.16/gal)Tax Amount
	! Debit		COS     6??.??  $ 1.60          Tax Amount
	!
	!==============================================================
	!

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
	%INCLUDE "SOURCE:[PP.OPEN]PP_DAILY.HB"
	MAP (PP_DAILY)		PP_DAILY_CDD		PP_DAILY

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD		AR_CONTROL

	%INCLUDE "SOURCE:[PP.OPEN]PP_CONTROL.HB"
	MAP (PP_CONTROL)	PP_CONTROL_CDD		PP_CONTROL

	%INCLUDE "SOURCE:[PP.OPEN]PP_SITE.HB"
	MAP (PP_SITE)		PP_SITE_CDD		PP_SITE

	%INCLUDE "SOURCE:[PP.OPEN]PP_SITE_PRODUCT.HB"
	MAP (PP_SITE_PRODUCT)	PP_SITE_PRODUCT_CDD	PP_SITE_PRODUCT

	%INCLUDE "SOURCE:[PP.OPEN]PP_CARDEXEMPT.HB"
	MAP (PP_CARDEXEMPT)	PP_CARDEXEMPT_CDD	PP_CARDEXEMPT

	RECORD SUBTOTAL_CDD
		STRING	BUYFRAN = 3%	! Location
		STRING	PRODUCT = 14%	! Product
		LONG	COUNTER		! Number of lines
		REAL	QUANTITY	! Number of gallons
		REAL	PRICE		! Total paid
	END RECORD

	DIM SUBTOTAL_CDD SUBTOTAL(300%)

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	%INCLUDE "SOURCE:[PP.OPEN]PP_MONTHLY.HB"
	%INCLUDE "SOURCE:[AR.OPEN]AR_SJH.HB"
	%INCLUDE "SOURCE:[AR.OPEN]AR_SJL.HB"
	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSACCT.HB"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	AR_EXAM_CUSTOM
	EXTERNAL LONG	FUNCTION	ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION	OUTP_UNDEFCODES
	EXTERNAL LONG	FUNCTION	PD_EXAM_PRODUCT
	EXTERNAL LONG	FUNCTION	PP_TRAN_POST
	EXTERNAL LONG	FUNCTION	AR_TRAN_POSTSJ

	!
	! Declare internal variables
	!
	DECLARE	UTL_REPORTX_CDD		UTL_REPORTX
	DECLARE PP_MONTHLY_CDD		PP_MONTHLY
	DECLARE AR_SJH_CDD		AR_SJH
	DECLARE AR_SJL_CDD		AR_SJL
	DECLARE AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM
	DECLARE GL_CHART_CDD		GL_CHART_EXAM
	DECLARE PD_PRODUCT_CDD		PD_PRODUCT_EXAM
	DECLARE	UTL_TRANSACCT_CDD	UTL_TRANSACCT_READ

	DECLARE	LONG	EXIT_STATUS
	DECLARE	LONG	INTR_STATUS

	DECLARE	STRING	PP.INTER.PERIOD
	DECLARE	STRING	BATCH_NUMBER
	DECLARE	STRING	POSTDATE
	DECLARE	STRING	POSTTIME
	DECLARE	STRING	TEXT
	DECLARE	STRING	TITLE(10%)

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
	TITLE(1%) = "DAILY  TRANSACTION  JOURNAL  POSTING  PROTOCOL"
	TITLE(2%) = "Pacific Pride System"
	TITLE(3%) = ""
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

	BATCH_NO$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(0%))
	YYYY_PP$ = LEFT(BATCH_NO$, 6%)

	!++
	! Abstract:FLD01
	!	^*(01) Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field enters the
	!	batch to be posted.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	%PAGE

200	!
	! Open AR control file and grab record
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.OPN"

		GET #AR_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE #AR_CONTROL.CH%
		CALL ASSG_FREECHANNEL(AR_CONTROL.CH%)
	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

203	!
	! Open AR control file and grab record
	!
	%INCLUDE "SOURCE:[PP.OPEN]PP_CONTROL.MOD"

	GET #PP_CONTROL.CH%, RECORD 1%

	INVOICE_NUMBER$ = PP_CONTROL::LAST_INV

205	!
	! Open site file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PP.OPEN]PP_SITE.OPN"
	USE
		FILENAME$ = "PP_SITE"
		CONTINUE HelpError
	END WHEN

210	!
	! Open site product file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PP.OPEN]PP_SITE_PRODUCT.OPN"
	USE
		FILENAME$ = "PP_SITE_PRODUCT"
		CONTINUE HelpError
	END WHEN

220	!
	! Open Card Exception file
	!
	%INCLUDE "SOURCE:[PP.OPEN]PP_CARDEXEMPT.OPN"

	%PAGE

300	!
	! Open Daily Transaction file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PP.OPEN]PP_DAILY.UPD"
	USE
		FILENAME$ = "PP_DAILY"
		CONTINUE HelpError
	END WHEN

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
		UTL_REPORTX, "PP_DAILY", BATCH_NO$, PP.INTER.PERIOD, "")

	SELECT INTR_STATUS
	!
	! Success; keep going
	!
	CASE CMC$_NORMAL

	!
	! Process was interrupted
	!
	CASE CMC$_WARNING

		IF TRM$(PP.INTER.PERIOD) <> ""
		THEN
			GOTO Aborted IF PP_TRAN_POST(OPT_RESTART, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				"", YYYY_PP$) <> CMC$_NORMAL
		END IF

		IF TRM$(PP.INTER.PERIOD) <> ""
		THEN
			GOTO Aborted IF AR_TRAN_POSTSJ(OPT_RESTART, &
				SUBOPT_REGISTER, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				AR_SJH, AR_SJL, "PP") <> CMC$_NORMAL
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
		UTL_REPORTX, "PP_DAILY", BATCH_NO$, "", "") <> CMC$_NORMAL

	EXIT_STATUS = PP_TRAN_POST(OPT_CHECK, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "", YYYY_PP$)

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

	EXIT_STATUS = AR_TRAN_POSTSJ(OPT_CHECK, SUBOPT_REGISTER, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, AR_SJH, AR_SJL, "PP")

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

	!******************************************************************
	!	1) List the debits/credits transmittal for the user while
	!		also putting the journal data in temporary files
	!	2) If confirmation, then go on
	!******************************************************************

	!******************************************************************
	! Create transmittal
	!******************************************************************
	GOTO Aborted IF ASSG_POSTBATCH(OPT_ADDREC, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "PP_DAILY", BATCH_NO$, "", "") <> CMC$_NORMAL

	RESET #PP_DAILY.CH%

	POSTDATE = DATE_TODAY
	POSTTIME = TIME_NOW

	!
	! Blank flags
	!
	CUSTNUM$, PRODUCT$ = " "

 ReadHeader:
	!
	! Read in one record from the header file
	!
3000	WHEN ERROR IN
		GET #PP_DAILY.CH%
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
	! Subtotal by location
	!
	SUBBUYFRAN$ = PP_DAILY::SLTYPE + "  "
	GOSUB AddSubtotal

	!
	! Grand Subtotal
	!
	SUBBUYFRAN$ = "~~~"
	GOSUB AddSubtotal

	!
	! Get the customer description
	!
	EXIT_STATUS = AR_EXAM_CUSTOM(PP_DAILY::CUSNUM, AR_35CUSTOM_EXAM)

	SELECT EXIT_STATUS
	!
	! Success; go on
	!
	CASE CMC$_NORMAL

	!
	! Undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		CUSTNUM$ = "*"

	!
	! Untrapped error
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! Check Product
	!
	EXIT_STATUS = PD_EXAM_PRODUCT(PP_DAILY::PRODUCT, PD_PRODUCT_EXAM)

	SELECT EXIT_STATUS
	!
	! Success; go on
	!
	CASE CMC$_NORMAL

	!
	! Undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		PRODUCT$ = "*"

	!
	! Untrapped error
	!
	CASE ELSE
		TEXT$ = "Error in Product Number " + PP_DAILY::PRODUCT

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		PRODUCT$ = "*"
		GOTO Aborted

	END SELECT

	!
	! Check for undefined codes
	!
	IF INSTR(1%, PRODUCT$, "*")
	THEN
		TEXT$ = CUSTNUM$ + PP_DAILY::CUSNUM + " " + &
			PRODUCT$ + PP_DAILY::PRODUCT

		!
		! Keep undefined codes
		!
		GOTO Aborted IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), &
			UTL_REPORTX, TEXT$) <> CMC$_NORMAL

		!
		! Blank flags
		!
		PRODUCT$ = " "

	END IF

	!
	! Generate a PP_MONTHLY record to pass through to the
	! post function
	!
	PP_MONTHLY::CUSNUM	= PP_DAILY::CUSNUM
	PP_MONTHLY::VEHICLE	= PP_DAILY::VEHICLE
	PP_MONTHLY::DRIVER	= PP_DAILY::DRIVER
	PP_MONTHLY::TRANDATE	= PP_DAILY::TRANDATE
	PP_MONTHLY::TRANTIME	= PP_DAILY::TRANTIME
	PP_MONTHLY::HOST	= PP_DAILY::HOST
	PP_MONTHLY::SITE	= PP_DAILY::SITE
	PP_MONTHLY::STYPE	= PP_DAILY::STYPE
	PP_MONTHLY::PRODUCT	= PP_DAILY::PRODUCT
	PP_MONTHLY::UOM		= PP_DAILY::UOM
	PP_MONTHLY::QUANTITY	= PP_DAILY::QUANTITY
	PP_MONTHLY::ODOM	= PP_DAILY::ODOM
	PP_MONTHLY::SLTYPE	= PP_DAILY::SLTYPE
	PP_MONTHLY::SELLPRICE	= PP_DAILY::SELLPRICE
	PP_MONTHLY::TRANCOST	= PP_DAILY::TRANCOST
	PP_MONTHLY::MISCKEYB	= PP_DAILY::MISCKEYB
	PP_MONTHLY::TRNTYPE	= PP_DAILY::TRNTYPE
	PP_MONTHLY::DISCOUNT	= PP_DAILY::DISCOUNT
	PP_MONTHLY::ICBDATE	= PP_DAILY::ICBDATE
	PP_MONTHLY::TRNNUM	= PP_DAILY::TRNNUM
	PP_MONTHLY::STAXRATE	= PP_DAILY::STAXRATE
	PP_MONTHLY::PUMP	= PP_DAILY::PUMP
	PP_MONTHLY::BUYFRAN	= PP_DAILY::BUYFRAN
	PP_MONTHLY::CAPDATE	= PP_DAILY::CAPDATE
	PP_MONTHLY::CAPTIME	= PP_DAILY::CAPTIME
	PP_MONTHLY::POSTBNUM	= PP_DAILY::POSTBNUM
	PP_MONTHLY::TRANSOURCE	= PP_DAILY::TRANSOURCE
	PP_MONTHLY::EDITACT	= PP_DAILY::EDITACT
	PP_MONTHLY::JULIANDAY	= PP_DAILY::JULIANDAY
	PP_MONTHLY::RSTATION	= PP_DAILY::RSTATION
	PP_MONTHLY::STATE	= PP_DAILY::STATE
	PP_MONTHLY::BATCH	= BATCH_NUMBER
	PP_MONTHLY::IDENTITY	= PP_DAILY::IDENTITY

	!
	! Call the post function
	!
	GOTO Aborted IF PP_TRAN_POST(OPT_ADDREC, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		PP_MONTHLY, YYYY_PP$) <> CMC$_NORMAL

	!
	! Handle SJ File
	!
	SELECT PP_DAILY::SLTYPE

	CASE "F"
		GOSUB ForeignSale

	CASE "P"
		GOSUB ForeignPurchase

	CASE ELSE
		GOSUB LocalSale

	END SELECT

3200	!
	! Check for undefined codes
	!
	IF INSTR(1%, CUSTNUM$, "*")
	THEN
		TEXT$ = CUSTNUM$ + PP_DAILY::CUSNUM

		!
		! Keep undefined codes
		!
		GOTO Aborted IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), &
			UTL_REPORTX, TEXT$) <> CMC$_NORMAL

		!
		! Blank flags
		!
		CUSTNUM$ = " "

	END IF

	GOTO ReadHeader

	%PAGE

 LocalSale:
	!*******************************************************************
	! Post  SJH records
	!*******************************************************************

3400	!
	! Get record from site file
	!
	LOCA$ = ""

	WHEN ERROR IN
		GET #PP_SITE.CH%, &
			KEY #0% EQ PP_DAILY::HOST + PP_DAILY::SITE + PP_DAILY::STYPE, &
			REGARDLESS
	USE
		CONTINUE BadSite
	END WHEN

	LOCA$ = PP_SITE::LOCSALE

	!
	! Get site product tax data
	!
3410	WHEN ERROR IN
		GET #PP_SITE_PRODUCT.CH%, &
			KEY #0% EQ PP_DAILY::HOST + PP_DAILY::SITE + PP_DAILY::STYPE + &
				PP_DAILY::PRODUCT, &
			REGARDLESS
	USE
		CONTINUE BadSiteProduct
	END WHEN

3420	!
	! Handle Any Liability amounts
	!
	GOSUB LiabilityCalc

	!
	! Create Header Record
	!
	AR_SJH::INVNUM		= INVOICE_NUMBER$
	AR_SJH::CUSNUM		= PP_DAILY::CUSNUM
	AR_SJH::TRATYP		= "01"
	AR_SJH::TRADAT		= BATCH_NO$
	AR_SJH::AMOUNT		= FUNC_ROUND(SELLPRICE * PP_DAILY::QUANTITY, 2%)
	AR_SJH::ARACCT		= AR_CONTROL::AR_ACCT
	AR_SJH::RECNUM		= ""
	AR_SJH::CHECK		= ""
	AR_SJH::DEPOSIT		= ""
	AR_SJH::DESCR		= "Pacific Pride"
	AR_SJH::DUEDATE		= ""
	AR_SJH::DISCOUNTDATE	= ""
	AR_SJH::SUBACCT		= LOCA$

	!
	! Call the post function
	!
	GOTO Aborted IF AR_TRAN_POSTSJ(OPT_ADDREC, SUBOPT_REGISTER, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		AR_SJH, AR_SJL, "PP") <> CMC$_NORMAL

	!
	! Add sales journal line item
	!
	AR_SJL::INVNUM	= INVOICE_NUMBER$
	AR_SJL::SLINE	= "001"
	AR_SJL::ACCT	= "5" + LEFT(PP_DAILY::PRODUCT, 2%) + "." + LOCA$
	AR_SJL::SUBACCT	= LOCA$
	AR_SJL::AMOUNT	= -FUNC_ROUND(SELLPRICE * PP_DAILY::QUANTITY, 2%)
	AR_SJL::QTY	= -PP_DAILY::QUANTITY
	AR_SJL::LTYPE	= "S"
	AR_SJL::TAXTYP	= ""
	AR_SJL::DESCR	= PP_DAILY::DRIVER + " " + PP_DAILY::VEHICLE

	!
	! Call the post function
	!
	GOTO Aborted IF AR_TRAN_POSTSJ(OPT_ADDREC, SUBOPT_LINEITEM, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		AR_SJH, AR_SJL, "PP") <> CMC$_NORMAL

	!
	! Post any Liability Amounts
	!
	GOSUB LiabilityPost

	!
	! Increment Invoice Number
	!
	V% = FUNC_NUMBERADD(INVOICE_NUMBER$, 1%)

	RETURN

	%PAGE

 ForeignSale:
	!*******************************************************************
	! Post  SJH records
	!*******************************************************************

	!
	! Get record from site file
	!
3500	LOCA$ = ""

	WHEN ERROR IN
		GET #PP_SITE.CH%, &
			KEY #0% EQ PP_DAILY::HOST + PP_DAILY::SITE + PP_DAILY::STYPE, &
			REGARDLESS
	USE
		CONTINUE BadSite
	END WHEN

	LOCA$ = PP_SITE::FORSALE

3510	AR_SJH::INVNUM		= INVOICE_NUMBER$
	AR_SJH::CUSNUM		= PP_CONTROL::CUSNUM
	AR_SJH::TRATYP		= "01"
	AR_SJH::TRADAT		= BATCH_NO$
	AR_SJH::AMOUNT		= &
		FUNC_ROUND(PP_DAILY::TRANCOST * PP_DAILY::QUANTITY, 2%)
	AR_SJH::ARACCT		= AR_CONTROL::AR_ACCT
	AR_SJH::RECNUM		= ""
	AR_SJH::CHECK		= ""
	AR_SJH::DEPOSIT		= ""
	AR_SJH::DESCR		= "Pacific Pride"
	AR_SJH::DUEDATE		= ""
	AR_SJH::DISCOUNTDATE	= ""
	AR_SJH::SUBACCT		= LOCA$

	!
	! Call the post function
	!
	GOTO Aborted IF AR_TRAN_POSTSJ(OPT_ADDREC, SUBOPT_REGISTER, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		AR_SJH, AR_SJL, "PP") <> CMC$_NORMAL

	!
	! Add sales journal line item
	!
	AR_SJL::INVNUM	= INVOICE_NUMBER$
	AR_SJL::SLINE	= "001"
	AR_SJL::ACCT	= "5" + LEFT(PP_DAILY::PRODUCT, 2%) + "." + LOCA$
	AR_SJL::SUBACCT	= LOCA$
	AR_SJL::AMOUNT	= -FUNC_ROUND(PP_DAILY::TRANCOST * PP_DAILY::QUANTITY, 2%)
	AR_SJL::QTY	= -PP_DAILY::QUANTITY
	AR_SJL::LTYPE	= "S"
	AR_SJL::TAXTYP	= ""
	AR_SJL::DESCR	= PP_DAILY::DRIVER + " " + PP_DAILY::VEHICLE

	!
	! Call the post function
	!
	GOTO Aborted IF AR_TRAN_POSTSJ(OPT_ADDREC, SUBOPT_LINEITEM, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		AR_SJH, AR_SJL, "PP") <> CMC$_NORMAL

	!
	! Increment Invoice Number
	!
	V% = FUNC_NUMBERADD(INVOICE_NUMBER$, 1%)

	RETURN

	%PAGE

 ForeignPurchase:
	!*******************************************************************
	! Post  SJH records
	!*******************************************************************

	!
	! Get record from site file
	!
3600	LOCA$ = ""

	WHEN ERROR IN
		GET #PP_SITE.CH%, &
			KEY #0% EQ PP_DAILY::HOST + PP_DAILY::SITE + PP_DAILY::STYPE, &
			REGARDLESS
	USE
		CONTINUE BadSite
	END WHEN

	LOCA$ = PP_SITE::FORPUR

	!
	! Get site product tax data
	!
3610	WHEN ERROR IN
		GET #PP_SITE_PRODUCT.CH%, &
			KEY #0% EQ PP_DAILY::HOST + PP_DAILY::SITE + PP_DAILY::STYPE + &
				PP_DAILY::PRODUCT, &
			REGARDLESS
	USE
		CONTINUE BadSiteProduct
	END WHEN

3620	!
	! Calculate any liability amounts
	!
	GOSUB LiabilityCalc

	!
	! Calculate total cost
	!
	FULLCOST = FUNC_ROUND(PP_DAILY::SELLPRICE * PP_DAILY::QUANTITY + &
		FED_TAX + STA_TAX + COU_TAX + CTY_TAX + STX_TAX, 2%)

	!
	! Post header
	!
	AR_SJH::INVNUM		= INVOICE_NUMBER$
	AR_SJH::CUSNUM		= PP_DAILY::CUSNUM
	AR_SJH::TRATYP		= "01"
	AR_SJH::TRADAT		= BATCH_NO$
	AR_SJH::AMOUNT		= FULLCOST
	AR_SJH::ARACCT		= AR_CONTROL::AR_ACCT
	AR_SJH::RECNUM		= ""
	AR_SJH::CHECK		= ""
	AR_SJH::DEPOSIT		= ""
	AR_SJH::DESCR		= "Pacific Pride"
	AR_SJH::DUEDATE		= ""
	AR_SJH::DISCOUNTDATE	= ""
	AR_SJH::SUBACCT		= LOCA$

	!
	! Call the post function
	!
	GOTO Aborted IF AR_TRAN_POSTSJ(OPT_ADDREC, SUBOPT_REGISTER, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		AR_SJH, AR_SJL, "PP") <> CMC$_NORMAL

	!
	! Add sales journal line item
	!
	AR_SJL::INVNUM	= INVOICE_NUMBER$
	AR_SJL::SLINE	= "001"
	AR_SJL::ACCT	= "5" + LEFT(PP_DAILY::PRODUCT, 2%) + "." + LOCA$
	AR_SJL::SUBACCT	= LOCA$
	AR_SJL::AMOUNT	= -FULLCOST
	AR_SJL::QTY	= -PP_DAILY::QUANTITY
	AR_SJL::LTYPE	= "S"
	AR_SJL::TAXTYP	= ""
	AR_SJL::DESCR	= PP_DAILY::DRIVER + " " + PP_DAILY::VEHICLE

	!
	! Call the post function
	!
	GOTO Aborted IF AR_TRAN_POSTSJ(OPT_ADDREC, SUBOPT_LINEITEM, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		AR_SJH, AR_SJL, "PP") <> CMC$_NORMAL

	!
	! Post any Liability Amounts
	!
	GOSUB LiabilityPost

	!
	! Increment Invoice Number
	!
	V% = FUNC_NUMBERADD(INVOICE_NUMBER$, 1%)

	!
	! Post the transfer cost
	!
	AR_SJH::INVNUM		= INVOICE_NUMBER$
	AR_SJH::CUSNUM		= PP_CONTROL::CUSNUM
	AR_SJH::TRATYP		= "01"
	AR_SJH::TRADAT		= BATCH_NO$
	AR_SJH::AMOUNT		= &
		-FUNC_ROUND(PP_DAILY::TRANCOST * PP_DAILY::QUANTITY, 2%)
	AR_SJH::ARACCT		= AR_CONTROL::AR_ACCT
	AR_SJH::RECNUM		= ""
	AR_SJH::CHECK		= ""
	AR_SJH::DEPOSIT		= ""
	AR_SJH::DESCR		= "Pacific Pride"
	AR_SJH::DUEDATE		= ""
	AR_SJH::DISCOUNTDATE	= ""
	AR_SJH::SUBACCT		= LOCA$

	!
	! Call the post function
	!
	GOTO Aborted IF AR_TRAN_POSTSJ(OPT_ADDREC, SUBOPT_REGISTER, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		AR_SJH, AR_SJL, "PP") <> CMC$_NORMAL

	!
	! Add sales journal line item
	!
	AR_SJL::INVNUM	= INVOICE_NUMBER$
	AR_SJL::SLINE	= "001"
	AR_SJL::ACCT	= "6" + LEFT(PP_DAILY::PRODUCT, 2%) + "." + LOCA$
	AR_SJL::SUBACCT	= LOCA$
	AR_SJL::AMOUNT	= FUNC_ROUND(PP_DAILY::TRANCOST * PP_DAILY::QUANTITY, 2%)
	AR_SJL::QTY	= PP_DAILY::QUANTITY
	AR_SJL::LTYPE	= "S"
	AR_SJL::TAXTYP	= ""
	AR_SJL::DESCR	= PP_DAILY::DRIVER + " " + PP_DAILY::VEHICLE

	!
	! Call the post function
	!
	GOTO Aborted IF AR_TRAN_POSTSJ(OPT_ADDREC, SUBOPT_LINEITEM, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		AR_SJH, AR_SJL, "PP") <> CMC$_NORMAL

	!
	! Increment Invoice Number
	!
	V% = FUNC_NUMBERADD(INVOICE_NUMBER$, 1%)

	RETURN

	%PAGE

 LiabilityCalc:
	!*******************************************************************
	! Calculate any liability amounts
	!
	! This needs to be called before setting up posting because
	! of the possible exemptions from taxes.
	!
	! Need to have the PP_SITE, PP_SITE_PRODUCT and PP_DAILY loaded
	! before calling this routine.
	!*******************************************************************

4900	!
	! Get the cardexempt file
	!
	SELLPRICE = PP_DAILY::SELLPRICE

	FED_TAX = 0.0
	STA_TAX = 0.0
	COU_TAX = 0.0
	CTY_TAX = 0.0
	STX_TAX = 0.0

	WHEN ERROR IN
		GET #PP_CARDEXEMPT.CH%, &
			KEY #0% EQ PP_DAILY::CUSNUM + PP_DAILY::VEHICLE + PP_DAILY::PRODUCT, &
			REGARDLESS
	USE
		PP_CARDEXEMPT::AUTHORITY = ""

		CONTINUE 4910
	END WHEN

4910	!
	! Federal
	!
	IF (PP_SITE_PRODUCT::FED_RATE <> 0.0) AND &
		(PP_SITE_PRODUCT::FED_INTP = "N")
	THEN
		IF (INSTR(1%, PP_CARDEXEMPT::AUTHORITY, "F") <> 0%)
		THEN
			SELLPRICE = SELLPRICE - PP_SITE_PRODUCT::FED_RATE
		ELSE
			FED_TAX = PP_SITE_PRODUCT::FED_RATE
		END IF
	END IF

	!
	! State
	!
	IF (PP_SITE_PRODUCT::STA_RATE <> 0.0) AND &
		(PP_SITE_PRODUCT::STA_INTP = "N")
	THEN
		IF (INSTR(1%, PP_CARDEXEMPT::AUTHORITY, "S") <> 0%)
		THEN
			SELLPRICE = SELLPRICE - PP_SITE_PRODUCT::STA_RATE
		ELSE
			STA_TAX = PP_SITE_PRODUCT::STA_RATE
		END IF
	END IF

	!
	! County
	!
	IF (PP_SITE_PRODUCT::COU_RATE <> 0.0) AND &
		(PP_SITE_PRODUCT::COU_INTP = "N")
	THEN
		IF (INSTR(1%, PP_CARDEXEMPT::AUTHORITY, "D") <> 0%)
		THEN
			SELLPRICE = SELLPRICE - PP_SITE_PRODUCT::COU_RATE
		ELSE
			COU_TAX = PP_SITE_PRODUCT::COU_RATE
		END IF
	END IF

	!
	! City
	!
	IF (PP_SITE_PRODUCT::CTY_RATE <> 0.0) AND &
		(PP_SITE_PRODUCT::CTY_INTP = "N")
	THEN
		IF (INSTR(1%, PP_CARDEXEMPT::AUTHORITY, "C") <> 0%)
		THEN
			SELLPRICE = SELLPRICE - PP_SITE_PRODUCT::CTY_RATE
		ELSE
			CTY_TAX = PP_SITE_PRODUCT::CTY_RATE
		END IF
	END IF

	!
	! Sales Tax
	!
	IF (PP_SITE_PRODUCT::STX_RATE <> 0.0) AND &
		(PP_SITE_PRODUCT::STX_INTP = "N")
	THEN
		IF (INSTR(1%, PP_CARDEXEMPT::AUTHORITY, "A") <> 0%)
		THEN
			SELLPRICE = SELLPRICE - PP_SITE_PRODUCT::STX_RATE
		ELSE
			STX_TAX = PP_SITE_PRODUCT::STX_RATE
		END IF
	END IF

	RETURN

	%PAGE

 LiabilityPost:
	!*******************************************************************
	! Post any liability amounts
	!
	! This needs to be called after calling LiabilityCalc, and
	! setting up line records.
	!
	! Need to have the PP_SITE, PP_SITE_PRODUCT and PP_DAILY loaded
	! before calling this routine.
	!*******************************************************************

	TOTAL = 0.0

	!
	! Federal
	!
	IF FED_TAX <> 0.0
	THEN
		!
		! Add sales journal line item
		!
		AR_SJL::SLINE	= "010"
		AR_SJL::ACCT	= PP_SITE_PRODUCT::FED_ACCOUNT
		AR_SJL::AMOUNT	= -FUNC_ROUND(FED_TAX * PP_DAILY::QUANTITY, 2%)
		AR_SJL::LTYPE	= ""
		AR_SJL::TAXTYP	= ""
		AR_SJL::DESCR	= "Federal Tax"

		!
		! Call the post function
		!
		GOTO Aborted IF AR_TRAN_POSTSJ(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			AR_SJH, AR_SJL, "PP") <> CMC$_NORMAL

		TOTAL = TOTAL + AR_SJL::AMOUNT
	END IF

	!
	! State Tax
	!
	IF STA_TAX <> 0.0
	THEN
		!
		! Add sales journal line item
		!
		AR_SJL::SLINE	= "012"
		AR_SJL::ACCT	= PP_SITE_PRODUCT::STA_ACCOUNT
		AR_SJL::AMOUNT	= -FUNC_ROUND(STA_TAX * PP_DAILY::QUANTITY, 2%)
		AR_SJL::LTYPE	= ""
		AR_SJL::TAXTYP	= ""
		AR_SJL::DESCR	= "State Tax"

		!
		! Call the post function
		!
		GOTO Aborted IF AR_TRAN_POSTSJ(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			AR_SJH, AR_SJL, "PP") <> CMC$_NORMAL

		TOTAL = TOTAL + AR_SJL::AMOUNT
	END IF

	!
	! County Tax
	!
	IF COU_TAX <> 0.0
	THEN
		!
		! Add sales journal line item
		!
		AR_SJL::SLINE	= "014"
		AR_SJL::ACCT	= PP_SITE_PRODUCT::COU_ACCOUNT
		AR_SJL::AMOUNT	= -FUNC_ROUND(COU_TAX * PP_DAILY::QUANTITY, 2%)
		AR_SJL::LTYPE	= ""
		AR_SJL::TAXTYP	= ""
		AR_SJL::DESCR	= "County Tax"

		!
		! Call the post function
		!
		GOTO Aborted IF AR_TRAN_POSTSJ(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			AR_SJH, AR_SJL, "PP") <> CMC$_NORMAL

		TOTAL = TOTAL + AR_SJL::AMOUNT
	END IF

	!
	! City Tax
	!
	IF CTY_TAX <> 0.0
	THEN
		!
		! Add sales journal line item
		!
		AR_SJL::SLINE	= "016"
		AR_SJL::ACCT	= PP_SITE_PRODUCT::CTY_ACCOUNT
		AR_SJL::AMOUNT	= -FUNC_ROUND(CTY_TAX * PP_DAILY::QUANTITY, 2%)
		AR_SJL::LTYPE	= ""
		AR_SJL::TAXTYP	= ""
		AR_SJL::DESCR	= "City Tax"

		!
		! Call the post function
		!
		GOTO Aborted IF AR_TRAN_POSTSJ(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			AR_SJH, AR_SJL, "PP") <> CMC$_NORMAL

		TOTAL = TOTAL + AR_SJL::AMOUNT
	END IF

	!
	! Sales Tax
	!
	IF STX_TAX <> 0.0
	THEN
		!
		! Add sales journal line item
		!
		AR_SJL::SLINE	= "018"
		AR_SJL::ACCT	= PP_SITE_PRODUCT::STX_ACCOUNT
		AR_SJL::AMOUNT	= -FUNC_ROUND(STX_TAX * PP_DAILY::QUANTITY, 2%)
		AR_SJL::LTYPE	= ""
		AR_SJL::TAXTYP	= ""
		AR_SJL::DESCR	= "Sales Tax"

		!
		! Call the post function
		!
		GOTO Aborted IF AR_TRAN_POSTSJ(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			AR_SJH, AR_SJL, "PP") <> CMC$_NORMAL

		TOTAL = TOTAL + AR_SJL::AMOUNT
	END IF

	!
	! Post the total
	!
	IF TOTAL <> 0.0
	THEN
		!
		! Add sales journal line item
		!
		AR_SJL::SLINE	= "010"
		AR_SJL::ACCT	= "6" + LEFT(PP_DAILY::PRODUCT, 2%) + "." + LOCA$
		AR_SJL::AMOUNT	= FUNC_ROUND(TOTAL, 2%)
		AR_SJL::LTYPE	= ""
		AR_SJL::TAXTYP	= ""
		AR_SJL::DESCR	= "Taxes"

		!
		! Call the post function
		!
		GOTO Aborted IF AR_TRAN_POSTSJ(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			AR_SJH, AR_SJL, "PP") <> CMC$_NORMAL
	END IF

	RETURN

	%PAGE

 BadSiteProduct:
	!*******************************************************************
	! Unable to find record for site product
	!*******************************************************************

	TEXT$ = "Host: " + PP_DAILY::HOST + &
		" Site: " + PP_DAILY::SITE + &
		" Stype: " + PP_DAILY::STYPE + &
		" Product: " + PP_DAILY::PRODUCT + &
		" is undefined in site product file!"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	PRODUCT$ = "*"

	GOTO 3200

	%PAGE

 BadSite:
	!*******************************************************************
	! Unable to find record for site product
	!*******************************************************************

	TEXT$ = "Host: " + PP_DAILY::HOST + &
		" Site: " + PP_DAILY::SITE + &
		" Stype: " + PP_DAILY::STYPE + &
		" is undefined in site file!"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	PRODUCT$ = "*"

	GOTO 3200

	%PAGE

 Confirm:
	!******************************************************************
	! Confirm posting
	!******************************************************************

	EXIT_STATUS = EXIT_STATUS AND OUTP_UNDEFCODES(OPT_CONFIRM, TITLE(), &
		UTL_REPORTX, "")

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
	! Post to the MONTHLY file
	!
	GOTO Interrupt IF PP_TRAN_POST(OPT_POSTFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", YYYY_PP$) <> CMC$_NORMAL

	GOTO Interrupt IF AR_TRAN_POSTSJ(OPT_POSTFILE, SUBOPT_REGISTER, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, AR_SJH, AR_SJL, "PP") <> CMC$_NORMAL

	GOTO Interrupt IF AR_TRAN_POSTSJ(OPT_POSTFILE, SUBOPT_LINEITEM, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, AR_SJH, AR_SJL, "PP") <> CMC$_NORMAL

	%PAGE

	!
	! Remove files
	!
5000	CLOSE PP_DAILY.CH%

5010 !	WHEN ERROR IN
 !		KILL PP_DAILY.DEV$ + "PP_DAILY_" + BATCH_NO$ + ".JRL" &
 !			FOR I% = 1% TO 10%
 !	USE
 !		CONTINUE 5100
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(PP_DAILY.DEV$ + &
		"PP_DAILY_" + BATCH_NO$ + ".JRL;*")

5100	WHEN ERROR IN
		GET #PP_CONTROL.CH%, RECORD 1%

		PP_CONTROL::LAST_INV = INVOICE_NUMBER$

		UPDATE #PP_CONTROL.CH%
	USE
		!
		! Locked block
		!
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		FILENAME$ = "PP_CONTROL"
		CONTINUE HelpError
	END WHEN

 Complete:
	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "PP_DAILY", BATCH_NO$, "", "")

 ExitProgram:
	!******************************************************************
	! Exit normally
	!******************************************************************
	!
	! Print credit and debit transmittals
	!
	! Print undefined codes (if any)
	!
	TEXT = "Order#     Customer#   Loc   Product#"

	EXIT_STATUS = OUTP_UNDEFCODES(OPT_SUMMARY, TITLE(), UTL_REPORTX, TEXT)

	!
	! Dump out subtotals
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 3000%)

	TEXT$ = "   " + &
		"Product       " + &
		"       Count " + &
		"      Quantity " + &
		"       Dollars"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	XTOTAL$ = "~~~~~~~~"
	COUNTER% = 0%
	QUANTITY = 0.0
	PRICE = 0.0

	FOR I% = 1% TO SUBTOTAL%

		IF SUBTOTAL(I%)::BUYFRAN <> XTOTAL$
		THEN
			IF COUNTER%
			THEN
				TEXT$ = "      Total       " + &
					FORMAT$(COUNTER%, "###,###,### ") + &
					FORMAT$(QUANTITY, "###,###,###.## ") + &
					FORMAT$(PRICE, "###,###,###.##")

				CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
				CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

				COUNTER% = 0%
				QUANTITY = 0.0
				PRICE = 0.0
			END IF

			SELECT SUBTOTAL(I%)::BUYFRAN
			CASE "~~~"
				TEXT$ = "Total of all Locations"
			CASE "F"
				TEXT$ = "Total of Foreign Sales (F)"
			CASE "P"
				TEXT$ = "Total of Foreign Purchases(P)"
			CASE ELSE
				TEXT$ = "Total of Local Sales (" + &
					SUBTOTAL(I%)::BUYFRAN + ")"
			END SELECT

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			XTOTAL$ = SUBTOTAL(I%)::BUYFRAN
		END IF

		TEXT$ = "   " + &
			SUBTOTAL(I%)::PRODUCT + &
			FORMAT$(SUBTOTAL(I%)::COUNTER, " ###,###,### ") + &
			FORMAT$(SUBTOTAL(I%)::QUANTITY, "###,###,###.## ") + &
			FORMAT$(SUBTOTAL(I%)::PRICE, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		COUNTER% = COUNTER% + SUBTOTAL(I%)::COUNTER
		QUANTITY = QUANTITY + SUBTOTAL(I%)::QUANTITY
		PRICE = PRICE + SUBTOTAL(I%)::PRICE

	NEXT I%

	IF COUNTER%
	THEN
		TEXT$ = "      Total       " + &
			FORMAT$(COUNTER%, "###,###,### ") + &
			FORMAT$(QUANTITY, "###,###,###.## ") + &
			FORMAT$(PRICE, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	END IF

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
	IF INTR_STATUS <> CMC$_WARNING
	THEN
		EXIT_STATUS = ASSG_POSTBATCH(OPT_ABORT,	BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "PP_DAILY", BATCH_NO$, "", "")

		GOTO ExitProgram
	END IF

	%PAGE

 Interrupt:
	!******************************************************************
	! Interrupt process
	!******************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_INTERRUPT, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "PP_DAILY", BATCH_NO$, "", "")

	GOTO ExitProgram

	%PAGE

	!*******************************************************************
	! Calculate subtotals
	!*******************************************************************

 AddSubtotal:
	FOR I% = 1% TO SUBTOTAL%
		GOTO AddSubtotal1 &
			IF (SUBTOTAL(I%)::BUYFRAN = SUBBUYFRAN$) AND &
			(SUBTOTAL(I%)::PRODUCT = PP_DAILY::PRODUCT)

		GOTO AddSubtotal2 &
			IF (SUBTOTAL(I%)::BUYFRAN + SUBTOTAL(I%)::PRODUCT) > &
			SUBBUYFRAN$ +  PP_DAILY::PRODUCT
	NEXT I%
	I% = SUBTOTAL% + 1%

 AddSubtotal2:
	!
	! Insert new subtotal at I%
	!
	SUBTOTAL(J%+1%) = SUBTOTAL(J%) FOR J% = SUBTOTAL% TO I% STEP -1%
	SUBTOTAL% = SUBTOTAL% + 1%
	SUBTOTAL(I%)::BUYFRAN = SUBBUYFRAN$
	SUBTOTAL(I%)::PRODUCT = PP_DAILY::PRODUCT
	SUBTOTAL(I%)::COUNTER = 0%
	SUBTOTAL(I%)::QUANTITY = 0.0
	SUBTOTAL(I%)::PRICE = 0.0

 AddSubTotal1:
	!
	! Increment counts
	!
	SUBTOTAL(I%)::COUNTER = SUBTOTAL(I%)::COUNTER + 1%
	SUBTOTAL(I%)::QUANTITY = SUBTOTAL(I%)::QUANTITY + PP_DAILY::QUANTITY
	SUBTOTAL(I%)::PRICE = SUBTOTAL(I%)::PRICE + &
		FUNC_ROUND(PP_DAILY::SELLPRICE * PP_DAILY::QUANTITY, 2%)

	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_PRINTMESS(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR), UTL_REPORTX, TITLE(), 0%)

	GOTO Aborted

	%PAGE

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	!
	! Trap untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
	!+-+-+
	!++
	! Abstract:FLD03
	!	^*(03) Check Date\*
	!	.b
	!	.lm +5
	!	The ^*Check Date\* field checks all dates of
	!	the entries in the journal to insure they are within a particular
	!	date range (accounting period) for posting.
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes
	!	.te
	!	^*N\* - No
	!	.end table
	!	By entering ^*N\*, for No, the dates will not be checked and all entries
	!	will be posted. A ^*Y\*, for Yes, entry causes all dates to be
	!	checked. If ^*Y\* is entered and dates are found that are not within
	!	the date range for posting, the posting will be aborted.
	!	.lm -5
	!
	! Index:
	!
	!--
