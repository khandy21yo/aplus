1	%TITLE "Ticket Journal Posting"
	%SBTTL "PS_POST_TICKET"
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
	! ID:PS0012
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Post Ticket Journal\* process
	!	transfers information from
	!	the Ticket Journal file to the
	!	OE Register and Inventory
	!	transaction file as well as the
	!	General Ledger and Accounts Receivable.
	!	.lm -5
	!
	! Index:
	!	.x Post>Ticket Journal
	!	.x Ticket Journal>Post
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PS_SOURCE:PS_POST_TICKET/LINE
	!	$ LINK/EXE=PS_EXE: PS_POST_TICKET, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PS_POST_TICKET.OBJ;*
	!
	! Author:
	!
	!	11/11/91 - Frank F. Starman
	!
	! Modification history:
	!
	!	12/18/91 - Kevin Handy
	!		Removed unused map POST_TO_GL.
	!
	!	03/12/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	04/10/92 - Frank F. Starman
	!		Show discount and other charges in AR_OPEN file.
	!
	!	04/15/92 - Frank F. Starman
	!		Read OE_REASONACCT table for misc charges.
	!
	!	04/25/92 - Frank F. Starman
	!		Read Sales Account from OE_ACCOUNT table.
	!
	!	04/27/92 - Frank F. Starman
	!		Fix posting into inventory if we handle returns.
	!
	!	04/28/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/28/92 - Frank F. Starman
	!		Kill PS_CASHINOUT file.
	!
	!	06/08/92 - Dan Perkins
	!		Kill Journal files before completing posting, if
	!		posting gets that far.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	02/05/93 - Kevin Handy
	!		Added AR_OPEN::DUEDATE and AR_OPEN::DISCOUNTDATE.
	!
	!	02/19/93 - Dan Perkins
	!		Added arguements to OUTP_UNDEFCODES so the program
	!		would work.  Added omited code to handle posting
	!		OE_ORDERJOUR::HANDLING to General Ledger.
	!		Enhanced error messages on undefined accounts.
	!
	!	03/03/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/20/93 - Frank F. Starman
	!		Added subroutine SUBR_TRANTYPE and rewrite posting.
	!		Combine posting with a journal report.
	!
	!	04/09/93 - Frank F. Starman
	!		Added posting to WP
	!
	!	04/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/14/93 - Dan Perkins
	!		Corrected spelling in MO_TRAN_POST function.
	!		Enhanced undefined code messages.
	!
	!	05/25/93 - Dan Perkins
	!		Added Handling to order total line.
	!		Improved undefined code messages.
	!
	!	05/25/93 - Dan Perkins
	!		Moved line titles to print with lines if there
	!		are lines to print.
	!
	!	06/09/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/11/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/18/93 - Frank F. Starman
	!		Calculate due and discount date.
	!
	!	06/21/93 - Dan Perkins
	!		Set AR_35CUSTOM_EXAM::CUSNAM = OE_ORDERJOUR::SHIPNAM
	!		if there is no customer number.
	!		Set GL_YYYY_PP::DESCR = AR_35CUSTOM_EXAM::CUSNAM instead
	!		of AR_35CUSTOM::CUSNAM which was nothing.
	!
	!	06/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/24/93 - Frank F. Starman
	!		Open files for read only if report is printed.
	!
	!	06/25/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	07/20/93 - Frank F. Starman
	!		Post header to GL if invoice is assigned.
	!		Change sign while posting sales tax
	!		open MO_MODELCODE file.
	!
	!	08/12/93 - Frank F. Starman
	!		Modify after MO_ORDERLINEOPT key change.
	!		(MAKE+MODELCODE)
	!
	!	08/20/93 - Frank F. Starman
	!		Added warning is cost is greater than price.
	!
	!	08/26/93 - Frank F. Starman
	!		Added case for transfers on backorder.
	!
	!	09/02/93 - Frank F. Starman
	!		Fixed bug for incorrect salesman to AR_OPEN.
	!
	!	09/16/93 - Kevin Handy
	!		Fixed posting to sales tax ledger, so that
	!		everything goes in, and the tax flag is posted
	!		instead of a hard coded 1.
	!
	!	10/19/93 - Frank F. Starman
	!		Handle case CMC$_WARNING as a output of PD_EXAM_PRODUCT
	!		function.
	!
	!	11/16/93 - Frank F. Starman
	!		Set OE_REGLINE::TDATE based on the transaction.
	!
	!	11/26/93 - Frank F. Starman
	!		Added a new M flag for posting to GL and AR.
	!
	!	01/25/94 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/29/94 - Kevin Handy
	!		Fixed bug where loop created by MORegLine would
	!		not terminate because it set LIN$ to
	!		OE_REGLINE_READ::LIN instead of
	!		MO_REGLINE_READ::LIN.
	!
	!	05/27/94 - Kevin Handy
	!		Modified to display the code number for the
	!		error "Undefined OEOrdl transaction.." and
	!		it's like.
	!
	!	05/31/94 - Kevin Handy
	!		Added "136" case for transfer with cancel.
	!
	!	06/01/94 - Kevin Handy
	!		Added "264" case for transfer with partial shipment.
	!
	!	06/17/94 - Kevin Handy
	!		Added code for ::MISCH2.
	!
	!	06/20/94 - Kevin Handy
	!		Added code to handle non-taxable miscellanous
	!		amounts.
	!
	!	08/18/94 - Kevin Handy
	!		Modified so that it won't kill the journals if
	!		it gets an error while renaming.
	!
	!	10/11/94 - Kevin Handy
	!		Modified to flag customers on report as undefined
	!		instead of puttine in a random custoimers info.
	!
	!	02/21/95 - Kevin Handy
	!		Modified to post requested ship date to OE_ORDERLINE
	!		when possible on orders.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/08/95 - Kevin Handy
	!		Modification so that orders do not overwrite the
	!		ship date after setting it to the ship date.
	!		Modified so REGHEADER::SDATE is the ship date.
	!
	!	06/02/95 - Kevin Handy
	!		Modified to handle non-sales taxable categories.
	!
	!	06/02/95 - Kevin Handy
	!		Modified to make sales tax ledger handle splitting
	!		invoice into sales-taxable and non-sales-taxable.
	!
	!	06/20/95 - Kevin Handy
	!		Reformat closer to 80 columns.
	!
	!	09/01/95 - Kevin Handy
	!		Lose unecessary external statements.
	!		Reformat source closer to 80 columns.
	!
	!	11/20/95 - Kevin Handy
	!		Clean up source code somewhat.
	!		Make it fit better in 80 columns.
	!
	!	11/20/95 - Kevin Handy
	!		Handle PD_ACCOUNT_TRAN close to where it is needed,
	!		instead of sonetimes setting it up in a previous
	!		select statement.
	!
	!	12/08/95 - Kevin Handy
	!		Change layout for OE_ORDERLINE::NOTES and
	!		OE_REGLINE::NOTES, and added OE_ORDERLINE::SUBACCT
	!		and OE_REGLINE::SUBACCT.
	!
	!	01/11/96 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		Remove commented out code.
	!
	!	06/10/96 - Kevin Handy
	!		Remove include of SA_SALESMAN, since it is
	!		never used.
	!
	!	07/09/96 - Kevin Handy
	!		Modifications to fix description of the job
	!		when posting to JC_JOB. (new variables TOPJOB$
	!		and TOPDESC$)
	!
	!	07/19/96 - Kevin Handy
	!		Removed references to "OE_REASONACCT", which were
	!		never used.
	!
	!	10/14/96 - Kevin Handy
	!		Reformatting, added comments, lost commented out code.
	!
	!	10/16/96 - Kevin Handy
	!		Added code to print out profit line.
	!
	!	10/17/96 - Kevin Handy
	!		Added %profit
	!
	!	03/11/97 - Kevin Handy
	!		Another try at getting MO descriptions into
	!		the GL and JOB file properly. (Also threw
	!		product number into TRANKEY so it was saved
	!		somehow)
	!
	!	06/30/97 - Kevin Handy
	!		Post returns (16) to OE_REGISTER.
	!
	!	08/26/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/13/98 - Kevin Handy
	!		Add TAXFLAG parameter to OE_READ_SALESTAX
	!
	!	01/05/98 - Kevin Handy
	!		Add posting for check amounts paid (needs deposit
	!		number filled in to work properly)
	!		[Pull the cash account from the AP control file]
	!
	!	01/11/98 - Kevin Handy
	!		Post cash IN/OUT's
	!		Don't pull cash account from the AP control file.
	!
	!	05/26/99 - Kevin Handy
	!		Loop through all records in PS_CASHINOUT instead
	!		of just the first record.
	!
	!	09/13/99 - Kevin Handy
	!		Make sure that PD_ACCOUNT_READ has been set up
	!		in a 'CASE ELSE' so we don't get impure accounts
	!		defined.
	!
	!	07/20/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Add several REGARDLESS clauses
	!
	!	09/19/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!
	!	11/03/2000 - Kevin Handy
	!		Use A"x"B
	!
	!	11/27/2001 - Kevin Handy
	!		Post DEALS
	!
	!	01/09/2002 - Kevin Handy
	!		Fix deal posting
	!
	!	02/05/2002 - Kevin Handy
	!		Use same date here in promo lookup as was used
	!		in the journal when created (OE_ORDERJOUR::ORDDATE
	!		instead of OE_ORDLINE::REQDATE)
	!
	!	03/08/2002 - Kevin Handy
	!		Zero DEAL_PRICE at some point.
	!
	!	03/14/2002 - Kevin Handy
	!		Use 'INTEGER(TRANQTY(0))' instead of NUM1$ in
	!		select statements.  Should be faster.
	!
	!	12/30/2002 - Kevin Handy
	!		Yet another attempt to fix DEAL prices
	!
	!	03/20/2003 - Kevin Handy
	!		Add a "GOTO ExitProgram" at end of "aborted"
	!		routine.
	!
	!	08/10/2004 - Kevin Handy
	!		Penny rounding problem between entry and posting.
	!
	!	04/27/2005 - Kevin Handy
	!		Create "better" entries for AR_OPEN_DIST.
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
	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.HB"
	MAP (OE_ORDERJOUR)	OE_ORDERJOUR_CDD	OE_ORDERJOUR

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERLINE.HB"
	MAP (OE_ORDERLINE)	OE_ORDERLINE_CDD	OE_ORDERLINE

	%INCLUDE "SOURCE:[MO.OPEN]MO_MODELCODE.HB"
	MAP (MO_MODELCODE)	MO_MODELCODE_CDD	MO_MODELCODE

	%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINE.HB"
	MAP (MO_ORDERLINE)	MO_ORDERLINE_CDD	MO_ORDERLINE

	%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINEOPT.HB"
	MAP (MO_ORDERLINEOPT)	MO_ORDERLINEOPT_CDD	MO_ORDERLINEOPT

	%INCLUDE "SOURCE:[OE.OPEN]OE_CONTROL.HB"
	MAP (OE_CONTROL)	OE_CONTROL_CDD		OE_CONTROL

	%INCLUDE "SOURCE:[WP.OPEN]WP_ORDERLINE.HB"
	MAP (WP_ORDERLINE)	WP_ORDERLINE_CDD	WP_ORDERLINE

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQLINE.HB"
	MAP (WP_REQLINE)	WP_REQLINE_CDD		WP_REQLINE

	%INCLUDE "SOURCE:[WP.OPEN]WP_JOB.HB"
	MAP (WP_JOB)		WP_JOB_CDD		WP_JOB

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (JC_JOB)		JC_JOB_CDD		JC_JOB

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TERMS.HB"
	MAP (UTL_TERMS)		UTL_TERMS_CDD		UTL_TERMS

	%INCLUDE "SOURCE:[PS.OPEN]PS_CONTROL.HB"
	MAP (PS_CONTROL)	PS_CONTROL_CDD		PS_CONTROL

	%INCLUDE "SOURCE:[PS.OPEN]PS_CASHREG.HB"
	MAP (PS_CASHREG)	PS_CASHREG_CDD		PS_CASHREG

	%INCLUDE "SOURCE:[PS.OPEN]PS_CASHINOUT.HB"
	MAP	(PS_CASHINOUT)	PS_CASHINOUT_CDD	PS_CASHINOUT

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN_DIST.HB"
	%INCLUDE "SOURCE:[AR.OPEN]AR_SALTAXLED.HB"

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"

	%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.HB"

	%INCLUDE "SOURCE:[OE.OPEN]OE_ACCOUNT.HB"
	%INCLUDE "SOURCE:[OE.OPEN]OE_PROMO.HB"
	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	%INCLUDE "SOURCE:[OE.OPEN]OE_SALESTAX.HB"

	%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINE.HB"
	%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINEOPT.HB"

	%INCLUDE "SOURCE:[PD.OPEN]PD_ACCOUNT.HB"
	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"

	%INCLUDE "SOURCE:[SA.OPEN]SA_COMMACCT.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSACCT.HB"

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION AR_EXAM_CUSTOM
	EXTERNAL LONG	FUNCTION AR_TRAN_POSTAR
	EXTERNAL LONG	FUNCTION ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION GL_EXAM_CHART
	EXTERNAL LONG	FUNCTION GL_TRAN_POSTGL
	EXTERNAL LONG	FUNCTION IC_TRAN_POST
	EXTERNAL LONG	FUNCTION OE_READ_ACCOUNT
	EXTERNAL LONG	FUNCTION OE_READ_PROMO
	EXTERNAL LONG   FUNCTION PC_READ_DEAL
	EXTERNAL LONG	FUNCTION OE_READ_REGLINE
	EXTERNAL LONG	FUNCTION OE_READ_SALESTAX
	EXTERNAL LONG	FUNCTION OE_TRAN_POST
	EXTERNAL LONG	FUNCTION OUTP_UNDEFCODES
	EXTERNAL LONG	FUNCTION PD_EXAM_PRODUCT
	EXTERNAL LONG	FUNCTION PD_READ_ACCOUNT
	EXTERNAL LONG	FUNCTION MO_READ_REGLINE
	EXTERNAL LONG	FUNCTION MO_READ_REGLINEOPT
	EXTERNAL LONG	FUNCTION MO_TRAN_POST
	EXTERNAL LONG	FUNCTION WP_READ_REGLINE
	EXTERNAL LONG	FUNCTION WP_TRAN_POST
	EXTERNAL LONG	FUNCTION WP_TRAN_POSTREQ
	EXTERNAL LONG	FUNCTION UTL_EXAM_LOCATION
	EXTERNAL LONG	FUNCTION UTL_READ_TRANSACCT
	EXTERNAL REAL	FUNCTION PC_READ_COST

	!
	! Declare internal variables
	!
	DECLARE AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM
	DECLARE AR_OPEN_CDD		AR_OPEN
	DECLARE AR_OPEN_DIST_CDD	AR_OPEN_DIST
	DECLARE AR_SALTAXLED_CDD	AR_SALTAXLED

	DECLARE GL_CHART_CDD		GL_CHART_EXAM
	DECLARE	GL_YYYY_PP_CDD		GL_YYYY_PP

	DECLARE	IC_TRANSACTION_CDD	IC_TRANSACTION

	DECLARE OE_ACCOUNT_CDD		OE_ACCOUNT_READ
	DECLARE OE_PROMO_CDD		OE_PROMO_READ
	DECLARE OE_REGHEADER_CDD	OE_REGHEADER
	DECLARE OE_REGLINE_CDD		OE_REGLINE
	DECLARE OE_REGLINE_CDD		OE_REGLINE_READ
	DECLARE OE_SALESTAX_CDD		OE_SALESTAX_READ

	DECLARE MO_REGLINE_CDD		MO_REGLINE
	DECLARE MO_REGLINE_CDD		MO_REGLINE_READ
	DECLARE MO_REGLINEOPT_CDD	MO_REGLINEOPT
	DECLARE MO_REGLINEOPT_CDD	MO_REGLINEOPT_READ

	DECLARE WP_REGLINE_CDD		WP_REGLINE
	DECLARE WP_REGLINE_CDD		WP_REGLINE_READ
	DECLARE WP_REQREGISTER_CDD	WP_REQREGISTER

	DECLARE PD_ACCOUNT_CDD		PD_ACCOUNT_READ
	DECLARE PD_ACCOUNT_CDD		PD_ACCOUNT_TRAN
	DECLARE PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	DECLARE UTL_LOCATION_CDD	UTL_LOCATION_EXAM
	DECLARE	UTL_REPORTX_CDD		UTL_REPORTX
	DECLARE	UTL_TRANSACCT_CDD	UTL_TRANSACCT_READ

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
	DECLARE	STRING			TITLE(15%)

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
	TITLE(1%) = "ORDER-INVOICE JOURNAL POSTING PROTOCOL"
	TITLE(2%) = "Order-Invoice System"
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

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^* (01) Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field enters a
	!	particular batch to be posted.
	!	.b
	!	Only one batch at a time may be posted.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number>Print Journal
	!	.x Print Journal>Batch Number
	!
	! Required:
	!--

	ICPERIOD = EDIT$(UTL_REPORTX::OPTDEF(1%), -1%)

	!++
	! Abstract:FLD02
	!	.ts 55
	!	^*(02) Period	YYYYPP\*
	!	.b
	!	.lm +5
	!	The ^*Period\* field enters the
	!	accounting period into which this batch
	!	will be posted.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.X Post>Period
	!	.X Period>Post
	!
	!--

	CHECK_DATE = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Check Date\*
	!	.b
	!	.lm +5
	!	The ^*Check Date\* field
	!	checks the dates of all
	!	transactions to insure they are being
	!	posted to the correct accounting
	!	period.  A ^*N\* - No entry, the dates
	!	will not be checked and all transactions
	!	will be posted.  A ^*Y\* - Yes entry
	!	will cause all dates to be checked.  If
	!	dates are found that are not within the
	!	date range for posting, the posting
	!	will be aborted.
	!	.lm -5
	!
	! Index:
	!	.x Check Dates
	!
	!--

	CHECK_PERIOD = SUBOPT_NOOPT
	CHECK_PERIOD = SUBOPT_CHECK IF CHECK_DATE = "Y"

	REG_NO$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^* (05) Cash Register Number\*
	!	.b
	!	.lm +5
	!	The ^*Cash Register Number\* field
	!	enters a particular batch to be posted.
	!	.b
	!	Only one batch at a time may be posted.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number>Post Journal
	!
	! Required:
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	.x Sort by
	!	^*(06) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\*
	!	field determines the order
	!	in which the report will print.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*D\* - Document Number
	!	.te
	!	^*T\* - Sales Type
	!	.te
	!	^*N\* - Customer Number
	!	.te
	!	^*C\* - Order Category
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
	!	The ^*From Item\* field
	!	enters an item with which the
	!	report will begin printing.
	!	The value entered must be
	!	in agreement with
	!	field (06) Sort by.
	!	.b
	!	A blank field will cause the
	!	report to begin with the first
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
	!	The ^*To Item\* field
	!	enters an item with which the
	!	report will end printing.
	!	The value entered must be
	!	in agreement with
	!	field (06) Sort by.
	!	.b
	!	A blank field will cause the
	!	report to end with the last
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
	!	for Wildcarding Technique.
	!	.B
	!	The value entered must be in agreement
	!	with field (06) Sort by.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

	POSTGL$ = EDIT$(UTL_REPORTX::OPTDEF(9%), -1%)

	!++
	! Abstract:FLD10
	!	.x Post to GL
	!	^*(10) Post to GL\*
	!	.b
	!	.lm +5
	!	The ^*Post to GL\*
	!	field determines if a journal
	!	will be posted to the GL and AR.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes
	!	.te
	!	^*M\* - No and Move
	!	.te
	!	^*N\* - No
	!	.end table
	!	.lm -5
	!
	! Index:
	!
	!--

	BATCH_NO$ = REG_NO$ + "_" + BATCH_NO$

	%PAGE

300	!
	! Open Sales Order header file
	!
	WHEN ERROR IN
		IF SORTBY$ = ""
		THEN
			%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.UPD"
		ELSE
			%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.OPN"
		END IF
	USE
		FILENAME$ = "OE_ORDERJOUR"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Sales Journal line items file
	!
	WHEN ERROR IN
		IF SORTBY$ = ""
		THEN
			%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERLINE.UPD"
		ELSE
			%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERLINE.OPN"
		END IF
	USE
		CONTINUE 320 IF ERR = 5%
		FILENAME$ = "OE_ORDERLINE"
		CONTINUE HelpError
	END WHEN

320	!
	! Open MO Journal line items file
	!
	WHEN ERROR IN
		IF SORTBY$ = ""
		THEN
			%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINE.UPD"
		ELSE
			%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINE.OPN"
		END IF
	USE
		CONTINUE 330 IF ERR = 5%
		FILENAME$ = "MO_ORDERLINE"
		CONTINUE HelpError
	END WHEN

330	!
	! Open MO Journal line option items file
	!
	WHEN ERROR IN
		IF SORTBY$ = ""
		THEN
			%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINEOPT.UPD"
		ELSE
			%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINEOPT.OPN"
		END IF
	USE
		CONTINUE 340 IF ERR = 5%
		FILENAME$ = "MO_ORDERLINEOPT"
		CONTINUE HelpError
	END WHEN

340	!
	! Open WP job line
	!
	WHEN ERROR IN
		IF SORTBY$ = ""
		THEN
			%INCLUDE "SOURCE:[WP.OPEN]WP_ORDERLINE.UPD"
		ELSE
			%INCLUDE "SOURCE:[WP.OPEN]WP_ORDERLINE.OPN"
		END IF
	USE
		CONTINUE 350 IF ERR = 5%
		FILENAME$ = "WP_ORDERLINE"
		CONTINUE HelpError
	END WHEN

350	!
	! Open WP req line
	!
	WHEN ERROR IN
		IF SORTBY$ = ""
		THEN
			%INCLUDE "SOURCE:[WP.OPEN]WP_REQLINE.UPD"
		ELSE
			%INCLUDE "SOURCE:[WP.OPEN]WP_REQLINE.OPN"
		END IF
	USE
		CONTINUE 360 IF ERR = 5%
		FILENAME$ = "WP_REQLINE"
		CONTINUE HelpError
	END WHEN

360	!
	! Open UTL terms
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_TERMS.OPN"
	USE
		CONTINUE 370 IF ERR = 5%
		FILENAME$ = "UTL_TERMS"
		CONTINUE HelpError
	END WHEN

370	!
	! Open MO Model
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[MO.OPEN]MO_MODELCODE.OPN"
	USE
		CONTINUE 380 IF ERR = 5%
		FILENAME$ = "MO_MODELCODE"
		CONTINUE HelpError
	END WHEN

380	!
	! Open PS_CONTROL
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PS.OPEN]PS_CONTROL.OPN"
		GET #PS_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE PS_CONTROL.CH%
	USE
		CONTINUE 390 IF ERR = 5%
		FILENAME$ = "PS_CONTROL"
		CONTINUE HelpError
	END WHEN

390	!

400	!
	! Open the Cash Register file
	!
	I% = INSTR(1%, BATCH_NO$ + "_", "_")
	BATCH_REF$ = LEFT(BATCH_NO$, I% - 1%)

	WHEN ERROR IN
		%INCLUDE "SOURCE:[PS.OPEN]PS_CASHREG.OPN"
		GET #PS_CASHREG.CH%, KEY #0% EQ BATCH_REF$, REGARDLESS
		CLOSE #PS_CASHREG.CH%
	USE
		PS_CASHREG::CASHREG = ""
		PS_CASHREG::DESCR = ""
		PS_CASHREG::LOCATION = ""
		PS_CASHREG::NOTES(0) = ""
		PS_CASHREG::NOTES(1) = ""
		PS_CASHREG::LAST_INVNUM = ""
		PS_CASHREG::PETTYCASH = "?PettyCash?"

		CLOSE #PS_CASHREG.CH%

		CONTINUE 410
	END WHEN

410	!
	! Cash IN/OUT journal
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PS.OPEN]PS_CASHINOUT.OPN"
	USE
		CONTINUE 420
	END WHEN

420	!

 SortBy:
	GOTO ExitSortby IF SORTBY$ = ""
	SELECT SORTBY$

	CASE "D"
		K_NUM% = 0%
		TITLE(1%) = "ORDER-INVOICE JOURNAL " + BATCH_NO$ + &
			" BY DOCUMENT NUMBER"
		FROM_ITEM$ = SPACE$(LEN(OE_ORDERJOUR::ORDNUM) - &
			LEN(FROM_ITEM$)) + FROM_ITEM$
		TO_ITEM$ = SPACE$(LEN(OE_ORDERJOUR::ORDNUM) - &
			LEN(TO_ITEM$)) + TO_ITEM$

	CASE "T"
		K_NUM% = 1%
		TITLE(1%) = "ORDER-INVOICE JOURNAL " + BATCH_NO$ + &
			" BY SALES TYPE"

	CASE "N"
		K_NUM% = 2%
		TITLE(1%) = "ORDER-INVOICE JOURNAL " + BATCH_NO$ + &
			" BY CUSTOMER NUMBER"

	CASE "C"
		K_NUM% = 3%
		TITLE(1%) = "ORDER-INVOICE JOURNAL " + BATCH_NO$ + &
			" BY ORDER CATEGORY"
	END SELECT

	TITLE(4%) = "Document#  ST  Cat  CusNumber  CustomerName" + &
		"                       Invoice# InvDate    " + &
		"ShipDate    Terms Operator   Location"

	TITLE(5%) = "                       SalesTax      Discount  " + &
		"     Freight      Handling          Misc  "      + &
		"MiscAcct            ShipVia      InvTotal"

	TITLE(6%) = "."

	!
	! Set values to zero
	!
	CTR% = 0%
	FREIGHT_ST, ORDDISC_ST, MISC_ST, TAX_ST, TOTAL_ST = 0.0
	BATCH_NUMBER = "OOOOOO"

	GOTO StartReport

 ExitSortby:
	!******************************************************************
	! Check if posting process has been interrupted
	!******************************************************************

	!
	! Open up batch control file and check if interrupted
	!
	INTR_STATUS = ASSG_POSTBATCH(OPT_RESTART, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "OE_ORDERJOUR", BATCH_NO$, &
		IC.INTER.PERIOD, IC.INTER.PERIOD)

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
			GOTO Aborted &
				IF OE_TRAN_POST(OPT_RESTART, &
				SUBOPT_NOOPT, BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", "") <> CMC$_NORMAL

			GOTO Aborted &
				IF MO_TRAN_POST(OPT_RESTART, &
				SUBOPT_NOOPT, BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", "") <> CMC$_NORMAL

			GOTO Aborted &
				IF WP_TRAN_POST(OPT_RESTART, &
				SUBOPT_NOOPT, BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", "") <> CMC$_NORMAL

			GOTO Aborted &
				IF WP_TRAN_POSTREQ(OPT_RESTART, &
				SUBOPT_NOOPT, BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "") <> CMC$_NORMAL

			GOTO Aborted &
				IF IC_TRAN_POST(OPT_RESTART, &
				SUBOPT_NOOPT, BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", &
				IC.INTER.PERIOD) <> CMC$_NORMAL

			GOTO Aborted &
				IF GL_TRAN_POSTGL(OPT_RESTART, &
				SUBOPT_NOOPT, BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", "", &
				IC.INTER.PERIOD) <> CMC$_NORMAL

			GOTO Aborted &
				IF AR_TRAN_POSTAR(OPT_RESTART, &
				SUBOPT_NOOPT, BATCH_NUMBER, TITLE(), &
				UTL_REPORTX, "", "", "", &
				IC.INTER.PERIOD) <> CMC$_NORMAL

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
	GOTO Aborted &
		IF ASSG_POSTBATCH(OPT_ASSIGN, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "OE_ORDERJOUR", BATCH_NO$, &
		ICPERIOD, ICPERIOD) <> CMC$_NORMAL

 StartReport:
	EXIT_STATUS = OE_TRAN_POST(OPT_CHECK, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "", "")

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

	EXIT_STATUS = MO_TRAN_POST(OPT_CHECK, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "", "")

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

	EXIT_STATUS = WP_TRAN_POST(OPT_CHECK, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "", "")

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

	EXIT_STATUS = WP_TRAN_POSTREQ(OPT_CHECK, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "")

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

	!******************************************************************
	! Create transmittal
	!******************************************************************
	IF SORTBY$ = ""
	THEN
		GOTO Aborted &
			IF ASSG_POSTBATCH(OPT_ADDREC, BATCH_NUMBER, TITLE(), &
			UTL_REPORTX, "OE_ORDERJOUR", BATCH_NO$, "", "") <> &
			CMC$_NORMAL
	END IF

	!
	! If from item is blank then reset Header file
	! else try to find the first record
	!
3000	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #OE_ORDERJOUR.CH%, KEY #K_NUM%
		ELSE
			FIND #OE_ORDERJOUR.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE 3500 IF ERR = 155%
		FILENAME$ = "OE_ORDERJOUR"
		CONTINUE HelpError
	END WHEN

	POSTDATE = DATE_TODAY
	POSTTIME = TIME_NOW
	SLINE% = 0%

 ReadHeader:
3020	!
	! Read in one record from the header file
	!
	WHEN ERROR IN
		GET #OE_ORDERJOUR.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 3500 IF ERR = 11%
		FILENAME$ = "OE_ORDERJOUR"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record if should be printed
	!
	SELECT SORTBY$

	CASE "D"
		GOTO 3500 IF (OE_ORDERJOUR::ORDNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO ReadHeader IF COMP_ARRAY(EDIT$( &
			OE_ORDERJOUR::ORDNUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO 3500 IF (OE_ORDERJOUR::ORDTYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO ReadHeader IF COMP_ARRAY(EDIT$( &
			OE_ORDERJOUR::ORDTYPE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		GOSUB SubTotal IF (OE_ORDERJOUR::ORDTYPE <> &
			LAST_ORDTYPE$) AND CTR%

		LAST_ORDTYPE$ = OE_ORDERJOUR::ORDTYPE

	CASE "N"
		GOTO 3500 IF (OE_ORDERJOUR::CUSNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO ReadHeader IF COMP_ARRAY(EDIT$( &
			OE_ORDERJOUR::CUSNUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		GOSUB SubTotal IF (OE_ORDERJOUR::CUSNUM <> &
			LAST_CUSNUM$) AND CTR%

		LAST_CUSNUM$ = OE_ORDERJOUR::CUSNUM

	CASE "C"
		GOTO 3500 IF (OE_ORDERJOUR::ORDCAT > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO ReadHeader IF COMP_ARRAY(EDIT$( &
			OE_ORDERJOUR::ORDCAT, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		GOSUB SubTotal IF (OE_ORDERJOUR::ORDCAT <> &
			LAST_ORDCAT$) AND CTR%

		LAST_ORDCAT$ = OE_ORDERJOUR::ORDCAT

	END SELECT

3030	!
	! Better having no description than the wrong one.
	!
	TOPJOB$ = ""
	TOPDESC$ = ""

	IF OE_ORDERJOUR::CUSNUM <> ""
	THEN
		!
		! Check customer number
		!
		EXIT_STATUS = AR_EXAM_CUSTOM(OE_ORDERJOUR::CUSNUM, &
			AR_35CUSTOM_EXAM)

		SELECT EXIT_STATUS

		!
		! Success; go on
		!
		CASE CMC$_NORMAL

		!
		! Undefined; set flag and go on
		!
		CASE CMC$_UNDEFINED
			TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT) + &
				" (CustNumber)* "  + &
				OE_ORDERJOUR::CUSNUM

			!
			! Keep undefined codes
			!
			GOTO Aborted &
				IF OUTP_UNDEFCODES(OPT_ADDREC, &
				TITLE(), UTL_REPORTX, TEXT$) <> &
				CMC$_NORMAL

			!
			! Flag customer as undefined.
			!
			AR_35CUSTOM_EXAM::CUSNAM = "(Undefined)"

		!
		! Untrapped error
		!
		CASE ELSE
			GOTO Aborted

		END SELECT

	ELSE
		!
		! Check location
		!
		EXIT_STATUS = UTL_EXAM_LOCATION(OE_ORDERJOUR::SHIPLIN, &
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
			TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT) + &
				" (ShpLocation)* " + &
				OE_ORDERJOUR::SHIPLIN

			!
			! Keep undefined codes
			!
			GOTO Aborted &
				IF OUTP_UNDEFCODES(OPT_ADDREC, &
				TITLE(), UTL_REPORTX, TEXT$) <> &
				CMC$_NORMAL

		!
		! Untrapped error
		!
		CASE ELSE
			GOTO Aborted

		END SELECT

		AR_35CUSTOM_EXAM::CUSNAM = OE_ORDERJOUR::SHIPNAM
	END IF

	!
	! Check location
	!
	EXIT_STATUS = UTL_EXAM_LOCATION(OE_ORDERJOUR::LOCATION, &
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
		TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT) + &
			" (Location)* " + &
			OE_ORDERJOUR::LOCATION

		!
		! Keep undefined codes
		!
		GOTO Aborted &
			IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), &
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
		TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT) + " " + &
			OE_ORDERJOUR::ORDTYPE + "  " + &
			OE_ORDERJOUR::ORDCAT + " " + &
			OE_ORDERJOUR::CUSNUM + " " + &
			LEFT$(AR_35CUSTOM_EXAM::CUSNAM, 34%) + " " + &
			OE_ORDERJOUR::INVNUM + " " + &
			PRNT_DATE(OE_ORDERJOUR::TRANDATE, 8%) + " " + &
			PRNT_DATE(OE_ORDERJOUR::SHIPDATE, 8%) + "  " + &
			OE_ORDERJOUR::TERMS + "    " + &
			OE_ORDERJOUR::OPERATOR + " " + &
			OE_ORDERJOUR::LOCATION

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 2%)
	END IF

	!
	! Read expense accounts
	!
	READ_ACCOUNT% = OE_READ_ACCOUNT(AR_35CUSTOM_EXAM::TTYPE, &
		OE_ORDERJOUR::ORDTYPE, OE_ORDERJOUR::LOCATION, OE_ACCOUNT_READ)

	!
	! Find the first line item for the header
	!
	ORDER_TOT = 0.0
	ORDER_TAX = 0.0
	ORDER_DISC = 0.0
	LTOTAL = 0.0
	LCOST = 0.0
	LNOTAX = 0.0
	GL_HEADER% = 0%
	NEWLIN% = 0%
	OPLIN% = 0%

	!
	! Don't worry if order doesn't exist in the register file
	!
	IF OE_ORDERJOUR::REG_FLAG = "Y"
	THEN
		LIN$ = "    "
 MORegLine:
		IF MO_READ_REGLINE(OE_ORDERJOUR::ORDNUM, &
			LIN$, "GT", MO_REGLINE_READ, QTY()) = CMC$_NORMAL
		THEN
			LIN$ = MO_REGLINE_READ::LIN
			WHEN ERROR IN
				NEWLIN% = VAL%(LIN$)
			USE
				CONTINUE 3100 IF ERR = 52%
				FILENAME$ = "OE_ORDERJOUR"
				CONTINUE HelpError
			END WHEN
			GOTO MORegLine
		END IF
	END IF

	%PAGE

3100	!*******************************************************************
	! Handle MO Order Line file
	!*******************************************************************

	WHEN ERROR IN
		FIND #MO_ORDERLINE.CH%, KEY #0% EQ OE_ORDERJOUR::ORDNUM
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE OEOrderLine IF ERR = 155% OR ERR = 9%
		FILENAME$ = "MO_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	IF SORTBY$ <> ""
	THEN
		TEXT$ = STRING$(LEN(OE_ORDERJOUR::ORDNUM), A"."B) + &
			"  OdLine  Make        Year  Mtype  " + &
			"Msize   MdlCode        OrdQty        InvQty  " + &
			"   UnitPrice  Discount      ExtPrice"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		TEXT$ = STRING$(LEN(OE_ORDERJOUR::ORDNUM), A"."B) + &
			"        OpLine  OptGroup  Option  " + &
			"      OrdQty        InvQty     UnitPrice  " + &
			"    ExtPrice"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
	END IF

 MOLineItem:
	!
	! Get the (next) line item
	!
3110	WHEN ERROR IN
		GET #MO_ORDERLINE.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE OEOrderLine IF ERR = 11%
		FILENAME$ = "OE_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	!
	! Finish up the header if we're done with the line items
	!
	GOTO OEOrderLine IF MO_ORDERLINE::ORDNUM <> OE_ORDERJOUR::ORDNUM

	IF MO_ORDERLINE::LIN = "NEWL"
	THEN
		NEWLIN%, LIN% = NEWLIN% + 1%
	ELSE
		LIN% = VAL%(MO_ORDERLINE::LIN)
	END IF

	CALL SUBR_TRANTYPE(OE_ORDERJOUR::CUSNUM, &
		MO_ORDERLINE::LIN, &
		MO_ORDERLINE::ORDQTY, &
		MO_ORDERLINE::SHPQTY, MO_ORDERLINE::BCKQTY, &
		TRANTYPE$(), TRANQTY())

	PROMO = 0.0
	DISCOUNT = 0.0
	MISCH = 0.0
	MISCH2 = 0.0
	EXTPRICE = 0.0
	EXTCOST = 0.0
	FEXTPRICE = 0.0

	MO_REGLINE::ORDNUM	= MO_ORDERLINE::ORDNUM
	MO_REGLINE::LIN		= FORMAT$(LIN%, "<0>###")
	MO_REGLINE::PRODUCT	= MO_ORDERLINE::PRODUCT
	MO_REGLINE::TDATE	= MO_ORDERLINE::REQDATE
	MO_REGLINE::PRICE	= MO_ORDERLINE::PRICE
	MO_REGLINE::COST	= MO_ORDERLINE::COST
	MO_REGLINE::DISCOUNT	= MO_ORDERLINE::DISCOUNT
	MO_REGLINE::POSTDATE	= POSTDATE
	MO_REGLINE::POSTTIME	= POSTTIME
	MO_REGLINE::BATCH	= BATCH_NUMBER
	MO_REGLINE::PERIOD	= ICPERIOD
	MO_REGLINE::NOTES(0%)	= MO_ORDERLINE::NOTES(0%)
	MO_REGLINE::NOTES(1%)	= MO_ORDERLINE::NOTES(1%)
	MO_REGLINE::MAKE	= MO_ORDERLINE::MAKE
	MO_REGLINE::YEAR	= MO_ORDERLINE::YEAR
	MO_REGLINE::MTYPE	= MO_ORDERLINE::MTYPE
	MO_REGLINE::MSIZE	= MO_ORDERLINE::MSIZE
	MO_REGLINE::MODELCODE	= MO_ORDERLINE::MODELCODE
	MO_REGLINE::IDNUM	= MO_ORDERLINE::IDNUM

	SELECT INTEGER(TRANQTY(0%) + .001)

	! RT
	CASE 2%

		IF MO_ORDERLINEOPT::LIN <> "NEWL"
		THEN
			MO_REGLINE::TRANTYPE	= "02"
		ELSE
			MO_REGLINE::TRANTYPE	= "01"
		END IF

		MO_REGLINE::QTY = -TRANQTY(1%)
		MO_REGLINE::SHIPNO = "00"

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF MO_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			MO_REGLINE, MO_REGLINEOPT) <> CMC$_NORMAL

	! SA
	CASE 4%

		IF MO_ORDERLINE::LIN <> "NEWL"
		THEN
			MO_REGLINE::SHIPNO	= "01"
		ELSE
			MO_REGLINE::TRANTYPE	= "01"
			MO_REGLINE::QTY		= -TRANQTY(1%)
			MO_REGLINE::SHIPNO	= "00"

			!
			! Call the post function for order
			!
			GOTO Aborted &
				IF MO_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				MO_REGLINE, MO_REGLINEOPT) <> CMC$_NORMAL
		END IF

		MO_REGLINE::TRANTYPE	= "02"
		MO_REGLINE::QTY		= -TRANQTY(1%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF MO_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			MO_REGLINE, MO_REGLINEOPT) <> CMC$_NORMAL

	! TR
	CASE 8%

	! Credit
	CASE 16%
		!
		! Credit only - no inventory
		!

	! RT, Credit
	CASE 18%


	! SA, OE
	CASE 68%

		MO_REGLINE::SHIPNO	= "00"
		MO_REGLINE::TRANTYPE	= "22"
		MO_REGLINE::QTY		= TRANQTY(2%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF MO_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			MO_REGLINE, MO_REGLINEOPT) <> CMC$_NORMAL

	! Cancel only
	CASE 128%

		MO_REGLINE::SHIPNO	= "00"
		MO_REGLINE::TRANTYPE	= "03"
		MO_REGLINE::QTY		= TRANQTY(1%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF MO_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			MO_REGLINE, MO_REGLINEOPT) <> CMC$_NORMAL

		GOTO GoToNextMO

	! OE
	CASE 256%

		MO_REGLINE::SHIPNO	= "00"
		MO_REGLINE::TRANTYPE	= "01"
		MO_REGLINE::QTY		= -TRANQTY(1%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF MO_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			MO_REGLINE, MO_REGLINEOPT) <> CMC$_NORMAL

		GOTO GoToNextMO

	! SA, OE
	CASE 260%

		IF MO_ORDERLINE::LIN = "NEWL"
		THEN
			MO_REGLINE::SHIPNO	= "00"
			MO_REGLINE::TRANTYPE	= "01"
			MO_REGLINE::QTY		= -TRANQTY(1%) - TRANQTY(2%)

			!
			! Call the post function for order
			!
			GOTO Aborted &
				IF MO_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				MO_REGLINE, MO_REGLINEOPT) <> CMC$_NORMAL
		END IF

		MO_REGLINE::SHIPNO	= "01"
		MO_REGLINE::TRANTYPE	= "02"
		MO_REGLINE::QTY		= -TRANQTY(1%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF MO_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			MO_REGLINE, MO_REGLINEOPT) <> CMC$_NORMAL

	! SA, LS, OE
	CASE 324%

		MO_REGLINE::SHIPNO	= "00"
		MO_REGLINE::TRANTYPE	= "01"
		MO_REGLINE::QTY		= -TRANQTY(1%) - TRANQTY(2%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF MO_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			MO_REGLINE, MO_REGLINEOPT) <> CMC$_NORMAL

		MO_REGLINE::SHIPNO	= "00"
		MO_REGLINE::TRANTYPE	= "02"
		MO_REGLINE::QTY		= -TRANQTY(1%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF MO_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			MO_REGLINE, MO_REGLINEOPT) <> CMC$_NORMAL

	! SA,SO
	CASE 516%

		MO_REGLINE::SHIPNO	= "01"
		MO_REGLINE::TRANTYPE	= "02"
		MO_REGLINE::QTY		= -TRANQTY(1%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF MO_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			MO_REGLINE, MO_REGLINEOPT) <> CMC$_NORMAL


	! SA,SO,LS
	CASE 580%

		MO_REGLINE::SHIPNO	= "01"
		MO_REGLINE::TRANTYPE	= "02"
		MO_REGLINE::QTY		= -TRANQTY(1%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF MO_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			MO_REGLINE, MO_REGLINEOPT) <> CMC$_NORMAL

	CASE ELSE
		TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT) + " " + &
			FORMAT$(LIN%, "###") + " " + &
			MO_ORDERLINE::PRODUCT + " " + &
			"Undefined MOOrdl transaction.." + &
			NUM1$(TRANQTY(0%))

		!
		! Keep undefined codes
		!
		GOTO Aborted &
			IF OUTP_UNDEFCODES(OPT_ADDREC, &
			TITLE(), UTL_REPORTX, TEXT$) <> CMC$_NORMAL

		GOTO GoToNextMO

	END SELECT

	!
	! Lets hang onto the first lines description
	! for the finished product
	!
	IF (MO_ORDERLINE::ORDNUM <> TOPJOB$)
	THEN
		TOPJOB$ = MO_ORDERLINE::ORDNUM
		TOPDESC$ = MO_ORDERLINE::MAKE + " " + &
			MO_ORDERLINE::MODELCODE
	END IF


	!
	! Generate a JC_JOB record to pass through to the
	! post function
	!
	IF MO_ORDERLINE::IDNUM <> ""
	THEN
		!
		! Test serial number
		!
		EXIT_STATUS = PD_EXAM_PRODUCT(MO_ORDERLINE::IDNUM, &
			PD_PRODUCT_EXAM)

		SELECT EXIT_STATUS

		!
		! Code found; go on
		!
		CASE CMC$_WARNING

		!
		! Product number undefined; set flag and go on
		!
		CASE CMC$_UNDEFINED, CMC$_NORMAL

			!
			! Test product number
			!
			IF PD_EXAM_PRODUCT(MO_ORDERLINE::PRODUCT, &
				PD_PRODUCT_EXAM) <> CMC$_NORMAL
			THEN
				TEXT$ = CONV_STRING( &
					OE_ORDERJOUR::ORDNUM, &
					CMC$_LEFT) + " " + &
					FORMAT$(LIN%, "###") + &
					" (OEOrdl ProdNum)* " + &
					OE_ORDERLINE::PRODUCT

				!
				! Keep undefined codes
				!
				GOTO Aborted &
					IF OUTP_UNDEFCODES(OPT_ADDREC, &
					TITLE(), &
					UTL_REPORTX, TEXT$) <> &
					CMC$_NORMAL

			END IF

			GOSUB 18100

			JC_JOB::JOB		= MO_ORDERLINE::IDNUM
			JC_JOB::SUBJECT		= "E"
			JC_JOB::DESCR		= MO_ORDERLINE::MAKE + " " + &
				MO_MODELCODE::DESCR
			JC_JOB::TTYPE		= PD_PRODUCT_EXAM::PROD_TYPE
			JC_JOB::CLASS		= PD_PRODUCT_EXAM::CATEGORY
			JC_JOB::BDATE		= OE_ORDERJOUR::ORDDATE
			JC_JOB::SSTATUS		= "A"
			JC_JOB::EDATE		= ""
			JC_JOB::LOCATION	= OE_ORDERJOUR::LOCATION
			JC_JOB::OPERATOR	= OE_ORDERJOUR::OPERATOR
			JC_JOB::REFNO		= &
				CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT)
			JC_JOB::BATCH		= BATCH_NUMBER
			JC_JOB::POST_TIME	= POSTTIME
			JC_JOB::POST_DATE	= POSTDATE

			!
			! Call the post function
			!
			GOTO Aborted &
				IF WP_TRAN_POST(OPT_ADDREC, &
				SUBOPT_REGISTER, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				JC_JOB, WP_REGLINE) <> CMC$_NORMAL

		!
		! Something's going wrong
		!
		CASE ELSE
			GOTO Aborted

		END SELECT
	ELSE

		!
		! Test product number
		!
		EXIT_STATUS = PD_EXAM_PRODUCT(MO_ORDERLINE::PRODUCT, &
			PD_PRODUCT_EXAM)

		SELECT EXIT_STATUS

		!
		! Code found; go on
		!
		CASE CMC$_NORMAL, CMC$_WARNING

		!
		! Product number undefined; set flag and go on
		!
		CASE CMC$_UNDEFINED

			TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, &
				CMC$_LEFT) + " " + &
				FORMAT$(LIN%, "###") + &
				" (OEOrdl ProdNum)* " + &
				OE_ORDERLINE::PRODUCT

			!
			! Keep undefined codes
			!
			GOTO Aborted &
				IF OUTP_UNDEFCODES(OPT_ADDREC, &
				TITLE(), &
				UTL_REPORTX, TEXT$) <> CMC$_NORMAL

		!
		! Something's going wrong
		!
		CASE ELSE
			GOTO Aborted

		END SELECT

	END IF

	GOTO GotoNextMO IF POSTGL$ = "N" OR POSTGL$ = "M"

	GL_YYYY_PP::SOURCE	= "OIJ"
	GL_YYYY_PP::REFNO	= OE_ORDERJOUR::INVNUM
	GL_YYYY_PP::TRANDAT	= OE_ORDERJOUR::ORDDATE
	GL_YYYY_PP::DESCR	= TOPDESC$
	GL_YYYY_PP::XREFNO	= OE_ORDERJOUR::CUSNUM
	GL_YYYY_PP::CKNO	= ""
	GL_YYYY_PP::TRANKEY	= MO_ORDERLINE::PRODUCT
	GL_YYYY_PP::SUBACC	= MO_ORDERLINE::IDNUM
	GL_YYYY_PP::OPERATION	= ""
	GL_YYYY_PP::HOURS	= 0.0
	GL_YYYY_PP::UPDSTA	= ""
	GL_YYYY_PP::POSDAT	= POSTDATE
	GL_YYYY_PP::POSTIM	= POSTTIME
	GL_YYYY_PP::BTHNUM	= BATCH_NUMBER

	!
	! post inventory and COS to GL
	!
	SELECT INTEGER(TRANQTY(0%) + .01)

	CASE 2%, 4%, 8%, 18%, 32%, 34%, 36%, 68%, 260%, 320%, &
		324%, 516%, 580%

		GL_HEADER% = -1%

		EXIT_STATUS = PD_READ_ACCOUNT(OE_ORDERJOUR::LOCATION, &
			PD_PRODUCT_EXAM::PROD_TYPE, PD_ACCOUNT_READ)

		!
		! Is inventory account number defined?
		!
		EXIT_STATUS = GL_EXAM_CHART(PD_ACCOUNT_READ::INVACCT, &
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

			TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, &
				CMC$_LEFT) + " " + &
				FORMAT$(LIN%, "###") + &
				"  (MOOrdl Inv Acct)* " + &
				PD_ACCOUNT_READ::INVACCT

			!
			! Keep undefined codes
			!
			GOTO Aborted &
				IF OUTP_UNDEFCODES(OPT_ADDREC, &
				TITLE(), UTL_REPORTX, TEXT$) <> &
				CMC$_NORMAL

		!
		! Weird happenin's
		!
		CASE ELSE
			GOTO Aborted

		END SELECT

		COST = FUNC_ROUND(-MO_ORDERLINE::COST * TRANQTY(1%), 2%)

		!
		! Inventory
		!
		GL_YYYY_PP::ACCT	= PD_ACCOUNT_READ::INVACCT
		GL_YYYY_PP::AMOUNT	= -COST
		GL_YYYY_PP::UNITS	= -TRANQTY(1%)

		EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, &
			BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, &
			ICPERIOD)

		IF READ_ACCOUNT% = CMC$_NORMAL AND &
			OE_ACCOUNT_READ::COSACCT <> "Order COS Acct"
		THEN
			PD_ACCOUNT_READ::COSACCT = OE_ACCOUNT_READ::COSACCT
		ELSE
			IF (TRANQTY(0%) = 8.0) AND (OE_ORDERJOUR::CUSNUM = "")
			THEN
				EXIT_STATUS = &
					PD_READ_ACCOUNT(OE_ORDERJOUR::SHIPLIN, &
					PD_PRODUCT_EXAM::PROD_TYPE, &
					PD_ACCOUNT_TRAN)

				PD_ACCOUNT_READ::COSACCT = &
					PD_ACCOUNT_TRAN::INVACCT
			END IF

		END IF

		!
		! Is COS account number defined?
		!
		EXIT_STATUS = GL_EXAM_CHART(PD_ACCOUNT_READ::COSACCT, &
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

			TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, &
				CMC$_LEFT) + " " + &
				FORMAT$(LIN%, "###") + &
				"  (MOOrdl COS Acct)* " + &
				PD_ACCOUNT_READ::COSACCT

			!
			! Keep undefined codes
			!
			GOTO Aborted &
				IF OUTP_UNDEFCODES(OPT_ADDREC, &
				TITLE(), UTL_REPORTX, TEXT$) <> &
				CMC$_NORMAL

		!
		! Weird happenin's
		!
		CASE ELSE
			GOTO Aborted

		END SELECT

		GL_YYYY_PP::ACCT	= PD_ACCOUNT_READ::COSACCT
		GL_YYYY_PP::AMOUNT	= COST
		GL_YYYY_PP::UNITS	= TRANQTY(1%)

		EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, &
			BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, &
			ICPERIOD)

	END SELECT

	!
	! post revenue to GL
	!
	SELECT INTEGER(TRANQTY(0%) + .01)

	CASE 4%, 8%, 16%, 18%, 36%, 68%, 260%, 320%, 324%, 516%, 580%

		GL_HEADER% = -1%

		IF READ_ACCOUNT% <> CMC$_NORMAL OR &
			OE_ACCOUNT_READ::SALES = "Order Sales Acct"
		THEN
			V% = UTL_READ_TRANSACCT(OE_ORDERJOUR::LOCATION, &
				LEFT(TRANTYPE$(1%), 2%), &
				PD_PRODUCT_EXAM::PROD_TYPE, &
				UTL_TRANSACCT_READ)
		ELSE
			UTL_TRANSACCT_READ::ACCOUNT = OE_ACCOUNT_READ::SALES
		END IF

		EXIT_STATUS = GL_EXAM_CHART(UTL_TRANSACCT_READ::ACCOUNT, &
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
			TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, &
				CMC$_LEFT) + &
				FORMAT$(LIN%, " ###") + &
				"  (MOOrdLin Trans Acct)* " + &
				UTL_TRANSACCT_READ::ACCOUNT

			!
			! Keep undefined codes
			!
			GOTO Aborted &
				IF OUTP_UNDEFCODES(OPT_ADDREC, &
				TITLE(), UTL_REPORTX, TEXT$) <> &
				CMC$_NORMAL

		!
		! Weird happenin's
		!
		CASE ELSE
			GOTO Aborted

		END SELECT

		EXTPRICE = FUNC_ROUND(-TRANQTY(1%) * MO_ORDERLINE::PRICE, 2%)
		EXTCOST = FUNC_ROUND(-TRANQTY(1%) * MO_ORDERLINE::COST, 2%)

		EXTACCOUNT$ = UTL_TRANSACCT_READ::ACCOUNT

		GL_YYYY_PP::ACCT	= UTL_TRANSACCT_READ::ACCOUNT
		GL_YYYY_PP::AMOUNT	= -EXTPRICE
		GL_YYYY_PP::UNITS	= -TRANQTY(1%)

		EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, &
			BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, GL_YYYY_PP, &
			GL_CHART_EXAM, ICPERIOD)

		DISCOUNT = -FUNC_ROUNDLOW(TRANQTY(1%) * &
			MO_ORDERLINE::PRICE * &
			MO_ORDERLINE::DISCOUNT / 100.0, 2%)

	END SELECT

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
			TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, &
				CMC$_LEFT) + &
				FORMAT$(LIN%, " ###") + &
				"  (MOOrdLin Disc Acct)* " + &
				PD_ACCOUNT_READ::DISCACCT

			!
			! Keep undefined codes
			!
			GOTO Aborted &
				IF OUTP_UNDEFCODES(OPT_ADDREC, &
				TITLE(), UTL_REPORTX, TEXT$) <> &
				CMC$_NORMAL
		!
		! Weird happenin's
		!
		CASE ELSE
			GOTO Aborted

		END SELECT

		GL_YYYY_PP::ACCT	= PD_ACCOUNT_READ::DISCACCT
		GL_YYYY_PP::AMOUNT	= DISCOUNT
		GL_YYYY_PP::UNITS	= TRANQTY(1%)

		EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, &
			BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, &
			ICPERIOD)

	END IF

 GoToNextMO:

	FEXTPRICE = (EXTPRICE - DISCOUNT)
	LTOTAL = LTOTAL + (EXTPRICE - DISCOUNT)
	LCOST = LCOST + EXTCOST

	IF SORTBY$ <> ""
	THEN
		!
		! Print one report line
		!
		TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT) + &
			"    " + &
			FORMAT$(LIN%, "####") + " " + &
			MO_ORDERLINE::MAKE + " " + &
			MO_ORDERLINE::YEAR + " " + &
			MO_ORDERLINE::MTYPE + " " + &
			MO_ORDERLINE::MSIZE + " " + &
			MO_ORDERLINE::MODELCODE + "    " + &
			MO_ORDERLINE::IDNUM + " "  + &
			FORMAT$(MO_ORDERLINE::ORDQTY, "#,###,###.##") + "  " + &
			FORMAT$(MO_ORDERLINE::SHPQTY, "#,###,###.##") + "  " + &
			FORMAT$(MO_ORDERLINE::PRICE, "#,###,###.##") + "    " + &
			FORMAT$(MO_ORDERLINE::DISCOUNT, "##.###") + "  " + &
			FORMAT$(FEXTPRICE, "#,###,###.##")

		CTR% = -1%

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT) + " " + &
			SPACE$(30%) + ".......... "

		TEXT$ = TEXT$ + RIGHT(TRANTYPE$(I%), 3%) &
			FOR I% = 1% TO VAL%(TRANTYPE$(0%))

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	END IF

	%PAGE

	!*********************************************************************
	! MO_ORDERLINEOPT
	!*********************************************************************

	NEWOPLIN% = 0%

	!
	! Don't worry if order doesn't exist in the register file
	!
	GOTO 3200 IF OE_ORDERJOUR::REG_FLAG <> "Y"

	LIN$ = "    "

 MORegLineOpt:
	IF MO_READ_REGLINEOPT(OE_ORDERJOUR::ORDNUM, &
		MO_ORDERLINE::LIN, LIN$, "GT", &
		MO_REGLINEOPT_READ, QTY()) = CMC$_NORMAL
	THEN
		LIN$ = MO_REGLINEOPT_READ::OPTLIN
		NEWOPLIN% = VAL%(LIN$)

		GOTO MORegLineOpt
	END IF

3200	WHEN ERROR IN
		FIND #MO_ORDERLINEOPT.CH%, KEY #0% EQ OE_ORDERJOUR::ORDNUM + &
			MO_ORDERLINE::LIN + MO_ORDERLINE::MAKE + &
			MO_ORDERLINE::MODELCODE
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE MOLineItem IF ERR = 155% OR ERR = 9%
		FILENAME$ = "MO_ORDERLINEOPT"
		CONTINUE HelpError
	END WHEN

 MOLineItemOpt:
	!
	! Get the (next) line item
	!
3210	WHEN ERROR IN
		GET #MO_ORDERLINEOPT.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE MOLineItem IF ERR = 11%
		FILENAME$ = "MO_ORDERLINEOPT"
		CONTINUE HelpError
	END WHEN

	!
	! Finish up the line if we're done with the line option items
	!
	GOTO MoLineItem IF MO_ORDERLINEOPT::ORDNUM + MO_ORDERLINEOPT::LIN + &
		MO_ORDERLINEOPT::MAKE + MO_ORDERLINEOPT::MODELCODE <> &
		OE_ORDERJOUR::ORDNUM + MO_ORDERLINE::LIN + &
		MO_ORDERLINE::MAKE + &
		MO_ORDERLINE::MODELCODE

	IF MO_ORDERLINEOPT::LINOPT = "NEWL"
	THEN
		NEWOPLIN%, OPLIN% = NEWOPLIN% + 1%
	ELSE
		OPLIN% = VAL%(MO_ORDERLINEOPT::LINOPT)
	END IF

	CALL SUBR_TRANTYPE(OE_ORDERJOUR::CUSNUM, MO_ORDERLINEOPT::LIN, &
		MO_ORDERLINEOPT::ORDQTY, &
		MO_ORDERLINEOPT::SHPQTY, MO_ORDERLINEOPT::BCKQTY, &
		TRANTYPE$(), TRANQTY())

	EXTPRICE = 0.0
	EXTCOST = 0.0
	FEXTPRICE = 0.0

	MO_REGLINEOPT::ORDNUM	= MO_ORDERLINEOPT::ORDNUM
	MO_REGLINEOPT::LIN	= MO_REGLINE::LIN
	MO_REGLINEOPT::OPTLIN	= FORMAT$(OPLIN%, "<0>###")
	MO_REGLINEOPT::PRODUCT	= MO_ORDERLINEOPT::PRODUCT
	MO_REGLINEOPT::TDATE	= MO_ORDERLINE::REQDATE
	MO_REGLINEOPT::PRICE	= MO_ORDERLINEOPT::PRICE
	MO_REGLINEOPT::COST	= MO_ORDERLINEOPT::COST
	MO_REGLINEOPT::OPTGROUP	= MO_ORDERLINEOPT::OPTGROUP
	MO_REGLINEOPT::OPTN	= MO_ORDERLINEOPT::OPTN
	MO_REGLINEOPT::POSTDATE	= POSTDATE
	MO_REGLINEOPT::POSTTIME	= POSTTIME
	MO_REGLINEOPT::BATCH	= BATCH_NUMBER
	MO_REGLINEOPT::PERIOD	= ICPERIOD
	MO_REGLINEOPT::OPTDESCR	= MO_ORDERLINEOPT::OPTDESCR

	SELECT INTEGER(TRANQTY(0%) + .01)

	CASE 2% ! RT

		IF MO_ORDERLINEOPT::LINOPT <> "NEWL"
		THEN
			MO_REGLINEOPT::TRANTYPE	= "02"
		ELSE
			MO_REGLINEOPT::TRANTYPE	= "01"
		END IF

		MO_REGLINEOPT::QTY	= -TRANQTY(1%)
		MO_REGLINEOPT::SHIPNO	= "00"

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF MO_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEMOPT, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			MO_REGLINE, MO_REGLINEOPT) <> CMC$_NORMAL

	CASE 4% ! SA

		IF MO_ORDERLINEOPT::LINOPT <> "NEWL"
		THEN
			MO_REGLINEOPT::SHIPNO	= "01"
		ELSE
			MO_REGLINEOPT::TRANTYPE	= "01"
			MO_REGLINEOPT::QTY	= -TRANQTY(1%)
			MO_REGLINEOPT::SHIPNO	= "00"

			!
			! Call the post function for order
			!
			GOTO Aborted &
				IF MO_TRAN_POST(OPT_ADDREC, &
				SUBOPT_LINEITEMOPT, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				MO_REGLINE, MO_REGLINEOPT) <> CMC$_NORMAL
		END IF

		MO_REGLINEOPT::TRANTYPE	= "02"
		MO_REGLINEOPT::QTY	= -TRANQTY(1%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF MO_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEMOPT, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			MO_REGLINE, MO_REGLINEOPT) <> CMC$_NORMAL

	CASE 8% ! TR

	CASE 16% ! Credit
		!
		! Credit only - no inventory
		!

	CASE 18% ! RT, Credit

	CASE 32% ! WA

		MO_REGLINEOPT::TRANTYPE	= "02"
		MO_REGLINEOPT::QTY	= -TRANQTY(1%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF MO_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEMOPT, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			MO_REGLINE, MO_REGLINEOPT)  <> CMC$_NORMAL

	CASE 128%	! Cancel only

		MO_REGLINEOPT::SHIPNO	= "00"
		MO_REGLINEOPT::TRANTYPE	= "03"
		MO_REGLINEOPT::QTY	= TRANQTY(1%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF MO_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEMOPT, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			MO_REGLINE, MO_REGLINEOPT) <> CMC$_NORMAL

		GOTO GoToNextMOOpt

	CASE 256%	! OE

		MO_REGLINEOPT::SHIPNO	= "00"
		MO_REGLINEOPT::TRANTYPE	= "01"
		MO_REGLINEOPT::QTY	= -TRANQTY(1%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF MO_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEMOPT, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			MO_REGLINE, MO_REGLINEOPT) <> CMC$_NORMAL

		GOTO GoToNextMOOpt

	CASE 260%	! SA, OE

		IF MO_ORDERLINEOPT::LINOPT = "NEWL"
		THEN
			MO_REGLINEOPT::SHIPNO	= "00"
			MO_REGLINEOPT::TRANTYPE	= "01"
			MO_REGLINEOPT::QTY	= -TRANQTY(1%) - TRANQTY(2%)

			!
			! Call the post function for order
			!
			GOTO Aborted &
				IF MO_TRAN_POST(OPT_ADDREC, &
				SUBOPT_LINEITEMOPT, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				MO_REGLINE, MO_REGLINEOPT) <> CMC$_NORMAL
		END IF

		MO_REGLINEOPT::SHIPNO	= "01"
		MO_REGLINEOPT::TRANTYPE	= "02"
		MO_REGLINEOPT::QTY	= -TRANQTY(1%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF MO_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEMOPT, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			MO_REGLINE, MO_REGLINEOPT) <> CMC$_NORMAL

	CASE 320%	! OE, LS

		MO_REGLINEOPT::SHIPNO	= "00"
		MO_REGLINEOPT::TRANTYPE	= "01"
		MO_REGLINEOPT::QTY	= -TRANQTY(2%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF MO_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEMOPT, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			MO_REGLINE, MO_REGLINEOPT) <> CMC$_NORMAL

		GOTO GoToNextMOOpt

	CASE 324%	! SA, LS, OE

		MO_REGLINEOPT::SHIPNO	= "00"
		MO_REGLINEOPT::TRANTYPE	= "01"
		MO_REGLINEOPT::QTY	= -TRANQTY(1%) - TRANQTY(2%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF MO_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEMOPT, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			MO_REGLINE, MO_REGLINEOPT) <> CMC$_NORMAL

		MO_REGLINEOPT::SHIPNO	= "00"
		MO_REGLINEOPT::TRANTYPE	= "02"
		MO_REGLINEOPT::QTY	= -TRANQTY(1%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF MO_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEMOPT, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			MO_REGLINE, MO_REGLINEOPT) <> CMC$_NORMAL

	CASE 516% ! SA,SO

		MO_REGLINEOPT::SHIPNO	= "01"
		MO_REGLINEOPT::TRANTYPE	= "02"
		MO_REGLINEOPT::QTY	= -TRANQTY(1%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF MO_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEMOPT, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			MO_REGLINE, MO_REGLINEOPT) <> CMC$_NORMAL

	CASE 580% ! SA,SO,LS

		MO_REGLINEOPT::SHIPNO	= "01"
		MO_REGLINEOPT::TRANTYPE	= "02"
		MO_REGLINEOPT::QTY	= -TRANQTY(1%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF MO_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEMOPT, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			MO_REGLINE, MO_REGLINEOPT) <> CMC$_NORMAL

	CASE ELSE
		TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT) + " " + &
			FORMAT$(LIN%, "###") + " " + &
			FORMAT$(OPLIN%, "###") + " " + &
			MO_ORDERLINEOPT::PRODUCT + " " + &
			"Undefined MOOptl transaction.." + &
			NUM1$(TRANQTY(0%))

		!
		! Keep undefined codes
		!
		GOTO Aborted &
			IF OUTP_UNDEFCODES(OPT_ADDREC, &
			TITLE(), UTL_REPORTX, TEXT$) <> CMC$_NORMAL

		GOTO GoToNextMOOpt

	END SELECT

 GoToNextMOOpt:
	IF SORTBY$ <> ""
	THEN
		!
		! Print one report line
		!
		TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT) + &
			"    " + &
			FORMAT$(LIN%, "####") + "  " + &
			FORMAT$(OPLIN%, "####") + "  " + &
			MO_ORDERLINEOPT::OPTGROUP + "        " + &
			MO_ORDERLINEOPT::OPTN + "    " + &
			FORMAT$(MO_ORDERLINEOPT::ORDQTY, "#,###,###.##") + &
			"  " + &
			FORMAT$(MO_ORDERLINEOPT::SHPQTY, "#,###,###.##")

		CTR% = -1%

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT) + " " + &
			SPACE$(30%) + ".......... "

		TEXT$ = TEXT$ + RIGHT(TRANTYPE$(I%), 3%) &
			FOR I% = 1% TO VAL%(TRANTYPE$(0%))

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	END IF

	GOTO MOLineItemOpt

	%PAGE

 OEOrderLine:
	!*****************************************************************
	! OE_ORDERLINE
	!*****************************************************************

	NEWLIN% = 0%

	!
	! Don't worry if order doesn't exist in the register file
	!
	GOTO 3300 IF OE_ORDERJOUR::REG_FLAG <> "Y"

	LIN$ = "    "

 RegLine:
	IF OE_READ_REGLINE(OE_ORDERJOUR::ORDNUM, &
		LIN$, "GT", OE_REGLINE_READ, QTY()) = CMC$_NORMAL
	THEN
		LIN$ = OE_REGLINE_READ::LIN
		NEWLIN% = VAL%(LIN$)

		GOTO RegLine
	END IF

3300	WHEN ERROR IN
		FIND #OE_ORDERLINE.CH%, KEY #0% EQ OE_ORDERJOUR::ORDNUM
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE ProcessHeader IF ERR = 155% OR ERR = 9%
		FILENAME$ = "OE_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	IF SORTBY$ <> ""
	THEN
		TEXT$ = STRING$(LEN(OE_ORDERJOUR::ORDNUM), A"."B) + &
			"    Line  Product#        Description" + &
			"            OrdQty        InvQty  " + &
			"   UnitPrice         Promo  Discount%      ExtPrice"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
	END IF

 LineItem:
	!
	! Get the (next) line item
	!
3310	WHEN ERROR IN
		GET #OE_ORDERLINE.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE ProcessHeader IF ERR = 11%
		FILENAME$ = "OE_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	!
	! Finish up the header if we're done with the line items
	!
	GOTO ProcessHeader IF OE_ORDERLINE::ORDNUM <> OE_ORDERJOUR::ORDNUM

	IF OE_ORDERLINE::LIN = "NEWL"
	THEN
		LIN%, NEWLIN% = NEWLIN% + 1%
	ELSE
		LIN% = VAL%(OE_ORDERLINE::LIN)
	END IF

	!
	! Test product number
	!
	EXIT_STATUS = PD_EXAM_PRODUCT(OE_ORDERLINE::PRODUCT, PD_PRODUCT_EXAM)

	SELECT EXIT_STATUS

	!
	! Code found; go on
	!
	CASE CMC$_NORMAL, CMC$_WARNING

	!
	! Product number undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, &
			CMC$_LEFT) + " " + &
			FORMAT$(LIN%, "###") + &
			" (OEOrdl ProdNum)* " + &
			OE_ORDERLINE::PRODUCT

		!
		! Keep undefined codes
		!
		GOTO Aborted &
			IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), &
			UTL_REPORTX, TEXT$) <> CMC$_NORMAL

	!
	! Something's going wrong
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	CALL SUBR_TRANTYPE(OE_ORDERJOUR::CUSNUM,OE_ORDERLINE::LIN, &
		OE_ORDERLINE::ORDQTY, OE_ORDERLINE::SHPQTY, &
		OE_ORDERLINE::BCKQTY, &
		TRANTYPE$(), TRANQTY())

	PROMO = 0.0
	DISCOUNT = 0.0
	MISCH = 0.0
	MISCH2 = 0.0
	EXTPRICE = 0.0
	EXTCOST = 0.0
	FEXTPRICE = 0.0
	SHIPQTY = 0.0

	OE_REGLINE::ORDNUM	= OE_ORDERLINE::ORDNUM
	OE_REGLINE::LIN		= FORMAT$(LIN%, "<0>###")
	OE_REGLINE::PRODUCT	= OE_ORDERLINE::PRODUCT
	IF (OE_ORDERLINE::ORDQTY <> 0.0) OR &
		(OE_ORDERLINE::REQDATE <= "00000000")
	THEN
		OE_REGLINE::TDATE	= OE_ORDERJOUR::SHIPDATE
	ELSE
		OE_REGLINE::TDATE	= OE_ORDERLINE::REQDATE
	END IF
	OE_REGLINE::PRICE	= OE_ORDERLINE::PRICE
	OE_REGLINE::DISCOUNT	= OE_ORDERLINE::DISCOUNT
	OE_REGLINE::COST	= OE_ORDERLINE::COST
	OE_REGLINE::POSTDATE	= POSTDATE
	OE_REGLINE::POSTTIME	= POSTTIME
	OE_REGLINE::BATCH	= BATCH_NUMBER
	OE_REGLINE::SHIPNO	= ""
	OE_REGLINE::REFNUM	= OE_ORDERJOUR::INVNUM
	OE_REGLINE::PROMO	= OE_ORDERLINE::PROMO
	OE_REGLINE::PERIOD	= ICPERIOD
	OE_REGLINE::MISCH	= OE_ORDERLINE::MISCH
	OE_REGLINE::MISCH2	= OE_ORDERLINE::MISCH2
	OE_REGLINE::NOTES1	= OE_ORDERLINE::NOTES1
	OE_REGLINE::NOTES2	= OE_ORDERLINE::NOTES2
	OE_REGLINE::SUBACCT	= OE_ORDERLINE::SUBACCT

	IC_TRANSACTION::PRODUCT		= OE_ORDERLINE::PRODUCT
	IC_TRANSACTION::LOCATION	= OE_ORDERJOUR::LOCATION
	IC_TRANSACTION::TRANS_DATE	= OE_ORDERJOUR::SHIPDATE
	IC_TRANSACTION::PRIMARY_REF	= &
		CONV_STRING(OE_ORDERLINE::ORDNUM, CMC$_LEFT) + &
		FORMAT$(LIN%, "<0>###")
	IC_TRANSACTION::CROSS_REF	= OE_ORDERJOUR::CUSNUM
	IC_TRANSACTION::SUBACCOUNT	= OE_ORDERJOUR::SALESMAN
	IC_TRANSACTION::LOT		= ""
	IC_TRANSACTION::STATIONMAN	= OE_ORDERJOUR::OPERATOR
	IC_TRANSACTION::TRANSACCT	= ""
	IC_TRANSACTION::POSTDATE	= POSTDATE
	IC_TRANSACTION::POSTTIME	= POSTTIME
	IC_TRANSACTION::BATCH		= BATCH_NUMBER

	SELECT INTEGER(TRANQTY(0%) + .01)

	CASE 2% ! RT

		IC_TRANSACTION::TYPE_A		= "RT"
		IC_TRANSACTION::QUANTITY_A	= TRANQTY(1%)
		IC_TRANSACTION::TYPE_B		= ""
		IC_TRANSACTION::QUANTITY_B	= 0.0

		IC_TRANSACTION::COST = &
			ABS(FUNC_ROUND(OE_ORDERLINE::COST * &
			TRANQTY(1%), 2%))

		IC_TRANSACTION::PRICE = &
			ABS(FUNC_ROUND(OE_ORDERLINE::PRICE * &
			TRANQTY(1%), 2%))

		!
		! Post shipping to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

	CASE 4%
		IC_TRANSACTION::TYPE_A		= "SA"
		IC_TRANSACTION::QUANTITY_A	= TRANQTY(1%)
		IC_TRANSACTION::TYPE_B		= ""
		IC_TRANSACTION::QUANTITY_B	= 0.0
		SHIPQTY = TRANQTY(1%)

		IF OE_ORDERLINE::LIN <> "NEWL"
		THEN
			IC_TRANSACTION::TYPE_B		= "SO"
			IC_TRANSACTION::QUANTITY_B	= OE_ORDERLINE::ORDQTY

			OE_REGLINE::SHIPNO	= "01"
		ELSE
			OE_REGLINE::TRANTYPE	= "01"
			OE_REGLINE::QTY		= -TRANQTY(1%)
			OE_REGLINE::SHIPNO	= "00"

			!
			! Call the post function for order
			!
			GOTO Aborted &
				IF OE_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				OE_REGHEADER, OE_REGLINE) <> CMC$_NORMAL
		END IF

		OE_REGLINE::TRANTYPE	= "02"
		OE_REGLINE::QTY		= -TRANQTY(1%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF OE_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			OE_REGHEADER, OE_REGLINE) <> CMC$_NORMAL

		IC_TRANSACTION::COST = &
			ABS(FUNC_ROUND(OE_ORDERLINE::COST * &
			TRANQTY(1%), 2%))

		IC_TRANSACTION::PRICE = &
			ABS(FUNC_ROUND(OE_ORDERLINE::PRICE * &
			TRANQTY(1%), 2%))

		!
		! Post shipping to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

	CASE 8% ! TR

		IC_TRANSACTION::TYPE_A		= "TR"
		IC_TRANSACTION::QUANTITY_A	= TRANQTY(1%)
		IC_TRANSACTION::TYPE_B		= ""
		IC_TRANSACTION::QUANTITY_B	= 0.0

		IC_TRANSACTION::COST = &
			ABS(FUNC_ROUND(-OE_ORDERLINE::COST * TRANQTY(1%), 2%))

		IC_TRANSACTION::PRICE = &
			ABS(FUNC_ROUND(-OE_ORDERLINE::PRICE * TRANQTY(1%), 2%))

		!
		! Post shipping to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

		IC_TRANSACTION::LOCATION	= OE_ORDERJOUR::SHIPLIN
		IC_TRANSACTION::TYPE_A		= "TR"
		IC_TRANSACTION::QUANTITY_A	= -TRANQTY(1%)
		IC_TRANSACTION::TYPE_B		= ""
		IC_TRANSACTION::QUANTITY_B	= 0.0

		!
		! Post shipping to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

	CASE 16% ! Credit
		!
		! Credit only - no inventory
		!

	CASE 18% ! RT, Credit

		IF OE_ORDERLINE::LIN <> "NEWL"
		THEN
			IC_TRANSACTION::TYPE_B		= "SO"
			IC_TRANSACTION::QUANTITY_B	= OE_ORDERLINE::ORDQTY

			OE_REGLINE::SHIPNO	= "01"
		ELSE
			IC_TRANSACTION::TYPE_B		= ""
			IC_TRANSACTION::QUANTITY_B	= 0.0

			OE_REGLINE::TRANTYPE	= "01"
			OE_REGLINE::QTY		= -TRANQTY(1%)
			OE_REGLINE::SHIPNO	= "00"

			!
			! Call the post function for order
			!
			GOTO Aborted &
				IF OE_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				OE_REGHEADER, OE_REGLINE) <> CMC$_NORMAL
		END IF

		OE_REGLINE::TRANTYPE	= "02"
		OE_REGLINE::QTY		= -TRANQTY(1%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF OE_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			OE_REGHEADER, OE_REGLINE) <> CMC$_NORMAL

		IC_TRANSACTION::TYPE_A		= "RT"
		IC_TRANSACTION::QUANTITY_A	= TRANQTY(1%)

		IC_TRANSACTION::COST = &
			ABS(FUNC_ROUND(-OE_ORDERLINE::COST * TRANQTY(1%), 2%))

		IC_TRANSACTION::PRICE = &
			ABS(FUNC_ROUND(-OE_ORDERLINE::PRICE * TRANQTY(1%), 2%))

		!
		! Post shipping to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

	CASE 32% ! WA

		OE_REGLINE::TRANTYPE	= "02"
		OE_REGLINE::QTY		= -TRANQTY(1%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF OE_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			OE_REGHEADER, OE_REGLINE) <> CMC$_NORMAL

		IC_TRANSACTION::TYPE_A		= "WA"
		IC_TRANSACTION::QUANTITY_A	= TRANQTY(1%)
		IC_TRANSACTION::TYPE_B		= ""
		IC_TRANSACTION::QUANTITY_B	= 0.0

		IC_TRANSACTION::COST = &
			ABS(FUNC_ROUND(-OE_ORDERLINE::COST * TRANQTY(1%), 2%))

		IC_TRANSACTION::PRICE = &
			ABS(FUNC_ROUND(-OE_ORDERLINE::PRICE * TRANQTY(1%), 2%))

		!
		! Post shipping to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

	CASE 34% ! WA, RT

		OE_REGLINE::TRANTYPE	= "02"
		OE_REGLINE::QTY		= -TRANQTY(2%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF OE_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			OE_REGHEADER, OE_REGLINE) <> CMC$_NORMAL

		IC_TRANSACTION::TYPE_A		= "WA"
		IC_TRANSACTION::QUANTITY_A	= TRANQTY(2%)
		IC_TRANSACTION::TYPE_B		= ""
		IC_TRANSACTION::QUANTITY_B	= 0.0

		IC_TRANSACTION::COST = &
			ABS(FUNC_ROUND(-OE_ORDERLINE::COST * TRANQTY(2%), 2%))

		IC_TRANSACTION::PRICE = &
			ABS(FUNC_ROUND(-OE_ORDERLINE::PRICE * TRANQTY(2%), 2%))

		!
		! Post shipping to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

		IC_TRANSACTION::TYPE_A		= "RT"
		IC_TRANSACTION::QUANTITY_A	= TRANQTY(1%)
		IC_TRANSACTION::TYPE_B		= ""
		IC_TRANSACTION::QUANTITY_B	= 0.0

		IC_TRANSACTION::COST = &
			ABS(FUNC_ROUND(OE_ORDERLINE::COST * &
			TRANQTY(1%), 2%))

		IC_TRANSACTION::PRICE = &
			ABS(FUNC_ROUND(OE_ORDERLINE::PRICE * &
			TRANQTY(1%), 2%))

		!
		! Post shipping to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

	CASE 36% ! SA, LS

		IC_TRANSACTION::TYPE_B		= ""
		IC_TRANSACTION::QUANTITY_B	= 0.0

		IF OE_ORDERLINE::LIN <> "NEWL"
		THEN
			IC_TRANSACTION::TYPE_A		= "SA"
			IC_TRANSACTION::QUANTITY_A	= TRANQTY(1%)
			IC_TRANSACTION::TYPE_B		= "SO"
			IC_TRANSACTION::QUANTITY_B	= OE_ORDERLINE::ORDQTY
			SHIPQTY = TRANQTY(1%)

			IC_TRANSACTION::COST = &
				ABS(FUNC_ROUND(-OE_ORDERLINE::COST * &
				TRANQTY(1%), 2%))

			IC_TRANSACTION::PRICE = &
				ABS(FUNC_ROUND(-OE_ORDERLINE::PRICE * &
				TRANQTY(1%), 2%))

			!
			! Post shipping to inventory transaction file
			!
			EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
				CHECK_PERIOD, BATCH_NUMBER, &
				TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

			IC_TRANSACTION::TYPE_A		= "LS"
			IC_TRANSACTION::QUANTITY_A	= TRANQTY(2%)
			IC_TRANSACTION::TYPE_B		= ""
			IC_TRANSACTION::QUANTITY_B	= 0.0

			IC_TRANSACTION::COST = &
				ABS(FUNC_ROUND(OE_ORDERLINE::COST * &
				TRANQTY(2%), 2%))

			IC_TRANSACTION::PRICE = &
				ABS(FUNC_ROUND(OE_ORDERLINE::PRICE * &
				TRANQTY(2%), 2%))

			!
			! Post shipping to inventory transaction file
			!
			EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
				CHECK_PERIOD, BATCH_NUMBER, &
				TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

			OE_REGLINE::SHIPNO	= "01"
		ELSE

			IC_TRANSACTION::TYPE_A		= "SA"
			IC_TRANSACTION::QUANTITY_A	= TRANQTY(1%)
			IC_TRANSACTION::TYPE_B		= ""
			IC_TRANSACTION::QUANTITY_B	= 0.0

			IC_TRANSACTION::COST = &
				ABS(FUNC_ROUND(-OE_ORDERLINE::COST * &
				TRANQTY(1%), 2%))

			IC_TRANSACTION::PRICE = &
				ABS(FUNC_ROUND(-OE_ORDERLINE::PRICE * &
				TRANQTY(1%), 2%))

			!
			! Post shipping to inventory transaction file
			!
			EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
				CHECK_PERIOD, BATCH_NUMBER, &
				TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

			IC_TRANSACTION::TYPE_A		= "SO"
			IC_TRANSACTION::QUANTITY_A	= -TRANQTY(2%)
			IC_TRANSACTION::TYPE_B		= "LS"
			IC_TRANSACTION::QUANTITY_B	= -TRANQTY(2%)

			IC_TRANSACTION::COST = &
				ABS(FUNC_ROUND(OE_ORDERLINE::COST * &
				TRANQTY(2%), 2%))

			IC_TRANSACTION::PRICE = &
				ABS(FUNC_ROUND(OE_ORDERLINE::PRICE * &
				TRANQTY(2%), 2%))

			!
			! Post shipping to inventory transaction file
			!
			EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
				CHECK_PERIOD, BATCH_NUMBER, &
				TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

			OE_REGLINE::TRANTYPE	= "01"
			OE_REGLINE::QTY		= -TRANQTY(1%)
			OE_REGLINE::SHIPNO	= "00"

			!
			! Call the post function for order
			!
			GOTO Aborted &
				IF OE_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				OE_REGHEADER, OE_REGLINE) <> CMC$_NORMAL
		END IF

		OE_REGLINE::TRANTYPE	= "02"
		OE_REGLINE::QTY		= -TRANQTY(1%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF OE_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			OE_REGHEADER, OE_REGLINE) <> CMC$_NORMAL

	CASE 64% ! LS

		IC_TRANSACTION::TRANS_DATE	= OE_ORDERJOUR::ORDDATE

		IF OE_ORDERLINE::LIN = "NEWL"
		THEN
			OE_REGLINE::SHIPNO	= "00"
			OE_REGLINE::TRANTYPE	= "01"
			OE_REGLINE::QTY		= TRANQTY(1%)

			!
			! Call the post function for order
			!
			GOTO Aborted &
				IF OE_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				OE_REGHEADER, OE_REGLINE) <> CMC$_NORMAL
		END IF

		OE_REGLINE::TRANTYPE	= "03"
		OE_REGLINE::QTY		= TRANQTY(1%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF OE_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			OE_REGHEADER, OE_REGLINE) <> CMC$_NORMAL

		IC_TRANSACTION::TYPE_A		= "LS"
		IC_TRANSACTION::QUANTITY_A	= TRANQTY(1%)

		IF OE_ORDERLINE::LIN = "NEWL"
		THEN
			IC_TRANSACTION::TYPE_B		= "SO"
			IC_TRANSACTION::QUANTITY_B	= -TRANQTY(1%)
		ELSE
			IC_TRANSACTION::TYPE_B		= ""
			IC_TRANSACTION::QUANTITY_B	= 0.0
		END IF

		IC_TRANSACTION::COST = &
			ABS(FUNC_ROUND(OE_ORDERLINE::COST * &
			TRANQTY(1%), 2%))

		IC_TRANSACTION::PRICE = &
			ABS(FUNC_ROUND(OE_ORDERLINE::PRICE * &
			TRANQTY(1%), 2%))

		!
		! Post shipping to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

	CASE 68% ! SA, LS

		IF OE_ORDERLINE::LIN = "NEWL"
		THEN
			OE_REGLINE::SHIPNO	= "00"
			OE_REGLINE::TRANTYPE	= "01"
			OE_REGLINE::QTY		= -TRANQTY(1%) + TRANQTY(2%)

			!
			! Call the post function for order
			!
			GOTO Aborted &
				IF OE_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				OE_REGHEADER, OE_REGLINE) <> CMC$_NORMAL
		END IF

		OE_REGLINE::SHIPNO	= "00"
		OE_REGLINE::TRANTYPE	= "02"
		OE_REGLINE::QTY		= TRANQTY(1%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF OE_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			OE_REGHEADER, OE_REGLINE) <> CMC$_NORMAL

		OE_REGLINE::SHIPNO	= "00"
		OE_REGLINE::TRANTYPE	= "03"
		OE_REGLINE::QTY		= -TRANQTY(2%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF OE_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			OE_REGHEADER, OE_REGLINE) <> CMC$_NORMAL


		IC_TRANSACTION::TYPE_A		= "LS"
		IC_TRANSACTION::QUANTITY_A	= TRANQTY(2%)

		IF OE_ORDERLINE::LIN = "NEWL"
		THEN
			IC_TRANSACTION::TYPE_B		= "SO"
			IC_TRANSACTION::QUANTITY_B	= -TRANQTY(2%)
		ELSE
			IC_TRANSACTION::TYPE_B		= ""
			IC_TRANSACTION::QUANTITY_B	= 0.0
		END IF

		IC_TRANSACTION::COST = &
			ABS(FUNC_ROUND(OE_ORDERLINE::COST * &
			TRANQTY(2%), 2%))

		IC_TRANSACTION::PRICE = &
			ABS(FUNC_ROUND(OE_ORDERLINE::PRICE * &
			TRANQTY(2%), 2%))

		!
		! Post shipping to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

		IC_TRANSACTION::TYPE_A		= "SA"
		IC_TRANSACTION::QUANTITY_A	= TRANQTY(1%)
		IC_TRANSACTION::TYPE_B		= ""
		IC_TRANSACTION::QUANTITY_B	= 0.0
		SHIPQTY = TRANQTY(1%)

		IC_TRANSACTION::COST = &
			ABS(FUNC_ROUND(OE_ORDERLINE::COST * &
			TRANQTY(1%), 2%))

		IC_TRANSACTION::PRICE = &
			ABS(FUNC_ROUND(OE_ORDERLINE::PRICE * &
			TRANQTY(1%), 2%))

		!
		! Post shipping to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)


	CASE 128%	! Cancel only

		OE_REGLINE::SHIPNO	= "00"
		OE_REGLINE::TRANTYPE	= "03"
		OE_REGLINE::QTY		= TRANQTY(1%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF OE_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			OE_REGHEADER, OE_REGLINE) <> CMC$_NORMAL

		IC_TRANSACTION::TYPE_A		= "SO"
		IC_TRANSACTION::QUANTITY_A	= TRANQTY(1%)
		IC_TRANSACTION::TYPE_B		= ""
		IC_TRANSACTION::QUANTITY_B	= 0.0

		IC_TRANSACTION::COST = &
			ABS(FUNC_ROUND(OE_ORDERLINE::COST * &
			TRANQTY(1%), 2%))

		IC_TRANSACTION::PRICE = &
			ABS(FUNC_ROUND(OE_ORDERLINE::PRICE * &
			TRANQTY(1%), 2%))

		!
		! Post shipping to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

		GOTO GoToNext

	CASE 136% ! TR, Cancel

		OE_REGLINE::SHIPNO	= "01"
		OE_REGLINE::TRANTYPE	= "02"
		OE_REGLINE::QTY		= TRANQTY(1%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF OE_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			OE_REGHEADER, OE_REGLINE) <> CMC$_NORMAL

		OE_REGLINE::SHIPNO	= "01"
		OE_REGLINE::TRANTYPE	= "01"
		OE_REGLINE::QTY		= TRANQTY(2%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF OE_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			OE_REGHEADER, OE_REGLINE) <> CMC$_NORMAL

		IC_TRANSACTION::TYPE_A		= "TR"
		IC_TRANSACTION::QUANTITY_A	= TRANQTY(1%)
		IC_TRANSACTION::TYPE_B		= "SO"
		IC_TRANSACTION::QUANTITY_B	= TRANQTY(2%)

		IC_TRANSACTION::COST = &
			ABS(FUNC_ROUND(-OE_ORDERLINE::COST * &
			(TRANQTY(1%) + TRANQTY(2%)), 2%))

		IC_TRANSACTION::PRICE = &
			ABS(FUNC_ROUND(-OE_ORDERLINE::PRICE * &
			(TRANQTY(1%) + TRANQTY(2%)), 2%))

		!
		! Post shipping to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

		IC_TRANSACTION::LOCATION	= OE_ORDERJOUR::SHIPLIN
		IC_TRANSACTION::TYPE_A		= "TR"
		IC_TRANSACTION::QUANTITY_A	= -TRANQTY(1%)
		IC_TRANSACTION::TYPE_B		= ""
		IC_TRANSACTION::QUANTITY_B	= 0.0

		IC_TRANSACTION::COST = &
			ABS(FUNC_ROUND(-OE_ORDERLINE::COST * TRANQTY(1%), 2%))

		IC_TRANSACTION::PRICE = &
			ABS(FUNC_ROUND(-OE_ORDERLINE::PRICE * TRANQTY(1%), 2%))

		!
		! Post shipping to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)


	CASE 256%	! OE

		IC_TRANSACTION::TRANS_DATE	= OE_ORDERJOUR::ORDDATE

		OE_REGLINE::SHIPNO	= "00"
		OE_REGLINE::TRANTYPE	= "01"
		OE_REGLINE::QTY		= -TRANQTY(1%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF OE_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			OE_REGHEADER, OE_REGLINE) <> CMC$_NORMAL

		IC_TRANSACTION::TYPE_A		= "SO"
		IC_TRANSACTION::QUANTITY_A	= TRANQTY(1%)
		IC_TRANSACTION::TYPE_B		= ""
		IC_TRANSACTION::QUANTITY_B	= 0.0

		IC_TRANSACTION::COST = &
			ABS(FUNC_ROUND(OE_ORDERLINE::COST * &
			TRANQTY(1%), 2%))

		IC_TRANSACTION::PRICE = &
			ABS(FUNC_ROUND(OE_ORDERLINE::PRICE * &
			TRANQTY(1%), 2%))

		!
		! Post shipping to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

		GOTO GoToNext

	CASE 260%	! SA, OE

		IF OE_ORDERLINE::LIN = "NEWL"
		THEN
			OE_REGLINE::SHIPNO	= "00"
			OE_REGLINE::TRANTYPE	= "01"
			OE_REGLINE::QTY		= -TRANQTY(1%) - TRANQTY(2%)

			!
			! Call the post function for order
			!
			GOTO Aborted &
				IF OE_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
				BATCH_NUMBER, TITLE(), UTL_REPORTX, &
				OE_REGHEADER, OE_REGLINE) <> CMC$_NORMAL

			IC_TRANSACTION::TYPE_B		= "SO"
			IC_TRANSACTION::QUANTITY_B	= TRANQTY(2%)
		ELSE
			IC_TRANSACTION::TYPE_B		= "SO"
			IC_TRANSACTION::QUANTITY_B	= -TRANQTY(1%)
		END IF

		OE_REGLINE::SHIPNO	= "01"
		OE_REGLINE::TRANTYPE	= "02"
		OE_REGLINE::QTY		= -TRANQTY(1%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF OE_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			OE_REGHEADER, OE_REGLINE) <> CMC$_NORMAL

		IC_TRANSACTION::TYPE_A		= "SA"
		IC_TRANSACTION::QUANTITY_A	= TRANQTY(1%)
		SHIPQTY = TRANQTY(1%)

		IC_TRANSACTION::COST = &
			ABS(FUNC_ROUND(-OE_ORDERLINE::COST * TRANQTY(1%), 2%))

		IC_TRANSACTION::PRICE = &
			ABS(FUNC_ROUND(-OE_ORDERLINE::PRICE * TRANQTY(1%), 2%))

		!
		! Post shipping to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

	CASE 264% ! TR

		OE_REGLINE::SHIPNO	= "00"
		OE_REGLINE::TRANTYPE	= "01"
		OE_REGLINE::QTY		= -TRANQTY(2%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF OE_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			OE_REGHEADER, OE_REGLINE) <> CMC$_NORMAL

		IC_TRANSACTION::TYPE_A		= "TR"
		IC_TRANSACTION::QUANTITY_A	= TRANQTY(1%)
		IC_TRANSACTION::TYPE_B		= ""
		IC_TRANSACTION::QUANTITY_B	= 0.0

		IC_TRANSACTION::COST = &
			ABS(FUNC_ROUND(-OE_ORDERLINE::COST * TRANQTY(1%), 2%))

		IC_TRANSACTION::PRICE = &
			ABS(FUNC_ROUND(-OE_ORDERLINE::PRICE * TRANQTY(1%), 2%))

		!
		! Post shipping to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

		IC_TRANSACTION::LOCATION	= OE_ORDERJOUR::SHIPLIN
		IC_TRANSACTION::TYPE_A		= "TR"
		IC_TRANSACTION::QUANTITY_A	= -TRANQTY(1%)
		IC_TRANSACTION::TYPE_B		= ""
		IC_TRANSACTION::QUANTITY_B	= 0.0

		!
		! Post shipping to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

		!
		! On Order
		!
		IC_TRANSACTION::LOCATION	= OE_ORDERJOUR::LOCATION
		IC_TRANSACTION::TRANS_DATE	= OE_ORDERJOUR::ORDDATE

		IC_TRANSACTION::TYPE_A		= "SO"
		IC_TRANSACTION::QUANTITY_A	= TRANQTY(2%)
		IC_TRANSACTION::TYPE_B		= ""
		IC_TRANSACTION::QUANTITY_B	= 0.0

		IC_TRANSACTION::COST = &
			ABS(FUNC_ROUND(OE_ORDERLINE::COST * &
			TRANQTY(2%), 2%))

		IC_TRANSACTION::PRICE = &
			ABS(FUNC_ROUND(OE_ORDERLINE::PRICE * &
			TRANQTY(2%), 2%))

		!
		! Post shipping to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

	CASE 320%	! OE, LS

		IC_TRANSACTION::TRANS_DATE	= OE_ORDERJOUR::ORDDATE

		OE_REGLINE::SHIPNO	= "00"
		OE_REGLINE::TRANTYPE	= "01"
		OE_REGLINE::QTY		= -TRANQTY(2%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF OE_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			OE_REGHEADER, OE_REGLINE) <> CMC$_NORMAL

		IC_TRANSACTION::TYPE_A		= "SO"
		IC_TRANSACTION::QUANTITY_A	= TRANQTY(2%) - TRANQTY(1%)
		IC_TRANSACTION::TYPE_B		= "LS"
		IC_TRANSACTION::QUANTITY_B	= TRANQTY(1%)

		IC_TRANSACTION::COST = &
			ABS(FUNC_ROUND(OE_ORDERLINE::COST * &
			TRANQTY(2%), 2%))

		IC_TRANSACTION::PRICE = &
			ABS(FUNC_ROUND(OE_ORDERLINE::PRICE * &
			TRANQTY(2%), 2%))

		!
		! Post shipping to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

		GOTO GoToNext

	CASE 324%	! SA, LS, OE

		OE_REGLINE::SHIPNO	= "00"
		OE_REGLINE::TRANTYPE	= "01"
		OE_REGLINE::QTY		= -TRANQTY(1%) - TRANQTY(2%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF OE_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			OE_REGHEADER, OE_REGLINE) <> CMC$_NORMAL

		OE_REGLINE::SHIPNO	= "00"
		OE_REGLINE::TRANTYPE	= "02"
		OE_REGLINE::QTY		= -TRANQTY(1%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF OE_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			OE_REGHEADER, OE_REGLINE) <> CMC$_NORMAL

		IC_TRANSACTION::TYPE_A		= "SA"
		IC_TRANSACTION::QUANTITY_A	= TRANQTY(1%)
		IC_TRANSACTION::TYPE_B		= ""
		IC_TRANSACTION::QUANTITY_B	= 0.0
		SHIPQTY = TRANQTY(1%)

		IC_TRANSACTION::COST = &
			ABS(FUNC_ROUND(OE_ORDERLINE::COST * &
			TRANQTY(1%), 2%))

		IC_TRANSACTION::PRICE = &
			ABS(FUNC_ROUND(OE_ORDERLINE::PRICE * &
			TRANQTY(1%), 2%))

		!
		! Post shipping to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

		IC_TRANSACTION::TYPE_B		= "SO"
		IC_TRANSACTION::QUANTITY_B	= TRANQTY(3%)
		IC_TRANSACTION::TYPE_A		= "LS"
		IC_TRANSACTION::QUANTITY_A	= TRANQTY(2%)

		IC_TRANSACTION::COST = &
			ABS(FUNC_ROUND(OE_ORDERLINE::COST * &
			TRANQTY(3%), 2%))

		IC_TRANSACTION::PRICE = &
			ABS(FUNC_ROUND(OE_ORDERLINE::PRICE * &
			TRANQTY(3%), 2%))

		!
		! Post shipping to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

	CASE 516% ! SA,SO

		IC_TRANSACTION::TYPE_A		= "SA"
		IC_TRANSACTION::QUANTITY_A	= TRANQTY(1%)
		IC_TRANSACTION::TYPE_B		= "SO"
		IC_TRANSACTION::QUANTITY_B	= TRANQTY(2%)
		SHIPQTY = TRANQTY(1%)

		OE_REGLINE::SHIPNO	= "01"
		OE_REGLINE::TRANTYPE	= "02"
		OE_REGLINE::QTY		= -TRANQTY(1%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF OE_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			OE_REGHEADER, OE_REGLINE) <> CMC$_NORMAL

		IC_TRANSACTION::COST = &
			ABS(FUNC_ROUND(OE_ORDERLINE::COST * &
			TRANQTY(1%), 2%))

		IC_TRANSACTION::PRICE = &
			ABS(FUNC_ROUND(OE_ORDERLINE::PRICE * &
			TRANQTY(1%), 2%))

		!
		! Post shipping to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

	CASE 520% ! TR, SO

		OE_REGLINE::SHIPNO	= "01"
		OE_REGLINE::TRANTYPE	= "02"
		OE_REGLINE::QTY		= -TRANQTY(1%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF OE_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			OE_REGHEADER, OE_REGLINE) <> CMC$_NORMAL

		IC_TRANSACTION::TYPE_A		= "TR"
		IC_TRANSACTION::QUANTITY_A	= TRANQTY(1%)
		IC_TRANSACTION::TYPE_B		= "SO"
		IC_TRANSACTION::QUANTITY_B	= TRANQTY(2%)

		IC_TRANSACTION::COST = &
			ABS(FUNC_ROUND(-OE_ORDERLINE::COST * TRANQTY(1%), 2%))

		IC_TRANSACTION::PRICE = &
			ABS(FUNC_ROUND(-OE_ORDERLINE::PRICE * TRANQTY(1%), 2%))

		!
		! Post shipping to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

		IC_TRANSACTION::LOCATION	= OE_ORDERJOUR::SHIPLIN
		IC_TRANSACTION::TYPE_A		= "TR"
		IC_TRANSACTION::QUANTITY_A	= -TRANQTY(1%)
		IC_TRANSACTION::TYPE_B		= ""
		IC_TRANSACTION::QUANTITY_B	= 0.0

		!
		! Post shipping to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)


	CASE 580% ! SA,SO,LS

		OE_REGLINE::SHIPNO	= "01"
		OE_REGLINE::TRANTYPE	= "03"
		OE_REGLINE::QTY		= TRANQTY(2%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF OE_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			OE_REGHEADER, OE_REGLINE) <> CMC$_NORMAL

		OE_REGLINE::SHIPNO	= "01"
		OE_REGLINE::TRANTYPE	= "02"
		OE_REGLINE::QTY		= -TRANQTY(1%)

		!
		! Call the post function for order
		!
		GOTO Aborted &
			IF OE_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			OE_REGHEADER, OE_REGLINE) <> CMC$_NORMAL


		IC_TRANSACTION::TYPE_A		= "SA"
		IC_TRANSACTION::QUANTITY_A	= TRANQTY(1%)
		IC_TRANSACTION::TYPE_B		= "SO"
		IC_TRANSACTION::QUANTITY_B	= TRANQTY(3%)
		SHIPQTY = TRANQTY(1%)

		IC_TRANSACTION::COST = &
			ABS(FUNC_ROUND(OE_ORDERLINE::COST * &
			TRANQTY(1%), 2%))

		IC_TRANSACTION::PRICE = &
			ABS(FUNC_ROUND(OE_ORDERLINE::PRICE * &
			TRANQTY(1%), 2%))

		!
		! Post shipping to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

		IC_TRANSACTION::TYPE_A		= "LS"
		IC_TRANSACTION::QUANTITY_A	= TRANQTY(2%)
		IC_TRANSACTION::TYPE_B		= ""
		IC_TRANSACTION::QUANTITY_B	= 0.0

		!
		! Post shipping to inventory transaction file
		!
		EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

	CASE ELSE
		TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT) + " " + &
			FORMAT$(LIN%, "###") + " " + &
			OE_ORDERLINE::PRODUCT + " " + &
			"Undefined OEOrdl transaction.." + &
			NUM1$(TRANQTY(0%))

		!
		! Keep undefined codes
		!
		GOTO Aborted &
			IF OUTP_UNDEFCODES(OPT_ADDREC, &
			TITLE(), UTL_REPORTX, TEXT$) <> CMC$_NORMAL

		GOTO GoToNext

	END SELECT

	!
	! Generate a AR_OPEN_DIST record to pass through to the
	! post function
	!
	SLINE% = SLINE% + 1%
	AR_OPEN_DIST::INVNUM	= OE_ORDERJOUR::INVNUM
	AR_OPEN_DIST::CUSNUM	= OE_ORDERJOUR::CUSNUM
	AR_OPEN_DIST::SLINE	= FORMAT$(SLINE%, "<0>##")
	AR_OPEN_DIST::ACCT	= PD_ACCOUNT_READ::INVACCT
	AR_OPEN_DIST::SUBACCT	= OE_ORDERLINE::SUBACCT
	AR_OPEN_DIST::AMOUNT	= FUNC_ROUND(-TRANQTY(1%) * OE_ORDERLINE::PRICE, 2%)
	AR_OPEN_DIST::QTY	= -TRANQTY(1%)
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
	GOTO Aborted &
		IF AR_TRAN_POSTAR(OPT_ADDREC, SUBOPT_LINEITEM, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		AR_OPEN, AR_OPEN_DIST, AR_SALTAXLED, ICPERIOD) <> CMC$_NORMAL

	GOTO GotoNext IF POSTGL$ = "N" OR POSTGL$ = "M"

	GL_YYYY_PP::SOURCE	= "OIJ"
	GL_YYYY_PP::REFNO	= OE_ORDERJOUR::INVNUM
	GL_YYYY_PP::TRANDAT	= OE_ORDERJOUR::ORDDATE
	GL_YYYY_PP::DESCR	= OE_ORDERLINE::PRODUCT + &
		OE_ORDERJOUR::LOCATION
	GL_YYYY_PP::XREFNO	= OE_ORDERJOUR::CUSNUM
	GL_YYYY_PP::CKNO	= ""
	GL_YYYY_PP::TRANKEY	= ""
	GL_YYYY_PP::SUBACC	= OE_ORDERLINE::SUBACCT
	GL_YYYY_PP::OPERATION	= ""
	GL_YYYY_PP::HOURS	= 0.0
	GL_YYYY_PP::UPDSTA	= ""
	GL_YYYY_PP::POSDAT	= POSTDATE
	GL_YYYY_PP::POSTIM	= POSTTIME
	GL_YYYY_PP::BTHNUM	= BATCH_NUMBER

	!
	! post inventory and COS to GL
	!
	SELECT INTEGER(TRANQTY(0%) + .01)

	CASE 2%, 4%, 8%, 18%, 32%, 34%, 36%, 68%, 136%, 260%, 264%, &
		320%, 324%, 516%, 520%, 580%

		GL_HEADER% = -1%

		EXIT_STATUS = PD_READ_ACCOUNT(OE_ORDERJOUR::LOCATION, &
			PD_PRODUCT_EXAM::PROD_TYPE, PD_ACCOUNT_READ)

		!
		! Is inventory account number defined?
		!
		EXIT_STATUS = GL_EXAM_CHART(PD_ACCOUNT_READ::INVACCT, &
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
			TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, &
				CMC$_LEFT) + " " + &
				FORMAT$(LIN%, "###") + " " + &
				" (OEOrdl Inv Acct)* " + &
				PD_ACCOUNT_READ::INVACCT

			!
			! Keep undefined codes
			!
			GOTO Aborted &
				IF OUTP_UNDEFCODES(OPT_ADDREC, &
				TITLE(), UTL_REPORTX, TEXT$) <> &
				CMC$_NORMAL

		!
		! Weird happenin's
		!
		CASE ELSE
			GOTO Aborted

		END SELECT

		COST = FUNC_ROUND(-OE_ORDERLINE::COST * TRANQTY(1%), 2%)

		!
		! inventory
		!
		GL_YYYY_PP::ACCT	= PD_ACCOUNT_READ::INVACCT
		GL_YYYY_PP::AMOUNT	= -COST
		GL_YYYY_PP::UNITS	= -TRANQTY(1%)

		EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, GL_YYYY_PP, &
			GL_CHART_EXAM, ICPERIOD)

		IF READ_ACCOUNT% = CMC$_NORMAL AND &
			OE_ACCOUNT_READ::COSACCT <> "Order COS Acct"
		THEN
			PD_ACCOUNT_READ::COSACCT = OE_ACCOUNT_READ::COSACCT
		ELSE
			EXIT_STATUS = PD_READ_ACCOUNT(OE_ORDERJOUR::SHIPLIN, &
				PD_PRODUCT_EXAM::PROD_TYPE, PD_ACCOUNT_TRAN)

			PD_ACCOUNT_READ::COSACCT = PD_ACCOUNT_TRAN::INVACCT &
				IF OE_ORDERJOUR::CUSNUM = ""
		END IF

		!
		! Is COS account number defined?
		!
		EXIT_STATUS = GL_EXAM_CHART(PD_ACCOUNT_READ::COSACCT, &
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
			TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, &
				CMC$_LEFT) + " " + &
				FORMAT$(LIN%, "###") + " " + &
				" (OEOrdl COS Acct)* " + &
				PD_ACCOUNT_READ::COSACCT

			!
			! Keep undefined codes
			!
			GOTO Aborted &
				IF OUTP_UNDEFCODES(OPT_ADDREC, &
				TITLE(), UTL_REPORTX, TEXT$) <> CMC$_NORMAL

		!
		! Weird happenin's
		!
		CASE ELSE
			GOTO Aborted

		END SELECT

		GL_YYYY_PP::ACCT	= PD_ACCOUNT_READ::COSACCT
		GL_YYYY_PP::AMOUNT	= COST
		GL_YYYY_PP::UNITS	= TRANQTY(1%)

		EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, GL_YYYY_PP, &
			GL_CHART_EXAM, ICPERIOD)

	CASE ELSE
		!
		! Too much below is depending on the accounts having
		! been read to assume that they have.
		!
		EXIT_STATUS = PD_READ_ACCOUNT(OE_ORDERJOUR::LOCATION, &
			PD_PRODUCT_EXAM::PROD_TYPE, PD_ACCOUNT_READ)

	END SELECT

	!
	! post revenue to GL
	!
	SELECT INTEGER(TRANQTY(0%) + .01)

	CASE 4%, 8%, 16%, 18%, 36%, 68%, 136%, 260%, 264%, 320%, &
		324%, 516%, 520%, 580%

		GL_HEADER% = -1%

		IF READ_ACCOUNT% <> CMC$_NORMAL OR &
			OE_ACCOUNT_READ::SALES = "Order Sales Acct"
		THEN
			V% = UTL_READ_TRANSACCT(OE_ORDERJOUR::LOCATION, &
				LEFT(TRANTYPE$(1%), 2%), &
				PD_PRODUCT_EXAM::PROD_TYPE, &
				UTL_TRANSACCT_READ)
		ELSE
			UTL_TRANSACCT_READ::ACCOUNT = OE_ACCOUNT_READ::SALES
		END IF

		EXIT_STATUS = GL_EXAM_CHART(UTL_TRANSACCT_READ::ACCOUNT, &
			GL_CHART_EXAM)

		EXTPRICE = FUNC_ROUND(-TRANQTY(1%) * OE_ORDERLINE::PRICE, 2%)
		EXTCOST = FUNC_ROUND(-TRANQTY(1%) * OE_ORDERLINE::COST, 2%)

		IF EXTPRICE <> 0.0
		THEN
			SELECT EXIT_STATUS

			!
			! Account number defined
			!
			CASE CMC$_NORMAL

			!
			! Undefined Account Number
			!
			CASE CMC$_UNDEFINED

				TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, &
					CMC$_LEFT) + " " + &
					FORMAT$(LIN%, "###") + &
					"  (OEOrdl Trans Acct)* " + &
					UTL_TRANSACCT_READ::ACCOUNT

				!
				! Keep undefined codes
				!
				GOTO Aborted &
					IF OUTP_UNDEFCODES(OPT_ADDREC, &
					TITLE(), UTL_REPORTX, TEXT$) <> &
					CMC$_NORMAL

			!
			! Weird happenin's
			!
			CASE ELSE
				GOTO Aborted

			END SELECT

			EXTACCOUNT$ = UTL_TRANSACCT_READ::ACCOUNT

			GL_YYYY_PP::ACCT	= UTL_TRANSACCT_READ::ACCOUNT
			GL_YYYY_PP::AMOUNT	= -EXTPRICE
			GL_YYYY_PP::UNITS	= -TRANQTY(1%)

			EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, &
				CHECK_PERIOD, BATCH_NUMBER, &
				TITLE(), UTL_REPORTX, GL_YYYY_PP, &
				GL_CHART_EXAM, ICPERIOD)
		END IF

		PROMO = FUNC_ROUND(-TRANQTY(1%) * OE_ORDERLINE::PROMO, 2%)

		DISCOUNT = -FUNC_ROUNDLOW(TRANQTY(1%) * &
			(OE_ORDERLINE::PRICE - OE_ORDERLINE::PROMO) * &
			OE_ORDERLINE::DISCOUNT / 100.0, 2%)

		MISCH = FUNC_ROUND(-TRANQTY(1%) * OE_ORDERLINE::MISCH, 2%)
		MISCH2 = FUNC_ROUND(-TRANQTY(1%) * OE_ORDERLINE::MISCH2, 2%)

	END SELECT

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

			TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, &
				CMC$_LEFT) + " " + &
				FORMAT$(LIN%, "###") + " " + &
				" (OEOrdl Disc Acct)* " + &
				PD_ACCOUNT_READ::DISCACCT

			!
			! Keep undefined codes
			!
			GOTO Aborted &
				IF OUTP_UNDEFCODES(OPT_ADDREC, &
				TITLE(), UTL_REPORTX, TEXT$) <> CMC$_NORMAL

		!
		! Weird happenin's
		!
		CASE ELSE
			GOTO Aborted

		END SELECT

		GL_YYYY_PP::ACCT	= PD_ACCOUNT_READ::DISCACCT
		GL_YYYY_PP::AMOUNT	= DISCOUNT
		GL_YYYY_PP::UNITS	= TRANQTY(1%)

		EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, GL_YYYY_PP, &
			GL_CHART_EXAM, ICPERIOD)

	END IF

	!
	! Check misc charges
	!
	IF MISCH <> 0.0
	THEN
		!
		! Is discount account number defined?
		!
		EXIT_STATUS = GL_EXAM_CHART(PD_ACCOUNT_READ::MISCHACCT, &
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
			TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, &
				CMC$_LEFT) + " " + &
				FORMAT$(LIN%, "###") + " " + &
				" (OEOrdl Misc Acct)* " + &
				PD_ACCOUNT_READ::MISCHACCT

			!
			! Keep undefined codes
			!
			GOTO Aborted &
				IF OUTP_UNDEFCODES(OPT_ADDREC, &
				TITLE(), UTL_REPORTX, TEXT$) <> CMC$_NORMAL

		!
		! Weird happenin's
		!
		CASE ELSE
			GOTO Aborted

		END SELECT

		GL_YYYY_PP::ACCT	= PD_ACCOUNT_READ::MISCHACCT
		GL_YYYY_PP::AMOUNT	= -MISCH
		GL_YYYY_PP::UNITS	= -TRANQTY(1%)

		EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, GL_YYYY_PP, &
			GL_CHART_EXAM, ICPERIOD)

	END IF

	!
	! Check misc charges
	!
	IF MISCH2 <> 0.0
	THEN
		!
		! Is discount account number defined?
		!
		EXIT_STATUS = GL_EXAM_CHART(PD_ACCOUNT_READ::MISCH2ACCT, &
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
			TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, &
				CMC$_LEFT) + " " + &
				FORMAT$(LIN%, "###") + " " + &
				" (OEOrdl Misc2 Acct)* " + &
				PD_ACCOUNT_READ::MISCH2ACCT

			!
			! Keep undefined codes
			!
			GOTO Aborted &
				IF OUTP_UNDEFCODES(OPT_ADDREC, &
				TITLE(), UTL_REPORTX, TEXT$) <> CMC$_NORMAL

		!
		! Weird happenin's
		!
		CASE ELSE
			GOTO Aborted

		END SELECT

		GL_YYYY_PP::ACCT	= PD_ACCOUNT_READ::MISCH2ACCT
		GL_YYYY_PP::AMOUNT	= -MISCH2
		GL_YYYY_PP::UNITS	= -TRANQTY(1%)

		EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, GL_YYYY_PP, &
			GL_CHART_EXAM, ICPERIOD)

	END IF

	!
	! Check Product Promo
	!
	DEAL_PRICE = 0.0
	IF PROMO <> 0.0
	THEN
		V% = OE_READ_PROMO(OE_ORDERLINE::PRODUCT, &
			OE_ORDERJOUR::ORDDATE, &
			OE_ORDERJOUR::CUSNUM, OE_PROMO_READ, 0.0, 0.0)

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
		! Undefined Account Number
		!
		CASE CMC$_UNDEFINED
			TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, &
				CMC$_LEFT) + " " + &
				FORMAT$(LIN%, "###") + " " + &
				" (OEOrdl Promo Acct)* " + &
				OE_PROMO_READ::ACCOUNT

			!
			! Keep undefined codes
			!
			GOTO Aborted &
				IF OUTP_UNDEFCODES(OPT_ADDREC, &
				TITLE(), UTL_REPORTX, TEXT$) <> CMC$_NORMAL

		!
		! Weird happenin's
		!
		CASE ELSE
			GOTO Aborted

		END SELECT

		GL_YYYY_PP::ACCT	= OE_PROMO_READ::ACCOUNT
		GL_YYYY_PP::AMOUNT	= PROMO
		GL_YYYY_PP::UNITS	= TRANQTY(1%)

		EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, GL_YYYY_PP, &
			GL_CHART_EXAM, ICPERIOD)

	ELSE
		IF SHIPQTY <> 0.0 AND PC_READ_DEAL(OE_ORDERLINE::PRODUCT, &
			OE_ORDERJOUR::ORDDATE, &
			OE_ORDERJOUR::CUSNUM, &
			OE_ORDERLINE::PRICE, DEAL_FINAL, &
			OUT_ACCOUNT$, OUT_COMMENT$, DEAL_PRICE) = CMC$_NORMAL
		THEN
			!
			! Is promo account number defined?
			!
			EXIT_STATUS = GL_EXAM_CHART(OUT_ACCOUNT$, &
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
				TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, &
					CMC$_LEFT) + " " + &
					FORMAT$(LIN%, "###") + " " + &
					" (OEOrdl Deal Acct)* " + &
					OUT_ACCOUNT$

			!
			! Keep undefined codes
			!
				GOTO Aborted &
					IF OUTP_UNDEFCODES(OPT_ADDREC, &
					TITLE(), UTL_REPORTX, TEXT$) <> &
					CMC$_NORMAL

			!
			! Weird happenin's
			!
			CASE ELSE
				GOTO Aborted

			END SELECT

			GL_YYYY_PP::ACCT	= OUT_ACCOUNT$
			GL_YYYY_PP::AMOUNT	= &
				FUNC_ROUND(DEAL_PRICE * -SHIPQTY, 2%)
			GL_YYYY_PP::UNITS	= SHIPQTY

			EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, &
				CHECK_PERIOD, BATCH_NUMBER, &
				TITLE(), UTL_REPORTX, GL_YYYY_PP, &
				GL_CHART_EXAM, ICPERIOD)

			GL_YYYY_PP::ACCT	= EXTACCOUNT$
			GL_YYYY_PP::AMOUNT	= &
				-FUNC_ROUND(DEAL_PRICE * -SHIPQTY, 2%)
			GL_YYYY_PP::UNITS	= -SHIPQTY

			EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, &
				CHECK_PERIOD, BATCH_NUMBER, &
				TITLE(), UTL_REPORTX, GL_YYYY_PP, &
				GL_CHART_EXAM, ICPERIOD)

			EXTPRICE = EXTPRICE + &
				FUNC_ROUND(DEAL_PRICE * -SHIPQTY, 2%)

		END IF

	END IF

 GoToNext:

	FEXTPRICE = (EXTPRICE - PROMO - DISCOUNT - &
		FUNC_ROUND(DEAL_PRICE * -SHIPQTY, 2%)) + MISCH + MISCH2
	LTOTAL = LTOTAL + &
		(EXTPRICE - PROMO - DISCOUNT - &
		FUNC_ROUND(DEAL_PRICE * -SHIPQTY, 2%)) + MISCH + MISCH2
	LCOST = LCOST + EXTCOST

	IF TRM$(PS_CONTROL::CUSBAL) <> ""
	THEN
		IF COMP_STRING(TRM$(PD_PRODUCT_EXAM::CATEGORY), &
			TRM$(PS_CONTROL::CUSBAL))
		THEN
			LNOTAX = LNOTAX + &
				(EXTPRICE - PROMO - DISCOUNT - &
				FUNC_ROUND(DEAL_PRICE * -SHIPQTY, 2%))
		END IF
	END IF

	IF PS_CONTROL::MISCTAXABLE = "N"
	THEN
		LNOTAX = LNOTAX + MISCH
	END IF

	IF PS_CONTROL::MISC2TAXABLE = "N"
	THEN
		LNOTAX = LNOTAX + MISCH2
	END IF

	IF SORTBY$ <> ""
	THEN
		!
		! Print one report line
		!
		TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT) + &
			"    " + &
			FORMAT$(LIN%, "####") + "  " + &
			OE_ORDERLINE::PRODUCT + "  " + &
			LEFT$(PD_PRODUCT_EXAM::DESCRIPTION,  15%) + "  " + &
			FORMAT$(OE_ORDERLINE::ORDQTY, "#,###,###.##") + "  " + &
			FORMAT$(OE_ORDERLINE::SHPQTY, "#,###,###.##") + "  " + &
			FORMAT$(OE_ORDERLINE::PRICE + DEAL_PRICE, "#,###,###.##") + "  " + &
			FORMAT$(OE_ORDERLINE::PROMO + DEAL_PRICE, "#,###,###.##") + &
			"     " + &
			FORMAT$(OE_ORDERLINE::DISCOUNT, "##.###") + "  " + &
			FORMAT$(FEXTPRICE, "#,###,###.##")

		CTR% = -1%

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT)  + " "

		IF (OE_ORDERLINE::SUBACCT = "")
		THEN
			TEXT$ = TEXT$ + SPACE$(30%) + ".......... "
		ELSE
			TEXT$ = TEXT$ + "  SN#" + &
				OE_ORDERLINE::SUBACCT + &
				SPACE$(15%) + ".......... "
		END IF

		TEXT$ = TEXT$ + RIGHT(TRANTYPE$(I%), 3%) &
			FOR I% = 1% TO VAL%(TRANTYPE$(0%))

		IF OE_ORDERLINE::PRICE < OE_ORDERLINE::COST
		THEN
			TEXT$ = TEXT$ + " Cost is higher than price!!!"
		END IF

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
	END IF

	GOTO LineItem

 ProcessHeader:
	IF OE_ORDERJOUR::REG_FLAG <> "Y"
	THEN
		!
		! Generate a OE_REGHEADER record to pass through to the
		! post function
		!
		OE_REGHEADER::ORDNUM	= OE_ORDERJOUR::ORDNUM
		OE_REGHEADER::ORDTYPE	= OE_ORDERJOUR::ORDTYPE
		OE_REGHEADER::ORDCAT	= OE_ORDERJOUR::ORDCAT
		OE_REGHEADER::ORDDATE	= OE_ORDERJOUR::ORDDATE
		OE_REGHEADER::ASTATUS	= "O"
		OE_REGHEADER::SDATE	= OE_ORDERJOUR::SHIPDATE
		OE_REGHEADER::CUSNUM	= OE_ORDERJOUR::CUSNUM
		OE_REGHEADER::SHIPNAM	= OE_ORDERJOUR::SHIPNAM
		OE_REGHEADER::SHIPLIN	= OE_ORDERJOUR::SHIPLIN
		OE_REGHEADER::ADD1	= OE_ORDERJOUR::ADD1
		OE_REGHEADER::ADD2	= OE_ORDERJOUR::ADD2
		OE_REGHEADER::ADD3	= OE_ORDERJOUR::ADD3
		OE_REGHEADER::CITY	= OE_ORDERJOUR::CITY
		OE_REGHEADER::STATE	= OE_ORDERJOUR::STATE
		OE_REGHEADER::ZIP	= OE_ORDERJOUR::ZIP
		OE_REGHEADER::COUNTRY	= OE_ORDERJOUR::COUNTRY
		OE_REGHEADER::CUSTPO	= OE_ORDERJOUR::CUSTPO
		OE_REGHEADER::SHIPVIA	= OE_ORDERJOUR::SHIPVIA
		OE_REGHEADER::TERMS	= OE_ORDERJOUR::TERMS
		OE_REGHEADER::DISC	= OE_ORDERJOUR::DISC
		OE_REGHEADER::TAXCODE	= OE_ORDERJOUR::TAXCODE
		OE_REGHEADER::TAXFLAG	= OE_ORDERJOUR::TAXFLAG
		OE_REGHEADER::LOCATION	= OE_ORDERJOUR::LOCATION
		OE_REGHEADER::COMMAMT	= OE_ORDERJOUR::COMMAMT
		OE_REGHEADER::SHIPNO	= "00"
		OE_REGHEADER::BATCH	= BATCH_NUMBER
		OE_REGHEADER::SALESMAN	= OE_ORDERJOUR::SALESMAN
		OE_REGHEADER::SALCOMM	= OE_ORDERJOUR::SALCOMM
		OE_REGHEADER::OPERATOR	= OE_ORDERJOUR::OPERATOR
		OE_REGHEADER::NOTES(0%)	= OE_ORDERJOUR::NOTES(0%)
		OE_REGHEADER::NOTES(1%)	= OE_ORDERJOUR::NOTES(1%)
		OE_REGHEADER::NOTES(2%)	= OE_ORDERJOUR::NOTES(2%)

		!
		! Call the post function
		!
		GOTO Aborted &
			IF OE_TRAN_POST(OPT_ADDREC, SUBOPT_REGISTER, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			OE_REGHEADER, OE_REGLINE) <> CMC$_NORMAL

	END IF

	ORDER_DISC = FUNC_ROUND(LTOTAL * OE_ORDERJOUR::DISC / 100.0, 2%)

	IF OE_ORDERJOUR::SALESTAX <> 0.0
	THEN
		ORDER_TAX = FUNC_ROUND((LTOTAL - LNOTAX - ORDER_DISC) * &
			OE_ORDERJOUR::SALESTAX / 100.0, 2%)
	ELSE
		ORDER_TAX = 0.0
		!
		! Make invoice hold together in sales tax ledger
		!
		LNOTAX = 0.0
	END IF

	ORDER_TOT = OE_ORDERJOUR::FREIGHT + &
		OE_ORDERJOUR::HANDLING + &
		OE_ORDERJOUR::MISC + &
		ORDER_TAX + LTOTAL - ORDER_DISC

	GOTO GoToNextHeader IF GL_HEADER% = 0% AND &
		OE_ORDERJOUR::INVNUM = "" OR &
		POSTGL$ = "N" OR POSTGL$ = "M"

	!
	! Begin GL post function for Header information
	! Generate a GL record to pass through to the post function
	!
	GL_YYYY_PP::SOURCE	= "OIJ"
	GL_YYYY_PP::REFNO	= OE_ORDERJOUR::INVNUM
	GL_YYYY_PP::TRANDAT	= OE_ORDERJOUR::ORDDATE
	GL_YYYY_PP::DESCR	= AR_35CUSTOM_EXAM::CUSNAM
	GL_YYYY_PP::XREFNO	= OE_ORDERJOUR::CUSNUM
	GL_YYYY_PP::CKNO	= ""
	GL_YYYY_PP::TRANKEY	= ""
	GL_YYYY_PP::SUBACC	= OE_ORDERJOUR::SALESMAN
	GL_YYYY_PP::OPERATION	= ""
	GL_YYYY_PP::HOURS	= 0.0
	GL_YYYY_PP::UPDSTA	= ""
	GL_YYYY_PP::POSDAT	= POSTDATE
	GL_YYYY_PP::POSTIM	= POSTTIME
	GL_YYYY_PP::BTHNUM	= BATCH_NUMBER

	!
	! Check Invoice Miscellaneous Charges
	!
	GOTO Freight IF OE_ORDERJOUR::MISC = 0.0

	!
	! Is miscellaneous account number defined?
	!
	EXIT_STATUS = GL_EXAM_CHART(OE_ORDERJOUR::MISCACCT, GL_CHART_EXAM)

	SELECT EXIT_STATUS

	!
	! Account number defined
	!
	CASE CMC$_NORMAL

	!
	! Number undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED
		TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, &
			CMC$_LEFT) + &
			" (MiscAccount)* " + &
			OE_ORDERJOUR::MISCACCT

		!
		! Keep undefined codes
		!
		GOTO Aborted &
			IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), &
			UTL_REPORTX, TEXT$) <> CMC$_NORMAL

	!
	! Weird happenin's
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	GL_YYYY_PP::ACCT	= OE_ORDERJOUR::MISCACCT
	GL_YYYY_PP::AMOUNT	= -OE_ORDERJOUR::MISC
	GL_YYYY_PP::UNITS	= 0.0
	GL_YYYY_PP::DESCR	= AR_35CUSTOM_EXAM::CUSNAM

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, ICPERIOD)

 Freight:
	!
	! Check Invoice Freight
	!
	GOTO Handling IF OE_ORDERJOUR::FREIGHT = 0.0

	!
	! Is freight account number defined?
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
		TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, &
			CMC$_LEFT)  + &
			" (Freight Acct)* " + &
			OE_ACCOUNT_READ::FRACCT

		!
		! Keep undefined codes
		!
		GOTO Aborted &
			IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), &
			UTL_REPORTX, TEXT$) <> CMC$_NORMAL

	!
	! Weird happenin's
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	GL_YYYY_PP::ACCT	= OE_ACCOUNT_READ::FRACCT
	GL_YYYY_PP::AMOUNT	= -OE_ORDERJOUR::FREIGHT
	GL_YYYY_PP::UNITS	= 0.0
	GL_YYYY_PP::DESCR	= AR_35CUSTOM_EXAM::CUSNAM

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, ICPERIOD)

 Handling:
	!
	! Check Invoice Handline
	!
	GOTO OrderDisc IF OE_ORDERJOUR::HANDLING = 0.0

	!
	! Is handling account number defined?
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
		TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, &
			CMC$_LEFT)   + &
			" (Handling Acct)* " + &
			OE_ACCOUNT_READ::HANDLING

		!
		! Keep undefined codes
		!
		GOTO Aborted &
			IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), &
			UTL_REPORTX, TEXT$) <> CMC$_NORMAL

	!
	! Weird happenin's
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	GL_YYYY_PP::ACCT	= OE_ACCOUNT_READ::HANDLING
	GL_YYYY_PP::AMOUNT	= -OE_ORDERJOUR::HANDLING
	GL_YYYY_PP::UNITS	= 0.0
	GL_YYYY_PP::DESCR	= AR_35CUSTOM_EXAM::CUSNAM

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, ICPERIOD)

 OrderDisc:
	!
	! Check Invoice Discount
	!
	GOTO SalesTax IF ORDER_DISC = 0.0

	!
	! Is Header Discount account number defined?
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
		TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, &
			CMC$_LEFT) + &
			" (Disc Acct)* "   + &
			OE_ACCOUNT_READ::DISACCT

		!
		! Keep undefined codes
		!
		GOTO Aborted &
			IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), &
			UTL_REPORTX, TEXT$) <> CMC$_NORMAL

	!
	! Weird happenin's
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	GL_YYYY_PP::ACCT	= OE_ACCOUNT_READ::DISACCT
	GL_YYYY_PP::AMOUNT	= ORDER_DISC
	GL_YYYY_PP::UNITS	= 0.0
	GL_YYYY_PP::DESCR	= AR_35CUSTOM_EXAM::CUSNAM

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, ICPERIOD)

 SalesTax:
	!
	! Check Invoice Sales Tax
	!
	GOTO OrderTotal IF ORDER_TAX = 0.0

	V% = OE_READ_SALESTAX(OE_ORDERJOUR::TAXCODE, &
		"1", OE_SALESTAX_READ)

	TOTAL = OE_SALESTAX_READ::STATETAX + &
		OE_SALESTAX_READ::CITYTAX + &
		OE_SALESTAX_READ::COUNTYTAX

	IF TOTAL <> 0.0
	THEN
		STATETAX = FUNC_ROUND(ORDER_TAX * &
			(OE_SALESTAX_READ::STATETAX / TOTAL), 2%)

		CITYTAX = FUNC_ROUND(ORDER_TAX * &
			(OE_SALESTAX_READ::CITYTAX/TOTAL), 2%)
	ELSE
		STATETAX = 0.0
		CITYTAX  = 0.0
	END IF

	IF STATETAX <> 0.0
	THEN
		!
		! Is Line account number defined?
		!
		EXIT_STATUS = GL_EXAM_CHART(OE_SALESTAX_READ::STATEACC, &
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
			TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, &
				CMC$_LEFT) + &
				" (State Tax Acct)* " + &
				OE_SALESTAX_READ::STATEACC

			!
			! Keep undefined codes
			!
			GOTO Aborted &
				IF OUTP_UNDEFCODES(OPT_ADDREC, &
				TITLE(), UTL_REPORTX, TEXT$) <> CMC$_NORMAL

		!
		! Weird happenin's
		!
		CASE ELSE
			GOTO Aborted

		END SELECT

		GL_YYYY_PP::ACCT	= OE_SALESTAX_READ::STATEACC
		GL_YYYY_PP::AMOUNT	= -STATETAX
		GL_YYYY_PP::UNITS	= 0.0
		GL_YYYY_PP::DESCR	= AR_35CUSTOM_EXAM::CUSNAM

		EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, &
			CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, GL_YYYY_PP, &
			GL_CHART_EXAM, ICPERIOD)

	END IF

	IF CITYTAX <> 0.0
	THEN
		IF OE_SALESTAX_READ::COUNTYTAX <> 0.0
		THEN
			ACCT_AMT = -CITYTAX
		ELSE
			ACCT_AMT = -(ORDER_TAX - STATETAX)
		END IF

		!
		! Is Line account number defined?
		!
		EXIT_STATUS = GL_EXAM_CHART(OE_SALESTAX_READ::CITYACC, &
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
			TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, &
				CMC$_LEFT)   + &
				" (City Tax Acct)* " + &
				OE_SALESTAX_READ::CITYACC

			!
			! Keep undefined codes
			!
			GOTO Aborted &
				IF OUTP_UNDEFCODES(OPT_ADDREC, &
				TITLE(), UTL_REPORTX, TEXT$) <> CMC$_NORMAL

		!
		! Weird happenin's
		!
		CASE ELSE
			GOTO Aborted

		END SELECT

		GL_YYYY_PP::ACCT	= OE_SALESTAX_READ::CITYACC
		GL_YYYY_PP::AMOUNT	= ACCT_AMT
		GL_YYYY_PP::UNITS	= 0.0
		GL_YYYY_PP::DESCR	= AR_35CUSTOM_EXAM::CUSNAM

		EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, GL_YYYY_PP, &
			GL_CHART_EXAM, ICPERIOD)

	END IF

	IF OE_SALESTAX_READ::COUNTYTAX <> 0.0
	THEN
		ACCT_AMT  = -(ORDER_TAX-STATETAX-CITYTAX)

		!
		! Is Line account number defined?
		!
		EXIT_STATUS = GL_EXAM_CHART(OE_SALESTAX_READ::COUNTYACC, &
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
			TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, &
				CMC$_LEFT) + &
				" (County Tax Acct)* " + &
				OE_SALESTAX_READ::COUNTYACC

			!
			! Keep undefined codes
			!
			GOTO Aborted &
				IF OUTP_UNDEFCODES(OPT_ADDREC, &
				TITLE(), UTL_REPORTX, TEXT$) <> CMC$_NORMAL

		!
		! Weird happenin's
		!
		CASE ELSE
			GOTO Aborted

		END SELECT

		GL_YYYY_PP::ACCT	= OE_SALESTAX_READ::COUNTYACC
		GL_YYYY_PP::AMOUNT	= ACCT_AMT
		GL_YYYY_PP::UNITS	= 0.0
		GL_YYYY_PP::DESCR	= AR_35CUSTOM_EXAM::CUSNAM

		EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, GL_YYYY_PP, &
			GL_CHART_EXAM, ICPERIOD)

	END IF

 OrderTotal:

	EXIT_STATUS = GL_EXAM_CHART(OE_ACCOUNT_READ::ACCOUNT, GL_CHART_EXAM)

	SELECT EXIT_STATUS

	!
	! Account number defined
	!
	CASE CMC$_NORMAL

	!
	! Undefined Account Number
	!
	CASE CMC$_UNDEFINED
		TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, &
			CMC$_LEFT) + &
			" (Order Acct)* "  + &
			OE_ACCOUNT_READ::ACCOUNT

		!
		! Keep undefined codes
		!
		GOTO Aborted &
			IF OUTP_UNDEFCODES(OPT_ADDREC, &
			TITLE(), UTL_REPORTX, TEXT$) <> CMC$_NORMAL

	!
	! Weird happenin's
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	GL_YYYY_PP::ACCT	= OE_ACCOUNT_READ::ACCOUNT
	GL_YYYY_PP::AMOUNT	= ORDER_TOT
	GL_YYYY_PP::UNITS	= 0.0
	GL_YYYY_PP::DESCR	= GL_CHART_EXAM::DESCR

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, ICPERIOD)

	GOTO GoToNextHeader IF OE_ORDERJOUR::CUSNUM = ""

 if (0%)
 then
	!
	! Generate a AR_OPEN_DIST record to pass through to the
	! post function
	!
	AR_OPEN_DIST::INVNUM	= OE_ORDERJOUR::INVNUM
	AR_OPEN_DIST::CUSNUM	= OE_ORDERJOUR::CUSNUM
	AR_OPEN_DIST::SLINE	= ""
	AR_OPEN_DIST::ACCT	= ""
	AR_OPEN_DIST::SUBACCT	= ""
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
	GOTO Aborted &
		IF AR_TRAN_POSTAR(OPT_ADDREC, SUBOPT_LINEITEM, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		AR_OPEN, AR_OPEN_DIST, AR_SALTAXLED, ICPERIOD) <> CMC$_NORMAL
 end if

	!
	! Post to sales tax file if correct type
	!
	AR_SALTAXLED::TAXTYP	= OE_ORDERJOUR::TAXFLAG
	AR_SALTAXLED::CUSNUM	= OE_ORDERJOUR::CUSNUM
	AR_SALTAXLED::INVNUM	= OE_ORDERJOUR::INVNUM
	AR_SALTAXLED::AMOUNT	= LTOTAL - LNOTAX - ORDER_DISC
	AR_SALTAXLED::BATCH	= BATCH_NUMBER
	AR_SALTAXLED::TRADAT	= OE_ORDERJOUR::ORDDATE

	GOTO Aborted &
		IF AR_TRAN_POSTAR(OPT_ADDREC, SUBOPT_LEDGER, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, AR_OPEN, &
		AR_OPEN_DIST, AR_SALTAXLED, ICPERIOD) <> CMC$_NORMAL

	IF LNOTAX <> 0.0
	THEN
		!
		! Post to sales tax file if correct type
		!
		AR_SALTAXLED::TAXTYP	= "4"
		AR_SALTAXLED::CUSNUM	= OE_ORDERJOUR::CUSNUM
		AR_SALTAXLED::INVNUM	= OE_ORDERJOUR::INVNUM
		AR_SALTAXLED::AMOUNT	= LNOTAX
		AR_SALTAXLED::BATCH	= BATCH_NUMBER
		AR_SALTAXLED::TRADAT	= OE_ORDERJOUR::ORDDATE

		GOTO Aborted &
			IF AR_TRAN_POSTAR(OPT_ADDREC, SUBOPT_LEDGER, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, AR_OPEN, &
			AR_OPEN_DIST, AR_SALTAXLED, ICPERIOD) <> CMC$_NORMAL
	END IF

	CHARGES = OE_ORDERJOUR::FREIGHT + OE_ORDERJOUR::HANDLING + &
		OE_ORDERJOUR::MISC + ORDER_TAX

3330	!
	! Utility Terms Description File
	!
	DISCDAYS% = 30%
	DUEDAYS% = 30%
	DUEDAY$ = ""
	DISCDATE$ = ""

	WHEN ERROR IN
		GET #UTL_TERMS.CH%, KEY #0% EQ OE_ORDERJOUR::TERMS, REGARDLESS
	USE
		CONTINUE InitAR IF ERR = 9% OR ERR = 155%
		FILENAME$ = "UTL_TERMS"
		CONTINUE HelpError
	END WHEN

	IF EDIT$(UTL_TERMS::DUEDATE, -1%) <> ""
	THEN
		DUEDAY$  = UTL_TERMS::DUEDATE
		DUE_DATE$  = OE_ORDERJOUR::TRANDATE
	ELSE
		DUEDAYS%  = UTL_TERMS::DUEDAYS
	END IF

	IF EDIT$(UTL_TERMS::DISCOUNTDATE, -1%) <> ""
	THEN
		DISCDATE$ = UTL_TERMS::DISCOUNTDATE
		DISC_DATE$ = OE_ORDERJOUR::TRANDATE
	ELSE
		DISCDAYS% = UTL_TERMS::DISCOUNTDAYS
	END IF

 InitAR:
	SALAMT_SUM = 0.0
	DISAMT_SUM = 0.0
	OTHCHG_SUM = 0.0

	FOR PAY% = 1% TO OE_ORDERJOUR::PAYMNT

		IF DUEDAY$ = ""
		THEN
			DUE_DATE$ = DATE_INVDCODE(DATE_DAYCODE( &
				OE_ORDERJOUR::TRANDATE) + DUEDAYS% * PAY%)
		ELSE
			MON% = VAL%(MID(DUE_DATE$, 5%, 2%)) + 1%
			IF MON% = 13%
			THEN
				DUE_DATE$ = &
					FORMAT$(VAL%(LEFT(DUE_DATE$, 4%)) + 1%, &
					"<0>###") + "01" + DUEDAY$
			ELSE
				DUE_DATE$ = LEFT(DUE_DATE$, 4%) + &
					FORMAT$(MON%, "<0>#") + DUEDAY$
			END IF
		END IF

		IF DISCDATE$ = ""
		THEN
			DISC_DATE$ = DATE_INVDCODE(DATE_DAYCODE( &
				OE_ORDERJOUR::TRANDATE) + DISCDAYS% * PAY%)
		ELSE
			MON% = VAL%(MID(DISC_DATE$, 5%, 2%)) + 1%
			IF MON% = 13%
			THEN
				DISC_DATE$ = &
					FORMAT$(VAL%(LEFT(DISC_DATE$, 4%)) + &
					1%, &
					"<0>###") + "01" + DISCDATE$
			ELSE
				DISC_DATE$ = LEFT(DISC_DATE$, 4%) + &
					FORMAT$(MON%, "<0>#") + DISCDATE$
			END IF
		END IF

		SALAMT = FUNC_ROUND(ORDER_TOT  / OE_ORDERJOUR::PAYMNT, 2%)
		DISAMT = FUNC_ROUND(ORDER_DISC / OE_ORDERJOUR::PAYMNT, 2%)
		OTHCHG = FUNC_ROUND(CHARGES / OE_ORDERJOUR::PAYMNT, 2%)

		IF PAY% = OE_ORDERJOUR::PAYMNT
		THEN
			SALAMT = ORDER_TOT  - SALAMT_SUM
			DISAMT = ORDER_DISC - DISAMT_SUM
			OTHCHG = CHARGES - OTHCHG_SUM
		ELSE
			SALAMT_SUM = SALAMT_SUM + SALAMT
			DISAMT_SUM = DISAMT_SUM + DISAMT
			OTHCHG_SUM = OTHCHG_SUM + OTHCHG
		END IF

		!
		! Generate a AR record to pass through to the post function
		!
		AR_OPEN::CUSNUM		= OE_ORDERJOUR::CUSNUM
		AR_OPEN::INVNUM		= OE_ORDERJOUR::INVNUM
		AR_OPEN::TRATYP		= "01"
		AR_OPEN::TRADAT		= OE_ORDERJOUR::SHIPDATE
		AR_OPEN::SALAMT		= SALAMT
		AR_OPEN::DISAMT		= -DISAMT
		AR_OPEN::OTHCHG		= OTHCHG
		AR_OPEN::RECNUM		= ""
		AR_OPEN::CHKNUM		= ""
		AR_OPEN::ARACCT		= OE_ACCOUNT_READ::ACCOUNT
		AR_OPEN::SUBACC		= ""
		AR_OPEN::SALNUM		= OE_ORDERJOUR::SALESMAN

		AR_OPEN::DESCR		= OE_ORDERJOUR::ORDNUM + &
			"-" + NUM1$(PAY%)

		AR_OPEN::BATCH		= BATCH_NUMBER
		AR_OPEN::UPDATED	= ICPERIOD
		AR_OPEN::CLOSEDATE	= ""
		AR_OPEN::DUEDATE	= DUE_DATE$
		AR_OPEN::DISCOUNTDATE	= DISC_DATE$

		!
		! Call the post function
		!
		GOTO Aborted &
			IF AR_TRAN_POSTAR(OPT_ADDREC, SUBOPT_REGISTER, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, AR_OPEN, &
			AR_OPEN_DIST, AR_SALTAXLED, ICPERIOD) <> CMC$_NORMAL

	NEXT PAY%

	!
	! Post any payments
	!
	IF OE_ORDERJOUR::AMTPAID <> 0.0
	THEN
		!
		! Generate a AR record to pass through to the post function
		!
		AR_OPEN::CUSNUM		= OE_ORDERJOUR::CUSNUM
		AR_OPEN::INVNUM		= OE_ORDERJOUR::INVNUM
		AR_OPEN::TRATYP		= "09"
		AR_OPEN::TRADAT		= OE_ORDERJOUR::SHIPDATE
		AR_OPEN::SALAMT		= -OE_ORDERJOUR::AMTPAID
		AR_OPEN::DISAMT		= 0.0
		AR_OPEN::OTHCHG		= 0.0
		AR_OPEN::RECNUM		= OE_ORDERJOUR::DEPOSIT
		AR_OPEN::CHKNUM		= OE_ORDERJOUR::CHECK
		AR_OPEN::ARACCT		= OE_ACCOUNT_READ::ACCOUNT
		AR_OPEN::SUBACC		= ""
		AR_OPEN::SALNUM		= OE_ORDERJOUR::SALESMAN
		AR_OPEN::DESCR		= OE_ORDERJOUR::ORDNUM
		AR_OPEN::BATCH		= BATCH_NUMBER
		AR_OPEN::UPDATED	= ICPERIOD
		AR_OPEN::CLOSEDATE	= ""
		AR_OPEN::DUEDATE	= ""
		AR_OPEN::DISCOUNTDATE	= ""

		!
		! Call the post function
		!
		GOTO Aborted &
			IF AR_TRAN_POSTAR(OPT_ADDREC, SUBOPT_REGISTER, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, AR_OPEN, &
			AR_OPEN_DIST, AR_SALTAXLED, ICPERIOD) <> CMC$_NORMAL

		!
		! Now, lower the AR account
		!
		EXIT_STATUS = GL_EXAM_CHART(OE_ACCOUNT_READ::ACCOUNT, GL_CHART_EXAM)

		SELECT EXIT_STATUS

		!
		! Account number defined
		!
		CASE CMC$_NORMAL

		!
		! Undefined Account Number
		!
		CASE CMC$_UNDEFINED
			TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, &
				CMC$_LEFT) + &
				" (Order Acct)* "  + &
				OE_ACCOUNT_READ::ACCOUNT

			!
			! Keep undefined codes
			!
			GOTO Aborted &
				IF OUTP_UNDEFCODES(OPT_ADDREC, &
				TITLE(), UTL_REPORTX, TEXT$) <> CMC$_NORMAL

		!
		! Weird happenin's
		!
		CASE ELSE
			GOTO Aborted

		END SELECT

		GL_YYYY_PP::ACCT	= OE_ACCOUNT_READ::ACCOUNT
		GL_YYYY_PP::AMOUNT	= -OE_ORDERJOUR::AMTPAID
		GL_YYYY_PP::UNITS	= 0.0
		GL_YYYY_PP::DESCR	= GL_CHART_EXAM::DESCR
		GL_YYYY_PP::CKNO	= OE_ORDERJOUR::DEPOSIT

		EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, ICPERIOD)

		!
		! And post to the cash account
		!
		EXIT_STATUS = GL_EXAM_CHART(PS_CASHREG::PETTYCASH, GL_CHART_EXAM)

		SELECT EXIT_STATUS

		!
		! Account number defined
		!
		CASE CMC$_NORMAL

		!
		! Undefined Account Number
		!
		CASE CMC$_UNDEFINED
			TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, &
				CMC$_LEFT) + &
				" (Order Acct)* "  + &
				OE_ACCOUNT_READ::ACCOUNT

			!
			! Keep undefined codes
			!
			GOTO Aborted &
				IF OUTP_UNDEFCODES(OPT_ADDREC, &
				TITLE(), UTL_REPORTX, TEXT$) <> CMC$_NORMAL

		!
		! Weird happenin's
		!
		CASE ELSE
			GOTO Aborted

		END SELECT

		GL_YYYY_PP::ACCT	= PS_CASHREG::PETTYCASH
		GL_YYYY_PP::AMOUNT	= OE_ORDERJOUR::AMTPAID
		GL_YYYY_PP::UNITS	= 0.0
		GL_YYYY_PP::DESCR	= GL_CHART_EXAM::DESCR
		GL_YYYY_PP::CKNO	= OE_ORDERJOUR::DEPOSIT

		EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, ICPERIOD)

		GL_YYYY_PP::CKNO	= ""

	END IF

 GoToNextHeader:
	IF SORTBY$ <> ""
	THEN
		!
		! Print out ~Totals~
		!
		TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT) + &
			SPACE$(9%) + &
			FORMAT$(ORDER_TAX, "#,###,###.##") + "  " + &
			FORMAT$(ORDER_DISC, "#,###,###.##") + "  " + &
			FORMAT$(OE_ORDERJOUR::FREIGHT, "#,###,###.##") + &
			"  " + &
			FORMAT$(OE_ORDERJOUR::HANDLING, "#,###,###.##") + &
			"  " + &
			FORMAT$(OE_ORDERJOUR::MISC, "#,###,###.##") + "  " + &
			OE_ORDERJOUR::MISCACCT + "  " + &
			OE_ORDERJOUR::SHIPVIA + "       " + &
			FORMAT$(ORDER_TOT, "#,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, -1%)

		PPRICE = LTOTAL - ORDER_DISC
		IF (PPRICE = 0.0)
		THEN
			PPERCENT = 0.0
		ELSE
			PPERCENT = ((PPRICE - LCOST) / PPRICE) * 100.0
		END IF

		TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT) + &
			SPACE$(9%) + &
			"Price: " + &
			FORMAT$(PPRICE, "#,###,###.## ") + &
			"Cost: " + &
			FORMAT$(LCOST, "#,###,###.## ") + &
			"Profit: " + &
			FORMAT$(PPRICE - LCOST, "#,###,###.## ") + &
			"Percentage:" + &
			FORMAT$(PPERCENT, "#####.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, -2%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), "", -3%)

		!
		! Find Sub Totals
		!
		ORDDISC_ST = ORDDISC_ST + ORDER_DISC
		TAX_ST = TAX_ST + ORDER_TAX
		FREIGHT_ST = FREIGHT_ST + OE_ORDERJOUR::FREIGHT
		MISC_ST = MISC_ST + OE_ORDERJOUR::MISC
		TOTAL_ST = TOTAL_ST + ORDER_TOT
	END IF

	%PAGE

	!*******************************************************************
	! Handle WP Register
	!*******************************************************************

	LINECNT$ = "0000"
	WPREG_FLAG% = 0%

	!
	! Find the first line item for the header
	!
	WHILE WP_READ_REGLINE(CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT), &
		LINECNT$, "GT", WP_REGLINE_READ, QTY()) = CMC$_NORMAL

		LINECNT$ = WP_REGLINE_READ::LLINE
		WPREG_FLAG% = -1%
	NEXT

3400	WHEN ERROR IN
		FIND #WP_ORDERLINE.CH%, &
			KEY #0% EQ CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT)
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE ReadHeader IF ERR = 155% OR ERR = 9%
		FILENAME$ = "WP_ORDERLINE"
		CONTINUE HelpError
	END WHEN

 WPLineItem:
	!
	! Get the (next) line item
	!
3410	WHEN ERROR IN
		GET #WP_ORDERLINE.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE ProcessWOHeader IF ERR = 11%
		FILENAME$ = "WP_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	!
	! Finish up the header if we're done with the line items
	!
	GOTO ProcessWOHeader IF WP_ORDERLINE::JOB <> &
		CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT)

	V% = FUNC_INCREMENT(LINECNT$)

	WP_REGLINE::JOB		= WP_ORDERLINE::JOB
	WP_REGLINE::LLINE	= LINECNT$
	WP_REGLINE::REC_TYPE	= "01"
	WP_REGLINE::ITEMCODE	= WP_ORDERLINE::ITEMCODE
	WP_REGLINE::QTY		= WP_ORDERLINE::QTY
	WP_REGLINE::COST	= WP_ORDERLINE::COST
	WP_REGLINE::POST_DATE	= POSTDATE
	WP_REGLINE::POST_TIME	= POSTTIME
	WP_REGLINE::BATCH	= BATCH_NUMBER
	WP_REGLINE::TTYPE	= WP_ORDERLINE::TTYPE
	WP_REGLINE::DESCR	= WP_ORDERLINE::DESCR
	WP_REGLINE::START_DATE  = WP_ORDERLINE::START_DATE
	WP_REGLINE::COMP_DATE   = WP_ORDERLINE::COMP_DATE

	!
	! Call the post function for orders
	!
	GOTO Aborted &
		IF WP_TRAN_POST(OPT_ADDREC, SUBOPT_LINEITEM, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		JC_JOB, WP_REGLINE) <> CMC$_NORMAL

	!
	! Bypass post to inventory if not a Material line
	!
	GOTO WPLineItem IF WP_ORDERLINE::TTYPE <> "M"

	!
	! Check Product
	!
	EXIT_STATUS = PD_EXAM_PRODUCT(WP_ORDERLINE::ITEMCODE, PD_PRODUCT_EXAM)

	SELECT EXIT_STATUS

	!
	! Success; go on
	!
	CASE CMC$_NORMAL

	!
	! Product number undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED, CMC$_WARNING
		TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, &
			CMC$_LEFT) + " " + &
			LINECNT$ + " " + &
			" (WPProdNum)* " + &
			WP_ORDERLINE::ITEMCODE

	!
	! Untrapped error
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	IC_TRANSACTION::PRODUCT		= WP_ORDERLINE::ITEMCODE
	IC_TRANSACTION::LOCATION	= OE_ORDERJOUR::LOCATION
	IC_TRANSACTION::TRANS_DATE	= OE_ORDERJOUR::ORDDATE
	IC_TRANSACTION::PRIMARY_REF	= ""
	IC_TRANSACTION::CROSS_REF	= OE_ORDERJOUR::CUSNUM
	IC_TRANSACTION::SUBACCOUNT	= WP_ORDERLINE::JOB
	IC_TRANSACTION::LOT		= RIGHT(WP_REGLINE::LLINE, 3%)
	IC_TRANSACTION::STATIONMAN	= OE_ORDERJOUR::OPERATOR
	IC_TRANSACTION::TYPE_A		= "WO"
	IC_TRANSACTION::QUANTITY_A	= WP_ORDERLINE::QTY
	IC_TRANSACTION::TYPE_B		= ""
	IC_TRANSACTION::QUANTITY_B	= 0.0
	IC_TRANSACTION::COST		= &
		FUNC_ROUND(WP_ORDERLINE::COST * WP_ORDERLINE::QTY, 2%)
	IC_TRANSACTION::PRICE		= 0.0
	IC_TRANSACTION::TRANSACCT	= ""
	IC_TRANSACTION::POSTDATE	= POSTDATE
	IC_TRANSACTION::POSTTIME	= POSTTIME
	IC_TRANSACTION::BATCH		= BATCH_NUMBER

	!
	! Post order to inventory transaction file
	!
	EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

	!
	! Find the first line item for the req line
	!
3450	WHEN ERROR IN
		FIND #WP_REQLINE.CH%, &
			KEY #0% EQ WP_ORDERLINE::JOB + WP_ORDERLINE::LLINE
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE WPLineItem IF ERR = 155% OR ERR = 9%
		FILENAME$ = "WP_REQLINE"
		CONTINUE HelpError
	END WHEN

 WPReqLine:
	!
	! Get the (next) line item
	!
3460	WHEN ERROR IN
		GET #WP_REQLINE.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE WPLineItem IF ERR = 11%
		FILENAME$ = "WP_REQLINE"
		CONTINUE HelpError
	END WHEN

	!
	! Finish up  if we're done with the line items
	!
	GOTO WPLineItem IF WP_ORDERLINE::JOB + WP_ORDERLINE::LLINE <> &
		WP_REQLINE::JOB + WP_REQLINE::LLINE

	!
	! Check Product
	!
	EXIT_STATUS = PD_EXAM_PRODUCT(WP_REQLINE::PRODUCT, PD_PRODUCT_EXAM)

	SELECT EXIT_STATUS

	!
	! Success; go on
	!
	CASE CMC$_NORMAL

	!
	! Product number undefined; set flag and go on
	!
	CASE CMC$_UNDEFINED, CMC$_WARNING
		TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, &
			CMC$_LEFT) + " " + &
			LINECNT$ + " " + &
			" (WPReqProdNum)* " + &
			WP_REQLINE::PRODUCT

	!
	! Untrapped error
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	COST = PC_READ_COST(WP_REQLINE::PRODUCT, OE_ORDERJOUR::LOCATION, &
		OE_ORDERJOUR::ORDDATE, "")

	PD_PRODUCT_EXAM::PRODUCT_FACTOR = 1.0 &
		IF PD_PRODUCT_EXAM::PRODUCT_FACTOR = 0.0

	WP_REQREGISTER::JOB	= WP_REQLINE::JOB
	WP_REQREGISTER::LLINE	= WP_REQLINE::LLINE
	WP_REQREGISTER::REQNUM	= WP_REQLINE::REQNUM
	WP_REQREGISTER::REQLIN	= WP_REQLINE::REQLINE
	WP_REQREGISTER::RECTYP   = "01"
	WP_REQREGISTER::PRODUCT  = WP_REQLINE::PRODUCT
	WP_REQREGISTER::LOCATION = OE_ORDERJOUR::LOCATION
	WP_REQREGISTER::QTY      = WP_REQLINE::QTY
	WP_REQREGISTER::AMT      = COST
	WP_REQREGISTER::TRANDATE = OE_ORDERJOUR::ORDDATE
	WP_REQREGISTER::OPERATOR = OE_ORDERJOUR::OPERATOR
	WP_REQREGISTER::PERIOD   = ICPERIOD
	WP_REQREGISTER::POSTDATE = POSTDATE
	WP_REQREGISTER::POSTTIME = POSTTIME
	WP_REQREGISTER::BATCH    = BATCH_NUMBER

	!
	! Call the post function for orders
	!
	GOTO Aborted &
		IF WP_TRAN_POSTREQ(OPT_ADDREC, SUBOPT_LINEITEM, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		WP_REQREGISTER) <> CMC$_NORMAL

	IC_TRANSACTION::PRODUCT		= WP_REQLINE::PRODUCT
	IC_TRANSACTION::LOCATION	= OE_ORDERJOUR::LOCATION
	IC_TRANSACTION::TRANS_DATE	= OE_ORDERJOUR::ORDDATE

	IC_TRANSACTION::PRIMARY_REF	= RIGHT(WP_REQLINE::LLINE, 3%) + &
		WP_REQLINE::REQNUM + WP_REQLINE::REQLINE

	IC_TRANSACTION::CROSS_REF	= OE_ORDERJOUR::CUSNUM
	IC_TRANSACTION::SUBACCOUNT	= WP_REQLINE::JOB
	IC_TRANSACTION::LOT		= ""
	IC_TRANSACTION::STATIONMAN	= OE_ORDERJOUR::OPERATOR
	IC_TRANSACTION::TYPE_A		= "RQ"

	IC_TRANSACTION::QUANTITY_A	= FUNC_ROUND( &
		-WP_REQLINE::QTY / PD_PRODUCT_EXAM::PRODUCT_FACTOR, 3%)

	IC_TRANSACTION::TYPE_B		= ""
	IC_TRANSACTION::QUANTITY_B	= 0.0

	IC_TRANSACTION::COST		= ABS(FUNC_ROUND(COST * &
		WP_REQLINE::QTY / PD_PRODUCT_EXAM::PRODUCT_FACTOR, 2%))

	IC_TRANSACTION::PRICE		= 0.0
	IC_TRANSACTION::TRANSACCT	= ""
	IC_TRANSACTION::POSTDATE	= POSTDATE
	IC_TRANSACTION::POSTTIME	= POSTTIME
	IC_TRANSACTION::BATCH		= BATCH_NUMBER

	!
	! Post order to inventory transaction file
	!
	EXIT_STATUS = IC_TRAN_POST(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, IC_TRANSACTION, ICPERIOD)

	GOTO WPReqLine

 ProcessWOHeader:
	!
	! Process the header
	!
	!
	! Generate a JC_JOB record to pass through to the
	! post function
	!
	IF WPREG_FLAG% = 0%
	THEN
		IF TOPDESC$ = ""
		THEN
			!
			! Hopefully this won't ever happen, because there
			! should always be at least one line, but ...
			!
			TOPDESC$ = OE_ORDERJOUR::NOTES(0%)
		END IF

		JC_JOB::JOB		= &
			CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT)
		JC_JOB::SUBJECT		= "J"
		JC_JOB::DESCR		= TOPDESC$
		JC_JOB::TTYPE		= OE_ORDERJOUR::ORDTYPE
		JC_JOB::CLASS		= OE_ORDERJOUR::ORDCAT
		JC_JOB::BDATE		= OE_ORDERJOUR::ORDDATE
		JC_JOB::SSTATUS		= "A"
		JC_JOB::EDATE		= ""
		JC_JOB::LOCATION	= OE_ORDERJOUR::LOCATION
		JC_JOB::OPERATOR	= OE_ORDERJOUR::OPERATOR
		JC_JOB::REFNO		= OE_ORDERJOUR::CUSNUM
		JC_JOB::BATCH		= BATCH_NUMBER
		JC_JOB::POST_TIME	= POSTTIME
		JC_JOB::POST_DATE	= POSTDATE

		!
		! Call the post function
		!
		GOTO Aborted &
			IF WP_TRAN_POST(OPT_ADDREC, SUBOPT_REGISTER, &
			BATCH_NUMBER, TITLE(), UTL_REPORTX, &
			JC_JOB, WP_REGLINE) <> CMC$_NORMAL
	END IF

	GOTO ReadHeader

	%PAGE

3500	!*******************************************************************
	! Post the Cash In/Out journal
	!*******************************************************************

	WHEN ERROR IN
		RESET #PS_CASHINOUT.CH%
	USE
		CONTINUE 3590
	END WHEN

3510	WHEN ERROR IN
		GET #PS_CASHINOUT.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 3590
	END WHEN

3520	EXIT_STATUS = GL_EXAM_CHART(PS_CASHREG::PETTYCASH, &
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
		TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, &
			CMC$_LEFT) + " " + &
			FORMAT$(LIN%, "###") + " " + &
			" (Cashreg Petty Acct)* " + &
			PS_CASHREG::PETTYCASH

		!
		! Keep undefined codes
		!
		GOTO Aborted &
			IF OUTP_UNDEFCODES(OPT_ADDREC, &
			TITLE(), UTL_REPORTX, TEXT$) <> &
			CMC$_NORMAL

	!
	! Weird happenin's
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	GL_YYYY_PP::ACCT	= PS_CASHREG::PETTYCASH
	GL_YYYY_PP::AMOUNT	= PS_CASHINOUT::AMOUNT
	GL_YYYY_PP::SOURCE	= "OIJ"
	GL_YYYY_PP::REFNO	= ""
	GL_YYYY_PP::TRANDAT	= PS_CASHINOUT::CASHDATE
	GL_YYYY_PP::DESCR	= PS_CASHINOUT::NOTES
	GL_YYYY_PP::XREFNO	= PS_CASHINOUT::OPERATOR
	GL_YYYY_PP::CKNO	= PS_CASHINOUT::DEPOSIT
	GL_YYYY_PP::TRANKEY	= ""
	GL_YYYY_PP::SUBACC	= ""
	GL_YYYY_PP::OPERATION	= ""
	GL_YYYY_PP::HOURS	= 0.0
	GL_YYYY_PP::UPDSTA	= ""
	GL_YYYY_PP::POSDAT	= POSTDATE
	GL_YYYY_PP::POSTIM	= POSTTIME
	GL_YYYY_PP::BTHNUM	= BATCH_NUMBER

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, ICPERIOD)

	EXIT_STATUS = GL_EXAM_CHART(PS_CASHINOUT::ACCOUNT, &
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
		TEXT$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, &
			CMC$_LEFT) + " " + &
			FORMAT$(LIN%, "###") + " " + &
			" (Cashinout Acct)* " + &
			PS_CASHINOUT::ACCOUNT

		!
		! Keep undefined codes
		!
		GOTO Aborted &
			IF OUTP_UNDEFCODES(OPT_ADDREC, &
			TITLE(), UTL_REPORTX, TEXT$) <> &
			CMC$_NORMAL

	!
	! Weird happenin's
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	GL_YYYY_PP::ACCT	= PS_CASHINOUT::ACCOUNT
	GL_YYYY_PP::AMOUNT	= -PS_CASHINOUT::AMOUNT

	EXIT_STATUS = GL_TRAN_POSTGL(OPT_ADDREC, CHECK_PERIOD, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, GL_YYYY_PP, GL_CHART_EXAM, ICPERIOD)

	GOTO 3510

3590	! GOTO Confirm

	%PAGE

 Confirm:
	!******************************************************************
	! Confirm posting
	!******************************************************************

	!
	! Almost done if just report
	!
	IF SORTBY$ <> ""
	THEN
		GOSUB SubTotal IF CTR%
	END IF

	EXIT_STATUS = IC_TRAN_POST(OPT_CONFIRM, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "", ICPERIOD)

	EXIT_STATUS = EXIT_STATUS AND &
		GL_TRAN_POSTGL(OPT_CONFIRM, SUBOPT_NOOPT, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "", "", ICPERIOD)

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

	GOTO Aborted &
		IF EXIT_STATUS <> CMC$_NORMAL

	%PAGE

	!******************************************************************
	!	1) Since everything's correct and we have the user's
	!		permission, transfer all data from the temporary
	!		files into the ledger
	!******************************************************************
	!
	! Begin posting
	!
	! Post to the register header
	!
	GOTO Interrupt &
		IF OE_TRAN_POST(OPT_POSTFILE, SUBOPT_REGISTER, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "") <> CMC$_NORMAL

	!
	! Post to the register lines
	!
	GOTO Interrupt &
		IF OE_TRAN_POST(OPT_POSTFILE, SUBOPT_LINEITEM, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "") <> CMC$_NORMAL

	!
	! Post to the register lines
	!
	GOTO Interrupt IF MO_TRAN_POST(OPT_POSTFILE, SUBOPT_LINEITEM, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "", "") <> CMC$_NORMAL

	!
	! Post to the register option lines
	!
	GOTO Interrupt IF MO_TRAN_POST(OPT_POSTFILE, SUBOPT_LINEITEMOPT, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, "", "") <> CMC$_NORMAL

	!
	! Post to the WP register
	!
	GOTO Interrupt &
		IF WP_TRAN_POST(OPT_POSTFILE, SUBOPT_REGISTER, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "") <> CMC$_NORMAL

	!
	! Post to the WP register lines
	!
	GOTO Interrupt &
		IF WP_TRAN_POST(OPT_POSTFILE, SUBOPT_LINEITEM, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "") <> CMC$_NORMAL

	!
	! Post to the WP req register
	!
	GOTO Interrupt &
		IF WP_TRAN_POSTREQ(OPT_POSTFILE, &
			SUBOPT_LINEITEM, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX,  "") <> CMC$_NORMAL

	!
	! Post to the inventory ledger
	!
	GOTO Interrupt IF IC_TRAN_POST(OPT_POSTFILE, SUBOPT_LEDGER, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		"", ICPERIOD) <> CMC$_NORMAL

	IF POSTGL$ <> "N" AND POSTGL$ <> "M"
	THEN
		!
		! Post to the general ledger
		!
		GOTO Interrupt &
			IF GL_TRAN_POSTGL(OPT_POSTFILE, SUBOPT_DETAIL, &
			BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", ICPERIOD) <> CMC$_NORMAL

		!
		! Post the AR Sales Journal header
		!
		GOTO Interrupt &
			IF AR_TRAN_POSTAR(OPT_POSTFILE, SUBOPT_REGISTER, &
			BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", ICPERIOD) <> &
			CMC$_NORMAL

		!
		! Continue by posting the Sales Journal line items
		!
		GOTO Interrupt &
			IF AR_TRAN_POSTAR(OPT_POSTFILE, SUBOPT_LINEITEM, &
			BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", ICPERIOD) <> &
			CMC$_NORMAL

		!
		! Post any Sales Tax items
		!
		GOTO Interrupt &
			IF AR_TRAN_POSTAR(OPT_POSTFILE, SUBOPT_LEDGER, &
			BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "", ICPERIOD) <> &
			CMC$_NORMAL

	END IF

	%PAGE

	!
	! Remove files
	!
5000	CLOSE OE_ORDERJOUR.CH%
	CLOSE OE_ORDERLINE.CH%
	CLOSE MO_ORDERLINE.CH%
	CLOSE MO_ORDERLINEOPT.CH%
	CLOSE WP_ORDERLINE.CH%
	CLOSE WP_REQLINE.CH%

5010	IF POSTGL$ = "M"
	THEN

		CALL FIND_FILE("CMC_OUTBOX:OE_ORDERJOUR_*_%%.JRL", &
			ARRAY_NEW_FILE$(), 16%, "", "")

		FILE% = VAL%(ARRAY_NEW_FILE$(0%))
		NEW_BATCH_NO$ = BATCH_NO$

 NewBatch:
		FOR I% = 1% TO FILE%

			IF NEW_BATCH_NO$ = &
				EDIT$(RIGHT(ARRAY_NEW_FILE$(I%), 14%), -1%)
			THEN
				DELKA% = LEN(BATCH_NO$)
				NEW_BATCH_NO$ = LEFT(BATCH_NO$, DELKA% - 2%) + &
					FORMAT$(I% + 1%, "<0>#")
				GOTO NewBatch
			END IF
		NEXT I%

		WHEN ERROR IN
			NAME OE_ORDERJOUR.DEV$ + "OE_ORDERJOUR_" + &
				BATCH_NO$ + ".JRL" AS &
				"CMC_OUTBOX:OE_ORDERJOUR_" + NEW_BATCH_NO$ + ".JRL"
			NAME OE_ORDERLINE.DEV$ + "OE_ORDERLINE_" + &
				BATCH_NO$ + ".JRL" AS &
				"CMC_OUTBOX:OE_ORDERLINE_" + NEW_BATCH_NO$ + ".JRL"
		USE
			CONTINUE 5030
		END WHEN

		GOTO 5030
	ELSE
		SMG_STATUS% = LIB$DELETE_FILE(OE_ORDERJOUR.DEV$ + &
			"OE_ORDERJOUR_" + BATCH_NO$ + ".JRL;*")
	END IF

5020	SMG_STATUS% = LIB$DELETE_FILE(OE_ORDERLINE.DEV$ + &
		"OE_ORDERLINE_" + BATCH_NO$ + ".JRL;*")

5030	CALL READ_DEVICE("PS_CASHINOUT", PS_CASHINOUT.DEV$, STAT%)

	SMG_STATUS% = LIB$DELETE_FILE(PS_CASHINOUT.DEV$ + &
		"PS_CASHINOUT_" + BATCH_NO$ + ".JRL;*")

5040	SMG_STATUS% = LIB$DELETE_FILE(MO_ORDERLINE.DEV$ + &
		"MO_ORDERLINE_" + BATCH_NO$ + ".JRL;*")

5050	SMG_STATUS% = LIB$DELETE_FILE(MO_ORDERLINEOPT.DEV$ + &
		"MO_ORDERLINEOPT_" + BATCH_NO$ + ".JRL;*")

5060	SMG_STATUS% = LIB$DELETE_FILE(WP_ORDERLINE.DEV$ + &
		"WP_ORDERLINE_" + BATCH_NO$ + ".JRL;*")

5070	SMG_STATUS% = LIB$DELETE_FILE(WP_REQLINE.DEV$ + &
		"WP_REQLINE_" + BATCH_NO$ + ".JRL;*")

 Complete:
	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "OE_ORDERJOUR", BATCH_NO$, "", "")

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

 SubTotal:
	!
	! Print out subtotal line
	!
	IF SORTBY$ <> "D"
	THEN
		!
		! Print out ~Totals~
		!
		TEXT$ = "SUB_TOTAL       " + &
			FORMAT$(ORDDISC_ST, "#,###,###.##") + "  " + &
			FORMAT$(TAX_ST, "#,###,###.##") + SPACE$(15%) + &
			FORMAT$(FREIGHT_ST, "#,###,###.##") + " " + &
			FORMAT$(MISC_ST, "#,###,###.##") + SPACE$(38%) + &
			FORMAT$(TOTAL_ST, "#,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), "", -1%)
	END IF

	!
	! Set values to zero
	!
	CTR% = 0%
	FREIGHT_ST, ORDDISC_ST, MISC_ST, TAX_ST, TOTAL_ST = 0.0

	RETURN

 Aborted:
	!******************************************************************
	! Abort process
	!******************************************************************
	TEXT$ = "%" + CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT) + " " + &
		FORMAT$(LIN%, "<0>###") + " aborted"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	GOTO ExitProgram &
		IF SORTBY$ <> ""

	IF INTR_STATUS <> CMC$_WARNING
	THEN
		EXIT_STATUS = ASSG_POSTBATCH(OPT_ABORT,	BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "OE_ORDERJOUR", &
			BATCH_NO$, "", "")

	END IF

	GOTO ExitProgram

	%PAGE

 Interrupt:
	!******************************************************************
	! Interrupt process
	!******************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_INTERRUPT, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "OE_ORDERJOUR", BATCH_NO$, "", "")

	GOTO ExitProgram

	%PAGE

18100	WHEN ERROR IN
		GET #MO_MODELCODE.CH%, &
			KEY #0% EQ MO_ORDERLINE::MODELCODE, &
			REGARDLESS
	USE
		MO_MODELCODE::DESCR = MO_ORDERLINE::MODELCODE

		CONTINUE 18110 IF ERR = 155% OR ERR = 131% OR ERR = 9%
		FILENAME$ = "MO_MODELCODE"
		CONTINUE HelpError
	END WHEN

18110	RETURN

	%PAGE

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

	!******************************************************************
	! End of posting program PS_POST_TICKET
	!******************************************************************
32767	END
