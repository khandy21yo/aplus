1	%TITLE "AR Collection Analysis and Projection"
	%SBTTL "AR_RPRT_ANALYS_AGE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1990 BY
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
	! ID:AR035
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program estimates the amount of cash to be received
	!	by customers based on past history.
	!	.b
	!	^*Note:\*  This program works best when all customers are open item,
	!	since balance forward does not link an invoice to the
	!	cash receipts.
	!	.b
	!	^*Method of Calculation:\*  To estimate the expected date(s)
	!	that a customer will pay on,
	!	this program looks at the customers past history,
	!	and estimates how many days normally exist between an invoice
	!	and the payments, as well as the percentage paid.
	!	It then looks at all unpaid invoices, and estimates on what days
	!	the payments will be made based on this past performance.
	!	.b
	!	This program only estimates when existing invoices will be
	!	paid.  It does not project for future invoices.
	!	.lm -5
	!
	! Index:
	!	.X Cash>Projection
	!	.X Projection>Cash
	!	.X Receipts>Projection
	!	.X Projection>Receipts
	!
	! Option:
	!
	!
	! Input:
	!
	!
	! Output:
	!
	!
	! Example:
	!
	!
	! Author:
	!
	!	10/11/90 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_RPRT_ANALYS_AGE/LINE
	!	$ LINK/EXECUTABLE=AR_EXE:*.EXE AR_RPRT_ANALYS_AGE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_RPRT_ANALYS_AGE.OBJ;*
	!
	! Modification history:
	!
	!	06/14/93 - Kevin Handy
	!		Added REGARDLESS and CLOSE to AR_CONTROL.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/19/96 - Kevin Handy
	!		Add transaction type "11", adjustment.
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD		AR_CONTROL

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN)		AR_OPEN_CDD		AR_OPEN

	%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.HB"
	MAP (AR_CLOSED)		AR_CLOSED_CDD		AR_CLOSED

	!
	! Declare variables and constants
	!
	DECLARE	STRING	LYT_LINE

	DECLARE INTEGER CONSTANT DATE_COUNT = 11%
	DIM DATE_LIST$(DATE_COUNT), DATE_EXPECT(DATE_COUNT), &
		GRAND_DATE_EXPECT(DATE_COUNT)

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Init:
	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes the printing
	!	to begin with the selected Item _#. The
	!	value must be in agreement with field (05) Sort by.
	!	.b
	!	A blank field will cause the report to start with the first
	!	Item _# in the file.
	!	.lm -5
	!
	! Index:
	!	.X From>Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item _#\* field causes the printing
	!	to end with the selected Item _#. The value must
	!	be in agreement with field (05) Sort by.
	!	.b
	!	A blank field will cause the report to end with the last
	!	Item _# in the file.
	!	.lm -5
	!
	! Index:
	!	.x To>Item
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)
	!++
	! Abstract:FLD05
	!	^*(05) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines the order
	!	in which the report will print.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*N\* - Number
	!	.te
	!	^*T\* - Type
	!	.te
	!	^*C\* - Category
	!	.te
	!	^*A\* - Alphabetical
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort by
	!
	!--

	SELECT SORTBY$
	CASE "N"
		K_NUM% = 0%
	CASE "T"
		K_NUM% = 1%
	CASE "C"
		K_NUM% = 2%
	CASE "A"
		K_NUM% = 3%
	END SELECT

	PRINTBY$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Print By\*
	!	.b
	!	.lm +5
	!	The ^*Print By\* field enters the code indicating the
	!	order in which the report will be printed.
	!	.lm -5
	!
	! Index:
	!	.x Print By
	!
	!--

	BASEDATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(6%), -1%))
	!++
	! Abstract:FLD07
	!	^*(07) Base Date\*
	!	.b
	!	.lm +5
	!	The ^*Base Day\* field is used with
	!	the ^*Print By\* field (06) to determine the columns
	!	that will be printed.
	!	.lm -5
	!
	! Index:
	!	.x Base Date
	!
	!--

	DEFAULT_AVERAGE% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(7%), -1%))
	!++
	! Abstract:FLD08
	!	^*(08) Default Days\*
	!	.b
	!	.lm +5
	!	The ^*Default Days\* period enters the number of days
	!	which will be used as a default for customers who do not have a history.
	!	.lm -5
	!
	! Index:
	!
	!--

	DEFAULT_PCT = VAL(EDIT$(UTL_REPORTX::OPTDEF(8%), -1%)) / 100.0
	!++
	! Abstract:FLD09
	!	^*(09) Default Percentage\*
	!	.b
	!	.lm +5
	!	The ^*Default Percentage\* field is used with
	!	the ^*Default Days\* field (08) for customers that
	!	do not have history yet.
	!	.b
	!	This field may be set approximately by entering a
	!	default value for a first run, then reviewing the
	!	total at the bottom and entering the
	!	average percentage of invoice paid from there.
	!	.lm -5
	!
	! Index:
	!	.x Default Percentage
	!
	!--
	!*******************************************************************
	! Determine dates to work with
	!*******************************************************************

	DEF FNENDMON$(MON$)

		TEMP$ = LEFT(DATE_INVMCODE(DATE_MONCODE(MON$) + 1%), 6%) + "01"
		FNENDMON$ = DATE_INVDCODE(DATE_DAYCODE(TEMP$) - 1%)

	FNEND

	SELECT PRINTBY$

	!
	! Monthly
	!
	CASE "M"
		TITLE$(2%) = "By Month"
		BASEDATE$ = LEFT(BASEDATE$, 6%) + "01"

		!
		! Calculate previous date
		!
		S1DAY% = DATE_MONCODE(BASEDATE$) - 1%
		DATE_LIST$(1%) = FNENDMON$(DATE_INVMCODE(S1DAY%))

		!
		! Calculate foreward days
		!
		S1DAY% = DATE_MONCODE(BASEDATE$) - 1%

		FOR I% = 2% TO DATE_COUNT

			S1DAY% = S1DAY% + 1%
			DATE_LIST$(I%) = FNENDMON$(DATE_INVMCODE(S1DAY%))

		NEXT I%

	!
	! Weekly
	!
	CASE "W"
		TITLE$(2%) = "By Week"

		!
		! Calculate previous date
		!
		S1DAY% = DATE_DAYCODE(BASEDATE$) - 7%
		DATE_LIST$(1%) = DATE_INVDCODE(S1DAY%)

		!
		! Calculate foreward days
		!
		S1DAY% = DATE_DAYCODE(BASEDATE$) - 7%

		FOR I% = 2% TO DATE_COUNT

			S1DAY% = S1DAY% + 7%
			DATE_LIST$(I%) = DATE_INVDCODE(S1DAY%)

		NEXT I%

	!
	! Daily
	!
	CASE ELSE
		TITLE$(2%) = "By Day"

		!
		! Calculate previous date
		!
		S1DAY% = DATE_DAYCODE(BASEDATE$) - 1%
		S1DAY% = S1DAY% - 1% UNTIL DATE_DAYOFWEEK(S1DAY%) < 6%
		DATE_LIST$(1%) = DATE_INVDCODE(S1DAY%)

		!
		! Calculate foreward days
		!
		S1DAY% = DATE_DAYCODE(BASEDATE$) - 1%

		FOR I% = 2% TO DATE_COUNT

			S1DAY% = S1DAY% + 1%
			S1DAY% = S1DAY% + 1% UNTIL DATE_DAYOFWEEK(S1DAY%) < 6%
			DATE_LIST$(I%) = DATE_INVDCODE(S1DAY%)

		NEXT I%

	END SELECT

300	!
	! Open Customer file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

310	!
	! Open control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.OPN"
		GET #AR_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE AR_CONTROL.CH%
	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

330	!
	! Open open item file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.OPN"
	USE
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

340	!
	! Open closed item file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.OPN"
	USE
		CONTINUE 350 IF ERR = 5%
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

350	!

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Collection Analysis and Projection"
	TITLE$(3%) = "AR System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = LEFT(AR_CONTROL::CTITLE, 10%) + &
		" Name                           Average Days  Average %Paid"

	TITLE$(6%) = "            Previous "
	TITLE$(6%) = TITLE$(6%) + PRNT_DATE(DATE_LIST$(I%), 8%) + " " &
		FOR I% = 2% TO DATE_COUNT - 1%

	TITLE$(6%) = TITLE$(6%) + "    Future"
	TITLE$(7%) = ""

	!
	! Layouts for printed lines
	!
	LYT_LINE =""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	GRAND_COUNT_DAYS      = 0.0
	GRAND_COUNT_PAYMENTS% = 0%
	GRAND_COUNT_PCTPAY    = 0.0

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AR_35CUSTOM.CH%, KEY #K_NUM%
		ELSE
			FIND #AR_35CUSTOM.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AR_35CUSTOM.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORTBY$
	CASE "N"
		GOTO ExitTotal IF (AR_35CUSTOM::CUSNUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "T"
		GOTO ExitTotal IF (AR_35CUSTOM::TTYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "C"
		GOTO ExitTotal IF (AR_35CUSTOM::CATEGORY > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "A"
		GOTO ExitTotal IF (AR_35CUSTOM::ALPSRT > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	END SELECT

	!*******************************************************************
	! Initialize counters
	!*******************************************************************

	COUNT_DAYS = 0%
	COUNT_PCTPAY = 0.0
	COUNT_PAYMENTS% = 0%

17030	!*******************************************************************
	! Scan through open file for number of days
	!*******************************************************************

	WHEN ERROR IN
		GET #AR_OPEN.CH%, KEY #0% GE AR_35CUSTOM::CUSNUM, REGARDLESS
	USE
		CONTINUE 17200	! Skip this customer
	END WHEN

	GOTO 17200 IF AR_OPEN::CUSNUM <> AR_35CUSTOM::CUSNUM

	CURRENT_INVDAT$ = AR_OPEN::TRADAT
	CURRENT_INVNUM$ = AR_OPEN::INVNUM
	COUNT_INVAMT = 0.0

17040	IF AR_OPEN::CUSNUM = AR_35CUSTOM::CUSNUM
	THEN
		IF CURRENT_INVNUM$ <> AR_OPEN::INVNUM
		THEN
			CURRENT_INVNUM$ = AR_OPEN::INVNUM
			CURRENT_INVDAT$ = AR_OPEN::TRADAT
		END IF

		NET = AR_OPEN::SALAMT

		SELECT AR_OPEN::TRATYP

		!
		! Payment (Assume match invoice
		!
		CASE "09", "10", "11"
			COUNT1_DAYS = &
				DATE_DAYCODE(AR_OPEN::TRADAT) - &
				DATE_DAYCODE(CURRENT_INVDAT$)
			COUNT1_DAYS = 0.0 IF COUNT1_DAYS < 0%
			COUNT_DAYS = COUNT_DAYS + COUNT1_DAYS
			COUNT_PAYMENTS% = COUNT_PAYMENTS% + 1%

			IF (COUNT_INVAMT = 0.0)
			THEN
				COUNT_PCTPAY = COUNT_PCTPAY + 1.0
			ELSE
				COUNT1_PCTPAY = - NET / COUNT_INVAMT
				COUNT1_PCTPAY = 2.0 IF COUNT1_PCTPAY > 2.0
				COUNT1_PCTPAY = 0.0 IF COUNT1_PCTPAY < 0.0
				COUNT_PCTPAY = COUNT_PCTPAY + COUNT1_PCTPAY
			END IF
			CURRENT_INVDAT$ = AR_OPEN::TRADAT

		!
		! Cash sale (? Use)
		!
		CASE "02"
			COUNT_PCTPAY = COUNT_PCTPAY + 1.0
			COUNT_PAYMENTS% = COUNT_PAYMENTS% + 1%

		!
		! Everything else is an invoice amount
		!
		CASE ELSE
			COUNT_INVAMT = COUNT_INVAMT + NET

		END SELECT

		!
		! Continue to next record
		!
		WHEN ERROR IN
			GET #AR_OPEN.CH%, REGARDLESS
		USE
			CONTINUE 17050
		END WHEN

		GOTO 17040
	END IF

17050	!*******************************************************************
	! Scan through closedfile for number of days
	!*******************************************************************

	WHEN ERROR IN
		GET #AR_CLOSED.CH%, KEY #0% GE AR_35CUSTOM::CUSNUM, REGARDLESS
	USE
		CONTINUE 17070
	END WHEN

	CURRENT_INVDAT$ = AR_CLOSED::TRADAT
	CURRENT_INVNUM$ = AR_CLOSED::INVNUM
	COUNT_INVAMT = 0.0

17060	IF AR_CLOSED::CUSNUM = AR_35CUSTOM::CUSNUM
	THEN
		IF CURRENT_INVNUM$ <> AR_CLOSED::INVNUM
		THEN
			CURRENT_INVNUM$ = AR_CLOSED::INVNUM
			CURRENT_INVDAT$ = AR_CLOSED::TRADAT
		END IF

		NET = AR_CLOSED::SALAMT

		SELECT AR_CLOSED::TRATYP

		!
		! Payment (Assume match invoice
		!
		CASE "09", "10", "11"
			COUNT1_DAYS = &
				DATE_DAYCODE(AR_CLOSED::TRADAT) - &
				DATE_DAYCODE(CURRENT_INVDAT$)
			COUNT1_DAYS = 0.0 IF COUNT1_DAYS < 0%
			COUNT_DAYS = COUNT_DAYS + COUNT1_DAYS
			COUNT_PAYMENTS% = COUNT_PAYMENTS% + 1%

			IF (COUNT_INVAMT = 0.0)
			THEN
				COUNT_PCTPAY = COUNT_PCTPAY + 1.0
			ELSE
				COUNT1_PCTPAY = - NET / COUNT_INVAMT
				COUNT1_PCTPAY = 2.0 IF COUNT1_PCTPAY > 2.0
				COUNT1_PCTPAY = 0.0 IF COUNT1_PCTPAY < 0.0
				COUNT_PCTPAY = COUNT_PCTPAY + COUNT1_PCTPAY
			END IF
			CURRENT_INVDAT$ = AR_CLOSED::TRADAT

		!
		! Cash sale (? Use)
		!
		CASE "02"
			COUNT_PCTPAY = COUNT_PCTPAY + 1.0
			COUNT_PAYMENTS% = COUNT_PAYMENTS% + 1%

		!
		! Everything else is an invoice amount
		!
		CASE ELSE
			COUNT_INVAMT = COUNT_INVAMT + NET

		END SELECT

		!
		! Continue to next record
		!
		WHEN ERROR IN
			GET #AR_CLOSED.CH%, REGARDLESS
		USE
			CONTINUE 17070
		END WHEN

		GOTO 17060
	END IF

17070	!*******************************************************************
	! Print aging line
	!*******************************************************************

	IF COUNT_PAYMENTS% = 0%
	THEN
		AVERAGE% = DEFAULT_AVERAGE%
		AVERAGE_PCT = DEFAULT_PCT
	ELSE
		AVERAGE% = COUNT_DAYS / COUNT_PAYMENTS%
		AVERAGE_PCT = COUNT_PCTPAY / COUNT_PAYMENTS%
	END IF

	GRAND_COUNT_PCTPAY = GRAND_COUNT_PCTPAY + COUNT_PCTPAY
	GRAND_COUNT_DAYS = GRAND_COUNT_DAYS + COUNT_DAYS
	GRAND_COUNT_PAYMENTS% = GRAND_COUNT_PAYMENTS% + COUNT_PAYMENTS%

	TEXT$ = AR_35CUSTOM::CUSNUM + " " + &
		LEFT(AR_35CUSTOM::CUSNAM, 30%) + " " + &
		FORMAT$(AVERAGE%, "##########      ") + &
		FORMAT$(AVERAGE_PCT * 100.0, "#####.#### ")

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 2%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

17100	!*******************************************************************
	! Now, look through all invoices that are not paid off, and
	! gustimate when payment(s) will occur
	!*******************************************************************

	DATE_EXPECT(I%) = 0.0 FOR I% = 1% TO DATE_COUNT

	WHEN ERROR IN
		GET #AR_OPEN.CH%, KEY #0% GE AR_35CUSTOM::CUSNUM, REGARDLESS
	USE
		CONTINUE 17150
	END WHEN

	THIS_INVNUM$ = AR_OPEN::INVNUM
	THIS_INVDATE$ = AR_OPEN::TRADAT
	NET = 0.0
	PAY = 0.0

17110	!
	! Total up this invoice
	!
	IF AR_OPEN::CUSNUM <> AR_35CUSTOM::CUSNUM
	THEN
		GOSUB MungInvoice
		GOTO 17150
	END IF

	IF THIS_INVNUM$ <> AR_OPEN::INVNUM
	THEN
		GOSUB MungInvoice
		THIS_INVNUM$ = AR_OPEN::INVNUM
		THIS_INVDATE$ = AR_OPEN::TRADAT
		NET = 0.0
		PAY = 0.0
	END IF

	SELECT AR_OPEN::TRATYP

	CASE "09", "10", "11"
		THIS_INVDATE$ = AR_OPEN::TRADAT
		PAY = PAY - AR_OPEN::SALAMT

	CASE "02"
		THIS_INVDATE$ = AR_OPEN::TRADAT
		NET = NET + AR_OPEN::SALAMT
		PAY = PAY + AR_OPEN::SALAMT

	CASE ELSE
		NET = NET + AR_OPEN::SALAMT

	END SELECT

	WHEN ERROR IN
		GET #AR_OPEN.CH%, REGARDLESS
	USE
		CONTINUE 17150
	END WHEN

	GOTO 17110

17150	!
	! Print out results
	!
	TEXT$ = "          "
	TEXT$ = TEXT$ + FORMAT$(DATE_EXPECT(I%), "<%>######.## ") &
		FOR I% = 1% TO DATE_COUNT

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GRAND_DATE_EXPECT(I%) = GRAND_DATE_EXPECT(I%) + DATE_EXPECT(I%) &
		FOR I% = 1% TO DATE_COUNT

17200	!*******************************************************************
	! Done with this customer
	!*******************************************************************

	GOTO GetNextRec

	%PAGE

 MungInvoice:
17900	!*******************************************************************
	! Mung abount with one invoice, and spread it around
	!*******************************************************************

	!
	! Skip if already paid for
	!
	RETURN IF PAY = NET

	!
	! Guestimate a payment amount
	!
	GUESS_AMT = FUNC_ROUND(NET * AVERAGE_PCT, 2%)

	IF (GUESS_AMT <= 0.0) OR (GUESS_AMT > NET-PAY)
	THEN
		GUESS_AMT = NET - PAY
	END IF

	!
	! Guess a payment date
	!
	GUESS_DATE$ = DATE_INVDCODE(DATE_DAYCODE(THIS_INVDATE$) + AVERAGE%)

	!
	! Load into array if it fits
	!
	FOR I% = 1% TO DATE_COUNT - 1%

		IF GUESS_DATE$ <= DATE_LIST$(I%)
		THEN
			DATE_EXPECT(I%) = DATE_EXPECT(I%) + GUESS_AMT
			GOTO 17950
		END IF

	NEXT I%

	!
	! Force finish
	!
	DATE_EXPECT(DATE_COUNT) = DATE_EXPECT(DATE_COUNT) + NET - PAY

	RETURN

17950	!
	! Load up for another pass
	!
	PAY = PAY + GUESS_AMT
	THIS_INVDATE$ = GUESS_DATE$

	GOTO 17900

	%PAGE

 ExitTotal:
	!*******************************************************************
	! Handle end of report
	!*******************************************************************
	IF GRAND_COUNT_PAYMENTS% = 0%
	THEN
		AVERAGE% = 0.0
		AVERAGE_PCT = 0.0
	ELSE
		AVERAGE% = GRAND_COUNT_DAYS / GRAND_COUNT_PAYMENTS%
		AVERAGE_PCT = GRAND_COUNT_PCTPAY / GRAND_COUNT_PAYMENTS%
	END IF

	TEXT$ = "           " + &
		"Total" + SPACE$(27%) + &
		FORMAT$(AVERAGE%, "##########      ") + &
		FORMAT$(AVERAGE_PCT * 100.0, "#####.#### ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 2%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = "          "
	TEXT$ = TEXT$ + FORMAT$(GRAND_DATE_EXPECT(I%), "#######.## ") &
		FOR I% = 1% TO DATE_COUNT

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

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
