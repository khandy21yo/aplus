1	%TITLE "Cash In and Out Journal Report"
	%SBTTL "PS_RPRT_CASHINOUT"
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
	! ID:PS014
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Cash In and Out Report\* contains
	!	the following information:
	!	.table 3,25
	!	.te
	!	Transaction Date
	!	.te
	!	Transaction Time
	!	.te
	!	Description
	!	.te
	!	Trans Amount
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Cash In and Out
	!	.x Cash In and Out>Report
	!
	! Compile:
	!
	!	$ BAS PS_SOURCE:PS_RPRT_CASHINOUT/LINE
	!	$ LINK/EXE=PS_EXE: PS_RPRT_CASHINOUT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PS_RPRT_CASHINOUT.OBJ;*
	!
	! Author:
	!
	!	02/21/92 - Frank F. Starman
	!
	! Modification History:
	!
	!	02/24/92 - Kevin Handy
	!		Cleaned out (check)
	!
	!	03/19/92 - Dan Perkins
	!		Adjusted for charge orders. No cash in or out.
	!		Aligned title headings.
	!
	!	03/22/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/28/92 - Frank F. Starman
	!		Added ticket number.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/17/93 - Dan Perkins
	!		Added OE_ORDERJOUR::HANDLING in to calculation
	!		for ORDER_TOTAL.  Modified error trapping to exit
	!		NewOrder loop if there is no CASHINOUT file.
	!
	!	06/20/93 - Frank F. Starman
	!		Work only with TIME not DATE.
	!
	!	06/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/17/94 - Kevin Handy
	!		Added code foe ::MISCH2.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	11/14/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	09/13/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	05/21/99 - Kevin Handy
	!		Modified to use record structure for temp file
	!
	!	05/21/99 - Kevin Handy
	!		Added deposit# and account to printout (and all
	!		the necessary code to get the account number for
	!		the AMTPAID item)
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include CODES
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.HB"
	MAP (OE_ORDERJOUR)	OE_ORDERJOUR_CDD	OE_ORDERJOUR

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERLINE.HB"
	MAP (OE_ORDERLINE)	OE_ORDERLINE_CDD	OE_ORDERLINE

	%INCLUDE "SOURCE:[PS.OPEN]PS_CASHINOUT.HB"
	MAP(PS_CASHINOUT)	PS_CASHINOUT_CDD	PS_CASHINOUT

	RECORD TEMP_CDD

		STRING INVNUM = 8%
		STRING OPERATOR = 10%, &
		STRING TTIME =  6%
		STRING TTYPE = 2%
		STRING CAT = 4%
		STRING DDESC = 42%
		STRING CHECK = 8%
		STRING TICNUM = 10%
		STRING DEPOSIT = 6%
		STRING ACCOUNT = 18%
		GFLOAT AMTIN
		GFLOAT AMTOUT

	END RECORD

	MAP (TEMP_DATA) TEMP_CDD TEMP_DATA

	%INCLUDE "SOURCE:[OE.OPEN]OE_ACCOUNT.HB"
	DECLARE OE_ACCOUNT_CDD	OE_ACCOUNT_READ

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	AR_EXAM_CUSTOM
	EXTERNAL LONG	FUNCTION	OE_READ_ACCOUNT

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^*(01) Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field selects
	!	designated programs to be printed by entering a batch number
	!	for identification.
	!	.b
	!	Only one batch at a time may be printed.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number
	!
	!--

	REG_NO$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Cash Register Number\*
	!	.b
	!	.lm +5
	!	The ^*Cash Register Number\* field enters the
	!	cash register number.
	!	.lm -5
	!
	! Index:
	!	.x Cash Register Number
	!
	!--

	BATCH_NO$ = REG_NO$ + "_" + BATCH_NO$

300	!
	! Open ticket Journal file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.OPN"
	USE
		FILENAME$ = "OE_ORDERJOUR_" + BATCH_NO$
		CONTINUE HelpError
	END WHEN

320	!
	! Open ticket Line file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERLINE.OPN"
	USE
		CONTINUE 330 IF ERR = 5%
		FILENAME$ = "OE_ORDERLINE_" + BATCH_NO$
		CONTINUE HelpError
	END WHEN

330	!
	! Open Cash file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PS.OPEN]PS_CASHINOUT.OPN"
	USE
		CONTINUE 340 IF ERR = 5%
		FILENAME$ = "PS_CASHINOUT_" + BATCH_NO$
		CONTINUE HelpError
	END WHEN

340	CALL ASSG_CHANNEL(TEMP.CH%, STAT%)

	CALL ENTR_3MESSAGE(SCOPE, "Creating temporary file", 1% + 16%)

	WHEN ERROR IN
		OPEN "TEMP.TMP" FOR OUTPUT AS FILE TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP TEMP_DATA, &
			PRIMARY KEY (TEMP_DATA::TTIME) DUPLICATES, &
			ALTERNATE KEY (TEMP_DATA::TTYPE) DUPLICATES, &
			TEMPORARY, &
			BUFFER 32%, &
			ACCESS MODIFY, ALLOW NONE
	USE
		FILENAME$ = "TEMP"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "CASH REGISTER " + BATCH_NO$ + " TRANSACTIONS"
	TITLE$(2%) = "POS System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Operator   Time     Tp Inv#     Doc#       "  + &
		"Check#       AmtIn    AmtOut   Balance Cat  " + &
		"Depst  Account      Description"

	TITLE$(5%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	WHEN ERROR IN
		RESET #OE_ORDERJOUR.CH%
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "OE_ORDERJOUR_" + BATCH_NO$
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitTotal IF UTL_REPORTX::STAT

	!
	! Get next ticket record
	!
	WHEN ERROR IN
		GET #OE_ORDERJOUR.CH%, REGARDLESS
	USE
		CONTINUE ExitOrder IF ERR = 11%
		FILENAME$ = "OE_ORDERJOUR_" + BATCH_NO$
		CONTINUE HelpError
	END WHEN

	LTOTAL = 0.0

17100	WHEN ERROR IN
		FIND #OE_ORDERLINE.CH%, KEY #0% EQ OE_ORDERJOUR::ORDNUM, REGARDLESS
	USE
		CONTINUE NewOrder IF ERR = 155% OR ERR = 9%
		FILENAME$ = "OE_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	!
	! Get OE_ORDERLINE record
	!
 Invline:
17120	WHEN ERROR IN
		GET #OE_ORDERLINE.CH%, REGARDLESS
	USE
		CONTINUE NewOrder IF ERR = 11%
		FILENAME$ = "OE_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	GOTO NewOrder IF OE_ORDERLINE::ORDNUM <> OE_ORDERJOUR::ORDNUM

	EXTPRICE = FUNC_ROUND(OE_ORDERLINE::SHPQTY * OE_ORDERLINE::PRICE, 2%)

	PROMO = FUNC_ROUND(OE_ORDERLINE::SHPQTY * OE_ORDERLINE::PROMO, 2%)

	DISCOUNT = FUNC_ROUND(OE_ORDERLINE::SHPQTY * (OE_ORDERLINE::PRICE - &
		OE_ORDERLINE::PROMO) * OE_ORDERLINE::DISCOUNT/100, 2%)

	MISCH = FUNC_ROUND(OE_ORDERLINE::SHPQTY * OE_ORDERLINE::MISCH, 2%)
	MISCH2 = FUNC_ROUND(OE_ORDERLINE::SHPQTY * OE_ORDERLINE::MISCH2, 2%)

	LTOTAL = LTOTAL + EXTPRICE - DISCOUNT - PROMO + MISCH + MISCH2

	GOTO Invline

 NewOrder:
	IF OE_ORDERJOUR::DISC <> 0.0
	THEN
		ORDER_DISC = FUNC_ROUND(LTOTAL * OE_ORDERJOUR::DISC / 100.0, 2%)
	ELSE
		ORDER_DISC = 0.0
	END IF

	IF OE_ORDERJOUR::SALESTAX <> 0.0
	THEN
		ORDER_TAX = FUNC_ROUND((LTOTAL - ORDER_DISC) * &
			OE_ORDERJOUR::SALESTAX / 100.0, 2%)
	ELSE
		ORDER_TAX = 0.0
	END IF

	ORDER_TOT = OE_ORDERJOUR::FREIGHT + OE_ORDERJOUR::MISC + &
		OE_ORDERJOUR::HANDLING + ORDER_TAX + LTOTAL - ORDER_DISC

	IF ORDER_TOT = OE_ORDERJOUR::AMTPAID
	THEN
		WARNING$ = "  "
	ELSE
		WARNING$ = "* "
	END IF

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
		!
		! Flag customer as undefined.
		!
		AR_35CUSTOM_EXAM::CUSNUM = "(Undefined)"
		AR_35CUSTOM_EXAM::TTYPE = ""

	!
	! Untrapped error
	!
	CASE ELSE
		GOTO ExitProgram

	END SELECT

	!
	! Read expense accounts
	!
	READ_ACCOUNT% = OE_READ_ACCOUNT(AR_35CUSTOM_EXAM::TTYPE, &
		OE_ORDERJOUR::ORDTYPE, OE_ORDERJOUR::LOCATION, OE_ACCOUNT_READ)

	TEMP_DATA::INVNUM = OE_ORDERJOUR::INVNUM
	TEMP_DATA::TICNUM = CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT)
	TEMP_DATA::TTIME = OE_ORDERJOUR::TRANTIME
	TEMP_DATA::TTYPE = OE_ORDERJOUR::ORDTYPE
	TEMP_DATA::OPERATOR = OE_ORDERJOUR::OPERATOR
	TEMP_DATA::CAT = OE_ORDERJOUR::ORDCAT
	TEMP_DATA::DDESC = WARNING$ + OE_ORDERJOUR::NOTES(0%)
	TEMP_DATA::CHECK = OE_ORDERJOUR::CHECK
	TEMP_DATA::DEPOSIT = OE_ORDERJOUR::DEPOSIT
	TEMP_DATA::ACCOUNT = OE_ACCOUNT_READ::ACCOUNT

	!
	! Assume charge, or returned merchandise.
	!
	IF OE_ORDERJOUR::AMTPAID > 0.0
	THEN
		TEMP_DATA::AMTIN   = OE_ORDERJOUR::AMTPAID
		TEMP_DATA::AMTOUT  = 0.0
	ELSE
		TEMP_DATA::AMTIN   = 0.0
		TEMP_DATA::AMTOUT  = -OE_ORDERJOUR::AMTPAID
	END IF

17200	WHEN ERROR IN
		PUT #TEMP.CH%
	USE
		FILENAME$ = "TEMP"
		CONTINUE HelpError
	END WHEN

	!
	! Try for next Order record
	!
	GOTO GetNextRec

 ExitOrder:
17300	WHEN ERROR IN
		RESET #PS_CASHINOUT.CH%
	USE
		CONTINUE ExitTotal IF ERR = 9%
		FILENAME$ = "PS_CASHINOUT_" + BATCH_NO$
		CONTINUE HelpError
	END WHEN

 CashInOut:
17320	WHEN ERROR IN
		GET #PS_CASHINOUT.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PS_CASHINOUT_" + BATCH_NO$
		CONTINUE HelpError
	END WHEN

	TEMP_DATA::OPERATOR = PS_CASHINOUT::OPERATOR
	TEMP_DATA::INVNUM = ""
	TEMP_DATA::TICNUM = ""
	TEMP_DATA::TTIME = PS_CASHINOUT::CASHTIME
	TEMP_DATA::CAT = "    "
	TEMP_DATA::TTYPE = "  "
	TEMP_DATA::DDESC = "  " + PS_CASHINOUT::NOTES
	TEMP_DATA::CHECK = ""
	TEMP_DATA::DEPOSIT = PS_CASHINOUT::DEPOSIT
	TEMP_DATA::ACCOUNT = PS_CASHINOUT::ACCOUNT

	IF PS_CASHINOUT::AMOUNT > 0.0
	THEN
		TEMP_DATA::AMTIN = PS_CASHINOUT::AMOUNT
		TEMP_DATA::AMTOUT = 0.0
	ELSE
		TEMP_DATA::AMTIN   = 0.0
		TEMP_DATA::AMTOUT  = -PS_CASHINOUT::AMOUNT
	END IF

17400	WHEN ERROR IN
		PUT #TEMP.CH%
	USE
		FILENAME$ = "TEMP"
		CONTINUE HelpError
	END WHEN

	GOTO CashInOut

 ExitTotal:
	WHEN ERROR IN
		RESET #TEMP.CH%
	USE
		FILENAME$ = "TEMP"
		CONTINUE HelpError
	END WHEN

	BALANCE = 0.0

 ReadTemp:
17500	WHEN ERROR IN
		GET #TEMP.CH%
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "TEMP"
		CONTINUE HelpError
	END WHEN

	BALANCE = BALANCE + TEMP_DATA::AMTIN - TEMP_DATA::AMTOUT

	TEXT$ = TEMP_DATA::OPERATOR + " " + &
		PRNT_TIME(TEMP_DATA::TTIME, 0%) + " " + &
		TEMP_DATA::TTYPE + " " + &
		TEMP_DATA::INVNUM + " " + &
		TEMP_DATA::TICNUM + " " + &
		TEMP_DATA::CHECK + &
		FORMAT$(TEMP_DATA::AMTIN, "<%>##,###.##") + &
		FORMAT$(TEMP_DATA::AMTOUT, "<%>##,###.##") + &
		FORMAT$(BALANCE, "###,###.## ") + &
		TEMP_DATA::CAT + " " + &
		TEMP_DATA::DEPOSIT + " " + &
		LEFT(TEMP_DATA::ACCOUNT, 10%) + " " + &
		LEFT(TEMP_DATA::DDESC, 25%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ReadTemp

	%PAGE

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
	!***************************************************************
	! Help Message for an error
	!***************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

19000	!***************************************************************
	! ERROR TRAPPING
	!***************************************************************

	!
	! Resume to display untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
