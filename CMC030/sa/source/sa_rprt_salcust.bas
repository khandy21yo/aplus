1	%TITLE "Sales Period by Customer"
	%SBTTL "SA_RPRT_SALCUST"
	%IDENT "V3.5"

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
	! ID:SA0016
	!
	! Abstract:HELP
	!	.p
	!	The ^*Sales Period by Customer\* Report contains
	!	the following information:
	!	.b
	!	.lm +10
	!	.list 0,"*"
	!	.le
	!	Customer Number
	!	.le
	!	Customer Name
	!	.le
	!	Customer Type
	!	.le
	!	Customer Category
	!	.le
	!	Cost of Sale
	!	.le
	!	Sale Total
	!	.le
	!	Sale percent
	!	.le
	!	Gross Margin
	!	.le
	!	Gross percent
	!	.le
	!	Margin percent
	!	.le
	!	City
	!	.le
	!	State
	!	.els
	!	.lm -10
	!
	! Index:
	!	.x Report>Sales Account
	!	.x Sales Account>Report
	!
	! Compile:
	!
	!	$ BAS SA_SOURCE:SA_RPRT_SALCUST/LINE
	!	$ LINK/EXE=SA_EXE: SA_RPRT_SALCUST, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE SA_RPRT_SALCUST.OBJ;*
	!
	! Author:
	!
	!	08/03/90 - Lance Williams
	!
	! Modification History:
	!
	!	01/17/91 - Val James Allen
	!		Modified so that it will work
	!
	!	04/28/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/18/95 - Kevin Handy
	!		Clean up (Check)
	!		Reformat source closer to 80 columns.
	!
	!	08/14/96 - Kevin Handy
	!		Reformat source code.
	!
	!	06/03/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/08/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include scope.com
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.HB"
	MAP (SB_ACCOUNT)	SB_ACCOUNT_CDD		SB_ACCOUNT

	%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.HB"
	MAP (SB_CONTROL)	SB_CONTROL_CDD		SB_CONTROL

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP)	GL_YYYY_PP_CDD		GL_YYYY_PP

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	!
	! Declare external functions
	!
	EXTERNAL LONG    FUNCTION SA_READ_SALCUST
	EXTERNAL LONG    FUNCTION SB_READ_ACCOUNT

	%PAGE

	TEMPCAT$ = "????"
	TEMPTYPE$ = "??"

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^*(01) Sort by (A,C,N,T)\*
	!	.p
	!	The ^*Sort by\* field determines the order
	!	to print in.
	!	.p
	!	Valid settings are:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	A - Alpha Sort
	!	.le
	!	C - Customer Category
	!	.le
	!	N - Customer Number
	!	.le
	!	T - Order Type
	!	.els
	!	.lm -10
	!	.p
	!	A setting is required in this field.  No other settings are
	!	valid.
	!
	! Index:
	!	.x Sort by
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.p
	!	The ^*From Item\* field enters the
	!	item with which to begin with.
	!	.p
	!	A blank field causes the report to begin with the first
	!	item in the file.
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.p
	!	The ^*To Item\* field specifies the item
	!	with which to end with.
	!	.p
	!	A blank field causes the report to end with the last
	!	item in the file.
	!
	! Index:
	!
	!--


	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.p
	!	The ^*Wildcard\* field selects
	!	designated items to be printed by entering a "wildcard"
	!	using the Wildcarding Technique.
	!
	! Index:
	!
	!--


300	!
	! Open the control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.OPN"
		GET #SB_CONTROL.CH%, KEY #0% EQ "SA", REGARDLESS
		CLOSE #SB_CONTROL.CH%
	USE
		FILENAME$ = "SB_CONTROL"
		CONTINUE HelpError
	END WHEN

	YYYY_PP$ = LEFT$(SB_CONTROL::PERIOD, 4%) + "_" + &
		RIGHT$(SB_CONTROL::PERIOD, 5%)

310	!
	! Open Sales Account file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.OPN"
	USE
		FILENAME$ = "SB_ACCOUNT"
		CONTINUE HelpError
	END WHEN

320	!
	! Open AR_35CUSTOM file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

330	!
	! Open General Ledger account file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"
	USE
		FILENAME$ = "GL_YYYY_PP"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(2%) = "Sales Analysis System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!	'     1234567890123456789012345678901234567890
	TITLE$(4%) = "Customer#   CustomerName                        " + &
		"CT  CusCat       CostofSale     SaleTotal Sale%  " + &
		"GrossMargin   Gross%  Margin%"
	TITLE$(5%) = "               City, St"
	TITLE$(6%) = "."
	%PAGE

	!
	! Select which method to sort by
	!
	SELECT SORTBY$

	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = " SALES ANALYSIS BY CUSTOMER TYPE"
	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = " SALES ANALYSIS BY CUSTOMER CATEGORY"
	CASE "N"
		K_NUM% = 0%
		TITLE$(1%) = " SALES ANALYSIS BY CUSTOMER NUMBER"
	CASE "A"
		K_NUM% = 3%
		TITLE$(1%) = " SALES ANALYSIS BY ALPHA SORT"
	END SELECT

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************
	!
	! If from ACCOUNT blank then reset ACCOUNT file
	! else try to find the first record
	!
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

	LOOP% = LOOP% + 1%

 GetNextRec:
17020	!
	! Main loop
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get record from customer file
	!
	WHEN ERROR IN
		GET #AR_35CUSTOM.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record if should be printed
	!
	SELECT SORTBY$

	CASE "C"
		GOTO ExitTotal IF (AR_35CUSTOM::CATEGORY > TO_ITEM$) AND &
			TO_ITEM$ <> ""
		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(AR_35CUSTOM::CATEGORY, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "A"
		GOTO ExitTotal IF (AR_35CUSTOM::ALPSRT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(AR_35CUSTOM::ALPSRT, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitTotal IF (AR_35CUSTOM::TTYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""
		GOTO GetNextRec IF COMP_STRING(EDIT$(AR_35CUSTOM::TTYPE, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "N"
		GOTO ExitTotal IF (AR_35CUSTOM::CUSNUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""
		GOTO GetNextRec IF COMP_STRING(EDIT$(AR_35CUSTOM::CUSNUM, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

	COSTTOTAL = 0.0
	SALETOTAL = 0.0

17050	WHEN ERROR IN
		FIND #GL_YYYY_PP.CH%, KEY #2% EQ AR_35CUSTOM::CUSNUM, REGARDLESS
	USE
		CONTINUE 17020 IF ERR = 155%
		FILENAME$ = "GL_YYYY_PP"
		CONTINUE HelpError
	END WHEN

17060	!
	! Get the General Ledger file
	!
	WHEN ERROR IN
		GET #GL_YYYY_PP.CH%, REGARDLESS
	USE
		CONTINUE 17100 IF ERR = 11%
		FILENAME$ = "GL_YYYY_PP"
		CONTINUE HelpError
	END WHEN

	GOTO 17100 IF GL_YYYY_PP::XREFNO <> AR_35CUSTOM::CUSNUM

	V% = SB_READ_ACCOUNT("SA", GL_YYYY_PP::ACCT, FLAG$)

	SELECT FLAG$
	CASE "SALE"
		SALETOTAL = SALETOTAL + GL_YYYY_PP::AMOUNT

	CASE "COS"
		COSTTOTAL = COSTTOTAL + GL_YYYY_PP::AMOUNT

	END SELECT

	GOTO 17060

17100	COSTTOTAL = FUNC_ROUND(COSTTOTAL, 2%)
	SALETOTAL = -FUNC_ROUND(SALETOTAL, 2%)

	V% = SA_READ_SALCUST(AR_35CUSTOM::CUSNUM, "S", &
		SB_CONTROL::PERIOD, BBPER, BBYEAR)
	V% = SA_READ_SALCUST(AR_35CUSTOM::CUSNUM, "O", &
		SB_CONTROL::PERIOD, CBBPER, CBBYEAR)

	IF LOOP% = 1%
	THEN
		PCOST = PCOST + COSTTOTAL
		PSALE = PSALE + SALETOTAL
		YCOST = YCOST + COSTTOTAL + (CBBPER - CBBYEAR)
		YSALE = YSALE + SALETOTAL + (BBPER - BBYEAR)
		GOTO GetNextRec
	END IF


	IF (SALETOTAL = 0.0) AND (COSTTOTAL = 0.0) AND &
		((BBPER - BBYEAR) = 0.0) AND ((CBBPER - CBBYEAR) = 0.0)
	THEN
		GOTO GetNextRec
	END IF

		SELECT SORTBY$

		CASE "C"
			GOSUB SubTotals &
				IF (AR_35CUSTOM::CATEGORY <> TEMPCAT$) AND &
				(FIRSTLINE = -1%)
			TEMPCAT$ = AR_35CUSTOM::CATEGORY

		CASE "T"
			GOSUB SubTotals &
				IF (AR_35CUSTOM::TTYPE <> TEMPTYPE$) AND &
				(FIRSTLINE = -1%)
			TEMPTYPE$ = AR_35CUSTOM::TTYPE
		END SELECT

		GROSS = SALETOTAL - COSTTOTAL

		IF SALETOTAL = 0.0
		THEN
			MARGINS = 0.0
		ELSE
			MARGINS = 100 * (GROSS / SALETOTAL)
		END IF
		!
		! Print out one line
		!
		FIRSTP = 0.0
		SECOUNDP = 0.0
		FIRSTP = 100 * (SALETOTAL / PSALE) IF PSALE <> 0.0
		SECOUNDP = 100 * (GROSS / PGROSS) IF PGROSS <> 0.0

		TEXT$ = AR_35CUSTOM::CUSNUM + "  " + &
			LEFT$(AR_35CUSTOM::CUSNAM, 35%) + " " + &
			AR_35CUSTOM::TTYPE + "  " + &
			AR_35CUSTOM::CATEGORY + "   " + &
			"PTD:" + &
			FORMAT$(COSTTOTAL, "#,###,###.##") + "  " + &
			FORMAT$(SALETOTAL, "#,###,###.##") + " " + &
			FORMAT$(FIRSTP, "###.#") + " " + &
			FORMAT$(GROSS, "#,###,###.##") + "    " + &
			FORMAT$(SECOUNDP, "###.#") + "    " + &
			FORMAT$(MARGINS, "###.#")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		LENGTHS% = LEN(TRM$(AR_35CUSTOM::CITY))
		CITY$ = LEFT$(AR_35CUSTOM::CITY, LENGTHS%) + "," + &
			AR_35CUSTOM::STATE + SPACE$(16% - LENGTHS% - 3%)

		GROSSY = (SALETOTAL + (BBPER - BBYEAR)) - &
			(COSTTOTAL + (CBBPER - CBBYEAR))

		!
		! Check for margins
		!
		IF SALETOTAL + (BBPER - BBYEAR) = 0.0
		THEN
			MARGINS1 = 0.0
		ELSE
			MARGINS1 = 100 * (GROSSY / (SALETOTAL + BBPER - BBYEAR))
		END IF

		!
		! Print out one line
		!
		FIRSTP = 0.0
		SECOUNDP = 0.0
		FIRSTP = 100 * ((SALETOTAL + (BBPER - BBYEAR)) / YSALE) &
			IF YSALE <> 0.0
		SECOUNDP = 100 * (GROSSY / YGROSS) IF YGROSS <> 0.0

		TEXT$ = AR_35CUSTOM::CUSNUM + "     " + &
			CITY$ + "                            " + &
			"YTD:" + &
			FORMAT$(COSTTOTAL + (CBBPER-CBBYEAR), "#,###,###.##") + "  " + &
			FORMAT$(SALETOTAL + (BBPER-BBYEAR), "#,###,###.##") + " " + &
			FORMAT$(FIRSTP, "###.#") + " " + &
			FORMAT$(GROSSY, "#,###,###.##") + "    " + &
			FORMAT$(SECOUNDP, "###.#") + "    " + &
			FORMAT$(MARGINS1, "###.#")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		FIRSTLINE = -1%

		SUBTOTAL1 = SUBTOTAL1 + COSTTOTAL
		SUBTOTAL2 = SUBTOTAL2 + SALETOTAL

		YTDSUB1 = YTDSUB1 + COSTTOTAL + (CBBPER - CBBYEAR)
		YTDSUB2 = YTDSUB2 + SALETOTAL + (BBPER - BBYEAR)

		SUBCUST% = SUBCUST% + 1%
		CUSTTOTAL% = CUSTTOTAL% + 1%
	!
	! Try for next record
	!
	GOTO GetNextRec

 SubTotals:
	!
	! Print out the subtotals
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	MARGIN1 = SUBTOTAL2 - SUBTOTAL1

	IF SUBTOTAL2 = 0.0
	THEN
		MARGIN2 = 0.0
	ELSE
		MARGIN2 = 100 * (MARGIN1 / SUBTOTAL2)
	END IF

	FIRSTP = 0.0
	SECOUNDP = 0.0
	FIRSTP = 100 * (SUBTOTAL2 / PSALE) IF PSALE <> 0.0
	SECOUNDP = 100 * (MARGIN1 / PGROSS) IF PGROSS <> 0.0

	IF SORTBY$ = "T"
	THEN
		TEXT$ = "            SubTotals for " + &
			FORMAT$(SUBCUST%, "###") + " customers  Type: " + &
			TEMPTYPE$ + "          PTD:" + &
			FORMAT$(SUBTOTAL1, "#,###,###.##") + "  " + &
			FORMAT$(SUBTOTAL2, "#,###,###.##") + " " + &
			FORMAT$(FIRSTP, "###.#") + " " + &
			FORMAT$(MARGIN1, "#,###,###.##") + "    " + &
			FORMAT$(SECOUNDP, "###.#") + "    " + &
			FORMAT$(MARGIN2, "###.#")
	ELSE
		TEXT$ = "            SubTotals for " + &
			FORMAT$(SUBCUST%, "###") + " customers  Category: " + &
			TEMPCAT$ + "    PTD:" + &
			FORMAT$(SUBTOTAL1, "#,###,###.##") + "  " + &
			FORMAT$(SUBTOTAL2, "#,###,###.##") + " " + &
			FORMAT$(FIRSTP, "###.#") + " " + &
			FORMAT$(MARGIN1, "#,###,###.##") + "    " + &
			FORMAT$(SECOUNDP, "###.#") + "    " + &
			FORMAT$(MARGIN2, "###.#")
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	MARGIN1 = YTDSUB2 - YTDSUB1

	IF YTDSUB2 = 0.0
	THEN
		MARGIN2 = 0.0
	ELSE
		MARGIN2 = 100 * (MARGIN1 / YTDSUB2)
	END IF

	FIRSTP = 0.0
	SECOUNDP = 0.0
	FIRSTP = 100 * (YTDSUB2 / YSALE) IF YSALE <> 0.0
	SECOUNDP = 100 * (MARGIN1 / YGROSS) IF YGROSS <> 0.0

	IF SORTBY$ = "T"
	THEN
		TEXT$ = "            SubTotals for " + &
			FORMAT$(SUBCUST%, "###") + " customers  Type: " + &
			TEMPTYPE$ + "          YTD:" + &
			FORMAT$(YTDSUB1, "#,###,###.##") + "  " + &
			FORMAT$(YTDSUB2, "#,###,###.##") + " " + &
			FORMAT$(FIRSTP, "###.#") + " " + &
			FORMAT$(MARGIN1, "#,###,###.##") + "    " + &
			FORMAT$(SECOUNDP, "###.#") + "    " + &
			FORMAT$(MARGIN2, "###.#")
	ELSE
		TEXT$ = "            SubTotals for " + &
			FORMAT$(SUBCUST%, "###") + " customers  Category: " + &
			TEMPCAT$ + "    YTD:" + &
			FORMAT$(YTDSUB1, "#,###,###.##") + "  " + &
			FORMAT$(YTDSUB2, "#,###,###.##") + " " + &
			FORMAT$(FIRSTP, "###.#") + " " + &
			FORMAT$(MARGIN1, "#,###,###.##") + "    " + &
			FORMAT$(SECOUNDP, "###.#") + "    " + &
			FORMAT$(MARGIN2, "###.#")
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Reset the variables
	!
	SUBCUST% = 0
	SUBTOTAL1 = 0.0
	SUBTOTAL2 = 0.0

	YTDSUB1 = 0.0
	YTDSUB2 = 0.0

	RETURN

 ExitTotal:
17400	!
	! Handle end of the report
	!
	IF LOOP% = 1%
	THEN
		PGROSS = PSALE - PCOST
		YGROSS = YSALE - YCOST
		GOTO 17000
	END IF

	IF FIRSTLINE = -1%
	THEN

		GOSUB SubTotals IF (SORTBY$ = "C") OR (SORTBY$ = "T")
		!
		! Print out one line
		!
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		IF PSALE = 0.0
		THEN
			TOTMARGIN2 = 0.0
		ELSE
			TOTMARGIN2 = 100 * (PGROSS / PSALE)
		END IF

		TEXT$ = "            Totals for " + &
			FORMAT$(CUSTTOTAL%, "###") + &
			" customers                       PTD:" + &
			FORMAT$(PCOST, "#,###,###.##") + "  " + &
			FORMAT$(PSALE, "#,###,###.##") + " " + &
			FORMAT$(100.0, "###.#") + " " + &
			FORMAT$(PGROSS, "#,###,###.##") + "    " + &
			FORMAT$(100.0, "###.#") + "    " + &
			FORMAT$(TOTMARGIN2, "###.#")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		!
		! Print out one line
		!
		IF YSALE = 0.0
		THEN
			YTDMARGIN2 = 0.0
		ELSE
			YTDMARGIN2 = 100 * (YGROSS / YSALE)
		END IF

		TEXT$ = "            Totals for " + &
			FORMAT$(CUSTTOTAL%, "###") + &
			" customers                       YTD:" + &
			FORMAT$(YCOST, "#,###,###.##") + "  " + &
			FORMAT$(YSALE, "#,###,###.##") + " " + &
			FORMAT$(100.0, "###.#") + " " + &
			FORMAT$(YGROSS, "#,###,###.##") + "    " + &
			FORMAT$(100.0, "###.#") + "    " + &
			FORMAT$(YTDMARGIN2, "###.#")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

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

	!
	! Exit from the program after showing error message
	!
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
