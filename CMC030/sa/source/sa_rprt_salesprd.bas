1	%TITLE "Sales Account Product "
	%SBTTL "SA_RPRT_SALESPRD"
	%IDENT "V3.5"

	!
	! COPYRIGHT (C) 1991 BY
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
	! ID:SA0020
	!
	! Abstract:HELP
	!	.p
	!	The ^*Sales Account Product\* Report contains
	!	the following information:
	!	.b
	!	.lm +10
	!	.list 0,"*"
	!	.le
	!	Product Number
	!	.le
	!	Product Description
	!	.le
	!	Product Type
	!	.le
	!	Product Category
	!	.le
	!	(Period-to-date and Year-to-date)
	!	.le
	!	Quantity Sold
	!	.le
	!	Cost of Sale
	!	.le
	!	Sales Total
	!	.le
	!	Percent of all product sales
	!	.le
	!	Gross Margin (dollars)
	!	.le
	!	Percent of all Gross Margin dollars
	!	.le
	!	Margin Percentage
	!	.els
	!	.lm -10
	!
	! Index:
	!	.x Report>Product Sales
	!	.x Product Sales>Report
	!
	! Compile:
	!
	!	$ BAS SA_SOURCE:SA_RPRT_SALESPRD/LINE
	!	$ LINK/EXE=SA_EXE: SA_RPRT_SALESPRD, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE SA_RPRT_SALESPRD.OBJ;*
	!
	! Author:
	!
	!	10/05/91 - Dan Perkins
	!
	! Modification History:
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
	!		Update to V3.6 coding standards
	!
	!	09/13/96 - Kevin Handy
	!		Reformat source code
	!
	!	06/03/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/15/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP)	GL_YYYY_PP_CDD		GL_YYYY_PP

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	!
	! Declare external functions
	!
	EXTERNAL LONG	FUNCTION IC_READ_HISTORY

	%PAGE

	TEMPCAT$ = "????"
	TEMPTYPE$ = "??"

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	YYYYPP$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	YYYY_PP$ = LEFT$(YYYYPP$, 4%) + "_" + RIGHT$(YYYYPP$, 5%)

	!++
	! Abstract:FLD01
	!	^*(01) Period\*
	!	.p
	!	The ^*Period\* field enters the
	!	period to print.
	!
	! Index:
	!	.x Period
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(1%), -1%)

	!++
	! Abstract:FLD02
	!	^*(02) Sort by (P,C,T,D,S)\*
	!	.p
	!	The ^*Sort by\* field determines the order
	!	to print in.
	!	.p
	!	Valid settings are:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	P - Product Number
	!	.le
	!	C - Product Category
	!	.le
	!	T - Product Type
	!	.le
	!	D - Product Description
	!	.le
	!	S - Secondary Code
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

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) From Item\*
	!	.p
	!	The ^*From Item\* field enters the
	!	item with to begin with.
	!	.p
	!	A blank field causes the report to begin with the first
	!	item in the file.
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) To Item\*
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

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Wildcard\*
	!	.p
	!	The ^*Wildcard\* field selects
	!	designated programs to be printed by entering a "wildcard"
	!	for Wildcarding Technique.
	!
	! Index:
	!
	!--

	!
	! Open Product Description file
	!
300	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

320	!
	! Open Inventory Transaction file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"
	USE
		FILENAME$ = "GL_YYYY_PP"
		CONTINUE HelpError
	END WHEN

400	!
	! Create temporary file now
	!
	CALL ASSG_CHANNEL(SA_TEMP.CH%, STAT%)

	WHEN ERROR IN
		OPEN "SA_TEMP.TMP" FOR OUTPUT AS FILE SA_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP GL_YYYY_PP, &
			PRIMARY KEY (GL_YYYY_PP::DESCR) DUPLICATES, &
			TEMPORARY, &
			BUFFER 32%, &
			ACCESS MODIFY, ALLOW NONE
	USE
		FILENAME$ = "SA_TEMP"
		CONTINUE HelpError
	END WHEN

	!
	! Set up temp file
	!
	CALL ENTR_3MESSAGE(SCOPE, "Sorting GL Ledger File", 1% + 16%)

	WHEN ERROR IN
		RESET #GL_YYYY_PP.CH%, KEY #0%
	USE
		FILENAME$ = "SA_TEMP"
		CONTINUE HelpError
	END WHEN

 ReadGLPeriod:
500	WHEN ERROR IN
		GET #GL_YYYY_PP.CH%, REGARDLESS
	USE
		CONTINUE ReportTitle IF ERR = 11%
		FILENAME$ = GL_YYYY_PP$
		CONTINUE HelpError
	END WHEN

510	WHEN ERROR IN
		PUT #SA_TEMP.CH%
	USE
		FILENAME$ = "SA_TEMP"
		CONTINUE HelpError
	END WHEN

	GOTO ReadGLPeriod

 ReportTitle:
	!
	! Title
	!
	! Select which method to sort by
	!
	SELECT SORTBY$
	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = " SALES ANALYSIS BY PRODUCT TYPE"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = " SALES ANALYSIS BY PRODUCT CATEGORY"

	CASE "P"
		K_NUM% = 0%
		TITLE$(1%) = " SALES ANALYSIS BY PRODUCT NUMBER"

	CASE "D"
		K_NUM% = 3%
		TITLE$(1%) = " SALES ANALYSIS BY PRODUCT DESCRIPTION"

	CASE "S"
		K_NUM% = 4%
		TITLE$(1%) = " SALES ANALYSIS BY SECONDARY PRODUCT CODE"

	END SELECT

	TITLE$(2%) = "Sales Analysis System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!	'     1234567890123456789012345678901234567890
	TITLE$(4%) = "ProductNumber  Description              " + &
		"            Type Categ        QtySold Co" + &
		"stofSale  SaleTotal Sale% GrossMargn Gross% Margin%"

	TITLE$(5%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************
	!
	! If from ITEM blank then reset file
	! else try to find the first record
	!
 FindNextRec:
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PD_PRODUCT.CH%, KEY #K_NUM%
		ELSE
			FIND #PD_PRODUCT.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	GOTO ExitProgram IF UTL_REPORTX::STAT

	LOOP% = LOOP% + 1%

	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record if should be printed
	!
	SELECT SORTBY$
	CASE "C"
		GOTO ExitTotal IF (PD_PRODUCT::CATEGORY > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$ &
			(PD_PRODUCT::CATEGORY, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitTotal IF (PD_PRODUCT::PROD_TYPE> TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$ &
			(PD_PRODUCT::PROD_TYPE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "P"
		GOTO ExitTotal IF (PD_PRODUCT::PRODUCT_NUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$ &
			(PD_PRODUCT::PRODUCT_NUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "D"
		GOTO ExitTotal IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$ &
			(PD_PRODUCT::DESCRIPTION, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "S"
		GOTO ExitTotal IF (PD_PRODUCT::SECONDARY_CODE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$ &
			(PD_PRODUCT::SECONDARY_CODE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

 FindTempRec:
17200	WHEN ERROR IN
		FIND #SA_TEMP.CH%, &
			KEY #0% EQ PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155%
		FILENAME$ = "SA_TEMP"
		CONTINUE HelpError
	END WHEN

 GetTempRec:
17210	WHEN ERROR IN
		GET #SA_TEMP.CH%, REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 11%
		FILENAME$ = "SA_TEMP"
		CONTINUE HelpError
	END WHEN

	GOTO GetTempRec &
		IF LEFT$(GL_YYYY_PP::DESCR, LEN(PD_PRODUCT::PRODUCT_NUM)) &
		<> PD_PRODUCT::PRODUCT_NUM

	COSTTOTAL = FUNC_ROUND(COSTTOTAL, 2%)
	SALETOTAL = FUNC_ROUND(SALETOTAL, 2%)
	QTYTOTAL  = FUNC_ROUND(QTYTOTAL, 2%)

	!
	! Lookup sales history here by using Read_History routine
	!
	V% = IC_READ_HISTORY(PD_PRODUCT::PRODUCT_NUM, "", YYYYPP$, &
		TOTHQ, TOTHS, TOTHC)

	TOTHQ = -TOTHQ
	TOTHS = -TOTHS
	TOTHC = -TOTHC

	! Check if loop 1 and if so just add up for overall totals
	!
	IF LOOP% = 1%
	THEN
		PCOST = PCOST + COSTTOTAL
		PSALE = PSALE + SALETOTAL
		YCOST = YCOST + (COSTTOTAL + TOTHC)
		YSALE = YSALE + (SALETOTAL + TOTHS)
		GOTO GetNextRec
	END IF

	!
	! See if there are totals to print
	!
	GOTO GetNextRec IF (SALETOTAL = 0.0) AND (COSTTOTAL = 0.0) AND &
		(QTYTOTAL = 0.0) AND (TOTHQ = 0.0) AND &
		(TOTHS = 0.0) AND (TOTHC = 0.0)

	SELECT SORTBY$
	CASE "C"
		GOSUB SubTotals &
			IF (PD_PRODUCT::CATEGORY <> TEMPCAT$) AND &
			(FIRSTLINE = -1%)
		TEMPCAT$ = PD_PRODUCT::CATEGORY

	CASE "T"
		GOSUB SubTotals &
			IF (PD_PRODUCT::PROD_TYPE <> TEMPTYPE$) AND &
			(FIRSTLINE = -1%)
		TEMPTYPE$ = PD_PRODUCT::PROD_TYPE
	END SELECT

	GROSS = SALETOTAL - COSTTOTAL

	IF SALETOTAL = 0.0
	THEN
		MARGINS = 0.0
	ELSE
		MARGINS = 100 * (GROSS / SALETOTAL)
	END IF

 PrintLine:
	!
	! Print out one line
	!
	FIRSTP = 0.0
	SECOUNDP = 0.0
	FIRSTP = 100 * (SALETOTAL / PSALE) IF PSALE <> 0.0
	SECOUNDP = 100 * (GROSS / PGROSS) IF PGROSS <> 0.0

	TEXT$ = PD_PRODUCT::PRODUCT_NUM + " " + &
		LEFT$(PD_PRODUCT::DESCRIPTION, 36%) + " " + &
		PD_PRODUCT::PROD_TYPE + "   " + &
		PD_PRODUCT::CATEGORY + "  " + &
		"PTD:" + &
		FORMAT$(QTYTOTAL, "#######.##") + " " + &
		FORMAT$(COSTTOTAL, "#######.##") + " " + &
		FORMAT$(SALETOTAL, "#######.##") + " " + &
		FORMAT$(FIRSTP, "###.#") + " " + &
		FORMAT$(GROSS, "#######.##") + "  " + &
		FORMAT$(SECOUNDP, "###.#") + "   " + &
		FORMAT$(MARGINS, "###.#")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GROSSY = (SALETOTAL + TOTHS) - (COSTTOTAL + TOTHC)

	!
	! Check for margins
	!
	IF SALETOTAL + TOTHS = 0.0
	THEN
		MARGINS1 = 0.0
	ELSE
		MARGINS1 = 100 * (GROSSY / (SALETOTAL + TOTHS))
	END IF

	!
	! Print out one line
	!
	FIRSTP = 0.0
	SECOUNDP = 0.0
	FIRSTP = 100 * ((SALETOTAL + TOTHS) / YSALE) IF YSALE <> 0.0
	SECOUNDP = 100 * (GROSSY / YGROSS) IF YGROSS <> 0.0

	TEXT$ = PD_PRODUCT::PRODUCT_NUM + SPACE$(49%) + &
		"YTD:" + &
		FORMAT$(QTYTOTAL + TOTHQ, "#######.##") + " " + &
		FORMAT$(COSTTOTAL + TOTHC, "#######.##") + " " + &
		FORMAT$(SALETOTAL + TOTHS, "#######.##") + " " + &
		FORMAT$(FIRSTP, "###.#") + " "  + &
		FORMAT$(GROSSY, "#######.##") + "  " + &
		FORMAT$(SECOUNDP, "###.#") + "   " + &
		FORMAT$(MARGINS1, "###.#")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FIRSTLINE = -1%

	SUBTOTAL1 = SUBTOTAL1 + COSTTOTAL
	SUBTOTAL2 = SUBTOTAL2 + SALETOTAL
	SUBTOTAL3 = SUBTOTAL3 + QTYTOTAL

	YTDSUB1 = YTDSUB1 + COSTTOTAL + TOTHC
	YTDSUB2 = YTDSUB2 + SALETOTAL + TOTHS
	YTDSUB3 = YTDSUB3 + QTYTOTAL  + TOTHQ

	SUBCUST% = SUBCUST% + 1
	CUSTTOTAL% = CUSTTOTAL% + 1

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
		TEXT$ = "            SubTotals for" + &
			FORMAT$(SUBCUST%, "###") + &
			" products   Type: " + TEMPTYPE$	+ &
			"              PTD:" + &
			FORMAT$(SUBTOTAL3, "#######.##") + " " + &
			FORMAT$(SUBTOTAL1, "#######.##") + " " + &
			FORMAT$(SUBTOTAL2, "#######.##") + " " + &
			FORMAT$(FIRSTP, "###.#") + " " + &
			FORMAT$(MARGIN1, "#######.##") + "  " + &
			FORMAT$(SECOUNDP, "###.#") + "   " + &
			FORMAT$(MARGIN2, "###.#")
	ELSE

		TEXT$ = "            SubTotals for " + &
			FORMAT$(SUBCUST%, "###") + &
			" products   Category: " + TEMPCAT$ + &
			"        PTD:" + &
			FORMAT$(SUBTOTAL3, "#######.##") + " " + &
			FORMAT$(SUBTOTAL1, "#######.##") + " " + &
			FORMAT$(SUBTOTAL2, "#######.##") + " " + &
			FORMAT$(FIRSTP, "###.#") + " "   + &
			FORMAT$(MARGIN1, "#######.##") + "  " + &
			FORMAT$(SECOUNDP, "###.#") + "   " + &
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
			FORMAT$(SUBCUST%, "###") + &
			" products   Type: " + TEMPTYPE$ + &
			"              YTD:" + &
			FORMAT$(YTDSUB3, "#######.##") + " " + &
			FORMAT$(YTDSUB1, "#######.##") + " " + &
			FORMAT$(YTDSUB2, "#######.##") + " " + &
			FORMAT$(FIRSTP, "###.#") + " "   + &
			FORMAT$(MARGIN1, "#######.##") + "  " + &
			FORMAT$(SECOUNDP, "###.#") + "   " + &
			FORMAT$(MARGIN2, "###.#")
	ELSE
		TEXT$ = "            SubTotals for " + &
			FORMAT$(SUBCUST%, "###") + &
			" products   Category: " + TEMPCAT$ + &
			"        YTD:" + &
			FORMAT$(YTDSUB3, "#######.##") + " " + &
			FORMAT$(YTDSUB1, "#######.##") + " " + &
			FORMAT$(YTDSUB2, "#######.##") + " " + &
			FORMAT$(FIRSTP, "###.#") + " "   + &
			FORMAT$(MARGIN1, "#######.##") + "  " + &
			FORMAT$(SECOUNDP, "###.#") + "   " + &
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
	SUBTOTAL3 = 0.0

	YTDSUB1 = 0.0
	YTDSUB2 = 0.0
	YTDSUB3 = 0.0

	RETURN

 ExitTotal:
	IF LOOP% = 1%
	THEN
		PGROSS = PSALE - PCOST
		YGROSS = YSALE - YCOST
		GOTO FindNextRec
	END IF

	IF FIRSTLINE = -1%
	THEN

		GOSUB SubTotals IF (SORTBY$ = "C") OR (SORTBY$ = "T")
		!
		! Print out one line
		!
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)


		IF PSALE = 0.0
		THEN
			TOTMARGIN2 = 0.0
		ELSE
			TOTMARGIN2 = 100 * (PGROSS / PSALE)
		END IF

		TEXT$ = "            Totals for " + &
			FORMAT$(CUSTTOTAL%, "###") + &
			" products              " + &
			"              PTD:           " + &
			FORMAT$(PCOST, "#######.##") + " " + &
			FORMAT$(PSALE, "#######.##") + " " + &
			FORMAT$(100.0, "###.#") + " " + &
			FORMAT$(PGROSS, "#######.##") + "  " + &
			FORMAT$(100.0, "###.#") + "   " + &
			FORMAT$(TOTMARGIN2, "###.#")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		!
		! Print out one line
		!
		IF YSALE = 0.0
		THEN
			YTDMARGIN2 = 0.0
		ELSE
			YTDMARGIN2 = 100*(YGROSS/YSALE)
		END IF

		TEXT$ = "            Totals for " + &
			FORMAT$(CUSTTOTAL%, "###") + &
			" products              " + &
			"              YTD:           " + &
			FORMAT$(YCOST, "#######.##") + " " + &
			FORMAT$(YSALE, "#######.##") + " " + &
			FORMAT$(100.0, "###.#") + " "   + &
			FORMAT$(YGROSS, "#######.##") + "  " + &
			FORMAT$(100.0, "###.#") + "   " + &
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
