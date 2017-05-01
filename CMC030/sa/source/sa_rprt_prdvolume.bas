1	%TITLE "Sales Volume by Product"
	%SBTTL "SA_RPRT_PRDVOLUME"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1992 BY
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
	! ID:SA0019
	!
	! Abstract:HELP
	!	.p
	!	The ^*Sales Volume by Product Report\* option
	!	prints a sales report listing the following information:
	!	.b
	!	.lm +10
	!	.list 0,"*"
	!	.le
	!	Product Number
	!	.le
	!	Product Type
	!	.le
	!	Product Category
	!	.le
	!	Product Description
	!	.le
	!	Product Secondary Code
	!	.le
	!	Sales Quantity
	!	.le
	!	Sales Amount
	!	.le
	!	Sales Percentage
	!	.le
	!	Cost
	!	.le
	!	Gross Profit
	!	.le
	!	Gross Margin
	!	.els
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS SA_SOURCE:SA_RPRT_PRDVOLUME/LINE
	!	$ LINK/EXECUTABLE=SA_EXE: SA_RPRT_PRDVOLUME, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE SA_RPRT_PRDVOLUME.OBJ;*
	!
	! Author:
	!
	!	01/13/92 - Dan Perkins
	!
	! Modification history:
	!
	!	01/17/92 - Dan Perkins
	!		Changed decimal value to allow higher
	!		numbers.  Changed fields to allow larger numbers.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	03/22/94 - Kevin Handy
	!		Reformatted to 80  columns.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/13/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/12/97 - Kevin Handy
	!		Define UTL_WORK.DEV$ before use.
	!
	!	07/30/97 - Kevin Handy
	!		Change XAGE parameter of READ_PERIOD to integer
	!
	!	08/20/97 - Kevin Handy
	!		Don't allocate channel for report
	!
	!	08/26/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	01/12/98 - Kevin Handy
	!		Broke line 17500 into lines 17500-18999, so I
	!		could tell where an error was really occurring
	!
	!	01/12/97 - Kevin Handy
	!		Modify margin calculation to that it assignes
	!		zero when price is zero instead of leaving it as
	!		garbage.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	05/24/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Use a record for TEMP map
	!		Ability to sort by product number
	!
	!	06/06/2000 - Kevin Handy
	!		Add label to end of line (LL)
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

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP (IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL

	%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.HB"
	MAP (IC_35HISTORY)	IC_35HISTORY_CDD	IC_35HISTORY

	DECLARE			IC_35HISTORY_CDD	IC_HISTORY(10%)

	!
	! Define Record Structure
	!
	RECORD TEMP_RFA
		DECIMAL(10, 2)	SORTKEY
		STRING		PRODUCT = 14%
		STRING		DESCR    = 40%
		STRING		TYPEX   =  2%
		STRING		CAT     =  4%
		STRING		SECONDC = 10%
		REAL		QTY
		REAL		PRICE
		REAL		COST
		REAL		PROFIT
		REAL		MARG
		STRING		LABEL = 4%
	END RECORD

	MAP (TEMP) TEMP_RFA TEMP

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Initialization:
	!
	! Initialize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^*(01) Sort by (P,T,C,D,S)\*
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
	!	T - Product Type
	!	.le
	!	C - Product Category
	!	.le
	!	D - Product Description
	!	.le
	!	S - Product Secondary_Code
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
	!	^*(02)From Item\*
	!	.p
	!	The ^*From Item\* field enters the
	!	item to begin with.
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

	LOC_WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Location Wildcard\*
	!	.p
	!	The ^*Location Wildcard\* field selects
	!	designated locations to be printed by entering a "wildcard"
	!	using the Wildcarding Technique.  All locations will be included
	!	if this field is left blank
	!
	! Index:
	!
	!--

	RANK_BY$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Rank by\*
	!	.p
	!	The ^*Rank by\* field
	!	ranks the report by the indicated item.
	!	.p
	!	Valid settings are:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	S - Sales
	!	.le
	!	P - Profit
	!	.le
	!	M - Gross Margin
	!	.le
	!	I - Inventory Product Number
	!	.els
	!	.lm -10
	!	.p
	!	A setting is required in this field.  No other settings are
	!	valid.
	!
	! Index:
	!	.x Rank by
	!
	!--

	FROM_PERIOD$ = EDIT$(UTL_REPORTX::OPTDEF(6%), -1%)

	!++
	! Abstract:FLD07
	!	^*(07) From Period\*
	!	.p
	!	The ^*From Period\* field contains the beginning period
	!	to start with.
	!
	! Index:
	!
	!--

	TO_PERIOD$ = EDIT$(UTL_REPORTX::OPTDEF(7%), -1%)

	!++
	! Abstract:FLD08
	!	^*(08) To Period\*
	!	.p
	!	The ^*To Period\* field contains the ending period
	!	to end with.
	!
	! Index:
	!
	!--

	%PAGE

300	!
	! Open Product file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

310	!
	! Open IC_CONTROL file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.OPN"
		GET #IC_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE #IC_CONTROL.CH%
	USE
		FILENAME$ = "IC_CONTROL"
		CONTINUE HelpError
	END WHEN

320	!
	! Create work file
	!
	CALL ASSG_CHANNEL(SA_TEMP.CH%, STAT%)
	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

	WHEN ERROR IN
		IF RANK_BY$ = "I"
		THEN
			OPEN UTL_WORK.DEV$ + "SA_TEMP.TMP" FOR OUTPUT AS FILE SA_TEMP.CH%, &
				ORGANIZATION INDEXED FIXED, &
				TEMPORARY, &
				BUFFER 32%, &
				MAP TEMP, &
				PRIMARY KEY TEMP::PRODUCT DUPLICATES, &
				ALLOW NONE, &
				ACCESS MODIFY
		ELSE
			OPEN UTL_WORK.DEV$ + "SA_TEMP.TMP" FOR OUTPUT AS FILE SA_TEMP.CH%, &
				ORGANIZATION INDEXED FIXED, &
				TEMPORARY, &
				BUFFER 32%, &
				MAP TEMP, &
				PRIMARY KEY TEMP::SORTKEY DUPLICATES, &
				ALLOW NONE, &
				ACCESS MODIFY
		END IF
	USE
		FILENAME$ = "SA_TEMP"
		CONTINUE HelpError
	END WHEN

330	!
	! Open History file
	!
	YYYYPP$ = FROM_PERIOD$

	Y% = 0%
	NY% = 0%

	WHILE READ_PERIOD("FIND", IC_CONTROL::ERA, YYYYPP$, &
		PERIOD_DESCR$, STAT$, START_DATE$, END_DATE$, NY%) = 0%

		NY% = 1%
		YYYY$ = LEFT$(YYYYPP$, 4%)
		IF YYYY$ <> ARR_YYYY$(Y%)
		THEN
			Y% = Y% + 1%
			PERIOD_DESCR$(Y%) = TRM$(PERIOD_DESCR$)

			WHEN ERROR IN
				%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.OPN"
			USE
				CONTINUE Years IF ERR = 5%
				FILENAME$ = "IC_35HISTORY"
				CONTINUE HelpError
			END WHEN

			IC_35HISTORY.CH%(Y%) = IC_35HISTORY.CH%
			IC_35HISTORY.CH% = 0%
			ARR_YYYY$(Y%) = YYYY$
			S.IND%(Y%) = VAL%(RIGHT$(YYYYPP$, 5%))
		END IF

		E_IND%(Y%) = VAL%(RIGHT$(YYYYPP$, 5%))
		PERIOD_DESCR$(0%) = TRM$(PERIOD_DESCR$)
		GOTO Years IF YYYYPP$ = TO_PERIOD$

	NEXT

 Years:
	YEARS% = Y%

	%PAGE

 ReportTitle:
	!
	! Title
	!
	SELECT SORTBY$
	CASE "P"
		K_NUM% = 0%
		TITLE$(1%) = "SALES VOLUME BY PRODUCT NUMBER"

	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = "SALES VOLUME BY PRODUCT TYPE"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = "SALES VOLUME BY PRODUCT CATEGORY"

	CASE "D"
		K_NUM% = 3%
		TITLE$(1%) = "SALES VOLUME BY PRODUCT DESCRIPTION"

	CASE "S"
		K_NUM% = 4%
		TITLE$(1%) = "SALES VOLUME BY PRODUCT SECONDARY CODE"

	END SELECT

	TITLE$(3%) = "FROM " + PERIOD_DESCR$(1%) + " TO " + PERIOD_DESCR$(0%)
	TITLE$(4%) = "Sales Analysis System"
	TITLE$(5%) = ""

	!
	! Heading
	!
	TITLE$(6%) = "Product#       Description              " + &
		"PT PCat SecCode         Qty        Cost" + &
		"       Sales  Sales%       GProf   Prof%  GMarg% Labl"

	TITLE$(7%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

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
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	PRINT_FLAG% = 0%

	CALL ENTR_3MESSAGE(SCOPE, "Creating work file . . .", 1% + 16%)

 GetNextRec:
	!
	! Print totals after each product
	!
	IF PRINT_FLAG%
	THEN
		GOSUB ProdTotal &
			IF QUANTITY <> 0.0 OR PRICE <> 0.0 OR COST <> 0.0
	END IF

17020	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	SELECT SORTBY$

	CASE "P"
		GOTO ExitTotal IF (PD_PRODUCT::PRODUCT_NUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PD_PRODUCT::PRODUCT_NUM, -1%), &
			WLDCRD$) = 0% AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitTotal IF (PD_PRODUCT::PROD_TYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PD_PRODUCT::PROD_TYPE, -1%), &
			WLDCRD$) = 0% AND WLDCRD$ <> ""

	CASE "C"
		GOTO ExitTotal IF (PD_PRODUCT::CATEGORY > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PD_PRODUCT::CATEGORY, -1%), &
			WLDCRD$) = 0% AND WLDCRD$ <> ""

	CASE "D"
		GOTO ExitTotal IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PD_PRODUCT::DESCRIPTION, -1%), &
			WLDCRD$) = 0% AND WLDCRD$ <> ""

	CASE "S"
		GOTO ExitTotal IF (PD_PRODUCT::SECONDARY_CODE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PD_PRODUCT::SECONDARY_CODE, -1%), &
			WLDCRD$) = 0% AND WLDCRD$ <> ""

	END SELECT

17100	!
	! Get History record
	!
	FOR Y% = 1% TO YEARS%

		EOF%(Y%) = 0%

		WHEN ERROR IN
			GET #IC_35HISTORY.CH%(Y%), &
				KEY #3% EQ PD_PRODUCT::PRODUCT_NUM, &
				REGARDLESS
		USE
			IF ERR = 155%
			THEN
				EOF%(Y%) = 1%
				CONTINUE 17115
			END IF

			FILENAME$ = "IC_35HISTORY"
			CONTINUE HelpError
		END WHEN

		IC_HISTORY(Y%) = IC_35HISTORY

17115	NEXT Y%
	SY% = 1%
	GOTO TestKey

 GetHistRec:
17120	SY% = Y%
	WHEN ERROR IN
		GET #IC_35HISTORY.CH%(Y%), REGARDLESS
	USE
		IF ERR = 11%
		THEN
			EOF%(Y%) = 1%
			CONTINUE TestKey
		END IF
		FILENAME$ = "IC_35HISTORY"
		CONTINUE HelpError
	END WHEN

	IC_HISTORY(Y%) = IC_35HISTORY

 TestKey:
	TEST.EOF% = 1%
	FOR Y% = 1% TO YEARS%
		EOF%(Y%) = 1% &
			IF IC_HISTORY(Y%)::PRODUCT <> &
			PD_PRODUCT::PRODUCT_NUM
		TEST.EOF% = TEST.EOF% * EOF%(Y%)
	NEXT Y%

	GOTO GetNextRec IF TEST.EOF% = 1%

	FOR Y% = SY% TO YEARS%
		IF EOF%(Y%) <> 1%
		THEN
			GOTO GetHistRec IF IC_HISTORY(Y%)::TRANSTYPE <> "SA"

			GOTO GetHistRec &
				IF COMP_STRING(EDIT$(IC_HISTORY(Y%)::LOCATION, &
				-1%), LOC_WLDCRD$) = 0% &
				AND LOC_WLDCRD$ <> ""
		END IF

	NEXT Y%

	FOR Y% = 1% TO YEARS%

		IF EOF%(Y%) = 0%
		THEN
			FOR I% = S.IND%(Y%) TO E_IND%(Y%)

				QUANTITY = QUANTITY - &
					IC_HISTORY(Y%)::PQUANTITY(I%)
				PRICE = PRICE + IC_HISTORY(Y%)::PRICEAMT(I%)
				COST = COST + IC_HISTORY(Y%)::COSTAMT(I%)

			NEXT I%
		END IF

	NEXT Y%

	PRINT_FLAG% = -1%

17130	FOR Y% = YEARS% TO 1% STEP -1%

		IF EOF%(Y%) = 0%
		THEN
			SY% = Y%

			WHEN ERROR IN
				GET #IC_35HISTORY.CH%(Y%), REGARDLESS
			USE
				IF ERR = 11%
				THEN
					EOF%(Y%) = 1%
					CONTINUE 17135
				END IF

				FILENAME$ = "IC_35HISTORY"
				CONTINUE HelpError
			END WHEN

			IC_HISTORY(Y%) = IC_35HISTORY
		END IF

17135	NEXT Y%

	GOTO TestKey

	%PAGE

 ExitTotal:
	RESET #SA_TEMP.CH%, KEY #0%

 GetTempRec:
17500	WHEN ERROR IN
		GET #SA_TEMP.CH%, REGARDLESS
	USE
		CONTINUE PrintTotals IF ERR = 11%
		FILENAME$ = "SA_TEMP"
		CONTINUE HelpError
	END WHEN

	IF TOTAL_PRICE <> 0.0
	THEN
		PERPRICE = FUNC_ROUND(TEMP::PRICE / TOTAL_PRICE * 100.0, 2%)
	ELSE
		PERPRICE = 0.0
	END IF

	TOTAL.PERPRICE = TOTAL.PERPRICE + PERPRICE

	IF TOTAL_PROFIT <> 0.0
	THEN
		PERPROFIT = FUNC_ROUND(TEMP::PROFIT / TOTAL_PROFIT * 100.0, 2%)
	ELSE
		PERPROFIT = 0.0
	END IF

	TOTAL.PERPROFIT = TOTAL.PERPROFIT + PERPROFIT

	IF TOTAL_PRICE <> 0.0
	THEN
		TOTAL.MARG = FUNC_ROUND((TOTAL_PRICE - TOTAL_COST) / &
				TOTAL_PRICE * 100.0, 2%)
	ELSE
		TOTAL.MARG = 0.0
	END IF

	TEXT$ = TEMP::PRODUCT + " " + &
		LEFT$(TEMP::DESCR, 24%) + " " + &
		TEMP::TYPEX + " " + &
		TEMP::CAT + " " + &
		TEMP::SECONDC + " " + &
		FORMAT$(TEMP::QTY, "########") + " " + &
		FORMAT$(TEMP::COST, "###,###,###") + " " + &
		FORMAT$(TEMP::PRICE, "###,###,###") + " " + &
		FORMAT$(PERPRICE, "####.#%") + " " + &
		FORMAT$(TEMP::PROFIT, "###,###,###") + " " + &
		FORMAT$(PERPROFIT, "####.#%") + " " + &
		FORMAT$(TEMP::MARG, "####.#%") + " " + &
		TEMP::LABEL

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEMP::QTY = 0.0
	TEMP::PRICE = 0.0
	TEMP::COST = 0.0
	TEMP::PROFIT = 0.0
	TEMP::MARG = 0.0

	GOTO GetTempRec

 PrintTotals:
17550	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = "G R A N D   T O T A L" + SPACE$(47%) + &
		FORMAT$(TOTAL_COST, "###,###,###") + " " + &
		FORMAT$(TOTAL_PRICE, "###,###,###") + " " + &
		FORMAT$(TOTAL.PERPRICE, "####.#%") + " " + &
		FORMAT$(TOTAL_PROFIT, "###,###,###") + " " + &
		FORMAT$(TOTAL.PERPROFIT, "####.#%") + " " + &
		FORMAT$(TOTAL.MARG, "####.#%")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

 ExitProgram:
18000	CALL OUTP_FINISH(UTL_REPORTX)

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

 ProdTotal:
18200	!
	! Create TEMP record
	!
	SELECT RANK_BY$

	CASE "S"
		TEMP::SORTKEY = -PRICE
		TITLE$(2%) = "RANKED BY SALES"

	CASE "P"
		TEMP::SORTKEY = -(PRICE - COST)
		TITLE$(2%) = "RANKED BY PROFIT"

	CASE "M"
		IF PRICE <> 0.0
		THEN
			TEMP::SORTKEY = -FUNC_ROUND((PRICE - COST) / &
				PRICE * 100.0, 2%)
		ELSE
			TEMP::SORTKEY = 0.0
		END IF
		TITLE$(2%) = "RANKED BY GROSS MARGIN"

	CASE "I"
		TEMP::SORTKEY = 0.0
		TITLE$(2%) = "RANKED BY PART NUMBER"

	END SELECT

	TEMP::PRODUCT = PD_PRODUCT::PRODUCT_NUM
	TEMP::DESCR = PD_PRODUCT::DESCRIPTION
	TEMP::TYPEX = PD_PRODUCT::PROD_TYPE
	TEMP::CAT = PD_PRODUCT::CATEGORY
	TEMP::SECONDC = PD_PRODUCT::SECONDARY_CODE
	TEMP::QTY = QUANTITY
	TEMP::PRICE = PRICE
	TEMP::COST = COST
	TEMP::PROFIT = PRICE - COST
	TEMP::LABEL = PD_PRODUCT::LABEL

	IF PRICE <> 0.0
	THEN
		TEMP::MARG = FUNC_ROUND((PRICE - COST) / PRICE * 100.0, 2%)
	ELSE
		TEMP::MARG = 0.0
	END IF

	WHEN ERROR IN
		PUT #SA_TEMP.CH%
	USE
		FILENAME$ = "SA_TEMP " + NUM1$(TEMP::SORTKEY)
		CONTINUE HelpError
	END WHEN

	TOTAL_COST = TOTAL_COST + TEMP::COST
	TOTAL_PRICE = TOTAL_PRICE + TEMP::PRICE
	TOTAL_PROFIT = TOTAL_PROFIT + TEMP::PROFIT

	PRINT_FLAG% = 0%
	QUANTITY, PRICE, COST = 0.0

	RETURN

	%PAGE

 HelpError:
18900	!******************************************************************
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
	! End of report SA_RPRT_PRDVOLUME
	!******************************************************************
	END
