1	%TITLE "Sales Volume Report by Customer"
	%SBTTL "SA_RPRT_CUSVOLUME"
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
	! ID:SA0018
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Sales Volume by Customer Report\* option
	!	prints a sales report listing the following
	!	information:
	!	.table 3,25
	!	.te
	!	Customer Number
	!	.te
	!	Customer Name
	!	.te
	!	Customer Type
	!	.te
	!	Customer Category
	!	.te
	!	Cost
	!	.te
	!	Sales Amount
	!	.te
	!	Sales Percentage
	!	.te
	!	Gross Profit
	!	.te
	!	Gross Margin
	!	.end table
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS SA_SOURCE:SA_RPRT_CUSVOLUME/LINE
	!	$ LINK/EXECUTABLE=SA_EXE: SA_RPRT_CUSVOLUME, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE SA_RPRT_CUSVOLUME.OBJ;*
	!
	! Author:
	!
	!	01/15/92 - Dan Perkins
	!
	! Modification history:
	!
	!	01/17/92 - Dan Perkins
	!		Changed decimal value to allow larger numbers.
	!		Changed fields to allow larger numbers.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/13/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/12/97 - Kevin Handy
	!		Set up UTL_WORK.DEV$ before use.
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
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excess %PAGE's
	!
	!	12/13/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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
	! External functions
	!
	EXTERNAL LONG	FUNCTION PD_EXAM_PRODUCT

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP (IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL

	%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.HB"
	MAP (IC_35HISTORY)	IC_35HISTORY_CDD	IC_35HISTORY

	DECLARE			IC_35HISTORY_CDD	IC_HISTORY(10%)

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	!
	! Define Record Structure
	!
	MAP (TEMP) &
		DECIMAL(10, 2)	TEMP_SORTKEY, &
		STRING		TEMP_CUSNUM   = 10%, &
		STRING		TEMP_CUSNAM   = 50%, &
		STRING		TEMP_TYPE     =  2%, &
		STRING		TEMP_CATEGORY =  4%, &
		REAL		TEMP_PRICE, &
		REAL		TEMP_COST, &
		REAL		TEMP_PROFIT, &
		REAL		TEMP_MARG

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
	!	^*(01) Sort by (N,T,C,A)\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines the order
	!	in which the report will print.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*N\* - Customer Number
	!	.te
	!	^*T\* - Customer Type
	!	.te
	!	^*C\* - Customer Category
	!	.te
	!	^*A\* - Customer Alpha Sort
	!	.end table
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
	!	.b
	!	.lm +5
	!	The ^*From Item\* field enters the
	!	item with which the report is to begin printing.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field enters the item with which the
	!	report is to end printing.
	!	.b
	!	A blank field will cause the report to end with the last
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects
	!	designated items to be printed by entering a "wildcard"
	!	using the Wildcarding Technique.
	!	.lm -5
	!
	! Index:
	!
	!--

	LOC_WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Location Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Location Wildcard\* field selects
	!	designated locations to be printed by entering a "wildcard"
	!	using the Wildcarding Technique.
	!	.lm -5
	!
	! Index:
	!
	!--

	RANK_BY$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Rank by\*
	!	.b
	!	.lm +5
	!	The ^*Rank by\* field
	!	ranks the report by the indicated item.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*S\* - Sales
	!	.te
	!	^*P\* - Profit
	!	.te
	!	^*M\* - Gross Margin
	!	.end table
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
	!	.b
	!	.lm +5
	!	The ^*From Period\* field contains the beginning period
	!	with which the report will begin printing.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_PERIOD$ = EDIT$(UTL_REPORTX::OPTDEF(7%), -1%)

	!++
	! Abstract:FLD08
	!	^*(08) To Period\*
	!	.b
	!	.lm +5
	!	The ^*To Period\* field enters the period
	!	with which the report is to end printing.
	!	.lm -5
	!
	! Index:
	!
	!--

	%PAGE

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
	CALL ASSG_CHANNEL(SA_TEMP_CH%, STAT%)
	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "SA_TEMP_TMP" FOR OUTPUT AS FILE SA_TEMP_CH%, &
			ORGANIZATION INDEXED FIXED, &
			TEMPORARY, &
			BUFFER 32%, &
			MAP TEMP, &
			PRIMARY KEY (TEMP_SORTKEY) DUPLICATES, &
			ALLOW NONE, &
			ACCESS MODIFY
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
			S_IND%(Y%) = VAL%(RIGHT$(YYYYPP$, 5%))
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
	CASE "N"
		K_NUM% = 0%
		TITLE$(1%) = "SALES VOLUME BY CUSTOMER NUMBER"

	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = "SALES VOLUME BY CUSTOMER TYPE"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = "SALES VOLUME BY CUSTOMER CATEGORY"

	CASE "A"
		K_NUM% = 3%
		TITLE$(1%) = "SALES VOLUME BY CUSTOMER ALPHA SORT"

	END SELECT

	TITLE$(3%) = "FROM " + PERIOD_DESCR$(1%) + " TO " + PERIOD_DESCR$(0%)
	TITLE$(4%) = "Sales Analysis System"
	TITLE$(5%) = ""

	!
	! Heading
	!
	TITLE$(6%) = "Customer#  Name" + SPACE$(42%) + &
		"CType Category        Cost" + &
		"       Sales  Sales%       GProf   Prof%  GMarg%"

	TITLE$(7%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

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
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	PRINT_FLAG% = 0%

	CALL ENTR_3MESSAGE(SCOPE, "Creating work file . . .", 1% + 16%)

 GetNextRec:
	!
	! Print totals after each customer
	!
	IF PRINT_FLAG%
	THEN
		GOSUB CustTotal IF PRICE <> 0.0 OR COST <> 0.0
	END IF

17020	!
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
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	SELECT SORTBY$
	CASE "N"
		GOTO ExitTotal IF (AR_35CUSTOM::CUSNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$(AR_35CUSTOM::CUSNUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitTotal IF (AR_35CUSTOM::TTYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$(AR_35CUSTOM::TTYPE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "C"
		GOTO ExitTotal IF (AR_35CUSTOM::CATEGORY > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$(AR_35CUSTOM::CATEGORY, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "A"
		GOTO ExitTotal IF (AR_35CUSTOM::ALPSRT > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$(AR_35CUSTOM::ALPSRT, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

17100	!
	! Get History record
	!
	FOR Y% = 1% TO YEARS%

		EOF%(Y%) = 0%
		WHEN ERROR IN
			GET #IC_35HISTORY.CH%(Y%), &
				KEY #1% EQ AR_35CUSTOM::CUSNUM, &
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
	TEST_EOF% = 1%
	FOR Y% = 1% TO YEARS%
		EOF%(Y%) = 1% IF IC_HISTORY(Y%)::CROSSREF <> AR_35CUSTOM::CUSNUM
		TEST_EOF% = TEST_EOF% * EOF%(Y%)
	NEXT Y%

	GOTO GetNextRec IF TEST_EOF% = 1%

	FOR Y% = SY% TO YEARS%
		IF EOF%(Y%) <> 1%
		THEN
			GOTO GetHistRec IF IC_HISTORY(Y%)::TRANSTYPE <> "SA"

			GOTO GetHistRec IF COMP_STRING(EDIT$(IC_HISTORY(Y%)::LOCATION, -1%), LOC_WLDCRD$) = 0% &
				AND LOC_WLDCRD$ <> ""

			GOTO GetHistRec IF PD_EXAM_PRODUCT(IC_HISTORY(Y%)::PRODUCT, PD_PRODUCT_EXAM) <> CMC$_NORMAL
		END IF

	NEXT Y%

	FOR Y% = 1% TO YEARS%

		IF EOF%(Y%) = 0%
		THEN
			FOR I% = S_IND%(Y%) TO E_IND%(Y%)

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
	RESET #SA_TEMP_CH%, KEY #0%

 GetTempRec:
17500	WHEN ERROR IN
		GET #SA_TEMP_CH%, REGARDLESS
	USE
		CONTINUE PrintTotals IF ERR = 11%
		FILENAME$ = "SA_TEMP"
		CONTINUE HelpError
	END WHEN

	IF TOTAL_PRICE <> 0.0
	THEN
		PERPRICE = FUNC_ROUND(TEMP_PRICE / TOTAL_PRICE * 100.0, 2%)
	ELSE
		PERPRICE = 0.0
	END IF

	TOTAL_PERPRICE = TOTAL_PERPRICE + PERPRICE

	IF TOTAL_PROFIT <> 0.0
	THEN
		PERPROFIT = FUNC_ROUND(TEMP_PROFIT / TOTAL_PROFIT * 100.0, 2%)
	ELSE
		PERPROFIT = 0.0
	END IF

	TOTAL_PERPROFIT = TOTAL_PERPROFIT + PERPROFIT

	IF TOTAL_PRICE <> 0.0
	THEN
		TOTAL_MARG = FUNC_ROUND((TOTAL_PRICE - TOTAL_COST) / &
			TOTAL_PRICE * 100.0, 2%)
	ELSE
		TOTAL_MARG = 0.0
	END IF

	TEXT$ = TEMP_CUSNUM + " " + &
		LEFT$(TEMP_CUSNAM, 45%) + " " + &
		TEMP_TYPE + "    " + &
		TEMP_CATEGORY + "     " + &
		FORMAT$(TEMP_COST, "###,###,###") + " " + &
		FORMAT$(TEMP_PRICE, "###,###,###") + " " + &
		FORMAT$(PERPRICE, "####.#%") + " " + &
		FORMAT$(TEMP_PROFIT, "###,###,###") + " " + &
		FORMAT$(PERPROFIT, "####.#%") + " " + &
		FORMAT$(TEMP_MARG, "####.#%")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEMP_PRICE = 0.0
	TEMP_COST = 0.0
	TEMP_PROFIT = 0.0
	TEMP_MARG = 0.0

	GOTO GetTempRec

 PrintTotals:
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = "G R A N D   T O T A L" + SPACE$(51%) + &
		FORMAT$(TOTAL_COST,  "###,###,###") + " " + &
		FORMAT$(TOTAL_PRICE, "###,###,###") + " " + &
		FORMAT$(TOTAL_PERPRICE, "####.#%") + " " + &
		FORMAT$(TOTAL_PROFIT, "###,###,###") + " " + &
		FORMAT$(TOTAL_PERPROFIT, "####.#%") + " " + &
		FORMAT$(TOTAL_MARG, "####.#%")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

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

 CustTotal:
	!
	! Create TEMP record
	!
	SELECT RANK_BY$

	CASE "S"
		TEMP_SORTKEY = -PRICE
		TITLE$(2%) = "RANKED BY SALES"

	CASE "P"
		TEMP_SORTKEY = -(PRICE - COST)
		TITLE$(2%) = "RANKED BY PROFIT"

	CASE "M"
		TEMP_SORTKEY = -FUNC_ROUND((PRICE - COST) / &
			PRICE * 100., 2%) IF PRICE <> 0.0
		TITLE$(2%) = "RANKED BY GROSS MARGIN"

	END SELECT

	TEMP_CUSNUM = AR_35CUSTOM::CUSNUM
	TEMP_CUSNAM = AR_35CUSTOM::CUSNAM
	TEMP_TYPE = AR_35CUSTOM::TTYPE
	TEMP_CATEGORY = AR_35CUSTOM::CATEGORY
	TEMP_PRICE = PRICE
	TEMP_COST = COST
	TEMP_PROFIT = PRICE - COST

	IF PRICE <> 0.0
	THEN
		TEMP_MARG = FUNC_ROUND((PRICE - COST) / PRICE * 100., 2%)
	ELSE
		TEMP_MARG = 0.0
	END IF

	PUT #SA_TEMP_CH%

	TOTAL_COST = TOTAL_COST + TEMP_COST
	TOTAL_PRICE = TOTAL_PRICE + TEMP_PRICE
	TOTAL_PROFIT = TOTAL_PROFIT + TEMP_PROFIT

	PRINT_FLAG% = 0%
	PRICE, COST = 0.0

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
	! End of report SA_RPRT_CUSVOLUME
	!******************************************************************
	END
