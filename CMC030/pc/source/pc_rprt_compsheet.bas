1	%TITLE "Price Comparison Multiple Sheet"
	%SBTTL "PC_RPRT_COMPSHEET"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986, 1988 BY
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
	! ID:PC006
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Price Comparison Multiple Sheet\* report compares
	!	several price types and then sort in the desired order. The fields included
	!	in this report are the following:
	!	.table 3,25
	!	.te
	!	Product Number
	!	.te
	!	Description
	!	.te
	!	Type
	!	.te
	!	Category
	!	.te
	!	Standard Cost
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Multiple Price Comparison
	!	.x Multiple Price Comparison>Report
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PC_SOURCE:PC_RPRT_COMPSHEET/LINE
	!	$ LINK/EXE=PC_EXE: PC_RPRT_COMPSHEET, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PC_RPRT_COMPSHEET.OBJ;*
	!
	! Author:
	!
	!	04/04/88 - Frank F. Starman
	!
	! Modification History:
	!
	!	09/25/90 - Frank F. Starman
	!		Add percent column (comperisson the price to STD)
	!
	!	04/08/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/10/93 - Dan Perkins
	!		Added code for secondary_code in from item, to item,
	!		and wildcard sort by areas.
	!		Changeed COMP_STRING to COMP_ARRAY.
	!
	!	06/09/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	06/20/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	06/20/95 - Kevin Handy
	!		Added Label column to report.
	!
	!	08/25/95 - Kevin Handy
	!		Modify to be able to exclude standard cost column.
	!		Uses new report settings field.
	!		Also, don't print %'s when not printing cost.
	!
	!	09/06/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/26/97 - Kevin Handy
	!		Add "CD" sort option. (LL)
	!
	!	08/28/97 - Kevin Handy
	!		Change "%" to "Gross Margin %"
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/24/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	06/07/2000 - Kevin Handy
	!		Add wildcard label (LL)
	!		Seems there was a start of this (variable WILDLABEL$)
	!		but it was never filled in anywhere.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	!
	! External functions
	!
	EXTERNAL REAL    FUNCTION PC_READ_PRICE
	EXTERNAL REAL    FUNCTION PC_READ_COST

	DIM PC_TYPE$(50%)
	DIM PERC_COUNT%(50%)
	DIM PERC_TOTAL%(50%)

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes the report
	!	to begin with a selected item number.
	!	.b
	!	A blank setting will cause the report to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Price Comparison Multiple Sheet
	!	.x Price Comparison Multiple Sheet>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes the report
	!	to end with a selected item number.
	!	.b
	!	A blank setting will cause the report to end with the last
	!	item number in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Price Comparison Multiple Sheet
	!	.x Price Comparison Multiple Sheet>To Item
	!	.x Item>To
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated items
	!	to be printed on the report by entering a ^*Wildcard\* selection.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Price Comparison Multiple Sheet
	!	.x Price Comparison Multiple Sheet>Wildcard
	!
	!--

	WLDCRD_LABEL$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard Label\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated labels (as defined
	!	in the product master file)
	!	to be printed on the report by entering a ^*Wildcard\* selection in
	!	this field.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard Label>Price Comparison Multiple Sheet
	!	.x Price Comparison Multiple Sheet>Wildcard Label
	!
	!--

	LOCATION$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 132%)

	!++
	! Abstract:FLD05
	!	^*(05) Locations\*
	!	.b
	!	.lm +5
	!	The ^*Locations\* field enters a location code
	!	which is established in the Company Profile file which is located
	!	in the Utility system.
	!	.b
	!	This field will accommodate up to twenty (20) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Locations>Price Comparison Multiple Sheet
	!	.x Price Comparison Multiple Sheet>Locations
	!
	!--

	PRICE_TYPE$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Price Types\*
	!	.b
	!	.lm +5
	!	The ^*Price Types\* field refers to the two character code which indicates
	!	the type of price category these prices fall into. The valid codes are
	!	entered in the Price Type Table.
	!	Enter the various codes to be printed seperated by commas (,).
	!	.lm -5
	!
	! Index:
	!	.x Price Types
	!
	!--

	SHOW_COST$ = LEFT$(UTL_REPORTX::OPTDEF(6%), 1%)

	!++
	! Abstract:FLD07
	!	^*(07) Show Cost	(Yes/No)\*
	!	.b
	!	.lm +5
	!	Determines if the standard cost
	!	field be displayed on the report.
	!	.lm -5
	!
	! Index:
	!	.x Price Types
	!
	!--

	TOTIME$ = EDIT$(UTL_REPORTX::OPTDEF(7%), 132%)

	!++
	! Abstract:FLD08
	!	^*(08) Time\*
	!	.b
	!	.lm +5
	!	The ^*Time\* field enters the time of day in which
	!	the prices went into effect.
	!	.lm -5
	!
	! Index:
	!	.x Time
	!
	!--

	TODATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(8%), 132%))

	!++
	! Abstract:FLD09
	!	^*(09) Date\*
	!	.b
	!	.lm +5
	!	The ^*Date\* field enters the date of the time frame
	!	for which the assigned prices are in question.
	!	.lm -5
	!
	! Index:
	!	.x Date
	!
	!--

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(9%), 132%)

	!++
	! Abstract:FLD10
	!	^*(10) Sort (C,D,P,S,T,CD)\*
	!	.b
	!	.lm +5
	!	The ^*Sort\* field selects the order
	!	in which the report will print.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*C\* = Product Category
	!	.te
	!	^*D\* = Product Description
	!	.te
	!	^*P\* = Product Number
	!	.te
	!	^*S\* = Product Secondary Code
	!	.te
	!	^*T\* = Product Type
	!	.te
	!	^*CD\* = Category/Description
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort>Price Comparison Multiple Sheet
	!	.x Price Comparison Multiple Sheet>Sort
	!
	!--

	SELECT SORT_BY$

	CASE "C"
		SORT_KEY% = 2%
		ADD_TITLE$ = "BY CATEGORY"

	CASE "CD"
		SORT_KEY% = 0%
		ADD_TITLE$ = "BY  CATEGORY/DESCRIPTION"

	CASE "D"
		SORT_KEY% = 3%
		ADD_TITLE$ = "BY DESCRIPTION"

	CASE "P"
		SORT_KEY% = 0%
		ADD_TITLE$ = "BY PRODUCT NUMBER"

	CASE "T"
		SORT_KEY% = 1%
		ADD_TITLE$ = "BY PRODUCT TYPE"

	CASE "S"
		SORT_KEY% = 4%
		ADD_TITLE$ = "BY SECONDARY NUMBER"

	END SELECT

	GOTO 300 IF PRICE_TYPE$ = ""

	PRICE_TYPE$ = PRICE_TYPE$ + ","

	WHILE PRICE_TYPE$ <> ""

		COMMA% = INSTR(1%, PRICE_TYPE$, ",")
		I% = I% + 1%
		PC_TYPE$(I%) = LEFT(PRICE_TYPE$, COMMA% - 1%)
		PRICE_TYPE$ = RIGHT(PRICE_TYPE$, COMMA% + 1%)
	NEXT

	I% = 6% IF I% > 6%
	PC_TYPE_LOOP% = I%

	FOR I% = 1% TO PC_TYPE_LOOP%
		TITLE2$ = TITLE2$ + SPACE$(10% - LEN(PC_TYPE$(I%))) + &
			PC_TYPE$(I%)
		TITLE2$ = TITLE2$ + " GM%" IF SHOW_COST$ <> "N"
	NEXT I%

	IF SHOW_COST$ <> "N"
	THEN
		TITLE3$ = "       Std Cost"
	ELSE
		TITLE3$ = ""
	END IF

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

500	GOTO 600 IF SORT_BY$ <> "CD"

	CALL ENTR_3MESSAGE(SCOPE, "Creating sort file", 1%)

	!======================================================================
	! PD_PRODUCT file (open read only)
	!======================================================================

	CALL ASSG_CHANNEL(PD_PRODUCT_TEMP.CH%, STAT%)

	PD_PRODUCT_TEMP.NAME$ = "PD_PRODUCT.TEMP"

	WHEN ERROR IN
		OPEN PD_PRODUCT_TEMP.NAME$ FOR OUTPUT AS FILE PD_PRODUCT_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			TEMPORARY, &
			MAP PD_PRODUCT, &
			PRIMARY KEY &
			( &
				PD_PRODUCT::CATEGORY, &
				PD_PRODUCT::DESCRIPTION &
			) DUPLICATES, &
			ACCESS MODIFY
	USE
		FILENAME$ = "SORT FILE"
		CONTINUE HelpError
	END WHEN

510	!
	! Build sorted file
	!
	WHEN ERROR IN
		IF FROM_ITEM$ <> ""
		THEN
			FIND #PD_PRODUCT.CH%, KEY #2% GE FROM_ITEM$, REGARDLESS
		ELSE
			RESET #PD_PRODUCT.CH%
		END IF
	USE
		FILENAME$ = "SORT FILE"
		CONTINUE HelpError
	END WHEN

520	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, REGARDLESS
	USE
		CONTINUE 590
	END WHEN

	GOTO 520 IF WLDCRD_LABEL$ <> "" AND &
		COMP_STRING(EDIT$(PD_PRODUCT::LABEL, -1%), WLDCRD_LABEL$) = 0%

	GOTO 520 IF PD_PRODUCT::SSTATUS <> "A"

	GOTO 520 IF (PD_PRODUCT::CATEGORY > TO_ITEM$) AND TO_ITEM$ <> ""

	PUT #PD_PRODUCT_TEMP.CH%

	GOTO 520

590	CLOSE #PD_PRODUCT.CH%

	PD_PRODUCT.CH% = PD_PRODUCT_TEMP.CH%

600	!

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "MULTIPLE  PRICE  COMPARISON  SHEET"
	TITLE$(3%) = "Product Price & Cost System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "Product#       Description              " + &
		"         Tp Cat  " + TITLE3$ + TITLE2$ + &
		" Label"

	TITLE$(6%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		RESET #UTL_LOCATION.CH%
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

 NextLocation:
	WHEN ERROR IN
		GET #UTL_LOCATION.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

	GOTO NextLocation IF LOCATION$ <> "" AND &
		COMP_ARRAY(EDIT$(UTL_LOCATION::LOCATION, -1%), LOCATION$) = 0%

	TITLE$(2%) = "AT LOCATION " + TRM$(UTL_LOCATION::LOCATION) + " " + &
		TRM$(UTL_LOCATION::LOCNAME) + "  SORT " + ADD_TITLE$

	IF PRINT_FLAG%
	THEN
		GOSUB LocTotal
		LIN% = 999%
	END IF

17010	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PD_PRODUCT.CH%, KEY #SORT_KEY%
		ELSE
			FIND #PD_PRODUCT.CH%, &
				KEY #SORT_KEY% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE NextLocation IF ERR = 155%
		FILENAME$ = "PD_PRODUCT"
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
		GET #PD_PRODUCT.CH%, REGARDLESS
	USE
		CONTINUE NextLocation IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO GetNextRec IF PD_PRODUCT::SSTATUS <> "A"

	IF WLDCRD_LABEL$ <> ""
	THEN
		GOTO GetNextRec IF &
			COMP_ARRAY(EDIT$(PD_PRODUCT::LABEL, -1%), &
			WLDCRD_LABEL$) = 0%
	END IF

	SELECT SORT_BY$

	CASE "C", "CD"
		GOTO NextLocation IF (PD_PRODUCT::CATEGORY > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF &
				COMP_ARRAY(EDIT$(PD_PRODUCT::CATEGORY, -1%), &
				WLDCRD$) = 0%
		END IF

		IF TEST_CATEGORY$ <> PD_PRODUCT::CATEGORY AND &
			TEST_CATEGORY$ <> "" AND PRINT_LINE%
		THEN
			IF SHOW_COST$ <> "N"
			THEN
				TEXT$ = "Category % average" + SPACE$(39%)
				TEXT$ = TEXT$ + SPACE$(15%) IF SHOW_COST$ <> "N"

				FOR I% = 1% TO PC_TYPE_LOOP%

					PERC% = 0%

					PERC% = 100% - PERC_TOTAL%(I%) / PERC_COUNT%(I%) &
						IF PERC_COUNT%(I%) <> 0.0

					TEXT$ = TEXT$ + SPACE$(10%) + &
						FORMAT$(PERC%, "###%")

					PERC_TOTAL%(I%), PERC_COUNT%(I%) = 0%
				NEXT I%

				CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
				CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
			END IF

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 2%)
			PRINT_LINE% = 0%
		END IF

		TEST_CATEGORY$ = PD_PRODUCT::CATEGORY

	CASE "D"
		GOTO NextLocation IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF &
				COMP_ARRAY(EDIT$(PD_PRODUCT::DESCRIPTION, -1%), &
				WLDCRD$) = 0%
		END IF

	CASE "P"
		GOTO NextLocation IF (PD_PRODUCT::PRODUCT_NUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF &
				COMP_ARRAY(EDIT$(PD_PRODUCT::PRODUCT_NUM, -1%), &
				WLDCRD$) = 0%
		END IF

	CASE "S"
		GOTO NextLocation IF (PD_PRODUCT::SECONDARY_CODE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF &
				COMP_ARRAY(EDIT$(PD_PRODUCT::SECONDARY_CODE, -1%), &
				WLDCRD$) = 0%
		END IF

	CASE "T"
		GOTO NextLocation IF (PD_PRODUCT::PROD_TYPE> TO_ITEM$) &
			AND TO_ITEM$ <> ""

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF &
				COMP_ARRAY(EDIT$(PD_PRODUCT::PROD_TYPE, -1%), &
				WLDCRD$) = 0%
		END IF

		IF TEST_PRODTYPE$ <> PD_PRODUCT::PROD_TYPE AND &
			TEST_PRODTYPE$ <>"" AND PRINT_LINE%
		THEN
			IF SHOW_COST$ <> "N"
			THEN
				TEXT$ = "Type     % average" + SPACE$(39%)
				TEXT$ = TEXT$ + SPACE$(15%) IF SHOW_COST$ <> "N"

				FOR I% = 1% TO PC_TYPE_LOOP%

					PERC% = 0%

					PERC% = 100% - PERC_TOTAL%(I%) / PERC_COUNT%(I%) &
						IF PERC_COUNT%(I%) <> 0.0

					TEXT$ = TEXT$ + SPACE$(10%) + &
						FORMAT$(PERC%, "###%")

					PERC_TOTAL%(I%), PERC_COUNT%(I%) = 0%
				NEXT I%

				CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
				CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)

			END IF

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 2%)
			PRINT_LINE% = 0%
		END IF

		TEST_PRODTYPE$ = PD_PRODUCT::PROD_TYPE

	END SELECT

	COST = PC_READ_COST(PD_PRODUCT::PRODUCT_NUM, UTL_LOCATION::LOCATION, &
		TODATE$, "")

	TEXT$ = ""

	FOR I% = 1% TO PC_TYPE_LOOP%

		PRICE = PC_READ_PRICE(PD_PRODUCT::PRODUCT_NUM, &
			UTL_LOCATION::LOCATION, PC_TYPE$(I%), &
			TODATE$, TOTIME$, "", "")

		PERC% = 0%
		PERC% = 100% - COST / PRICE * 100% IF PRICE <> 0.0

		TEXT$ = TEXT$ + FORMAT$(PRICE, "#######.##")

		TEXT$ = TEXT$ + FORMAT$(PERC%, "###%") &
			IF SHOW_COST$ <> "N"

		IF PERC% <> 0%
		THEN
			!
			! add non zero lines
			!
			PERC_COUNT%(I%) = PERC_COUNT%(I%) + 1%
			PERC_TOTAL%(I%) = PERC_TOTAL%(I%) + PERC%

			PERC_GCOUNT%(I%) = PERC_GCOUNT%(I%) + 1%
			PERC_GTOTAL%(I%) = PERC_GTOTAL%(I%) + PERC%
		END IF

	NEXT I%

	!
	! Merge product description into prices
	!
	TEXT1$ = PD_PRODUCT::PRODUCT_NUM + " " + &
		LEFT(PD_PRODUCT::DESCRIPTION, 33%) + " " + &
		PD_PRODUCT::PROD_TYPE + " " + &
		PD_PRODUCT::CATEGORY + " "

	TEXT1$ = TEXT1$ + &
		FORMAT$(COST, "#,###,###.#####") &
		IF SHOW_COST$ <> "N"

	TEXT$ = TEXT1$ + &
		TEXT$ + " " + &
		PD_PRODUCT::LABEL

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, LIN%)

	LIN% = 0%
	PRINT_FLAG% = -1%
	PRINT_LINE% = -1%

	!
	! Try for next record
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT
	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!
	GOSUB LocTotal

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

 LocTotal:
	SELECT SORT_BY$

	CASE "C"
		IF SHOW_COST$ <> "N"
		THEN
			TEXT$ = "Category % average" + SPACE$(39%)
			TEXT$ = TEXT$ + SPACE$(15%) IF SHOW_COST$ <> "N"

			FOR I% = 1% TO PC_TYPE_LOOP%

				PERC% = 0%

				PERC% = 100% - PERC_TOTAL%(I%) / PERC_GCOUNT%(I%) &
					IF PERC_GCOUNT%(I%) <> 0%

				TEXT$ = TEXT$ + SPACE$(10%) + &
					FORMAT$(PERC%, "###%")

				PERC_TOTAL%(I%), PERC_COUNT%(I%) = 0%
			NEXT I%

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
		END IF

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 2%)

	CASE "T"
		IF SHOW_COST$ <> "N"
		THEN
			TEXT$ = "Type     % average" + SPACE$(39%)
			TEXT$ = TEXT$ + SPACE$(15%) IF SHOW_COST$ <> "N"

			FOR I% = 1% TO PC_TYPE_LOOP%

				PERC% = 0%

				PERC% = 100% - PERC_TOTAL%(I%) / PERC_GCOUNT%(I%) &
					IF PERC_GCOUNT%(I%) <> 0%

				TEXT$ = TEXT$ + SPACE$(10%) + &
					FORMAT$(PERC%, "###%")

				PERC_TOTAL%(I%), PERC_COUNT%(I%) = 0%

			NEXT I%

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
		END IF

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 2%)

	END SELECT

	IF SHOW_COST$ <> "N"
	THEN
		TEXT$ = "Location % average" + SPACE$(39%)
		TEXT$ = TEXT$ + SPACE$(15%) IF SHOW_COST$ <> "N"

		FOR I% = 1% TO PC_TYPE_LOOP%

			PERC% = 0%

			PERC% = 100% - PERC_GTOTAL%(I%) / PERC_GCOUNT%(I%) &
				IF PERC_GCOUNT%(I%) <> 0%

			TEXT$ = TEXT$ + SPACE$(10%) + FORMAT$(PERC%, "###%")

		NEXT I%

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 1%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	RETURN

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
