1	%TITLE "Product Price File List"
	%SBTTL "PC_RPRT_PRICE"
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
	! ID:PC003
	!
	! Abstract:HELP
	!	.p
	!	The ^*Product Price File\* option
	!	prints a list of the records in the Product Price File. The
	!	report will include the following information:
	!	.lm +10
	!	.b
	!	.list 0,"*"
	!	.le
	!	Product _#
	!	.le
	!	Description
	!	.le
	!	Type
	!	.le
	!	Category
	!	.le
	!	Unit of Measure
	!	.le
	!	Location
	!	.le
	!	Price Type
	!	.le
	!	Date
	!	.le
	!	Time
	!	.le
	!	Price
	!	.els
	!
	! Index:
	!	.x Product Price File>Report
	!	.x Report>Product Price File
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PC_SOURCE:PC_RPRT_PRICE/LINE
	!	$ LINK/EXE=PC_EXE: PC_RPRT_PRICE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PC_RPRT_PRICE.OBJ;*
	!
	! Author:
	!
	!	07/28/87 - Frank Starman
	!
	! Modification History:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/27/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to "" in several places.
	!
	!	09/06/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/28/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/21/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	11/01/2000 - Kevin Handy
	!		Use A"x"B
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.HB"
	MAP	(PC_PRICE)	PC_PRICE_CDD	PC_PRICE

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP	(PD_PRODUCT)	PD_PRODUCT_CDD	PD_PRODUCT

	DECLARE STRING TEST_PRINTING

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.p
	!	The ^*From Item\* field
	!	begins the list with a selected item number
	!	code.
	!	.p
	!	If this field is blank, the list will begin with the
	!	first record in the file.
	!
	! Index:
	!	.x From Item>Product Price File
	!	.x Prodcut Price File>From Item
	!	.x Item>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.p
	!	The ^*To Item\* field
	!	concludes the list with a selected item code.
	!	.p
	!	If this field is blank, the list will conclude with the last
	!	record in the file.
	!
	! Index:
	!	.x To Item>Product Price File
	!	.x Product Price File>To Item
	!	.x Item>To
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.p
	!	This ^*Wildcard\* field
	!	selects designated product description
	!	codes to be printed on the list by entering a "product code
	!	wildcard" value.
	!
	! Index:
	!	.x Wildcard>Product Price File
	!	.x Product Price File>Wildcard
	!
	!--

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(9%), 132%)

	!++
	! Abstract:FLD10
	!	^*(10) Sort (P,T,S)\*
	!	.p
	!	The ^*Sort\* field selects an order in which the report
	!	will print.
	!	.p
	!	Valid settings are:
	!	.lm +10
	!	.b
	!	.list 0,"*"
	!	.le
	!	P = Product Number
	!	.le
	!	T = Price Type
	!	.le
	!	S = Product Secondary Code
	!	.lm -10
	!	.p
	!	.els
	!	An entry is required in this field.
	!
	! Index:
	!	.x Sort>Product Price File
	!	.x Product Price File>Sort
	!
	!--


	SELECT SORT_BY$
	CASE "P"
		SORT_KEY% = 0%
		ADD_TITLE$ = "BY  PRODUCT"
	CASE "T"
		SORT_KEY% = 1%
		ADD_TITLE$ = "BY  PRICE  TYPE"
	END SELECT


300	WHEN ERROR IN
		%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.OPN"
	USE
		FILENAME$ = "PC_PRICE"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "PRODUCT  PRICE  FILE  LIST  SORT  " + ADD_TITLE$
	TITLE$(2%) = "Product Price & Cost System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Product#       Description              " + &
		"                Tp Cat  UOM Loc   PrTp " + &
		"Date       Time               Price"
	TITLE$(5%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PC_PRICE.CH%, KEY #SORT_KEY%
		ELSE
			FIND #PC_PRICE.CH%, &
				KEY #SORT_KEY% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to find beginning record!", 0%)
		CONTINUE ExitProgram
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
		GET #PC_PRICE.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PC_PRICE"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORT_BY$

	CASE "P"
		GOTO ExitTotal IF (PC_PRICE::PRODUCT_NUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PC_PRICE::PRODUCT_NUM, -1%), &
			WLDCRD$) = 0%

	CASE "T"
		GOTO ExitTotal  IF (PC_PRICE::PCTYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PC_PRICE::PCTYPE, -1%), WLDCRD$) = 0%

		IF TEST_PRICETYPE$ <> PC_PRICE::PCTYPE AND &
			TEST_PRICETYPE$ <>"" AND PRINT_LINE%
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), " ", -1%)
			PRINT_LINE% = 0%
		END IF

		TEST_PRICETYPE$ = PC_PRICE::PCTYPE

	END SELECT

17200	IF TEST_PRINTING = PC_PRICE::PRODUCT_NUM + PC_PRICE::LOCATION
	THEN
		PD_PRODUCT::DESCRIPTION = ""
		PD_PRODUCT::PROD_TYPE = ""
		PD_PRODUCT::CATEGORY = ""
		PD_PRODUCT::UOM = ""
	ELSE
		WHEN ERROR IN
			GET #PD_PRODUCT.CH%, &
				KEY #0% EQ PC_PRICE::PRODUCT_NUM, &
				REGARDLESS
		USE
			PD_PRODUCT::DESCRIPTION = &
				STRING$(LEN(PD_PRODUCT::DESCRIPTION), A"?"B)
			PD_PRODUCT::PROD_TYPE = &
				STRING$(LEN(PD_PRODUCT::PROD_TYPE), A"?"B)
			PD_PRODUCT::CATEGORY = &
				STRING$(LEN(PD_PRODUCT::CATEGORY), A"?"B)
			PD_PRODUCT::UOM = &
				STRING$(LEN(PD_PRODUCT::UOM), A"?"B)

			CONTINUE 17300 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "PD_PRODUCT"
			CONTINUE HelpError
		END WHEN
	END IF

17300	!
	! Print out one line
	!
	TEXT$ = PC_PRICE::PRODUCT_NUM + " " + &
		PD_PRODUCT::DESCRIPTION + " " + &
		PD_PRODUCT::PROD_TYPE + " " + &
		PD_PRODUCT::CATEGORY + " " + &
		PD_PRODUCT::UOM + "  " + &
		PC_PRICE::LOCATION + "  " + &
		PC_PRICE::PCTYPE + "   " + &
		PRNT_DATE(PC_PRICE::XDATE, 8%) + " " + &
		PRNT_TIME(PC_PRICE::XTIME, 0%) + " " + &
		FORMAT$(PC_PRICE::PRICECOST, "#,###,###.#####")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	TEST_PRINTING = PC_PRICE::PRODUCT_NUM + PC_PRICE::LOCATION
	PRINT_LINE% = -1%

	GOTO ExitProgram IF UTL_REPORTX::STAT

17350	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
17400	!
	! Handle end of report
	!

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
