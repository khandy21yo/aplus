1	%TITLE "Purchase Order Product Description List"
	%SBTTL "PO_RPRT_PRODUCT"
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
	! ID:PO010
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Product Description\* option
	!	accesses the print routine which
	!	lists the product description file and the associated vendors
	!	from whom the products can be purchased.
	!	.b
	!	Column headings for the product information are:
	!	.table 3,25
	!	.te
	!	Product _#
	!	.te
	!	Description
	!	.te
	!	Type
	!	.te
	!	Category
	!	.te
	!	Secondary Code
	!	.te
	!	Unit of Measure
	!	.te
	!	Label Code
	!	.te
	!	Method
	!	.end table
	!	Column headings for the vendor information are:
	!	.table 3,25
	!	.te
	!	Vendor number
	!	.te
	!	Vendor Product Number
	!	.te
	!	Vendor Product Description
	!	.te
	!	Vendor's Unit of measure
	!	.te
	!	Vendor's conversion factor
	!	.te
	!	Our conversion factor
	!	.te
	!	Lead time
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Reports>Purchase Order Vendor Product List
	!	.x Products by Vendor>List
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_RPRT_PRODUCT/LINE
	!	$ LINK/EXE=PO_EXE: PO_RPRT_PRODUCT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PO_RPRT_PRODUCT.OBJ;*
	!
	! Author:
	!
	!	06/01/90 - Aaron Redd
	!
	! Modification History:
	!
	!	10/08/91 - Deborah K. Fries
	!		Cleaned source code
	!		Improved error trapping
	!
	!	03/09/92 - Dan Perkins
	!		Added more sort key selection to sort field.
	!		Removed PACK field which is no longer used.
	!
	!	03/12/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	07/01/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/16/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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
	MAP	(PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[PO.OPEN]PO_PARTCROSS.HB"
	MAP	(PO_PARTCROSS)	PO_PARTCROSS_CDD	PO_PARTCROSS

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Init:	!
	! Initialize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field
	!	begins the list with a selected product
	!	code by entering that selection in this field.
	!	.b
	!	In order to be operable, (10) must be ^*P\*
	!	for Product.
	!	.b
	!	If this field is blank, the list will begin with the first record
	!	in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field
	!	concludes the list with a selected
	!	product code.
	!	.b
	!	In order to be operable, field (10) must be ^*P\*
	!	for Product.
	!	.b
	!	If this field is blank, the list will conclude with the last
	!	record in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	This ^*Wildcard\* field
	!	selects designated product description
	!	codes to be printed on the list by entering a "product code wildcard"
	!	value in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(9%), 132%)

	!++
	! Abstract:FLD10
	!	.ts 55
	!	^*(10) Sort	P,T,C,D,S\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field
	!	determines the sorting
	!	order in which the report will print.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*P\* - Product Number
	!	.te
	!	^*T\* - Product Type
	!	.te
	!	^*C\* - Product Category
	!	.te
	!	^*D\* - Product Description
	!	.te
	!	^*S\* - Product Secondary Code
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	!
	! Open Product description file
	!
300	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Open the Vendor Part Cross Reference file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_PARTCROSS.OPN"
	USE
		FILENAME$ = "PO_PARTCROSS"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	SELECT SORT_BY$

	CASE "P"
		SORT_KEY% = 0%
		ADD_TITLE$ = "BY  PRODUCT  NUMBER"

	CASE "T"
		SORT_KEY% = 1%
		ADD_TITLE$ = "BY  PRODUCT  TYPE"

	CASE "C"
		SORT_KEY% = 2%
		ADD_TITLE$ = "BY  CATEGORY"

	CASE "D"
		SORT_KEY% = 3%
		ADD_TITLE$ = "BY  DESCRIPTION"

	CASE "S"
		SORT_KEY% = 4%
		ADD_TITLE$ = "BY  SECONDARY  CODE"

	END SELECT

	TITLE$(1%) = "VENDOR  PART  CROSS  REFERENCE  LIST  SORTED  " + ADD_TITLE$
	TITLE$(2%) = "Purchase Order System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "ProductNum      ProductDescription           " + &
		"             Type  Categ  SecCode     UOM  Label  Method"

	TITLE$(5%) = "                  VendorNum   VendorProdNum   " + &
		"VendorProdDescr                 VenUOM  "       + &
		"VenConvFactor  OurConvFactor  LeadTime"

	TITLE$(6%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	PRINT_FLAG% = 0%

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PD_PRODUCT.CH%, KEY #SORT_KEY%
		ELSE
			FIND #PD_PRODUCT.CH%, &
				KEY #SORT_KEY% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%) IF PRINT_FLAG%

	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORT_BY$

	CASE "C"
		GOTO ExitProgram IF (PD_PRODUCT::CATEGORY > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PD_PRODUCT::CATEGORY, -1%), &
			WLDCRD$) = 0%

	CASE "D"
		GOTO ExitProgram IF (PD_PRODUCT::DESCRIPTION > &
			TO_ITEM$) AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PD_PRODUCT::DESCRIPTION, -1%), &
			WLDCRD$) = 0%

	CASE "P"
		GOTO ExitProgram IF (PD_PRODUCT::PRODUCT_NUM > &
			TO_ITEM$) AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PD_PRODUCT::PRODUCT_NUM, -1%), &
			WLDCRD$) = 0%

	CASE "S"
		GOTO ExitProgram IF (PD_PRODUCT::SECONDARY_CODE > &
			TO_ITEM$) AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PD_PRODUCT::SECONDARY_CODE, -1%), &
			WLDCRD$) = 0%

	CASE "T"
		GOTO ExitProgram IF (PD_PRODUCT::PROD_TYPE> &
			TO_ITEM$) AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PD_PRODUCT::PROD_TYPE, -1%), &
			WLDCRD$) = 0%
	END SELECT

	!
	! Print out one line of the Product description file
	!
	TEXT$ = PD_PRODUCT::PRODUCT_NUM + "  " + &
		PD_PRODUCT::DESCRIPTION + "  " + &
		PD_PRODUCT::PROD_TYPE + "    " + &
		PD_PRODUCT::CATEGORY + "   "  + &
		PD_PRODUCT::SECONDARY_CODE + "  " + &
		PD_PRODUCT::UOM + "   " + &
		PD_PRODUCT::LABEL + "   " + &
		PD_PRODUCT::METHOD

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	PRINT_FLAG% = -1%

	!
	! FIND the first of any associated vendors
	!
17310	WHEN ERROR IN
		FIND #PO_PARTCROSS.CH%, &
			KEY #0% GE PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155% OR ERR = 11%
		FILENAME$ = "PO_PARTCROSS"
		CONTINUE HelpError
	END WHEN

 PartRec:
	WHEN ERROR IN
		GET #PO_PARTCROSS.CH%, REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155% OR ERR = 11%
		FILENAME$ = "PO_PARTCROSS"
		CONTINUE HelpError
	END WHEN

	!
	! Are we still on the right product?
	!
	GOTO GetNextRec IF PO_PARTCROSS::PRODUCT <> PD_PRODUCT::PRODUCT_NUM

	TEXT$ = PD_PRODUCT::PRODUCT_NUM + "    " + &
		PO_PARTCROSS::VENDOR + "  " + &
		PO_PARTCROSS::VENPROD + "  " + &
		LEFT(PO_PARTCROSS::DESCR, 30%) + "  " + &
		PO_PARTCROSS::VENUOM + "           " + &
		FORMAT$(PO_PARTCROSS::VENFAC, "####.###") + "       " + &
		FORMAT$(PO_PARTCROSS::FACTOR, "####.###") + "      " + &
		FORMAT$(PO_PARTCROSS::LEAD, "####")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO PartRec

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
