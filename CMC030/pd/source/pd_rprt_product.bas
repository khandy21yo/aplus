1	%TITLE "Product Description List"
	%SBTTL "PD_RPRT_PRODUCT"
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
	! ID:PD005
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Product Description\* option
	!	accesses the print routine which will print a list of
	!	product descriptions.
	!	.b
	!	The following fields are included:
	!	.table 3,25
	!	.te
	!	Product
	!	.te
	!	Product Description
	!	.te
	!	Product Type
	!	.te
	!	Product Category
	!	.te
	!	Unit of Measure
	!	.te
	!	Label Code
	!	.te
	!	Costing Method
	!	.te
	!	Onset Date
	!	.te
	!	End Date
	!	.te
	!	Secondary Code
	!	.te
	!	Weight
	!	.te
	!	Manufacturer UOM
	!	.te
	!	Product Factor
	!	.end table
	!	The list may be printed in product code or type code order.
	!
	! Index:
	!	.x Reports>Product Description List
	!	.x Product Description>List
	!
	! Compile:
	!
	!	$ BAS PD_SOURCE:PD_RPRT_PRODUCT/LINE
	!	$ LINK/EXE=PD_EXE: PD_RPRT_PRODUCT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PD_RPRT_PRODUCT.OBJ;*
	!
	! Author:
	!
	!	07/17/87 - Frank F. Starman
	!
	! Modification History:
	!
	!	08/27/91 - Deborah K. Fries
	!		Added more fields on the-report they are
	!		Secondary Code (SecCode), Status (Stat),
	!		Beginning Date (OnSetDate). Include a Function to print
	!		the date in the proper way MM/DD/YYYY.
	!
	!	01/20/92 - Dan Perkins
	!		Added option to print long or short report.
	!		Changed fields to accomodate changes in
	!		file layout.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	05/01/92 - Frank F. Starman
	!		Allow to print inactive product.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	11/13/95 - Kevin Handy
	!		Modified source closer to 80 columns.
	!
	!	09/09/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/21/99 - Kevin Handy
	!		Add wildcard category field
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
	!	The ^*From Item\* field begins the list
	!	printing with a selected product code.
	!	.b
	!	In order to be operable, field (10) must be ^*P\*
	!	for Product.
	!	.b
	!	A blank field will cause the list to begin with the first record
	!	in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Product Description List
	!	.x Product Description List>From Item
	!	.x Item>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field concludes
	!	the list printing with a selected
	!	product code.
	!	.b
	!	In order to be operable, field (10) must be ^*P\* for
	!	Product.
	!	.b
	!	A blank field will cause the report to end with the last
	!	record in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Product Description List
	!	.x Product Description List>To Item
	!	.x Item>To
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
	!	.x Wildcard>Product Description List
	!	.x Product Description List>Wildcard
	!
	!--

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	.x Sort>Product Description List
	!	^*(04) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort (P,T,C,D,S)\* field
	!	prints the list in a particular order.
	!	.b
	!	Valid settings are:
	!	.table 3,15
	!	.te
	!	^*P\*	- Product Number
	!	.te
	!	^*T\*	- Product Type
	!	.te
	!	^*C\*	- Product Category
	!	.te
	!	^*D\*	- Product Description
	!	.te
	!	^*S\*	- Product Secondary Code
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Product Description List>Sort
	!
	!--

	FORMTYPE$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	.x Form Type>Product Description List
	!	^*(05) Form Type\*
	!	.b
	!	.lm +5
	!	The ^*Form Type (L,S)\* field determines
	!	whether the Product Report will be printed in "long" or
	!	"short" form.
	!	.b
	!	Valid values are:
	!	.table 3,25
	!	.te
	!	^*L\*	- Long form
	!	.te
	!	^*S\*	- Short form
	!	.end table
	!	If the long form is chosen, the report will also print the
	!	Manufacturers Unit of Measure, Product Weight, and Product
	!	Factor which is the product units in one pack.
	!	.lm -5
	!
	! Index:
	!	.x Product Description List>Form Type
	!
	!--

	CATEGORY$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Wildcard Category\*
	!	.b
	!	.lm +5
	!	Specifies which categories to include.
	!	.lm -5
	!
	! Index:
	!	.x Product Description List>Form Type
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	SELECT SORT_BY$

	CASE "P"
		SORT_KEY% = 0%
		TITLE$(1%) = "PRODUCT DESCRIPTION LIST BY PRODUCT NUMBER"

	CASE "T"
		SORT_KEY% = 1%
		TITLE$(1%) = "PRODUCT DESCRIPTION LIST BY PRODUCT TYPE"

	CASE "C"
		SORT_KEY% = 2%
		TITLE$(1%) = "PRODUCT DESCRIPTION LIST BY PRODUCT CATEGORY"

	CASE "D"
		SORT_KEY% = 3%
		TITLE$(1%) = "PRODUCT DESCRIPTION LIST BY PRODUCT DESCRIPTION"

	CASE "S"
		SORT_KEY% = 4%
		TITLE$(1%) = "PRODUCT DESCRIPTION LIST BY SECONDARY CODE"

	END SELECT

	TITLE$(2%) = ""

	!
	! Heading
	!
	TITLE$(3%) = "Product#       Description                    " + &
		"          Type Cat  SecCode    UOM Label Meth"  + &
		" ST OnSetDate StatDate"

	IF FORMTYPE$ = "L"
	THEN
		TITLE$(4%) = SPACE$(60%) + "ManufUOM     Weight ProdFactor"

		TITLE$(5%) = "."
	ELSE
		TITLE$(4%) = "."
	END IF

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

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
17020	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	IF (CATEGORY$ <> "")
	THEN
		GOTO 17020 IF COMP_STRING(PD_PRODUCT::CATEGORY, CATEGORY$) = 0%
	END IF

	!
	! Check current record
	!
	SELECT SORT_BY$

	CASE "P"
		GOTO ExitProgram IF (PD_PRODUCT::PRODUCT_NUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::PRODUCT_NUM, -1%), &
			WLDCRD$) = 0%

	CASE "T"
		GOTO ExitProgram IF (PD_PRODUCT::PROD_TYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::PROD_TYPE, -1%), &
			WLDCRD$) = 0%

	CASE "C"
		GOTO ExitProgram IF (PD_PRODUCT::CATEGORY > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::CATEGORY, -1%), &
			WLDCRD$) = 0%

	CASE "D"
		GOTO ExitProgram IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::DESCRIPTION, -1%), &
			WLDCRD$) = 0%

	CASE "S"
		GOTO ExitProgram &
			IF (PD_PRODUCT::SECONDARY_CODE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::SECONDARY_CODE, -1%), &
			WLDCRD$) = 0%

	END SELECT

	TEXT$ = PD_PRODUCT::PRODUCT_NUM + " " + &
		PD_PRODUCT::DESCRIPTION + " " + &
		PD_PRODUCT::PROD_TYPE + "   " + &
		PD_PRODUCT::CATEGORY + " " + &
		PD_PRODUCT::SECONDARY_CODE + " " + &
		PD_PRODUCT::UOM + "  "  + &
		PD_PRODUCT::LABEL + "  " + &
		PD_PRODUCT::METHOD + " " + &
		PD_PRODUCT::SSTATUS + "  " + &
		PRNT_DATE(PD_PRODUCT::BDATE, 6%) + "  "  + &
		PRNT_DATE(PD_PRODUCT::EDATE, 6%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	IF FORMTYPE$ = "L"
	THEN
		TEXT$ = SPACE$(60%) + PD_PRODUCT::BOMUOM + "       " + &
			FORMAT$(PD_PRODUCT::WEIGHT, "###,###.##") + "    " + &
			FORMAT$(PD_PRODUCT::PRODUCT_FACTOR, "####.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO GetNextRec

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
