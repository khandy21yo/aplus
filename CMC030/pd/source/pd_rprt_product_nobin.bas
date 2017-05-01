1	%TITLE "Product Description List"
	%SBTTL "PD_RPRT_PRODUCT_NOBIN"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1999 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
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
	!	$ BAS PD_SOURCE:PD_RPRT_PRODUCT_NOBIN/LINE
	!	$ LINK/EXE=PD_EXE: PD_RPRT_PRODUCT_NOBIN, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PD_RPRT_PRODUCT_NOBIN.OBJ;*
	!
	! Author:
	!
	!	10/21/99 - Kevin Handy
	!
	! Modification History:
	!
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

	%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.HB"
	MAP (IC_BINMAP) IC_BINMAP_CDD IC_BINMAP

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

	EXCLUDE$ = EDIT$(UTL_REPORTX::OPTDEF(6%), -1%)

	!++
	! Abstract:FLD07
	!	^*(07) Exclude Types\*
	!	.b
	!	.lm +5
	!	Specifies which Product Types to exclude.
	!	.lm -5
	!
	! Index:
	!	.x Product Description List>Form Type
	!
	!--

	LOCATION$ = LEFT$(UTL_REPORTX::OPTDEF(7%), 4%)

	!++
	! Abstract:FLD08
	!	^*(08) Location\*
	!	.b
	!	.lm +5
	!	Specifies which Location to use.
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

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.OPN"
	USE
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN


 ReportTitle:
	!
	! Title
	!
	SELECT SORT_BY$

	CASE "P"
		SORT_KEY% = 0%
		TITLE$(1%) = "MISSING BINMAP LIST BY PRODUCT NUMBER"

	CASE "T"
		SORT_KEY% = 1%
		TITLE$(1%) = "MISSING BINMAP LIST BY PRODUCT TYPE"

	CASE "C"
		SORT_KEY% = 2%
		TITLE$(1%) = "MISSING BINMAP LIST BY PRODUCT CATEGORY"

	CASE "D"
		SORT_KEY% = 3%
		TITLE$(1%) = "MISSING BINMAP LIST BY PRODUCT DESCRIPTION"

	CASE "S"
		SORT_KEY% = 4%
		TITLE$(1%) = "MISSING BINMAP LIST BY SECONDARY CODE"

	END SELECT

	TITLE$(2%) = "FOR LOCATION " + LOCATION$
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Product#       Description                    " + &
		"          Type Cat  SecCode    UOM Label Meth"  + &
		" ST OnSetDate StatDate"

	IF FORMTYPE$ = "L"
	THEN
		TITLE$(5%) = SPACE$(60%) + "ManufUOM     Weight ProdFactor"

		TITLE$(6%) = "."
		TITLE$(7%) = ""
	ELSE
		TITLE$(5%) = "."
		TITLE$(6%) = ""
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

	!
	! Only do actives
	!
	GOTO 17020 IF PD_PRODUCT::SSTATUS <> "A"

	!
	! Check the category
	!
	IF (EXCLUDE$ <> "")
	THEN
		GOTO 17020 IF COMP_STRING(PD_PRODUCT::PROD_TYPE, EXCLUDE$) <> 0%
	END IF

	!
	! Check the category
	!
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

	!
	! Scan for BIN Numbers in any cycle count map
	!
	WHEN ERROR IN
		GET #IC_BINMAP.CH%, &
			KEY #0% EQ PD_PRODUCT::PRODUCT_NUM + LOCATION$, &
			REGARDLESS
	USE
		CONTINUE 17025
	END WHEN

 !	WHILE IC_BINMAP::PRODUCT = PD_PRODUCT::PRODUCT_NUM

		GOTO 17020 IF IC_BINMAP::BIN(I%) <> "      " &
			FOR I% = 0% TO 3%

 !		WHEN ERROR IN
 !			GET #IC_BINMAP.CH%, &
 !				REGARDLESS
 !		USE
 !			CONTINUE 17025
 !		END WHEN
 !
 !	NEXT

17025	!
	! Output line
	!
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
	RESUME HelpError

32767	END
