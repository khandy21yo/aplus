1	%TITLE "Product Structure List"
	%SBTTL "BM_RPRT_PRODOPER"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2000 BY
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
	! ID:BM004
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program prints out the Product Structure List
	!	in the Bill of Materials System.  The list includes the following fields:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Product Number
	!	.le
	!	Product Description
	!	.le
	!	Item
	!	.le
	!	Component Number
	!	.le
	!	Component Description
	!	.le
	!	Quantity
	!	.le
	!	Operation
	!	.le
	!	Scrap Percentage
	!	.els
	!	.lm -5
	!
	! Index:
	!	.x Product Structure List
	!	.x List>Product Structure
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS BM_SOURCE:BM_RPRT_PRODOPER/LINE
	!	$ LINK/EXECUTABLE=BM_EXE: BM_RPRT_PRODOPER, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE BM_RPRT_PRODOPER.OBJ;*
	!
	! Author:
	!
	!	06/19/2000 - Kevin Handy
	!
	! Modification History:
	!
	!	07/13/2000 - Kevin Handy
	!		Added wildcard product number, so you can sort by
	!		name but still limit it to a specified range. (LL)
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.HB"
	MAP	(BM_PRODOPER)	BM_PRODOPER_CDD	BM_PRODOPER

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP	(PD_PRODUCT)	PD_PRODUCT_CDD	PD_PRODUCT

	!
	! External functions
	!
	EXTERNAL INTEGER FUNCTION COMP_ARRAY

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
	!	printing with a selected product code by entering
	!	that selection in this field.
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
	!	product code by entering the selected code in this field.
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

	WILD_PROD$ = EDIT$(UTL_REPORTX::OPTDEF(6%), -1%)

	!++
	! Abstract:FLD07
	!	^*(06) Wildcard Product\*
	!	.b
	!	.lm +5
	!	Specifies which products to include.
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
		%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.OPN"
	USE
		FILENAME$ = "BM_PRODOPER"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	SELECT SORT_BY$

	CASE "P"
		SORT_KEY% = 0%
		TITLE$(1%) = "PRODUCT OPERATION LIST BY PRODUCT NUMBER"

	CASE "T"
		SORT_KEY% = 1%
		TITLE$(1%) = "PRODUCT OPERATION LIST BY PRODUCT TYPE"

	CASE "C"
		SORT_KEY% = 2%
		TITLE$(1%) = "PRODUCT OPERATION LIST BY PRODUCT CATEGORY"

	CASE "D"
		SORT_KEY% = 3%
		TITLE$(1%) = "PRODUCT OPERATION LIST BY PRODUCT DESCRIPTION"

	CASE "S"
		SORT_KEY% = 4%
		TITLE$(1%) = "PRODUCT OPERATION LIST BY SECONDARY CODE"

	END SELECT

	TITLE$(2%) = ""

	!
	! Heading
	!
	TITLE$(3%) = "Product#       Description                    " + &
		"          Type Cat  Line Oper     Oper Date  CompSbttl" + &
		"  LevelHrs  TotalHrs"
	TITLE$(4%) = "."
	TITLE$(5%) = ""

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

	IF (WILD_PROD$ <> "")
	THEN
		GOTO 17020 &
			IF COMP_STRING(PD_PRODUCT::PRODUCT_NUM, WILD_PROD$) = 0%
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

	WHEN ERROR IN
		FIND #BM_PRODOPER.CH%, &
			KEY #0% EQ PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		CONTINUE GetNextRec
	END WHEN

 InnerLoop:

	WHEN ERROR IN
		GET #BM_PRODOPER.CH%, &
			REGARDLESS
	USE
		CONTINUE GetNextRec
	END WHEN

	GOTO GetNextRec IF PD_PRODUCT::PRODUCT_NUM <> BM_PRODOPER::PRODUCT

	TEXT$ = PD_PRODUCT::PRODUCT_NUM + " " + &
		PD_PRODUCT::DESCRIPTION + " " + &
		PD_PRODUCT::PROD_TYPE + "   " + &
		PD_PRODUCT::CATEGORY + " " + &
		BM_PRODOPER::ITEMNUM + " " + &
		BM_PRODOPER::OPERATION + " " + &
		PRNT_DATE(BM_PRODOPER::EFFDATE, 8%) + " " + &
		FORMAT$(BM_PRODOPER::HOURS - BM_PRODOPER::THISHOURS, &
			"######.## ") + &
		FORMAT$(BM_PRODOPER::THISHOURS, "######.## ") + &
		FORMAT$(BM_PRODOPER::HOURS, "######.## ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO InnerLoop

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
