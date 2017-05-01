1	%TITLE "Product Structure List"
	%SBTTL "BM_RPRT_RELATION"
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
	!	$ BAS BM_SOURCE:BM_RPRT_RELATION/LINE
	!	$ LINK/EXECUTABLE=BM_EXE: BM_RPRT_RELATION, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE BM_RPRT_RELATION.OBJ;*
	!
	! Author:
	!
	!	09/10/88 - Frank F. Starman
	!
	! Modification History:
	!
	!	03/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/26/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to "" in several places.
	!
	!	05/13/97 - Kevin Handy
	!		Reformat source code.
	!		Use integer for #key
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/05/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Fix TO_ITEM bug when sorting by component
	!
	!	10/30/2000 - Kevin Handy
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
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.HB"
	MAP	(BM_RELATION)	BM_RELATION_CDD	BM_RELATION

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
	!	The ^*From Item\* field
	!	begins the report with a selected
	!	product number by entering the selection in this field.  The value entered
	!	must be in agreement with the value in field (10) Sort by.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Product Structure List
	!	.x Product Structure List>From Item
	!	.x Item>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field
	!	selects a specific product with which
	!	the report will end by entering the selection in this field.  The value
	!	entered must be in agreement with the value in field (10) Sort by.
	!	.lm -5
	!
	! Index:
	!	.x Product Structure List>To Item
	!	.x To Item>Product Structure List
	!	.x Item>To
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)
	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field
	!	selects certain products to be printed
	!	using Wildcarding techniques.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Product Structure List
	!	.x Product Structure List>Wildcard
	!
	!--

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(9%), 132%)
	!++
	! Abstract:FLD10
	!	^*(10) Sort\*
	!	.b
	!	.lm +5
	!	The ^*Sort (P,C)\* field
	!	enters a flag which determines if the
	!	report will print by product number, or showing the components in each
	!	product by component number, showing the products in which
	!	each component is used.
	!	.b
	!	This field requires an entry and will accommodate one (1)
	!	alphabetic character only. Valid entries are:
	!	.table 3,25
	!	.te
	!	^*P\* = Sort by Product
	!	.te
	!	^*C\* = Sort by Component
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Sort>Product Structure List
	!	.x Product Structure List>Sort
	!
	!--
	SELECT SORT_BY$
	CASE "C"
		SORT_KEY% = 1%
		ADD_TITLE$ = "BY  COMPONENT"

	CASE "P"
		SORT_KEY% = 0%
		ADD_TITLE$ = "BY  PRODUCT  NUMBER"
	END SELECT

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.OPN"
	USE
		FILENAME$ = "BM_RELATION"
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
	TITLE$(1%) = "PRODUCT  STRUCTURE  LIST  " + ADD_TITLE$
	TITLE$(2%) = "Bill of Material System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Product#       Description               " + &
		"    Item   Component#     Description         " + &
		"           Quantity Operation   Scrap(%)"
	TITLE$(5%) = "."

	LYT_LINE$ = "$Product#:015,$Description:045,$Item:052," + &
		"$Component#:067,$Description:098," + &
		"VQuantity:107,$Operation:119,VScrap(%):131"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #BM_RELATION.CH%, KEY #SORT_KEY%
		ELSE
			FIND #BM_RELATION.CH%, &
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
		GET #BM_RELATION.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORT_BY$
	CASE "P"
		GOTO ExitTotal IF (BM_RELATION::PRODUCT > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec &
				IF COMP_ARRAY(EDIT$(BM_RELATION::PRODUCT, -1%), &
				WLDCRD$) = 0%
		END IF

	CASE "C"
		GOTO ExitTotal IF (BM_RELATION::COMPONENT > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF (BM_RELATION::COMPONENT < FROM_ITEM$) &
			AND FROM_ITEM$ <> ""

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec &
				IF COMP_ARRAY(EDIT$(BM_RELATION::COMPONENT, -1%), &
				WLDCRD$) = 0%
		END IF
	END SELECT

17200	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, &
			KEY #0% EQ BM_RELATION::PRODUCT, &
			REGARDLESS
		PD_PRODUCT_PRODUCT$ = PD_PRODUCT::DESCRIPTION
	USE
		PD_PRODUCT_PRODUCT$ = &
			STRING$(LEN(PD_PRODUCT::DESCRIPTION), A"?"B)

		CONTINUE 17210 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

17210	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, &
			KEY #0% EQ BM_RELATION::COMPONENT, &
			REGARDLESS
	USE
		PD_PRODUCT::DESCRIPTION = &
			STRING$(LEN(PD_PRODUCT::DESCRIPTION), A"?"B)

		CONTINUE 17300 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

17300	!
	! Print out one line
	!
	SELECT SORT_BY$
	CASE "C"
		IF TEST_PRODUCT$ <> ""
		THEN
			IF TEST_PRODUCT$ = BM_RELATION::COMPONENT
			THEN
				PD_PRODUCT::DESCRIPTION = ""
			END IF
		END IF
		TEST_PRODUCT$  = BM_RELATION::COMPONENT

	CASE "P"
		IF TEST_PRODUCT$ <> ""
		THEN
			IF TEST_PRODUCT$ = BM_RELATION::PRODUCT
			THEN
				PD_PRODUCT_PRODUCT$ = ""
			END IF
		END IF
		TEST_PRODUCT$  = BM_RELATION::PRODUCT
	END SELECT

	TEXT$ =  BM_RELATION::PRODUCT + " " + &
		LEFT(PD_PRODUCT_PRODUCT$, 28%) + "  " + &
		BM_RELATION::ITEMNUM + "   " + &
		BM_RELATION::COMPONENT + " " + &
		LEFT(PD_PRODUCT::DESCRIPTION, 28%) + &
		FORMAT$(BM_RELATION::QUANTITY, "###,###.### ") + &
		BM_RELATION::OPERATION + "     " + &
		FORMAT$(BM_RELATION::SCRAP / 100.0, "<%>##.##")

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

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
