1	%TITLE "Product Description List"
	%SBTTL "PD_RPRT_SUBSTITUTE"
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
	!	$ BAS PD_SOURCE:PD_RPRT_SUBSTITUTE/LINE
	!	$ LINK/EXE=PD_EXE: PD_RPRT_SUBSTITUTE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PD_RPRT_SUBSTITUTE.OBJ;*
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

	%INCLUDE "SOURCE:[PD.OPEN]PD_SUBSTITUTE.HB"
	MAP (PD_SUBSTITUTE) PD_SUBSTITUTE_CDD PD_SUBSTITUTE

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

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_SUBSTITUTE.OPN"
	USE
		FILENAME$ = "PD_SUBSTITUTE"
		CONTINUE HelpError
	END WHEN


 ReportTitle:
	!
	! Title
	!
	SELECT SORT_BY$

	CASE "P"
		SORT_KEY% = 0%
		TITLE$(1%) = "SUBSTITUTES BY PRODUCT NUMBER"

	CASE "T"
		SORT_KEY% = 1%
		TITLE$(1%) = "SUBSTITUTES BY VENDOR PRODUCT"

	CASE "V"
		SORT_KEY% = 2%
		TITLE$(1%) = "SUBSTITUTES BY PRODUCT VENDOR"

	END SELECT

	TITLE$(2%) = ""

	!
	! Heading
	!
	TITLE$(3%) = "Product#       Description                    " + &
		"         Vendor     Vendor ID"

	TITLE$(4%) = "."
	TITLE$(5%) = ""

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PD_SUBSTITUTE.CH%, KEY #SORT_KEY%
		ELSE
			FIND #PD_SUBSTITUTE.CH%, &
				KEY #SORT_KEY% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "PD_SUBSTITUTE"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		GET #PD_SUBSTITUTE.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "PD_SUBSTITUTE"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORT_BY$

	CASE "P"
		GOTO ExitProgram IF (PD_SUBSTITUTE::OUR_PRODUCT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_SUBSTITUTE::OUR_PRODUCT, -1%), &
			WLDCRD$) = 0%

	CASE "T"
		GOTO ExitProgram IF (PD_SUBSTITUTE::THEIR_PRODUCT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_SUBSTITUTE::THEIR_PRODUCT, -1%), &
			WLDCRD$) = 0%

	CASE "V"
		GOTO ExitProgram IF (PD_SUBSTITUTE::VENDOR > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_SUBSTITUTE::VENDOR, -1%), &
			WLDCRD$) = 0%

	END SELECT

	!
	! Scan for BIN Numbers in any cycle count map
	!
	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, &
			KEY #0% EQ PD_SUBSTITUTE::OUR_PRODUCT, &
			REGARDLESS
	USE
		PD_PRODUCT::DESCRIPTION = "????"
		CONTINUE 17025
	END WHEN

17025	!
	! Output line
	!
	TEXT$ = PD_SUBSTITUTE::OUR_PRODUCT + " " + &
		PD_PRODUCT::DESCRIPTION + " " + &
		PD_SUBSTITUTE::VENDOR + " " + &
		PD_SUBSTITUTE::THEIR_PRODUCT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
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
