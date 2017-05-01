1	%TITLE "Product Standard Cost File List"
	%SBTTL "PC_RPRT_COST"
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
	! ID:PC013
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	Accessing the ^*Product Standard Cost File\* option will
	!	provide a report which will include the following information:
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
	!	Unit of Measure
	!	.te
	!	Location
	!	.te
	!	Date
	!	.te
	!	Standard Cost
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Product Standard Cost File>Report
	!	.x Report>Product Standard Cost File
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PC_SOURCE:PC_RPRT_COST/LINE
	!	$ LINK/EXE=PC_EXE: PC_RPRT_COST, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PC_RPRT_COST.OBJ;*
	!
	! Author:
	!
	!	06/25/88 - Frank Starman
	!
	! Modification History:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/27/96 - Kevin Handy
	!		Reformat source code.
	!		Changed STRING$(...,ASCII(" ")) to "" in several places.
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code.
	!		Use integer for #key
	!
	!	04/17/98 - Kevin Handy
	!		Added from/to date fields.
	!		Changed heading from 'date' to 'eff date'
	!		Added 'previous cost' coilumn
	!
	!	04/20/98 - Kevin Handy
	!		Add Previous Date to Previous
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/01/2000 - Kevin Handy
	!		Use A"x"B
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
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[PC.OPEN]PC_COST.HB"
	MAP	(PC_COST)	PC_COST_CDD	PC_COST

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
	!	^*(01) From Product\*
	!	.b
	!	.lm +5
	!	The ^*From Product\* field
	!	begins the list with a selected product
	!	number code by entering that selection in this field.
	!	.b
	!	If this field is blank, the list will begin with the
	!	first record in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Product>Standard Cost File
	!	.x Standard Cost File>From Product
	!	.x Product>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Product\*
	!	.b
	!	.lm +5
	!	The ^*To Product\* field
	!	concludes the list with a selected
	!	product number code.
	!	.b
	!	If this field is blank the list will end with the last
	!	record in the file.
	!
	! Index:
	!	.x To Product>Standard Cost File
	!	.x Standard Cost File>To Product
	!	.x Product>To
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
	!	codes to be printed on the list by entering a "product code
	!	wildcard" value.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Standard Cost File
	!	.x Standard Cost File>Wildcard
	!
	!--

	FROM_DATE$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(5%))

	!++
	! Abstract:FLD06
	!	^*(06) From Date\*
	!	.b
	!	.lm +5
	!	This fiels is used to select only those costs that
	!	have been changed since a specific date.
	!	.p
	!	Leave this field blank to select from the beginning
	!	of the file
	!	.lm -5
	!
	! Index:
	!	.x From Date>Standard Cost File
	!	.x Standard Cost File>From date
	!
	!--

	TO_DATE$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(6%))

	!++
	! Abstract:FLD07
	!	^*(07) To Date\*
	!	.b
	!	.lm +5
	!	This fiels is used to select only those costs that
	!	have been changed up to a specific date.
	!	.p
	!	Leave this field blank to select to the end
	!	of the file
	!	.lm -5
	!
	! Index:
	!	.x To Date>Standard Cost File
	!	.x Standard Cost File>To date
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[PC.OPEN]PC_COST.OPN"
	USE
		FILENAME$ = "PC_COST"
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
	TITLE$(1%) = "PRODUCT  STANDARD  COST  FILE  LIST"
	TITLE$(2%) = "Product Price & Cost System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Product#       Description              " + &
		"                Tp Cat  UOM Loc   " + &
		"Eff Date           StdCost       Previous"
	TITLE$(5%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PC_COST.CH%
		ELSE
			FIND #PC_COST.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to find beginning record!", 0%)
		CONTINUE ExitProgram
	END WHEN

	PREVIOUS$ = ""
	TEST_PREV$ = ""

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PC_COST.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PC_COST"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (PC_COST::PRODUCT > TO_ITEM$) AND &
		TO_ITEM$ <> ""

	GOTO GetNextRec IF WLDCRD$ <> "" AND &
		COMP_STRING(EDIT$(PC_COST::PRODUCT, -1%), WLDCRD$) = 0%

	IF (TEST_PREV$ <> PC_COST::PRODUCT)
	THEN
		PREVIOUS$ = ""
		TEST_PREV$ = PC_COST::PRODUCT
	END IF

	IF (PC_COST::EFFDATE < FROM_DATE$) OR &
		(PC_COST::EFFDATE > TO_DATE$ AND TO_DATE$ > "00000000")
	THEN
		PREVIOUS$ = PRNT_DATE(PC_COST::EFFDATE, 8%) + " " + &
			FORMAT$(PC_COST::COST, "#,###,###.#####")
		GOTO GetNextRec
	END IF

17200	IF TEST_PRINTING = PC_COST::PRODUCT + PC_COST::LOCATION
	THEN
		PD_PRODUCT::DESCRIPTION = ""
		PD_PRODUCT::PROD_TYPE = ""
		PD_PRODUCT::CATEGORY = ""
		PD_PRODUCT::UOM = ""
	ELSE
		WHEN ERROR IN
			GET #PD_PRODUCT.CH%, &
				KEY #0% EQ PC_COST::PRODUCT, &
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
	TEXT$ = PC_COST::PRODUCT + " " + &
		PD_PRODUCT::DESCRIPTION + " " + &
		PD_PRODUCT::PROD_TYPE + " " + &
		PD_PRODUCT::CATEGORY + " " + &
		PD_PRODUCT::UOM + "  " + &
		PC_COST::LOCATION + "  " + &
		PRNT_DATE(PC_COST::EFFDATE, 8%) + " " + &
		FORMAT$(PC_COST::COST, "#,###,###.#####  ") + &
		PREVIOUS$

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	TEST_PRINTING = PC_COST::PRODUCT + PC_COST::LOCATION
	PREVIOUS$ = PRNT_DATE(PC_COST::EFFDATE, 8%) + " " + &
		FORMAT$(PC_COST::COST, "#,###,###.#####")

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
