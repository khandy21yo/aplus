1	%TITLE "Copy Labor through All Levels"
	%SBTTL "BM_SPEC_COPYLABOR"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1995 BY
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
	! ID:BM002
	!
	! Abstract:HELP
	!	.B
	!	.LM +5
	!	This program  will copy labor through all levels of
	!	a Bill of Material for a specified base range.
	!	.LM -5
	!
	! Index:
	!	.x Copy Labor
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS BM_SOURCE:BM_SPEC_COPYLABOR/LINE
	!	$ LINK/EXE=BM_EXE: BM_SPEC_COPYLABOR, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE BM_SPEC_COPYLABOR.OBJ;*
	!
	! Author:
	!
	!	02/01/95 - Kevin Handy
	!
	! Modification History:
	!
	!	02/14/95 - Kevin Handy
	!		Fix bug looking for labor to roll up to.
	!		When copying up a new bit of laboe, don't copy
	!		up the THISHOURS field.
	!		Fixed so doesn't overdup new labor records.
	!
	!	03/07/95 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/21/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	01/28/99 - Kevin Handy
	!		Added an abort when there are just too many
	!		levels seen (to prevent recursive definitions from
	!		killing the program)
	!
	!	09/26/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.HB"
	MAP (BM_RELATION)	BM_RELATION_CDD		BM_RELATION

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.HB"
	MAP (BM_PRODOPER)	BM_PRODOPER_CDD		BM_PRODOPER

	COM (BM_RELATION_CH) &
		BM_RELATION.CH%

	COM (BM_PRODOPER_CH) &
		BM_PRODOPER.CH%

	RECORD SUMMARY_LEVEL
		INTEGER		TOTAL_OPER
		BM_PRODOPER_CDD	PRODOPER(50%)
	END RECORD

	DECLARE SUMMARY_LEVEL THIS_LEVEL

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
	!	The ^*From Item\* field begins the report
	!	with a selected item by entering the selection in this field.
	!	The value entered must be in agreement with the value in field
	!	(10) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Product Indented Structure Report
	!	.x Product Indented Structure Report>From Item
	!	.x Item>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field enters an item
	!	with which the report will end by entering the selected
	!	item in this field.  The value entered must be in agreement with
	!	the value in field (10) Sort by.
	!	.b
	!	A blank setting will cause the report to end with the
	!	last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Product Indented Structure Report
	!	.x Product Indented Structure Report>To Item
	!	.x Item>To
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated
	!	items to be printed on the report by entering a "Wildcard"
	!	selection in this field.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Product Indented Structure Report
	!	.x Product Indented Structure Report>Wildcard
	!
	!--

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(9%), 132%)

	!++
	! Abstract:FLD10
	!	^*(10) Sort\*
	!	.B
	!	.LM +5
	!	The ^*Sort\* field selects an order
	!	by which the report will print.
	!	.B
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
	!	.end table
	!	An entry is required in this field.
	!	.LM -5
	!
	! Index:
	!	.x Sort>Product Indented Structure Report
	!	.x Product Indented Structure Report>Sort
	!
	!--

	SELECT SORT_BY$

	CASE "C"
		SORT_KEY% = 2%
		ADD_TITLE$ = "BY  PRODUCT  CATEGORY"

	CASE "D"
		SORT_KEY% = 3%
		ADD_TITLE$ = "BY  PRODUCT  DESCRIPTION"

	CASE "P"
		SORT_KEY% = 0%
		ADD_TITLE$ = "BY  PRODUCT  NUMBER"

	CASE "S"
		SORT_KEY% = 4%
		ADD_TITLE$ = "BY  PRODUCT  SECONDARY  CODE"

	CASE "T"
		SORT_KEY% = 1%
		ADD_TITLE$ = "BY  PRODUCT  TYPE"

	END SELECT

300	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.OPN"

310	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"

320	%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.MOD"

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "PRODUCT COPY LABOR " + ADD_TITLE$
	TITLE$(2%) = "Bill of Material System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Product#       Description                   "
	TITLE$(5%) = "."
	TITLE$(6%) = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	IF FROM_ITEM$ = ""
	THEN
		RESET #PD_PRODUCT.CH%, KEY #SORT_KEY%
	ELSE
		FIND #PD_PRODUCT.CH%, KEY #SORT_KEY% GE FROM_ITEM$, REGARDLESS
	END IF

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	GET #PD_PRODUCT.CH%, REGARDLESS

	!
	! Check current record
	!
	GOTO GetNextRec IF PD_PRODUCT::SSTATUS <> "A"

	SELECT SORT_BY$

	CASE "C"

		GOTO ExitProgram IF (PD_PRODUCT::CATEGORY > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::CATEGORY, -1%), WLDCRD$) = 0%

	CASE "D"

		GOTO ExitProgram IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::DESCRIPTION, -1%), WLDCRD$) = 0%

	CASE "P"

		GOTO ExitProgram IF (PD_PRODUCT::PRODUCT_NUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::PRODUCT_NUM, -1%), WLDCRD$) = 0%

	CASE "S"
		GOTO ExitProgram IF (PD_PRODUCT::SECONDARY_CODE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::SECONDARY_CODE, -1%), WLDCRD$) = 0%

	CASE "T"
		GOTO ExitProgram IF (PD_PRODUCT::PROD_TYPE> TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::PROD_TYPE, -1%), WLDCRD$) = 0%

	END SELECT

	TEXT$ = PD_PRODUCT::PRODUCT_NUM + "  " + &
		PD_PRODUCT::DESCRIPTION

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	!
	! Copy labor for this level
	!
	THIS_LEVEL::TOTAL_OPER = 0%
	CALL COPYALLLEVELS(UTL_REPORTX, PD_PRODUCT::PRODUCT_NUM, THIS_LEVEL, 1%)

17350	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Try for next record
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%) IF TEXT.TEXT%

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

	%Page

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	SELECT ERR
	CASE 154%	! Locked record
		SLEEP 5%
		RESUME
	END SELECT

	FILENAME$ = ""

	SELECT ERL
	!
	! Can't open BM_RELATION file
	!
	CASE 300%
		FILENAME$ = "BM_RELATION"

	!
	! Can't open PD_PRODUCT file
	!
	CASE 310%
		FILENAME$ = "PD_PRODUCT"

	!
	! Can't find PD_PRODUCT record
	!
	CASE 17000%
		Resume ExitProgram IF ERR = 155%
		FILENAME$ = "PD_PRODUCT"

	!
	! End of PD_PRODUCT file
	!
	CASE 17020%
		RESUME ExitProgram IF ERR = 11%
		FILENAME$  = "PD_PRODUCT"

	END SELECT

	!
	! Untrapped error
	!
	RESUME HelpError

19999	END


20000	SUB COPYALLLEVELS(UTL_REPORTX_CDD UTL_REPORTX, &
		STRING THIS_PRODUCT, &
		SUMMARY_LEVEL THIS_LEVEL, &
		INTEGER LEVELCOUNT)

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.HB"
	MAP (BM_RELATION)	BM_RELATION_CDD		BM_RELATION

	%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.HB"
	MAP (BM_PRODOPER)	BM_PRODOPER_CDD		BM_PRODOPER

	COM (BM_RELATION_CH) &
		BM_RELATION.CH%

	COM (BM_PRODOPER_CH) &
		BM_PRODOPER.CH%

	RECORD SUMMARY_LEVEL
		INTEGER		TOTAL_OPER
		BM_PRODOPER_CDD	PRODOPER(50%)
	END RECORD

	DECLARE SUMMARY_LEVEL NEXT_LEVEL
	DECLARE RFA THIS_BM

	IF LEVELCOUNT >= 35%
	THEN
		TEXT$ = "*** This level is too deep to be reasonable ***"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO 20600
	END IF

	!
	! Loop through this level, filling in the operations we know
	! with the default amounts.
	!
	THIS_LEVEL::TOTAL_OPER = 0%

20200	WHEN ERROR IN
		FIND #BM_PRODOPER.CH%, KEY #0% EQ THIS_PRODUCT, REGARDLESS
	USE
		CONTINUE 20400
	END WHEN

20300	WHEN ERROR IN
		GET #BM_PRODOPER.CH%, REGARDLESS
	USE
		CONTINUE 20400
	END WHEN

	IF (BM_PRODOPER::PRODUCT = THIS_PRODUCT)
	THEN
		THIS_LEVEL::TOTAL_OPER = THIS_LEVEL::TOTAL_OPER + 1%
		BM_PRODOPER::HOURS = BM_PRODOPER::THISHOURS
		THIS_LEVEL::PRODOPER(THIS_LEVEL::TOTAL_OPER) = &
			BM_PRODOPER

		GOTO 20300
	END IF

20400	!
	! Go through bill of materials and summarize
	!
	WHEN ERROR IN
		FIND #BM_RELATION.CH%, KEY #0% EQ THIS_PRODUCT, REGARDLESS
	USE
		CONTINUE 20600
	END WHEN

20500	WHEN ERROR IN
		GET #BM_RELATION.CH%, REGARDLESS
	USE
		CONTINUE 20600
	END WHEN

	GOTO 20600 IF BM_RELATION::PRODUCT <> THIS_PRODUCT

	!
	! Remember this bm level
	!
	THIS_BM = GETRFA(BM_RELATION.CH%)

	TEXT$ = SPACE$(2% * LEVELCOUNT) + ">" + BM_RELATION::COMPONENT + &
		FORMAT$(BM_RELATION::QUANTITY, " ###.###")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	!
	! Pull in labor for this level
	!
	CALL COPYALLLEVELS(UTL_REPORTX, BM_RELATION::COMPONENT + "", &
		NEXT_LEVEL, LEVELCOUNT + 1%)

	!
	! Recall the original BM
	!
	WHEN ERROR IN
		GET #BM_RELATION.CH%, RFA THIS_BM, REGARDLESS
	USE
		CONTINUE 20600
	END WHEN

	!
	! Summarize into current level
	!
	FOR I% = 1% TO NEXT_LEVEL::TOTAL_OPER

		FOR J% = 1% TO THIS_LEVEL::TOTAL_OPER

			IF THIS_LEVEL::PRODOPER(J%)::OPERATION = &
				NEXT_LEVEL::PRODOPER(I%)::OPERATION
			THEN
				THIS_LEVEL::PRODOPER(J%)::HOURS = &
					THIS_LEVEL::PRODOPER(J%)::HOURS + &
					NEXT_LEVEL::PRODOPER(I%)::HOURS * &
					BM_RELATION::QUANTITY
				GOTO 20590
			END IF

		NEXT J%

		THIS_LEVEL::TOTAL_OPER = THIS_LEVEL::TOTAL_OPER + 1%
		THIS_LEVEL::PRODOPER(THIS_LEVEL::TOTAL_OPER) = &
			NEXT_LEVEL::PRODOPER(I%)
		THIS_LEVEL::PRODOPER(THIS_LEVEL::TOTAL_OPER)::PRODUCT = &
			THIS_PRODUCT
		THIS_LEVEL::PRODOPER(THIS_LEVEL::TOTAL_OPER)::THISHOURS = &
			0.0
		THIS_LEVEL::PRODOPER(THIS_LEVEL::TOTAL_OPER)::HOURS = &
			NEXT_LEVEL::PRODOPER(I%)::HOURS * &
			BM_RELATION::QUANTITY

20590	NEXT I%

	GOTO 20500

20600	!
	! Update this level's labor
	!

	!
	! Erase old version
	!
	WHEN ERROR IN
		FIND #BM_PRODOPER.CH%, KEY #0% EQ THIS_PRODUCT
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 20800
	END WHEN

20700	WHEN ERROR IN
		GET #BM_PRODOPER.CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 20800
	END WHEN

	IF BM_PRODOPER::PRODUCT = THIS_PRODUCT
	THEN
		DELETE #BM_PRODOPER.CH%

		GOTO 20700
	END IF

20800	!
	! Put the records back in the file
	!
	FOR I% = 1% TO THIS_LEVEL::TOTAL_OPER

		BM_PRODOPER = THIS_LEVEL::PRODOPER(I%)
		BM_PRODOPER::PRODUCT = THIS_PRODUCT
		BM_PRODOPER::ITEMNUM = FORMAT$(I%, "<0>###")
		BM_PRODOPER::STAT = "A"

		TEXT$ = SPACE$(2% * LEVELCOUNT) + "-" + &
			BM_PRODOPER::OPERATION + &
			FORMAT$(BM_PRODOPER::THISHOURS, "  ###.###") + &
			FORMAT$(BM_PRODOPER::HOURS, " ###.###")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		PUT #BM_PRODOPER.CH%

	NEXT I%

	END SUB
