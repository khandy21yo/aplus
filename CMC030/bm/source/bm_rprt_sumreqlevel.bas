1	%TITLE "Bill of Material"
	%SBTTL "BM_RPRT_SUMREQLEVEL"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1996 BY
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
	! ID:BM007
	!
	! Abstract:HELP
	!	.p
	!	This report prints out the required on hand material
	!	and purchase requirements to produce a given
	!	product based upon the inventory currently available.
	!
	! Index:
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS BM_SOURCE:BM_RPRT_SUMREQLEVEL/LINE
	!	$ LINK/EXE=BM_EXE: BM_RPRT_SUMREQLEVEL, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE BM_RPRT_SUMREQLEVEL.OBJ;*
	!
	! Author:
	!
	!	12/09/96 - Kevin Handy
	!		Started with BM_RPRT_SUMREQ
	!
	! Modification History:
	!
	!	12/23/96 - Kevin Handy
	!		Added code to be able to stop at a gevin product type
	!
	!	01/10/97 - Kevin Handy
	!		Modified to drop drawings off of report.
	!
	!	02/18/97 - Kevin Handy
	!		Lose error trap for 17200, which doesn't exist.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/05/2000 - Kevin Handy
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
	DECLARE		UTL_REPORTX_CDD			UTL_REPORTX

	%INCLUDE "SOURCE:[BM.OPEN]BM_CONTROL.HB"
	MAP (BM_CONTROL)	BM_CONTROL_CDD		BM_CONTROL

	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.HB"
	MAP (BM_RELATION)	BM_RELATION_CDD		BM_RELATION

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	RECORD PROCESS_RECORD
		STRING PRODUCT = 14%	! Product number
		REAL QTY_NEEDED		! Quanity Needed
		REAL QTY_AVAILABLE	! Quanity Availeble
		INTEGER LOWEST		! Is this at the lowest level?
	END RECORD

	DIM PROCESS_RECORD UNPROCESSED(500%), PROCESSED(1000%)
	DECLARE PROCESS_RECORD THIS_RECORD

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION PD_EXAM_PRODUCT
	EXTERNAL LONG    FUNCTION IC_READ_35BALANCE
	EXTERNAL REAL    FUNCTION PC_READ_COST

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
	!	item in this field.  The value entered must be in agreement
	!	with the value in field (10) Sort by.
	!	.b
	!	A blank setting will cause the report to end with the
	!	last item in the file.
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
	!	The ^*Wildcard\* field selects designated
	!	items to be printed on the report by entering a "Wildcard"
	!	selection in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	LOCATION$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Location _#\*
	!	.b
	!	.lm +5
	!	The ^*Location _#\* field enters a location
	!	code which is to be printed.
	!	.b
	!	This field will accommodate up to four (4) alphanumeric
	!	characters.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	PHANTOM_PARTS$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 128%)

	!++
	! Abstract:FLD06
	!	^*(06) Phantom Parts\*
	!	.b
	!	List of the phantom parts.
	!	These parts types represent an intermediate step,
	!	and should not appear on the report.
	!
	! Index:
	!
	!--

	END_TYPE$ = EDIT$(UTL_REPORTX::OPTDEF(7%), -1%)

	!++
	! Abstract:FLD08
	!	^*(08) End at Product Type\*
	!	.b
	!	.lm +5
	!	Used to control which level the report will go down to.
	!	A blank entry here will cause it to go to the lowest BOM level.
	!	Wildcards are supported in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	QUANTITY% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(8%), -1%))
	QUANTITY% = 1% IF QUANTITY% = 0%

	!++
	! Abstract:FLD09
	!	^*(09) Quantity to Make\*
	!	.b
	!	.lm +5
	!	The ^*Quantity to Make\* field enters the number
	!	of completed units which are desired.
	!	.lm -5
	!
	! Index:
	!
	!--

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(9%), 132%)

	!++
	! Abstract:FLD10
	!	^*(10) Sort\*
	!	.b
	!	.lm +5
	!	The ^*Sort\* field selects an order
	!	by which the report will print.
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
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.OPN"
	USE
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[BM.OPEN]BM_CONTROL.OPN"

		GET #BM_CONTROL.CH%, RECORD 1%, REGARDLESS

		CLOSE #BM_CONTROL.CH%
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "BM_CONTROL"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	SELECT SORT_BY$

	CASE "C"
		SORT_KEY% = 2%
		ADD_TITLE$ = "BY  CATEGORY"

	CASE "D"
		SORT_KEY% = 3%
		ADD_TITLE$ = "BY  DESCRIPTION"

	CASE "P"
		SORT_KEY% = 0%
		ADD_TITLE$ = "BY  PRODUCT  NUMBER"

	CASE "S"
		SORT_KEY% = 4%
		ADD_TITLE$ = "BY  SECONDARY  CODE"

	CASE "T"
		SORT_KEY% = 1%
		ADD_TITLE$ = "BY  PRODUCT  TYPE"
	END SELECT

	TITLE$(1%) = "PRODUCT  STRUCTURE  " + ADD_TITLE$ + &
		"  ON  THE  LOWEST  LEVEL"

	TITLE$(2%) = "Bill of Material System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Product#       Description               Type" + &
		" Cat  SecCode    UOM    Ext_Qty    On_Hand   " + &
		"   Alloc    On_Ordr   Required       Cost"

	TITLE$(5%) = SPACE$(16%) + "Type Component#     " + &
		"Description"


	TITLE$(6%) = "."

	%PAGE

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
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORT_BY$

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

	CASE "P"
		GOTO ExitProgram IF (PD_PRODUCT::PRODUCT_NUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::PRODUCT_NUM, -1%), &
			WLDCRD$) = 0%

	CASE "S"
		GOTO ExitProgram IF (PD_PRODUCT::SECONDARY_CODE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::SECONDARY_CODE, -1%), &
			WLDCRD$) = 0%

	CASE "T"
		GOTO ExitProgram IF (PD_PRODUCT::PROD_TYPE> TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(PD_PRODUCT::PROD_TYPE, -1%), &
			WLDCRD$) = 0%

	END SELECT

17300	!
	! Print out Product
	!
	PD_PRODUCT$ = PD_PRODUCT::PRODUCT_NUM

 !	V% = IC_READ_35BALANCE(PD_PRODUCT$, LOCATION$, BALANCE(,))
 !
 !	ONHAND = BALANCE(1%, 1%) + BALANCE(1%, 2%) + BALANCE(1%, 3%)
 !	ALLOC   = BALANCE(2%, 1%) + BALANCE(2%, 2%) + BALANCE(2%, 3%)
 !	ONORDER = BALANCE(3%, 1%) + BALANCE(3%, 2%) + BALANCE(3%, 3%)
 !
 !	REQUIRED = -(ONHAND + ALLOC + ONORDER - QUANTITY%)
 !	REQUIRED = QUANTITY% IF QUANTITY% < REQUIRED
 !	REQUIRED = 0.0 IF REQUIRED < 0.0

	TEXT$ = PD_PRODUCT::PRODUCT_NUM + " " + &
		LEFT(PD_PRODUCT::DESCRIPTION, 25%) + " " + &
		PD_PRODUCT::PROD_TYPE + "   " + &
		PD_PRODUCT::CATEGORY + " " + &
		PD_PRODUCT::SECONDARY_CODE + " " + &
		PD_PRODUCT::BOMUOM + &
		FORMAT$(QUANTITY%, "#,###,###.##")

 !		FORMAT$(ONHAND, "####,###.##") + &
 !		FORMAT$(-ALLOC, "####,###.##") + &
 !		FORMAT$(ONORDER, "####,###.##") + &
 !		FORMAT$(REQUIRED, "<%>###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	UNPROCESSED% = 1%
	UNPROCESSED(1%)::PRODUCT = PD_PRODUCT::PRODUCT_NUM
	UNPROCESSED(1%)::QTY_NEEDED = QUANTITY%
	UNPROCESSED(1%)::QTY_AVAILABLE = 0.0
	UNPROCESSED(1%)::LOWEST = 0%

	!
	! Must have to force it to look lower
	!
	PROCESSED% = 1%
	PROCESSED(1%)::PRODUCT = PD_PRODUCT::PRODUCT_NUM
	PROCESSED(1%)::QTY_NEEDED = 0.0
	PROCESSED(1%)::QTY_AVAILABLE = 0.0
	PROCESSED(1%)::LOWEST = 0%

	WHILE UNPROCESSED% > 0%
		GOSUB ProcessBottom
	NEXT

	!
	! Print array
	!
	FOR I% = 1% TO PROCESSED%

		XPD_PRODUCT$ = PROCESSED(I%)::PRODUCT

		!
		! Read Product File
		!
		V% = PD_EXAM_PRODUCT(XPD_PRODUCT$, PD_PRODUCT_EXAM)

		!
		! Calculate cost
		!
		COST = PC_READ_COST(XPD_PRODUCT$, LOCATION$, "", "")

		!
		! Convert quantity from BM to IC quantity
		!
		QTY_NEEDED = PROCESSED(I%)::QTY_NEEDED

		IF (QTY_NEEDED <> 0.0)
		THEN
			!
			! Compute $ totals for this part
			!
			TOTAL_COMPONENT = FUNC_ROUND(QTY_NEEDED * COST, 3%)

			TOTAL_PRODUCT = TOTAL_PRODUCT + &
				FUNC_ROUND(QTY_NEEDED * COST, 3%)

			V% = IC_READ_35BALANCE(PROCESSED(I%)::PRODUCT, &
				LOCATION$, BALANCE(,))

			ONHAND = BALANCE(1%, 1%) + BALANCE(1%, 2%) + &
				BALANCE(1%, 3%)
			ALLOC   = BALANCE(2%, 1%) + BALANCE(2%, 2%) + &
				BALANCE(2%, 3%)
			ONORDER = BALANCE(3%, 1%) + BALANCE(3%, 2%) + &
				BALANCE(3%, 3%)

			REQUIRED = -(PROCESSED(I%)::QTY_AVAILABLE - QTY_NEEDED)
			REQUIRED = QTY_NEEDED IF QTY_NEEDED < REQUIRED
			REQUIRED = 0.0 IF REQUIRED < 0.0

			!
			! Print out part
			!
			TEXT$ = PD_PRODUCT$ + "  "  + &
				PD_PRODUCT_EXAM::PROD_TYPE + "   " + &
				PROCESSED(I%)::PRODUCT + " " + &
				LEFT(PD_PRODUCT_EXAM::DESCRIPTION, 25%) + " " + &
				PD_PRODUCT_EXAM::UOM + &
				FORMAT$(QTY_NEEDED, "#,###,###.##") + &
				FORMAT$(ONHAND, "####,###.##") + &
				FORMAT$(-ALLOC, "####,###.##") + &
				FORMAT$(ONORDER, "####,###.##") + &
				FORMAT$(REQUIRED, "<%>###,###.##") + &
				FORMAT$(TOTAL_COMPONENT, "###,###.###")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		END IF
	NEXT I%

	IF PROCESSED%
	THEN
		TEXT$ = PD_PRODUCT$ + SPACE$(89%) + &
			"TOTAL" + SPACE$(10%) + &
			FORMAT$(TOTAL_PRODUCT, "#,###,###.###*")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)
	END IF

	GOTO ExitProgram IF UTL_REPORTX::STAT

17350	!
	! Try for next record
	!
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

 ProcessBottom:
18100	!*******************************************************************
	! Pop off the bottom item from the unprocessed list,
	! and either create an processed item, or more
	! unprocessed items
	!
	!	1. If we have available in the processed area
	!	2. If we can make an available for this product
	!	3. If we can generate lower level
	!	4. Give up and stuff it into processed area.
	!
	!*******************************************************************

	!
	! Pop off bottom record
	!
	THIS_RECORD = UNPROCESSED(UNPROCESSED%)
	UNPROCESSED% = UNPROCESSED% - 1%

	!
	! Do we already have one of these in the processed list
	! that we can just add this onto
	!
	FOR PLOOP% = 1% TO PROCESSED%
		IF PROCESSED(PLOOP%)::PRODUCT = THIS_RECORD::PRODUCT
		THEN
			!
			! This is the item. Find out how many are
			! available to work with
			!
			CANDO = PROCESSED(PLOOP%)::QTY_AVAILABLE - &
				PROCESSED(PLOOP%)::QTY_NEEDED

			!
			! If this is negative, then assume that we
			! must have required more before, and need to
			! do so again.
			!
			IF (PROCESSED(PLOOP%)::LOWEST <> 0%) OR &
				CANDO >= THIS_RECORD::QTY_NEEDED
			THEN
				CANDO = THIS_RECORD::QTY_NEEDED
			ELSE
				CANDO = 0%
			END IF

			!
			! Add to the qty used for this item
			!
			PROCESSED(PLOOP%)::QTY_NEEDED = &
				PROCESSED(PLOOP%)::QTY_NEEDED + &
				CANDO

			!
			! Reduce the required qty
			!
			THIS_RECORD::QTY_NEEDED = &
				THIS_RECORD::QTY_NEEDED - &
				CANDO

			GOTO 18140
		END IF

	NEXT PLOOP%

18110	!
	! Skip out if nothing else to do
	!
	GOTO 18190 IF THIS_RECORD::QTY_NEEDED = 0.0

	V% = PD_EXAM_PRODUCT(THIS_RECORD::PRODUCT, PD_PRODUCT_EXAM)
	GOTO 18160 IF COMP_STRING(END_TYPE$, PD_PRODUCT_EXAM::PROD_TYPE) <> 0%

	!
	! If type is listed in BM control file, don't stop at this level.
	!
	IF PHANTOM_PARTS$ <> ""
	THEN
		GOTO 18140 IF COMP_STRING(EDIT$( &
			PD_PRODUCT_EXAM::PROD_TYPE, -1%), &
			PHANTOM_PARTS$) <> 0%
	END IF

	!
	! If we get here, then nothing exists for this product
	! in the processed file. Lets see if we can make something
	!
	V% = IC_READ_35BALANCE(THIS_RECORD::PRODUCT, LOCATION$, BALANCE(,))

	ONHAND = BALANCE(1%, 1%) + BALANCE(1%, 2%) + BALANCE(1%, 3%)
	ALLOC   = BALANCE(2%, 1%) + BALANCE(2%, 2%) + BALANCE(2%, 3%)
	ONORDER = BALANCE(3%, 1%) + BALANCE(3%, 2%) + BALANCE(3%, 3%)

	CANDO = ONHAND + ALLOC + ONORDER
	CANDO = 0% IF CANDO < 0%

	IF CANDO > 0%
	THEN
		!
		! Yes, we can make something out of this.
		! We have available material.
		!
		AVAIL = CANDO
		IF CANDO > THIS_RECORD::QTY_NEEDED
		THEN
			CANDO = THIS_RECORD::QTY_NEEDED
		END IF

		PROCESSED% = PROCESSED% + 1%
		PROCESSED(PROCESSED%) = THIS_RECORD
		PROCESSED(PROCESSED%)::QTY_NEEDED = CANDO
		PROCESSED(PROCESSED%)::QTY_AVAILABLE = AVAIL

		THIS_RECORD::QTY_NEEDED = &
			THIS_RECORD::QTY_NEEDED - &
			CANDO
	END IF

18140	!
	! Skip out if nothing else to do
	!
	GOTO 18190 IF THIS_RECORD::QTY_NEEDED = 0.0

	!
	! Try to go down one level in the bill of material
	!
	WHEN ERROR IN
		FIND #BM_RELATION.CH%, &
			KEY #0% EQ THIS_RECORD::PRODUCT, &
			REGARDLESS
	USE
		CONTINUE 18160
	END WHEN

18150	WHEN ERROR IN
		GET #BM_RELATION.CH%, REGARDLESS
	USE
		CONTINUE 18190
	END WHEN

	IF BM_RELATION::PRODUCT = THIS_RECORD::PRODUCT
	THEN
		!
		! Get the multiplier
		!
		V% = PD_EXAM_PRODUCT(BM_RELATION::COMPONENT, PD_PRODUCT_EXAM)

		PD_PRODUCT_EXAM::PRODUCT_FACTOR = 1.0 &
			IF PD_PRODUCT_EXAM::PRODUCT_FACTOR = 0.0

		!
		! Add to unprocessed list. Should probibly try to merge
		! with other unprocessed products, but maybe this will
		! work ok anyway
		!
		UNPROCESSED% = UNPROCESSED% + 1%

		UNPROCESSED(UNPROCESSED%)::PRODUCT = BM_RELATION::COMPONENT
		UNPROCESSED(UNPROCESSED%)::QTY_NEEDED = &
			BM_RELATION::QUANTITY * &
			THIS_RECORD::QTY_NEEDED / &
			PD_PRODUCT_EXAM::PRODUCT_FACTOR
		UNPROCESSED(UNPROCESSED%)::QTY_AVAILABLE = 0.0

		GOTO 18150
	END IF

	GOTO 18190

18160	!
	! We get here if we couldn't find any BM level below this
	! product
	!
	! Try to add it to a just created item so that it doesn't
	! get duplicated.
	!
	IF PROCESSED(PROCESSED%)::PRODUCT = THIS_RECORD::PRODUCT
	THEN
		PROCESSED(PROCESSED%)::QTY_NEEDED = &
			PROCESSED(PROCESSED%)::QTY_NEEDED + &
			THIS_RECORD::QTY_NEEDED
		PROCESSED(PROCESSED%)::LOWEST = -1%
	ELSE
		PROCESSED% = PROCESSED% + 1%
		PROCESSED(PROCESSED%) = THIS_RECORD
		PROCESSED(PROCESSED%)::LOWEST = -1%
	END IF

18190	RETURN

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
