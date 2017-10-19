1	%TITLE "Create WO and Requisition from MO Order"
	%SBTTL "MO_WRIT_ORDER"
	%IDENT "V3.6a Calico"

	FUNCTION LONG MO_WRIT_ORDER(STRING ORDER, STRING BATCH)

	!
	!	COPYRIGHT (C) 1986 BY
	!	Computer Management Center, Idaho Falls, Idaho.
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
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This function creates records in WP Order journal and
	!	in WP Requisition journal.
	!	.lm -5
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS MO_SOURCE:MO_WRIT_ORDER/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP MO_WRIT_ORDER
	!	$ DELETE MO_WRIT_ORDER.OBJ;*
	!
	! Author:
	!
	!	12/29/92 - Frank F. Starman
	!
	! Modification history:
	!
	!	01/08/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/02/93 - Kevin Handy
	!		Put 19000 in numerical order.
	!
	!	04/12/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/11/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/28/93 - Frank F. Starman
	!		Do not create any requisitions.
	!
	!	02/24/94 - Kevin Handy
	!		Removed about 30 pounds of commented out code
	!		so I could see what was actually here.
	!
	!	02/24/94 - Kevin Handy
	!		Removed/modified code to lose from-to which were
	!		both set to ORDER.
	!
	!	06/17/94 - Kevin Handy
	!		Added code for ::MISCH2.
	!
	!	04/06/95 - Kevin Handy
	!		(V3.6)
	!		Updated source code to V3.6 standards.
	!		Added code to PD_EXAM to initialize dummy info if
	!		product was not found.
	!
	!	12/06/95 - Kevin Handy
	!		Remove bunches of commented out code.
	!		Reformat source closer to 80 columns.
	!
	!	12/08/95 - Kevin Handy
	!		Modified for change in OE_ORDERLINE::NOTES, and
	!		addition of OE_ORDERLINE::SUBACCT.
	!
	!	07/22/96 - Kevin Handy
	!		Reformat source.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	05/05/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	07/28/2000 - Kevin Handy
	!		Handle PRODUCT_FACTOR when dealing with WP_REQLINE::QTY
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:WP_WINDOW.INC"

	!
	! Map file
	!
	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.HB"
	MAP	(OE_ORDERJOUR)	OE_ORDERJOUR_CDD	OE_ORDERJOUR

	%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINE.HB"
	MAP	(MO_ORDERLINE)	MO_ORDERLINE_CDD	MO_ORDERLINE

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERLINE.HB"
	MAP	(OE_ORDERLINE)	OE_ORDERLINE_CDD	OE_ORDERLINE

	%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINEOPT.HB"
	MAP	(MO_ORDERLINEOPT)	MO_ORDERLINEOPT_CDD	MO_ORDERLINEOPT

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE		AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	%INCLUDE "SOURCE:[WP.OPEN]WP_ORDERLINE.HB"
	MAP	(WP_ORDERLINE)	WP_ORDERLINE_CDD	WP_ORDERLINE

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQLINE.HB"
	MAP	(WP_REQLINE)	WP_REQLINE_CDD	WP_REQLINE

	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.HB"
	MAP	(BM_RELATION)	BM_RELATION_CDD	BM_RELATION

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE		PD_PRODUCT_CDD	PD_PRODUCT_EXAM

	COM (CH_OE_ORDERJOUR)	OE_ORDERJOUR.CH%
	COM (CH_MO_ORDERLINE)	MO_ORDERLINE.CH%
	COM (CH_MO_ORDERLINEOPT)MO_ORDERLINEOPT.CH%
	COM (CH_WP_ORDERLINE)	WP_ORDERLINE.CH%
	COM (CH_WP_REQLINE)	WP_REQLINE.CH%
	COM (CH_OE_ORDERLINE)	OE_ORDERLINE.CH%
	COM (CH_BM_RELATION_READ) BM_RELATION.CH%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION IC_WRIT_35BALANCE
	EXTERNAL REAL   FUNCTION PC_READ_COST
	EXTERNAL REAL   FUNCTION PC_READ_PRICE
	EXTERNAL LONG   FUNCTION PD_EXAM_PRODUCT
	EXTERNAL LONG   FUNCTION AR_EXAM_CUSTOM

	DECLARE LONG SMG_COPY
	DECLARE RFA RFALEVEL0

	%PAGE

	ON ERROR GOTO 19000

	BATCH_NO$ = BATCH

	!
	! Paint the background, and confirm close
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		5%, &
		60%, &
		SMG_COPY, &
		SMG$M_BORDER &
	)

	!
	! Label the display
	!
	SMG_STATUS% = SMG$LABEL_BORDER(SMG_COPY, &
		"Create WO ")

	SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY, &
		"Work Order Number " + ORDER, 1%, 2%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_COPY, &
		SCOPE::SMG_PBID, &
		11%, &
		10% &
	)


300	IF MO_ORDERLINE.CH% <= 0%
	THEN
		!
		! Open order line file
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINE.CRE"
		USE
			FILENAME$ = "MO_ORDERLINE"
			CONTINUE HelpError
		END WHEN
	END IF

310	IF MO_ORDERLINEOPT.CH% <= 0%
	THEN
		!
		! Open option file
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINEOPT.CRE"
		USE
			FILENAME$ = "MO_ORDERLINEOPT"
			CONTINUE HelpError
		END WHEN
	END IF

320	IF WP_ORDERLINE.CH% <= 0%
	THEN
		!
		! Open job line file
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_ORDERLINE.CRE"
		USE
			FILENAME$ = "WP_ORDERLINE"
			CONTINUE HelpError
		END WHEN
	END IF

330	IF WP_REQLINE.CH% <= 0%
	THEN
		!
		! Open req line file
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_REQLINE.CRE"
		USE
			FILENAME$ = "WP_REQLINE"
			CONTINUE HelpError
		END WHEN
	END IF

340	IF OE_ORDERLINE.CH% <= 0%
	THEN
		!
		! Open order line file
		!
		%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERLINE.CRE"
	END IF

350	!
	! Open BM_RELATION channel
	!
	IF BM_RELATION.CH% <= 0%
	THEN
		%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.OPN"
	END IF

 ToItem:
	SCOPE::PRG_ITEM = "CONFIRM"
	INP$ = ENTR_3YESNO(SCOPE, SMG_COPY, &
		"", "Confirm creating - then press <Do> ", "N", 0%, "", "")

	IF INP$ <> "Y"
	THEN
		GOTO ExitFunction
	END IF

	CALL ENTR_3MESSAGE(SCOPE, "Creating a work order", 1% + 16%)

	X% = 3%
	ITEM% = 0%

700	WHEN ERROR IN
		GET #OE_ORDERJOUR.CH%, KEY #0% EQ ORDER, REGARDLESS
	USE
		CONTINUE 2100 IF ERR = 155% OR ERR = 131%
		FILENAME$ = "OE_ORDERJOUR"
		CONTINUE HelpError
	END WHEN

800	!
	! Remove old lines
	!
	WHEN ERROR IN
		GET #WP_ORDERLINE.CH%, &
			KEY #0% EQ EDIT$(OE_ORDERJOUR::ORDNUM, -1%)

		DELETE #WP_ORDERLINE.CH%
	USE
		CONTINUE 810 IF ERR = 11% OR ERR = 131% OR ERR = 155% &
			OR ERR = 154%
		FILENAME$ = "WP_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	V% = IC_WRIT_35BALANCE(WP_ORDERLINE::ITEMCODE, &
		OE_ORDERJOUR::LOCATION, "WO", -WP_ORDERLINE::QTY)

	GOTO 800

810	!
	! Remove old lines
	!
	WHEN ERROR IN
		GET #WP_REQLINE.CH%, KEY #0% EQ EDIT$(OE_ORDERJOUR::ORDNUM, -1%)
		DELETE #WP_REQLINE.CH%
	USE
		CONTINUE 820 IF ERR = 11% OR ERR = 131% OR ERR = 155% &
			OR ERR = 154%
		FILENAME$ = "WP_REQLINE"
		CONTINUE HelpError
	END WHEN

	IF PD_EXAM_PRODUCT(MO_ORDERLINE::PRODUCT, PD_PRODUCT_EXAM) <> &
		CMC$_NORMAL
	THEN
		PD_PRODUCT_EXAM::PRODUCT_FACTOR = 1.0
	END IF

	PD_PRODUCT_EXAM::PRODUCT_FACTOR = 1.0 &
		IF PD_PRODUCT_EXAM::PRODUCT_FACTOR = 0.0

	V% = IC_WRIT_35BALANCE(WP_REQLINE::PRODUCT, &
		OE_ORDERJOUR::LOCATION, "RQ", &
		FUNC_ROUND(-WP_REQLINE::QTY / &
		PD_PRODUCT_EXAM::PRODUCT_FACTOR, 3%))
	GOTO 810

820	!
	! Remove old lines
	!
	WHEN ERROR IN
		GET #OE_ORDERLINE.CH%, KEY #0% EQ OE_ORDERJOUR::ORDNUM

		DELETE #OE_ORDERLINE.CH%
	USE
		CONTINUE 920 IF ERR = 11% OR ERR = 131% OR ERR = 155% &
			OR ERR = 154%
		FILENAME$ = "OE_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	V% = IC_WRIT_35BALANCE(OE_ORDERLINE::PRODUCT, &
		OE_ORDERJOUR::LOCATION, "MO", -OE_ORDERLINE::ORDQTY)

	GOTO 820

920	WHEN ERROR IN
		FIND #MO_ORDERLINE.CH%, &
			KEY #0% EQ OE_ORDERJOUR::ORDNUM, &
			REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 11% OR ERR = 131% OR &
			ERR = 155% OR ERR = 136%
		FILENAME$ = "MO_ORDERLINE"
		CONTINUE HelpError
	END WHEN

 GetLine:
	WHEN ERROR IN
		GET #MO_ORDERLINE.CH%, REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 11% OR ERR = 131% OR &
			ERR = 155% OR ERR = 136%
		FILENAME$ = "MO_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	GOTO GetLine IF MO_ORDERLINE::ORDQTY <= 0.0

	GOTO ExitFunction IF OE_ORDERJOUR::ORDNUM <> MO_ORDERLINE::ORDNUM

	WP_ORDERLINE::ITEMCODE = MO_ORDERLINE::PRODUCT
	LEVEL% = 0%

	IF PD_EXAM_PRODUCT(MO_ORDERLINE::PRODUCT, PD_PRODUCT_EXAM) <> &
		CMC$_NORMAL
	THEN
		! Fake a description from the product number if undefined
		PD_PRODUCT_EXAM::DESCRIPTION = MO_ORDERLINE::PRODUCT
		PD_PRODUCT_EXAM::LABEL = ""
	END IF

	IF LEFT(OE_ORDERJOUR::ORDCAT, 2%) = "BK" OR &
		LEFT(OE_ORDERJOUR::ORDCAT, 2%) = "PK"
	THEN
		BM_RELATION::COMPONENT,PRODUCT$ = MO_ORDERLINE::PRODUCT
		BM_RELATION::QUANTITY = MO_ORDERLINE::ORDQTY
		GOSUB 21925
	END IF

	ITEM% = ITEM% + 1%
	WP_ORDERLINE::JOB	= EDIT$(OE_ORDERJOUR::ORDNUM, -1%)
	WP_ORDERLINE::TTYPE	= "M"
	WP_ORDERLINE::LLINE	= FORMAT$(ITEM%, "<0>###")
	WP_ORDERLINE::QTY	= MO_ORDERLINE::ORDQTY
	WP_ORDERLINE::COST	= MO_ORDERLINE::COST
	WP_ORDERLINE::START_DATE= OE_ORDERJOUR::ORDDATE
	WP_ORDERLINE::COMP_DATE = MO_ORDERLINE::REQDATE
	WP_ORDERLINE::DESCR	= PD_PRODUCT_EXAM::DESCRIPTION

	IF X% <> 5%
	THEN
		WHEN ERROR IN
			PUT #WP_ORDERLINE.CH%
		USE
			CONTINUE ExitFunction IF ERR = 131% OR ERR = 136%
			FILENAME$ = "MO_ORDERLINE"
			CONTINUE HelpError
		END WHEN

		V% = IC_WRIT_35BALANCE(WP_ORDERLINE::ITEMCODE, &
			OE_ORDERJOUR::LOCATION, "WO", WP_ORDERLINE::QTY)
	END IF

	GOSUB GetComp IF LEVEL% = 1%

940	WHEN ERROR IN
		FIND #MO_ORDERLINEOPT.CH%, &
			KEY #0% EQ OE_ORDERJOUR::ORDNUM + &
			MO_ORDERLINE::LIN + MO_ORDERLINE::MAKE + &
			MO_ORDERLINE::MODELCODE, REGARDLESS
	USE
		CONTINUE GetLine IF ERR = 11% OR ERR = 131% OR &
			ERR = 155%
		FILENAME$ = "MO_ORDERLINEOPT"
		CONTINUE HelpError
	END WHEN

 GetLineOpt:
	WHEN ERROR IN
		GET #MO_ORDERLINEOPT.CH%, REGARDLESS
	USE
		CONTINUE GetLine IF ERR = 11% OR ERR = 131% OR &
			ERR = 155%
		FILENAME$ = "MO_ORDERLINEOPT"
		CONTINUE HelpError
	END WHEN

	GOTO GetLine &
		IF MO_ORDERLINEOPT::ORDNUM <> OE_ORDERJOUR::ORDNUM OR &
		MO_ORDERLINEOPT::LIN <> MO_ORDERLINE::LIN OR &
		MO_ORDERLINEOPT::MAKE <> MO_ORDERLINE::MAKE OR &
		MO_ORDERLINEOPT::MODELCODE <> MO_ORDERLINE::MODELCODE

	WP_ORDERLINE::ITEMCODE = MO_ORDERLINEOPT::PRODUCT
	LEVEL% = 0%

	IF LEFT(OE_ORDERJOUR::ORDCAT, 2%) = "BK" OR &
		LEFT(OE_ORDERJOUR::ORDCAT, 2%) = "PK"
	THEN
		BM_RELATION::COMPONENT, PRODUCT$ = MO_ORDERLINEOPT::PRODUCT
		BM_RELATION::QUANTITY = MO_ORDERLINEOPT::ORDQTY
		GOSUB 21925
	END IF

	ITEM% = ITEM% + 1%
	WP_ORDERLINE::LLINE	= FORMAT$(ITEM%, "<0>###")
	WP_ORDERLINE::ITEMCODE	= MO_ORDERLINEOPT::PRODUCT
	WP_ORDERLINE::QTY	= MO_ORDERLINEOPT::ORDQTY
	WP_ORDERLINE::COST	= MO_ORDERLINEOPT::COST
	WP_ORDERLINE::COMP_DATE = MO_ORDERLINE::REQDATE
	WP_ORDERLINE::DESCR	= MO_ORDERLINEOPT::OPTDESCR

	IF X% <> 5%
	THEN
		PUT #WP_ORDERLINE.CH%

		V% = IC_WRIT_35BALANCE(WP_ORDERLINE::ITEMCODE, &
			OE_ORDERJOUR::LOCATION, "WO", WP_ORDERLINE::QTY)
	END IF

	GOTO GetLineOpt

2100	!
 ExitFunction:

	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_COPY)

	CALL ENTR_3MESSAGE(SCOPE, "", 1%)
	EXIT FUNCTION

	%PAGE

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

	%PAGE

21925	WHEN ERROR IN
		FIND #BM_RELATION.CH%, KEY #0% EQ PRODUCT$, REGARDLESS
	USE
		CONTINUE RetBOM IF ERR = 131% OR ERR = 155% OR ERR = 11%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

 GetComp:
	WHEN ERROR IN
		GET #BM_RELATION.CH%, RFA RFALEVEL0, REGARDLESS IF LEVEL% = 1%

		GET #BM_RELATION.CH%, REGARDLESS
	USE
		CONTINUE RetBOM IF ERR = 131% OR ERR = 155% OR ERR = 11%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	GOTO RetBOM IF BM_RELATION::PRODUCT <> PRODUCT$

	RFALEVEL0 = GETRFA(BM_RELATION.CH%)
	LEVEL% = 0%

21930	WHEN ERROR IN
		FIND #BM_RELATION.CH%, &
			KEY #0% EQ BM_RELATION::COMPONENT, &
			REGARDLESS
	USE
		CONTINUE AddOrderLine IF ERR = 155%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	LEVEL% = 1%

 AddOrderLine:
	MO_ORDERLINE::COST = &
		PC_READ_COST(BM_RELATION::COMPONENT,OE_ORDERJOUR::LOCATION, &
		OE_ORDERJOUR::ORDDATE, "")

	IF PD_EXAM_PRODUCT(BM_RELATION::COMPONENT, PD_PRODUCT_EXAM) <> &
		CMC$_NORMAL
	THEN
		! Couldn't find, create a dummy label
		PD_PRODUCT_EXAM::DESCRIPTION = MO_ORDERLINE::PRODUCT
		PD_PRODUCT_EXAM::LABEL = ""
	END IF

	GOTO 21950 IF LEFT(PD_PRODUCT_EXAM::LABEL, 2%) <> "BK" AND &
		LEFT(OE_ORDERJOUR::ORDCAT, 2%) <> "PK"

	V% = AR_EXAM_CUSTOM(OE_ORDERJOUR::CUSNUM, AR_35CUSTOM_EXAM)

21940	WHEN ERROR IN
		GET #OE_ORDERLINE.CH%, KEY #1% EQ BM_RELATION::COMPONENT
	USE
		CONTINUE 21945 IF ERR = 155%
		FILENAME$ = "OE_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	OE_ORDERLINE::ORDQTY = OE_ORDERLINE::ORDQTY + BM_RELATION::QUANTITY

	IF MO_ORDERLINE::SHPQTY = 0.0
	THEN
		OE_ORDERLINE::BCKQTY = OE_ORDERLINE::BCKQTY + &
			BM_RELATION::QUANTITY
		TRAN.TYPE$ = "OE"
	ELSE
		OE_ORDERLINE::SHPQTY = OE_ORDERLINE::SHPQTY + &
			BM_RELATION::QUANTITY
		TRAN.TYPE$ = "SA"
	END IF

	WHEN ERROR IN
		UPDATE #OE_ORDERLINE.CH%
	USE
		CONTINUE 21945 IF ERR = 155%
		FILENAME$ = "OE_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	V% = IC_WRIT_35BALANCE(OE_ORDERLINE::PRODUCT, &
		OE_ORDERJOUR::LOCATION, TRAN.TYPE$, BM_RELATION::QUANTITY)

	GOTO 21950

21945	OE_ORDERLINE::ORDNUM	= OE_ORDERJOUR::ORDNUM
	OE_ORDERLINE::LIN	= "NEWL"
	OE_ORDERLINE::PRODUCT	= BM_RELATION::COMPONENT
	OE_ORDERLINE::ORDQTY	= BM_RELATION::QUANTITY
	OE_ORDERLINE::COST	= MO_ORDERLINE::COST
	OE_ORDERLINE::REQDATE   = MO_ORDERLINE::REQDATE

	IF LEFT(PD_PRODUCT_EXAM::LABEL, 2%) <> "BK"
	THEN
		OE_ORDERLINE::PRICE = &
			PC_READ_PRICE(BM_RELATION::COMPONENT, &
			OE_ORDERJOUR::LOCATION, &
			AR_35CUSTOM_EXAM::TTYPE, &
			OE_ORDERJOUR::ORDDATE, "", "", "")
	ELSE
		OE_ORDERLINE::PRICE = 0.0
	END IF

	OE_ORDERLINE::PROMO	= 0.0
	OE_ORDERLINE::DISCOUNT	= 0.0
	OE_ORDERLINE::MISCH	= 0.0
	OE_ORDERLINE::MISCH2	= 0.0
	OE_ORDERLINE::NOTES1	= ""
	OE_ORDERLINE::NOTES2	= ""
	OE_ORDERLINE::SUBACCT	= ""

	IF MO_ORDERLINE::SHPQTY = 0.0
	THEN
		OE_ORDERLINE::SHPQTY	= 0.0
		OE_ORDERLINE::BCKQTY	= BM_RELATION::QUANTITY
		TRAN.TYPE$ = "OE"
	ELSE
		OE_ORDERLINE::SHPQTY	= BM_RELATION::QUANTITY
		OE_ORDERLINE::BCKQTY	= 0.0
		TRAN.TYPE$ = "SA"
	END IF

	PUT #OE_ORDERLINE.CH%

	V% = IC_WRIT_35BALANCE(OE_ORDERLINE::PRODUCT, &
		OE_ORDERJOUR::LOCATION, TRAN.TYPE$, OE_ORDERLINE::ORDQTY)

21950	GOTO GetComp IF LEVEL% = 0% OR X% = 5%

	GOTO GetComp

 RetBOM:
	RETURN

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	MO_WRIT_ORDER = CMC$_UNTERROR
	GOTO ExitFunction

32767	END FUNCTION