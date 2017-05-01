1	%TITLE "Print Register"
	%SBTTL "PO_RPRT_REGISTER"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1992 BY
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
	! ID:PO001
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Register\* option
	!	prints the Register file.
	!	.lm -5
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_RPRT_REGISTER/LINE
	!	$ LINK/EXE=PO_EXE: PO_RPRT_REGISTER, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PO_RPRT_REGISTER.OBJ;*
	!
	! AUTHOR:
	!
	!	09/18/92 - Dan Perkins
	!
	! MODIFICATION HISTORY:
	!
	!	09/21/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	10/16/93 - Frank F. Starman
	!		Added ONLY_OPEN$ flag.
	!
	!	01/25/94 - Kevin Handy
	!		Clean up (Check)
	!
	!	10/05/94 - Kevin Handy
	!		Added rounding to calculations of BALANCE.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	08/07/95 - Kevin Handy
	!		Increase dimension of TEMP() from 100 to 400.
	!		LL Was getting subscript out of range error.
	!
	!	09/09/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/26/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	04/04/2001 - Kevin Handy
	!		Add wildcard location.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include SCOPE.COM
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	MAP (PO_REG_LINE)	PO_REG_LINE_CDD		PO_REG_LINE

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.HB"
	MAP (PO_REG_SUB_LINE)	PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	DECLARE			AP_VENDOR_CDD		AP_VENDOR_EXAM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	!
	! Declare external functions
	!
	EXTERNAL LONG     FUNCTION AP_EXAM_VENDOR
	EXTERNAL LONG     FUNCTION PD_EXAM_PRODUCT

	DECLARE STRING TEMP(400%)

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	.TS 55
	!	^*(01) Sort by	N,T,V,P,B\*
	!	.B
	!	.LM +5
	!	The ^*Sort by\* field
	!	determines the sorting
	!	order in which the report will print.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*N\* - Purchase Order Number
	!	.te
	!	^*T\* - Purchase Order Type
	!	.te
	!	^*V\* - Vendor Number
	!	.te
	!	^*P\* - Product Number
	!	.te
	!	^*B\* - Batch Number
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort by>Register
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field enters the item from which the
	!	report will begin. The value entered must be in agreement with the value
	!	in field (01) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Register
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field specifies the item at which the
	!	report will end. The value entered must be in agreement with
	!	field (01) Sort by.
	!	.b
	!	A blank field will cause the report to end with the last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Order Status Detail
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field enters a wildcard value which
	!	will cause selected purchase orders to be printed in the journal report.
	!	.b
	!	If this field is blank, all purchase orders in the batch will be printed
	!	in the journal report.
	!	.lm -5
	!
	! Index:
	!
	!--

	ONLY_OPEN$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 132%)

	!++
	! Abstract:FLD05
	!	^*(05) Only Open Balances\*
	!	.b
	!	.lm +5
	!	The ^*Only Open Balances\* field enters ^*YES\*
	!	if only open purchase order should be printed. The flag should
	!	be NO to print all orders.
	!	.lm -5
	!
	! Index:
	!	.x Only Open Balances>Register
	!
	!--

	WILD_LOCATION$ = TRM$(UTL_REPORTX::OPTDEF(5%))

	!++
	! Abstract:FLD06
	!	^*(06) Wildcard Location\*
	!	.b
	!	.lm +5
	!	Selects which location(s) should be included in the
	!	list of PO's.
	!	.b
	!	A blank entry will select all PO's.
	!	.lm -5
	!
	! Index:
	!	.x Only Open Balances>Register
	!
	!--

300	!
	! Open Order Register file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.OPN"
	USE
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Order Line file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.OPN"
	USE
		FILENAME$ = "PO_REG_SUB_LINE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	!
	! Select which method to sort by
	!
	SELECT SORTBY$

	CASE "N"
		K_NUM% = 0%
		TITLE$(1%) = "REGISTER DETAIL REPORT BY ORDER"

		!
		! Routine to load left justified spaces into FROM_ITEM
		! and TO_ITEM if any order numbers are entered as ranges
		!
		FROM_ITEM$ = SPACE$(LEN(PO_REG_LINE::PO) - &
			LEN(FROM_ITEM$)) + FROM_ITEM$ &
			IF FROM_ITEM$ <> ""

		TO_ITEM$ = SPACE$(LEN(PO_REG_LINE::PO) - &
			LEN(TO_ITEM$)) + TO_ITEM$ &
			IF TO_ITEM$ <> ""

	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = "REGISTER DETAIL REPORT BY PO TYPE"

	CASE "V"
		K_NUM% = 2%
		TITLE$(1%) = "REGISTER DETAIL REPORT BY VENDOR NUMBER"

	CASE "B"
		K_NUM% = 3%
		TITLE$(1%) = "REGISTER DETAIL REPORT BY BATCH NUMBER"

	CASE "P"
		K_NUM% = 4%
		TITLE$(1%) = "REGISTER DETAIL REPORT BY PRODUCT NUMBER"

	END SELECT

	TITLE$(2%) = "Purchase Order System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "PO#         Line  Type  Vendor      Name               " + &
		"             OrderDate   Location"

	TITLE$(5%) = "                     Product         Description" + &
		"                     UOM  Batch"

	TITLE$(6%) = "                        Action  ActionDate           Qty" + &
		"         Price  Subacct     Account             Batch"

	TITLE$(7%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	LOOP% = 0%
	BALANCE = 0.0

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PO_REG_LINE.CH%, KEY #K_NUM%
		ELSE
			FIND #PO_REG_LINE.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next Register record
	!
	WHEN ERROR IN
		GET #PO_REG_LINE.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record if should be printed
	!
	SELECT SORTBY$

	CASE "N"
		GOTO ExitProgram IF (PO_REG_LINE::PO > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF COMP_STRING(EDIT$( &
				PO_REG_LINE::PO, -1%), WLDCRD$) = 0%
		END IF

	CASE "T"
		GOTO ExitProgram IF (PO_REG_LINE::PO_TYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF COMP_STRING(EDIT$( &
				PO_REG_LINE::PO_TYPE, -1%), WLDCRD$) = 0%
		END IF

	CASE "V"
		GOTO ExitProgram IF (PO_REG_LINE::VENDOR > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF COMP_STRING(EDIT$( &
				PO_REG_LINE::VENDOR, -1%), WLDCRD$) = 0%
		END IF

	CASE "B"
		GOTO ExitProgram IF (PO_REG_LINE::BATCH > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF COMP_STRING(EDIT$( &
				PO_REG_LINE::BATCH, -1%), WLDCRD$) = 0%
		END IF

	CASE "P"
		GOTO ExitProgram IF (PO_REG_LINE::PRODUCT > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF COMP_STRING(EDIT$( &
				PO_REG_LINE::PRODUCT, -1%), WLDCRD$) = 0% &
		END IF

	END SELECT

	IF WILD_LOCATION$ <> ""
	THEN
		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PO_REG_LINE::FROMLOCATION, -1%), WILD_LOCATION$) = 0%
	END IF

	!
	! Get the vendor name
	!
	V% = AP_EXAM_VENDOR(PO_REG_LINE::VENDOR, AP_VENDOR_EXAM)

	!
	! Get the product description
	!
	V% = PD_EXAM_PRODUCT(PO_REG_LINE::PRODUCT, PD_PRODUCT_EXAM)

	!
	! Print out one line
	!
	TEXT$ = CONV_STRING(PO_REG_LINE::PO, CMC$_LEFT) + "  " + &
		PO_REG_LINE::PO_LINE + "  " + &
		PO_REG_LINE::PO_TYPE + "    " + &
		PO_REG_LINE::VENDOR + "  " + &
		LEFT(AP_VENDOR_EXAM::VENNAM, 30%) + "  " + &
		PRNT_DATE(PO_REG_LINE::ORDDATE, 8%) + "  " + &
		PO_REG_LINE::FROMLOCATION

	IF ONLY_OPEN$ = "Y"
	THEN
		LOOP% = LOOP% + 1%
		TEMP(LOOP%) = TEXT$
	ELSE
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	TEXT$ = CONV_STRING(PO_REG_LINE::PO, CMC$_LEFT) + "  " + &
		PO_REG_LINE::PO_LINE + "     " + &
		PO_REG_LINE::PRODUCT + "  " + &
		LEFT(PD_PRODUCT_EXAM::DESCRIPTION, 30%) + "  " + &
		PO_REG_LINE::UOM + "   " + &
		PO_REG_LINE::BATCH

	IF ONLY_OPEN$ = "Y"
	THEN
		LOOP% = LOOP% + 1%
		TEMP(LOOP%) = TEXT$
	ELSE
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	GOTO ExitProgram IF UTL_REPORTX::STAT

17100	!
	! Check current Register Sub Line record
	!
	WHEN ERROR IN
		FIND #PO_REG_SUB_LINE.CH%, &
			KEY #0% EQ PO_REG_LINE::PO + PO_REG_LINE::PO_LINE, &
			REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "PO_REG_SUB_LINE"
		CONTINUE HelpError
	END WHEN

 GetLine:
17120	WHEN ERROR IN
		GET #PO_REG_SUB_LINE.CH%, REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 11%
		FILENAME$ = "PO_REG_SUB_LINE"
		CONTINUE HelpError
	END WHEN

	GOTO PrintBlank IF PO_REG_SUB_LINE::PO <> PO_REG_LINE::PO OR &
		PO_REG_SUB_LINE::PO_LINE <> PO_REG_LINE::PO_LINE

	SELECT PO_REG_SUB_LINE::PO_ACTION

	CASE "01"
		TTYPE$ = "O"
		BALANCE = FUNC_ROUND(BALANCE + PO_REG_SUB_LINE::QTY, 4%)

	CASE "02"
		TTYPE$ = "R"
		BALANCE = FUNC_ROUND(BALANCE - PO_REG_SUB_LINE::QTY, 4%)

	CASE "03"
		TTYPE$ = "C"
		BALANCE = FUNC_ROUND(BALANCE - PO_REG_SUB_LINE::QTY, 4%)

	CASE "09"
		TTYPE$ = "I"

	END SELECT

	!
	! Print out one line
	!
	TEXT$ = CONV_STRING(PO_REG_SUB_LINE::PO, CMC$_LEFT) + "  " + &
		PO_REG_SUB_LINE::PO_LINE + "        " + &
		TTYPE$ + "       " + &
		PRNT_DATE(PO_REG_SUB_LINE::ACTION_DATE, 8%) + "  " + &
		FORMAT$(PO_REG_SUB_LINE::QTY, "#,###,###.##") + "  " + &
		FORMAT$(PO_REG_SUB_LINE::PRICE, "#,###,###.##") + "  " + &
		PO_REG_SUB_LINE::SUBACCT + "  " + &
		PO_REG_SUB_LINE::ACCOUNT + "  " + &
		PO_REG_SUB_LINE::BATCH

	IF ONLY_OPEN$ = "Y"
	THEN
		LOOP% = LOOP% + 1%
		TEMP(LOOP%) = TEXT$
	ELSE
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO GetLine

 PrintBlank:

	IF ONLY_OPEN$ = "Y"
	THEN
		IF BALANCE > 0.0
		THEN
			FOR I% = 1% TO LOOP%
				CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
					TEMP(I%), 0%)
			NEXT I%
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		END IF
		LOOP% = 0%
		BALANCE = 0.0
	ELSE
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	END IF


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
	!***************************************************************
	! Help Message for an error
	!***************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

19000	!***************************************************************
	! ERROR TRAPPING
	!***************************************************************

	!
	! Resume to display untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
