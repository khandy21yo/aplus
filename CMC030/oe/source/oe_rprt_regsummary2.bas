1	%TITLE "Order Status Summary"
	%SBTTL "OE_RPRT_REGSUMMARY2"
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
	! ID:OE015
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Order Status Summary\* Report contains the following information:
	!	.table 3,25
	!	.te
	!	Document Number	Sales Type
	!	.te
	!	Sales Category	Customer Number
	!	.te
	!	Customer Name	Customer PO Number
	!	.te
	!	Order Date	Location
	!	.te
	!	Ship Via	Line _#
	!	.te
	!	Product Number	Product Description
	!	.te
	!	Quantity Ordered	Quantity Shipped
	!	.te
	!	Quantity Backordered	Quantity Canceled
	!	.te
	!	Balance
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Order Status Summary
	!	.x Order Status Summary>Report
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_RPRT_REGSUMMARY2/LINE
	!	$ LINK/EXE=OE_EXE: OE_RPRT_REGSUMMARY2, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_RPRT_REGSUMMARY2.OBJ;*
	!
	! AUTHOR:
	!
	!	10/12/94 - Kevin Handy
	!		Taken from OE_RPRT_REGSUMMARY
	!
	! MODIFICATION HISTORY:
	!
	!	10/13/94 - Kevin Handy
	!		Added sorting by Alpha
	!
	!	10/14/94 - Kevin Handy
	!		Fixed from-to-wildcard on alpha sort
	!
	!	11/01/94 - Kevin Handy
	!		Modifications to make everything round consistantly.
	!
	!	03/28/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico standards.
	!		Added the number of days between the req date
	!		and the ship date.
	!
	!	05/18/95 - Kevin Handy
	!		Clean up (Check)
	!		Reformat source closer to 80 columns.
	!
	!	09/09/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/14/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include codes.inc
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	MAP	(OE_REGLINE)	OE_REGLINE_CDD	OE_REGLINE
	DECLARE			OE_REGLINE_CDD		OE_REGLINE_READ

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_CUSTOM_EXAM

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	MAP (OE_REGHEADER)	OE_REGHEADER_CDD	OE_REGHEADER

	RECORD TEMP_SORT_CDD
		STRING CUSNUM = 10%
		STRING ALPSRT = 20%
		STRING ORDNUM = 10%
	END RECORD

	MAP (TEMP_SORT) TEMP_SORT_CDD TEMP_SORT

	!
	! Declare external functions
	!
	EXTERNAL LONG    FUNCTION PD_EXAM_PRODUCT
	EXTERNAL LONG    FUNCTION AR_EXAM_CUSTOM

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
	!	.x Sort>Order Status Summary Report
	!	^*(01) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field
	!	determines the order in which the
	!	report will print.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*D\* - Document Number
	!	.te
	!	^*T\* - Sale Type
	!	.te
	!	^*C\* - Sale Category
	!	.te
	!	^*N\* - Customer Number
	!	.end table
	!	A value is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field enters an item from which the
	!	report will begin printing.
	!	The value entered must be in agreement with
	!	field (01) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Order Status Summary Report
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field enters an item with which
	!	the report will end printing.
	!	The value entered must be in agreement with
	!	field (01) Sort by.
	!	.b
	!	A blank field will cause the report to end with the last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Order Status Summary Report
	!
	!--


	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated items to be
	!	printed by entering a wildcard.  The value entered must be in agreement with
	!	field (01) Sort by.
	!	.b
	!	See Appendix B for more information regarding wildcarding techniques.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Order Status Summary Report
	!
	!--

	SHIP_SINCE$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(4%))

	!++
	! Abstract:FLD05
	!	.x Backorders Only>Order Status Summary Report
	!	^*(05) Backorders Only\*
	!	.b
	!	.lm +5
	!	The ^*Backorders Only\* field causes
	!	the report to print information which will include backorders
	!	only or all orders.
	!	.b
	!	Valid setting are:
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes, Backorders only
	!	.te
	!	^*N\* - No, All orders
	!	.end table
	!	A setting is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--


300	!
	! Open Order Register file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.OPN"
	USE
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.OPN"
	USE
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	! Select which method to sort by
	!
	SELECT SORTBY$

	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = " REGISTER SUMMARY BY SALE TYPE"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = " REGISTER SUMMARY BY SALE CATEGORY"

	CASE "N"
		K_NUM% = 3%
		TITLE$(1%) = " REGISTER SUMMARY BY CUSTOMER NUMBER"

	CASE "A"
		K_NUM% = -1%
		TITLE$(1%) = " REGISTER SUMMARY BY CUSTOMER NAME"

	CASE ELSE
		K_NUM% = 0%
		TITLE$(1%) = " REGISTER SUMMARY BY DOCUMENT NUMBER"
		FROM_ITEM$ = SPACE$(LEN(OE_REGHEADER::ORDNUM) - &
			LEN(FROM_ITEM$)) + FROM_ITEM$
		TO_ITEM$ = SPACE$(LEN(OE_REGHEADER::ORDNUM) - &
			LEN(TO_ITEM$)) + TO_ITEM$

	END SELECT

500	!*******************************************************************
	! Create sort file by alpha if necessary
	!*******************************************************************

	GOTO 600 UNLESS K_NUM% = -1%

510	CALL ASSG_CHANNEL(TEMP_SORT.CH%, STATUS%)

	OPEN "TEMP_SORT.TMP" FOR OUTPUT AS FILE TEMP_SORT.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP TEMP_SORT, &
		TEMPORARY, &
		PRIMARY KEY (TEMP_SORT::ALPSRT, TEMP_SORT::ORDNUM), &
		BUFFER 32%, &
		ACCESS MODIFY, &
		ALLOW NONE

520	RESET #OE_REGHEADER.CH%

530	WHEN ERROR IN
		GET #OE_REGHEADER.CH%, REGARDLESS
	USE
		CONTINUE 600 IF ERR = 11%
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

540	IF (AR_CUSTOM_EXAM::CUSNUM <> OE_REGHEADER::CUSNUM)
	THEN
		V% = AR_EXAM_CUSTOM(OE_REGHEADER::CUSNUM, AR_CUSTOM_EXAM)

		IF V% <> CMC$_NORMAL
		THEN
			AR_CUSTOM_EXAM::CUSNUM = OE_REGHEADER::CUSNUM
			AR_CUSTOM_EXAM::ALPSRT = ""
		END IF
	END IF

	GOTO 530 IF (AR_CUSTOM_EXAM::ALPSRT > TO_ITEM$) &
		AND TO_ITEM$ <> ""

	GOTO 530 IF (AR_CUSTOM_EXAM::ALPSRT < FROM_ITEM$)

	GOTO 530 IF COMP_STRING(EDIT$(AR_CUSTOM_EXAM::ALPSRT, -1%), &
		WLDCRD$) = 0% AND WLDCRD$ <> ""

550	TEMP_SORT::CUSNUM = OE_REGHEADER::CUSNUM
	TEMP_SORT::ALPSRT = AR_CUSTOM_EXAM::ALPSRT
	TEMP_SORT::ORDNUM = OE_REGHEADER::ORDNUM

	PUT #TEMP_SORT.CH%

	GOTO 530

600	!
	!
16000	TITLE$(2%) = "Shipped Since " + PRNT_DATE(SHIP_SINCE$, 8%)
	TITLE$(3%) = ""

	!
	! Heading
	!
	!	'     1234567890123456789012345678901234567890
	TITLE$(4%) = "Doc#        ST SalCat CusNumber    " + &
		"CusName                          CustPo#        " + &
		"OrdDate     Location       Shipvia"
	TITLE$(5%) = "                Line   Product        " + &
		"Descr                 QtyOrd  QtyShip   QtyCan  " + &
		"QtyBack  Balance   OrderDt    ShipDt    Days"
	TITLE$(6%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	!
	! If from item is blank then reset Register file
	! else try to find the first record
	!
	WHEN ERROR IN
		IF K_NUM% <> -1%
		THEN
			IF FROM_ITEM$ = ""
			THEN
				RESET #OE_REGHEADER.CH%, KEY #K_NUM%
			ELSE
				FIND #OE_REGHEADER.CH%, &
					KEY #K_NUM% GE FROM_ITEM$, &
					REGARDLESS
			END IF
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

	PRINT_HEADER% = -1%

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next Order Register record
	!
	WHEN ERROR IN
		IF K_NUM% <> -1%
		THEN
			GET #OE_REGHEADER.CH%, REGARDLESS
		ELSE
			GET #TEMP_SORT.CH%

			GET #OE_REGHEADER.CH%, &
				KEY #0% EQ TEMP_SORT::ORDNUM, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record if should be printed
	!
	SELECT SORTBY$

	CASE "C"
		GOTO ExitProgram IF (OE_REGHEADER::ORDCAT > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$(OE_REGHEADER:: &
			CUSNUM, -1%), WLDCRD$) = 0% AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitProgram IF (OE_REGHEADER::ORDTYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$(OE_REGHEADER:: &
			ORDTYPE, -1%), WLDCRD$) = 0% AND WLDCRD$ <> ""

	CASE "N"
		GOTO ExitProgram IF (OE_REGHEADER::CUSNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$(OE_REGHEADER:: &
			CUSNUM, -1%), WLDCRD$) = 0% AND WLDCRD$ <> ""

	CASE "A"
		!

	CASE ELSE
		GOTO ExitProgram IF (OE_REGHEADER::ORDNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$(OE_REGHEADER:: &
			ORDNUM, -1%), WLDCRD$) = 0% AND WLDCRD$ <> ""

	END SELECT

	PRINT_HEADER% = 0%

	!
	! Read customer name
	!
	V% = AR_EXAM_CUSTOM(OE_REGHEADER::CUSNUM, AR_CUSTOM_EXAM)

	IF V% <> CMC$_NORMAL
	THEN
		AR_CUSTOM_EXAM::CUSNAM = "(Undefined)"
	END IF

	HEADER$ = CONV_STRING(OE_REGHEADER::ORDNUM, CMC$_LEFT) + "  " + &
		OE_REGHEADER::ORDTYPE + " " + &
		OE_REGHEADER::ORDCAT + "   " + &
		OE_REGHEADER::CUSNUM + "   " + &
		LEFT$(AR_CUSTOM_EXAM::CUSNAM, 30%) + "   " + &
		OE_REGHEADER::CUSTPO + "     " + &
		PRNT_DATE(OE_REGHEADER::ORDDATE, 8%) + "  " + &
		OE_REGHEADER::LOCATION + "           " + &
		OE_REGHEADER::SHIPVIA

	NEXT_LINE$ = "   "

 ReadRegline:
17200	!
	! Try to find any line for the header
	!
	!
	! Set initial value
	!
	IND% = 0%
	QTY(I%) = 0.0 FOR I% = 1% TO 10%
	ORIG_ORDNUM = ORDNUM
	REQ_DATE$ = ""
	SHP_DATE$ = ""

	!
	! Get OE_REGLINE file
	!
17300	WHEN ERROR IN
		FIND #OE_REGLINE.CH%, &
			KEY #0% GT OE_REGHEADER::ORDNUM + NEXT_LINE$, &
			REGARDLESS
	USE
		CONTINUE NewOrder IF ERR = 155%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

17320	WHEN ERROR IN
		GET #OE_REGLINE.CH%, REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 11%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

	IF (OE_REGLINE::ORDNUM <> OE_REGHEADER::ORDNUM) OR &
		((OE_REGLINE::LIN <> LAST_LIN$) AND (IND% <> 0%))
	THEN
		GOTO NewOrder IF IND% = 0%
		GOTO ExitFunction
	END IF

17330	OE_REGLINE_READ = OE_REGLINE IF IND% = 0%
	LAST_LIN$ = OE_REGLINE::LIN

	SELECT OE_REGLINE::TRANTYPE

	CASE "01" ! Order

		IND% = 1%
		REQ_DATE$ = OE_REGLINE::TDATE

		QTY(9%) = FUNC_ROUND(QTY(9%) + &
			(OE_REGLINE::PRICE - &
			OE_REGLINE::PROMO) * &
			(1 - OE_REGLINE::DISCOUNT / 100.0) * &
			OE_REGLINE::QTY, 3%)

	CASE "02", "03" ! Ship, Cancel

		IND% = VAL%(OE_REGLINE::TRANTYPE)
		SHP_DATE$ = OE_REGLINE::TDATE &
			IF SHP_DATE$ < OE_REGLINE::TDATE

	CASE "12"
		IND% = 4%

	CASE "13"
		IND% = 5%

	CASE ELSE
		IND% = 10%

	END SELECT

	QTY(IND%) = FUNC_ROUND(QTY(IND%) + OE_REGLINE::QTY, 3%)

	!
	! Go for next line
	!
	GOTO 17320

 ExitFunction:

	!
	! Balance - remaining qty to ship
	!
	QTY(0%) = FUNC_ROUND(QTY(1%) - QTY(2%) - QTY(3%), 3%)
	QTY(0%) = 0.0 IF FUNC_ROUND(QTY(0%), 3%) <= 0.0

	!
	! Balance - remaining qty to ship incl journals qty
	!
	QTY(6%) = FUNC_ROUND(QTY(1%) - QTY(2%) - QTY(3%) - QTY(4%) - &
		QTY(5%), 3%)
	QTY(6%) = 0.0 IF FUNC_ROUND(QTY(6%), 3%) <= 0.0

	!
	! Back Orders
	!
	IF DATE_TODAY > REQ_DATE$ AND QTY(1%) <> 0.0
	THEN
		QTY(7%) = QTY(0%)
		QTY(8%) = QTY(6%)
	END IF

	NEXT_LINE$ = OE_REGLINE_READ::LIN

	!
	! Is this something we want to see on the report?
	!
	IF QTY(7%) = 0.0
	THEN
		IF SHP_DATE$ < SHIP_SINCE$
		THEN
			GOTO ReadRegline
		END IF
	END IF

	!
	! Read description for the product
	!
	V% = PD_EXAM_PRODUCT(OE_REGLINE_READ::PRODUCT, PD_PRODUCT_EXAM)

	!
	! Print header if it is the first line for the order
	!
	IF PRINT_HEADER% = 0%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), HEADER$, 0%)
		PRINT_HEADER% = -1%
	END IF

	!
	! Print line with backorders
	!
	TEXT$ = CONV_STRING(OE_REGHEADER::ORDNUM, CMC$_LEFT) + &
		"     " + &
		OE_REGLINE_READ::LIN + "   " + &
		OE_REGLINE_READ::PRODUCT + " " + &
		LEFT$(PD_PRODUCT_EXAM::DESCRIPTION, 20%) + &
		FORMAT$(QTY(1%), "########") + " " + &
		FORMAT$(QTY(2%), "########") + " " + &
		FORMAT$(QTY(3%), "########") + " " + &
		FORMAT$(QTY(7%), "########") + " " + &
		FORMAT$(QTY(0%), "########") + " " + &
		PRNT_DATE(REQ_DATE$, 8%) + " " + &
		PRNT_DATE(SHP_DATE$, 8%)

	IF (REQ_DATE$ > "00000000") AND &
		(SHP_DATE$ > "00000000")
	THEN
		AGE% = DATE_DAYCODE(SHP_DATE$) - DATE_DAYCODE(REQ_DATE$)
		TEXT$ = TEXT$ + FORMAT$(AGE%, "######")
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ReadRegline

 NewOrder:
	IF PRINT_HEADER% = -1%
	THEN
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
