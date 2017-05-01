1	%TITLE "Register by Make"
	%SBTTL "MO_RPRT_REGMAKE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1992 BY
	!
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be
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
	! ID:MO0??
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This reports prints out orders by make.
	!	.b
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS MO_SOURCE:MO_RPRT_REGMAKE/LINE
	!	$ LINK/EXE=MO_EXE: MO_RPRT_REGMAKE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE MO_RPRT_REGMAKE.OBJ;*
	!
	! AUTHOR:
	!
	!	11/16/92 - Dan Perkins
	!
	! MODIFICATION HISTORY:
	!
	!	11/17/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	12/02/92 - Dan Perkins
	!		Added extended price column to report.
	!
	!	12/08/92 - Frank F. Starman
	!		Correct make price total.
	!
	!	12/09/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	11/17/95 - Kevin Handy
	!		Clean up source code.
	!
	!	07/10/96 - Kevin Handy
	!		Reformat source code.
	!		Changed title line to match what is actually
	!		printed.
	!		Pulled back totals so they line up with detail.
	!
	!	07/25/96 - Kevin Handy
	!		Changed references from MO_REGHEADER to
	!		OE_REGHEADER, since that's what actually was
	!		being read.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/19/98 - Kevin Handy
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
	! Include CMC codes
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINE.HB"
	MAP (MO_REGLINE)	MO_REGLINE_CDD		MO_REGLINE
	DECLARE			MO_REGLINE_CDD		MO_REGLINE_TEST

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	DECLARE			OE_REGHEADER_CDD	OE_REGHEADER_READ

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	!
	! Declare external functions
	!
	EXTERNAL LONG	FUNCTION OE_READ_REGHEADER
	EXTERNAL LONG	FUNCTION AR_EXAM_CUSTOM

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
	!	^*(01) From Make\*
	!	.b
	!	.lm +5
	!	The ^*From Make\* field begins the report
	!	with a particular make.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	make in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Make\*
	!	.b
	!	.lm +5
	!	The ^*To Make\* field ends printing
	!	with a particular make.
	!	.b
	!	A blank field will cause the report to end with the last
	!	make in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects
	!	designated makes to be printed by entering a "wildcard"
	!	using the Wildcarding Technique.
	!	.b
	!	For information on "Wildcarding" techniques refer to Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

	DISPLAY_QTY$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	.x Display Quantity
	!	^*(04) Display Quantity
	!	.b
	!	.lm +5
	!	The ^*Display Quantity\* field selects
	!	designated quantities to be printed.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*O\* - Ordered Quantity
	!	.te
	!	^*S\* - Shipped Quantity
	!	.te
	!	^*C\* - Cancelled Quantity
	!	.te
	!	^*B\* - Backordered Quantity
	!	.te
	!	^*Q\* - Quantity Balance
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

310	!
	! Open Order Line file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINE.OPN"
	USE
		FILENAME$ = "MO_REGLINE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "REGISTER REPORT BY MAKE"

	SELECT DISPLAY_QTY$

	CASE "O"
		TITLE$(2%) = "QUANTITY ORDERED"
		QD$ = " QtyOrd"
		QD% = 1%

	CASE "S"
		TITLE$(2%) = "QUANTITY SHIPPED"
		QD$ = "QtyShip"
		QD% = 2%

	CASE "C"
		TITLE$(2%) = "QUANTITY CANCELLED"
		QD$ = " QtyCan"
		QD% = 3%

	CASE "B"
		TITLE$(2%) = "QUANTITY BACK ORDERED"
		QD$ = "QtyBack"
		QD% = 7%

	CASE "Q"
		TITLE$(2%) = "BALANCE"
		QD$ = "Balance"
		QD% = 0%

	END SELECT

	!
	! Heading
	!
	TITLE$(3%) = "Manufacture to Order System"
	TITLE$(4%) = ""

	TITLE$(5%) = "Make       Year MType MSize MCode CusNumber  " + &
		"CusName                   Quote#     Line      " + &
		"Qty  ReqDate      " + QD$

	TITLE$(6%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************
	GOTO ExitProgram IF UTL_REPORTX::STAT

	PRINTED% = 0%
	MAKETOTALPRICE, TOTALPRICE = 0.0

	!
	! If from category blank then reset REGLINE file
	! else try to find the first record
	!
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #MO_REGLINE.CH%, KEY #3%
		ELSE
			FIND #MO_REGLINE.CH%, KEY #3% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "MO_REGLINE"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	WHEN ERROR IN
		GET #MO_REGLINE.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "MO_REGLINE"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record if should be printed
	!
	GOTO ExitProgram IF (MO_REGLINE::MAKE > TO_ITEM$) &
		AND TO_ITEM$ <> ""

	GOTO GetNextRec IF COMP_STRING(EDIT$( &
		MO_REGLINE::MAKE, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

	GOSUB PrintLine IF MO_REGLINE::ORDNUM <> MO_REGLINE_TEST::ORDNUM OR &
		MO_REGLINE::LIN <> MO_REGLINE_TEST::LIN

	GOSUB MakeTotal IF PRINTED% AND ( &
		MO_REGLINE::MAKE <> MO_REGLINE_TEST::MAKE OR &
		MO_REGLINE::YEAR <> MO_REGLINE_TEST::YEAR OR &
		MO_REGLINE::MTYPE <> MO_REGLINE_TEST::MTYPE OR &
		MO_REGLINE::MSIZE <> MO_REGLINE_TEST::MSIZE OR &
		MO_REGLINE::MODELCODE <> MO_REGLINE_TEST::MODELCODE)

	MO_REGLINE_TEST = MO_REGLINE

	!
	! See if there are quantities to print
	!
	SELECT MO_REGLINE::TRANTYPE

	CASE "01" ! Order
		IND% = 1%
		REQ_DATE$ = MO_REGLINE::TDATE
		TRADE_IN% = -1% IF MO_REGLINE::QTY < 0.0

	CASE "02", "03" ! Ship, Cancel
		IND% = VAL%(MO_REGLINE::TRANTYPE)

	CASE "12"
		IND% = 4%

	CASE "13"
		IND% = 5%

	CASE ELSE
		IND% = 10%

	END SELECT

	QTY(IND%) = QTY(IND%) + MO_REGLINE::QTY

	GOTO GetNextRec

 PrintLine:
	!
	! Test Quantities
	!
	! Balance - remaining qty to ship
	!
	QTY(0%) = FUNC_ROUND(QTY(1%) - QTY(2%) - QTY(3%), 2%)
	QTY(0%) = 0.0 IF QTY(0%) < 0.0

	!
	! Balance - remaining qty to ship incl journals qty
	!
	QTY(6%) = FUNC_ROUND(QTY(1%) - QTY(2%) - QTY(3%) - &
		QTY(4%) - QTY(5%), 2%)
	QTY(6%) = 0.0 IF QTY(6%) < 0.0

	!
	! Back Orders
	!
	IF DATE_TODAY > REQ_DATE$ AND QTY(1%) <> 0.0
	THEN
		QTY(7%) = QTY(0%)
		QTY(8%) = QTY(6%)
	END IF

	GOTO EndPrintLine IF FUNC_ROUND(QTY(QD%), 3%) = 0.0 OR TRADE_IN%

	!
	! Get the RegHeader record
	!
	IF OE_READ_REGHEADER(MO_REGLINE_TEST::ORDNUM, OE_REGHEADER_READ) <> &
		CMC$_NORMAL
	THEN
		OE_REGHEADER_READ::CUSNUM = ""
		OE_REGHEADER_READ::CUSTPO = ""
	END IF

	!
	! Get the Customer record
	!
	IF AR_EXAM_CUSTOM(OE_REGHEADER_READ::CUSNUM, AR_35CUSTOM_EXAM) <> &
		CMC$_NORMAL
	THEN
		AR_35CUSTOM_EXAM::CUSNAM = &
			STRING$(LEN(AR_35CUSTOM_EXAM::CUSNAM), A"?"B)
	END IF

	EXTPRICE = FUNC_ROUND(MO_REGLINE_TEST::PRICE * MO_REGLINE_TEST::QTY, 2%)

	TEXT$ = MO_REGLINE_TEST::MAKE + " " + &
		MO_REGLINE_TEST::YEAR + " " + &
		MO_REGLINE_TEST::MTYPE + "    " + &
		MO_REGLINE_TEST::MSIZE + "  " + &
		MO_REGLINE_TEST::MODELCODE + "  " + &
		OE_REGHEADER_READ::CUSNUM + " " + &
		LEFT$(AR_35CUSTOM_EXAM::CUSNAM, 25%) + " " + &
		CONV_STRING(MO_REGLINE_TEST::ORDNUM, CMC$_LEFT) + " " + &
		MO_REGLINE_TEST::LIN + " " + &
		FORMAT$(QTY(QD%), "########") + " " + &
		PRNT_DATE(REQ_DATE$, 6%) + " " + &
		FORMAT$(EXTPRICE, "#,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TOTAL = TOTAL + QTY(QD%)
	MAKETOTALPRICE = MAKETOTALPRICE + EXTPRICE

	PRINTED% = -1%

 EndPrintLine:
	QTY(I%) = 0.0 FOR I% = 0% TO 10%
	TRADE_IN% = 0%

	RETURN

 MakeTotal:
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	TEXT$ = SPACE$(43%) + &
		"Make Total" + &
		SPACE$(34%) + &
		FORMAT$(TOTAL, "########          ") + &
		FORMAT$(MAKETOTALPRICE, "#,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -2%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -3%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	PRINTED% = 0%
	TOTAL = 0.0
	TOTALPRICE = TOTALPRICE + MAKETOTALPRICE
	MAKETOTALPRICE = 0.0

	RETURN

 ExitTotal:
	GOSUB PrintLine

	GOSUB MakeTotal IF PRINTED%

	TEXT$ = SPACE$(43%) + &
		"Grand Total Price" + &
		SPACE$(44%) + &
		FORMAT$(TOTALPRICE, "##,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

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
