1	%TITLE "IC35BALANCE Update"
	%SBTTL "OE_CONV_IC35BALANCE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
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
	! ID:
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*IC35BALANCE\* program converts the IC35 beginning
	!	balance to the balance of the OE_REGLINE file. The program
	!	was specifically written to solve a particular problem at
	!	King B.
	!	.lm -5
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_CONV_IC35BALANCE/LINE
	!	$ LINK/EXE=OE_EXE: OE_CONV_IC35BALANCE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_CONV_IC35BALANCE.OBJ;*
	!
	! AUTHOR:
	!
	!	11/20/91 - Dan Perkins
	!
	! MODIFICATION HISTORY:
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/13/2000 - Kevin Handy
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
	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	MAP (OE_REGLINE)	OE_REGLINE_CDD		OE_REGLINE
	MAP (OE_REGLINE)	OE_REGLINE_CDD		OE_REGLINE_OLD

	%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.HB"
	MAP (IC_35BALANCE)	IC_35BALANCE_CDD	IC_35BALANCE

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	DECLARE			OE_REGHEADER_CDD	OE_REGHEADER_READ

	!
	! Declare external functions
	!
	EXTERNAL REAL    FUNCTION FUNC_ROUND
	EXTERNAL LONG    FUNCTION OE_READ_REGHEADER
	EXTERNAL STRING  FUNCTION DATE_TODAY

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	INPUT "Convert files for which location"; LOCATION$
	LOCATION$ = EDIT$(LOCATION$, -1%)
	PRINT "Working... Please Wait."

300	!
	! Open Product file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Order Line file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.OPN"
	USE
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

320	!
	! Open Balance file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.UPD"
	USE
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	WHEN ERROR IN
		RESET #PD_PRODUCT.CH%, KEY #0%
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record if should be printed
	!
	TEST_ORDNUM$   = ""
	TEST_LINE$     = ""

17030	WHEN ERROR IN
		FIND #OE_REGLINE.CH%, KEY #1% EQ PD_PRODUCT::PRODUCT_NUM, REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

 GetRegLine:
17050	WHEN ERROR IN
		GET #OE_REGLINE.CH%, REGARDLESS
	USE
		CONTINUE PrintTotal IF ERR = 11%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

	IF OE_REGLINE::PRODUCT <> PD_PRODUCT::PRODUCT_NUM
	THEN
		GOSUB BackOrderTest
		GOTO PrintTotal
	END IF

	IF TEST_ORDNUM$ = "" AND TEST_LINE$ = ""
	THEN
		TEST_ORDNUM$ = OE_REGLINE::ORDNUM
		TEST_LINE$   = OE_REGLINE::LIN
	END IF

	!
	! Get location from OE_REGHEADER
	!
	V% = OE_READ_REGHEADER(OE_REGLINE::ORDNUM, OE_REGHEADER_READ)

	IF OE_REGHEADER_READ::LOCATION <> LOCATION$
	THEN
		GOTO GetRegLine
	ELSE
		LAST_LOCATION$ = OE_REGHEADER_READ::LOCATION
	END IF

	IF OE_REGLINE::ORDNUM + OE_REGLINE::LIN = &
			TEST_ORDNUM$ + TEST_LINE$
	THEN
		GOSUB TestQty
		GOTO GetRegLine
	ELSE
		GOSUB BackOrderTest
		GOSUB TestQty
		GOTO GetRegLine
	END IF

 TestQty:
	!
	! See if there are quantities to print
	!
	SELECT OE_REGLINE::TRANTYPE

		CASE "01" ! Order
			IND% = 1%
			REQ_DATE$ = OE_REGLINE::TDATE

		CASE "02", "03" ! Ship, Cancel
			IND% = VAL(OE_REGLINE::TRANTYPE)

		CASE "12"
			IND% = 4%

		CASE "13"
			IND% = 5%

		CASE ELSE
			IND% = 10%

	END SELECT

	QTY(IND%) = QTY(IND%) + OE_REGLINE::QTY

	RETURN

 BackOrderTest:
	!
	! Balance - remaining qty to ship
	!
	QTY(0%) = QTY(1%) - QTY(2%) - QTY(3%)
	QTY(0%) = 0.0 IF QTY(0%) < 0.0

	!
	! Balance - remaining qty to ship incl journals qty
	!
	QTY(6%) = QTY(1%) - QTY(2%) - QTY(3%) - QTY(4%) - QTY(5%)
	QTY(6%) = 0.0 IF QTY(6%) < 0.0

	!
	! Back Orders
	!
	IF DATE_TODAY > REQ_DATE$ AND QTY(1%) <> 0.0
	THEN
		QTY(7%) = QTY(0%)
		QTY(8%) = QTY(6%)
	END IF

	GOTO EndPrintLine IF QTY(0%) = 0.0

 PrintLine:
	TEXT$ = PD_PRODUCT::PRODUCT_NUM + " " + &
		LEFT$(PD_PRODUCT::DESCRIPTION, 25%) + " " + &
		LAST_LOCATION$ + " " + &
		FORMAT$(QTY(0%), "#,###,###.##")

	PRINT TEXT$

 EndPrintLine:
	PRODUCT1$ = PD_PRODUCT::PRODUCT_NUM
	TEST_ORDNUM$ = OE_REGLINE::ORDNUM
	TEST_LINE$   = OE_REGLINE::LIN
	LOOP% = -1%
	TOTAL = TOTAL + QTY(0%)
	QTY(I%) = 0.0 FOR I% = 0% TO 10%

	RETURN

 PrintTotal:
	GOTO GetNextRec IF NOT LOOP%

17200	WHEN ERROR IN
		GET #IC_35BALANCE.CH%, KEY #0% EQ PRODUCT1$ + &
			LAST_LOCATION$ + "SO"
	USE
		CONTINUE GetNextRec IF ERR = 155% OR ERR = 11%
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

	IC_35BALANCE::PBALANCE = -FUNC_ROUND(TOTAL, 3%)

17210	WHEN ERROR IN
		UPDATE #IC_35BALANCE.CH%
	USE
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

	TEXT$ = PRODUCT1$ + &
		"Total: " + &
		FORMAT$(TOTAL, "#,###,###.##")

	PRINT TEXT$
	PRINT "Record updated"
	TOTAL = 0.0
	LOOP% = 0%

	GOTO GetNextRec

 ExitProgram:

	!
	! Exit to next program or menu
	!
	GOTO 32767

	%PAGE

 HelpError:
	!***************************************************************
	! Help Message for an error
	!***************************************************************
	PRINT ERR
	PRINT ERL
	PRINT FILENAME$
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
