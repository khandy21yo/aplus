1	%TITLE " Aged Backorders Report"
	%SBTTL "OE_RPRT_BACKAGE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
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
	! ID:OE036
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Aged Backorders\* Report contains information on
	!	backordered items in the Order Entry System.
	!	Aging is based on the period selected by the user
	!	when running this report. The period control is in the
	!	control file.
	!	.b
	!	You may select (1)first, (2)second, (3)third, (4)fourth,
	!	(5)fifth period or (A)ll backorders.
	!	.b
	!	The report lists
	!	the items in date sequence (oldest date to most recent) and will reflect
	!	the following information:
	!	.table 3,25
	!	.te
	!	Scheduled Ship Date
	!	.te
	!	Number of Days on Backorder
	!	.te
	!	Document Number
	!	.te
	!	Line
	!	.te
	!	Product
	!	.te
	!	Description
	!	.te
	!	Quantity Backordered
	!	.End table
	!	.lm -5
	!
	! Index:
	!	.x Report>Backorders
	!	.x Backorders Report
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_RPRT_BACKAGE/LINE
	!	$ LINK/EXE=OE_EXE: OE_RPRT_BACKAGE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_RPRT_BACKAGE.OBJ;*
	!
	! Author:
	!
	!	07/10/91 - Val James "Dazed an' Confused" Allen
	!
	! Modification History:
	!
	!	08/30/91 - Deborah K. Fries
	!		Reformatted output printout, added group totals
	!		Improved source code structure.
	!
	!	09/18/91 - Dan Perkins
	!		Substituted SELECT CASE statements for IF - THEN.
	!		Incorporated READ and EXAM functions instead of
	!		opening files.  Checked error trapping.
	!
	!	10/14/91 - Frank F. Starman
	!		Close OE_CONTROL file.
	!
	!	01/16/92 - Dan Perkins
	!		Changed quantities to display integer values.
	!
	!	04/09/92 - Dan Perkins
	!		Use CONV_STRING to lset ORDER NUMBER.
	!
	!	06/02/92 - Dan Perkins
	!		Broker2 variable is now OE_REGHEADER::OPERATOR to
	!		accomodate changes in Regheader file layout.
	!
	!	04/08/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/06/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/22/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include scope.com and codes
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	MAP (OE_REGHEADER)	OE_REGHEADER_CDD	OE_REGHEADER

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	DECLARE			OE_REGLINE_CDD		OE_REGLINE_READ

	%INCLUDE "SOURCE:[OE.OPEN]OE_CONTROL.HB"
	MAP (OE_CONTROL)	OE_CONTROL_CDD		OE_CONTROL

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	MAP (OE_TEMP)	OE_TEMP.DATE$    =  8%, &
			OE_TEMP.ORDER$   = 10%, &
			OE_TEMP.CUSNUM$  = 10%, &
			OE_TEMP.LINE$    =  4%, &
			OE_TEMP.PRODUCT$ = 14%, &
			OE_TEMP.BROKER1$ = 10%, &
			OE_TEMP.BROKER2$ = 10%, &
			OE_TEMP.QTY, &
			OE_TEMP.PRICE, &
			OE_TEMP.NUMDAYS

	!
	! Declare external functions
	!
	EXTERNAL STRING	FUNCTION CONV_STRING
	EXTERNAL LONG	FUNCTION OE_READ_REGLINE
	EXTERNAL LONG	FUNCTION PD_EXAM_PRODUCT

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	CONTROL$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1)

	!++
	! Abstract:FLD01
	!	.x Aging Code
	!	^*(01) Age Code\*
	!	.b
	!	.lm +5
	!	The ^*Age Code\* allows printing of a report
	!	showing only those backordered items with an expected
	!	ship date that falls in the indicated range of the code.
	!	(See Control File in the Utility Section)
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*1\* - First period
	!	.te
	!	^*2\* - Second period
	!	.te
	!	^*3\* - Third period
	!	.te
	!	^*4\* - Fourth period
	!	.te
	!	^*5\* - Fifth period
	!	.te
	!	^*A\* - All backorders in register
	!	.end table
	!	An entry is required in this field.
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

330	!
	! Open the controling file and read the sucker
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_CONTROL.OPN"
	USE
		FILENAME$ = "OE_CONTROL"
		CONTINUE HelpError
	END WHEN

340	WHEN ERROR IN
		GET #OE_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE #OE_CONTROL.CH%
	USE
		FILENAME$ = "OE_CONTROL"
		CONTINUE HelpError
	END WHEN

350	CALL ENTR_3MESSAGE(SCOPE, &
		"Creating temporary file.  Reading work files.", 1% + 16%)

	CALL ASSG_CHANNEL(OE_TEMP.CH%, STAT%)

	OPEN "OE_TEMP.TMP" FOR OUTPUT AS FILE #OE_TEMP.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP OE_TEMP, &
		PRIMARY KEY (OE_TEMP.DATE$, OE_TEMP.ORDER$) &
			DUPLICATES, &
		TEMPORARY, &
		BUFFER 32%, &
		ACCESS MODIFY, ALLOW NONE

	!*******************************************************************
	! Start putting stuff into OE_TEMP file
	!*******************************************************************

	SELECT CONTROL$

	CASE "1"
		LOWRANGE% = 1%
		HIGHRANGE% = OE_CONTROL::AGEPER(0%)

	CASE "2"
		LOWRANGE% = OE_CONTROL::AGEPER(0%) + 1%

		HIGHRANGE% = OE_CONTROL::AGEPER(0%) + &
			OE_CONTROL::AGEPER(1%)

	CASE "3"
		LOWRANGE% = OE_CONTROL::AGEPER(0%) + &
			OE_CONTROL::AGEPER(1%) + 1%

		HIGHRANGE% = OE_CONTROL::AGEPER(2%) + &
			OE_CONTROL::AGEPER(0%) + &
			OE_CONTROL::AGEPER(1%)

	CASE "4"
		LOWRANGE% = OE_CONTROL::AGEPER(0%) + &
			OE_CONTROL::AGEPER(1%) + &
			OE_CONTROL::AGEPER(2%) + 1%

		HIGHRANGE% = OE_CONTROL::AGEPER(0%) + &
			OE_CONTROL::AGEPER(1%) + &
			OE_CONTROL::AGEPER(2%) + &
			OE_CONTROL::AGEPER(3%)

	CASE "5"
		LOWRANGE% = OE_CONTROL::AGEPER(0%) + &
			OE_CONTROL::AGEPER(1%) + &
			OE_CONTROL::AGEPER(2%) + &
			OE_CONTROL::AGEPER(3%) + 1%

		IF OE_CONTROL::AGEPER(4%) = 0%
		THEN
			HIGHRANGE% = 999999%
		ELSE
			HIGHRANGE% = OE_CONTROL::AGEPER(0%) + &
				OE_CONTROL::AGEPER(1%) + &
				OE_CONTROL::AGEPER(2%) + &
				OE_CONTROL::AGEPER(3%) + &
				OE_CONTROL::AGEPER(4%)
		END IF

	CASE "A"
		LOWRANGE% = 0%
		HIGHRANGE% = 999999%

	END SELECT

	TODAYSDATE$ = DATE_TODAY

	Y% = DATE_DAYCODE(TODAYSDATE$)

400	WHEN ERROR IN
		RESET #OE_REGHEADER.CH%
	USE
		CONTINUE ReportTitle IF ERR = 9%
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

 NextHeader:
	WHEN ERROR IN
		GET #OE_REGHEADER.CH%
	USE
		CONTINUE ReportTitle IF ERR = 11% OR ERR = 9%
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

	X% = DATE_DAYCODE(OE_REGHEADER::SDATE)

	CHECK% = (Y% - X%)

	GOTO NextHeader IF CHECK% < LOWRANGE% OR CHECK% > HIGHRANGE%

	OE_TEMP.NUMDAYS = (Y% - X%)

	LINE$ = "    "

 NextRegline:
430	GOTO NextHeader IF OE_READ_REGLINE(OE_REGHEADER::ORDNUM, &
		LINE$, "GT", OE_REGLINE_READ, QTY()) <> CMC$_NORMAL

	LINE$ = OE_REGLINE_READ::LIN

	!
	! Create a temp record if we have a backorder
	!
	IF QTY(7%) > 0.0
	THEN
		OE_TEMP.DATE$    = OE_REGHEADER::SDATE
		OE_TEMP.ORDER$   = OE_REGHEADER::ORDNUM
		OE_TEMP.CUSNUM$  = OE_REGHEADER::CUSNUM
		OE_TEMP.LINE$    = LINE$
		OE_TEMP.PRODUCT$ = OE_REGLINE_READ::PRODUCT
		OE_TEMP.BROKER1$ = OE_REGHEADER::SALESMAN
		OE_TEMP.BROKER2$ = OE_REGHEADER::OPERATOR
		OE_TEMP.PRICE    = OE_REGLINE_READ::PRICE
		OE_TEMP.QTY      = QTY(7%)

		WHEN ERROR IN
			PUT #OE_TEMP.CH%
		USE
			FILENAME$ = "OE_TEMP"
			CONTINUE HelpError
		END WHEN

	END IF

	GOTO NextRegline

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "AGED BACKORDERED ITEMS REPORT"
	TITLE$(2%) = " Order Entry System"

	SELECT CONTROL$

	CASE "1"
		TITLE$(3) = OE_CONTROL::AGENAM(0%)

	CASE "2"
		TITLE$(3) = OE_CONTROL::AGENAM(1%)

	CASE "3"
		TITLE$(3) = OE_CONTROL::AGENAM(2%)

	CASE "4"
		TITLE$(3) = OE_CONTROL::AGENAM(3%)

	CASE "5"
		TITLE$(3) = OE_CONTROL::AGENAM(4%)

	CASE "A"
		TITLE$(3) = "All Backorder Items"

	END SELECT

	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "ExpShipDat Late CusNum      DocumNumber Line  " + &
		"ProductCode    Description                   "  + &
		"Price   BackOrderQty   Broker   Operator"

	TITLE$(6%) = "."


	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	RESET #OE_TEMP.CH%

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #OE_TEMP.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "OE_TEMP"
		CONTINUE HelpError
	END WHEN

	IF (LAST_LATE% > 30%) AND (OE_TEMP.NUMDAYS <= 30%) OR &
		(LAST_LATE% > 15%) AND (OE_TEMP.NUMDAYS <= 15%) OR &
		(LAST_LATE% > 10%) AND (OE_TEMP.NUMDAYS <= 10%) OR &
		(LAST_LATE% > 5%) AND (OE_TEMP.NUMDAYS <= 5%)
	THEN
		GOSUB Total
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	END IF

 PrintLine:
	PRICE = FUNC_ROUND(OE_TEMP.QTY * OE_TEMP.PRICE, 2%)

	V% = PD_EXAM_PRODUCT(OE_TEMP.PRODUCT$, PD_PRODUCT_EXAM)

	TEXT$ = PRNT_DATE(OE_TEMP.DATE$, 8%) + " " + &
		FORMAT$(OE_TEMP.NUMDAYS, "####") + " " + &
		CONV_STRING(OE_TEMP.CUSNUM$, CMC$_LEFT) + "  " + &
		OE_TEMP.ORDER$ + "  " + &
		OE_TEMP.LINE$ + "  " + &
		OE_TEMP.PRODUCT$ + " " + &
		LEFT$(PD_PRODUCT_EXAM::DESCRIPTION, 23%) + "" + &
		FORMAT$(PRICE, "#########.##") + "   " + &
		FORMAT$(OE_TEMP.QTY, "############") + "   " + &
		OE_TEMP.BROKER1$ + " " + &
		OE_TEMP.BROKER2$

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	ST_PRICE = PRICE + ST_PRICE
	ST_QTY   = OE_TEMP.QTY + ST_QTY

	TOTAL_PRICE = PRICE + TOTAL_PRICE
	TOTAL_QTY   = OE_TEMP.QTY + TOTAL_QTY
	LAST_LATE%  = OE_TEMP.NUMDAYS

	GOTO GetNextRec

 Total:
	TEXT$ = SPACE$(70%) + "Group Total:    " + &
		FORMAT$(ST_PRICE, "###,###.##") + "     " + &
		FORMAT$(ST_QTY, "###,######")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	!
	! Init variables
	!
	ST_PRICE = 0.0
	ST_QTY = 0.0

	RETURN

 ExitTotal:
	GOSUB Total

	TEXT$ = SPACE$(69%) + " Grand Total:" + &
		FORMAT$(TOTAL_PRICE, "##,####,###.##") + " " + &
		FORMAT$(TOTAL_QTY, "###,###,######")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
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
