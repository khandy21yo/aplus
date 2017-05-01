1	%TITLE "Items Shipped Summary Report"
	%SBTTL "OE_RPRT_SHIPITEM"
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
	! ID:OE030
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Items Shipped Summary\* Report contains the following information:
	!	.table 3,25
	!	.te
	!	Document Number	Sale Type
	!	.te
	!	Sale Category	Customer Number
	!	.te
	!	Line Number	Customer Name
	!	.te
	!	Product	Description
	!	.te
	!	Quantity Shipped	Location
	!	.te
	!	Ship Date	Days Late From Expected
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Items Shipped Summary
	!	.x Items Shipped Summary>Report
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_RPRT_SHIPITEM/LINE
	!	$ LINK/EXE=OE_EXE: OE_RPRT_SHIPITEM, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_RPRT_SHIPITEM.OBJ;*
	!
	! AUTHOR:
	!
	!	07/09/91 - Craig Tanner
	!
	! MODIFICATION HISTORY:
	!
	!	09/23/91 - Deborah K. Fries
	!		Cleaned source code
	!		Improved Error trapping
	!		Used functions to read files.
	!
	!	11/07/91 - Dan Perkins
	!		Changed primary key in temp
	!		file to allow duplicates.
	!
	!	01/16/92 - Dan Perkins
	!		Changed quantities to display in integer form.
	!
	!	04/09/92 - Dan Perkins
	!		Use function CONV_STRING to manipulate ORDER NUMBER.
	!
	!	04/28/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/06/96 - Kevin Handy
	!		Reformat source code.
	!
	!	10/20/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/28/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/15/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include scope.com
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	MAP (OE_REGLINE)	OE_REGLINE_CDD		OE_REGLINE

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	MAP (OE_REGHEADER)	OE_REGHEADER_CDD	OE_REGHEADER

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	DECLARE			UTL_LOCATION_CDD	UTL_LOCATION_EXAM

	RECORD OE_TEMP_CDD
		STRING LOCATION = 4%
		LONG ORDERS
		GFLOAT ITEMS
		LONG DAYSLATE
		LONG LINES
		LONG SHIP
	END RECORD

	MAP (OE_TEMP) OE_TEMP_CDD OE_TEMP

	!
	! Declare external functions
	!
	EXTERNAL STRING  FUNCTION CONV_STRING
	EXTERNAL LONG    FUNCTION AR_EXAM_CUSTOM
	EXTERNAL LONG    FUNCTION PD_EXAM_PRODUCT
	EXTERNAL LONG    FUNCTION UTL_EXAM_LOCATION

	DIM SHIP_ARRAY$(100%)

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
	!	.x Sort>Items Shipped Summary Report
	!	^*(01) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines the order
	!	in which the report will print.
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
	!	A setting is required in this field.
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
	!	The ^*From Item\* field enters the item from which the report
	!	is to begin printing. The value entered must be in agreement
	!	with field (01) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Items Shipped Summary Report
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field enters the item with which the
	!	report will end printing. The value entered must be in
	!	agreement with field (01) Sort by.
	!	.b
	!	A blank field will cause the report to end with the last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Items Shipped Summary Report
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated items to be
	!	printed by entering a wildcard.
	!	.b
	!	For information on "Wildcarding" techniques refer to Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Items Shipped Summary Report
	!
	!--

	FROM_DATE$ = EDIT$(DATE_STOREDATE(UTL_REPORTX::OPTDEF(5%)), -1%)

	!++
	! Abstract:FLD06
	!	.x From Date>Items Shipped Summary Report
	!	^*(06) From Date\*
	!	.b
	!	.lm +5
	!	The ^*From Date\* field enters of the date from which the report
	!	is to begin printing.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.b
	!	A blank field will cause the report to begin with the earliest dated
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_DATE$ = EDIT$(DATE_STOREDATE(UTL_REPORTX::OPTDEF(6%)), -1%)

	!++
	! Abstract:FLD07
	!	.x To Date>Items Shipped Summary Report
	!	^*(07) To Date\*
	!	.b
	!	.lm +5
	!	The ^*To Date\* field enters of the date at which the
	!	report will end printing.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.b
	!	A blank field will cause the report to end with the latest date in the file.
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
	! Open temporary file
	!
	CALL ASSG_CHANNEL(OE_TEMP.CH%, STAT%)

	WHEN ERROR IN
		OPEN "OE_TEMP_TMP" FOR OUTPUT AS FILE #OE_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP OE_TEMP, &
			PRIMARY KEY OE_TEMP::LOCATION DUPLICATES, &
			TEMPORARY, &
			BUFFER 32%, &
			ACCESS MODIFY, ALLOW NONE
	USE
		FILENAME$ = "OE_TEMP"
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
		TITLE$(1%) = " SHIPPED ITEMS SUMMARY BY SALE TYPE"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = " SHIPPED ITEMS SUMMARY BY SALE CATEGORY"

	CASE "N"
		K_NUM% = 3%
		TITLE$(1%) = " SHIPPED ITEMS SUMMARY BY CUSTOMER NUMBER"

	CASE "D"
		K_NUM% = 0%
		TITLE$(1%) = " SHIPPED ITEMS SUMMARY BY DOCUMENT NUMBER"
		FROM_ITEM$ = SPACE$(LEN(OE_REGHEADER::ORDNUM) - &
			LEN(FROM_ITEM$)) + FROM_ITEM$
		TO_ITEM$ = SPACE$(LEN(OE_REGHEADER::ORDNUM) - LEN(TO_ITEM$)) + &
			TO_ITEM$

	END SELECT

	TITLE$(2%) = " Order Entry System"
	TITLE$(3%) = "From " + PRNT_DATE(FROM_DATE$, 8%) + &
		" To " + PRNT_DATE(TO_DATE$, 8%)
	TITLE$(3%) = "Before " + PRNT_DATE(TO_DATE$, 8%) IF FROM_DATE$ = ""
	TITLE$(3%) = "After " + PRNT_DATE(FROM_DATE$, 8%) IF TO_DATE$ = ""
	TITLE$(3%) = "For All Dates" IF FROM_DATE$ + TO_DATE$ = ""
	TITLE$(4%) = ""

	!
	! Heading
	!
	!	'     1234567890123456789012345678901234567890
	TITLE$(5%) = "Doc#        ST SCat Line CusNumber  CusName" + &
		"                    Product        Descr" + &
		"                   QtyShip Location ShipDate Late"

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
		IF FROM_ITEM$ = ""
		THEN
			RESET #OE_REGHEADER.CH%, KEY #K_NUM%
		ELSE
			FIND #OE_REGHEADER.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitTotal IF ERR = 155%
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitTotal IF UTL_REPORTX::STAT

	!
	! Get next Order Register record
	!
	WHEN ERROR IN
		GET #OE_REGHEADER.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record if should be printed
	!
	SELECT SORTBY$

	CASE "C"
		GOTO ExitTotal IF (OE_REGHEADER::ORDCAT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(OE_REGHEADER::ORDCAT, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "D"
		GOTO ExitTotal &
			IF (OE_REGHEADER::ORDNUM > TO_ITEM$) AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(OE_REGHEADER::ORDNUM, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitTotal IF (OE_REGHEADER::ORDTYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(OE_REGHEADER::ORDTYPE, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "N"
		GOTO ExitTotal IF (OE_REGHEADER::CUSNUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(OE_REGHEADER::CUSNUM, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

17310	!
	! Try to find any line for the header
	!
	WHEN ERROR IN
		FIND #OE_REGLINE.CH%, &
			KEY #0% EQ OE_REGHEADER::ORDNUM, &
			REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

	NEW_ORDER% = 1%

	!
	! Read line for header
	!
 GetRegline:
17320	WHEN ERROR IN
		GET #OE_REGLINE.CH%, REGARDLESS
	USE
		CONTINUE NewOrder IF ERR = 11%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

	GOTO NewOrder &
		IF OE_REGLINE::ORDNUM <> OE_REGHEADER::ORDNUM

	SELECT OE_REGLINE::TRANTYPE

	CASE "01"
		REQ_DAY% = DATE_DAYCODE(OE_REGLINE::TDATE)
		GOTO GetRegline

	CASE "02"
		GOTO GetRegline &
			IF OE_REGLINE::QTY = 0.0
		GOTO GetRegline &
			IF (OE_REGLINE::TDATE < FROM_DATE$) OR &
			(OE_REGLINE::TDATE > TO_DATE$ AND (TO_DATE$ <> ""))
		SHIP_DAY% = DATE_DAYCODE(OE_REGLINE::TDATE)
		LATE_DAY% = SHIP_DAY% - REQ_DAY%

	CASE ELSE
		GOTO GetRegline
	END SELECT

17350	!
	! Put or update location record for statistics later on.
	!
	WHEN ERROR IN
		GET #OE_TEMP.CH%, KEY #0% EQ OE_REGHEADER::LOCATION
	USE
		CONTINUE 17500 IF ERR = 155%
		FILENAME$ = "OE_TEMP"
		CONTINUE HelpError
	END WHEN

	OE_TEMP::ORDERS = OE_TEMP::ORDERS + NEW_ORDER%

	!
	! Complicated routine to calculate number we don't need.
	!
	FOR I% = 1% TO SHIP_ARRAY%
		IF SHIP_ARRAY$(I%) = OE_REGLINE::SHIPNO
		THEN
			GOTO SkipShipUpdate
		END IF
	NEXT I%

	SHIP_ARRAY% = SHIP_ARRAY% + 1%
	SHIP_ARRAY$(SHIP_ARRAY%) = OE_REGLINE::SHIPNO
	OE_TEMP::SHIP = OE_TEMP::SHIP + 1%

 SkipShipUpdate:
	OE_TEMP::ITEMS = OE_TEMP::ITEMS + OE_REGLINE::QTY
	OE_TEMP::DAYSLATE = OE_TEMP::DAYSLATE + LATE_DAY%
	OE_TEMP::LINES = OE_TEMP::LINES + 1%

	UPDATE #OE_TEMP.CH%

	GOTO PrintLine

17500	OE_TEMP::LOCATION = OE_REGHEADER::LOCATION

	OE_TEMP::ORDERS = 1%

	OE_TEMP::ITEMS = OE_REGLINE::QTY
	OE_TEMP::DAYSLATE = LATE_DAY%
	OE_TEMP::LINES = 1%
	OE_TEMP::SHIP = 1%

	PUT #OE_TEMP.CH%

 PrintLine:
	!
	! Read customer name
	!
	V% = AR_EXAM_CUSTOM(OE_REGHEADER::CUSNUM, AR_35CUSTOM_EXAM)

	V% = PD_EXAM_PRODUCT(OE_REGLINE::PRODUCT, PD_PRODUCT_EXAM)

	!
	! Print line with backorders
	!
	TEXT$ = CONV_STRING(OE_REGHEADER::ORDNUM, CMC$_LEFT) + "  " + &
		OE_REGHEADER::ORDTYPE + " " + &
		OE_REGHEADER::ORDCAT + " " + &
		OE_REGLINE::LIN + " " + &
		OE_REGHEADER::CUSNUM + " " + &
		LEFT$(AR_35CUSTOM_EXAM::CUSNAM, 26%) + " " + &
		OE_REGLINE::PRODUCT + " "     + &
		LEFT$(PD_PRODUCT_EXAM::DESCRIPTION, 20%) + "  " + &
		FORMAT$(OE_REGLINE::QTY, "#########") + " " + &
		OE_REGHEADER::LOCATION + "     " + &
		PRNT_DATE(OE_REGLINE::TDATE, 6%) + " " + &
		FORMAT$(LATE_DAY%, "####")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	BLANK_FLAG = 1%

	NEW_ORDER% = 0%

	!
	! Go for next line
	!
	GOTO GetRegline

 NewOrder:
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%) &
		IF BLANK_FLAG = 1%
	BLANK_FLAG = 0%
	SHIP_ARRAY% = 0%
	SHIP_ARRAY$(I%) = "" FOR I% = 1% TO 100%

	!
	! Try for next Order Register record
	!
	GOTO GetNextRec

 ExitTotal:
17600	!
	! Handle end of report
	!

	!
	! Change title and goto new page to print out Stats.
	!
	WHEN ERROR IN
		RESET #OE_TEMP.CH%
	USE
		FILENAME$ = "OE_TEMP"
		CONTINUE HelpError
	END WHEN

	TITLE$(1%) = " SHIPPED ITEMS SUMMARY PAGE"
	TITLE$(5%) = "Location Location Name                             Orders   Items TotShip  AveDaysLate"
	LIN% = 999%

 GetTemp:
17620	!
	! Start reading the temp file
	!
	WHEN ERROR IN
		GET #OE_TEMP.CH%
	USE
		CONTINUE FinalTotal IF ERR = 11%
		FILENAME$ = "OE_TEMP"
		CONTINUE HelpError
	END WHEN

	V% = UTL_EXAM_LOCATION(OE_TEMP::LOCATION, UTL_LOCATION_EXAM)

	AVE_DAY_LATE% = OE_TEMP::DAYSLATE / OE_TEMP::LINES

	TEXT$ = OE_TEMP::LOCATION + "     " + &
		UTL_LOCATION_EXAM::LOCNAME + " " + &
		FORMAT$(OE_TEMP::ORDERS, "###,###") + " " + &
		FORMAT$(OE_TEMP::ITEMS, "#######") + " " + &
		FORMAT$(OE_TEMP::SHIP, "###,###") + "    "  + &
		FORMAT$(AVE_DAY_LATE%, "#,###,###")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, LIN%)
	LIN% = 0%

	TOTAL_COUNT% = TOTAL_COUNT% + 1%
	TOTAL_ORDERS% = TOTAL_ORDERS% + OE_TEMP::ORDERS
	TOTAL_ITEMS = TOTAL_ITEMS + OE_TEMP::ITEMS
	TOTAL_SHIP% = TOTAL_SHIP% + OE_TEMP::SHIP
	TOTAL_DAY% = TOTAL_DAY% + AVE_DAY_LATE%

	GOTO GetTemp

 FinalTotal:
	!
	! Print Final Totals
	!
	IF TOTAL_COUNT% > 1%
	THEN
		TEXT$ = SPACE$(50%) + "_______ _______ _______  ___________"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = SPACE$(50%) + &
			FORMAT$(TOTAL_ORDERS%, "###,###") + " " + &
			FORMAT$(TOTAL_ITEMS, "#######") + " " + &
			FORMAT$(TOTAL_SHIP%, "###,###") + "    " + &
			FORMAT$(TOTAL_DAY% / TOTAL_COUNT%, "#,###,###")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	END IF

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
