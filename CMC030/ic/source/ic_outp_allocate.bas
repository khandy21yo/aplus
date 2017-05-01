1	%TITLE "Product Allocation Report"
	%SBTTL "IC_OUTP_ALLOCATE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG IC_OUTP_ALLOCATE(PD_PRODUCT_CDD PD_PRODUCT, &
		UTL_LOCATION_CDD UTL_LOCATION)

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
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program prints Inventory allocation report
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_OUTP_ALLOCATE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP IC_OUTP_ALLOCATE
	!	$ DELETE IC_OUTP_ALLOCATE.OBJ;*
	!
	! Author:
	!
	!	11/11/92 - Dan Perkins
	!
	! Modification History:
	!
	!	11/17/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/17/92 - Dan Perkins
	!		Added code to make sure we are on the same
	!		product.  Read MO_REGLINEOPT file for product.
	!
	!	02/23/93 - Dan Perkins
	!		Look at MO_REGLINEOPT independantly of MO_REGLINE.
	!
	!	03/20/95 - Kevin Handy
	!		(V3.6)
	!		Reformatted source to 80 columns.
	!
	!	03/21/95 - Kevin Handy
	!		Modifications to report to handle problen that
	!		line numbers are not part of WP_REQREGISTER key
	!		number 2, so we have to sort the information
	!		to get proper results.
	!
	!	03/21/95 - Kevin Handy
	!		Modified to leave WP items out of the balance
	!		if the ReqQty id zero. Still prints other
	!		columns.
	!
	!	04/12/95 - Kevin Handy
	!		Update to V3.6 Calico coding standards.
	!
	!	06/19/95 - Kevin Handy
	!		Modified to use OUTP_INITFORM function, so that
	!		we don't end up with dozens of .tmp files open as
	!		often.
	!
	!	07/11/95 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/27/95 - Kevin Handy
	!		Fix calculation of "Product Posted Total" total
	!		so that is subtracts, instead of adding usage.
	!
	!	12/18/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		Lose several lines of commented out code.
	!
	!	12/19/95 - Kevin Handy
	!		Another try at getting "Product Posted Total"
	!		correct.
	!
	!	02/07/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	07/25/96 - Kevin Handy
	!		Change references from MO_REGHEADER to
	!		OE_REGHEADER.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/09/99 - Kevin Handy
	!		Increase dimension of WPREQ_ARRAY
	!
	!	08/30/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Fix IF statements so it uses OR instead of string
	!		concatination to match series of values
	!		Handle PRODUCT_FACTOR in WP_REQ calculations.
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

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.HB"
	MAP (UTL_REPORT)	UTL_REPORT_CDD		UTL_REPORT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	MAP (WP_REQREGISTER)	WP_REQREGISTER_CDD	WP_REQREGISTER
	DECLARE			WP_REQREGISTER_CDD	WP_REQREGISTER_TEST

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	MAP (OE_REGLINE)	OE_REGLINE_CDD		OE_REGLINE
	DECLARE			OE_REGLINE_CDD		OE_REGLINE_TEST

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	DECLARE			OE_REGHEADER_CDD	OE_REGHEADER_READ

	%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINE.HB"
	MAP (MO_REGLINE)	MO_REGLINE_CDD		MO_REGLINE
	DECLARE			MO_REGLINE_CDD		MO_REGLINE_TEST

	%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINEOPT.HB"
	MAP (MO_REGLINEOPT)	MO_REGLINEOPT_CDD	MO_REGLINEOPT
	DECLARE			MO_REGLINEOPT_CDD	MO_REGLINEOPT_TEST

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	RECORD WPREQ_RECORD
		STRING JOB = 10
		STRING LLINE = 4
		STRING REQNUM = 10
		STRING REQLIN = 4
		REAL REQ_QTY
		REAL ISS_QTY
		REAL CAN_QTY
		STRING REQ.DATE = 8
		STRING ISS.DATE = 8
	END RECORD

	DIM WPREQ_RECORD WPREQ_ARRAY(100%)

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION OE_READ_REGHEADER
	EXTERNAL LONG	FUNCTION AR_EXAM_CUSTOM
	EXTERNAL LONG	FUNCTION IC_READ_35BALANCE
	EXTERNAL LONG	FUNCTION OUTP_INITFORM

	%PAGE

	ON ERROR GOTO 19000

	TEMP_PROGRAM$ = SCOPE::PRG_PROGRAM
	TEMP_ITEM$ = SCOPE::PRG_ITEM
	TEMP_IDENT$ = SCOPE::PRG_IDENT

	SCOPE::PRG_PROGRAM = "IC_OUTP_ALLOCATE"
	SCOPE::PRG_ITEM = "HELP"
	SCOPE::PRG_IDENT = "H"

	IC_OUTP_ALLOCATE = 0%
	REPORT$ = "IC057"

340	!
	! Get Report
	!
	GOTO ExitFunction &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "") <> CMC$_NORMAL

	!
	! Open needed files
	!
600	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.OPN"
	USE
		FILENAME$ = "WP_REQREGISTER " + WP_REQREGISTER.DEV$
		CONTINUE HelpError
	END WHEN

610	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.OPN"
	USE
		CONTINUE 620 IF ERR = 5%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

620	WHEN ERROR IN
		%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINE.OPN"
	USE
		CONTINUE 630 IF ERR = 5%
		FILENAME$ = "MO_REGLINE"
		CONTINUE HelpError
	END WHEN

630	WHEN ERROR IN
		%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINEOPT.OPN"
	USE
		CONTINUE Init IF ERR = 5%
		FILENAME$ = "MO_REGLINEOPT"
		CONTINUE HelpError
	END WHEN

 Init:	!

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "ALLOCATION DETAIL"

	TITLE$(2%) = TRM$(PD_PRODUCT::PRODUCT_NUM) + "  " + &
			TRM$(PD_PRODUCT::DESCRIPTION) + "  AT  LOCATION  " + &
			TRM$(UTL_LOCATION::LOCATION) + "  " + &
			TRM$(UTL_LOCATION::LOCNAME)

	TITLE$(3%) = "Inventory Control System"
	TITLE$(4%) = ""
	TITLE$(5%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	IF PD_PRODUCT::PRODUCT_FACTOR = 0.0
	THEN
		FACTOR = 1.0
	ELSE
		FACTOR = PD_PRODUCT::PRODUCT_FACTOR
	END IF

	!
	! Print the WP title
	!
	TEXT$ = "JobNum     Line ReqNum     ReqLin" + SPACE$(36%) + &
		"ReqQty IssQty CanQty Balance ReqDate    IssDate"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	WHEN ERROR IN
		FIND #WP_REQREGISTER.CH%, &
			KEY #2% EQ PD_PRODUCT::PRODUCT_NUM + &
			UTL_LOCATION::LOCATION, &
			REGARDLESS
	USE
		CONTINUE FindOERegline IF ERR = 155% OR ERR = 9%
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	WP_REQ_QTY, WP_ISS_QTY, WP_CAN_QTY, WP_BALANCE = 0.0

	REQ_ARRAY% = 0%

 GetNextRec:
17020	WHEN ERROR IN
		GET #WP_REQREGISTER.CH%, REGARDLESS
	USE
		CONTINUE FindOERegline IF ERR = 11%
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	GOTO FindOERegline &
		IF WP_REQREGISTER::PRODUCT <> PD_PRODUCT::PRODUCT_NUM OR &
		WP_REQREGISTER::LOCATION <> UTL_LOCATION::LOCATION

	GOSUB PrintWPInfo &
		IF WP_REQREGISTER::JOB <> WP_REQREGISTER_TEST::JOB OR &
		WP_REQREGISTER::REQNUM <> WP_REQREGISTER_TEST::REQNUM

	WP_REQREGISTER_TEST = WP_REQREGISTER

	FOR LOOP% = 1% TO WPREQ_ARRAY%

		GOTO 17030 &
			IF WPREQ_ARRAY(LOOP%)::LLINE == WP_REQREGISTER::LLINE AND &
			WPREQ_ARRAY(LOOP%)::REQLIN == WP_REQREGISTER::REQLIN

	NEXT LOOP%

	LOOP%, WPREQ_ARRAY% = WPREQ_ARRAY% + 1%
	WPREQ_ARRAY(LOOP%)::JOB = WP_REQREGISTER::JOB
	WPREQ_ARRAY(LOOP%)::LLINE = WP_REQREGISTER::LLINE
	WPREQ_ARRAY(LOOP%)::REQNUM = WP_REQREGISTER::REQNUM
	WPREQ_ARRAY(LOOP%)::REQLIN = WP_REQREGISTER::REQLIN
	WPREQ_ARRAY(LOOP%)::REQ_QTY = 0.0
	WPREQ_ARRAY(LOOP%)::ISS_QTY = 0.0
	WPREQ_ARRAY(LOOP%)::CAN_QTY = 0.0
	WPREQ_ARRAY(LOOP%)::REQ.DATE = ""
	WPREQ_ARRAY(LOOP%)::ISS.DATE = ""

17030	SELECT WP_REQREGISTER::RECTYP

	CASE "01"
		WPREQ_ARRAY(LOOP%)::REQ_QTY = &
			WPREQ_ARRAY(LOOP%)::REQ_QTY + &
			WP_REQREGISTER::QTY / FACTOR

		WPREQ_ARRAY(LOOP%)::REQ.DATE = WP_REQREGISTER::TRANDATE &
			IF WP_REQREGISTER::TRANDATE > &
			WPREQ_ARRAY(LOOP%)::REQ.DATE

	CASE "02"
		WPREQ_ARRAY(LOOP%)::ISS_QTY = &
			WPREQ_ARRAY(LOOP%)::ISS_QTY + &
			WP_REQREGISTER::QTY / FACTOR

		WPREQ_ARRAY(LOOP%)::ISS.DATE = WP_REQREGISTER::TRANDATE &
			IF WP_REQREGISTER::TRANDATE > &
			WPREQ_ARRAY(LOOP%)::ISS.DATE

	CASE "03"
		WPREQ_ARRAY(LOOP%)::CAN_QTY = &
			WPREQ_ARRAY(LOOP%)::CAN_QTY + &
			WP_REQREGISTER::QTY / FACTOR

	END SELECT

	GOTO GetNextRec

 FindOERegline:
	GOSUB PrintWPInfo

	TEXT$ = "Work in Process Total" + SPACE$(48%) + &
		FORMAT$(WP_REQ_QTY, "######") + " " + &
		FORMAT$(WP_ISS_QTY, "######") + " " + &
		FORMAT$(WP_CAN_QTY, "######") + "  " + &
		FORMAT$(WP_BALANCE, "######")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	!
	! Print the OE title
	!
	TEXT$ = "Order#     Line OrdTp Cust#      CustomerName" + &
		SPACE$(24%) + "OrdQty ShpQty CanQty Balance OrdDate  " + &
		"  ShpDate"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

17100	WHEN ERROR IN
		FIND #OE_REGLINE.CH%, &
			KEY #1% EQ PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		CONTINUE FindMORegline IF ERR = 155% OR ERR = 9%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

	OE_ORD_QTY, OE_SHP_QTY, OE_CAN_QTY = 0.0
	SHP_DATE$ = "        "

 GetOERegline:
17120	WHEN ERROR IN
		GET #OE_REGLINE.CH%, REGARDLESS
	USE
		CONTINUE FindMORegline IF ERR = 11%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

	GOTO FindMORegline IF OE_REGLINE::PRODUCT <> PD_PRODUCT::PRODUCT_NUM

	GOSUB PrintOEInfo IF OE_REGLINE::ORDNUM <> OE_REGLINE_TEST::ORDNUM OR &
		OE_REGLINE::LIN <> OE_REGLINE_TEST::LIN

	OE_REGLINE_TEST = OE_REGLINE

	SELECT OE_REGLINE::TRANTYPE

	CASE "01"
		ORD_QTY = ORD_QTY + OE_REGLINE::QTY

	CASE "02"
		SHP_QTY = SHP_QTY + OE_REGLINE::QTY

		SHP_DATE$ = OE_REGLINE::TDATE &
			IF OE_REGLINE::TDATE > SHP_DATE$

	CASE "03"
		CAN_QTY = CAN_QTY + OE_REGLINE::QTY

	END SELECT

	GOTO GetOERegline

 FindMORegline:
	GOSUB PrintOEInfo

	OE_BALANCE = OE_ORD_QTY - OE_SHP_QTY - OE_CAN_QTY

	TEXT$ = "Order Entry Total    " + SPACE$(48%) + &
		FORMAT$(OE_ORD_QTY, "######") + " " + &
		FORMAT$(OE_SHP_QTY, "######") + " " + &
		FORMAT$(OE_CAN_QTY, "######") + "  " + &
		FORMAT$(OE_BALANCE, "######")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	!
	! Print the MO title
	!
	TEXT$ = "Order#     Line OptL OrdTp Cust#      CustomerName        " + &
		"           OrdQty ShpQty CanQty Balance OrdDate    ShpDate"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

17200	WHEN ERROR IN
		FIND #MO_REGLINE.CH%, &
			KEY #1% EQ PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		CONTINUE FindMOReglineOPt IF ERR = 155% OR ERR = 9%
		FILENAME$ = "MO_REGLINE"
		CONTINUE HelpError
	END WHEN

	MO_ORD_QTY, MO_SHP_QTY, MO_CAN_QTY = 0.0

 GetMORegline:
17220	WHEN ERROR IN
		GET #MO_REGLINE.CH%, REGARDLESS
	USE
		CONTINUE FindMOReglineOPt IF ERR = 11%
		FILENAME$ = "MO_REGLINE"
		CONTINUE HelpError
	END WHEN

	GOTO FindMOReglineOpt IF MO_REGLINE::PRODUCT <> PD_PRODUCT::PRODUCT_NUM

	GOSUB PrintMOInfo IF MO_REGLINE::ORDNUM <> MO_REGLINE_TEST::ORDNUM OR &
		MO_REGLINE::LIN <> MO_REGLINE_TEST::LIN

	MO_REGLINE_TEST = MO_REGLINE

	SELECT MO_REGLINE::TRANTYPE

	CASE "01"
		ORD_QTY = ORD_QTY + MO_REGLINE::QTY

	CASE "02"
		SHP_QTY = SHP_QTY + MO_REGLINE::QTY

		SHP_DATE$ = MO_REGLINE::TDATE &
			IF MO_REGLINE::TDATE > SHP_DATE$

	CASE "03"
		CAN_QTY = CAN_QTY + MO_REGLINE::QTY

	END SELECT

	GOTO GetMORegline

 FindMOReglineOpt:
	GOSUB PrintMOInfo

17300	WHEN ERROR IN
		FIND #MO_REGLINEOPT.CH%, &
			KEY #3% EQ PD_PRODUCT::PRODUCT_NUM, &
			REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 155% OR ERR = 9%
		FILENAME$ = "MO_REGLINEOPT"
		CONTINUE HelpError
	END WHEN

 GetMOReglineOpt:
17320	WHEN ERROR IN
		GET #MO_REGLINEOPT.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "MO_REGLINEOPT"
		CONTINUE HelpError
	END WHEN

	GOTO ExitTotal IF MO_REGLINEOPT::PRODUCT <> PD_PRODUCT::PRODUCT_NUM

	GOSUB PrintMOOptInfo &
		IF MO_REGLINEOPT::ORDNUM <> MO_REGLINEOPT_TEST::ORDNUM OR &
		MO_REGLINEOPT::LIN <> MO_REGLINEOPT_TEST::LIN OR &
		MO_REGLINEOPT::OPTLIN <> MO_REGLINEOPT_TEST::OPTLIN

	MO_REGLINEOPT_TEST = MO_REGLINEOPT

	SELECT MO_REGLINEOPT::TRANTYPE

	CASE "01"
		ORD_QTY = ORD_QTY + MO_REGLINEOPT::QTY

	CASE "02"
		SHP_QTY = SHP_QTY + MO_REGLINEOPT::QTY

	CASE "03"
		CAN_QTY = CAN_QTY + MO_REGLINEOPT::QTY

	END SELECT

	GOTO GetMOReglineOpt

 PrintWPInfo:
	FOR LOOP% = 1% TO WPREQ_ARRAY%

		BALANCE = WPREQ_ARRAY(LOOP%)::REQ_QTY - &
			WPREQ_ARRAY(LOOP%)::ISS_QTY - &
			WPREQ_ARRAY(LOOP%)::CAN_QTY

		IF FUNC_ROUND(BALANCE, 3%) > 0.0
		THEN
			IF WPREQ_ARRAY(LOOP%)::REQ_QTY = 0.0
			THEN
				BALANCE = 0.0
			END IF

			!
			! Print the stuff out
			!
			TEXT$ = WPREQ_ARRAY(LOOP%)::JOB + " " + &
				WPREQ_ARRAY(LOOP%)::LLINE + " " + &
				WPREQ_ARRAY(LOOP%)::REQNUM + " " + &
				WPREQ_ARRAY(LOOP%)::REQLIN + SPACE$(38%) + &
				FORMAT$(WPREQ_ARRAY(LOOP%)::REQ_QTY, "######") + " " + &
				FORMAT$(WPREQ_ARRAY(LOOP%)::ISS_QTY, "######") + " " + &
				FORMAT$(WPREQ_ARRAY(LOOP%)::CAN_QTY, "######") + "  " + &
				FORMAT$(BALANCE, "######") + " " + &
				PRNT_DATE(WPREQ_ARRAY(LOOP%)::REQ.DATE, 8%) + " " + &
				PRNT_DATE(WPREQ_ARRAY(LOOP%)::ISS.DATE, 8%)

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			GOTO ExitFunction IF UTL_REPORTX::STAT

			WP_REQ_QTY = WP_REQ_QTY + WPREQ_ARRAY(LOOP%)::REQ_QTY
			WP_ISS_QTY = WP_ISS_QTY + WPREQ_ARRAY(LOOP%)::ISS_QTY
			WP_CAN_QTY = WP_CAN_QTY + WPREQ_ARRAY(LOOP%)::CAN_QTY
			WP_BALANCE = WP_REQ_QTY - WP_ISS_QTY - WP_CAN_QTY


		END IF

	NEXT LOOP%

 ExitWP:
	WPREQ_ARRAY% = 0%

	RETURN

 PrintOEInfo:
	BALANCE = ORD_QTY - SHP_QTY - CAN_QTY

	GOTO ExitOE IF FUNC_ROUND(BALANCE, 3%) <= 0.0

	GOTO ExitOE IF OE_READ_REGHEADER(OE_REGLINE_TEST::ORDNUM, &
		OE_REGHEADER_READ) <> CMC$_NORMAL

	GOTO ExitOE IF OE_REGHEADER_READ::LOCATION <> UTL_LOCATION::LOCATION

	V% = AR_EXAM_CUSTOM(OE_REGHEADER_READ::CUSNUM, AR_35CUSTOM_EXAM)

	!
	! Print the stuff out
	!
	TEXT$ = CONV_STRING(OE_REGLINE_TEST::ORDNUM, CMC$_LEFT) + " " + &
		OE_REGLINE_TEST::LIN + " " + &
		OE_REGHEADER_READ::ORDTYPE + "    " + &
		OE_REGHEADER_READ::CUSNUM + " " + &
		LEFT(AR_35CUSTOM_EXAM::CUSNAM, 30%) + " " + &
		FORMAT$(ORD_QTY, "######") + " " + &
		FORMAT$(SHP_QTY, "######") + " " + &
		FORMAT$(CAN_QTY, "######") + "  " + &
		FORMAT$(BALANCE, "######") + " " + &
		PRNT_DATE(OE_REGHEADER_READ::SDATE, 8%) + " " + &
		PRNT_DATE(SHP_DATE$, 8%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitFunction IF UTL_REPORTX::STAT

	OE_ORD_QTY = OE_ORD_QTY + ORD_QTY
	OE_SHP_QTY = OE_SHP_QTY + SHP_QTY
	OE_CAN_QTY = OE_CAN_QTY + CAN_QTY

 ExitOE:
	ORD_QTY, SHP_QTY, CAN_QTY = 0.0
	SHP_DATE$ = "        "

	RETURN

 PrintMOInfo:
	BALANCE = ORD_QTY - SHP_QTY - CAN_QTY

	GOTO ExitMO IF FUNC_ROUND(BALANCE, 3%) <= 0.0

	GOTO ExitMO IF OE_READ_REGHEADER(MO_REGLINE_TEST::ORDNUM, &
		OE_REGHEADER_READ) <> CMC$_NORMAL

	GOTO ExitMO IF OE_REGHEADER_READ::LOCATION <> UTL_LOCATION::LOCATION

	V% = AR_EXAM_CUSTOM(OE_REGHEADER_READ::CUSNUM, AR_35CUSTOM_EXAM)

	!
	! Print the stuff out
	!
	TEXT$ = CONV_STRING(MO_REGLINE_TEST::ORDNUM, CMC$_LEFT) + " " + &
		MO_REGLINE_TEST::LIN + " " + &
		SPACE$(LEN(MO_REGLINEOPT::OPTLIN)) + " " + &
		OE_REGHEADER_READ::ORDTYPE + "    " + &
		OE_REGHEADER_READ::CUSNUM + " " + &
		LEFT(AR_35CUSTOM_EXAM::CUSNAM, 30%) + " " + &
		FORMAT$(ORD_QTY, "######") + " " + &
		FORMAT$(SHP_QTY, "######") + " " + &
		FORMAT$(CAN_QTY, "######") + "  " + &
		FORMAT$(BALANCE, "######") + " " + &
		PRNT_DATE(OE_REGHEADER_READ::SDATE, 8%) + " " + &
		PRNT_DATE(SHP_DATE$, 8%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitFunction IF UTL_REPORTX::STAT

	MO_ORD_QTY = MO_ORD_QTY + ORD_QTY
	MO_SHP_QTY = MO_SHP_QTY + SHP_QTY
	MO_CAN_QTY = MO_CAN_QTY + CAN_QTY

 ExitMO:
	ORD_QTY, SHP_QTY, CAN_QTY = 0.0
	SHP_DATE$ = "        "

	RETURN

 PrintMOOptInfo:
	BALANCE = ORD_QTY - SHP_QTY - CAN_QTY

	GOTO ExitMO IF FUNC_ROUND(BALANCE, 3%) <= 0.0

	GOTO ExitMO IF OE_READ_REGHEADER(MO_REGLINEOPT_TEST::ORDNUM, &
		OE_REGHEADER_READ) <> CMC$_NORMAL

	GOTO ExitMO IF OE_REGHEADER_READ::LOCATION <> UTL_LOCATION::LOCATION

	V% = AR_EXAM_CUSTOM(OE_REGHEADER_READ::CUSNUM, AR_35CUSTOM_EXAM)

	!
	! Print the stuff out
	!
	TEXT$ = CONV_STRING(MO_REGLINEOPT_TEST::ORDNUM, CMC$_LEFT) + " " + &
		MO_REGLINEOPT_TEST::LIN + " " + &
		MO_REGLINEOPT_TEST::OPTLIN + " " + &
		OE_REGHEADER_READ::ORDTYPE + "    " + &
		OE_REGHEADER_READ::CUSNUM + " " + &
		LEFT(AR_35CUSTOM_EXAM::CUSNAM, 30%) + " " + &
		FORMAT$(ORD_QTY, "######") + " " + &
		FORMAT$(SHP_QTY, "######") + " " + &
		FORMAT$(CAN_QTY, "######") + "  " + &
		FORMAT$(BALANCE, "######") + " " + &
		PRNT_DATE(OE_REGHEADER_READ::SDATE, 8%) + " " + &
		PRNT_DATE(SHP_DATE$, 8%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitFunction IF UTL_REPORTX::STAT

	MO_ORD_QTY = MO_ORD_QTY + ORD_QTY
	MO_SHP_QTY = MO_SHP_QTY + SHP_QTY
	MO_CAN_QTY = MO_CAN_QTY + CAN_QTY

 ExitMOOpt:
	ORD_QTY, SHP_QTY, CAN_QTY = 0.0

	RETURN

 ExitTotal:
	GOSUB PrintMOOptInfo

	MO_BALANCE = MO_ORD_QTY - MO_SHP_QTY - MO_CAN_QTY

	TEXT$ = "Manufacture Order Total" + SPACE$(46%) + &
		FORMAT$(MO_ORD_QTY, "######") + " " + &
		FORMAT$(MO_SHP_QTY, "######") + " " + &
		FORMAT$(MO_CAN_QTY, "######") + "  " + &
		FORMAT$(MO_BALANCE, "######")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TOTAL_ORD_QTY = WP_REQ_QTY + OE_ORD_QTY + MO_ORD_QTY
	TOTAL_REC_QTY = WP_ISS_QTY + OE_SHP_QTY + MO_SHP_QTY
	TOTAL_CAN_QTY = WP_CAN_QTY + OE_CAN_QTY + MO_CAN_QTY

	TOTAL_BALANCE = TOTAL_ORD_QTY - TOTAL_REC_QTY - TOTAL_CAN_QTY

 !		OE_ORD_QTY + MO_ORD_QTY - &
 !		OE_SHP_QTY - MO_SHP_QTY - &
 !		OE_CAN_QTY - MO_CAN_QTY - WP_BALANCE

	V% = IC_READ_35BALANCE(PD_PRODUCT::PRODUCT_NUM, &
		UTL_LOCATION::LOCATION, BALANCE(,))

	ALLOC = -FUNC_ROUND(BALANCE(2%, 1%) + BALANCE(2%, 2%), 3%)

	TEXT$ = SPACE$(35%) + &
		"Product Posted Total " + SPACE$(13%) + &
		FORMAT$(TOTAL_ORD_QTY, "######") + " " + &
		FORMAT$(TOTAL_REC_QTY, "######") + " " + &
		FORMAT$(TOTAL_CAN_QTY, "######") + "  " + &
		FORMAT$(TOTAL_BALANCE, "######")

	IF FUNC_ROUND(ALLOC - TOTAL_BALANCE, 3%) <> 0.0
	THEN
		TEXT$ = TEXT$ + "  Warning: Inv posted balance is " + &
			NUM1$(ALLOC)
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	RUN_TOTAL = -FUNC_ROUND(BALANCE(2%, 1%) + BALANCE(2%, 2%) + &
		BALANCE(2%, 3%), 3%)

	TEXT$ = SPACE$(35%) + &
		"Product Running Total" + SPACE$(35%) + &
		FORMAT$(RUN_TOTAL, "######")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

 ExitFunction:
	CALL OUTP_FINISH(UTL_REPORTX)

	CLOSE WP_REQREGISTER.CH%
	CALL ASSG_FREECHANNEL(WP_REQREGISTER.CH%)

	CLOSE OE_REGLINE.CH%
	CALL ASSG_FREECHANNEL(OE_REGLINE.CH%)

	CLOSE MO_REGLINE.CH%
	CALL ASSG_FREECHANNEL(MO_REGLINE.CH%)

	CLOSE MO_REGLINEOPT.CH%
	CALL ASSG_FREECHANNEL(MO_REGLINEOPT.CH%)

 Exit2:
	SCOPE::PRG_PROGRAM = TEMP_PROGRAM$
	SCOPE::PRG_ITEM = TEMP_ITEM$
	SCOPE::PRG_IDENT = TEMP_IDENT$

	EXIT FUNCTION

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	UTL_REPORTX::STAT = -1%
	GOTO ExitFunction

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	RESUME HelpError

32767	END FUNCTION
