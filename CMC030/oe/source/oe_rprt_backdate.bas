1	%TITLE " Backorders By Date Report"
	%SBTTL "OE_RPRT_BACKDATE"
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
	! ID:OE035
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Backorders by date\* Report contains information on all
	!	backordered items in the Order Entry System. The report lists
	!	the items in date sequence (oldest to most recent) and will reflect
	!	the following information.
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
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Backorders
	!	.x Backorders Report
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_RPRT_BACKDATE/LINE
	!	$ LINK/EXE=OE_EXE: OE_RPRT_BACKDATE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_RPRT_BACKDATE.OBJ;*
	!
	! Author:
	!
	!	07/09/91 - Val James "Hang'um Higher" Allen
	!
	! Modification History:
	!
	!	04/09/92 - Dan Perkins
	!		Use CONV_STRING to convert order number.
	!
	!	06/02/92 - Dan Perkins
	!		Broker2 variable is now OE_REGHEADER::OPERATOR to
	!		accomodate changes in Regheader file layout.
	!
	!	10/20/92 - Dan Perkins
	!		Use READ_REGLINE and EXAM_PRODUCT to read these
	!		files.  Cleaned program code.
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
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
	! Include scope.com
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

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	MAP (OE_TEMP)	OE_TEMP.DATE$    =  8%, &
			OE_TEMP.ORDER$   = 10%, &
			OE_TEMP.LINE$    =  4%, &
			OE_TEMP.PRODUCT$ = 14%, &
			OE_TEMP.DESCR$   = 40%, &
			OE_TEMP.BROKER1$ = 10%, &
			OE_TEMP.BROKER2$ = 10%, &
			OE_TEMP.QTY, &
			OE_TEMP.NUMDAYS

	!
	! Declare external functions
	!
	EXTERNAL STRING FUNCTION CONV_STRING
	EXTERNAL LONG	FUNCTION OE_READ_REGLINE
	EXTERNAL LONG	FUNCTION PD_EXAM_PRODUCT

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	CUT_DATE$ = EDIT$(DATE_STOREDATE(UTL_REPORTX::OPTDEF(0%)), -1)

	!++
	! Abstract:FLD01
	!	.x Cutoff>Date
	!	^*(01) Cutoff Date\*
	!	.b
	!	.lm +5
	!	The ^*Cutoff Date\* allows printing of a report
	!	including only those backordered items with an expected
	!	ship date that is older than the cutoff date entered in this field.
	!	.b
	!	A blank field will cause the report to print all
	!	backorders in the register file.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Date>Cutoff
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

350	CALL ENTR_3MESSAGE(SCOPE, "Creating temporary file.  Reading work files.", 1%)

	CALL ASSG_CHANNEL(OE_TEMP.CH%, STAT%)

	OPEN "OE_TEMP.TMP" FOR OUTPUT AS FILE #OE_TEMP.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP OE_TEMP, &
		PRIMARY KEY (OE_TEMP.DATE$, OE_TEMP.ORDER$) &
			DUPLICATES, &
		TEMPORARY, &
		BUFFER 32%, &
		ACCESS MODIFY, ALLOW NONE

400	!*******************************************************************
	! Start putting stuff into OE_TEMP file
	!*******************************************************************

	TODAYSDATE$ = DATE_TODAY

	Y% = DATE_DAYCODE(TODAYSDATE$)

	WHEN ERROR IN
		RESET #OE_REGHEADER.CH%
	USE
		CONTINUE ExitProgram
	END WHEN

 GetRegHeader:
410	WHEN ERROR IN
		GET #OE_REGHEADER.CH%, REGARDLESS
	USE
		CONTINUE ReportTitle IF ERR = 11%
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

	GOTO GetRegHeader IF OE_REGHEADER::SDATE > CUT_DATE$ AND CUT_DATE$ <> ""

	X% = DATE_DAYCODE(OE_REGHEADER::SDATE)

	OE_TEMP.NUMDAYS = (Y% - X%)

	LINE$ = "    "

 NextRegline:
	GOTO GetRegHeader IF OE_READ_REGLINE(OE_REGHEADER::ORDNUM, LINE$, &
		"GT", OE_REGLINE_READ, QTY()) <> CMC$_NORMAL

	GOTO GetRegHeader IF OE_REGLINE_READ::ORDNUM <> OE_REGHEADER::ORDNUM

	LINE$ = OE_REGLINE_READ::LIN

	GOTO NextRegLine IF QTY(0%) <= 0.0

	IF PD_EXAM_PRODUCT(OE_REGLINE_READ::PRODUCT, PD_PRODUCT_EXAM) <> &
		CMC$_NORMAL
	THEN
		PD_PRODUCT_EXAM::DESCRIPTION = ""
	END IF

	OE_TEMP.DATE$    = OE_REGHEADER::SDATE
	OE_TEMP.ORDER$   = OE_REGHEADER::ORDNUM
	OE_TEMP.LINE$    = OE_REGLINE_READ::LIN
	OE_TEMP.PRODUCT$ = OE_REGLINE_READ::PRODUCT
	OE_TEMP.BROKER1$ = OE_REGHEADER::SALESMAN
	OE_TEMP.BROKER2$ = OE_REGHEADER::OPERATOR
	OE_TEMP.QTY      = QTY(0%)
	OE_TEMP.DESCR$   = PD_PRODUCT_EXAM::DESCRIPTION

450	PUT #OE_TEMP.CH%

	GOTO NextRegline

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "BACKORDERED ITEMS REPORT BY DATE"
	TITLE$(2%) = " Order Entry System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "ExpShipDate  NumDaysOld  DocumNumber Line  " + &
		"ProductCode    Description           "       + &
		"                   BackOrderQty Broker  "    + &
		"   Operator"

	TITLE$(5%) = "."

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	WHEN ERROR IN
		RESET #OE_TEMP.CH%
	USE
		CONTINUE ExitProgram
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
		GET #OE_TEMP.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "OE_TEMP"
		CONTINUE HelpError
	END WHEN

	TEXT$ = PRNT_DATE(OE_TEMP.DATE$, 8%) + "   " + &
		FORMAT$(OE_TEMP.NUMDAYS, "##########") + "  " + &
		CONV_STRING(OE_TEMP.ORDER$, CMC$_LEFT) + "  " + &
		OE_TEMP.LINE$ + "  " + &
		OE_TEMP.PRODUCT$ + " " + &
		OE_TEMP.DESCR$ + " " + &
		FORMAT$(OE_TEMP.QTY, "#########.##") + " " + &
		OE_TEMP.BROKER1$ + " " + &
		OE_TEMP.BROKER2$

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

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

	%PAGE

19000	!***************************************************************
	! ERROR TRAPPING
	!***************************************************************

	!
	! Resume to display untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
