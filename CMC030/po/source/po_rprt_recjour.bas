1	%TITLE "Print Received Order Journal"
	%SBTTL "PO_RPRT_RECJOUR"
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
	! ID:PO023
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Received Order Journal\* option
	!	prints a batch of received orders prior to posting the batch.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_RPRT_RECJOUR/LINE
	!	$ LINK/EXE:PO_EXE PO_RPRT_RECJOUR, -
	!		FUNC_LIB:CMCLINK/OPTION/NOTRACEBACK
	!	$ DELETE PO_RPRT_RECJOUR.OBJ;*
	!
	! Author:
	!
	!	02/15/92 - Dan Perkins
	!
	! Modification History:
	!
	!	02/24/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	03/17/92 - Dan Perkins
	!		Added Vendor Number and Vendor Description
	!		to the report so we can see from whom we
	!		received.
	!
	!	11/04/94 - Kevin Handy
	!		Added parameter to PO_READ_REGLINE
	!
	!	02/27/95 - Kevin Handy
	!		Added error trap for 17000.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/19/2000 - Kevin Handy
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
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PO.OPEN]PO_RECJOUR.HB"
	MAP (PO_RECJOUR)	PO_RECJOUR_CDD		PO_RECJOUR

	%INCLUDE "SOURCE:[PO.OPEN]PO_RECLINE.HB"
	MAP (PO_RECLINE)	PO_RECLINE_CDD		PO_RECLINE

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	DECLARE			PO_REG_LINE_CDD		PO_REG_LINE_READ

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.HB"
	DECLARE			PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE_READ

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	DECLARE			AP_VENDOR_CDD		AP_VENDOR_EXAM

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION PO_READ_REG_LINE
	EXTERNAL LONG    FUNCTION AP_EXAM_VENDOR

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^* (01) Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field enters a
	!	particular batch to be printed.
	!	.b
	!	Only one batch at a time may be printed.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Purchase Order\*
	!	.b
	!	.lm +5
	!	The ^*From Purchase Order\* field enters a PO _#
	!	which will cause the report to begin printing with the
	!	PO _# specified.
	!	.b
	!	If this field is blank, the report will begin with the first purchase order in
	!	the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Purchase Order\* field enters a PO
	!	which causes the report to end printing with the PO specified.
	!	.b
	!	If this field is left blank, the report will end with the last
	!	purchase order in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field enters a wildcard value which
	!	will cause selected purchase orders to be printed.
	!	.b
	!	If this field is blank, all purchase orders in the batch will be printed
	!	in the journal report.
	!	.note
	!	See Appendix B for information in reference to wildcarding techniques.
	!	.end note
	!	.lm -5
	!
	! Index:
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_RECJOUR.OPN"
	USE
		FILENAME$ = "PO_RECJOUR"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_RECLINE.OPN"
	USE
		FILENAME$ = "PO_RECLINE"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	TITLE$(1%) = "RECEIVED PURCHASE ORDERS"
	TITLE$(2%) = "BATCH No. " + BATCH_NO$
	TITLE$(3%) = "Purchase Order System"
	TITLE$(4%) = ""
	TITLE$(5%) = "PO_Number       Rec_Date     RefNo                " + &
		"Vendor#        Vendor                             " + &
		"Received By"

	TITLE$(6%) = SPACE$(22%) + &
		"Line  Product#        Description" + SPACE$(31%) + &
		"UOM        QtyRec        QtyCan"

	TITLE$(7%) = "."

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	SPACE_FLAG% = 0%

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PO_RECJOUR.CH%, KEY #0%
		ELSE
			FIND #PO_RECJOUR.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
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
		GET #PO_RECJOUR.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "PO_RECJOUR"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitProgram IF (PO_RECJOUR::PO > TO_ITEM$) AND &
		TO_ITEM$ <> ""

	GOTO GetNextRec IF WLDCRD$ <> "" AND &
		COMP_STRING(EDIT$(PO_RECJOUR::PO, -1%), WLDCRD$) = 0%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%) IF SPACE_FLAG%

	V% = PO_READ_REG_LINE(PO_RECJOUR::PO, "", "EQ", PO_REG_LINE_READ, &
		PO_REG_SUB_LINE_READ, QTY(), "")

	V% = AP_EXAM_VENDOR(PO_REG_LINE_READ::VENDOR, AP_VENDOR_EXAM)

	TEXT$ = CONV_STRING(PO_RECJOUR::PO, CMC$_LEFT) + "      " + &
		PRNT_DATE(PO_RECJOUR::RECDATE, 6%) + "     " + &
		PO_RECJOUR::REFNO + "     " + &
		AP_VENDOR_EXAM::VENNUM + "     " + &
		LEFT(AP_VENDOR_EXAM::VENNAM, 30%) + "     " + &
		PO_RECJOUR::OPERATOR

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	SPACE_FLAG% = -1%

17100	WHEN ERROR IN
		FIND #PO_RECLINE.CH%, KEY #0% EQ PO_RECJOUR::PO, REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155%
		FILENAME$ = "PO_RECLINE"
		CONTINUE HelpError
	END WHEN

 GetLine:
17120	WHEN ERROR IN
		GET #PO_RECLINE.CH%, REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 11%
		FILENAME$ = "PO_RECLINE"
		CONTINUE HelpError
	END WHEN

	GOTO GetNextRec IF PO_RECLINE::PO <> PO_RECJOUR::PO

	TEXT$ = CONV_STRING(PO_RECJOUR::PO, CMC$_LEFT) + &
		SPACE$(12%) + &
		PO_RECLINE::PO_LINE + "  " + &
		PO_RECLINE::PRODUCT + "  " + &
		PO_RECLINE::DESCRIPTION + "  " + &
		PO_RECLINE::UOM + "   " + &
		FORMAT$(PO_RECLINE::RECQTY, "#,###,###.##") + "  " + &
		FORMAT$(PO_RECLINE::CANQTY, "#,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO GetLine

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
