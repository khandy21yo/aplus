1	%TITLE "Order Register Close/Purge"
	%SBTTL "OE_SPEC_PURGE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1990 BY
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
	! ID:OE021
	!
	! Abstract:HELP
	!	.B
	!	.LM +5
	!	This Report
	!	purges out completed ^*Order Entry\*,
	!	^*Point of Sale\* and ^*Material Order\*
	!	invoices that have been completed up to the
	!	specified purge date.
	!	.P
	!	The summary report contains
	!	the following information about the orders that are
	!	closed and/or purged.
	!	.table 3,25
	!	.te
	!	Document Number	Sale Type
	!	.te
	!	Sale Category	Customer Number
	!	.te
	!	Customer Name	Customer PO Number
	!	.te
	!	Order Date	Location
	!	.te
	!	Ship Via	Line
	!	.te
	!	Product	Description
	!	.te
	!	Quantity Ordered	Quantity Shipped
	!	.te
	!	Quantity Canceled
	!	.end table
	!	^*Note:\* Orders will only be removed if they are
	!	closed and are older than the purge date.
	!	If there is no purge date entered, only completed
	!	orders (all items shipped or canceled) will be reported.
	!	Each report is separate.
	!	.lm -5
	!
	! Index:
	!	.x Report>Order Entry Purge
	!	.x Order Entry Purge>Report
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_SPEC_PURGE/LINE
	!	$ LINK/EXE=OE_EXE: OE_SPEC_PURGE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_SPEC_PURGE.OBJ;*
	!
	! Author:
	!
	!	11/30/90 - Val James Allen
	!
	! Modification History:
	!
	!	12/04/91 - Dan Perkins
	!		Changed opens on REGHEADER and
	!		REGLINE from "OPN" to "UPD"
	!		so the program will work.
	!		Use functions to read customer and product files.
	!		If we change the status, we now purge the order.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	05/26/92 - Dan Perkins
	!		Major program modification to ARCHIVE records
	!		instead of purging.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	08/05/92 - Dan Perkins
	!		Fixed bug that killed the files even if we were
	!		on the first pass flag.
	!
	!	12/07/92 - Dan Perkins
	!		Use COMP_ARRAY instead of COMP_STRING.
	!
	!	03/03/93 - Dan Perkins
	!		Fixed bug to make sure every line in REGLINE file is
	!		checked for balances in subroutine EVALUATEPURGE.
	!		Added a balance column to report.  Commented out
	!		FROM ITEM and TO ITEM which will cause real problems
	!		if used the way the program is now written.
	!
	!	03/09/93 - Dan Perkins
	!		Generally rewrote the program so it makes sense.
	!
	!	04/08/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/14/94 - Kevin Handy
	!		Formatted to 80 columns.
	!
	!	06/23/94 - Kevin Handy
	!		Modified so that it prints out only those that
	!		actually get purged, not those that get flagged
	!		as closed.
	!
	!	06/23/94 - Kevin Handy
	!		Modified to beleve it when a order is already
	!		flagged closed. This should greatly speed up
	!		the purge.
	!
	!	09/13/94 - Kevin Handy
	!		Modification so that all spacing between purged
	!		items does not come at the beginning of the
	!		report before the items are printed.
	!
	!	12/08/94 - Kevin Handy
	!		Modified to dump one line of error message out
	!		to report in case of an error.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/07/95 - Kevin Handy
	!		Remove commented out code.
	!		Lose unecessary external definitions.
	!		Added document number to error message.
	!
	!	12/07/95 - Kevin Handy
	!		Lose .arc archive files. Don't need them, never
	!		used them, users have no access to them.
	!
	!	07/12/96 - Kevin Handy
	!		Lose lots of commented out code.
	!
	!	07/22/96 - Kevin Handy
	!		Reformat source code.
	!
	!	07/25/96 - Kevin Handy
	!		Modifications to handle MO register, which is
	!		integrated witn OE.
	!
	!	08/01/96 - Kevin Handy
	!		Continue modifications for purging MO.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/09/99 - Kevin Handy
	!		Use LOC in AST setup
	!
	!	09/18/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include scope.com
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
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
	MAP (OE_REGLINE)	OE_REGLINE_CDD		OE_REGLINE
	DECLARE			OE_REGLINE_CDD		OE_REGLINE_READ

	%INCLUDE "SOURCE:[OE.OPEN]OE_CONTROL.HB"
	MAP (OE_CONTROL)	OE_CONTROL_CDD		OE_CONTROL

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINE.HB"
	MAP (MO_REGLINE)	MO_REGLINE_CDD		MO_REGLINE
	DECLARE			MO_REGLINE_CDD		MO_REGLINE_READ

	%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINEOPT.HB"
	MAP (MO_REGLINEOPT)	MO_REGLINEOPT_CDD	MO_REGLINEOPT
	DECLARE			MO_REGLINEOPT_CDD	MO_REGLINEOPT_READ

	MAP (DP_OUTP_XUNSOL)				RRR_FLAG%

	!
	! Declare variables
	!
	DECLARE REAL QTY(22%)

	!
	! Declare external functions
	!
	EXTERNAL LONG     FUNCTION AR_EXAM_CUSTOM
	EXTERNAL LONG     FUNCTION OE_READ_REGLINE
	EXTERNAL LONG     FUNCTION MO_READ_REGLINE
	EXTERNAL LONG     FUNCTION MO_READ_REGLINEOPT
	EXTERNAL LONG     FUNCTION PD_EXAM_PRODUCT
	EXTERNAL LONG     FUNCTION OUTP_UNSOLICITED

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
	!	.x Sort by
	!	^*(01) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines the order
	!	in which the report will print.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*C\* - Sale Category
	!	.te
	!	^*D\* - Document Number
	!	.te
	!	^*T\* - Sale Type
	!	.te
	!	^*N\* - Customer Number
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	LOC_WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(1%), -1%)

	!++
	! Abstract:FLD02
	!	^*Location Wildcard\*
	!	.b
	!	.lm +5
	!	.b
	!	.b
	!	.b
	!	.b
	!	.lm -5
	!
	! Index:
	!
	!--

	PURGE_DATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(2%), 132%))

	!++
	! Abstract:FLD03
	!	^*(06) Archive Date\*
	!	.lm +5
	!	.b
	!	The ^*Archive Date\* entered in this field controls the removal
	!	of completed orders.  Only orders older than this date will be
	!	archived and only if they are complete.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.b
	!	^*Note:\*  If no date is entered (blank), completed orders
	!	will be closed and a report will be generated, but no records
	!	will be archived.
	!	.lm -5
	!
	! Index:
	!
	!--

	!
	! Open RegHeader files
	!
300	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.PST"
	USE
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.NEW"
	USE
		FILENAME$ = "OE_REGHEADER_NEW"
		CONTINUE HelpError
	END WHEN

	!
	! Open RegLine files
	!
350	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.PST"
	USE
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

360	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.NEW"
	USE
		FILENAME$ = "OE_REGLINE_NEW"
		CONTINUE HelpError
	END WHEN

	!
	! Open MO files
	!
370	MO_FILE% = 0%

	WHEN ERROR IN
		%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINE.PST"
	USE
		CONTINUE 380
	END WHEN

375	WHEN ERROR IN
		%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINE.NEW"
	USE
		FILENAME$ = "MO_REGLINE"
		CONTINUE HelpError
	END WHEN

380	WHEN ERROR IN
		%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINEOPT.PST"
	USE
		CONTINUE 400
	END WHEN

385	WHEN ERROR IN
		%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINEOPT.NEW"
	USE
		FILENAME$ = "MO_REGLINEOPT"
		CONTINUE HelpError
	END WHEN

	MO_FILE% = -1%

	!
	! Open and post into control file the archive date and any control here
	!
400	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_CONTROL.UPD"
	USE
		FILENAME$ = "OE_CONTROL"
		CONTINUE HelpError
	END WHEN

	!
	! Get the OE_CONTROL record
	!
410	WHEN ERROR IN
		GET #OE_CONTROL.CH%, RECORD 1%
	USE
		IF ERR = 154%   ! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		FILENAME$ = "OE_CONTROL"
		CONTINUE HelpError
	END WHEN

	!
	! Set status flag = 1
	! then when normal exit takes place reset flag to "0"
	!
	OE_CONTROL::STATUS_FLAG = "1"

	!
	! post new purge date into control record
	!
	OE_CONTROL::PURGDATE = PURGE_DATE$

420	WHEN ERROR IN
		UPDATE #OE_CONTROL.CH%
	USE
		FILENAME$ = "OE_CONTROL"
		CONTINUE HelpError
	END WHEN

	IF PURGE_DATE$ = ""
	THEN
		TITLE_DATE$ = DATE_TODAY
	ELSE
		TITLE_DATE$ = PURGE_DATE$
	END IF

 ReportTitle:
	!
	! Title
	!
	! Select which method to sort by
	!
	SELECT SORTBY$

	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = "CLOSED ORDERS AS OF " + &
			PRNT_DATE(TITLE_DATE$, 8%) + &
			" BY SALE TYPE"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = "CLOSED ORDERS AS OF " + &
			PRNT_DATE(TITLE_DATE$, 8%) + &
			" BY SALE CATEGORY"

	CASE "N"
		K_NUM% = 3%
		TITLE$(1%) = "CLOSED ORDERS AS OF " + &
			PRNT_DATE(TITLE_DATE$, 8%) + &
			" BY CUSTOMER NUMBER"

	CASE "D"
		K_NUM% = 0%
		TITLE$(1%) = "CLOSED ORDERS AS OF " + &
			PRNT_DATE(TITLE_DATE$, 8%) + &
			" BY DOCUMENT NUMBER"

	END SELECT

	TITLE$(2%) = " Order Entry System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Doc#        SaleType     SalCat CusNumber  CusName" + &
		"                        CustPo#    OrdDate   ClosDate  " + &
		"Location       Shipvia"

	TITLE$(5%) = "                         Line Product        Descr" + &
		"                       QtyOrd      QtyShip    QtyCancel" + &
		"      Balance"

	TITLE$(6%) = "."

	!
	! Set flag for 1st pass to close then 2nd pass to archive
	!
	PASSFLAG$ = "1"

	!
	! Set up to trap interrupt
	!
	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		LOC(OUTP_XUNSOL) BY VALUE, &
		LOC(SCOPE::SMG_KBID) BY VALUE)

	RRR_FLAG% = 0%

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	IN_REPORT% = -1%

	WHEN ERROR IN
		RESET #OE_REGHEADER.CH%, KEY #K_NUM%
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN


 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitTotal IF UTL_REPORTX::STAT

	!
	! Check for an interrupt
	!
	IF RRR_FLAG%
	THEN
		IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
		THEN
			TEXT$ = "%Abort Key Typed"
			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
			GOTO ExitProgram
		END IF
	END IF

	!
	! Get next Order Register record
	!
	WHEN ERROR IN
		GET #OE_REGHEADER.CH%
	USE
		IF ERR = 154%   ! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

	!
	! Check flag for close or archive processing
	!
	SELECT PASSFLAG$

	CASE "1"

		!
		! Don't bother with this one if it is already flagged
		! as closed. Assume it is coorectly flagged.
		!
		GOTO GetNextRec IF OE_REGHEADER::ASTATUS = "C"

		!
		! Check current record if should be closed and printed
		!
		GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
			OE_REGHEADER::LOCATION, -1%), LOC_WLDCRD$) = 0% AND &
			LOC_WLDCRD$ <> ""

		!
		! Check the lines to see if there are balances
		!
		NEXT_LINE$ = "    "
		LAST_DATE$ = OE_REGHEADER::ORDDATE

 CheckRegline:
		GOTO MOTest IF OE_READ_REGLINE(OE_REGHEADER::ORDNUM, &
			NEXT_LINE$, "GT", OE_REGLINE_READ, QTY()) <> CMC$_NORMAL

		NEXT_LINE$ = OE_REGLINE_READ::LIN

		LAST_DATE$ = OE_REGLINE_READ::TDATE IF &
			OE_REGLINE_READ::TDATE > LAST_DATE$

		GOTO CheckRegline IF FUNC_ROUND(QTY(0%), 5%) = 0.0

		GOTO GetNextRec

 MOTest:	!
		! Check the lines to see if there are balances
		!
		NEXT_LINE$ = "    "

 CheckMORegline:
		GOTO PrintLine IF MO_READ_REGLINE(OE_REGHEADER::ORDNUM, &
			NEXT_LINE$, "GT", MO_REGLINE_READ, QTY()) <> CMC$_NORMAL

		NEXT_LINE$ = MO_REGLINE_READ::LIN

		LAST_DATE$ = MO_REGLINE_READ::TDATE IF &
			MO_REGLINE_READ::TDATE > LAST_DATE$

		GOTO CheckMORegline IF FUNC_ROUND(QTY(0%), 5%) = 0.0

 MOOptTest:	!
		! Check the lines to see if there are balances
		!
		NEXT_OPTLINE$ = "    "

 CheckMOOptRegline:
		GOTO PrintLine IF MO_READ_REGLINEOPT(OE_REGHEADER::ORDNUM, &
			NEXT_LINE$, NEXT_OPTLINE, &
			"GT", MO_REGLINEOPT_READ, QTY()) <> &
			CMC$_NORMAL

		NEXT_OPTLINE$ = MO_REGLINEOPT_READ::OPTLIN

		LAST_DATE$ = MO_REGLINEOPT_READ::TDATE IF &
			MO_REGLINEOPT_READ::TDATE > LAST_DATE$

		GOTO CheckMOOptRegline IF FUNC_ROUND(QTY(0%), 5%) = 0.0

		GOTO  CheckMORegline

		!
		! Flag this one as being closed
		!
 PrintLine:
17100		OE_REGHEADER::ASTATUS = "C"
		OE_REGHEADER::SDATE = LAST_DATE$

17150		WHEN ERROR IN
			UPDATE #OE_REGHEADER.CH%
		USE
			FILENAME$ = "OE_REGHEADER"
			CONTINUE HelpError
		END WHEN

		GOTO GetNextRec


	CASE "2"

		!
		! Ready to Archive or Purge
		!
		CALL ENTR_3MESSAGE(SCOPE, "Purging records...", 1% + 16%)

		IF OE_REGHEADER::ASTATUS = "C" AND &
			PURGE_DATE$ >= OE_REGHEADER::SDATE
		THEN
			!
			! Read customer name
			!
			V% = AR_EXAM_CUSTOM(OE_REGHEADER::CUSNUM, &
				AR_35CUSTOM_EXAM)

			TEXT$ = CONV_STRING(OE_REGHEADER::ORDNUM, CMC$_LEFT) + "  " + &
				OE_REGHEADER::ORDTYPE + "           " + &
				OE_REGHEADER::ORDCAT + "   " + &
				OE_REGHEADER::CUSNUM + " " + &
				LEFT$(AR_35CUSTOM_EXAM::CUSNAM, 30%) + " " + &
				OE_REGHEADER::CUSTPO + " " + &
				PRNT_DATE(OE_REGHEADER::ORDDATE, 6%) + "  " + &
				PRNT_DATE(OE_REGHEADER::SDATE, 6%) + "  " + &
				OE_REGHEADER::LOCATION + "           " + &
				OE_REGHEADER::SHIPVIA

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

			GOSUB DumpTheLines

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

		END IF

		!
		! Try to find a line
		!
17200		WHEN ERROR IN
			FIND #OE_REGLINE.CH%, &
				KEY #0% EQ OE_REGHEADER::ORDNUM, &
				REGARDLESS
		USE
			CONTINUE 17400 IF ERR = 155%
			FILENAME$ = "OE_REGLINE"
			CONTINUE HelpError
		END WHEN

 ReadLine:
17220		WHEN ERROR IN
			GET #OE_REGLINE.CH%, REGARDLESS
		USE
			CONTINUE 17400 IF ERR = 11%
			FILENAME$ = "OE_REGLINE"
			CONTINUE HelpError
		END WHEN

		GOTO 17320 IF OE_REGLINE::ORDNUM <> OE_REGHEADER::ORDNUM

17300		IF OE_REGHEADER::ASTATUS <> "C" OR &
			PURGE_DATE$ < OE_REGHEADER::SDATE
		THEN
			WHEN ERROR IN
				PUT #OE_REGLINE.CH_NEW%
			USE
				FILENAME$ = "OE_REGLINE.NEW"
				CONTINUE HelpError
			END WHEN
		END IF

		GOTO ReadLine

		!
		! Move over any MO REGLINE records (just like the OE_REGLINE
		! stuff)
		!
17320		WHEN ERROR IN
			FIND #MO_REGLINE.CH%, &
				KEY #0% EQ OE_REGHEADER::ORDNUM, &
				REGARDLESS
		USE
			CONTINUE 17340
		END WHEN

17322		WHEN ERROR IN
			GET #MO_REGLINE.CH%, REGARDLESS
		USE
			CONTINUE 17340
		END WHEN

		GOTO 17340 IF MO_REGLINE::ORDNUM <> OE_REGHEADER::ORDNUM

17324		IF OE_REGHEADER::ASTATUS <> "C" OR &
			PURGE_DATE$ < OE_REGHEADER::SDATE
		THEN
			WHEN ERROR IN
				PUT #MO_REGLINE.CH_NEW%
			USE
				FILENAME$ = "MO_REGLINE"
				CONTINUE HelpError
			END WHEN
		END IF

		GOTO 17322

		!
		! Move over any MO REGLINE records (just like the OE_REGLINE
		! stuff)
		!
17340		WHEN ERROR IN
			FIND #MO_REGLINEOPT.CH%, &
				KEY #0% EQ OE_REGHEADER::ORDNUM, &
				REGARDLESS
		USE
			CONTINUE 17400
		END WHEN

17342		WHEN ERROR IN
			GET #MO_REGLINEOPT.CH%, REGARDLESS
		USE
			CONTINUE 17400
		END WHEN

		GOTO 17400 IF MO_REGLINEOPT::ORDNUM <> OE_REGHEADER::ORDNUM

17344		IF OE_REGHEADER::ASTATUS <> "C" OR &
			PURGE_DATE$ < OE_REGHEADER::SDATE
		THEN
			WHEN ERROR IN
				PUT #MO_REGLINEOPT.CH_NEW%
			USE
				FILENAME$ = "MO_REGLINEOPT"
				CONTINUE HelpError
			END WHEN
		END IF

		GOTO 17342

 ExitReadLine:
17400		IF OE_REGHEADER::ASTATUS <> "C" OR &
			PURGE_DATE$ < OE_REGHEADER::SDATE
		THEN
			WHEN ERROR IN
				PUT #OE_REGHEADER.CH_NEW%
			USE
				FILENAME$ = "OE_REGHEADER.NEW"
				CONTINUE HelpError
			END WHEN
		END IF

		GOTO GetNextRec

	END SELECT

	!*******************************************************************
	! Print out all line items
	!*******************************************************************

 DumpTheLines:
	NEXT_LINE$ = "    "

 GetTheRegline:
	RETURN IF OE_READ_REGLINE(OE_REGHEADER::ORDNUM, &
		NEXT_LINE$, "GT", OE_REGLINE_READ, QTY()) <> CMC$_NORMAL

	NEXT_LINE$ = OE_REGLINE_READ::LIN

	!
	! Read description for the product
	!
	V% = PD_EXAM_PRODUCT(OE_REGLINE_READ::PRODUCT, PD_PRODUCT_EXAM)

	!
	! Print line
	!
	TEXT$ = CONV_STRING(OE_REGHEADER::ORDNUM, CMC$_LEFT) + &
		SPACE$(15%) + &
		OE_REGLINE_READ::LIN + " "  + &
		OE_REGLINE_READ::PRODUCT + " "  + &
		LEFT$(PD_PRODUCT_EXAM::DESCRIPTION, 20%) + "  " + &
		FORMAT$(QTY(1%), "#,###,###.##") + " "  + &
		FORMAT$(QTY(2%), "#,###,###.##") + " "  + &
		FORMAT$(QTY(3%), "#,###,###.##") + " "  + &
		FORMAT$(QTY(0%), "#,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO GetTheRegline


 ExitTotal:
	!
	! Handle end of report
	!
	IF PASSFLAG$ = "1" AND PURGE_DATE$ <> ""
	THEN
		PASSFLAG$ = "2"
		GOTO 17000
	END IF

 ExitNormal:
17500	WHEN ERROR IN
		GET #OE_CONTROL.CH%, RECORD 1%
	USE
		IF ERR = 154%   ! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		FILENAME$ = "OE_CONTROL"
		CONTINUE HelpError
	END WHEN


	!
	! Set activity status flag back to "0"
	!
	OE_CONTROL::STATUS_FLAG = "0"

	UPDATE #OE_CONTROL.CH%

	CLOSE OE_REGHEADER.CH%
	CLOSE OE_REGHEADER.CH_NEW%
	CLOSE OE_REGLINE.CH%
	CLOSE OE_REGLINE.CH_NEW%
	CLOSE MO_REGLINE.CH%
	CLOSE MO_REGLINE.CH_NEW%
	CLOSE MO_REGLINEOPT.CH%
	CLOSE MO_REGLINEOPT.CH_NEW%

	GOTO ExitProgram IF PASSFLAG$ <> "2"

17510 !	KILL OE_REGHEADER.NAME$ FOR I% = 1% TO 10%

	SMG_STATUS% = LIB$DELETE_FILE(OE_REGHEADER.NAME$ + ";*")

17520 !	KILL OE_REGLINE.NAME$ FOR I% = 1% TO 10%

	SMG_STATUS% = LIB$DELETE_FILE(OE_REGLINE.NAME$ + ";*")

17522 !	KILL MO_REGLINE.NAME$ FOR I% = 1% TO 10%

	SMG_STATUS% = LIB$DELETE_FILE(MO_REGLINE.NAME$ + ";*")

17524 !	KILL MO_REGLINEOPT.NAME$ FOR I% = 1% TO 10%

	SMG_STATUS% = LIB$DELETE_FILE(MO_REGLINEOPT.NAME$ + ";*")

17530	WHEN ERROR IN
		NAME OE_REGHEADER.NAME_NEW$ AS OE_REGHEADER.NAME$
	USE
		CONTINUE 17540
	END WHEN

17540	NAME OE_REGLINE.NAME_NEW$ AS OE_REGLINE.NAME$

17542	NAME MO_REGLINE.NAME_NEW$ AS MO_REGLINE.NAME$

17544	NAME MO_REGLINEOPT.NAME_NEW$ AS MO_REGLINEOPT.NAME$

17550 !	KILL OE_REGHEADER.NAME_NEW$ FOR I% = 1% TO 10%

	SMG_STATUS% = LIB$DELETE_FILE(OE_REGHEADER.NAME_NEW$ + ";*")

17560 !	KILL OE_REGLINE.NAME_NEW$ FOR I% = 1% TO 10%

	SMG_STATUS% = LIB$DELETE_FILE(OE_REGLINE.NAME_NEW$ + ";*")

17562 !	KILL MO_REGLINE.NAME_NEW$ FOR I% = 1% TO 10%

	SMG_STATUS% = LIB$DELETE_FILE(MO_REGLINE.NAME_NEW$ + ";*")

17564 !	KILL MO_REGLINEOPT.NAME_NEW$ FOR I% = 1% TO 10%

	SMG_STATUS% = LIB$DELETE_FILE(MO_REGLINEOPT.NAME_NEW$ + ";*")

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

	!
	! Print error out on report if possible
	!
	IF IN_REPORT%
	THEN
		IN_REPORT% = 0%

		TEXT$ = NUM1$(ERL) + " " + ERT$(ERR) + "-" + &
			"E" + "-" + ERN$ + "  " + FILENAME$

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "  Currently processing document " + &
			OE_REGHEADER::ORDNUM

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	END IF

	!
	! Display error
	!
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

19000	!***************************************************************
	! ERROR TRAPPING
	!***************************************************************

	SELECT ERR

	CASE 154%
		!
		! Wait for 5 seconds if record is lock
		!
		SLEEP 5%
		RESUME

	END SELECT

	!
	! Resume to display untrapped error
	!
	RESUME HelpError

32767	END
