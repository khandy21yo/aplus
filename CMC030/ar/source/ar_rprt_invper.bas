1	%TITLE "Invoice Period Report"
	%SBTTL "AR_RPRT_INVPER"
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
	! ID:AR018
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Invoice Report\* program prints an invoice summary for
	!	the current period.  The report can be sorted by Customer
	!	Number, Salesman, or Invoice number.  The report shows totals
	!	for each customer or salesman and a grand total is printed for
	!	the total number of invoices.
	!	.lm -5
	!
	! Index:
	!	.x Invoice Report
	!
	! Option:
	!
	! Author:
	!
	!	01/15/92 - Frank F. Starman
	!
	! Compile:
	!
	!	$  BAS AR_SOURCE:AR_RPRT_INVPER.BAS/LINE
	!	$  LINK/EXE=AR_EXE:*.EXE AR_RPRT_INVPER, FUNC_LIB:CMCLINK/OPTION
	!	$  DELETE AR_RPRT_INVPER.OBJ;*
	!
	! Modification history:
	!
	!	01/16/92 - Dan Perkins
	!		Added Detail Option to report.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	02/25/92 - Kevin Handy
	!		Fixed "RESUME OutaHere ERR = 11%" to have an "IF"
	!		in it so that the program could compile after
	!		someones undocumented and untested change.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/14/95 - Kevin Handy
	!		Reformat source code closer to 80 columns.
	!		Change RIGHT(NUM1$()) to FORMAT$().
	!
	!	07/22/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/24/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	10/26/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD		AR_CONTROL

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)		GL_PERIOD_CDD		GL_PERIOD

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN)		AR_OPEN_CDD		AR_OPEN

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	MAP (OE_REGLINE)	OE_REGLINE_CDD		OE_REGLINE

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION AR_EXAM_CUSTOM
	EXTERNAL LONG	FUNCTION PD_EXAM_PRODUCT

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Set some initial variable values
	!
	TOTAL = 0.0
	THIS_TOTAL = 0.0

 Init:
	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^*(01) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field enters the order
	!	in which the report is to print.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*C\* - Customer Number
	!	.te
	!	^*I\* - Invoice Number
	!	.te
	!	^*S\* - Salesman
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort By>Invoice
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes the
	!	printing to begin with a particular
	!	item. The value must be in agreement with the value
	!	entered in field (01) Sort by.
	!	.b
	!	A blank field will cause the report to start with
	!	the first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Invoice Report
	!	.x Invoice Report>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes the
	!	printing to end with a particular
	!	item in the file. The value must be in agreement with
	!	field (01) Sort by.
	!	.b
	!	A blank field will cause the report to end with the
	!	last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Invoice Report
	!	.x Invoice Report>To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated
	!	items to be printed by entering a "wildcard" for Wildcarding Technique.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Invoice Report
	!	.x Invoice Report>Wildcard
	!
	!--

	PRINT_DETAIL$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Print Detail\*
	!	.b
	!	.lm +5
	!	The ^*Print Detail\* field selects
	!	between a summary report or a report showing line detail.
	!	.b
	!	Valid values are:
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes
	!	.te
	!	^*N\* - No
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Detail>Invoice Report
	!	.x Invoice Report>Detail
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.OPN"
	USE
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

310	!
	! Open the AR Control file, and grab the record
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.OPN"
		GET #AR_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE #AR_CONTROL.CH%
	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

320	!
	! Open the GL Control File and grab the record
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
		CLOSE #GL_PERIOD.CH%
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	CUR_PERIOD% = AR_CONTROL::LASTPERCLOSE + 1%
	YEAR$       = AR_CONTROL::YEAR

	IF CUR_PERIOD% > GL_PERIOD::FPFY
	THEN
		CUR_PERIOD% = 1%
		YEAR$ = FORMAT$(VAL%(YEAR$) + 1%, "<0>###")
	END IF

	PERIOD$ = YEAR$ + FORMAT$(CUR_PERIOD%, "<0>#")

330	!
	! Open the OE_REGLINE file, in case we print detail
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

	%PAGE

	!*******************************************************************
	! Titles
	!*******************************************************************

 ReportTitle:
	!
	! Set up titles
	!
	! Select which method to sort by
	!
	SELECT SORTBY$

	CASE "C"
		K_NUM% = 0%
		TITLE$(1%) = "INVOICE REPORT BY CUSTOMER"

	CASE "I"
		K_NUM% = 3%
		TITLE$(1%) = "INVOICE REPORT BY INVOICE NUMBER"

	CASE "S"
		K_NUM% = 2%
		TITLE$(1%) = "INVOICE REPORT BY SALESMAN"

	END SELECT

	TITLE$(2%) = "AR System"
	TITLE$(3%) = ""

	!
	! Column headings
	!
	TITLE$(4%) = "Custom #   Name" + SPACE$(27%) + &
		"Inv #    Date     Salesman # " + &
		"Descripton" + SPACE$(24%) + "Amount"

	IF PRINT_DETAIL$ = "Y"
	THEN
		TITLE$(5%) = "     Product #      Description" + &
			SPACE$(27%) + "Qty      Price" + &
			"      Promo  Disc%    GrossSale"

		TITLE$(6%) = "."
	ELSE
		TITLE$(5%) = "."
	END IF

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	THIS_ITEM$ = "12345678901"

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AR_OPEN.CH%, KEY #K_NUM%
		ELSE
			FIND #AR_OPEN.CH%, KEY #K_NUM% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CONTINUE ExitTotal IF ERR = 155%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		GET #AR_OPEN.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record if should be printed
	!
	GOTO GetNextRec IF AR_OPEN::TRATYP <> "01"
	GOTO GetNextRec IF AR_OPEN::UPDATED <> PERIOD$

	SELECT SORTBY$

	CASE "C"
		GOSUB SalesmanTotal IF AR_OPEN::CUSNUM <> THIS_ITEM$

		GOTO ExitTotal IF (AR_OPEN::CUSNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			AR_OPEN::CUSNUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		THIS_ITEM$ = AR_OPEN::CUSNUM + ""

	CASE "I"
		GOSUB SalesmanTotal IF AR_OPEN::INVNUM <> THIS_ITEM$

		GOTO ExitTotal IF (AR_OPEN::INVNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			AR_OPEN::INVNUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "S"
		GOSUB SalesmanTotal IF AR_OPEN::SALNUM <> THIS_ITEM$

		GOTO ExitTotal IF (AR_OPEN::SALNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			AR_OPEN::SALNUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		THIS_ITEM$ = AR_OPEN::SALNUM + ""

	END SELECT

	!
	! Look up customer name
	!
	V% = AR_EXAM_CUSTOM(AR_OPEN::CUSNUM, AR_35CUSTOM_EXAM)

	!
	! Print the cash receipt header
	!
	TEXT$ = AR_OPEN::CUSNUM	+ " " + &
		LEFT(AR_35CUSTOM_EXAM::CUSNAM, 30%) + " " + &
		AR_OPEN::INVNUM + " " + &
		PRNT_DATE(AR_OPEN::TRADAT, 6%) + " " + &
		AR_OPEN::SALNUM + " " + &
		AR_OPEN::DESCR + " " + &
		FORMAT$(AR_OPEN::SALAMT, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	IF PRINT_DETAIL$ = "Y"
	THEN
		GOSUB Detail
	END IF

	TOTAL = TOTAL + AR_OPEN::SALAMT
	THIS_TOTAL = THIS_TOTAL + AR_OPEN::SALAMT

	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!
	GOSUB SalesmanTotal

	TOTAL = FUNC_ROUND(TOTAL, 2%)
	TEXT$ = SPACE$(83%) + &
		"Grand Total:  " + FORMAT$(TOTAL, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO ExitProgram

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

	!
	! Print a total for one salesman
	!
 SalesmanTotal:
	SELECT SORTBY$

	CASE "C"
		SALETOTAL$ = "Customer Total:  "

	CASE "S"
		SALETOTAL$ = "Salesman Total:  "

	END SELECT

	IF THIS_ITEM$ <> "12345678901"
	THEN
		THIS_TOTAL = FUNC_ROUND(THIS_TOTAL, 2%)
		TEXT$ = SPACE$(80%) + &
			SALETOTAL$ + FORMAT$(THIS_TOTAL, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

 ReInit:
	THIS_TOTAL = 0.0

	RETURN

 Detail:
17500	WHEN ERROR IN
		FIND #OE_REGLINE.CH%, &
			KEY #3% EQ AR_OPEN::INVNUM + "02", &
			REGARDLESS
	USE
		CONTINUE OutaHere IF ERR = 155% OR ERR = 9%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

 GetReglineRec:
17510	WHEN ERROR IN
		GET #OE_REGLINE.CH%, REGARDLESS
	USE
		CONTINUE OutaHere IF ERR = 11%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

	!
	! Check if current record should be printed
	!
	GOTO OutaHere IF OE_REGLINE::REFNUM <> AR_OPEN::INVNUM
	GOTO OutaHere IF OE_REGLINE::TRANTYPE <> "02"

	!
	! Look up product information
	!
	V% = PD_EXAM_PRODUCT(OE_REGLINE::PRODUCT, PD_PRODUCT_EXAM)

	GROSS = FUNC_ROUND(OE_REGLINE::QTY * OE_REGLINE::PRICE, 2%)

	TEXT$ = "     " + &
		OE_REGLINE::PRODUCT + " " + &
		LEFT$(PD_PRODUCT_EXAM::DESCRIPTION, 30%) + " " + &
		FORMAT$(OE_REGLINE::QTY, "#######.##") + " " + &
		FORMAT$(OE_REGLINE::PRICE, "#######.##") + " " + &
		FORMAT$(OE_REGLINE::PROMO, "#######.##") + " " + &
		FORMAT$(OE_REGLINE::DISCOUNT, "###.#%") + " " + &
		FORMAT$(GROSS, "#########.##")


	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO GetReglineRec

 OutaHere:
	RETURN

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
