1	%TITLE "Product Promotions Report"
	%SBTTL "OE_RPRT_PROMO"
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
	! ID:OE022
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Product Promotions\* Report contains
	!	the following information:
	!	.table 3,25
	!	.te
	!	Promotion Number	From Date
	!	.te
	!	To Date	G/L Account Number
	!	.te
	!	Account Description	Product Number
	!	.te
	!	Product Description	Customer Number
	!	.te
	!	Customer Type	Customer Category
	!	.te
	!	Salesman	Dollar Discount Amt.
	!	.te
	!	Percentage Discount Amt.
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Product Promotions
	!	.x Product Promotions>Report
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_RPRT_PROMO/LINE
	!	$ LINK/EXE=OE_EXE: OE_RPRT_PROMO, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_RPRT_PROMO.OBJ;*
	!
	! AUTHOR:
	!
	!	12/05/90 - Val James Allen
	!
	! MODIFICATION HISTORY:
	!
	!	09/20/91 - Deborah K. Fries
	!		Used functions to read files
	!		Cleaned source code
	!		Modified error trapping
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	07/18/96 - Kevin Handy
	!		Reformat source.
	!
	!	05/28/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/18/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[OE.OPEN]OE_PRODPROMO.HB"
	MAP (OE_PRODPROMO)	OE_PRODPROMO_CDD	OE_PRODPROMO

	%INCLUDE "SOURCE:[OE.OPEN]OE_PROMO.HB"
	MAP (OE_PROMO)		OE_PROMO_CDD		OE_PROMO

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	DECLARE			GL_CHART_CDD		GL_CHART_EXAM

	!
	! Declare external functions
	!
	EXTERNAL LONG    FUNCTION PD_EXAM_PRODUCT
	EXTERNAL LONG    FUNCTION GL_EXAM_CHART

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
	!	^*(01) From Promo Number\*
	!	.b
	!	.lm +5
	!	The ^*From Promo Number\* field enters the
	!	promotion number with which the report will begin printing.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	promotion number in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Promo Number
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Promo Number\*
	!	.b
	!	.lm +5
	!	The ^*To Promo Number\* field enters a specific
	!	promotion number with which the report is to end printing.
	!	.b
	!	A blank field will cause the report to end with the last
	!	promotion number in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Promo Number
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects
	!	designated items to be printed by entering a "wildcard"
	!	using the Wildcarding
	!	Technique.
	!	.b
	!	For information on "Wildcarding" techniques refer to Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

300	!
	! Open Promotion master file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_PROMO.OPN"
	USE
		FILENAME$ = "OE_PROMO"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Product Promotion master file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_PRODPROMO.OPN"
	USE
		FILENAME$ = "OE_PRODPROMO"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "PRODUCT PROMOTIONS REPORT"
	TITLE$(2%) = " Order Entry System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!	      1234567890123456789012345678901234567890
	TITLE$(4%) = "PromotionNumber   FromDate    ToDate    " + &
		"  G/LAccountNumber    AccountDescription"

	TITLE$(5%) = "                                                          " + &
		"                 ---Promotional Select Criteria---"

	TITLE$(6%) = "                  ProductNumber  ProductDescription       " + &
		"                 Customer#  Ctype Ccate " + &
		"Salesman    DollarDisc  %Disc"

	TITLE$(7%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	!
	! If from category blank then reset Promo file
	! else try to find the first record
	!
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #OE_PROMO.CH%, KEY #0%
		ELSE
			FIND #OE_PROMO.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "OE_PROMO"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next Master Promo record
	!
	WHEN ERROR IN
		GET #OE_PROMO.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "OE_PROMO"
		CONTINUE HelpError
	END WHEN

	GOTO ExitProgram IF (OE_PROMO::REFPROMO > TO_ITEM$) AND &
		TO_ITEM$ <> ""

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(OE_PROMO::REFPROMO, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""


	!
	! Check current GL Account record
	!
	V% = GL_EXAM_CHART(OE_PROMO::ACCOUNT, GL_CHART_EXAM)

	!
	! Print out one line
	!
	TEXT$ = OE_PROMO::REFPROMO + "  " + &
		PRNT_DATE(OE_PROMO::FROMDATE, 8%) + "  " + &
		PRNT_DATE(OE_PROMO::TODATE, 8%) + "  " + &
		OE_PROMO::ACCOUNT + "  " + &
		GL_CHART_EXAM::DESCR

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	!
	! Check current Product Promotion Line record
	!
17040	WHEN ERROR IN
		FIND #OE_PRODPROMO.CH%, &
			KEY #1% EQ OE_PROMO::REFPROMO, &
			REGARDLESS
	USE
		CONTINUE 17330 IF ERR = 155%
		FILENAME$ = "OE_PRODPROMO"
		CONTINUE HelpError
	END WHEN

17200	WHEN ERROR IN
		GET #OE_PRODPROMO.CH%, REGARDLESS
	USE
		CONTINUE 17330 IF ERR = 11%
		FILENAME$ = "OE_PRODPROMO"
		CONTINUE HelpError
	END WHEN

	GOTO 17330 IF OE_PRODPROMO::REFPROMO <> OE_PROMO::REFPROMO

	!
	! Get the Product Description
	!
	V% = PD_EXAM_PRODUCT(OE_PRODPROMO::PRODUCT, PD_PRODUCT_EXAM)

	!
	! Print out one line
	!
	TEXT$ = OE_PROMO::REFPROMO + "  " + &
		OE_PRODPROMO::PRODUCT + " " + &
		PD_PRODUCT_EXAM::DESCRIPTION + "  "   + &
		OE_PRODPROMO::CUSTOMER + " " + &
		OE_PRODPROMO::CUSTYPE + "    " + &
		OE_PRODPROMO::CUSTCAT + "  "   + &
		OE_PRODPROMO::SALESMAN + "  "   + &
		FORMAT$(OE_PRODPROMO::PROMODOLL, "#######.##") + "  " + &
		FORMAT$(OE_PRODPROMO::PROMOPERC, "##.##")


	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO 17200


17330	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Try for next Order Register record
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
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
