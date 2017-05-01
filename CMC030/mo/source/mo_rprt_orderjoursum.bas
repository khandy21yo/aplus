1	%TITLE "Order Journal Report"
	%SBTTL "MO_RPRT_ORDERJOURSUM"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1993 BY
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
	! ID:MO0004
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Order Journal Entry\* Report will contain
	!	the following information:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Order Number
	!	.le
	!	Order Type
	!	.le
	!	Order Category
	!	.le
	!	Order Date
	!	.le
	!	Customer Number
	!	.le
	!	Customer Name
	!	.le
	!	Make
	!	.le
	!	Model Code
	!	.le
	!	Option Group
	!	.le
	!	Option Group Description
	!	.els
	!	.lm -10
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS MO_SOURCE:MO_RPRT_ORDERJOURSUM/LINE
	!	$ LINK/EXE=MO_EXE: MO_RPRT_ORDERJOURSUM, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE MO_RPRT_ORDERJOURSUM.OBJ;*
	!
	! Author:
	!
	!	04/08/93 - Dan Perkins
	!
	! Modification History:
	!
	!	04/12/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/09/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/06/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/21/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.HB"
	MAP (OE_ORDERJOUR)	OE_ORDERJOUR_CDD	OE_ORDERJOUR

	%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINE.HB"
	MAP (MO_ORDERLINE)	MO_ORDERLINE_CDD	MO_ORDERLINE

	%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINEOPT.HB"
	MAP (MO_ORDERLINEOPT)	MO_ORDERLINEOPT_CDD	MO_ORDERLINEOPT

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERLINE.HB"
	MAP (OE_ORDERLINE)	OE_ORDERLINE_CDD	OE_ORDERLINE

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	!
	! Declare external functions
	!
	EXTERNAL LONG    FUNCTION AR_EXAM_CUSTOM

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	REG_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^* (01) Cash Register Number\*
	!	.b
	!	.lm +5
	!	The ^*Cash Register Number\* field enters a
	!	particular batch to be posted.
	!	.b
	!	Only one batch at a time may be posted.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	BATCH_NO$ = REG_NO$ + "_" + BATCH_NO$

	!++
	! Abstract:FLD02
	!	^*(02) Batch Number\*
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

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field
	!	determines the order in which the
	!	report will print.
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
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field enters the
	!	item with which the report will begin printing.
	!	The value entered must be in agreement with
	!	field (03), Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 132%)

	!++
	! Abstract:FLD05
	!	^*(05) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field allows printing to
	!	end with a specified item.  The value entered must be in
	!	agreement with field (03), Sort by.
	!	.b
	!	A blank field will cause the report to end with the last
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects
	!	designated items to be printed by entering a "wildcard"
	!	for Wildcarding Technique.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

	!
	! Open Order Journal file
	!
300	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.OPN"
	USE
		FILENAME$ = "OE_ORDERJOUR"
		CONTINUE HelpError
	END WHEN

	!
	! Open Order Line file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINE.OPN"
	USE
		FILENAME$ = "MO_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	!
	! Open Order Line Option file
	!
320	WHEN ERROR IN
		%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINEOPT.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "MO_ORDERLINEOPT"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	! Select which method to sort by
	!
	SELECT SORTBY$

	CASE "D"
		K_NUM% = 0%
		ADD_TITLE$ = " BY DOCUMENT NUMBER"

		FROM_ITEM$ = SPACE$(LEN(OE_ORDERJOUR::ORDNUM) - &
			LEN(FROM_ITEM$)) + FROM_ITEM$
		TO_ITEM$ = SPACE$(LEN(OE_ORDERJOUR::ORDNUM) - &
			LEN(TO_ITEM$)) + TO_ITEM$

	CASE "T"
		K_NUM% = 1%
		ADD_TITLE$ = " BY SALE TYPE"

	CASE "N"
		K_NUM% = 2%
		ADD_TITLE$ = " BY CUSTOMER NUMBER"

	CASE "C"
		K_NUM% = 3%
		ADD_TITLE$ = " BY SALE CATEGORY"

	END SELECT

	TITLE$(1%) = "MANUFACTURING ORDER JOURNAL SUMMARY" + ADD_TITLE$

	TITLE$(2%) = "BATCH No. " + BATCH_NO$
	TITLE$(3%) = " Manufacturing Order System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "Doc #      STyp SCat OrderDate  CusNumber  " + &
		"CusName                         Make       " + &
		"MCode  OptGrp Description"

	TITLE$(6%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	!
	! If from item blank then reset item file
	! else try to find the first record
	!
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #OE_ORDERJOUR.CH%, KEY #K_NUM%
		ELSE
			FIND #OE_ORDERJOUR.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "OE_ORDERJOUR"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		GET #OE_ORDERJOUR.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "OE_ORDERJOUR"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record if should be printed
	!
	SELECT SORTBY$

	CASE "D"
		GOTO ExitProgram IF (OE_ORDERJOUR::ORDNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$(OE_ORDERJOUR::ORDNUM, &
			-1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitProgram IF (OE_ORDERJOUR::ORDTYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$(OE_ORDERJOUR::ORDTYPE, &
			-1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "N"
		GOTO ExitProgram IF (OE_ORDERJOUR::CUSNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$(OE_ORDERJOUR::CUSNUM, &
			-1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "C"
		GOTO ExitProgram IF (OE_ORDERJOUR::ORDCAT > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$(OE_ORDERJOUR::ORDCAT, &
			-1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

	!
	! Check current Custom record
	!
	V% = AR_EXAM_CUSTOM(OE_ORDERJOUR::CUSNUM, AR_35CUSTOM_EXAM)

	!
	! Print out one line
	!
	DOC$ = CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT) + " "

	TEXT1$ = DOC$ + &
		OE_ORDERJOUR::ORDTYPE + "   " + &
		OE_ORDERJOUR::ORDCAT + " " + &
		PRNT_DATE(OE_ORDERJOUR::ORDDATE, 8%) + " " + &
		OE_ORDERJOUR::CUSNUM + " " + &
		LEFT$(AR_35CUSTOM_EXAM::CUSNAM, 30%) + " "

	!
	! Check current Order Line record
	!
17100	WHEN ERROR IN
		FIND #MO_ORDERLINE.CH%, &
			KEY #0% GE OE_ORDERJOUR::ORDNUM, &
			REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155%
		FILENAME$ = "MO_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	OPT_PRINTED% = 0%

 OrderLine:
17120	WHEN ERROR IN
		GET #MO_ORDERLINE.CH%, REGARDLESS
	USE
		CONTINUE PrintSpace IF ERR = 11%
		FILENAME$ = "MO_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	GOTO PrintSpace IF MO_ORDERLINE::ORDNUM <> OE_ORDERJOUR::ORDNUM

	TEXT2$ = TEXT1$ + " " + &
		MO_ORDERLINE::MAKE + " " + &
		MO_ORDERLINE::MODELCODE + "  "

	!
	! Check current Order Line record
	!
17200	WHEN ERROR IN
		FIND #MO_ORDERLINEOPT.CH%, KEY #0% EQ MO_ORDERLINE::ORDNUM + &
			MO_ORDERLINE::LIN + MO_ORDERLINE::MAKE + &
			MO_ORDERLINE::MODELCODE, REGARDLESS
	USE
		CONTINUE PrintLine IF ERR = 155% OR ERR = 9%
		FILENAME$ = "MO_ORDERLINEOPT"
		CONTINUE HelpError
	END WHEN

 OrderLineOpt:
17220	WHEN ERROR IN
		GET #MO_ORDERLINEOPT.CH%, REGARDLESS
	USE
		CONTINUE PrintLine IF ERR = 11%
		FILENAME$ = "MO_ORDERLINEOPT"
		CONTINUE HelpError
	END WHEN

	GOTO PrintLine IF MO_ORDERLINEOPT::ORDNUM <> MO_ORDERLINE::ORDNUM OR &
		MO_ORDERLINEOPT::LIN <> MO_ORDERLINE::LIN OR &
		MO_ORDERLINEOPT::MAKE <> MO_ORDERLINE::MAKE OR &
		MO_ORDERLINEOPT::MODELCODE <> MO_ORDERLINE::MODELCODE

	!
	! Print one line
	!
	TEXT$ = TEXT2$ + " " + &
		MO_ORDERLINEOPT::OPTGROUP + "     " + &
		LEFT(MO_ORDERLINEOPT::OPTDESCR, 30%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	OPT_PRINTED% = -1%
	TEXT2$ = DOC$ + SPACE$(LEN(TEXT2$) - LEN(DOC$))

	GOTO OrderLineOpt

 PrintSpace:
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	GOTO GetNextRec

 PrintLine:
	GOTO Orderline IF OPT_PRINTED%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT2$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	OPT_PRINTED% = 0%
	TEXT1$ = DOC$ + SPACE$(LEN(TEXT1$) - LEN(DOC$))

	GOTO Orderline

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
