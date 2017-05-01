1	%TITLE "Order Journal Production Schedule Report"
	%SBTTL "MO_RPRT_ORDERJOURPRODSKED"
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
	! ID:MO0006
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Order Journal Production Schedule\* Report contains
	!	the following information:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	City
	!	.le
	!	Customer Number
	!	.le
	!	Order Category
	!	.le
	!	Production Number (Document Number)
	!	.le
	!	Model
	!	.le
	!	Make
	!	.le
	!	Year
	!	.le
	!	Make Type
	!	.le
	!	Make Size
	!	.le
	!	Fronts
	!	.le
	!	Backs
	!	.le
	!	Roofs
	!	.le
	!	Special Instructions
	!	.els
	!	.lm -10
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS MO_SOURCE:MO_RPRT_ORDERJOURPRODSKED/LINE
	!	$ LINK/EXE=MO_EXE: MO_RPRT_ORDERJOURPRODSKED, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE MO_RPRT_ORDERJOURPRODSKED.OBJ;*
	!
	! Author:
	!
	!	06/17/93 - Dan Perkins
	!
	! Modification History:
	!
	!	06/23/93 - Dan Perkins
	!		Print City in first field.  Print header and line
	!		notes on one line each.
	!
	!	06/25/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/28/93 - Dan Perkins
	!		Added customer number to report.
	!
	!	07/06/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/06/93 - Frank F. Starman
	!		Print longer a model description.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/02/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/04/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[MO.OPEN]MO_MODELCODE.HB"
	MAP (MO_MODELCODE)	MO_MODELCODE_CDD	MO_MODELCODE

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
	!	^*(02) Batch Number
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
	!	^*C\* - Sale Category
	!	.te
	!	^*N\* - Customer Number
	!	.end table
	!	A value is required in this field.
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
	!	field (02), Sort by.
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
	!	agreement with field (02), Sort by.
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
		CONTINUE 330 IF ERR = 5%
		FILENAME$ = "MO_ORDERLINEOPT"
		CONTINUE HelpError
	END WHEN

	!
	! Open Model Code file
	!
330	WHEN ERROR IN
		%INCLUDE "SOURCE:[MO.OPEN]MO_MODELCODE.OPN"
	USE
		CONTINUE 340 IF ERR = 5%
		FILENAME$ = "MO_MODELCODE"
		CONTINUE HelpError
	END WHEN

	!
	! Open Make file
	!
340	!%INCLUDE "SOURCE:[MO.OPEN]MO_MAKE.OPN"

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

	CASE "N"
		K_NUM% = 2%
		ADD_TITLE$ = " BY CUSTOMER NUMBER (CITY)"

	CASE "C"
		K_NUM% = 3%
		ADD_TITLE$ = " BY SALE CATEGORY"

	END SELECT

	TITLE$(1%) = "ORDER JOURNAL PRODUCTION SCHEDULE" + ADD_TITLE$
	TITLE$(2%) = "BATCH No. " + BATCH_NO$
	TITLE$(3%) = " Manufacturing Order System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "City       Cust#      SCat Doc#       Model           " + &
		"Make       Year Mtyp Msize  Fronts           "     + &
		"Backs            Roofs"

	TITLE$(6%) = "          Special Instructions"

	TITLE$(7%) = "."

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

		GOTO GetNextRec &
			IF COMP_ARRAY(EDIT$(OE_ORDERJOUR::ORDNUM, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "N"
		GOTO ExitProgram IF (OE_ORDERJOUR::CUSNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$(OE_ORDERJOUR::CUSNUM, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "C"
		GOTO ExitProgram IF (OE_ORDERJOUR::ORDCAT > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$(OE_ORDERJOUR::ORDCAT, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

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

 OrderLine:
17120	WHEN ERROR IN
		GET #MO_ORDERLINE.CH%, REGARDLESS
	USE
		CONTINUE PrintSpace IF ERR = 11%
		FILENAME$ = "MO_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	GOTO PrintSpace IF MO_ORDERLINE::ORDNUM <> OE_ORDERJOUR::ORDNUM

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

	FRONT$, BACK$, ROOF$ = SPACE$(15%)

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

	SELECT MO_ORDERLINEOPT::OPTGROUP

	CASE "F"
		FRONT$ = LEFT(MO_ORDERLINEOPT::OPTDESCR, 15%)

	CASE "B"
		BACK$ = LEFT(MO_ORDERLINEOPT::OPTDESCR, 15%)

	CASE "R"
		ROOF$ = LEFT(MO_ORDERLINEOPT::OPTDESCR, 15%)

	END SELECT

	GOTO OrderLineOpt

 PrintSpace:
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	GOTO GetNextRec

 PrintLine:
	!
	! Get the Model Code description
	!
	MO_MODELCODE::DESCR = ""

17300	WHEN ERROR IN
		GET #MO_MODELCODE.CH%, &
			KEY #0% EQ MO_ORDERLINE::MODELCODE, &
			REGARDLESS
	USE
		CONTINUE ModelCodeError IF ERR = 155% OR ERR = 9%
		FILENAME$ = "MO_MODELCODE"
		CONTINUE HelpError
	END WHEN

 ModelCodeError:
	!
	! Get the Make description
	!
	!MO_MAKE::DESCR = ""

17400	!GET #MO_MAKE.CH%, KEY #0% EQ MO_ORDERLINE::MAKE + MO_ORDERLINE::YEAR + &
	!	MO_ORDERLINE::MTYPE + MO_ORDERLINE::MSIZE, REGARDLESS

 MakeError:
	TEXT$ = LEFT(OE_ORDERJOUR::CITY, 10%) + " " + &
		OE_ORDERJOUR::CUSNUM + " " + &
		OE_ORDERJOUR::ORDCAT + " " + &
		CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT) + " " + &
		LEFT(MO_MODELCODE::DESCR, 16%) + " " + &
		MO_ORDERLINE::MAKE + " " + &
		MO_ORDERLINE::YEAR + " " + &
		MO_ORDERLINE::MTYPE + "   " + &
		MO_ORDERLINE::MSIZE + "   " + &
		FRONT$ + "  " + &
		BACK$ + "  " + &
		ROOF$

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Print notes, if any
	!
	TEXT$ = TRM$(OE_ORDERJOUR::NOTES(0%)) + " " + &
		TRM$(OE_ORDERJOUR::NOTES(1%)) + " " + &
		TRM$(OE_ORDERJOUR::NOTES(2%)) + " " + &
		TRM$(MO_ORDERLINE::NOTES(0%)) + " " + &
		TRM$(MO_ORDERLINE::NOTES(1%))

	TEXT$ = "          " + EDIT$(TEXT$, 16% + 128%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%) IF TEXT$ <> ""

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
