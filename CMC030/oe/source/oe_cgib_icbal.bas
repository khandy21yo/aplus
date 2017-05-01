1	%TITLE " Backorders Report"
	%SBTTL "OE_CGIB_ICBAL"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2004 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! ID:OE017
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Backorders\* Report contains columns for the following
	!	information:
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
	!	Quantity Backordered	Quantity Canceled
	!	.te
	!	Balance to Ship
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Backorders
	!	.x Backorders Report
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_CGIB_ICBAL/LINE
	!	$ LINK/EXE=OE_EXE: OE_CGIB_ICBAL, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_CGIB_ICBAL.OBJ;*
	!
	! AUTHOR:
	!
	!	04/01/2004 - Kevin Handy
	!
	! MODIFICATION HISTORY:
	!
	!	05/19/2004 - Kevin Handy
	!		Add CUSPO to output.
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include codes
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "$SSDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	%INCLUDE "SOURCE:[PD.OPEN]PD_SUBSTITUTE.HB"
	MAP (PD_SUBSTITUTE) PD_SUBSTITUTE_CDD PD_SUBSTITUTE

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP (IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

 !	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
 !	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.HB"
	MAP (UTL_PROFILE)       UTL_PROFILE_CDD         UTL_PROFILE

	!
	! Declare external functions
	!
	EXTERNAL LONG    FUNCTION IC_READ_35BALANCE

	%PAGE

	DEF CGI_TEXT$(X$)
		CGI_TEXT$ = '"' + EDIT$(X$, 8% + 128%) + '"'
	FNEND

	DEF CGI_NUMBER$(X, Y%)
		CGI_NUMBER$ = NUM1$(FUNC_ROUND(X, Y%))
	FNEND

	ON ERROR GOTO 19000

 !	OPEN "TT:" FOR OUTPUT AS FILE 1%, &
 !		RECORDSIZE 511%

	MARGIN 511%

	PRINT "Content-Type: Text/Plain"
	PRINT ""

	!
 Init:	! Initilize report
	!
	SYS_STATUS% = LIB$GET_SYMBOL("WWW_QUERY_STRING" BY DESC, &
		UQUERY$ BY DESC,,)

 !	PRINT "TestI: '"; UQUERY$; "'"

	IF (SYS_STATUS% AND 1%) = 0%
	THEN
		UQUERY$ = ""
	END IF

	FROM_ITEM% = INSTR(1%, UQUERY$, "=")
	IF (FROM_ITEM%)
	THEN
		FROM_ITEM$ = RIGHT(UQUERY$, FROM_ITEM% + 1%)
		IF LEFT(FROM_ITEM$, 1%) = '"'
		THEN
			FROM_ITEM$ = RIGHT(FROM_ITEM$, 2%)
		END IF
		IF RIGHT(FROM_ITEM$, LEN(FROM_ITEM$)) = '"'
		THEN
			FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(FROM_ITEM$) - 1%)
		END IF
		FROM_ITEM$ = LEFT(FROM_ITEM$ + "              ", 14%)
	END IF

 !	PRINT "Testing: '"; FROM_ITEM$; "'"

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field begins the
	!	printing with a particular item number.
	!	The value entered must be in agreement with
	!	field (01) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item
	!
	!--

 !	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)
	TO_ITEM$ = FROM_ITEM$

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field end printing of the
	!	report with a particular item.  The value entered must be in agreement with
	!	field (01) Sort by.
	!	.b
	!	A blank field will cause the report to end with the last
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item
	!
	!--

 !	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)
	WLDCRD$ = ""

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
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

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.OPN"
		GET #IC_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "IC_CONTROL"
		CONTINUE HelpError
	END WHEN

	CLOSE IC_CONTROL.CH%

	V% = READ_PERIOD("READ", IC_CONTROL::ERA, IC_CONTROL::PERIOD, &
		PERIOD_DESCR$, STAT$, START_DATE$, END_DATE$, 0%)

	YYYYPP$ = IC_CONTROL::PERIOD

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

320 !	WHEN ERROR IN
 !		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
 !	USE
 !		FILENAME$ = "UTL_LOCATION"
 !		CONTINUE HelpError
 !	END WHEN

330	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.OPN"
		GET #UTL_PROFILE.CH%, RECORD 1%, REGARDLESS
		CLOSE #UTL_PROFILE.CH%
		CALL ASSG_FREECHANNEL(UTL_PROFILE.CH%)
	USE
		FILENAME$ = "UTL_PROFILE"
		CONTINUE HelpError
	END WHEN

340	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_SUBSTITUTE.OPN"
	USE
		FILENAME$ = "PD_SUBSTITUTE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	IF FROM_ITEM$ = ""
	THEN
		GOTO ExitProgram
	END IF

 !	WHEN ERROR IN
 !		GET #UTL_LOCATION.CH%, &
 !			KEY #0% EQ UTL_PROFILE::DEFLOCATION, &
 !			REGARDLESS
 !	USE
 !		FILENAME$ = "UTL_LOCATION"
 !		CONTINUE HelpError
 !	END WHEN

17010	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, &
			KEY #0% EQ FROM_ITEM$, &
			REGARDLESS
	USE
		CONTINUE 17100 IF ERR = 155%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	GOSUB 17300

	GOTO ExitProgram

17100	!
	! Not found under product number, look for something
	! close
	!
	FROM_ITEM$ = "X" + TRM$(FROM_ITEM$)

	WHEN ERROR IN
		FIND #PD_SUBSTITUTE.CH%, &
			KEY #1% GE FROM_ITEM$, &
			REGARDLESS
	USE
		CONTINUE ExitProgram
	END WHEN

17110	WHEN ERROR IN
		GET #PD_SUBSTITUTE.CH%, &
			REGARDLESS
	USE
		CONTINUE ExitProgram
	END WHEN

	IF LEFT(PD_SUBSTITUTE::THEIR_PRODUCT, LEN(FROM_ITEM$)) = FROM_ITEM$
	THEN
		WHEN ERROR IN
			GET #PD_PRODUCT.CH%, &
				KEY #0% EQ PD_SUBSTITUTE::OUR_PRODUCT, &
				REGARDLESS
		USE
			CONTINUE 17110 IF ERR = 155%
			FILENAME$ = "PD_PRODUCT"
			CONTINUE HelpError
		END WHEN

		GOSUB 17300
		GOTO 17110
	END IF

	GOTO ExitProgram

17300	!
	! Print out one line
	!
	IF (IC_READ_35BALANCE(PD_PRODUCT::PRODUCT_NUM, &
		UTL_PROFILE::MAINLOCATION, BALANCE(,)) AND 1%) = 0%
	THEN
		ONHAND = 0.0
		ALLOC   = 0.0
		ONORDER = 0.0

		CUR_ONHAND = 0.0
		CUR_ALLOC = 0.0
		CUR_ONORDER = 0.0
	ELSE
		ONHAND = BALANCE(1%, 1%)
		ALLOC   = BALANCE(2%, 1%)
		ONORDER = BALANCE(3%, 1%)

		CUR_ONHAND = BALANCE(1%, 2%)
		CUR_ALLOC = BALANCE(2%, 2%)
		CUR_ONORDER = BALANCE(3%, 2%)
	END IF

	TEXT$ = SPACE$(LEN(PD_PRODUCT::PRODUCT_NUM)) + "  " + &
		SPACE$(LEN(PD_PRODUCT::DESCRIPTION) - 3%) + "CUR " + &
		SPACE$(LEN(PD_PRODUCT::PROD_TYPE)) + " " + &
		SPACE$(LEN(PD_PRODUCT::CATEGORY)) + " " + &
		SPACE$(LEN(PD_PRODUCT::SECONDARY_CODE)) + " " + &
		SPACE$(LEN(PD_PRODUCT::UOM)) + " " + &
		FORMAT$(ONHAND + CUR_ONHAND, "############") + "  " + &
		FORMAT$(ALLOC + CUR_ALLOC, "############") + "  " + &
		FORMAT$(ONORDER + CUR_ONORDER, "############")


	PRINT CGI_TEXT$(PD_PRODUCT::PRODUCT_NUM); ","; &
		CGI_TEXT$(PD_PRODUCT::DESCRIPTION); ","; &
		CGI_TEXT$(PD_PRODUCT::PROD_TYPE); ","; &
		CGI_TEXT$(PD_PRODUCT::CATEGORY); ",";  &
		CGI_TEXT$(PD_PRODUCT::UOM); ",";  &
		CGI_NUMBER$(ONHAND + CUR_ONHAND, 0%); ","; &
		CGI_NUMBER$(ALLOC + CUR_ALLOC, 0%); ","; &
		CGI_NUMBER$(ONORDER + CUR_ONORDER, 0%)
 !		CGI_TEXT$(UTL_PROFILE::DEFLOCATION)

	RETURN

 ExitProgram:

	EXIT PROGRAM

	%PAGE

 HelpError:
	!***************************************************************
	! Help Message for an error
	!***************************************************************
	PRINT "**UNEXPECTED ERROR: "; &
		NUM1$(ERL) + " " + ERT$(ERR)

 !	UTL_REPORTX::STAT = -1%
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
