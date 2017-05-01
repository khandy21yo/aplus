1	%TITLE "Accounts Payable Vendor Label Writer"
	%SBTTL "AP_RPRT_NAMELABEL"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2001 BY
	!
	! Software Solutions, Inc
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
	! Software Solutions, Inc
	!
	! Software Solutions, Inc assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc
	!
	!++
	! ID:AP051
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Name Address Labels\* routine prints
	!	labels for vendors.
	!	.b
	!	The print setting screen provides fields in which entries may be
	!	made to print only a partial listing of the file and also provides
	!	a setting in which the labels can be printed in customer number order,
	!	customer name order, or alphabetical order.
	!	.lm -5
	!
	! Index:
	!	.x Labels>Vendor
	!	.x Vendor>Labels
	!	.x Print>Vendor Labels
	!
	! Option:
	!
	!
	! Author:
	!
	!	06/06/2001 - Kevin Handy
	!		Copied from AP_rprt_namelabel so AP would have
	!		a decent label printer.
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_RPRT_NAMELABEL/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_RPRT_NAMELABEL, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_RPRT_NAMELABEL.OBJ;*
	!
	! Modification history:
	!
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

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTRY.HB"
	MAP (UTL_COUNTRY)	UTL_COUNTRY_CDD		UTL_COUNTRY

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR)	AP_VENDOR_CDD		AP_VENDOR

	!
	! Dimension Statements
	!
	DIM XLINE$(66%)

	!
	! External Functions
	!
	EXTERNAL LONG	FUNCTION COMP_STRING

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes the printing
	!	to begin with the selected Item _#.
	!	The value must be in agreement with field
	!	(03) Sort by.
	!	.b
	!	A blank setting will cause the report to begin with the first
	!	Item _# in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Name/Address Labels
	!	.x Name/Address Labels>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes the printing
	!	to end with the selected Item _#. The
	!	value must be in agreement with field (03) Sort by.
	!	.b
	!	A blank setting will cause the report to end with the last
	!	Item _# in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Name/Address Labels
	!	.x Name/Address Labels>To Item
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	.x Sort by>Name/Address Labels
	!	^*(03) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines the order
	!	in which the report will print.
	!	.b
	!	Valid settings are:
	!	.te
	!	.table 3,25
	!	^*N\* - Number
	!	.te
	!	^*A\* - Alphabetical
	!	.te
	!	^*Z\* - Zip Code
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Name/Address Labels>Sort by
	!
	!--

	SELECT SORTBY$

	CASE "N"
		K_NUM% = 0%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AP_VENDOR::VENNUM))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AP_VENDOR::VENNUM))

	CASE "M"
		K_NUM% = 1%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AP_VENDOR::VENNAM))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AP_VENDOR::VENNAM))

	CASE "Z"
		K_NUM% = 4%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AP_VENDOR::ZIP))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AP_VENDOR::ZIP))

	CASE "A"
		K_NUM% = 2%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AP_VENDOR::ALPSRT))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AP_VENDOR::ALPSRT))

	CASE "S"
		K_NUM% = 3%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AP_VENDOR::STATE))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AP_VENDOR::STATE))

	END SELECT

	INCLUDE_VENNUM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	.ts 55
	!	^*(04) Include Customer Number\*
	!	.b
	!	.lm +5
	!	The ^*Include Customer Number\* field
	!	determines whether or not the Customer Number will be printed
	!	on the label.
	!	.b
	!	Valid settings are:
	!	.te
	!	.table 3,25
	!	^*Y\* - Yes
	!	.te
	!	^*N\* - No
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	WILD_CUST$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 128%)

	!++
	! Abstract:FLD05
	!	^*(05) Wildcard Customer\*
	!
	! Index:
	!	.x Name/Address Labels>Wildcard Customer
	!
	!--

	WILD_TYPE$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 128%)

	!++
	! Abstract:FLD06
	!	^*(06) Wildcard Type\*
	!
	! Index:
	!	.x Name/Address Labels>Wildcard Type
	!
	!--

	LABEL_COUNT% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(7%), -1%))
	LABEL_COUNT% = 1% IF LABEL_COUNT% < 1%

	!++
	! Abstract:FLD08
	!	^*(08) Labels/Customer\*
	!
	! Index:
	!	.x Labels>Customer
	!	.x Customer>Labels
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.OPN"
	USE
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTRY.OPN"
	USE
		CONTINUE 320
	END WHEN

320	!

	%PAGE

16000	!*******************************************************************
	! Title
	!*******************************************************************

 ReportTitle:

	!
	! Calculate length for individual labels
	!
	XLINE% = UTL_REPORTX::PAGELEN
	XLINE% = 6% IF (XLINE% >= 66%) OR (XLINE% = 0%) OR &
		(UTL_REPORTX::PRINTTO = 1%)

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AP_VENDOR.CH%, KEY #K_NUM%
		ELSE
			FIND #AP_VENDOR.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "AP_VENDOR"
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
		GET #AP_VENDOR.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORTBY$
	CASE "N"
		GOTO ExitTotal IF (AP_VENDOR::VENNUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	CASE "M"
		GOTO ExitTotal IF (AP_VENDOR::VENNAM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	CASE "A"
		GOTO ExitTotal IF (AP_VENDOR::ALPSRT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	CASE "Z"
		GOTO ExitTotal IF (AP_VENDOR::ZIP > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	END SELECT

	!
	! Check Wildcard Customer Number
	!
	IF WILD_CUST$ <> ""
	THEN
		GOTO GetNextRec &
			IF COMP_STRING(AP_VENDOR::VENNUM, WILD_CUST$) = 0%
	END IF

17100	!
	! Try to get country definition
	!
	IF AP_VENDOR::COUNTRY = ""
	THEN
		THIS_COUNTRY$ = "US"
	ELSE
		THIS_COUNTRY$ = AP_VENDOR::COUNTRY
	END IF

	UTL_COUNTRY::DESCR = ""

	IF THIS_COUNTRY$ <> "US"
	THEN
		WHEN ERROR IN
			GET #UTL_COUNTRY.CH%, &
				KEY #0% EQ AP_VENDOR::COUNTRY, &
				REGARDLESS
		USE
			UTL_COUNTRY::DESCR = THIS_COUNTRY$
			CONTINUE 17110
		END WHEN
	END IF

17110	!
	! Print out one line
	!
	XLINE$(I%) = "" FOR I% = 1% TO XLINE%

	!
	! Pull name/address up to remove any blank lines
	!
	XLINE1% = 2%	!START AT LINE 2 OF LABEL

	IF INCLUDE_VENNUM$ = "Y"
	THEN
		XLINE$(2%) = TRM$(AP_VENDOR::VENNAM) + &
			"  (" + TRM$(AP_VENDOR::VENNUM) + ")"
	ELSE
		XLINE$(2%) = TRM$(AP_VENDOR::VENNAM)
	END IF

	IF AP_VENDOR::ADD1 <> ""
	THEN
		XLINE1% = XLINE1% + 1%
		XLINE$(XLINE1%) = AP_VENDOR::ADD1
	END IF

	IF AP_VENDOR::ADD2 <> ""
	THEN
		XLINE1% = XLINE1% + 1%
		XLINE$(XLINE1%) = AP_VENDOR::ADD2
	END IF

	XLINE1% = XLINE1% + 1%

	IF UTL_COUNTRY::DESCR = ""
	THEN
		XLINE$(XLINE1%) = AP_VENDOR::CITY + "  " + &
			AP_VENDOR::STATE + "  " + &
			AP_VENDOR::ZIP
	ELSE
		XLINE$(XLINE1%)  = AP_VENDOR::CITY + "  " + UTL_COUNTRY::DESCR
	END IF

	!
	! Create all the labels for this customer.
	!
	FOR J% = 1% TO LABEL_COUNT%

		FOR I% = 1% TO XLINE%
			CALL OUTP_LINENOTITLE("", UTL_REPORTX, XLINE$(I%), 0%)
		NEXT I%

	NEXT J%

17340	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!

 ExitProgram:
	CALL OUTP_FINISHNOTITLE(UTL_REPORTX)

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
