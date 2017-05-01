1	%TITLE "Ship To Address Report"
	%SBTTL "OE_RPRT_SHIPTO"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1988 BY
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
	! ID:OE020
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Ship To Address\* Report contains
	!	the following information:
	!	.table 3,25
	!	.te
	!	Customer Number	Customer Type
	!	.te
	!	Customer Category	Line
	!	.te
	!	Ship to Name	Ship to Address
	!	.te
	!	City	State
	!	.te
	!	Zip Code	Country
	!	.te
	!	Phone Number	Address No.1
	!	.te
	!	Address No.2	Address No.3
	!	.te
	!	Ship Via	Location
	!	.te
	!	Salesman	Tax Code
	!	.te
	!	Exempt Number
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Ship To Address
	!	.x Ship To Address>Report
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_RPRT_SHIPTO/LINE
	!	$ LINK/EXE=OE_EXE: OE_RPRT_SHIPTO, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_RPRT_SHIPTO.OBJ;*
	!
	! AUTHOR:
	!
	!	06/14/90 - Lance Williams
	!
	! MODIFICATION HISTORY:
	!
	!	08/15/90 - Craig Tanner
	!		Added AR_35CUSTOM file and sortby option
	!
	!	09/23/91 - Dan Perkins
	!		Cleaned up program code.
	!		Cleaned up error trapping.
	!
	!	12/02/91 - Dan Perkins
	!		Added Phone Field to report resulting
	!		from addition of field in record layout.
	!
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/06/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/17/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[OE.OPEN]OE_SHIPTO.HB"
	MAP (OE_SHIPTO)		OE_SHIPTO_CDD		OE_SHIPTO

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

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
	!	.x Sort by>Ship To Address
	!	^*(01) Sort by\*
	!	.B
	!	.lm +5
	!	The ^*Sort by\* field determines if the
	!	report is to be printed in customer number order, customer
	!	type, category or alpha code.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*N\* - Customer Number
	!	.te
	!	^*T\* - Customer Type
	!	.te
	!	^*C\* - Customer Category
	!	.te
	!	^*A\* - Alpha Sort
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Ship To Address Report>Sort by
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field enters the
	!	customer number with which the report is to begin printing.
	!	The value entered must be in agreement with
	!	field (01) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	customer in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field enters the item with which the
	!	report is to end printing.  The value entered must be in
	!	agreement with field (01) Sort by.
	!	.b
	!	A blank field will cause the report to end with the last
	!	customer in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects
	!	designated programs to be printed by entering a "wildcard"
	!	for Wildcarding Technique.
	!	.b
	!	For information on "Wildcarding" techniques refer to Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

300	!
	! Open Custom file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Ship to file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_SHIPTO.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "OE_SHIPTO"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	SELECT SORTBY$
	CASE "T"
		TITLE$(1%) = "SHIP TO ADDRESS REPORT BY CUSTOMER TYPE"
		K_NUM% = 1%

	CASE "C"
		TITLE$(1%) = "SHIP TO ADDRESS REPORT BY CATEGORY"
		K_NUM% = 2%

	CASE "A"
		TITLE$(1%) = "SHIP TO ADDRESS REPORT BY ALPHA SORT"
		K_NUM% = 3%

	CASE "N"
		TITLE$(1%) = "SHIP TO ADDRESS REPORT BY CUSTOMER NUMBER"
		K_NUM% = 0%
	END SELECT

	TITLE$(2%) = " Order Entry System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!	      1234567890123456789012345678901234567890123456789012345
	TITLE$(4%) = "Customer#  CT CCat Line ShipName                   " + &
		"    Address#1                 Address#2" + &
		"                 Address#3"

	TITLE$(5%) = "                        City            St Zip       " + &
		" Ctry   Phone         ShipVia Loc    Salesman   TxC" + &
		" Exempt"

	TITLE$(6%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	!
	! If from category blank then reset Category file
	! else try to find the first record
	!
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AR_35CUSTOM.CH%, KEY #K_NUM%
		ELSE
			FIND #AR_35CUSTOM.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next Order Type record
	!
	WHEN ERROR IN
		GET #AR_35CUSTOM.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORTBY$
	CASE "N"
		GOTO ExitProgram IF (AR_35CUSTOM::CUSNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(AR_35CUSTOM::CUSNUM, -1%), &
			WLDCRD$) = 0% AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitProgram IF (AR_35CUSTOM::TTYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$(AR_35CUSTOM::TTYPE, &
			-1%), WLDCRD$) = 0% AND WLDCRD$ <> ""

	CASE "C"
		GOTO ExitProgram IF (AR_35CUSTOM::CATEGORY > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$(AR_35CUSTOM::CATEGORY, &
			-1%), WLDCRD$) = 0% AND WLDCRD$ <> ""

	CASE "A"
		GOTO ExitProgram IF (AR_35CUSTOM::ALPSRT > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$(AR_35CUSTOM::ALPSRT, &
			-1%), WLDCRD$) = 0% AND WLDCRD$ <> ""
	END SELECT

	!
	! Print out one line
	!
	TEXT$ = AR_35CUSTOM::CUSNUM  + " " + &
		AR_35CUSTOM::TTYPE + " " + &
		AR_35CUSTOM::CATEGORY + "      " + &
		LEFT$(AR_35CUSTOM::CUSNAM, 30%) + " " + &
		AR_35CUSTOM::ADD1 + " " + &
		AR_35CUSTOM::ADD2 + " " + &
		AR_35CUSTOM::ADD3

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)

	TEXT$ = AR_35CUSTOM::CUSNUM + "              " + &
		AR_35CUSTOM::CITY + " " + &
		AR_35CUSTOM::STATE + " " + &
		AR_35CUSTOM::ZIP + " " + &
		AR_35CUSTOM::COUNTRY + "     " + &
		PRNT_PHONE(AR_35CUSTOM::PHONE, 0%) + " " + &
		AR_35CUSTOM::CARRIER  + "      " + &
		AR_35CUSTOM::LOCATION + "   " + &
		AR_35CUSTOM::SALESMAN + " " + &
		AR_35CUSTOM::TAXCODE  + "  " + &
		AR_35CUSTOM::TAXEXEMP

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

17320	!
	! Find Ship to record for customer
	!
	WHEN ERROR IN
		FIND #OE_SHIPTO.CH%, KEY #0% EQ AR_35CUSTOM::CUSNUM, REGARDLESS
	USE
		CONTINUE PrintBlank IF ERR = 155% OR ERR = 9% OR ERR = 11%
		FILENAME$ = "OE_SHIPTO"
		CONTINUE HelpError
	END WHEN

 GetShiptoRec:
	!
	! Get Ship to record
	!
	WHEN ERROR IN
		GET #OE_SHIPTO.CH%, REGARDLESS
	USE
		CONTINUE PrintBlank IF ERR = 155% OR ERR = 9% OR ERR = 11%
		FILENAME$ = "OE_SHIPTO"
		CONTINUE HelpError
	END WHEN

	GOTO PrintBlank IF OE_SHIPTO::CUSNUM <> AR_35CUSTOM::CUSNUM

	!
	! Print line for ship to address
	!
	TEXT$ = OE_SHIPTO::CUSNUM + "         " + &
		OE_SHIPTO::LINES  + "                                " + &
		OE_SHIPTO::ADD1   + " " + &
		OE_SHIPTO::ADD2   + " " + &
		OE_SHIPTO::ADD3

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)

	!
	! Print one line
	!
	TEXT$ = OE_SHIPTO::CUSNUM   + "              " + &
		OE_SHIPTO::CITY     + " " + &
		OE_SHIPTO::STATE    + " " + &
		OE_SHIPTO::ZIP      + " " + &
		OE_SHIPTO::COUNTRY  + "     " + &
		PRNT_PHONE(OE_SHIPTO::PHONE, 0%) + " " + &
		OE_SHIPTO::SHIPVIA  + "      " + &
		OE_SHIPTO::LOCATION + "   " + &
		OE_SHIPTO::SALESMAN + " " + &
		OE_SHIPTO::TAXCODE  + "  " + &
		OE_SHIPTO::TAXEXEMP

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO GetShiptoRec

 PrintBlank:
	!
	! Print blank line
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	!
	! Try for next Custom record
	!
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
