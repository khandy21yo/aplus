1	%TITLE "Customer Name/Address Master Report"
	%SBTTL "OE_RPRT_CUSTOM"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988 BY
	!
	! Computer Management Center
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
	! ID:AR050
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Customer Name/Address Report\* option provides a report which contains
	!	information concerning the customer file. Either a long form
	!	or a short form may be printed.
	!	.b
	!	The "long" form report will include the following information:
	!	.table 3,25
	!	.te
	!	Customer Name	Customer _#
	!	.te
	!	Telephone _#	Customer Type
	!	.te
	!	Category	Alpha Sort Key
	!	.te
	!	Onset Date	Status Flag
	!	.te
	!	Status Date	Method
	!	.te
	!	Statement	Service Charge
	!	.te
	!	Address 1	Address 2
	!	.te
	!	Address 3	City
	!	.te
	!	State	Zip
	!	.te
	!	County	Country
	!	.te
	!	Tax Code	Primary Location
	!	.te
	!	Terms	Carrier Code
	!	.te
	!	Salesperson Number	Credit Limit
	!	.te
	!	Discount Percentage	Backorders
	!	.end table
	!	The "short" form includes:
	!	.table 3,25
	!	.te
	!	Customer Number	Customer Name
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Print>Customer Names and Addresses
	!	.x Names and Addresses>Print
	!	.x Name>Report
	!	.x Report>Name
	!	.x Address>Report
	!	.x Report>Address
	!	.x Customer>Name & Address Listing
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_RPRT_CUSTOM/LINE
	!	$ LINK/EXECUTABLE=OE_EXE:*.EXE OE_RPRT_CUSTOM, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_RPRT_CUSTOM.OBJ;*
	!
	!
	! Author:
	!
	!	07/09/90 - Lance Williams
	!
	! Modification history:
	!
	!	08/14/90 - Craig Tanner
	!		Cleaned up error trapping
	!
	!	09/19/91 - Dan Perkins
	!		Cleared up code.  Refined error trapping.
	!		This program was copied to the AR system
	!		from the OE system.  Both system programs
	!		are the same.
	!
	!	10/04/93 - Kevin Handy
	!		Changed internal name from "AR_RPRT_CUSTOM" to
	!		"OE_RPRT_CUSTOM" so internal and external names
	!		would match.
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
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	12/18/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
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

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD		AR_CONTROL

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTACT.HB"
	MAP (AR_CONTACT)	AR_CONTACT_CDD		AR_CONTACT

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Init:
	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	.x Sort by>Name/Address Report
	!	^*(01) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines the
	!	order in which the report is to be printed.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*N\* - Number
	!	.te
	!	^*A\* - Alpha Sort
	!	.te
	!	^*T\* - Customer Type
	!	.te
	!	^*C\* - Category
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Name/Address Report>Sort by
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field determines the customer
	!	number with which the report will begin printing.
	!	The value must be in agreement with field
	!	(01) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first customer
	!	in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Name/Address Report
	!	.x Name/Address Report>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To item\* field determines the customer
	!	number with which the report will end printing.
	!	The value entered must be in agreement with
	!	field (01) Sort by.
	!	.b
	!	A blank field will cause the report to end with the last customer
	!	in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Name/Address Report
	!	.x Name/Address Report>To Item
	!
	!--

	FORMTYPE$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	.ts 55
	!	^*(04) Form Type\*
	!	.b
	!	.lm +5
	!	The ^*Form Type (L,S)\* field determines
	!	whether the Name/Address Report will be printed in "long" or
	!	"short" form.
	!	.table 3,25
	!	.te
	!	^*L\* - Long form
	!	.te
	!	^*S\* - Short form
	!	.lm -5
	!	.end table
	!
	! Index:
	!	.x Form Type>Name/Address Report
	!	.x Name/Address Report>Form Type
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
	! Open control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.OPN"

		GET #AR_CONTROL.CH%, RECORD 1%, REGARDLESS

		CLOSE AR_CONTROL.CH%
	USE
		CONTINUE 320 IF ERR = 5%
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

320	!
	! Open Contact file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTACT.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "AR_CONTACT"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	! Assign Keys
	!
	SELECT SORTBY$
	CASE "T"
		TITLE$(1%) = " Customer Address Report by Customer Type"
		K_NUM% = 1%

	CASE "C"
		TITLE$(1%) = " Customer Address Report by Category"
		K_NUM% = 2%

	CASE "A"
		TITLE$(1%) = " Customer Address Report by Alpha Sort"
		K_NUM% = 3%

	CASE "N"
		TITLE$(1%) = " Customer Address Report by Customer Number"
		K_NUM% = 0%
	END SELECT

	TITLE$(2%) = ""

	AR_CONTROL::CTITLE = "Cust#     " IF AR_CONTROL::CTITLE = ""

	!
	! Heading
	!
	IF FORMTYPE$ = "L"
	THEN
		TITLE$(3%) = LEFT$(AR_CONTROL::CTITLE, 10%) + &
			" Name                        " + &
			"                       Phone         Ty" + &
			"pe Cat  Alpha           Sdate    Fg" + &
			" Edate    M S Chrg "

		TITLE$(4%) = "           Address#1                 Ad" + &
			"dress#2                 Address#3      " + &
			"           City            St Zip      " + &
			"  Ctry Cnty  "

		TITLE$(5%) = "           Tax Loc  Term  Carr Salesman" + &
			"         Credit    Dis Backord"

		TITLE$(6%) = "."
	ELSE
		TITLE$(3%) = LEFT$(AR_CONTROL::CTITLE, 10%) + &
			" Name                        " + &
			"                       Phone         Ty" + &
			"pe Cat  Alpha        "

		TITLE$(4%) = "."
	END IF

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AR_35CUSTOM.CH%, KEY #K_NUM%
		ELSE
			FIND #AR_35CUSTOM.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF

		RESET #AR_CONTACT.CH%
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
	! Get next record
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
		GOTO ExitProgram IF (AR_35CUSTOM::CUSNUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "T"
		GOTO ExitProgram IF (AR_35CUSTOM::TTYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "C"
		GOTO ExitProgram IF (AR_35CUSTOM::CATEGORY > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE ELSE
		GOTO ExitProgram IF (AR_35CUSTOM::ALPSRT > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	END SELECT

	!
	! Print out one line if this is the long version.
	!
	IF FORMTYPE$ = "L"
	THEN
		TEXT$ = AR_35CUSTOM::CUSNUM + " " + &
			AR_35CUSTOM::CUSNAM + " " + &
			PRNT_PHONE(AR_35CUSTOM::PHONE, 0%) + " " + &
			AR_35CUSTOM::TTYPE + "   " + &
			AR_35CUSTOM::CATEGORY  + " " + &
			AR_35CUSTOM::ALPSRT + " " + &
			PRNT_DATE(AR_35CUSTOM::BDATE, 6%) + " " + &
			AR_35CUSTOM::SSTATUS + "  " + &
			PRNT_DATE(AR_35CUSTOM::EDATE, 6%) + " " + &
			AR_35CUSTOM::METHOD + " " + &
			AR_35CUSTOM::STMTFLG + " " + &
			AR_35CUSTOM::SERCHRG


		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "           " + &
			AR_35CUSTOM::ADD1 + " " + &
			AR_35CUSTOM::ADD2 + " " + &
			AR_35CUSTOM::ADD3 + " " + &
			AR_35CUSTOM::CITY + " " + &
			AR_35CUSTOM::STATE + " " + &
			AR_35CUSTOM::ZIP + " " + &
			AR_35CUSTOM::COUNTRY + "   " + &
			AR_35CUSTOM::COUNTY

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "           " + &
			AR_35CUSTOM::TAXCODE + "  " + &
			AR_35CUSTOM::LOCATION + " " + &
			AR_35CUSTOM::TERMS + "    " + &
			AR_35CUSTOM::CARRIER + "   " + &
			AR_35CUSTOM::SALESMAN + " " + &
			FORMAT$(AR_35CUSTOM::CREDITLIM, "#,###,###.##") + " " + &
			FORMAT$(AR_35CUSTOM::DISCOUNT, "###.##") + " " + &
			AR_35CUSTOM::BACKORDER

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

	ELSE
		!
		! Print out one line if this is the short version.
		!
		TEXT$ = AR_35CUSTOM::CUSNUM + " " + &
			AR_35CUSTOM::CUSNAM + " " + &
			PRNT_PHONE(AR_35CUSTOM::PHONE, 0%) + " " + &
			AR_35CUSTOM::TTYPE + "   " + &
			AR_35CUSTOM::CATEGORY  + " " + &
			AR_35CUSTOM::ALPSRT
		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

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
