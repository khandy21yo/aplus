1	%TITLE "Customer Name/Address Master Report"
	%SBTTL "AR_RPRT_CUSTOM"
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
	!	Telephone	Customer Type
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
	!	.te
	!	Telephone	Type
	!	.te
	!	Category	Alpha Code
	!	.te
	!	Salesman
	!	.end table
	!
	! Index:
	!	.x Print>Customer Names and Addresses
	!	.x Names and Addresses>Print
	!	.x Name>Report
	!	.x Report>Name
	!	.x Address>Report
	!	.x Report>Address
	!	.x Customer>Name & Address Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_RPRT_CUSTOM/LINE
	!	$ LINK/EXECUTABLE=AR_EXE:*.EXE AR_RPRT_CUSTOM, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_RPRT_CUSTOM.OBJ;*
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
	!	02/03/92 - Dan Perkins
	!		Added option to sort by salesman per
	!		request of KING B.
	!
	!	02/05/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/20/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	05/26/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/17/2000 - Kevin Handy
	!		Add wildcard status field
	!		Do 'short-circuit' evaluation on several IF's
	!
	!	11/03/2000 - Kevin Handy
	!		Fix Wildcard Status field
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
	!	The ^*Sort by\* field determines if the
	!	report is to be printed by customer number, customer
	!	type, category, alpha sort or salesman order.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*N\* - Customer Number
	!	.te
	!	^*T\* - Customer Type
	!	.te
	!	^*C\* - Category
	!	.te
	!	^*A\* - Alpha Sort
	!	.te
	!	^*S\* - Salesman
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
	!	The ^*From Item\* field determines the order in which
	!	the report begins printing. The value must be in
	!	agreement with field (01) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	item in the file.
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
	!	The ^*To Item\* field will determines
	!	with which "sortby" item the report will end printing with.
	!	The value must be in agreement with field
	!	(01) Sort by.
	!	.b
	!	A blank field will cause the report to end with the last item
	!	in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Name/Address Report
	!	.x Name/Address Report>To Item
	!
	! --

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated
	!	items to be printed by entering a
	!	"wildcard" value in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	FORMTYPE$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	.x Form Type>Name/Address Report
	!	^*(05) Form Type\*
	!	.b
	!	.lm +5
	!	The ^*Form Type\* field determines
	!	whether the Name/Address Report will be printed in "long" or
	!	"short" form.
	!	.TABLE 3,25
	!	.TE
	!	^*L\* - Long form
	!	.te
	!	^*S\* - Short form
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Name/Address Report>Form Type
	!
	!--

	WLDCRDSTAT$ = TRM$(UTL_REPORTX::OPTDEF(5%))

	!++
	! Abstract:FLD06
	!	^*(04) Wildcard Status\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* status field selects designated
	!	customers by their status to be printed
	!	by entering a
	!	"wildcard" value in this field.
	!	.lm -5
	!
	! Index:
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

		CLOSE AR_CONTOL.CH%
	USE
		CONTINUE 320 IF ERR = 5%
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

320	!
	! Open TEMP file
	!
	SELECT SORTBY$
	CASE "S"
		CALL ENTR_3MESSAGE(SCOPE, "Creating work file . . .", 1% + 16%)

		CALL ASSG_CHANNEL(AR_TEMP.CH%, STAT%)
		CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

		WHEN ERROR IN
			OPEN UTL_WORK.DEV$ + "AR_TEMP.TMP" FOR OUTPUT AS FILE AR_TEMP.CH%, &
				ORGANIZATION INDEXED FIXED, &
				TEMPORARY, &
				BUFFER 32%, &
				MAP AR_35CUSTOM, &
				PRIMARY KEY &
				( &
					AR_35CUSTOM::SALESMAN, &
					AR_35CUSTOM::CUSNUM &
				) DUPLICATES, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "AR_TEMP"
			CONTINUE HelpError
		END WHEN

330		WHEN ERROR IN
			RESET #AR_35CUSTOM.CH%
		USE
			FILENAME$ = "AR_TEMP"
			CONTINUE HelpError
		END WHEN

 GetCustomRec:
		WHEN ERROR IN
			GET #AR_35CUSTOM.CH%, REGARDLESS
		USE
			CONTINUE ReportTitle IF ERR = 11%
			FILENAME$ = "AR_TEMP"
			CONTINUE HelpError
		END WHEN

		PUT #AR_TEMP.CH%

		GOTO GetCustomRec
	END SELECT

 ReportTitle:
	IF SORTBY$ = "S"
	THEN
		CLOSE AR_35CUSTOM.CH%
		AR_35CUSTOM.CH% = AR_TEMP.CH%
	END IF

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

	CASE "S"
		TITLE$(1%) = " Customer Address Report by Salesman Number"
		K_NUM% = 0%
	END SELECT

	TITLE$(2%) = ""

	AR_CONTROL::CTITLE = "Cust#     " IF AR_CONTROL::CTITLE = ""

	!
	! Heading
	!
	IF FORMTYPE$ = "L"
	THEN
		TITLE$(3%) = &
			LEFT$(AR_CONTROL::CTITLE, 10%) + &
			" Name                        " + &
			"                       Phone         Ty" + &
			"pe Cat  Alpha           Sdate    Fg" + &
			" Edate    M S Chrg "

		TITLE$(4%) = &
			"           Address#1                 Ad" + &
			"dress#2                 Address#3      " + &
			"           City            St Zip      " + &
			"  Ctry Cnty  "

		TITLE$(5%) = &
			"           Tax Loc  Term  Carr Salesman" + &
			"         Credit    Dis Backord"

		TITLE$(6%) = "."
	ELSE
		TITLE$(3%) = LEFT$(AR_CONTROL::CTITLE, 10%) + &
			" Name                        " + &
			"                       Phone         Ty" + &
			"pe Cat  Alpha           Salesman"

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
		IF TO_ITEM$ <> ""
		THEN
			GOTO ExitProgram IF (AR_35CUSTOM::CUSNUM > TO_ITEM$)
		END IF

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF COMP_STRING(EDIT$( &
				AR_35CUSTOM::CUSNUM, -1%), WLDCRD$) = 0%
		END IF

	CASE "T"
		IF TO_ITEM$ <> ""
		THEN
			GOTO ExitProgram IF (AR_35CUSTOM::TTYPE > TO_ITEM$)
		END IF

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF COMP_STRING(EDIT$( &
				AR_35CUSTOM::TTYPE, -1%), WLDCRD$) = 0%
		END IF

	CASE "C"
		IF TO_ITEM$ <> ""
		THEN
			GOTO ExitProgram IF (AR_35CUSTOM::CATEGORY > TO_ITEM$)
		END IF

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF COMP_STRING(EDIT$( &
				AR_35CUSTOM::CATEGORY, -1%), WLDCRD$) = 0%
		END IF

	CASE "A"
		IF TO_ITEM$ <> ""
		THEN
			GOTO ExitProgram IF (AR_35CUSTOM::ALPSRT > TO_ITEM$)
		END IF

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF COMP_STRING(EDIT$( &
				AR_35CUSTOM::ALPSRT, -1%), WLDCRD$) = 0%
		END IF

	CASE "S"
		IF TO_ITEM$ <> ""
		THEN
			GOTO ExitProgram IF (AR_35CUSTOM::SALESMAN > TO_ITEM$)
		END IF

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF COMP_STRING(EDIT$( &
				AR_35CUSTOM::SALESMAN, -1%), WLDCRD$) = 0%
		END IF
	END SELECT

	IF WLDCRDSTAT$ <> ""
	THEN
		GOTO GetNextRec &
			IF COMP_STRING(AR_35CUSTOM::SSTATUS, WLDCRDSTAT$) = 0%
	END IF

	!
	! Print out one line if this is the long version.
	!
	IF FORMTYPE$ = "L"
	THEN
		TEXT$ = AR_35CUSTOM::CUSNUM + " " + &
			AR_35CUSTOM::CUSNAM + " " + &
			PRNT_PHONE(AR_35CUSTOM::PHONE, 0%) + " " + &
			AR_35CUSTOM::TTYPE + "   " + &
			AR_35CUSTOM::CATEGORY + " " + &
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
			AR_35CUSTOM::CATEGORY + " " + &
			AR_35CUSTOM::ALPSRT + " " + &
			AR_35CUSTOM::SALESMAN

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
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
