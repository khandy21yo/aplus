1	%TITLE "Accounts Payable Register Distribution Report"
	%SBTTL "AP_RPRT_USETAX"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1993 BY
	!
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho  83402
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
	! ID:APUSTX
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This report will print all items that have been set to 'Y'
	!	(for use tax) in the Accounts Payable system
	!	for a specified accounting period.
	!
	! Index:
	!	.x Accounts Payable>Reports>AP with Expense Distribution
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_RPRT_USETAX/LINE
	!	$ LINK/EXECUTABLE=AP_EXE: AP_RPRT_USETAX, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_RPRT_USETAX.OBJ;*
	!
	! Author:
	!
	!	03/23/93 - Kevin Handy
	!
	! Modification history:
	!
	!	05/18/93 - Kevin Handy
	!		Trapped EOF at 17210.
	!
	!	06/09/93 - Kevin Handy
	!		Trapped error 155 at 17200.
	!
	!	06/14/93 - Kevin Handy
	!		Added two REGARDLESS to AP_OPEN_DIST.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/28/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/13/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/25/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!******************************************************************
	! External modules needed
	!******************************************************************

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!******************************************************************
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs, etc.)
	!******************************************************************

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP	(AP_OPEN)	AP_OPEN_CDD		AP_OPEN

	%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.HB"
	MAP	(AP_CLOSE)	AP_CLOSE_CDD		AP_CLOSE

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_DIST.HB"
	MAP	(AP_OPEN_DIST)	AP_OPEN_DIST_CDD	AP_OPEN_DIST

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP	(AP_VENDOR)	AP_VENDOR_CDD		AP_VENDOR

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD		GL_CHART

	!
	! Declare variables and constants
	!
	DECLARE	STRING	LYT_LINE

	%PAGE

	!******************************************************************
	! Take care of anything else before starting the report
	!******************************************************************

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Initialization:
	!******************************************************************
	! Get ready to begin
	!******************************************************************

	!
	! Initialize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field determines the item
	!	with which the report will begin.  If the setting is blank,
	!	the report will begin with the first item in the file.
	!	.b
	!	The value entered must be in agreement with the value in field
	!	(03) Sort by.
	!	.lm -5
	!
	! Index:
	!	.x From>Item
	!	.x Item>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field determines the item
	!	with which the report will end. If this
	!	setting is blank, the report will end with the last item in
	!	the file.
	!	.b
	!	The value entered must be in agreement with the value in
	!	field (03) Sort by.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Register Distribution
	!	.x Register Distribution>To Item
	!	.x To>Item
	!	.x Item>To
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Sort Order (NU,NA,S)\*
	!	.b
	!	.lm +5
	!	The ^*Sort Order (NU,NA,S)\* field determines if
	!	the report is to be printed in vendor number order, in alphabetical
	!	order, or in name field order.
	!	.b
	!	.lm 15
	!	.LIST "*"
	!	.LIST ELEMENT
	!	^*NU\*#=#Vendor Number order
	!	.LIST ELEMENT
	!	^*NA\*#=#Name field order
	!	.LIST ELEMENT
	!	^*S\*##=#Alphabetical order
	!	.els
	!	.lm -5
	!	A blank setting will cause the report to print in vendor number
	!	order.
	!	.b
	!	There are no other valid values for this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort Order>Register Distribution
	!	.x Register Distribution>Sort Order
	!
	!--

	YYYY_PP$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Period\*
	!	.b
	!	.lm +5
	!	This field specifies which GL period will be printed.
	!	All entries posted to this period with the use tax flag set
	!	will be printed.  The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!	.x Period
	!
	!--

	TEXT_BATCH$ = ""
	TEXT_BATCH$ = " Period " + BATCH$ IF BATCH$ <> ""

	SELECT SORTBY$

	CASE "NU"
		K_NUM% = 0%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AP_VENDOR::VENNUM))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AP_VENDOR::VENNUM))

	CASE "NA"
		K_NUM% = 1%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AP_VENDOR::VENNAM))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AP_VENDOR::VENNAM))

	CASE ELSE
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AP_VENDOR::ALPSRT))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AP_VENDOR::ALPSRT))
		K_NUM% = 2%

	END SELECT

	%PAGE


300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.OPN"
	USE
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

305	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.OPN"
	USE
		CONTINUE 310 IF ERR = 5%
		FILENAME$ = "AP_CLOSE"
		CONTINUE HelpError
	END WHEN

	!
	! Open the Accounts Payable Open Distribution file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_DIST.OPN"
	USE
		FILENAME$ = "AP_OPEN_DIST"
		CONTINUE HelpError
	END WHEN

	!
	! Open the Accounts Payable Vendor file
	!
320	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.OPN"
	USE
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

	!
	! Open the General Ledger Chart of Accounts file
	!
330	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Accounts Payable Use Tax Report" + TEXT_BATCH$
	TITLE$(2%) = ""

	!
	! Heading
	!
	TITLE$(3%) = "      Trans    Date     Invoice         " + &
		"Account                 Amount"
	TITLE$(4%) = ""

	!
	! Line layout information
	!
	LYT_LINE = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	IF FROM_ITEM$ = ""
	THEN
		RESET #AP_VENDOR.CH%, KEY #K_NUM%
	ELSE
		FIND #AP_VENDOR.CH%, KEY #K_NUM% GE FROM_ITEM$, REGARDLESS
	END IF

 GetNextRec:
17020	!******************************************************************
	! Main report loop starts here
	!******************************************************************

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AP_VENDOR.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Check current record
	!
	SELECT SORTBY$
	CASE "NU"
		GOTO ExitTotal IF (AP_VENDOR::VENNUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "NA"
		GOTO ExitTotal IF (AP_VENDOR::VENNAM > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE ELSE
		GOTO ExitTotal IF (AP_VENDOR::ALPSRT > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	END SELECT

	VENDOR_PRINTED% = 0%

17030	!
	! Look for first open record for vendor
	!
	WHEN ERROR IN
		FIND #AP_OPEN.CH%, KEY #0% EQ AP_VENDOR::VENNUM, REGARDLESS
	USE
		CONTINUE 17050
	END WHEN

17040	WHEN ERROR IN
		GET #AP_OPEN.CH%, REGARDLESS
	USE
		CONTINUE 17050
	END WHEN

	IF AP_OPEN::VENNUM == AP_VENDOR::VENNUM
	THEN
		IF LEFT(AP_OPEN::UPDATED, 6%) = YYYY_PP$
		THEN
			TRANKEY$ = AP_OPEN::TRANKEY
			INVDAT$ = AP_OPEN::INVDAT
			INVNUM$ = AP_OPEN::INVNUM
			GOSUB CheckDist
		END IF

		GOTO 17040
	END IF

17050	!
	! Look for first open record for vendor
	!
	WHEN ERROR IN
		FIND #AP_CLOSE.CH%, KEY #0% EQ AP_VENDOR::VENNUM, REGARDLESS
	USE
		CONTINUE 17090
	END WHEN

17060	WHEN ERROR IN
		GET #AP_CLOSE.CH%, REGARDLESS
	USE
		CONTINUE 17090
	END WHEN

	IF AP_CLOSE::VENNUM == AP_VENDOR::VENNUM
	THEN
		IF LEFT(AP_CLOSE::UPDATED, 6%) = YYYY_PP$
		THEN
			TRANKEY$ = AP_CLOSE::TRANKEY
			INVDAT$ = AP_CLOSE::INVDAT
			INVNUM$ = AP_CLOSE::INVNUM
			GOSUB CheckDist
		END IF

		GOTO 17060
	END IF

17090	!
	! Try for next record
	!
	GOSUB VendorTotal

	GOTO 17020

	%PAGE

 CheckDist:
	!*******************************************************************
	! Look up distribution to see if there is anything use-tax
	!*******************************************************************

17200	WHEN ERROR IN
		FIND #AP_OPEN_DIST.CH%, KEY #0% EQ TRANKEY$, REGARDLESS
	USE
		CONTINUE 17290
	END WHEN

17210	WHEN ERROR IN
		GET #AP_OPEN_DIST.CH%, REGARDLESS
	USE
		CONTINUE 17290
	END WHEN

	IF AP_OPEN_DIST::TRANKEY = TRANKEY$
	THEN
		IF AP_OPEN_DIST::USE_TAX_FLAG = "Y"
		THEN
			IF VENDOR_PRINTED% = 0%
			THEN
				TEXT$ = AP_VENDOR::VENNUM + "  " + &
					AP_VENDOR::VENNAM
				CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
					TEXT$, 0%)

				VENDOR_PRINTED% = -1%
			END IF

			TEXT$ = "      " + &
				AP_OPEN_DIST::TRANKEY + " " + &
				PRNT_DATE(INVDAT$, 8%) + " " + &
				INVNUM$ + " " + &
				AP_OPEN_DIST::ACCT + " " + &
				FORMAT$(AP_OPEN_DIST::AMOUNT, "#,###,###.## ")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

			VENDOR_TOTAL = FUNC_ROUND(VENDOR_TOTAL + &
				AP_OPEN_DIST::AMOUNT, 2%)
			GRAND_TOTAL = FUNC_ROUND(GRAND_TOTAL + &
				AP_OPEN_DIST::AMOUNT, 2%)

		END IF

		GOTO 17210
	END IF

17290	RETURN

	%PAGE

 VendorTotal:
	!
	! Print out totals
	!
	IF VENDOR_PRINTED%
	THEN
		TEXT$ = "          Total      " + SPACE$(37%) + &
			FORMAT$(VENDOR_TOTAL, " #,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	END IF

	VENDOR_TOTAL = 0.0

	RETURN

	%PAGE

	!******************************************************************
	! Handle totals and other items before EXITing
	!******************************************************************

 ExitTotal:
	GOSUB VendorTotal

	!
	! Print out totals
	!
	TEXT$ = "          Grand Total" + SPACE$(37%) + &
		FORMAT$(GRAND_TOTAL, " #,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

 ExitProgram:
	!
	! Finish up the report
	!
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
