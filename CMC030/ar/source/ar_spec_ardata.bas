1	%TITLE "Accounts Receivable History Report"
	%SBTTL "AR_SPEC_ARDATA"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1998 BY
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
	! ID:AR032
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Author:
	!
	!	02/16/1998 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_SPEC_ARDATA/LINE
	!	$ LINK/EXECUTABLE=AR_EXE:*.EXE AR_SPEC_ARDATA, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_SPEC_ARDATA.OBJ;*
	!
	! Modification history:
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/26/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.HB"
	MAP	(AR_CLOSED)	AR_CLOSED_CDD	AR_CLOSED

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP	(AR_OPEN)	AR_OPEN_CDD	AR_OPEN

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP	(AR_35CUSTOM)	AR_35CUSTOM_CDD	AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP	(AR_CONTROL)	AR_CONTROL_CDD	AR_CONTROL

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes the printing
	!	to begin with the selected Item _#. The
	!	value in this field must be in agreement with field (03)
	!	Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	Item _# in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes the printing
	!	to end with the selected Item _#. This
	!	field must be in agreement with field (03) Sort by.
	!	.b
	!	A blank field will cause the report to end with the last
	!	Item _# in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	.x Sort by>Print Accounts Receivable History
	!	^*(03) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines the order
	!	in which the report will print.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*N\* - Number
	!	.te
	!	^*T\* - Type
	!	.te
	!	^*C\* - Category
	!	.te
	!	^*A\* - Alphabetical
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!--

	SELECT SORTBY$
	CASE "N"
		K_NUM% = 0%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AR_35CUSTOM::CUSNUM))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AR_35CUSTOM::CUSNUM))

	CASE "T"
		K_NUM% = 1%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AR_35CUSTOM::TTYPE))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AR_35CUSTOM::TTYPE))

	CASE "T"
		K_NUM% = 2%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AR_35CUSTOM::CATEGORY))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AR_35CUSTOM::CATEGORY))

	CASE "A"
		K_NUM% = 3%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AR_35CUSTOM::ALPSRT))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AR_35CUSTOM::ALPSRT))

	END SELECT

	FROM_DATE$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(3%))

	!++
	! Abstract:FLD04
	!	^*(04) From Date\*
	!	.b
	!	.lm +5
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_DATE$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(4%))

	!++
	! Abstract:FLD05
	!	^*(05) To Date\*
	!	.b
	!	.lm +5
	!	.lm -5
	!
	! Index:
	!
	!--

	GL_WILDCARD$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 132%)
	GL_WILDCARD$ = "*" IF GL_WILDCARD$ = ""

	!++
	! Abstract:FLD06
	!	^*(06) Wildcard Account\*
	!	.b
	!	.lm +5
	!	A ^*Wildcard\* field in the Account History report setting
	!	screen selects designated accounts to be
	!	printed by entering a "wildcard" value.
	!	.lm -5
	!
	! Index:
	!
	!--


	CUST_FILE$ = TRM$(UTL_REPORTX::OPTDEF(6%))

	!++
	! Abstract:FLD07
	!	^*(07) Name for customer data\*
	!	.b
	!	.lm +5
	!	.lm -5
	!
	! Index:
	!
	!--

	INV_FILE$ = TRM$(UTL_REPORTX::OPTDEF(7%))

	!++
	! Abstract:FLD08
	!	^*(07) Name for invoice data\*
	!	.b
	!	.lm +5
	!	.lm -5
	!
	! Index:
	!
	!--


300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.OPN"
	USE
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

305	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.OPN"
	USE
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Customer file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

330	!
	! Open control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.OPN"
		GET #AR_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

340	!
	! Customer file text data
	!
	CALL ASSG_CHANNEL(CUS_DATA.CH%, STAT%)
	WHEN ERROR IN
		OPEN CUST_FILE$ FOR OUTPUT AS FILE CUS_DATA.CH%, &
			RECORDSIZE 500%
	USE
		FILENAME$ = CUST_FILE$
		CONTINUE HelpError
	END WHEN

350	!
	! Customer file text data
	!
	CALL ASSG_CHANNEL(INV_DATA.CH%, STAT%)
	WHEN ERROR IN
		OPEN INV_FILE$ FOR OUTPUT AS FILE INV_DATA.CH%, &
			RECORDSIZE 500%
	USE
		FILENAME$ = INV_FILE$
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Accounts Receivable Ascii File"
	TITLE$(2%) = ""

	!
	! Display Heading
	!
	TITLE$(3%) = "Number of employees"
	TITLE$(TITLE% + 2%) = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	TOTAL_CUSTOMERS% = 0%
	TOTAL_INVOICES% = 0%

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AR_35CUSTOM.CH%, &
				KEY #K_NUM%
		ELSE
			FIND #AR_35CUSTOM.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
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
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORTBY$
	CASE "N"
		GOTO ExitTotal IF (AR_35CUSTOM::CUSNUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "T"
		GOTO ExitTotal IF (AR_35CUSTOM::TTYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "C"
		GOTO ExitTotal IF (AR_35CUSTOM::CATEGORY > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "A"
		GOTO ExitTotal IF (AR_35CUSTOM::ALPSRT > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	END SELECT

17030	!
	! Search for first invoice record for this customer
	!
	THIS_INVOICE$ = "XXXYYYZZZYYYZZZ"
	THIS_AMOUNT = 0.0
	THIS_IDATE$ = ""
	THIS_PDATE$ = ""
	THIS_DDATE$ = ""
	THIS_PAYMENT = 0.0
	THIS_INVTYPE$ = ""

	WHEN ERROR IN
		FIND #AR_CLOSED.CH%, KEY #0% EQ AR_35CUSTOM::CUSNUM, REGARDLESS
	USE
		CONTINUE 17050
	END WHEN

17040	!
	! Pull in next record
	!
	WHEN ERROR IN
		GET #AR_CLOSED.CH%, REGARDLESS
	USE
		CONTINUE 17050 IF ERR = 11%
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

	GOTO 17050 IF AR_35CUSTOM::CUSNUM <> AR_CLOSED::CUSNUM

	GOTO 17040 IF AR_CLOSED::TRADAT < FROM_DATE$ OR &
		AR_CLOSED::TRADAT > TO_DATE$

	IF (AR_CLOSED::INVNUM <> THIS_INVOICE$)
	THEN
		GOSUB DumpInvoice
		THIS_INVOICE$ = AR_CLOSED::INVNUM + ""
	END IF

	SELECT AR_CLOSED::TRATYP

	CASE "02"
		!
		! Cash sale.  Goes in both columns.  Balances to zero.
		!
		THIS_AMOUNT = THIS_AMOUNT + AR_CLOSED::SALAMT
		THIS_IDATE$ = AR_CLOSED::TRADAT
		THIS_PDATE$ = AR_CLOSED::TRADAT
		THIS_DDATE$ = AR_CLOSED::DUEDATE
		THIS_PAYMENT = THIS_PAYMENT + AR_CLOSED::SALAMT

	CASE "09", "10", "11"
		!
		! Payments.  Goes in right column.
		!
		THIS_PDATE$ = AR_CLOSED::TRADAT
		THIS_PAYMENT = THIS_PAYMENT - AR_CLOSED::SALAMT
		THIS_INVTYPE$ = AR_CLOSED::TRATYP IF THIS_INVTYPE$ = ""


	CASE ELSE
		!
		! All else goes in left column
		!
		THIS_INVTYPE$ = AR_CLOSED::TRATYP IF THIS_INVTYPE$ = ""
		THIS_AMOUNT = THIS_AMOUNT + AR_CLOSED::SALAMT
		THIS_IDATE$ = AR_CLOSED::TRADAT
		THIS_DDATE$ = AR_CLOSED::DUEDATE
	END SELECT

	GOTO 17040

17050	WHEN ERROR IN
		FIND #AR_OPEN.CH%, KEY #0% EQ AR_35CUSTOM::CUSNUM, REGARDLESS
	USE
		CONTINUE 17090
	END WHEN

17060	!
	! Pull in next record
	!
	WHEN ERROR IN
		GET #AR_OPEN.CH%, REGARDLESS
	USE
		CONTINUE 17090 IF ERR = 11%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	GOTO 17090 IF AR_35CUSTOM::CUSNUM <> AR_OPEN::CUSNUM

	GOTO 17060 IF AR_OPEN::TRADAT < FROM_DATE$ OR &
		AR_OPEN::TRADAT > TO_DATE$

	IF (AR_OPEN::INVNUM <> THIS_INVOICE$)
	THEN
		GOSUB DumpInvoice
		THIS_INVOICE$ = AR_OPEN::INVNUM + ""
	END IF

	SELECT AR_OPEN::TRATYP

	CASE "02"
		!
		! Cash sale.  Goes in both columns.  Balances to zero.
		!
		THIS_AMOUNT = THIS_AMOUNT + AR_OPEN::SALAMT
		THIS_IDATE$ = AR_OPEN::TRADAT
		THIS_PDATE$ = AR_OPEN::TRADAT
		THIS_DDATE$ = AR_OPEN::DUEDATE
		THIS_PAYMENT = THIS_PAYMENT + AR_OPEN::SALAMT

	CASE "09", "10", "11"
		!
		! Payments.  Goes in right column.
		!
		THIS_PDATE$ = AR_OPEN::TRADAT
		THIS_PAYMENT = THIS_PAYMENT - AR_OPEN::SALAMT
		THIS_INVTYPE$ = AR_OPEN::TRATYP IF THIS_INVTYPE$ = ""

	CASE ELSE
		!
		! All else goes in left column
		!
		THIS_INVTYPE$ = AR_OPEN::TRATYP IF THIS_INVTYPE$ = ""
		THIS_AMOUNT = THIS_AMOUNT + AR_OPEN::SALAMT
		THIS_IDATE$ = AR_OPEN::TRADAT
		THIS_DDATE$ = AR_OPEN::DUEDATE
	END SELECT

	GOTO 17060

17090	!
	! Finish up this customer
	!
	GOSUB DumpInvoice	! Dump any undumped info

	GOTO 17020

	%PAGE

 ExitTotal:
	!*******************************************************************
	! Handle end of report
	!*******************************************************************

	TEXT$ = "Generated " + NUM1$(TOTAL_CUSTOMERS%) + " customers and " + &
		NUM1$(TOTAL_INVOICES%) + " invoices"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 6%)

	TEXT$ = "Customer Text File: " + CUST_FILE$
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 6%)

	TEXT$ = "Invoice Text File:  " + INV_FILE$
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 6%)


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

 DumpInvoice:
18000	!*******************************************************************
	! Dump out all information collected for this one invoice.
	!*******************************************************************

	!
	! Skip if there is nothing here
	!
	GOTO 18090 IF (THIS_AMOUNT = 0.0) AND (THIS_PAYMENT = 0.0)

	THIS_INVTYPE$ = "01" IF THIS_INVTYPE$ = ""

	!
	! If we haven't printed the customer name yet, then do so.
	!
	IF CUSTOMER_PRINTED$ <> AR_35CUSTOM::CUSNUM
	THEN
		TEXT$ = '"' + TRM$(AR_35CUSTOM::CUSNUM) + '","' + &
			TRM$(AR_35CUSTOM::CUSNAM) + '","' + &
			TRM$(AR_35CUSTOM::ADD1) + '","' + &
			TRM$(AR_35CUSTOM::ADD2) + '","' + &
			TRM$(AR_35CUSTOM::ADD3) + '","' + &
			TRM$(AR_35CUSTOM::CITY) + '","' + &
			TRM$(AR_35CUSTOM::STATE) + '","' + &
			TRM$(AR_35CUSTOM::ZIP) + '","' + &
			TRM$(AR_35CUSTOM::PHONE) + '"'

		PRINT #CUS_DATA.CH%, TEXT$

		CUSTOMER_PRINTED$ = AR_35CUSTOM::CUSNUM
		TOTAL_CUSTOMERS% = TOTAL_CUSTOMERS% + 1%
	END IF

	!
	! Print the collected records
	!
	TEXT$ = '"' + TRM$(AR_35CUSTOM::CUSNUM) + '","' + &
		THIS_INVTYPE$ + '","' + &
		TRM$(THIS_INVOICE$) + '",' + &
		EDIT$(FORMAT$(THIS_AMOUNT, "##########.##"), -1%) + ',"' + &
		PRNT_DATE(THIS_IDATE$, 8%) + '","' + &
		PRNT_DATE(THIS_DDATE$, 8%) + '","' + &
		PRNT_DATE(THIS_PDATE$, 8%) + '",' + &
		EDIT$(FORMAT$(THIS_PAYMENT, "##########.##"), -1%)

	PRINT #INV_DATA.CH%, TEXT$

	TOTAL_INVOICES% = TOTAL_INVOICES% + 1%

18090	!
	! Prepare for next invoice number
	!
	THIS_AMOUNT = 0.0
	THIS_IDATE$ = ""
	THIS_PDATE$ = ""
	THIS_DDATE$ = ""
	THIS_PAYMENT = 0.0
	THIS_INVTYPE$ = ""

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
