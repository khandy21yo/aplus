1	%TITLE "Customer Name/Address Mailmerge Program"
	%SBTTL "AR_SPEC_MAILCOMMA"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
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
	! ID:AR050
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program generates a mailmerge file for
	!	a comma seperated customer file.
	!	.lm -5
	!
	! Index:
	!	.x Mail Merge>Customer
	!	.x Customer>Mail Merge
	!
	! Option:
	!
	!
	! Input:
	!
	!
	! Output:
	!
	!
	! Author:
	!
	!	08/01/96 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_SPEC_MAILCOMMA/LINE
	!	$ LINK/EXECUTABLE=AR_EXE:*.EXE AR_SPEC_MAILCOMMA, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_SPEC_MAILCOMMA.OBJ;*
	!
	! Modification history:
	!
	!	09/08/97 - Kevin Handy
	!		Lose unecessary external definitions.
	!
	!	06/23/98 - Kevin Handy
	!		Convert to a report so that we could use
	!		the report settingd dialog
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/18/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD		AR_CONTROL

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTACT.HB"
	MAP (AR_CONTACT)	AR_CONTACT_CDD		AR_CONTACT

	%INCLUDE "SOURCE:[OE.OPEN]OE_SHIPTO.HB"
	MAP (OE_SHIPTO)		OE_SHIPTO_CDD		OE_SHIPTO

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
	!	to begin with the selected Item _#. This
	!	field must be in agreement with field (03)
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

	INACTIVE$ = LEFT$(UTL_REPORTX::OPTDEF(4%), 1%)

	!++
	! Abstract:FLD05
	!	^*(05) Keep Inactive Customers\*
	!	.b
	!	.lm +5
	!	.lm -5
	!
	! Index:
	!
	!--

	SHIPTO$ = LEFT$(UTL_REPORTX::OPTDEF(5%), 1%)

	!++
	! Abstract:FLD06
	!	^*(06) Try to use Shipto address\*
	!	.b
	!	.lm +5
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


290	CALL ASSG_CHANNEL(FOUT.CH%, STAT%)

	WHEN ERROR IN
		OPEN CUST_FILE$ FOR OUTPUT AS FILE 1%, &
			RECORDSIZE 512%
	USE
		FILENAME$ = "TEXTFILE"
		CONTINUE HelpError
	END WHEN

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

310	!
	! Shipping address file
	!
	IF SHIPTO$ = "Y"
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_SHIPTO.OPN"
		USE
			CONTINUE 320 IF ERR = 5%
			FILENAME$ = "OE_SHIPTO"
			CONTINUE HelpError
		END WHEN
	END IF

320	!

	%PAGE

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
	TITLE$(4%) = ""

	PREBUFFER$ = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	TOTAL_CUSTOMERS% = 0%
	TOTAL_SHIP% = 0%

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
		CONTINUE 17025 IF ERR = 155%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

17025	!
	! Check current record
	!
	IF INACTIVE$ <> "Y"
	THEN
		GOTO GetNextRec IF (AR_35CUSTOM::SSTATUS <> "A")
	END IF

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
	! Try to get shipping address
	!
	GOTO 17040 IF SHIPTO$ <> "Y"

	WHEN ERROR IN
		GET #OE_SHIPTO.CH%, KEY #0% EQ AR_35CUSTOM::CUSNUM, REGARDLESS
	USE
		CONTINUE 17040
	END WHEN

	!
	! Print out one line using customer address
	!
	NEWTEXT$ = '"' + EDIT$(AR_35CUSTOM::CUSNUM, 4% + 8% + 128%) +  '","' + &
		EDIT$(AR_35CUSTOM::CUSNAM, 4% + 8% + 128%) + '","' + &
		EDIT$(OE_SHIPTO::ADD1, 4% + 8% + 128%) +  '","' + &
		EDIT$(OE_SHIPTO::ADD2, 4% + 8% + 128%) + '","' + &
		EDIT$(OE_SHIPTO::ADD3, 4% + 8% + 128%) + '","' + &
		EDIT$(OE_SHIPTO::CITY, 4% + 8% + 128%) + '",' + &
		EDIT$(OE_SHIPTO::STATE, 4% + 8% + 128%) + "," + &
		EDIT$(OE_SHIPTO::ZIP, 4% + 8% + 128%) + ',' + &
		EDIT$(OE_SHIPTO::COUNTRY, 4% + 8% + 128%) + ',"' + &
		EDIT$(PRNT_PHONE(OE_SHIPTO::PHONE, 0%), 4% + 8% + 128%) + '"'

	WHEN ERROR IN
		PRINT #1%, NEWTEXT$
	USE
		FILENAME$ = "TEXTFILE"
		CONTINUE HelpError
	END WHEN

	TOTAL_CUSTOMERS% = TOTAL_CUSTOMERS% + 1%
	TOTAL_SHIP% = TOTAL_SHIP% + 1%

	GOTO 17090

17040	!
	! Print out one line using customer address
	!
	NEWTEXT$ = '"' + EDIT$(AR_35CUSTOM::CUSNUM, 4% + 8% + 128%) +  '","' + &
		EDIT$(AR_35CUSTOM::CUSNAM, 4% + 8% + 128%) + '","' + &
		EDIT$(AR_35CUSTOM::ADD1, 4% + 8% + 128%) + '","' + &
		EDIT$(AR_35CUSTOM::ADD2, 4% + 8% + 128%) + '","' + &
		EDIT$(AR_35CUSTOM::ADD3, 4% + 8% + 128%) + '","' + &
		EDIT$(AR_35CUSTOM::CITY, 4% + 8% + 128%) + '",' + &
		EDIT$(AR_35CUSTOM::STATE, 4% + 8% + 128%) + "," + &
		EDIT$(AR_35CUSTOM::ZIP, 4% + 8% + 128%) + ',' + &
		EDIT$(AR_35CUSTOM::COUNTRY, 4% + 8% + 128%) + ',"' + &
		EDIT$(PRNT_PHONE(AR_35CUSTOM::PHONE, 0%), 4% + 8% + 128%) + '"'

	WHEN ERROR IN
		PRINT #1%, NEWTEXT$
	USE
		FILENAME$ = "TEXTFILE"
		CONTINUE HelpError
	END WHEN
	TOTAL_CUSTOMERS% = TOTAL_CUSTOMERS% + 1%

17090	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!
	!*******************************************************************
	! Handle end of report
	!*******************************************************************

	TEXT$ = "Generated " + NUM1$(TOTAL_CUSTOMERS%) + " customers"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	IF SHIPTO$ = "Y"
	THEN
		TEXT$ = "Found " + NUM1$(TOTAL_SHIP%) + " shipto addresses"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	TEXT$ = "Customer Text File: " + CUST_FILE$
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

 ExitProgram:
	CLOSE #FOUT.CH%

	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Exit to next program or menu
	!
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

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
