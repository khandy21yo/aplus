1	%TITLE "Sales Order G/L Account List"
	%SBTTL "OE_RPRT_ACCOUNT"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986, 1988 BY
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
	! ID:OE003
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Sales Order G/L Account Table Report\*
	!	provides a list of sale types with the description
	!	and the corresponding General Ledger account numbers.
	!	.b
	!	The ^*Sales Order G/L Account Table\* Report contains
	!	the following information:
	!	.table 3,25
	!	.te
	!	Customer Type
	!	.te
	!	Customer Description
	!	.te
	!	Sale Type
	!	.te
	!	Sale Description
	!	.te
	!	General Ledger Account Number
	!	.te
	!	Account Description
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Order Entry Account
	!	.x Order Entry Account>Report
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_RPRT_ACCOUNT/LINE
	!	$ LINK/EXE:OE_EXE OE_RPRT_ACCOUNT, -
	!		FUNC_LIB:CMCLINK/OPTION/NOTRACEBACK
	!	$ DELETE OE_RPRT_ACCOUNT.OBJ;*
	!
	! Author:
	!
	!	06/21/90 - J. Shad Rydalch
	!
	! Modification History:
	!
	!	07/06/90 - Lance Williams
	!		Added the Customer Type and Description.
	!
	!	08/14/90 - Craig Tanner
	!		Cleaned up error trapping.
	!
	!	09/16/91 - Dan Perkins
	!		Refined error trapping
	!		Included GL_EXAM_CHART function instead of
	!		opening GL_CHART file.
	!		Include LOCATION field and more account
	!		fields in report.
	!
	!	04/30/92 - Dan Perkins
	!		Modified to accomodate changes in file layout.
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
	!	06/20/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	11/01/2000 - Kevin Handy
	!		Use A"x"B
	!
	!	07/25/2001 - Kevin Handy
	!		Look at the COSACCT instead of the SALES to
	!		decide if the COSACCT should be printed.
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

	%INCLUDE "SOURCE:[OE.OPEN]OE_ACCOUNT.HB"
	MAP (OE_ACCOUNT)	OE_ACCOUNT_CDD		OE_ACCOUNT

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERTYPE.HB"
	MAP (OE_ORDERTYPE)	OE_ORDERTYPE_CDD	OE_ORDERTYPE

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	DECLARE			GL_CHART_CDD		GL_CHART_EXAM

	%INCLUDE "SOURCE:[AR.OPEN]AR_CUSTYPE.HB"
	MAP (AR_CUSTYPE)	AR_CUSTYPE_CDD		AR_CUSTYPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	DECLARE			UTL_LOCATION_CDD	UTL_LOCATION_EXAM

	!
	! Declare external functions
	!
	EXTERNAL LONG    FUNCTION GL_EXAM_CHART
	EXTERNAL LONG    FUNCTION UTL_EXAM_LOCATION

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Sale Type\*
	!	.b
	!	.lm +5
	!	The ^*From Sale Type\* field
	!	enters a sale type from which the report will begin
	!	printing.
	!	.b
	!	A blank field will cause the report to begin with the
	!	first sale type in the file.
	!	.lm -5
	!
	! Index:
	!
	!--
	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Sale Type\*
	!	.b
	!	.lm +5
	!	The ^*To Sale Type\* field
	!	enters a sale type with which the report will end
	!	printing.
	!	.b
	!	A blank field will cause the report to end with the
	!	last sale type in the file.
	!	.lm -5
	!
	! Index:
	!
	!--
	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Sale Type Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Sale Type Wildcard\* field selects
	!	designated sale types to be printed by entering a "wildcard"
	!	value in this field.
	!	.b
	!	For information on "Wildcarding" techniques refer to Appendix B.
	!	.lm -5
	!
	! Index:
	!
	!--

300	!
	! Open Order Account file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_ACCOUNT.OPN"
	USE
		FILENAME$ = "OE_ACCOUNT"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Order Type file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERTYPE.OPN"
	USE
		CONTINUE 330 IF ERR = 5%
		FILENAME$ = "OE_ORDERTYPE"
		CONTINUE HelpError
	END WHEN

	!
	! Read General Ledger Chart file
	!
	V% = GL_EXAM_CHART(OE_ACCOUNT::ACCOUNT, GL_CHART_EXAM)

330	!
	! Open Accounts Receivable Customer file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CUSTYPE.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "AR_CUSTYPE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "ORDER ENTRY ACCOUNT  "
	TITLE$(2%) = "Order Entry System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "CT   Description                       ST   Description" + &
		"                       Location   Description"

	TITLE$(5%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	!
	! If from category blank then reset Category file
	! else try to find the first record
	!
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #OE_ACCOUNT.CH%, KEY #0%
		ELSE
			FIND #OE_ACCOUNT.CH%, &
				KEY #0% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "OE_ACCOUNT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next Order Entry Account record
	!
	WHEN ERROR IN
		GET #OE_ACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "OE_ACCOUNT"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record if should be printed
	!
	GOTO ExitTotal IF (OE_ACCOUNT::ORDTYPE > TO_ITEM$) AND &
		TO_ITEM$ <> ""

	GOTO GetNextRec IF WLDCRD$ <> "" AND &
		COMP_STRING(EDIT$(OE_ACCOUNT::ORDTYPE, -1%), WLDCRD$) = 0%

17050	!
	! Find Description for customer type
	!
	IF INSTR(1%, OE_ACCOUNT::CUSTTYPE, "?") > 0%
	THEN
		AR_CUSTYPE::DESCRIPTION = "Customer Type Overlay Mask"
	ELSE
		WHEN ERROR IN
			GET #AR_CUSTYPE.CH%, &
				KEY #0% EQ OE_ACCOUNT::CUSTTYPE, &
				REGARDLESS
		USE
			AR_CUSTYPE::DESCRIPTION = &
				STRING$(LEN(AR_CUSTYPE::DESCRIPTION), A"?"B)

			CONTINUE 17060 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "AR_CUSTYPE"
			CONTINUE HelpError
		END WHEN
	END IF

17060	!
	! Find Description for Sale type
	!
	IF INSTR(1%, OE_ACCOUNT::ORDTYPE, "?") > 0%
	THEN
		OE_ORDERTYPE::DESCRIPTION = "Sale Type Overlay Mask"
	ELSE
		WHEN ERROR IN
			GET #OE_ORDERTYPE.CH%, &
				KEY #0% EQ OE_ACCOUNT::ORDTYPE, &
				REGARDLESS
		USE
			OE_ORDERTYPE::DESCRIPTION = &
				STRING$(LEN(OE_ORDERTYPE::DESCRIPTION), A"?"B)

			CONTINUE FindLoc IF ERR = 155% OR ERR = 9%
			FILENAME$ = "OE_ORDERTYPE"
			CONTINUE HelpError
		END WHEN
	END IF

 FindLoc:
	!
	! Find Description for location
	!
	IF INSTR(1%, OE_ACCOUNT::LOCATION, "?") > 0%
	THEN
		UTL_LOCATION_EXAM::LOCNAME = "Location Overlay Mask"
	ELSE
		V% = UTL_EXAM_LOCATION(OE_ACCOUNT::LOCATION, UTL_LOCATION_EXAM)
	END IF

 PrintLine:
	!
	! Print out one line
	!
	TEXT$ = OE_ACCOUNT::CUSTTYPE + "   " + &
		LEFT$(AR_CUSTYPE::DESCRIPTION, 30%) + "    " + &
		OE_ACCOUNT::ORDTYPE + "   " + &
		OE_ORDERTYPE::DESCRIPTION + "    " + &
		OE_ACCOUNT::LOCATION + "       " + &
		UTL_LOCATION_EXAM::LOCNAME

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	!
	! Print line for account number & description
	!
	V% = GL_EXAM_CHART(OE_ACCOUNT::ACCOUNT, GL_CHART_EXAM)

	TEXT$ = "          AR Account #       " + OE_ACCOUNT::ACCOUNT + &
		"     " + GL_CHART_EXAM::DESCR

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	!
	! Print line for GL Discount account number & description
	!
	IF OE_ACCOUNT::DISACCT <> ""
	THEN
		V% = GL_EXAM_CHART(OE_ACCOUNT::DISACCT, GL_CHART_EXAM)

		TEXT$ = "          Disc Account #     " + &
			OE_ACCOUNT::DISACCT + "     " + GL_CHART_EXAM::DESCR

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	!
	! Print line for freight account number & description
	!
	IF OE_ACCOUNT::FRACCT <> ""
	THEN
		V% = GL_EXAM_CHART(OE_ACCOUNT::FRACCT, GL_CHART_EXAM)

		TEXT$ = "          Freight Acct #     " + &
			OE_ACCOUNT::FRACCT + "     " + GL_CHART_EXAM::DESCR

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	!
	! Print line for handling account number & description
	!
	IF OE_ACCOUNT::HANDLING <> ""
	THEN
		V% = GL_EXAM_CHART(OE_ACCOUNT::HANDLING, GL_CHART_EXAM)

		TEXT$ = "          Handling Acct #    " + &
			OE_ACCOUNT::HANDLING + "     " + GL_CHART_EXAM::DESCR

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	!
	! Print line for sales account number & description
	!
	IF OE_ACCOUNT::SALES <> ""
	THEN
		V% = GL_EXAM_CHART(OE_ACCOUNT::SALES, GL_CHART_EXAM)

		TEXT$ = "          Sales Acct #       " + &
			OE_ACCOUNT::SALES + "     " + GL_CHART_EXAM::DESCR

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	!
	! Print line for cost of sale account number & description
	!
	IF OE_ACCOUNT::COSACCT <> ""
	THEN
		V% = GL_EXAM_CHART(OE_ACCOUNT::COSACCT, GL_CHART_EXAM)

		TEXT$ = "          Sales Cost Acct #  " + &
			OE_ACCOUNT::COSACCT + "     " + GL_CHART_EXAM::DESCR

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	!
	! Print a blank line
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
17400	!
	! Handle end of report
	!

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
	! Resume to display untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
