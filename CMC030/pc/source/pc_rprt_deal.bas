1	%TITLE "Deal File List"
	%SBTTL "PC_RPRT_DEAL"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2001 BY
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
	! ID:PC093
	!
	! Abstract:HELP
	!	.p
	!	The ^*Deal File\* option
	!	prints a list of the records in the Deal File. The
	!	report will include the following information:
	!	.lm +10
	!	.b
	!	.list 0,"*"
	!	.le
	!	Product _#
	!	.le
	!	Description
	!	.le
	!	Type
	!	.le
	!	Category
	!	.le
	!	Unit of Measure
	!	.le
	!	Location
	!	.le
	!	Price Type
	!	.le
	!	Date
	!	.le
	!	Time
	!	.le
	!	Price
	!	.els
	!
	! Index:
	!	.x Deal File>Report
	!	.x Report>Deal File
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PC_SOURCE:PC_RPRT_DEAL/LINE
	!	$ LINK/EXE=PC_EXE: PC_RPRT_DEAL, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PC_RPRT_DEAL.OBJ;*
	!
	! Author:
	!
	!	07/23/2001 - Kevin Handy
	!
	! Modification History:
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
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[PC.OPEN]PC_DEAL.HB"
	MAP (PC_DEAL)	PC_DEAL_CDD	PC_DEAL

	%INCLUDE "SOURCE:[PC.OPEN]PC_DEAL_PRODUCT.HB"
	MAP (PC_DEAL_PRODUCT)	PC_DEAL_PRODUCT_CDD	PC_DEAL_PRODUCT

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD	PD_PRODUCT

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD	AR_35CUSTOM

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.p
	!	The ^*From Item\* field
	!	begins the list with a selected item number
	!	code.
	!	.p
	!	If this field is blank, the list will begin with the
	!	first record in the file.
	!
	! Index:
	!	.x From Item>Deal File
	!	.x Prodcut Price File>From Item
	!	.x Item>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.p
	!	The ^*To Item\* field
	!	concludes the list with a selected item code.
	!	.p
	!	If this field is blank, the list will conclude with the last
	!	record in the file.
	!
	! Index:
	!	.x To Item>Deal File
	!	.x Deal File>To Item
	!	.x Item>To
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.p
	!	This ^*Wildcard\* field
	!	selects designated product description
	!	codes to be printed on the list by entering a "product code
	!	wildcard" value.
	!
	! Index:
	!	.x Wildcard>Deal File
	!	.x Deal File>Wildcard
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[PC.OPEN]PC_DEAL.OPN"
	USE
		FILENAME$ = "PC_DEAL"
		CONTINUE HelpError
	END WHEN

305	WHEN ERROR IN
		%INCLUDE "SOURCE:[PC.OPEN]PC_DEAL_PRODUCT.OPN"
	USE
		FILENAME$ = "PC_DEAL_PRODUCT"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "CUSTOMER DEAL FILE BY DEAL"
	TITLE$(2%) = "Product Price & Cost System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Deal                 Product        Description       " + &
		"                  Price  Description"
	TITLE$(5%) = "    Customer   Name                      " + &
		"Start Date  End Date"
	TITLE$(6%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PC_DEAL_PRODUCT.CH%, KEY #0%
		ELSE
			FIND #PC_DEAL_PRODUCT.CH%, &
				KEY #0% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to find beginning record!", 0%)
		CONTINUE ExitProgram
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
		GET #PC_DEAL_PRODUCT.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PC_DEAL_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (PC_DEAL_PRODUCT::DEAL > TO_ITEM$) AND &
		TO_ITEM$ <> ""

	IF WLDCRD$ <> ""
	THEN
		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(PC_DEAL_PRODUCT::DEAL, -1%), &
			WLDCRD$) = 0%
	END IF

17200	IF TEST_PRINTING$ <> PC_DEAL_PRODUCT::DEAL
	THEN
		GOSUB ListCustomers IF TEST_PRINTING$ <> ""

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

		TEST_PRINTING$ = PC_DEAL_PRODUCT::DEAL
	END IF

	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, &
			KEY #0% EQ PC_DEAL_PRODUCT::PRODUCT, &
			REGARDLESS
	USE
		PD_PRODUCT::DESCRIPTION = &
			STRING$(LEN(PD_PRODUCT::DESCRIPTION), A"?"B)
		PD_PRODUCT::PROD_TYPE = &
			STRING$(LEN(PD_PRODUCT::PROD_TYPE), A"?"B)
		PD_PRODUCT::CATEGORY = &
			STRING$(LEN(PD_PRODUCT::CATEGORY), A"?"B)
		PD_PRODUCT::UOM = &
			STRING$(LEN(PD_PRODUCT::UOM), A"?"B)
	END WHEN

17300	!
	! Print out one line
	!
	TEXT$ = PC_DEAL_PRODUCT::DEAL + " " + &
		PC_DEAL_PRODUCT::PRODUCT + " " + &
		LEFT(PD_PRODUCT::DESCRIPTION, 25%) + " " + &
		FORMAT$(PC_DEAL_PRODUCT::PRICE, "#,###,###.#####") + " " + &
		FORMAT$(PC_DEAL_PRODUCT::PERCENT, "#,###,###.#####") + " " + &
		PC_DEAL_PRODUCT::ACCOUNT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

17350	!
	! Try for next record
	!
	GOTO GetNextRec

	%PAGE

 ListCustomers:
17380	!
	! Dump out the customers included in the deal now
	!
	WHEN ERROR IN
		GET #PC_DEAL.CH%, &
			KEY #0% EQ TEST_PRINTING$, &
			REGARDLESS
	USE
		CONTINUE 17390
	END WHEN

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -2%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHILE PC_DEAL::DEAL = TEST_PRINTING$

		WHEN ERROR IN
			GET #AR_35CUSTOM.CH%, &
				KEY #0% EQ PC_DEAL::CUSTOMER, &
				REGARDLESS
		USE
			AR_35CUSTOM::CUSNAM = "??????????"
		END WHEN

		TEXT$ = "    " + PC_DEAL::CUSTOMER + " " + &
			LEFT(AR_35CUSTOM::CUSNAM, 25%) + " " + &
			PRNT_DATE(PC_DEAL::STARTD, 8%) + " " + &
			PRNT_DATE(PC_DEAL::ENDD, 8%) + " " + &
			PC_DEAL::DESCR

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		WHEN ERROR IN
			GET #PC_DEAL.CH%, &
				REGARDLESS
		USE
			CONTINUE 17390
		END WHEN
	NEXT

17390	RETURN

 ExitTotal:
17400	!
	! Handle end of report
	!
	GOSUB ListCustomers IF TEST_PRINTING$ <> ""

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
