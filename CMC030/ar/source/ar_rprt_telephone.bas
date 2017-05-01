1	%TITLE "Customer Name/Telephone Master Report"
	%SBTTL "AR_RPRT_TELEPHONE"
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
	! ID:AR010
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Telephone Number\* option provides a report which
	!	can be used as a telephone list.
	!	The report will not print anyone who has an end date assigned.
	!	.b
	!	The report will include the following information:
	!	.table 3,25
	!	.te
	!	Customer Name
	!	.te
	!	Telephone _#
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Print>Telephone Number
	!	.x Telephone Numbers>Print
	!	.x Report>Telephone Numbers
	!	.x Customer>Telephone Numbers
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
	! Example:
	!
	! Author:
	!
	!	09/17/91 - JEFF BEARD
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_RPRT_TELEPHONE/LINE
	!	$ LINK/EXECUTABLE=AR_EXE:*.EXE AR_RPRT_TELEPHONE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_RPRT_TELEPHONE.OBJ;*
	!
	! Modification history:
	!
	!	09/19/91 - Kevin Handy
	!		Slight modification to print format, and modified
	!		not to print anyone with an end date on them.
	!
	!	09/04/92 - Dan Perkins
	!		Added wildcard field and modified to print sort fields
	!		on the report.
	!
	!	09/21/92 - Kevin Handy
	!		Clean up (check)
	!
	!	06/15/93 - Kevin Handy
	!		Added REGARDLESS to AR_CONTACT.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/26/95 - Kevin Handy
	!		Lose unecessary externals.
	!		Looked at, but didn't change, for new :sstatus
	!		value of "C" (cash)
	!
	!	08/30/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	07/03/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
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

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	.x Sort by>Name/Address Report
	!	^*(01) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines if the
	!	report is to be printed in customer number order, customer
	!	name order, or alphabetical.
	!	.b
	!	Valid codes are:
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
	!	A blank field will cause the report to begin with the first item
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
	!	The ^*To item\* field determine the customer
	!	number with which the report will end printing. The value
	!	must be in agreement with field (01) Sort by.
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

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects
	!	designated items to be printed by entering a "wildcard"
	!	for Wildcarding Technique.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

300	!
	! Open customer file
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
	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

320	!
	! Open contact file
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
	TITLE$(1%) = EDIT$(AR_CONTROL::CTITLE, -1%) + " TELEPHONE FILE LIST"

	SELECT SORTBY$
	CASE "N"
		K_NUM% = 0%
		TITLE$(2%) = "SORTED BY CUSTOMER NUMBER"

	CASE "T"
		K_NUM% = 1%
		TITLE$(2%) = "SORTED BY CUSTOMER TYPE"

	CASE "C"
		K_NUM% = 2%
		TITLE$(2%) = "SORTED BY CUSTOMER CLASS"

	CASE "A"
		K_NUM% = 3%
		TITLE$(2%) = "SORTED BY CUSTOMER ALPHA"
	END SELECT

	TITLE$(3%) = "AR System"
	TITLE$(4%) = ""

	TITLE$(5%)= "CusNum     Name                                     " + &
		"Tp Cat  Phone"

	TITLE$(6%)= "               Contact Name                      " + &
			" Phone         Extension "

	TITLE$(7%) = "."

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
	! Skip if customer is terminated
	!
	GOTO GetNextRec UNLESS (AR_35CUSTOM::SSTATUS = " ") OR &
		(AR_35CUSTOM::SSTATUS = "A")

	!
	! Check current record
	!
	SELECT SORTBY$

	CASE "N"
		GOTO ExitProgram IF (AR_35CUSTOM::CUSNUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			AR_35CUSTOM::CUSNUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitProgram IF (AR_35CUSTOM::TTYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			AR_35CUSTOM::TTYPE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "C"
		GOTO ExitProgram IF (AR_35CUSTOM::CATEGORY > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			AR_35CUSTOM::CATEGORY, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "A"
		GOTO ExitProgram IF (AR_35CUSTOM::ALPSRT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			AR_35CUSTOM::ALPSRT, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

	TEXT$ = AR_35CUSTOM::CUSNUM + " " + &
		LEFT(AR_35CUSTOM::CUSNAM, 40%) + " " + &
		AR_35CUSTOM::TTYPE + " " + &
		AR_35CUSTOM::CATEGORY + " " + &
		PRNT_PHONE(AR_35CUSTOM::PHONE, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

17100	WHEN ERROR IN
		FIND #AR_CONTACT.CH%, KEY #0% EQ AR_35CUSTOM::CUSNUM, REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 9% OR ERR = 155%
		FILENAME$ = "AR_CONTACT"
		CONTINUE HelpError
	END WHEN

17120	WHEN ERROR IN
		GET #AR_CONTACT.CH%, REGARDLESS
	USE
		AR_CONTACT::CUSNUM       = SPACE$(LEN(AR_CONTACT::CUSNUM))
		AR_CONTACT::CONTACT_NAME = SPACE$(LEN(AR_CONTACT::CONTACT_NAME))
		AR_CONTACT::TITLE        = SPACE$(LEN(AR_CONTACT::TITLE))
		AR_CONTACT::PHONE        = SPACE$(LEN(AR_CONTACT::PHONE))
		AR_CONTACT::EXTENSION    = SPACE$(LEN(AR_CONTACT::EXTENSION))

		CONTINUE GetNextRec IF ERR = 11%
		FILENAME$ = "AR_CONTACT"
		CONTINUE HelpError
	END WHEN

	CUSNAM$ = STRING$(35%, A"."B)

	GOTO GetNextRec IF AR_CONTACT::CUSNUM <> AR_35CUSTOM::CUSNUM

	TEXT$ = SPACE$(15%) + &
		LEFT$(EDIT$(AR_CONTACT::CONTACT_NAME, 16%) + &
			CUSNAM$, 34%) + " " + &
		PRNT_PHONE(AR_CONTACT::PHONE, 0%) + " " + &
		AR_CONTACT::EXTENSION

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -2%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO 17120

 !	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

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
