1	%TITLE "Pacific Pride Card List"
	%SBTTL "PP_RPRT_CARD"
	%IDENT "V3.5"

	!
	! COPYRIGHT (C) 1992 BY
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
	! ID:PP001
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Pacific Pride Card List\* Report contains
	!	the following information:
	!	.table 3,25
	!	.te
	!	Card Number
	!	.te
	!	Customer Number
	!	.te
	!	Customer Name
	!	.te
	!	Card Type
	!	.te
	!	Description
	!	.te
	!	Odometer
	!	.te
	!	Exempted State
	!	.te
	!	Exemption Authority
	!	.te
	!	Exempted Product
	!	.end table
	!	.lm -5
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS PP_SOURCE:PP_RPRT_CARD/LINE
	!	$ LINK/EXE=PP_EXE: PP_RPRT_CARD, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PP_RPRT_CARD.OBJ;*
	!
	! Author:
	!
	!	12/28/92 - Dan Perkins
	!
	! Modification History:
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/10/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	06/23/2005 - Kevin Handy
	!		Ability to sort by ustomer number
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

	%INCLUDE "SOURCE:[PP.OPEN]PP_CARD.HB"
	MAP (PP_CARD)		PP_CARD_CDD		PP_CARD

	%INCLUDE "SOURCE:[PP.OPEN]PP_CARDEXEMPT.HB"
	MAP (PP_CARDEXEMPT)	PP_CARDEXEMPT_CDD	PP_CARDEXEMPT

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	!
	! Declare external functions
	!
	EXTERNAL INTEGER FUNCTION AR_EXAM_CUSTOM
	EXTERNAL INTEGER FUNCTION PD_EXAM_PRODUCT

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field specifies a starting
	!	point for which item will begin printing.
	!	This is based on the "Sort By" field.
	!	.b
	!	A blank field will cause the report to start with the first item
	!	in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(01) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field specifies the last item
	!	the report will end on.
	!	The item is based on the "Sory By" field.
	!	.b
	!	A blank field will cause the report to end with the
	!	last item in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects
	!	designated items using wildcard characters.
	!	The item is specified by the "Sort By" field.
	!	.b
	!	For information on "Wildcarding" techniques, refer to Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

	EXEMPT$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Print Exemptions\*
	!	.b
	!	.lm +5
	!	The ^*Print Exemptions\* field selects
	!	whether or not exemption information will be printed.
	!	.b
	!	Valid responses are ^*Y\* or ^*N\*
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Sort By\*
	!	.b
	!	.lm +5
	!	Sort by System card number (SY), or customer card number
	!	(CU), or customer ID number (NU).
	!	.lm -5
	!
	! Index:
	!
	!--

	TERMINATED$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Print Terminated\*
	!	.b
	!	.lm +5
	!	The ^*Print Terminated\* field selects
	!	whether or not terminated customers will be printed.
	!	.b
	!	Valid responses are ^*Y\* or ^*N\*
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--


	SELECT SORTBY$

	CASE "CU"
		SORTBY% = 2%
		SORTTITLE$ = "Customer Card Number"

	CASE "NU"
		SORTBY% = 0%
		SORTTITLE$ = "Customer Id Number"

	CASE ELSE
		SORTBY% = 1%
		SORTTITLE$ = "System Card Number"

	END SELECT

300	!
	! Open Card file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PP.OPEN]PP_CARD.OPN"
	USE
		FILENAME$ = "PP_CARD"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Card Exempt file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PP.OPEN]PP_CARDEXEMPT.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "PP_CARDEXEMPT"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "CARD LIST"
	TITLE$(2%) = "Pacific Pride System"
	TITLE$(3%) = "By " + SORTTITLE$
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "CardNum  CustomNum  CustomerName        " + &
		"           CardTyp Description        " + &
		"             Odometer  SysNum  Discount"

	IF EXEMPT$ = "Y"
	THEN
		TITLE$(6%) = "                       State Authority " + &
			"ProductNum     Description"
		TITLE$(7%) = "."
		TITLE$(8%) = ""
	ELSE
		TITLE$(6%) = "."
		TITLE$(7%) = ""
	END IF

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************
	PRINT_FLAG% = 0%

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PP_CARD.CH%, KEY #SORTBY%
		ELSE
			FIND #PP_CARD.CH%, &
				KEY #SORTBY% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "PP_CARD"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get record from Category file
	!
	WHEN ERROR IN
		GET #PP_CARD.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "PP_CARD"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record if should be printed
	!
	SELECT SORTBY%
	CASE 1%
		GOTO ExitProgram IF PP_CARD::SYSCUS > TO_ITEM$ AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
			PP_CARD::SYSCUS, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE 0%
		GOTO ExitProgram IF PP_CARD::CUSNUM > TO_ITEM$ AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
			PP_CARD::CUSNUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE ELSE
		GOTO ExitProgram IF PP_CARD::CARD > TO_ITEM$ AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
			PP_CARD::CARD, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

	!
	! Get Customer information
	!
	IF AR_EXAM_CUSTOM(PP_CARD::CUSNUM, AR_35CUSTOM_EXAM) <> CMC$_NORMAL
	THEN
		AR_35CUSTOM_EXAM::CUSNAM = "**Undefined**"
		AR_35CUSTOM_EXAM::SSTATUS = "I"
	END IF

	IF TERMINATED$ <> "Y" AND AR_35CUSTOM_EXAM::SSTATUS <> "A"
	THEN
		GOTO GetNextRec
	END IF

	!
	! Print out one line
	!
	TEXT$ = PP_CARD::CARD + " " + &
		PP_CARD::CUSNUM + " " + &
		LEFT(AR_35CUSTOM_EXAM::CUSNAM, 30%) + " " + &
		PP_CARD::CTYPE + "       " + &
		LEFT(PP_CARD::DESCRIPTION, 30%) + " " + &
		FORMAT$(PP_CARD::ODOMETER, "###,###.#") + " " + &
		PP_CARD::SYSCUS + " " + &
		PP_CARD::DISCOUNT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO OutaHere IF EXEMPT$ <> "Y"

17100	WHEN ERROR IN
		FIND #PP_CARDEXEMPT.CH%, &
			KEY #0% EQ PP_CARD::CUSNUM + PP_CARD::CARD, &
			REGARDLESS
	USE
		CONTINUE OutaHere IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PP_CARDEXEMPT"
		CONTINUE HelpError
	END WHEN

 GetExemptRec:
17120	WHEN ERROR IN
		GET #PP_CARDEXEMPT.CH%, REGARDLESS
	USE
		CONTINUE OutaHere IF ERR = 11%
		FILENAME$ = "PP_CARDEXEMPT"
		CONTINUE HelpError
	END WHEN

	GOTO OutaHere IF PP_CARDEXEMPT::CUSNUM <> PP_CARD::CUSNUM OR &
		PP_CARDEXEMPT::CARD <> PP_CARD::CARD

	!
	! Get Product information
	!
	V% = PD_EXAM_PRODUCT(PP_CARDEXEMPT::PRODUCT, PD_PRODUCT_EXAM)

	TEXT$ = PP_CARD::CARD + " " + &
		PP_CARD::CUSNUM + " "  + &
		PP_CARDEXEMPT::STATE + "    " + &
		PP_CARDEXEMPT::AUTHORITY + "     " + &
		PP_CARDEXEMPT::PRODUCT + " " + &
		PD_PRODUCT_EXAM::DESCRIPTION

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	PRINT_FLAG% = -1%

	GOTO GetExemptRec

 OutaHere:
	!
	! Try for next record
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%) IF PRINT_FLAG%
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

	!
	! Exit from the program after showing error message
	!
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
