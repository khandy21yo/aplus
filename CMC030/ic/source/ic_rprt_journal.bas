1	%TITLE "Inventory Journal Report"
	%SBTTL "IC_RPRT_JOURNAL"
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
	! ID:IC009
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Journal Report\* option prints a specific
	!	batch.
	!	.lm -5
	!
	! Index:
	!	.x Report>Transaction Journal
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_RPRT_JOURNAL/LINE
	!	$ LINK/EXE=IC_EXE: IC_RPRT_JOURNAL, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_RPRT_JOURNAL.OBJ;*
	!
	! Author:
	!
	!	09/14/87 - Frank F. Starman
	!
	! Modification History:
	!
	!	08/14/89 - Aaron Redd
	!		Added Debit/Credit page(s) to end of report.
	!
	!	08/06/90 - Craig Tanner
	!		Reformatted report for new file layout.
	!
	!	06/10/91 - Craig Tanner
	!		Modified to use GL_OUTP_ACCTSUM to do Debit/Credit page.
	!
	!	08/23/91 - JEFF BEARD
	!		Modified the heading around
	!
	!	11/12/91 - Frank Starman - Dan Perkins
	!		Added Running Total field.  Realigned heading.
	!
	!	12/17/91 - Dan Perkins
	!		Reformatted report for new file layout.
	!		Added TOLOCATION field to report.
	!
	!	12/26/91 - Dan Perkins
	!		Cost now coming directly form journal.  Removed
	!		PC_READ_COST function.
	!
	!	01/28/92 - Frank F. Starman
	!		Change condition when to use EXPACCT.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	10/23/92 - Dan Perkins
	!		Added arguement to GL_OUTP_ACCTSUM because of a change
	!		in that function.
	!
	!	03/18/93 - Kevin Handy
	!		Added parameter to GL_OUTP_ACCTSUM for units.
	!
	!	03/24/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards.
	!		Added additional keys to file: Primary_ref,
	!		Cross_ref, Subaccount.
	!
	!	 04/26/95 - Kevin Handy
	!		Change running total to dollar total.
	!
	!	04/27/95 - Kevin Handy
	!		Change titles from "qty" to "qty-a" and
	!		"qty-b"
	!
	!	09/05/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	10/31/2000 - Kevin Handy
	!		Use A"x"B
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


	!
	! CDD inclusions and related memory allocations
	!
	%INCLUDE "SOURCE:[IC.OPEN]IC_JOURNAL.HB"
	MAP (IC_JOURNAL)	IC_JOURNAL_CDD		IC_JOURNAL

	%INCLUDE "SOURCE:[PD.OPEN]PD_ACCOUNT.HB"
	DECLARE			PD_ACCOUNT_CDD		PD_ACCOUNT_EXAM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION GL_OUTP_ACCTSUM
	EXTERNAL LONG	FUNCTION PD_READ_ACCOUNT
	EXTERNAL LONG   FUNCTION PD_EXAM_PRODUCT

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Product\*
	!	.b
	!	.lm +5
	!	The ^*From Product\* field causes the
	!	report to begin printing with a selected Product number.
	!	.b
	!	A blank setting will cause the report to begin with the first
	!	product number in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Product
	!	.x Product>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Product\*
	!	.b
	!	.lm +5
	!	The ^*To Product\* field causes the report
	!	to end printing with a selected Product number.
	!	.b
	!	A blank setting will cause the report to end with the last Product
	!	number in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Product
	!	.x Product>To
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated
	!	product numbers to be printed by entering a "wildcard" value
	!	in this field.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

	SORT_BY$ = LEFT$(UTL_REPORTX::OPTDEF(3%), 1%)

	!++
	! Abstract:FLD04
	!	^*(04) Sort By\*
	!	.lm +5
	!	.b
	!	Specifies what order the report is to be printed in.
	!	Available orders are:
	!	.b
	!	*P Product
	!	.br
	!	*R Primary Reference
	!	.br
	!	*C Cross Reference
	!	.br
	!	*S Subaccount
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 132%)

	!++
	! Abstract:FLD05
	!	^*(05) Batch _#\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field enters the number
	!	of the batch which is to be printed.
	!	.b
	!	Only one batch at a time may be printed.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number
	!	.x Number>Batch
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_JOURNAL.OPN"
	USE
		FILENAME$ = "IC_JOURNAL"
		CONTINUE HelpError
	END WHEN

	SELECT SORT_BY$
	CASE "R"
		SORT_NAME$ = "Primary Reference"
		SORT_KEY% = 1%
	CASE "C"
		SORT_NAME$ = "Cross Reference"
		SORT_KEY% = 2%
	CASE "S"
		SORT_NAME$ = "Subaccount"
		SORT_KEY% = 3%
	CASE ELSE
		SORT_NAME$ = "Product"
		SORT_KEY% = 0%
	END SELECT

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "INVENTORY  JOURNAL  -  BATCH No. " + BATCH_NO$
	TITLE$(2%) = "Sorted by " + SORT_NAME$
	TITLE$(3%) = "Inventory control system"
	TITLE$(4%) = ""

	!
	! Heading
	!			  1         2         3         4         5
	!		 12345678901234567890123456789012345678901234567890
	TITLE$(5%) = "Product#       Description                        " + &
		"  Loc1 TransDate  PrimaryRef CrossRef   TransAcc"   + &
		"ount#      PriceAmount  Total-A"

	TITLE$(6%) = SPACE$(50%) + &
		"        Loc2  Operator   TT-A      Qty-A      TT-B" + &
		"      Qty-B  SubAccount"

	TITLE$(7%) = "."
	TITLE$(8%) = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF (FROM_ITEM$ = "")
		THEN
			RESET #IC_JOURNAL.CH%, KEY #SORT_KEY%
		ELSE
			FIND #IC_JOURNAL.CH%, &
				KEY #SORT_KEY% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "IC_JOURNAL"
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
		GET #IC_JOURNAL.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "IC_JOURNAL"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORT_BY$
	CASE "R"
		GOTO ExitTotal IF IC_JOURNAL::PRIMARY_REF > TO_ITEM$ AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(IC_JOURNAL::PRIMARY_REF, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""
	CASE "C"
		GOTO ExitTotal IF IC_JOURNAL::CROSS_REF > TO_ITEM$ AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(IC_JOURNAL::CROSS_REF, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""
	CASE "S"
		GOTO ExitTotal IF IC_JOURNAL::SUBACCOUNT > TO_ITEM$ AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(IC_JOURNAL::SUBACCOUNT, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""
	CASE ELSE
		GOTO ExitTotal IF IC_JOURNAL::PRODUCT > TO_ITEM$ AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(IC_JOURNAL::PRODUCT, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""
	END SELECT

	!
	! Get product information
	!
	V% = PD_EXAM_PRODUCT(IC_JOURNAL::PRODUCT, PD_PRODUCT_EXAM)

	COST_QTY_A = FUNC_ROUND(IC_JOURNAL::COST * IC_JOURNAL::QUANTITY_A, 2%)

	!
	! Put the Debit/Credit information into the temporary file
	! (Inventory record)
	!
	RUN_TOTAL = RUN_TOTAL + COST_QTY_A

	!
	! Get the Inventory account number
	!
	PD_ACCOUNT_EXAM::INVACCT = STRING$(LEN(PD_ACCOUNT_EXAM::INVACCT), A"?"B)

	V% =  PD_READ_ACCOUNT(IC_JOURNAL::LOCATION, &
		PD_PRODUCT_EXAM::PROD_TYPE, PD_ACCOUNT_EXAM)

	GOTO ExitTotal &
		IF GL_OUTP_ACCTSUM(OPT_ADDREC, PD_ACCOUNT_EXAM::INVACCT, &
		0.0, COST_QTY_A, 0.0, TITLE$(), UTL_REPORTX) <> CMC$_NORMAL

	IF TRM$(IC_JOURNAL::TOLOCATION) <> ""
	THEN
		RUN_TOTAL = RUN_TOTAL - COST_QTY_A

		IF IC_JOURNAL::EXPACCT <> ""
		THEN
			GOTO ExitTotal &
				IF GL_OUTP_ACCTSUM(OPT_ADDREC, &
				IC_JOURNAL::EXPACCT, &
				0.0, -COST_QTY_A, 0.0, TITLE$(), &
				UTL_REPORTX) <> CMC$_NORMAL
		ELSE

			PD_ACCOUNT_EXAM::INVACCT = &
				STRING$(LEN(PD_ACCOUNT_EXAM::INVACCT), &
				A"?"B)

			V% = PD_READ_ACCOUNT(IC_JOURNAL::TOLOCATION, &
				PD_PRODUCT_EXAM::PROD_TYPE, PD_ACCOUNT_EXAM)

			GOTO ExitTotal &
				IF GL_OUTP_ACCTSUM(OPT_ADDREC, &
				PD_ACCOUNT_EXAM::INVACCT, &
				0.0, -COST_QTY_A, 0.0, TITLE$(), &
				UTL_REPORTX) <> CMC$_NORMAL
		END IF

	ELSE
		IF IC_JOURNAL::EXPACCT <> ""
		THEN
			RUN_TOTAL = RUN_TOTAL - COST_QTY_A

			GOTO ExitTotal &
				IF GL_OUTP_ACCTSUM(OPT_ADDREC, &
				IC_JOURNAL::EXPACCT, &
				0.0, -COST_QTY_A, 0.0, TITLE$(), &
				UTL_REPORTX) <> CMC$_NORMAL
		END IF

	END IF

	!
	! Print out one line
	!
	TEXT$ = IC_JOURNAL::PRODUCT + " " + &
		LEFT(PD_PRODUCT_EXAM::DESCRIPTION, 36%) + " " + &
		IC_JOURNAL::LOCATION + " " + &
		PRNT_DATE(IC_JOURNAL::TRANS_DATE, 8%) + " " + &
		IC_JOURNAL::PRIMARY_REF + "   " + &
		IC_JOURNAL::CROSS_REF + " " + &
		IC_JOURNAL::EXPACCT + "   " + &
		FORMAT$(IC_JOURNAL::COST, "<%>#####.##") + &
		FORMAT$(COST_QTY_A, "######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)

	TEXT$ = SPACE$(58%) + &
		IC_JOURNAL::TOLOCATION + "  " + &
		IC_JOURNAL::STATIONMAN + " " + &
		IC_JOURNAL::TYPE_A + "    " + &
		FORMAT$(IC_JOURNAL::QUANTITY_A, "######.### ") + "    " + &
		IC_JOURNAL::TYPE_B + "   " + &
		FORMAT$(IC_JOURNAL::QUANTITY_B, "######.### ") + " " + &
		IC_JOURNAL::SUBACCOUNT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO GetNextRec

 ExitTotal:
	!
	! Print out the Debit/Credit information
	!
	V% = GL_OUTP_ACCTSUM(OPT_SUMMARY, "", 0.0, 0.0, 0.0, &
		TITLE$(), UTL_REPORTX)

	%PAGE

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
