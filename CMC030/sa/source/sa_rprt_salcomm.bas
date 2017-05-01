1	%TITLE "Sales Commission"
	%SBTTL "SA_RPRT_SALCOMM"
	%IDENT "V3.5"

	!
	! COPYRIGHT (C) 1988 BY
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
	! ID:OE002
	!
	! Abstract:HELP
	!	.p
	!	The ^*Sales Commission\* report contains
	!	the following information:
	!	.b
	!	.lm +10
	!	.list 0,"*"
	!	.le
	!	Salesman number
	!	.le
	!	Salesman name
	!	.le
	!	State of salesman
	!	.le
	!	Class
	!	.le
	!	Commission
	!	.els
	!	.lm -10
	!
	! Index:
	!	.x Report>Sales Account
	!	.x Sales Account>Report
	!
	! Compile:
	!
	!	$ BAS SA_SOURCE:SA_RPRT_SALCOMM/LINE
	!	$ LINK/EXE=SA_EXE: SA_RPRT_SALCOMM, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE SA_RPRT_SALCOMM.OBJ;*
	!
	! Author:
	!
	!	07/05/90 - Lance Williams
	!
	! Modification History:
	!
	!	08/16/90 - Craig Tanner
	!		Cleaned up error trapping, general layout
	!
	!	05/24/91 - J. Shad Rydalch
	!		Modified program to accomidate changes made in file
	!		layouts.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	03/30/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/16/97 - Kevin Handy
	!		Reformat source code.
	!
	!	06/03/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/20/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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

	%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.HB"
	MAP (SB_CONTROL)	SB_CONTROL_CDD		SB_CONTROL

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP)	GL_YYYY_PP_CDD		GL_YYYY_PP

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[SA.OPEN]SA_SALESMAN.HB"
	MAP (SB_SUBACCOUNT)	SA_SALESMAN_CDD		SA_SALESMAN

	!
	! Declare external functions
	!
	EXTERNAL LONG FUNCTION SB_READ_ACCOUNT

	%PAGE

	TEMPCLASS$ = "????"
	TEMPTYPE$ = "??"

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^*(01) Sort by (C,S,T)\*
	!	.p
	!	The ^*Sort by\* field determines the order
	!	to print in.
	!	.p
	!	Valid settings are:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	C - Class
	!	.le
	!	S - Subaccount
	!	.le
	!	T - Type
	!	.els
	!	.lm -10
	!	.p
	!	A setting is required in this field.  No other settings are
	!	valid.
	!
	! Index:
	!	.x Sort by
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02)From Item\*
	!	.p
	!	The ^*From Item\* field enters the
	!	item to begin with.
	!	.p
	!	A blank field causes the report to begin with the first
	!	item in the file.
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.p
	!	The ^*To Item\* field specifies the item
	!	to end with.
	!	.p
	!	A blank field causes the report to end with the last
	!	item in the file.
	!
	! Index:
	!
	!--


	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.p
	!	The ^*Wildcard\* field to selects
	!	designated items to be printed by entering a "wildcard"
	!	using the Wildcarding
	!	Technique.
	!
	! Index:
	!
	!--


300	!
	! Open Control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.OPN"

		GET #SB_CONTROL.CH%, KEY #0% EQ "SA", REGARDLESS
		CLOSE #SB_CONTROL.CH%
	USE
		FILENAME$ = "SB_CONTROL"
		CONTINUE HelpError
	END WHEN

	YYYY_PP$ = LEFT$(SB_CONTROL::PERIOD, 4%) + "_" + &
		RIGHT$(SB_CONTROL::PERIOD, 5%)

310	!
	! Open SUBACCOUNT file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
	USE
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

320	!
	! Open GL year period file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "GL_YYYY_PP"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(2%) = "Sales Analysis System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!	      1234567890123456789012345678901234567890
	TITLE$(4%) = "Salesman#   SalesmanName                        ST  Class       Commission"
	TITLE$(5%) = "."
	%PAGE

	!
	! Select which method to sort by
	!
	SELECT SORTBY$
	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = " SALES COMMISSION BY CLASS"
	CASE "S"
		K_NUM% = 0%
		TITLE$(1%) = " SALES COMMISSION BY SALESMAN"
	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = " SALES COMMISSION BY TYPE"
	END SELECT

	%PAGE


17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************
	!
	! If from ACCOUNT blank then reset ACCOUNT file
	! else try to find the first record
	!
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #SB_SUBACCOUNT.CH%, KEY #K_NUM%
		ELSE
			FIND #SB_SUBACCOUNT.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

	LOOP% = LOOP% + 1%

 GetNextRec:
17020	!
	! Main loop
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get record from ACCOUNT file
	!
	WHEN ERROR IN
		GET #SB_SUBACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

	GOTO 17020 IF SA_SALESMAN::SUBJECT <> "S"

	!
	! Check current record if should be printed
	!
	SELECT SORTBY$
	CASE "C"
		GOTO ExitTotal IF (SA_SALESMAN::CLASS > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(SA_SALESMAN::CLASS, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "S"

		GOTO ExitTotal IF (SA_SALESMAN::SALESMAN > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(SA_SALESMAN::SALESMAN, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitTotal IF (SA_SALESMAN::TTYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(SA_SALESMAN::TTYPE, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

	COMMTOTAL = 0.0

17050	WHEN ERROR IN
		FIND #GL_YYYY_PP.CH%, &
			KEY #1% EQ SA_SALESMAN::SALESMAN, &
			REGARDLESS
	USE
		CONTINUE 17020 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "GL_YYYY_PP"
		CONTINUE HelpError
	END WHEN

17060	WHEN ERROR IN
		GET #GL_YYYY_PP.CH%, REGARDLESS
	USE
		CONTINUE 17100 IF ERR = 11%
		FILENAME$ = "GL_YYYY_PP"
		CONTINUE HelpError
	END WHEN

	GOTO 17100 IF GL_YYYY_PP::SUBACC <> SA_SALESMAN::SALESMAN

	V% = SB_READ_ACCOUNT("SA", GL_YYYY_PP::ACCT, FLAG$)

	SELECT FLAG$
	CASE "C"
		COMMTOTAL = COMMTOTAL + GL_YYYY_PP::AMOUNT

	END SELECT

	GOTO 17060

17100	COMMTOTAL = FUNC_ROUND(COMMTOTAL, 2%)

	IF COMMTOTAL <> 0.0
	THEN
		SELECT SORTBY$

		CASE "C"
			GOSUB SubTotals &
				IF (SA_SALESMAN::CLASS <> TEMPCLASS$) AND &
				(FIRSTLINE = -1%)
			TEMPCLASS$ = SA_SALESMAN::CLASS

		CASE "T"
			GOSUB SubTotals &
				IF (SA_SALESMAN::TTYPE <> TEMPTYPE$) AND &
				(FIRSTLINE = -1%)
			TEMPTYPE$ = SA_SALESMAN::TTYPE
		END SELECT

		IF LOOP% = 1%
		THEN
			!
			! Print out one line
			!
			TEXT$ = SA_SALESMAN::SALESMAN + "  " + &
				LEFT$(SA_SALESMAN::DESCR, 35%) + " " + &
				SA_SALESMAN::TTYPE + "  " + &
				SA_SALESMAN::CLASS + "      " + &
				FORMAT$(COMMTOTAL, "#,###,###.##")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			GOTO ExitProgram IF UTL_REPORTX::STAT

			FIRSTLINE = -1%
		END IF

		TOTCOMM = TOTCOMM + COMMTOTAL
		TOTCOMM1 = TOTCOMM1 + COMMTOTAL

		SUBCUST% = SUBCUST% + 1
		CUSTTOTAL% = CUSTTOTAL% + 1
	END IF

	!
	! Try for next record
	!
	GOTO GetNextRec

 SubTotals:
		!
		! Print out one line
		!
		IF LOOP% = 2%
		THEN
		TEXT$ = "            SubTotals for " + &
			FORMAT$(SUBCUST%, "###") + &
			" salesmen                         " + &
			FORMAT$(TOTCOMM, "#,###,###.#")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		END IF

		SUBCUST% = 0%

		TOTCOMM = 0.0
		TOTCOMM1 = 0.0

		RETURN

 ExitTotal:
17400	!
	! Handle end of the report
	!

	IF FIRSTLINE = -1%
	THEN

		GOSUB SubTotals IF (SORTBY$ = "C") OR (SORTBY$ = "T")
		!
		! Print out one line
		!
		TEXT$ = "            Totals for " + &
			FORMAT$(CUSTTOTAL%, "###") + &
			" salesmen                            " + &
			FORMAT$(TOTCOMM1, "#,###,###.#")


		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

	END IF

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
