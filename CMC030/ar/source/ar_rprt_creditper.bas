1	%TITLE "Credit Memo Period Report"
	%SBTTL "AR_RPRT_CREDITPER"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
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
	! ID:AR008
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Credit Report\* program prints a report of issued Credit Memos.
	!	The report can be sorted by Customer Number, Salesman, or Credit Memo.
	!	The report shows totals for each customer or salesman and a grand
	!	total is printed for the total number of credit memos.
	!	.lm -5
	!
	! Index:
	!	.x Credit Report
	!
	! Option:
	!
	! Author:
	!
	!	11/12/91 - Dan Perkins
	!
	! Compile:
	!
	!	$  BAS AR_SOURCE:AR_RPRT_CREDITPER.BAS/LINE
	!	$  LINK/EXE=AR_EXE:*.EXE AR_RPRT_CREDITPER, FUNC_LIB:CMCLINK/OPTION
	!	$  DELETE AR_RPRT_CREDITPER.OBJ;*
	!
	! Modification history:
	!
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/14/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		Change RIGHT(NUM1$()) to FORMAT$().
	!
	!	06/19/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/13/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/24/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	08/25/97 - Kevin Handy
	!		Lose unecessary function definitions
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	10/26/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD		AR_CONTROL

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)		GL_PERIOD_CDD		GL_PERIOD

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN)		AR_OPEN_CDD		AR_OPEN

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION AR_EXAM_CUSTOM

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Set some initial variable values
	!
	TOTAL = 0.0
	THIS_TOTAL = 0.0

 Init:
	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^*(01) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field selects the order
	!	in which the report is to print.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*C\* - Customer Number
	!	.te
	!	^*M\* - Credit Memo Number
	!	.te
	!	^*S\* - Salesman
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort By>Credit
	!	.x Credit>Sort by
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes the
	!	printing to begin with a particular
	!	item. The value must be in agreement with the value
	!	entered in field (01), Sort by.
	!	.b
	!	A blank field will cause the report to begin with
	!	the first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Credit
	!	.x Credit>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes the
	!	printing to end with a particular
	!	item in the file. The value must be in agreement with
	!	the field (01), Sort by.
	!	.b
	!	A blank field causes the report to end with the
	!	last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Credit
	!	.x Credit>To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated
	!	items to be printed by entering a "wildcard" for
	!	Wildcarding Technique.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Credit
	!	.x Credit>Wildcard
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.OPN"
	USE
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

310	!
	! Open the AR Control file, and grab the record
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.OPN"
		GET #AR_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE #AR_CONTROL.CH%
	USE
		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

320	!
	! Open the GL Control File and grab the record
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
		CLOSE #GL_PERIOD.CH%
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	CUR_PERIOD% = AR_CONTROL::LASTPERCLOSE + 1%
	YEAR$       = AR_CONTROL::YEAR

	IF CUR_PERIOD% > GL_PERIOD::FPFY
	THEN
		CUR_PERIOD% = 1%
		YEAR$ = FORMAT$(VAL%(YEAR$) + 1%, "<0>###")
	END IF

	PERIOD$ = YEAR$ + FORMAT$(CUR_PERIOD%, "<0>#")

	%PAGE

	!*******************************************************************
	! Titles
	!*******************************************************************

 ReportTitle:
	!
	! Set up titles
	!
	! Select which method to sort by
	!
	SELECT SORTBY$

	CASE "C"
		K_NUM% = 0%
		TITLE$(1%) = " CREDIT MEMO REPORT BY CUSTOMER"

	CASE "M"
		K_NUM% = 3%
		TITLE$(1%) = " CREDIT MEMO REPORT BY MEMO NUMBER"

		!
		! Routine to load left justified zeros into FROM_ITEM$
		! and TO_ITEM$ if any order numbers are entered as ranges
		!
		FROM_ITEM$ = STRING$(8% - LEN(FROM_ITEM$), A"0"B) + &
			FROM_ITEM$ IF FROM_ITEM$ <> ""

		TO_ITEM$ = STRING$(8% - LEN(TO_ITEM$), A"0"B) + &
			TO_ITEM$ IF TO_ITEM$ <> ""

	CASE "S"
		K_NUM% = 2%
		TITLE$(1%) = " CREDIT MEMO REPORT BY SALESMAN"

	END SELECT

	TITLE$(2%) = "AR System"
	TITLE$(3%) = ""

	!
	! Column headings
	!
	TITLE$(4%) = "Custom #   Name" + SPACE$(27%) + &
		"Memo #   Date     Salesman # " + &
		"Descripton" + SPACE$(24%) + "Amount"

	TITLE$(5%) = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	THIS_ITEM$ = "12345678901"

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AR_OPEN.CH%, KEY #K_NUM%
		ELSE
			FIND #AR_OPEN.CH%, KEY #K_NUM% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CONTINUE ExitTotal IF ERR = 155%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		GET #AR_OPEN.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record if should be printed
	!
	GOTO GetNextRec IF AR_OPEN::TRATYP <> "08"
	GOTO GetNextRec IF AR_OPEN::UPDATED <> PERIOD$

	SELECT SORTBY$

	CASE "C"
		GOSUB SalesmanTotal IF AR_OPEN::CUSNUM <> THIS_ITEM$

		GOTO ExitTotal IF (AR_OPEN::CUSNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			AR_OPEN::CUSNUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		THIS_ITEM$ = AR_OPEN::CUSNUM + ""

	CASE "M"
		GOSUB SalesmanTotal IF AR_OPEN::INVNUM <> THIS_ITEM$

		GOTO ExitTotal IF (AR_OPEN::INVNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			AR_OPEN::INVNUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		THIS_ITEM$ = AR_OPEN::INVNUM + ""

	CASE "S"
		GOSUB SalesmanTotal IF AR_OPEN::SALNUM <> THIS_ITEM$

		GOTO ExitTotal IF (AR_OPEN::SALNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			AR_OPEN::SALNUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		THIS_ITEM$ = AR_OPEN::SALNUM + ""

	END SELECT

	!
	! Look up customer name
	!
	V% = AR_EXAM_CUSTOM(AR_OPEN::CUSNUM, AR_35CUSTOM_EXAM)

	!
	! Print the cash receipt header
	!
	TEXT$ = AR_OPEN::CUSNUM	+ " " + &
		LEFT(AR_35CUSTOM_EXAM::CUSNAM, 30%) + " " + &
		AR_OPEN::INVNUM + " " + &
		PRNT_DATE(AR_OPEN::TRADAT, 6%) + " " + &
		AR_OPEN::SALNUM + " " + &
		AR_OPEN::DESCR + " " + &
		FORMAT$(AR_OPEN::SALAMT, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TOTAL = TOTAL + AR_OPEN::SALAMT
	THIS_TOTAL = THIS_TOTAL + AR_OPEN::SALAMT

	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!
	GOSUB SalesmanTotal

	TOTAL = FUNC_ROUND(TOTAL, 2%)
	TEXT$ = SPACE$(83%) + &
		"Grand Total:  " + FORMAT$(TOTAL, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO ExitProgram

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

	!
	! Print a total for one salesman
	!
 SalesmanTotal:
	SELECT SORTBY$

	CASE "C"
		SALETOTAL$ = "Customer Total:  "

	CASE "M"
		GOTO ReInit

	CASE "S"
		SALETOTAL$ = "Salesman Total:  "

	END SELECT

	IF THIS_ITEM$ <> "12345678901"
	THEN
		THIS_TOTAL = FUNC_ROUND(THIS_TOTAL, 2%)
		TEXT$ = SPACE$(80%) + &
			SALETOTAL$ + FORMAT$(THIS_TOTAL, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -2%)
			GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

 ReInit:
	THIS_TOTAL = 0.0

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
