1	%TITLE "Operation Labor Variance Report"
	%SBTTL "WP_RPRT_OPRLABVAR"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1993 BY
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
	! ID:WP0026
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Operation Labor Variance\* Report reports labor variances
	!	with the following information:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Operation
	!	.le
	!	Job Number
	!	.le
	!	Period
	!	.le
	!	Operation Account Number
	!	.le
	!	Actual Labor
	!	.le
	!	Buyoff (Pounds or Cases Produced)
	!	.le
	!	Actual Hours
	!	.le
	!	Actual Hourly Rate
	!	.le
	!	Actual Piece Rate
	!	.le
	!	Standard Hourly Rate
	!	.le
	!	Standard Piece Rate
	!	.le
	!	Variance
	!	.els
	!	.lm -5
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_RPRT_OPRLABVAR/LINE
	!	$ LINK/EXE=WP_EXE: WP_RPRT_OPRLABVAR, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_RPRT_OPRLABVAR.OBJ;*
	!
	! Author:
	!
	!	03/04/93 - Dan Perkins
	!
	! Modification History:
	!
	!	03/17/93 - Dan Perkins
	!		Increase Actual Piece Rate, Standard Piece Rate, and
	!		Variance columns to three decimal places.
	!
	!	04/13/93 - Dan Perkins
	!		Added code to deal with hourly rates if there are
	!		no piece rates in PR_OPER.
	!
	!	06/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/30/93 - Frank F. Starman
	!		Added Total $ column.
	!
	!	07/06/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	09/18/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/26/97 - Kevin Handy
	!		Lose unecessary function definitions
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include codes.inc
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP)	GL_YYYY_PP_CDD		GL_YYYY_PP
	DECLARE			GL_YYYY_PP_CDD		GL_YYYY_PP_OLD

	%INCLUDE "SOURCE:[PR.OPEN]PR_OPER.HB"
	DECLARE	PR_OPER_CDD		PR_OPER_READ

	%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.HB"
	DECLARE			BM_PRODOPER_CDD		BM_PRODOPER_READ

	%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.HB"
	MAP (SB_ACCOUNT)	SB_ACCOUNT_CDD		SB_ACCOUNT

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	DECLARE			WP_REGLINE_CDD		WP_REGLINE_READ

	!
	! Declare external functions
	!
	EXTERNAL LONG	FUNCTION BM_READ_PRODOPER
	EXTERNAL LONG	FUNCTION PR_READ_OPERATION
	EXTERNAL LONG	FUNCTION WP_READ_REGLINE

	!
	! Array GL files
	!
	DECLARE INTEGER CONSTANT MAX_RANGE = 100%

	DIM RANGE.CH%(MAX_RANGE)
	DIM GL_YYYY_PP_FILE$(MAX_RANGE)

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	NOT_PRINTED% = -1%

	!
	! Find the GL period files
	!
	CALL READ_DEVICE("GL_YYYY_PP", GL_YYYY_PP.DEV$, STAT%)

	CALL FIND_FILE( GL_YYYY_PP.DEV$ + "GL_*.LED", GL_YYYY_PP_FILE$(), &
		16%, "", "")

	GL_YYYY_PP_FILE% = VAL%(GL_YYYY_PP_FILE$(0%))

	IF GL_YYYY_PP_FILE% = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"General ledger files do not exist", 0%)

		GOTO ExitProgram
	ELSE
		GL_YYYY_PP_FILE$(LOOP%) = &
			MID(GL_YYYY_PP_FILE$(LOOP%), 4%, 4%) + &
			MID(GL_YYYY_PP_FILE$(LOOP%), 9%, 2%) &
			FOR LOOP% = 1% TO GL_YYYY_PP_FILE%
	END IF

	!
	! Get input data from user
	!
	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Operation\*
	!	.b
	!	.lm +5
	!	The ^*From Operation\* field enters a selected
	!	Operation from which the report will begin printing.
	!	.b
	!	A blank field will cause the report to begin with the
	!	first Operation in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Operation\*
	!	.b
	!	.lm +5
	!	The ^*To Operation\* field enters an
	!	Operation with which the report will end printing.
	!	.b
	!	A blank setting will cause the report to end with the
	!	last Operation in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Operation Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Operation Wildcard\* field selects
	!	Operations to be printed on the report by entering a "wildcard"
	!	value in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	FROM_PERIOD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	IF FROM_PERIOD$ = ""
	THEN
		FROM_PERIOD% = 1%
	ELSE
		FROM_PERIOD% = 0%

		FOR I% = 1% TO GL_YYYY_PP_FILE%

			IF FROM_PERIOD$ <= GL_YYYY_PP_FILE$(I%)
			THEN
				FROM_PERIOD% = I%
				GOTO ToPeriod
			END IF
		NEXT I%

		IF FROM_PERIOD% = 0%
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
				"From period not in range", 0%)

			GOTO ExitProgram
		END IF

	END IF

	!++
	! Abstract:FLD04
	!	^*(04) From Perod\*
	!	.b
	!	.lm +5
	!	The ^*From Period\* field enters the period with which
	!	the report will begin printing.
	!	.b
	!	A blank field will cause the report to begin with the earliest
	!	period in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

  ToPeriod:
	TO_PERIOD$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	IF TO_PERIOD$ = ""
	THEN
		TO_PERIOD% = GL_YYYY_PP_FILE%
	ELSE
		TO_PERIOD% = 0%

		FOR I% = 1% TO GL_YYYY_PP_FILE%

			IF TO_PERIOD$ => GL_YYYY_PP_FILE$(I%)
			THEN
				TO_PERIOD% = I%
			END IF
		NEXT I%

		IF TO_PERIOD% = 0%
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
				"To period not in range", 0%)

			GOTO ExitProgram
		END IF

	END IF


	!++
	! Abstract:FLD05
	!	^*(05) To Period
	!	.b
	!	.lm +5
	!	The ^*To Period\* field enters a period with which the
	!	report will end printing.
	!	.b
	!	A blank field will cause the report to end with the last
	!	period in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TOTAL_ONLY$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Totals Only (Y/N)\*
	!	.b
	!	.lm +5
	!	The ^*Totals Only\* field selects
	!	whether only totals for each operation will be displayed.
	!	.lm -5
	!
	! Index:
	!
	!--

	!
	! Open GL period files
	!
	FOR I% = FROM_PERIOD% TO TO_PERIOD%

		YYYY_PP$ = LEFT(GL_YYYY_PP_FILE$(I%), 4%) + "_" + &
			RIGHT(GL_YYYY_PP_FILE$(I%), 5%)

		RANGE.CH%(I%) = -1%

		GL_YYYY_PP.CH% = 0%

300		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"
		USE
			FILENAME$ = "GL_" + YYYY_PP$
			CONTINUE HelpError
		END WHEN

		RANGE.CH%(I%) = GL_YYYY_PP.CH%

	NEXT I%

	!
	! Open SUBACCOUNT File
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.OPN"
	USE
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "OPERATION LABOR VARIANCE REPORT"
	TITLE$(2%) = "Work In Process System"

	TITLE$(3%) = "From " + FROM_PERIOD$ + " To " + &
		TO_PERIOD$

	TITLE$(3%) = "Before " + TO_PERIOD$ IF FROM_PERIOD$ = ""
	TITLE$(3%) = "After " + FROM_PERIOD$ IF TO_PERIOD$ = ""
	TITLE$(3%) = "For All Periods" IF FROM_PERIOD$ + TO_PERIOD$ = ""

	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "Operation"

	TITLE$(6%) = "   Job#        Period  AccountNumber           " + &
		"ActLab   ActLbs   ActHrs ActHrRt ActPcRt" + &
		" StdHrRt StdPcRt  Variance   TotDoll"

	TITLE$(7%) = "."

	%PAGE

1000	!*******************************************************************
	! Generate sort file
	!*******************************************************************

	CALL ENTR_3MESSAGE(SCOPE, "Creating work file ...", 1% + 16%)
	NOT_PRINTED% = -1%

	!
	! Create work file
	!
	CALL ASSG_CHANNEL(GL_TEMP.CH%, STAT%)

	WHEN ERROR IN
		OPEN "GL_TEMP.TMP" AS FILE GL_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP GL_YYYY_PP, &
			TEMPORARY, &
			BUFFER 32%, &
			PRIMARY KEY &
			( &
				GL_YYYY_PP::OPERATION, &
				GL_YYYY_PP::SUBACC, &
				GL_YYYY_PP::ACCT &
			)	DUPLICATES, &
			ACCESS MODIFY, ALLOW NONE
	USE
		FILENAME$ = "GL_TEMP"
		CONTINUE HelpError
	END WHEN

	LAB_WLD$, COMMA$ = ""

1100	WHEN ERROR IN
		FIND #SB_ACCOUNT.CH%, KEY #0% EQ "JC", REGARDLESS
	USE
		CONTINUE DoGL IF ERR = 155%
		FILENAME$ = "SB_ACCOUNT"
		CONTINUE HelpError
	END WHEN

 GetAccountRec:
1120	WHEN ERROR IN
		GET #SB_ACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE DoGL IF ERR = 11%
		FILENAME$ = "SB_ACCOUNT"
		CONTINUE HelpError
	END WHEN

	GOTO DoGL IF SB_ACCOUNT::SYSTEM <> "JC"

	GOTO GetAccountRec IF SB_ACCOUNT::ACCTGROUP <> "DLAB"

	LAB_WLD$ = LAB_WLD$ + COMMA$ + TRM$(SB_ACCOUNT::ACCOUNT)

	COMMA$ = ","

	GOTO GetAccountRec

 DoGL:
	GOTO ExitProgram IF LAB_WLD$ = ""

	FOR I% = FROM_PERIOD% TO TO_PERIOD%

1200		WHEN ERROR IN
			FIND #RANGE.CH%(I%), &
				KEY #1% GT SPACE$(LEN(GL_YYYY_PP::SUBACC)), &
				REGARDLESS
		USE
			CONTINUE 1600 IF ERR = 155%
			FILENAME$ = "GL_YYYY_PP"
			CONTINUE HelpError
		END WHEN

 GetGL:
1220		WHEN ERROR IN
			GET #RANGE.CH%(I%), REGARDLESS
		USE
			CONTINUE 1600 IF ERR = 11%
			FILENAME$ = "GL_YYYY_PP"
			CONTINUE HelpError
		END WHEN

		GOTO GetGL IF TRM$(GL_YYYY_PP::SOURCE) <> "PR"

		GOTO GetGL IF COMP_ARRAY(GL_YYYY_PP::ACCT, LAB_WLD$) = 0%

		!
		! Put the PERIOD in the Batch Number field so we can
		! carry it to the Temp File in order to print later
		!
		GL_YYYY_PP::BTHNUM = GL_YYYY_PP_FILE$(I%)

1500		WHEN ERROR IN
			PUT #GL_TEMP.CH%
		USE
			FILENAME$ = "GL_TEMP"
			CONTINUE HelpError
		END WHEN

		GOTO GetGL

1600	NEXT I%

	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

17000	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #GL_TEMP.CH%
		ELSE
			FIND #GL_TEMP.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "GL_TEMP"
		CONTINUE HelpError
	END WHEN

	NOT_PRINTED% = -1%
	TEST_OPER$ = ""
	TEST_JOB$ = ""

	SUB_AMOUNT, SUB_UNITS, SUB_HOURS = 0.0
	TOT_AMOUNT, TOT_UNITS, TOT_HOURS, TOT_DOL = 0.0
	TOT_STDPCRATE, TOT_CASES, COUNTER = 0.0

 GetNextRec:
	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
17020	WHEN ERROR IN
		GET #GL_TEMP.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_OPER"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (GL_YYYY_PP::OPERATION > TO_ITEM$) AND TO_ITEM$ <> ""

	GOTO GetNextRec IF WLDCRD$ <> "" AND &
		COMP_ARRAY(EDIT$(GL_YYYY_PP::OPERATION, -1%), WLDCRD$) = 0%

	!
	! Find the latest operation.
	! There must be one or we get the next record.
	!
	GOTO GetNextRec IF PR_READ_OPERATION(GL_YYYY_PP::OPERATION, &
		GL_YYYY_PP::TRANDAT, PR_OPER_READ) <> CMC$_NORMAL

	IF GL_YYYY_PP::OPERATION <> TEST_OPER$ AND TEST_OPER$ <> ""
	THEN
		GOSUB PrintLine
		GOSUB OperTotal
	ELSE
		GOSUB PrintLine IF GL_YYYY_PP::SUBACC + GL_YYYY_PP::ACCT <> &
			TEST_JOB$ AND TEST_JOB$ <> ""
	END IF

	TEST_OPER$ = GL_YYYY_PP::OPERATION
	TEST_JOB$  = GL_YYYY_PP::SUBACC + GL_YYYY_PP::ACCT

	GL_YYYY_PP_OLD = GL_YYYY_PP

	SUB_AMOUNT = SUB_AMOUNT + GL_YYYY_PP::AMOUNT
	SUB_UNITS  = SUB_UNITS  + GL_YYYY_PP::UNITS
	SUB_HOURS  = SUB_HOURS  + GL_YYYY_PP::HOURS

	TOT_AMOUNT = TOT_AMOUNT + GL_YYYY_PP::AMOUNT
	TOT_UNITS  = TOT_UNITS  + GL_YYYY_PP::UNITS
	TOT_HOURS  = TOT_HOURS  + GL_YYYY_PP::HOURS

	GOTO GetNextRec

 ExitTotal:
	GOSUB PrintLine
	GOSUB OperTotal

	TEXT$ = SPACE$(LEN(TEXT$) - 15%) + &
		FORMAT$(GTOT_DOL, "Total ##,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

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

 PrintLine:
	!
	! Print the OPERATION if we need to
	!
	IF NOT_PRINTED% AND TOTAL_ONLY$ <> "Y"
	THEN
		TEXT$ = GL_YYYY_PP_OLD::OPERATION

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		NOT_PRINTED% = 0%
	END IF

	!
	! Find the latest operation.
	!
	V% = PR_READ_OPERATION(GL_YYYY_PP_OLD::OPERATION, &
		GL_YYYY_PP_OLD::TRANDAT, PR_OPER_READ)

	!
	! Figure Piece Rate or Hourly Rate
	!
	IF FUNC_ROUND(PR_OPER_READ::PIECE_RATE, 5%) <> 0.0
	THEN
		STDPCRATE = PR_OPER_READ::PIECE_RATE
		STDHRRATE = 0.0
	ELSE
		STDHRRATE = PR_OPER_READ::HOUR_RATE

		!
		! Try to find buyoff qty and Product (Itemcode)
		! from the WP_REGLINE file
		!
		LIN$ = "    "

		V% = WP_READ_REGLINE(GL_YYYY_PP_OLD::SUBACC, LIN$, &
			"GT", WP_REGLINE_READ, QTY())

		SUB_UNITS = QTY(2%)
		TOT_CASES = TOT_CASES + QTY(2%)

		!
		! Try to find hourly rate from the BM_PRODOPER file
		!
		IF BM_READ_PRODOPER(WP_REGLINE_READ::ITEMCODE, &
			GL_YYYY_PP_OLD::OPERATION, "EQ", &
			GL_YYYY_PP_OLD::TRANDAT, &
			BM_PRODOPER_READ) = CMC$_NORMAL
		THEN
			STDPCRATE = FUNC_ROUND(BM_PRODOPER_READ::HOURS * &
				PR_OPER_READ::HOUR_RATE, 3%)
		ELSE
			STDPCRATE = 0.0
		END IF

		TOT_STDPCRATE = TOT_STDPCRATE + STDPCRATE
		COUNTER = COUNTER + 1.0
	END IF

	IF SUB_HOURS <> 0.0
	THEN
		ACTHRRATE = FUNC_ROUND(SUB_AMOUNT / SUB_HOURS, 2%)
	ELSE
		ACTHRRATE = 0.0
	END IF

	IF SUB_UNITS <> 0.0
	THEN
		ACTPCRATE = FUNC_ROUND(SUB_AMOUNT / SUB_UNITS, 3%)
	ELSE
		ACTPCRATE = 0.0
	END IF

	VAR = FUNC_ROUND(ACTPCRATE - STDPCRATE, 3%)

	IF TOTAL_ONLY$ <> "Y"
	THEN
		TEXT$ = "   " + GL_YYYY_PP_OLD::SUBACC + "  " + &
			GL_YYYY_PP_OLD::BTHNUM + "  " + &
			GL_YYYY_PP_OLD::ACCT + "  " + &
			FORMAT$(SUB_AMOUNT, "###,###.##") + "  " + &
			FORMAT$(SUB_UNITS, "###,###") + " " + &
			FORMAT$(SUB_HOURS, "#,###.##") + "  " + &
			FORMAT$(ACTHRRATE, "###.##") + " " + &
			FORMAT$(ACTPCRATE, "<%>##.###") + "  " + &
			FORMAT$(STDHRRATE, "<%>##.##") + " " + &
			FORMAT$(STDPCRATE, "<%>##.###") + " " + &
			FORMAT$(VAR, "#,###.###") + " " + &
			FORMAT$(FUNC_ROUND(SUB_UNITS * VAR, 2%), "##,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	TOT_DOL = TOT_DOL + FUNC_ROUND(SUB_UNITS * VAR, 2%)

	SUB_AMOUNT, SUB_UNITS, SUB_HOURS = 0.0

	RETURN

	%PAGE

 OperTotal:
	IF PR_OPER_READ::PIECE_RATE <> 0.0
	THEN
		STDPCRATE = PR_OPER_READ::PIECE_RATE
		STDHRRATE = 0.0
	ELSE
		STDPCRATE = FUNC_ROUND(TOT_STDPCRATE / COUNTER, 3%)
		STDHRRATE = PR_OPER_READ::HOUR_RATE
		TOT_UNITS = TOT_CASES
	END IF

	IF TOT_HOURS <> 0.0
	THEN
		ACTHRRATE = FUNC_ROUND(TOT_AMOUNT / TOT_HOURS, 2%)
	ELSE
		ACTHRRATE = 0.0
	END IF

	IF TOT_UNITS <> 0.0
	THEN
		ACTPCRATE = FUNC_ROUND(TOT_AMOUNT / TOT_UNITS, 3%)
	ELSE
		ACTPCRATE = 0.0
	END IF

	VAR = FUNC_ROUND(ACTPCRATE - STDPCRATE, 3%)

	TEXT$ = "Total for operation " + &
		PR_OPER_READ::OPER + SPACE$(15%) + &
		FORMAT$(TOT_AMOUNT, "###,###.##") + "  " + &
		FORMAT$(TOT_UNITS, "###,###") + " " + &
		FORMAT$(TOT_HOURS, "#,###.##") + "  " + &
		FORMAT$(ACTHRRATE, "###.##") + " " + &
		FORMAT$(ACTPCRATE, "<%>##.###") + "  " + &
		FORMAT$(STDHRRATE, "<%>##.##") + " " + &
		FORMAT$(STDPCRATE, "<%>##.###") + " " + &
		FORMAT$(VAR, "#,###.###") + " " + &
		FORMAT$(TOT_DOL, "##,###.##")

	GTOT_DOL = GTOT_DOL + TOT_DOL

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	NOT_PRINTED% = -1%

	TOT_AMOUNT, TOT_UNITS, TOT_HOURS, TOT_DOL  = 0.0
	TOT_STDPCRATE, TOT_CASES, COUNTER = 0.0

	RETURN

 HelpError:
	!***************************************************************
	! Help Message for an error
	!***************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	UTL_REPORTX::STAT = -1%
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
