1	%TITLE "Period Ledger List"
	%SBTTL "JC_RPRT_FULLED"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2000 BY
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
	! ID:JC0006
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Period Ledger List\*
	!	prints a list of the Period Ledger. This list contains the following fields:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Job Number
	!	.le
	!	Job Description
	!	.le
	!	Reference Number
	!	.le
	!	Type
	!	.le
	!	Class
	!	.le
	!	Source
	!	.le
	!	Date
	!	.le
	!	Operation
	!	.le
	!	Account Number
	!	.le
	!	Account Description
	!	.le
	!	Cross Reference Number
	!	.le
	!	Period Balance
	!	.els
	!	.lm -5
	!
	! Index:
	!	.x Period Ledger List
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS JC_SOURCE:JC_RPRT_FULLED/LINE
	!	$ LINK/EXE=JC_EXE: JC_RPRT_FULLED, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE JC_RPRT_FULLED.OBJ;*
	!
	! Author:
	!
	!	11/14/2000 - Kevin Handy
	!		Based on JC_RPRT_PERLED
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
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD		JC_JOB

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.HB"
	MAP (SB_CONTROL)	SB_CONTROL_CDD		SB_CONTROL

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	DECLARE			GL_CHART_CDD		GL_CHART_EXAM

	%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.HB"
	MAP (SB_ACCOUNT)	SB_ACCOUNT_CDD		SB_ACCOUNT

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP)	GL_YYYY_PP_CDD		GL_YYYY_PP

	!
	! External functions
	!
	EXTERNAL LONG		FUNCTION	GL_EXAM_CHART

	!
	! Array for listing
	!
	DECLARE INTEGER CONSTANT MAX_RANGE = 200%
	DIM GL_YYYY_PP_FILE$(MAX_RANGE)

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) Sort by (C,J,T)\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field causes the report to print in
	!	a selected order.
	!	.b
	!	The following values are valid:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	^*C\* = Class
	!	.le
	!	^*J\* = Job Number
	!	.le
	!	^*T\* = Job Type
	!	.els
	!	.lm -5
	!	An entry is required in this field.
	!
	! Index:
	!	.x Sort By
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* entered in this field causes the printing
	!	to begin with the selected item.  The value entered must be in agreement
	!	with field (01) Sort by.
	!	.b
	!	A blank field causes the report to begin with the first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* entered in this field causes the report to end with the
	!	selected item.  The value entered must be in agreement with
	!	field (01) Sort by.
	!	.b
	!	A blank field causes the report to end with the last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field enables the user to print a report including selected
	!	items only using the "wildcarding" technique.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

	STAT_WC$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Status\*
	!	.b
	!	.lm +5
	!	The ^*Status\* field enters the status of the job at
	!	the current time.
	!	.lm -5
	!
	! Index:
	!	.x Status
	!
	!--

	START_PERIOD$ = LEFT$(UTL_REPORTX::OPTDEF(5%), 6%)

	!++
	! Abstract:FLD06
	!	^*(06) Starting GL Period\*
	!	.b
	!	.lm +5
	!	The first period to scan for job detail.
	!	This field is used to speed up the report by allowing
	!	the first period with useful information in it to be
	!	defined, so that this program doesn't have to scan
	!	through more data than necessary.
	!	.lm -5
	!
	! Index:
	!	.x Status
	!
	!--

	END_PERIOD$ = LEFT$(UTL_REPORTX::OPTDEF(6%), 6%)

	!++
	! Abstract:FLD07
	!	^*(06) Ending GL Period\*
	!	.b
	!	.lm +5
	!	The last period to scan for job detail.
	!	This field is used to speed up the report by allowing
	!	the last period with useful information in it to be
	!	defined, so that this program doesn't have to scan
	!	through more data than necessary.
	!	.lm -5
	!
	! Index:
	!	.x Status
	!
	!--

	TOTALS_ONLY$ = LEFT$(UTL_REPORTX::OPTDEF(7%), 1%)

	!++
	! Abstract:FLD08
	!	^*(08) Totals Only (y,N)\*
	!	.b
	!	.lm +5
	!	Only display job totals.
	!	.lm -5
	!
	! Index:
	!	.x Status
	!
	!--

	WLDCRD_ACCOUNT$ = EDIT$(UTL_REPORTX::OPTDEF(9%), -1%)

	!++
	! Abstract:FLD10
	!	^*(10) Wildcard Account\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard Account\* field enables the user to print a report including selected
	!	items only using the "wildcarding" technique.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard Account
	!
	!--

	DEF_SYSTEM$ = "JC"
	DEF_SUBJECT$ = "J"

	!
	! Get a list of all the GL periods
	!
	CALL READ_DEVICE("GL_YYYY_PP", GL_YYYY_PP.DEV$, STAT%)

	CALL FIND_FILE(GL_YYYY_PP.DEV$ + "GL_*.LED", GL_YYYY_PP_FILE$(), &
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

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
	USE
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.OPN"
		GET #SB_CONTROL.CH%, KEY #0% EQ DEF_SYSTEM$, REGARDLESS
		CLOSE SB_CONTROL.CH%
	USE
		FILENAME$ = "SB_CONTROL"
		CONTINUE HelpError
	END WHEN

 !	YYYYPP$ = SB_CONTROL::PERIOD
 !	YYYY_PP$ = LEFT(YYYYPP$, 4%) + "_" + RIGHT(YYYYPP$, 5%)

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.OPN"
	USE
		FILENAME$ = "SB_ACOCUNT"
		CONTINUE HelpError
	END WHEN

400	!
	! Generate a sort file
	!
	CALL ASSG_CHANNEL(GL_TEMP.CH%, STAT%)
	GL_TEMP.NAME$ = GL_YYYY_PP.DEV$ + "GL_SORT.TMP"

	WHEN ERROR IN
		OPEN GL_TEMP.NAME$ FOR OUTPUT AS FILE GL_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			TEMPORARY, &
			BUFFER 32%, &
			MAP GL_YYYY_PP, &
			PRIMARY KEY &
			( &
				GL_YYYY_PP::SUBACC, &
				GL_YYYY_PP::OPERATION, &
				GL_YYYY_PP::ACCT &
			) DUPLICATES, &
			ACCESS MODIFY, ALLOW NONE
	USE
		FILENAME$ = "GL_TEMP"
		CONTINUE HelpError
	END WHEN

	FOR LOOP% = 1% TO GL_YYYY_PP_FILE%

		GOTO 490 &
			IF GL_YYYY_PP_FILE$(LOOP%) < START_PERIOD$ OR &
			(GL_YYYY_PP_FILE$(LOOP%) > END_PERIOD$ AND &
			END_PERIOD$ > "00000000")

		YYYY_PP$ = LEFT(GL_YYYY_PP_FILE$(LOOP%), 4%) + "_" + &
			RIGHT(GL_YYYY_PP_FILE$(LOOP%), 5%)

		CALL ENTR_3MESSAGE(SCOPE, "Sorting " + YYYY_PP$, 1%)

		!
		! Open GL period
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"
		USE
			CONTINUE 490
		END WHEN

		!
		! Copy all of the records with a subaccount on
		! them into the sort file
		!
		WHEN ERROR IN
			FIND #GL_YYYY_PP.CH%, &
				KEY #1% GT "          ", &
				REGARDLESS
		USE
			CONTINUE 480
		END WHEN

420		WHEN ERROR IN
			GET #GL_YYYY_PP.CH%, REGARDLESS
		USE
			CONTINUE 480
		END WHEN

		IF WLDCRD_ACCOUNT$ <> ""
		THEN
			GOTO 420 &
				IF COMP_STRING(GL_YYYY_PP::ACCT, &
				WLDCRD_ACCOUNT$) == 0%
		END IF

		WHEN ERROR IN
			PUT #GL_TEMP.CH%
		USE
			FILENAME$ = "GL_TEMP"
			CONTINUE HelpError
		END WHEN

		GOTO 420

480		CLOSE GL_YYYY_PP.CH%

490	NEXT LOOP%

 ReportTitle:
	!
	! Title
	!
	SELECT SORT_BY$

	CASE "J"
		SORT_KEY% = 0%
		ADD_TITLE$ = "BY  JOB  NUMBER"

	CASE "T"
		SORT_KEY% = 1%
		ADD_TITLE$ = "BY  JOB  TYPE"

	CASE "C"
		SORT_KEY% = 2%
		ADD_TITLE$ = "BY  JOB  CLASS"
	END SELECT

	TITLE$(1%) = "PERIOD  AUDIT  LEDGER  REPORT  " + ADD_TITLE$
	TITLE$(2%) = "Job Costing System"
	TITLE$(3%) = "Starting at Period " + START_PERIOD$ + " to " + &
		END_PERIOD$
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "Job#       Description          Ty Clas " + &
		"Operation Account#           Description"

	TITLE$(6%) = "                Reference#       Source Date     " + &
		"  Description          XRef#              " + &
		"                               PerBalance"

	TITLE$(7%) = "."
	TITLE$(8%) = ""

	%PAGE

	RRECORD% = -1%

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		FIND #SB_SUBACCOUNT.CH%, &
			KEY #SORT_KEY% GE "J" + FROM_ITEM$, &
			REGARDLESS
	USE
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
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
		GET #SB_SUBACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

	GOTO ExitTotal &
		IF JC_JOB::SUBJECT <> DEF_SUBJECT$

	GOTO GetNextRec &
		IF STAT_WC$ <> "" AND COMP_ARRAY(EDIT$( &
		JC_JOB::SSTATUS, -1%), STAT_WC$) = 0%

	SELECT SORT_BY$

	CASE "J"
		GOTO ExitTotal &
			IF (JC_JOB::JOB > TO_ITEM$) AND TO_ITEM$ <> ""

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec &
				IF COMP_ARRAY(EDIT$(JC_JOB::JOB, -1%), WLDCRD$) = 0%
		END IF

	CASE "T"
		GOTO ExitTotal &
			IF (JC_JOB::TTYPE > TO_ITEM$) AND TO_ITEM$ <> ""

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec &
				IF COMP_ARRAY(EDIT$(JC_JOB::TTYPE, -1%), WLDCRD$) = 0%
		END IF

		IF OLD_TYPE$ <> JC_JOB::TTYPE OR RRECORD% = -1%
		THEN
			GOSUB SubTotal &
				IF ST_CUR_BAL <> 0.0 AND RRECORD% = 0%
			OLD_TYPE$ = JC_JOB::TTYPE
			RRECORD% = 0%
		END IF

	CASE "C"
		GOTO ExitTotal &
			IF (JC_JOB::CLASS > TO_ITEM$) AND TO_ITEM$ <> ""

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec &
				IF COMP_ARRAY(EDIT$(JC_JOB::CLASS, -1%), WLDCRD$) = 0%
		END IF

		IF OLD_CLASS$ <> JC_JOB::CLASS OR RRECORD% = -1%
		THEN
			GOSUB Subtotal &
				IF ST_CUR_BAL <> 0.0 AND RRECORD% = 0%
			OLD_CLASS$ = JC_JOB::CLASS
			RRECORD% = 0%
		END IF

	END SELECT

17100	WHEN ERROR IN
		FIND #GL_TEMP.CH%, KEY #0% EQ JC_JOB::JOB, REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155%
		FILENAME$ = "GL_TEMP"
		CONTINUE HelpError
	END WHEN

	TOTAL_LINES% = 0%
	PASS% = 0%
	OLD_ACCT$ = ""

 GetNextRec2:
17120	WHEN ERROR IN
		GET #GL_TEMP.CH%, REGARDLESS
	USE
		CONTINUE PrintSub IF ERR = 11%
		FILENAME$ = "GL_TEMP"
		CONTINUE HelpError
	END WHEN

	GOTO PrintSub &
		IF GL_YYYY_PP::SUBACC <> JC_JOB::JOB

	WHEN ERROR IN
		RESET #SB_ACCOUNT.CH%
	USE
		CONTINUE PrintSub IF ERR = 11%
		FILENAME$ = "SB_ACCOUNT"
		CONTINUE HelpError
	END WHEN

 GetAccountRec:
17200	WHEN ERROR IN
		GET #SB_ACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE GetNextRec2 IF ERR = 11%
		CONTINUE HelpError
	END WHEN

	GOTO GetAccountRec &
		IF COMP_ARRAY(GL_YYYY_PP::ACCT, SB_ACCOUNT::ACCOUNT) = 0%

	IF PASS% = 0%
	THEN
		V% = GL_EXAM_CHART(GL_YYYY_PP::ACCT, GL_CHART_EXAM)

		TEXT$ = JC_JOB::JOB + " " + &
			LEFT(JC_JOB::DESCR, 20%) + " " + &
			JC_JOB::TTYPE + " " + &
			JC_JOB::CLASS + " " + &
			GL_YYYY_PP::OPERATION + "  " + &
			GL_YYYY_PP::ACCT + " " + &
			GL_CHART_EXAM::DESCR

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
		PASS% =  -1%
	ELSE
		IF GL_YYYY_PP::OPERATION <> OLD_OPER$ OR &
			GL_YYYY_PP::ACCT <> OLD_ACCT$
		THEN
			GOSUB PrintAcctTot

			V% = GL_EXAM_CHART(GL_YYYY_PP::ACCT, GL_CHART_EXAM)

			IF TOTALS_ONLY$ <> "Y"
			THEN
				TEXT$ = JC_JOB::JOB + " " + &
					STRING$(20%, A"."B) + "." + &
					STRING$(LEN(JC_JOB::TTYPE), A"."B) + "." + &
					STRING$(LEN(JC_JOB::CLASS), A"."B) + " " + &
					GL_YYYY_PP::OPERATION + "  " + &
					GL_YYYY_PP::ACCT + " " + &
					GL_CHART_EXAM::DESCR

				CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
				GOTO ExitProgram IF UTL_REPORTX::STAT
			END IF

		END IF
	END IF

	OLD_ACCT$ = GL_YYYY_PP::ACCT
	OLD_OPER$ = GL_YYYY_PP::OPERATION

	IF TOTALS_ONLY$ <> "Y"
	THEN
		TEXT$ = JC_JOB::JOB + SPACE$(6%) + &
			GL_YYYY_PP::REFNO + " " + &
			GL_YYYY_PP::SOURCE + "   " + &
			PRNT_DATE(GL_YYYY_PP::TRANDAT, 8%) + " " + &
			LEFT(GL_YYYY_PP::DESCR, 20%) + " " + &
			GL_YYYY_PP::XREFNO + SPACE$(36%) + &
			FORMAT$(GL_YYYY_PP::AMOUNT, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

	END IF

	AT_CUR_BAL = AT_CUR_BAL	+ GL_YYYY_PP::AMOUNT

	TOTAL_LINES% = TOTAL_LINES% + 1%

	!
	! Try for next record of GL_YYYY_PP
	!
	GOTO GetNextRec2

 PrintSub:
	!
	! Print the Job Subtotal
	!
	IF PASS% = -1%
	THEN
		GOSUB PrintAcctTot

		TEXT$ = JC_JOB::JOB + " " + &
			"JOB TOTALS " + STRING$(95%, A"."B) + &
			FORMAT$(JT_CUR_BAL, " ###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		ST_CUR_BAL = ST_CUR_BAL	+ JT_CUR_BAL
		JT_CUR_BAL = 0.0

	END IF

	!
	! Try for next record of SB_SUBACCOUNT
	!
	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!
	SELECT	SORT_BY$

	CASE "J"
		GT_CUR_BAL = ST_CUR_BAL

	CASE "T", "C"
		GOSUB SubTotal &
			IF ST_CUR_BAL <> 0.0

	END SELECT

	TEXT$ = "GRAND TOTALS" + SPACE$(104%) + &
		FORMAT$(GT_CUR_BAL, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

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

 SubTotal:
	IF TOTALS_ONLY$ <> "Y"
	THEN
		SELECT	SORT_BY$

		CASE "T"
			TEXT$ = "TYPE  " + OLD_TYPE$ + " TOTALS" + &
				SPACE$(103% - LEN(OLD_TYPE$))

		CASE "C"
			TEXT$ = "CLASS " + OLD_CLASS$ + " TOTALS" + &
				SPACE$(103% - LEN(OLD_CLASS$))

		END SELECT

		TEXT$ = TEXT$ + FORMAT$(ST_CUR_BAL, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	GT_CUR_BAL = GT_CUR_BAL	+ ST_CUR_BAL
	ST_CUR_BAL = 0.0

	RETURN

 PrintAcctTot:
	IF TOTALS_ONLY$ <> "Y"
	THEN
		IF TOTAL_LINES% > 1%
		THEN
			TEXT$ = JC_JOB::JOB + " " + &
				"OPERATION/ACCOUNT TOTALS " + &
				SPACE$(81%) + &
				FORMAT$(AT_CUR_BAL, " ###,###,###.##")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF
	END IF

	JT_CUR_BAL   = JT_CUR_BAL + AT_CUR_BAL
	AT_CUR_BAL   = 0.0
	TOTAL_LINES% = 0%

	RETURN

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
