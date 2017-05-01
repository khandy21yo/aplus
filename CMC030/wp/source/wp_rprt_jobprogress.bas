1	%TITLE "Job Status Summary Report"
	%SBTTL "WP_RPRT_JOBPROGRESS"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2003 BY
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
	! Software Solutions, Inc. assumes no responsibility for the
	! use or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! ID:WP0050
	!
	! Abstract:HELP
	!	.lm +5
	!	.b
	!	The ^*Job Status Summary\* report contains the following information:
	!	.table 3,25
	!	.te
	!	Job Number	Job Description
	!	.te
	!	Type	Class
	!	.te
	!	Order Date	Location
	!	.te
	!	Operator	Reference No
	!	.te
	!	Status Flag	Closed Date
	!	.te
	!	Line	Transaction Type
	!	.te
	!	Item code	Description
	!	.te
	!	Quantity Ordered	Quantity Completed
	!	.te
	!	Quantity Cancelled	Balance
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Job Status Summary
	!	.x Job Status Summary>Report
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_RPRT_JOBPROGRESS/LINE
	!	$ LINK/EXE=WP_EXE: WP_RPRT_JOBPROGRESS, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_RPRT_JOBPROGRESS.OBJ;*
	!
	! Author:
	!
	!	02/17/2003 - Kevin Handy
	!		Based on WP_RPRT_REGSUMMARY and WP_WRIT_CLOSEJOUR
	!
	! Modification History:
	!
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include codes
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	MAP (WP_REGLINE)	WP_REGLINE_CDD		WP_REGLINE
	DECLARE			WP_REGLINE_CDD		WP_REGLINE_READ

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD		JC_JOB

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	MAP (WP_REQREGISTER)	WP_REQREGISTER_CDD	WP_REQREGISTER
	DECLARE			WP_REQREGISTER_CDD	WP_REQREGISTER_READ

	%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.HB"
	MAP (SB_BALANCE)	SB_BALANCE_CDD		SB_BALANCE

	%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.HB"
	MAP (SB_ACCOUNT)	SB_ACCOUNT_CDD		SB_ACCOUNT
	DIM SB_ACCOUNT_CDD SB_ACCOUNT_ARRAY(100)

	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.HB"
	MAP (BM_RELATION)	BM_RELATION_CDD		BM_RELATION

	%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.HB"
	DECLARE			BM_PRODOPER_CDD		BM_PRODOPER_READ

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[BM.OPEN]BM_CONTROL.HB"
	MAP (BM_CONTROL)	BM_CONTROL_CDD		BM_CONTROL

	%INCLUDE "SOURCE:[PR.OPEN]PR_OPER.HB"
	DECLARE			PR_OPER_CDD		PR_OPER_READ

	%INCLUDE "SOURCE:[WP.OPEN]WP_CONTROL.HB"
	MAP (WP_CONTROL)	WP_CONTROL_CDD		WP_CONTROL

	%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.HB"
	MAP (SB_CONTROL)	SB_CONTROL_CDD		SB_CONTROL

	!
	! Declare external functions
	!
	EXTERNAL LONG   FUNCTION WP_READ_REGLINE
	EXTERNAL LONG	FUNCTION WP_READ_REQREGISTER
	EXTERNAL LONG	FUNCTION BM_READ_PRODOPER
	EXTERNAL REAL	FUNCTION PC_READ_COST
	EXTERNAL LONG	FUNCTION PR_READ_OPERATION
	EXTERNAL LONG	FUNCTION PD_EXAM_PRODUCT

	COM (CH_WP_REGLINE_READ)	WP_REGLINE.CH%
	COM (CH_WP_REQREGISTER_READ)	WP_REQREGISTER.CH%
	COM (CH_SB_BALANCE_READ)	SB_BALANCE.CH%
	COM (CH_SB_ACCOUNT_READ)	SB_ACCOUNT.CH%
	COM (CH_BM_RELATION_READ)	BM_RELATION.CH%
	COM (CH_BM_CONTROL_READ)	BM_CONTROL.CH%
	COM (CH_WP_CONTROL)		WP_CONTROL.CH%

	RECORD COMPONENT_RECORD
		STRING NUMBER = 14%
		REAL   QUANTITY
	END RECORD

	RECORD BOM_RECORD
		STRING	PRODUCT = 14%
		RFA	RFA_LEVEL
		REAL	QTY
	END RECORD

	RECORD ISSUE_RECORD
		STRING	PRODUCT
		REAL	QTY
		REAL	LABOR
		REAL	HOURS
	END RECORD

	DIM COMPONENT_RECORD	COMPONENT(5000%)
	DIM BOM_RECORD		BOM(400%)
	DIM ISSUE_RECORD	ISSUE(1000%)

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	.x Sort>Job Status Summary Report
	!	^*(01) Sort by (J,T,C)\*\*
	!	.lm +5
	!	.b
	!	The ^*Sort by\* field determines the order in which the report
	!	will print.
	!	.b
	!	Valid settings are:
	!	.Table 3,25
	!	.te
	!	^*J\* - Job Number
	!	.te
	!	^*T\* - Job Type
	!	.te
	!	^*C\* - Class
	!	.end table
	!	.b
	!	A setting is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02)From Item\*
	!	.lm +5
	!	.b
	!	The ^*From Item\* field
	!	enters the item with which be report is to begin printing.
	!	The value in this field must be in agreement with the
	!	setting in field (01) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the
	!	first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Job Status Summary Report
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.lm +5
	!	.b
	!	The ^*To Item\* field
	!	specifies the item with which the report will end.  The value in
	!	this field must be in agreement with the setting in
	!	field (01) Sort by.
	!	.b
	!	A blank field will cause the report to end with the last
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Job Status Summary Report
	!
	!--


	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.lm +5
	!	.b
	!	The ^*Wildcard\* field
	!	selects a designated item or group of items to be printed by
	!	entering a wildcard value.
	!	.b
	!	For further information about wildcard technique, refer
	!	to Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Job Status Summary Report
	!
	!--
	STATUS$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 132%)

	!++
	! Abstract:FLD05
	!	^*(05) Status Select\*
	!	.lm +5
	!	.b
	!	The ^*Status Select\* field
	!	selects the jobs with a given status code.
	!	.b
	!	Valid status codes are:
	!	.table 3,25
	!	.te
	!	A = Active Jobs
	!	.te
	!	I = Inactive Jobs
	!	.te
	!	C = Completed Jobs
	!	.end table
	!	A blank value in this field will cause all records in the
	!	register file to be printed regardless of status.
	!	.b
	!	Any single status code or any combination of two codes may
	!	be entered. For example:  AI = Active and Inactive,
	!	IC = Inactive and Completed
	!	.lm -5
	!
	! Index:
	!	.x Status>Select>Job Status Summary Report
	!
	!--

	VARFLAG$ = LEFT$(UTL_REPORTX::OPTDEF(5%), 1%)

	!++
	! Abstract:FLD06
	!	^*(06) Varience Flag\*
	!	.lm +5
	!	.b
	!	What flag to use when calculating variances
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	<blank> = Not Specified
	!	.te
	!	*P = Parts
	!	.te
	!	*E = Equipment
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Status>Select>Job Status Summary Report
	!
	!--

300	!
	! Open  Subaccount Register file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
	USE
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.OPN"
	USE
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.OPN"
	USE
		FILENAME$ = "SB_BALANCE"
		CONTINUE HelpError
	END WHEN

330	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_ACCOUNT.OPN"
	USE
		FILENAME$ = "SB_ACCOUNT"
		CONTINUE HelpError
	END WHEN

	SB_ACCOUNT_ARRAY% = 0%
	RESET #SB_ACCOUNT.CH%

335	WHEN ERROR IN
		GET #SB_ACCOUNT.CH%, REGARDLESS
		SB_ACCOUNT_ARRAY% = SB_ACCOUNT_ARRAY% + 1%
		SB_ACCOUNT_ARRAY(SB_ACCOUNT_ARRAY%) = SB_ACCOUNT
	USE
		CONTINUE 340 IF ERR = 11%
		FILENAME$ = "SB_ACCOUNT"
		CONTINUE HelpError
	END WHEN

340	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.OPN"
	USE
		FILENAME$ = "WP_REQLINE"
		CONTINUE HelpError
	END WHEN

350	WHEN ERROR IN
		%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.OPN"
	USE
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

360	WHEN ERROR IN
		%INCLUDE "SOURCE:[BM.OPEN]BM_CONTROL.OPN"
		GET #BM_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE #BM_CONTROL.CH%
	USE
		IF ERR = 138%	! File Locked
		THEN
			SLEEP 5%
			RETRY
		END IF

		BM_CONTROL::BURDENRATE = 0.0
		BM_CONTROL::PRODTYPE = ""
		BM_CONTROL::LABORRATE = 0.0
		BM_CONTROL::RMAT = ""
		BM_CONTROL::BURDENPERC = 0.0

		CALL ASSG_FREECHANNEL(BM_CONTROL.CH%)
		BM_CONTROL.CH% = 0%
		FILENAME$ = "BM_CONTROL"
		CONTINUE HelpError
	END WHEN

370	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_CONTROL.OPN"
		GET #WP_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE #WP_CONTROL.CH%
	USE
		IF ERR = 138%	! File Locked
		THEN
			SLEEP 5%
			RETRY
		END IF

		WP_CONTROL::ORDNUM = ""
		WP_CONTROL::PURGDATE = ""
		WP_CONTROL::STATUS_FLAG = ""
		WP_CONTROL::REQNUM = ""
		WP_CONTROL::INVMATPVAR = ""
		WP_CONTROL::INVMATUVAR = ""
		WP_CONTROL::INVLABRVAR = ""
		WP_CONTROL::INVLABEVAR = ""
		WP_CONTROL::INVBURVAR = ""
		WP_CONTROL::EQMATPVAR = ""
		WP_CONTROL::EQMATUVAR = ""
		WP_CONTROL::EQLABRVAR = ""
		WP_CONTROL::EQLABEVAR = ""
		WP_CONTROL::EQBURVAR = ""

		CALL ASSG_FREECHANNEL(WP_CONTROL.CH%)
		WP_CONTROL.CH% = 0%
		FILENAME$ = "WP_CONTROL"
		CONTINUE HelpError
	END WHEN

380	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.OPN"
		GET #SB_CONTROL.CH%, KEY #0% EQ "JC", REGARDLESS
		CLOSE #SB_CONTROL.CH%
	USE
		IF ERR = 138%	! File Locked
		THEN
			SLEEP 5%
			RETRY
		END IF

		SB_CONTROL::SYSTEM = ""
		SB_CONTROL::PERIOD = ""
		SB_CONTROL::CONTROLFLAG = ""
		SB_CONTROL::CDATE = ""
		SB_CONTROL::CTIME = ""
		SB_CONTROL::BATCH = ""
		SB_CONTROL::SUBJECT = ""
		SB_CONTROL::DEFNUMBER = ""

		CALL ASSG_FREECHANNEL(SB_CONTROL.CH%)
		SB_CONTROL.CH% = 0%
		FILENAME$ = "SB_CONTROL"
		CONTINUE HelpError
	END WHEN

	SYSTEM$ = SB_CONTROL::SYSTEM
	PERIOD$ = SB_CONTROL::PERIOD

 ReportTitle:
	!
	! Select which method to sort by
	!
	SELECT SORTBY$

	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = " REGISTER SUMMARY BY JOB TYPE"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = " REGISTER SUMMARY BY JOB CLASS"

	CASE "J"
		K_NUM% = 0%
		TITLE$(1%) = " REGISTER SUMMARY BY JOB NUMBER"

	END SELECT

	SELECT STATUS$

	CASE "A"
		TITLE$(2%) = "Active Jobs"

	CASE "I"
		TITLE$(2%) = "Inactive Jobs"

	CASE "C"
		TITLE$(2%) = "Closed Jobs"

	CASE "AI", "IA"
		TITLE$(2%) = "Active and Inactive Jobs"

	CASE "AC", "CA"
		TITLE$(2%) = "Active and Completed Jobs"

	CASE "IC", "CI"
		TITLE$(2%) = "Completed and Inactive Jobs"

	CASE ELSE
		TITLE$(2%) = "All Jobs Regardless of Status"

	END SELECT

	!
	! Title
	!
	TITLE$(3%) = "Work In Process System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "JobNumber  JobDescription               " + &
		"            Type Class   JobDate  Status"      + &
		" CloseDate  Location Operator   ReferenceNo"

	TITLE$(6%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	!
	! If from item is blank then reset Register file
	! else try to find the first record
	!
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			FIND #SB_SUBACCOUNT.CH%, &
				KEY #K_NUM% GE "J", &
				REGARDLESS
		ELSE
			FIND #SB_SUBACCOUNT.CH%, &
				KEY #K_NUM% GE "J" + FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		GET #SB_SUBACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

	GOTO ExitProgram IF JC_JOB::SUBJECT <> "J"

	!
	! Check current record if should be printed
	!
	SELECT SORTBY$
	CASE "C"
		GOTO ExitProgram &
			IF (JC_JOB::CLASS > TO_ITEM$) AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY &
			(EDIT$(JC_JOB::CLASS, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "J"
		GOTO ExitProgram &
			IF (JC_JOB::JOB > TO_ITEM$) AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_ARRAY(EDIT$(JC_JOB::JOB, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitProgram &
			IF (JC_JOB::TTYPE > TO_ITEM$) AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_ARRAY(EDIT$(JC_JOB::TTYPE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""
	END SELECT

	!
	! Check for Status type
	!
	IF STATUS$ <> ""
	THEN
		GOTO GetNextRec IF INSTR(1%, STATUS$, JC_JOB::SSTATUS) = 0%
	END IF

	!
	! Build and print the Header out now
	!
	TEXT$ = JC_JOB::JOB + " " + &
		JC_JOB::DESCR + " " + &
		JC_JOB::TTYPE + "   " + &
		JC_JOB::CLASS + "  " + &
		PRNT_DATE(JC_JOB::BDATE, 8%) + " " + &
		JC_JOB::SSTATUS + "      " + &
		PRNT_DATE(JC_JOB::EDATE, 8%) + " " + &
		JC_JOB::LOCATION + "     " + &
		JC_JOB::OPERATOR + " " + &
		JC_JOB::REFNO

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)


	!
	! Initialize standard and actual to zero
	!
	TEST_STDBURDEN = 0.0
	TEST_STDLABOR  = 0.0
	TEST_STDPARTS  = 0.0
	TEST_STDRAWMAT = 0.0

	TEST_ACTBURDEN = 0.0
	TEST_ACTLABOR  = 0.0
	TEST_ACTPARTS  = 0.0
	TEST_ACTRAWMAT = 0.0

	TESTLINE$ = SPACE$(LEN(WP_REGLINE_READ::LLINE))

 ReadRegLine:
	GOTO StartClose &
		IF WP_READ_REGLINE(JC_JOB::JOB, &
		TESTLINE$, "GT", WP_REGLINE_READ, QTY()) <> CMC$_NORMAL

	IF FUNC_ROUND(QTY(0%), 2%) <> 0.0
	THEN
 !		TEXT$ = "Rem buyoff qty " + &
 !			NUM1$(FUNC_ROUND(QTY(0%), 2%)) + " at job line " + &
 !			WP_REGLINE_READ::LLINE
 !
 !		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	TESTLINE$, LASTLINE$ = WP_REGLINE_READ::LLINE

	REQLINE$ = SPACE$(LEN(WP_REQREGISTER_READ::REQNUM + &
		WP_REQREGISTER_READ::REQLIN))

 ReadReqLine:
	GOTO ReadRegLine &
		IF LASTLINE$ <> TESTLINE$ OR &
		WP_READ_REQREGISTER(JC_JOB::JOB, LASTLINE$, REQLINE$, &
		"GT", WP_REQREGISTER_READ, QTY()) <> CMC$_NORMAL

	IF FUNC_ROUND(QTY(0%), 2%) <> 0.0
	THEN
 !		TEXT$ = "Rem issue qty " + &
 !			NUM1$(FUNC_ROUND(QTY(0%), 2%)) + " at req line " + &
 !			TRM$(WP_REQREGISTER_READ::REQNUM) + "," + &
 !			WP_REQREGISTER_READ::REQLIN
 !
 !		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	LASTLINE$ = WP_REQREGISTER_READ::LLINE
	REQLINE$  = WP_REQREGISTER_READ::REQNUM + WP_REQREGISTER_READ::REQLIN

	GOTO ReadReqLine

 StartClose:
	!*******************************************************************
	! Calculate ACTUAL totals, put them in TEST_CLOSELINE
	! (Reads from SB_BALANCE register.
	!*******************************************************************

17705	TOTAL_LABOR = 0.0
	TOTAL_BURDEN = 0.0
	TOTAL_MAT = 0.0
	TOTAL_RMAT = 0.0
	TOTAL_LABOR_HOURS = 0.0

	WHEN ERROR IN
		FIND #SB_BALANCE.CH%, &
			KEY #1% EQ PERIOD$ + SYSTEM$ + JC_JOB::JOB, &
			REGARDLESS
	USE
		CONTINUE 17800 IF ERR = 155%
		FILENAME$ = "SB_BALANCE"
		CONTINUE HelpError
	END WHEN

 NextBalance:
17707	WHEN ERROR IN
		GET #SB_BALANCE.CH%, REGARDLESS
	USE
		CONTINUE 17800 IF ERR = 11%
		FILENAME$ = "SB_BALANCE"
		CONTINUE HelpError
	END WHEN

	GOTO 17800 &
		IF (SB_BALANCE::PERIOD <> PERIOD$) OR &
		(SB_BALANCE::SYSTEM <> SYSTEM$) OR &
		(SB_BALANCE::SUBACCOUNT <> JC_JOB::JOB)

17710	FOR SB_LOOP% = 1% TO SB_ACCOUNT_ARRAY%

		SB_ACCOUNT = SB_ACCOUNT_ARRAY(SB_LOOP%)

17720		IF COMP_STRING(SB_BALANCE::ACCOUNT, SB_ACCOUNT::ACCOUNT)
		THEN
			SELECT SB_ACCOUNT::ACCTGROUP

			CASE "BURD", "ILAB"
				TOTAL_BURDEN = FUNC_ROUND(TOTAL_BURDEN + &
					SB_BALANCE::AMOUNT + &
					SB_BALANCE::BEG_AMOUNT, 2%)

			CASE "DLAB"
				TOTAL_LABOR = FUNC_ROUND(TOTAL_LABOR + &
					SB_BALANCE::AMOUNT + &
					SB_BALANCE::BEG_AMOUNT, 2%)

				TOTAL_LABOR_HOURS = FUNC_ROUND(TOTAL_LABOR_HOURS + &
					SB_BALANCE::HOURS + &
					SB_BALANCE::BEG_HOURS, 2%)

			CASE "PMAT"
				TOTAL_MAT = FUNC_ROUND(TOTAL_MAT + &
					SB_BALANCE::AMOUNT + &
					SB_BALANCE::BEG_AMOUNT, 2%)

			CASE "RMAT"
				TOTAL_RMAT = FUNC_ROUND(TOTAL_RMAT + &
					SB_BALANCE::AMOUNT + &
					SB_BALANCE::BEG_AMOUNT, 2%)

			END SELECT

 !			TEXT$ = JC_JOB::JOB + " " + &
 !				"A" + " " + &
 !				SB_ACCOUNT::ACCTGROUP + " " + &
 !				SB_BALANCE::ACCOUNT + " " + &
 !				FORMAT$(-FUNC_ROUND(SB_BALANCE::AMOUNT + &
 !				SB_BALANCE::BEG_AMOUNT, 2%), "###,###,###.##")
 !
 !			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

			GOTO NextBalance
		END IF

	NEXT SB_LOOP%

17800	!
	! Set up actuals in close journal
	!
	TEST_ACTBURDEN = TOTAL_BURDEN
	TEST_ACTLABOR  = TOTAL_LABOR
	TEST_ACTPARTS  = TOTAL_MAT
	TEST_ACTRAWMAT = TOTAL_RMAT

	ISSUE% = 0%

	!*******************************************************************
	! Calculate STANDARD totals
	!*******************************************************************

	!
	! Scan through the WP_REGLINE journal, finding out which
	! products have been ordered.
	!
18000	WHEN ERROR IN
		FIND #WP_REGLINE.CH%, &
			KEY #0% EQ JC_JOB::JOB, &
			REGARDLESS
	USE
		CONTINUE CalcVar IF ERR = 155% OR ERR = 9%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

 NextJobLine:
18003	WHEN ERROR IN
		GET #WP_REGLINE.CH%, REGARDLESS
	USE
		CONTINUE CalcVar IF ERR = 11%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

	GOTO CalcVar &
		IF WP_REGLINE::JOB <> JC_JOB::JOB

	!
	! Pick up the material from the "01" lines, and the
	! labor from the "02" lines.
	!
	GOTO NextJobLine &
		UNLESS (WP_REGLINE::REC_TYPE = "01" AND &
			WP_REGLINE::TTYPE <> "L") OR &
		(WP_REGLINE::REC_TYPE = "02" AND &
			WP_REGLINE::TTYPE = "L")

 !		IF WP_REGLINE::REC_TYPE <> "02"

 !	TEXT$ = "REGLINE " + &
 !		WP_REGLINE::JOB + " " + &
 !		WP_REGLINE::LLINE + " " + &
 !		WP_REGLINE::TTYPE + " " + &
 !		WP_REGLINE::ITEMCODE + " " + &
 !		WP_REGLINE::DESCR
 !
 !	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TOTAL_MAT = 0.0
	TOTAL_RMAT = 0.0
	TOTAL_LABOR = 0.0

	IF WP_REGLINE::TTYPE = "L"
	THEN
		TOTAL_LABOR = FUNC_ROUND(WP_REGLINE::COST * &
			WP_REGLINE::QTY, 2%)

		!
		! DONE WITH THE LINE
		!
		GOTO DisplayTotals
	END IF

	!
	! Create an initial component in case bill of materials
	! doesn't pull up anything.
	!
	COMPONENT% = 1%
	COMPONENT(COMPONENT%)::NUMBER	= WP_REGLINE::ITEMCODE
	COMPONENT(COMPONENT%)::QUANTITY = WP_REGLINE::QTY

	!
	! Check the BM_RELATION file to determine what
	! components are used in this product.
	!
18007	WHEN ERROR IN
		FIND #BM_RELATION.CH%, &
			KEY #0% EQ WP_REGLINE::ITEMCODE, &
			REGARDLESS
	USE
		CONTINUE 18033 IF ERR = 155%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

18010	WHEN ERROR IN
		GET #BM_RELATION.CH%, REGARDLESS
	USE
		CONTINUE 18033 IF ERR = 11%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	BOM(1%)::PRODUCT = BM_RELATION::PRODUCT

	COMPONENT% = 0%
	BOM(0%)::QTY = WP_REGLINE::QTY
	BOM% = 1%

	!
	! GoDownTree
	!
18015	BOM(BOM%)::PRODUCT	= BM_RELATION::PRODUCT
	BOM(BOM%)::QTY		= BOM(BOM% - 1%)::QTY * BM_RELATION::QUANTITY
	BOM(BOM%)::RFA_LEVEL	= GETRFA(BM_RELATION.CH%)
	SCRAP%			= BM_RELATION::SCRAP

18020	WHEN ERROR IN
		GET #BM_RELATION.CH%, &
			KEY #0% EQ BM_RELATION::COMPONENT, &
			REGARDLESS
	USE
		CONTINUE 18500 IF ERR = 155%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	GOTO CheckForLabor &
		IF SCRAP% = 0%

	!
	! try to figure out, if there is really a shrinkage
	!
	COMP_ISS_QTY = 0.0

18021	WHEN ERROR IN
		FIND #WP_REQREGISTER.CH%, &
			KEY #2% EQ BM_RELATION::COMPONENT + &
			JC_JOB::LOCATION + JC_JOB::JOB, &
			REGARDLESS
	USE
		CONTINUE CheckForLabor IF ERR = 155% OR ERR = 9%
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

 NextCompRec:
18022	WHEN ERROR IN
		GET #WP_REQREGISTER.CH%, REGARDLESS
	USE
		CONTINUE CheckForLabor IF ERR = 11%
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	IF (BM_RELATION::COMPONENT = WP_REQREGISTER::PRODUCT) AND &
		(JC_JOB::LOCATION = WP_REQREGISTER::LOCATION) AND &
		(JC_JOB::JOB = WP_REQREGISTER::JOB)
	THEN
		IF WP_REQREGISTER::RECTYP = "02"
		THEN
			COMP_ISS_QTY = COMP_ISS_QTY + WP_REQREGISTER::QTY
		END IF

		GOTO NextCompRec
	END IF

 CheckForLabor:
	!
	! test if the higher level had been issued
	!
	FOR L% = 1% TO ISSUE%

		IF BM_RELATION::PRODUCT = ISSUE(L%)::PRODUCT
		THEN
			IF ISSUE(L%)::QTY >= BOM(BOM%)::QTY
			THEN
				ISSUE(L%)::QTY = ISSUE(L%)::QTY - &
					BOM(BOM%)::QTY

				TOTAL_LABOR = TOTAL_LABOR - &
					FUNC_ROUND(BOM(BOM%)::QTY * &
					ISSUE(L%)::LABOR, 2%)

				!
				! add product to the array
				!
				ISSUE_QTY = BOM(BOM%)::QTY
				GOSUB 18550
				GOTO 18030
			ELSE
				IF ISSUE(L%)::QTY > 0.0
				THEN
					TOTAL_LABOR = TOTAL_LABOR - &
						ISSUE(L%)::QTY * &
						ISSUE(L%)::LABOR

					BOM(BOM%)::QTY = &
						BOM(BOM%)::QTY - &
						ISSUE(L%)::QTY

					!
					! add product to the array
					!
					ISSUE_QTY = ISSUE(L%)::QTY
					GOSUB 18550
				END IF

				GOTO ContDown
			END IF
		END IF

	NEXT L%

	!
	! try to figure out, how much had been issued for a product
	!
18023	WHEN ERROR IN
		FIND #WP_REQREGISTER.CH%, &
			KEY #2% EQ BM_RELATION::PRODUCT + &
			JC_JOB::LOCATION + JC_JOB::JOB, &
			REGARDLESS
	USE
		CONTINUE ContDown IF ERR = 155% OR ERR = 9%
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	ISSUE% = ISSUE% + 1%

	ISSUE(ISSUE%)::PRODUCT	= BM_RELATION::PRODUCT
	ISSUE(ISSUE%)::QTY	= 0.0
	ISSUE(ISSUE%)::LABOR	= 0.0
	ISSUE(ISSUE%)::HOURS	= 0.0

 NextProdRec:
18024	WHEN ERROR IN
		GET #WP_REQREGISTER.CH%, REGARDLESS
	USE
		CONTINUE 18026 IF ERR = 11%
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	IF (BM_RELATION::PRODUCT = WP_REQREGISTER::PRODUCT) AND &
		(JC_JOB::LOCATION = WP_REQREGISTER::LOCATION) AND &
		(JC_JOB::JOB = WP_REQREGISTER::JOB)
	THEN
		IF WP_REQREGISTER::RECTYP = "02"
		THEN
			ISSUE(ISSUE%)::QTY = ISSUE(ISSUE%)::QTY + &
				WP_REQREGISTER::QTY
		END IF

		GOTO NextProdRec
	END IF

	!
	! Force shrinkage
	!
18026	IF (SCRAP% <> 0%) AND (COMP_ISS_QTY = 0.0) AND &
		(ISSUE(ISSUE%)::QTY < BOM(BOM%)::QTY)
	THEN
		ISSUE(ISSUE%)::QTY = BOM(BOM%)::QTY
	END IF

	OPERATION$ = "        "

	!
	! try to figure out how much labor is needed for a product
	!
 ReadProdOper:
	IF BM_READ_PRODOPER(BM_RELATION::PRODUCT, &
		OPERATION$, "GT", WP_REGLINE::COMP_DATE, &
		BM_PRODOPER_READ) = CMC$_NORMAL
	THEN
		OPERATION$ = BM_PRODOPER_READ::OPERATION

		IF PR_READ_OPERATION(BM_PRODOPER_READ::OPERATION, &
			"", PR_OPER_READ) = CMC$_NORMAL
		THEN
			HOUR_RATE = PR_OPER_READ::HOUR_RATE
		ELSE
			HOUR_RATE = 0.0
		END IF

		!
		! dollar labor per unit
		!
		ISSUE(ISSUE%)::LABOR = FUNC_ROUND(ISSUE(ISSUE%)::LABOR + &
			HOUR_RATE * BM_PRODOPER_READ::HOURS, 2%)

		!
		! hours per unit
		!
		ISSUE(ISSUE%)::HOURS = ISSUE(ISSUE%)::HOURS + &
			BM_PRODOPER_READ::HOURS

		GOTO ReadProdOper
	END IF

	GOTO CheckForLabor

	!*******************************************************************
	! Go down another level
	!*******************************************************************
 ContDown:
	BOM% = BOM% + 1%
	GOTO 18015

	!*******************************************************************
	! Go Up One Level
	!*******************************************************************
18027	GOTO 18033 &
		IF BOM% = 1%

	BOM% = BOM% - 1%

18030	WHEN ERROR IN
		GET #BM_RELATION.CH%, RFA BOM(BOM%)::RFA_LEVEL, REGARDLESS

		GET #BM_RELATION.CH%, REGARDLESS
	USE
		CONTINUE 18027 IF ERR = 155% OR ERR = 11%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	IF BM_RELATION::PRODUCT <> BOM(BOM%)::PRODUCT
	THEN
		GOTO 18027
	ELSE
		GOTO 18015
	END IF

	!*******************************************************************
	!
	!*******************************************************************

18033	FOR I% = 1% TO COMPONENT%

		V% = PD_EXAM_PRODUCT(COMPONENT(I%)::NUMBER, PD_PRODUCT_EXAM)

		PD_PRODUCT_EXAM::PRODUCT_FACTOR = 1.0 &
			IF PD_PRODUCT_EXAM::PRODUCT_FACTOR = 0.0

		COST = PC_READ_COST(COMPONENT(I%)::NUMBER, &
			JC_JOB::LOCATION, WP_REGLINE::COMP_DATE, "")

		COST = COST / PD_PRODUCT_EXAM::PRODUCT_FACTOR

		IF COMP_STRING(PD_PRODUCT_EXAM::PROD_TYPE, BM_CONTROL::RMAT)
		THEN
			TOTAL_RMAT = TOTAL_RMAT + &
				FUNC_ROUND(COMPONENT(I%)::QUANTITY * COST, 2%)
		ELSE
			TOTAL_MAT = TOTAL_MAT + &
				FUNC_ROUND(COMPONENT(I%)::QUANTITY * COST, 2%)
		END IF

 !		TEXT$ = COMPONENT(I%)::NUMBER + " " + &
 !			FORMAT$(COMPONENT(I%)::QUANTITY, "###,###.#### ")
 !		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	NEXT I%

	!
	! total labor for parent
	!
	OPERATION$ = "        "
	PR_OPER_READ::PIECE_RATE = 0.0

 ReadOperation:
	IF BM_READ_PRODOPER(WP_REGLINE::ITEMCODE, &
		OPERATION$, "GT", WP_REGLINE::COMP_DATE, &
		BM_PRODOPER_READ) = CMC$_NORMAL
	THEN
		OPERATION$ = BM_PRODOPER_READ::OPERATION

		IF PR_READ_OPERATION(BM_PRODOPER_READ::OPERATION, &
			"", PR_OPER_READ) = CMC$_NORMAL
		THEN
			HOUR_RATE = PR_OPER_READ::HOUR_RATE
		ELSE
			HOUR_RATE = 0.0
		END IF

		TOTAL_LABOR = TOTAL_LABOR + &
			FUNC_ROUND(HOUR_RATE * BM_PRODOPER_READ::HOURS, 2%) * &
			WP_REGLINE::QTY

		GOTO ReadOperation
	END IF

	GOTO DisplayTotals

18500	!*******************************************************************
	! Array of terminals
	! Try to add this Bill of Material Item to the component list,
	! if it has the correct product type.
	!*******************************************************************

	FOR I% = 1% TO COMPONENT%

		IF BM_RELATION::COMPONENT = COMPONENT(I%)::NUMBER
		THEN
			COMPONENT(I%)::QUANTITY = COMPONENT(I%)::QUANTITY + &
				FUNC_ROUND(BOM(BOM%)::QTY, 3%)

			GOTO EndTerm
		END IF

	NEXT I%

	V% = PD_EXAM_PRODUCT(BM_RELATION::COMPONENT, PD_PRODUCT_EXAM)

18510	IF COMP_STRING(PD_PRODUCT_EXAM::PROD_TYPE, BM_CONTROL::PRODTYPE)
	THEN
		COMPONENT% = COMPONENT% + 1%

		COMPONENT(COMPONENT%)::NUMBER = BM_RELATION::COMPONENT
		COMPONENT(COMPONENT%)::QUANTITY = &
			FUNC_ROUND(BOM(BOM%)::QTY, 3%)
	END IF

 EndTerm:
	GOTO 18030

18550	!*******************************************************************
	! Array of terminals from higher level
	!*******************************************************************
	FOR I% = 1% TO COMPONENT%

		IF BM_RELATION::PRODUCT = COMPONENT(I%)::NUMBER
		THEN
			COMPONENT(I%)::QUANTITY = COMPONENT(I%)::QUANTITY + &
				FUNC_ROUND(ISSUE_QTY, 3%)

			GOTO Ret18550
		END IF

	NEXT I%

	COMPONENT% = COMPONENT% + 1%

	COMPONENT(COMPONENT%)::NUMBER = BM_RELATION::PRODUCT

	COMPONENT(COMPONENT%)::QUANTITY = FUNC_ROUND(ISSUE_QTY, 3%)

 Ret18550:
	RETURN

	!*******************************************************************
	! Done with one issue.
	!*******************************************************************
 DisplayTotals:
18600	TOTAL_COST = FUNC_ROUND(PC_READ_COST(WP_REGLINE::ITEMCODE, &
		JC_JOB::LOCATION, WP_REGLINE::COMP_DATE, "") * &
		WP_REGLINE::QTY, 2%)

 !
 ! Temporary stuff to see how things calculate
 !
 ! xxxyyy$ = TRM$(WP_REGLINE::JOB) + " " + TRM$(WP_REGLINE::LLINE) + " " + &
 !	NUM1$(TOTAL_BURDEN) + " " + NUM1$(TOTAL_LABOR) + " " + &
 !	NUM1$(TOTAL_MAT) + " " + NUM1$(TOTAL_RMAT) + " " + &
 !	NUM1$(TOTAL_COST)
 ! CALL ENTR_3MESSAGE(SCOPE, XXXYYY$, 0%)

	!
	! Apply a dollar rule. No labor if it comes to less than one
	! dollar.
	!
	IF ABS(TOTAL_LABOR) < 1.0
	THEN
		TOTAL_LABOR = 0.0
		TOTAL_BURDEN = 0.0
		TOTAL_MAT = FUNC_ROUND(TOTAL_COST - TOTAL_RMAT, 2%)
	ELSE
		TOTAL_BURDEN = FUNC_ROUND(TOTAL_COST - TOTAL_LABOR - &
			TOTAL_MAT - TOTAL_RMAT, 2%)
	END IF

 !
 ! Temporary stuff to see how things calculate
 !
 ! xxxyyy$ = "After:" + TRM$(WP_REGLINE::JOB) + " " + TRM$(WP_REGLINE::LLINE) + " " + &
 !	NUM1$(TOTAL_BURDEN) + " " + NUM1$(TOTAL_LABOR) + " " + &
 !	NUM1$(TOTAL_MAT) + " " + NUM1$(TOTAL_RMAT) + " " + &
 !	NUM1$(TOTAL_COST)
 ! CALL ENTR_3MESSAGE(SCOPE, XXXYYY$, 0%)

	!
	! Set up standards in TEST_CLOSEJOUR
	!
	TEST_STDBURDEN = FUNC_ROUND(TEST_STDBURDEN + &
		TOTAL_BURDEN, 2%)
	TEST_STDLABOR  = FUNC_ROUND(TEST_STDLABOR  + &
		TOTAL_LABOR, 2%)
	TEST_STDPARTS  = FUNC_ROUND(TEST_STDPARTS  + &
		TOTAL_MAT, 2%)
	TEST_STDRAWMAT = FUNC_ROUND(TEST_STDRAWMAT + &
		TOTAL_RMAT, 2%)

	STD_TOTAL = FUNC_ROUND(TEST_STDBURDEN + &
		TEST_STDLABOR + &
		TEST_STDPARTS + TEST_STDRAWMAT, 2%)

	GOTO NextJobLine

	!*******************************************************************
	! Calculate variance records
	!*******************************************************************
 CalcVar:
	GOTO Ef &
		IF VARFLAG$ = ""

	TOTAL_VAR = FUNC_ROUND( &
		TEST_ACTBURDEN + &
		TEST_ACTPARTS + &
		TEST_ACTRAWMAT + &
		TEST_ACTLABOR - &
 &
		TEST_STDBURDEN - &
		TEST_STDPARTS - &
		TEST_STDRAWMAT - &
		TEST_STDLABOR, 2%)

	!
	! Generate BURD variance record
	!
	BURDEN_VAR = FUNC_ROUND(TEST_ACTBURDEN - &
		TEST_STDBURDEN, 2%)

	IF VARFLAG$ = "E"
	THEN
		VACCT$ = WP_CONTROL::EQBURVAR
	ELSE
		VACCT$ = WP_CONTROL::INVBURVAR
	END IF

 !	TEXT$ = JC_JOB::JOB + " " + &
 !		"A" + " " + &
 !		"BURD" + " " + &
 !		VACCT$ + " " + &
 !		FORMAT$(BURDEN_VAR, "###,###,###.##")
 !
 !	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%) &
 !		IF BURDEN_VAR <> 0.0

	!
	! Generate MAT variance record
	!
	MAT_VAR = FUNC_ROUND(TEST_ACTPARTS + &
		TEST_ACTRAWMAT - &
		TEST_STDPARTS - TEST_STDRAWMAT, 2%)

	IF VARFLAG$ = "E"
	THEN
		VACCT$ = WP_CONTROL::EQMATUVAR
	ELSE
		VACCT$ = WP_CONTROL::INVMATUVAR
	END IF

 !	TEXT$ = JC_JOB::JOB + " " + &
 !		"A" + " " + &
 !		"MAT " + " " + &
 !		VACCT$ + " " + &
 !		FORMAT$(MAT_VAR, "###,###,###.##")
 !
 !	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%) &
 !		IF MAT_VAR <> 0.0


	LABE_VAR = 0.0

	IF (BM_CONTROL::BURDENRATE <> 0.0) AND (PR_OPER_READ::PIECE_RATE = 0.0)
	THEN
		LABE_VAR = FUNC_ROUND( &
			(TEST_ACTBURDEN / BM_CONTROL::BURDENRATE) * &
			BM_CONTROL::LABORRATE - TEST_STDLABOR, 2%)
	ELSE
		IF (BM_CONTROL::BURDENPERC = 0.0)
		THEN
			LABE_VAR = FUNC_ROUND( &
				TOTAL_VAR - MAT_VAR - BURDEN_VAR, 2%) &
				IF PR_OPER_READ::PIECE_RATE <> 0.0
		ELSE
			LABE_VAR = FUNC_ROUND( &
				(TOTAL_LABOR_HOURS * BM_CONTROL::LABORRATE) - &
				TEST_STDLABOR, 2%)
		END IF
	END IF

	!
	! Generate LABE variance record
	!
	IF VARFLAG$ = "E"
	THEN
		VACCT$ = WP_CONTROL::EQLABEVAR
	ELSE
		VACCT$ = WP_CONTROL::INVLABEVAR
	END IF

 !	TEXT$ = JC_JOB::JOB + " " + &
 !		"A" + " " + &
 !		"LABE" + " " + &
 !		VACCT$ + " " + &
 !		FORMAT$(LABE_VAR, "###,###,###.##")
 !
 !	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%) &
 !		IF LABE_VAR <> 0.0

	!
	! Generate LABR variance record
	!
	LABR_VAR = FUNC_ROUND(TOTAL_VAR - MAT_VAR - BURDEN_VAR - LABE_VAR, 2%)

	IF VARFLAG$ = "E"
	THEN
		VACCT$ = WP_CONTROL::EQLABRVAR
	ELSE
		VACCT$ = WP_CONTROL::INVLABRVAR
	END IF

 !	TEXT$ = JC_JOB::JOB + " " + &
 !		"A" + " " + &
 !		"LABR" + " " + &
 !		VACCT$ + " " + &
 !		FORMAT$(LABR_VAR, "###,###,###.##")
 !
 !	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%) &
 !		IF LABR_VAR <> 0.0



 Ef:
	!
	! Print Out the Statistics
	!
	TEXT$ = "Cost                           Burden               " + &
		"Labor               Parts              RawMat             " + &
		"  Total"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

	STD_TOTAL = TEST_STDBURDEN + TEST_STDLABOR + TEST_STDPARTS + &
		TEST_STDRAWMAT

	TEXT$ = "Standard         " + SPACE$(10%) + &
		FORMAT$(TEST_STDBURDEN, "###,###.##") + SPACE$(10%) + &
		FORMAT$(TEST_STDLABOR, "###,###.##") + SPACE$(10%) + &
		FORMAT$(TEST_STDPARTS, "###,###.##") + SPACE$(10%) + &
		FORMAT$(TEST_STDRAWMAT, "###,###.##") + SPACE$(10%) + &
		FORMAT$(STD_TOTAL, "###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	ACT_TOTAL = TEST_ACTBURDEN + TEST_ACTLABOR + &
		TEST_ACTPARTS + TEST_ACTRAWMAT

	TEXT$ = "Actual           " + SPACE$(10%) + &
		FORMAT$(TEST_ACTBURDEN, "###,###.##") + SPACE$(10%) + &
		FORMAT$(TEST_ACTLABOR, "###,###.##") + SPACE$(10%) + &
		FORMAT$(TEST_ACTPARTS, "###,###.##") + SPACE$(10%) + &
		FORMAT$(TEST_ACTRAWMAT, "###,###.##") + SPACE$(10%) + &
		FORMAT$(ACT_TOTAL, "###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	VAR_BURDEN = TEST_STDBURDEN - TEST_ACTBURDEN
	VAR_LABOR  = TEST_STDLABOR  - TEST_ACTLABOR
	VAR_PARTS  = TEST_STDPARTS  - TEST_ACTPARTS
	VAR_RAWMAT = TEST_STDRAWMAT - test_ACTRAWMAT

	VAR_TOTAL  = VAR_BURDEN + VAR_LABOR + VAR_PARTS + VAR_RAWMAT

	TEXT$ = "Variance             " + &
		"Burd  " + FORMAT$(VAR_BURDEN, "###,###.##") + "  " + &
		"LabEff  " + FORMAT$(VAR_LABOR, "###,###.##") + "  " + &
		"Parts   " + FORMAT$(VAR_PARTS, "###,###.##") + "  " + &
		"RawMat  " + FORMAT$(VAR_RAWMAT, "###,###.##") + "   " + &
		"Total  " + FORMAT$(VAR_TOTAL, "###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(39%) + "LabRat  " + FORMAT$(LABRTOT, "###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	GOTO ExitProgram &
		IF UTL_REPORTX::STAT


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
