1	%TITLE "Job Status Summary Report"
	%SBTTL "WP_RPRT_JOBPROGRESS3"
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
	!	$ BAS WP_SOURCE:WP_RPRT_JOBPROGRESS3/LINE
	!	$ LINK/EXE=WP_EXE: WP_RPRT_JOBPROGRESS3, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_RPRT_JOBPROGRESS3.OBJ;*
	!
	! Author:
	!
	!	02/17/2003 - Kevin Handy
	!
	! Modification History:
	!
	!	04/24/2003 - Kevin Handy
	!		Added product description to debug lines.
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

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[BM.OPEN]BM_CONTROL.HB"
	MAP (BM_CONTROL)	BM_CONTROL_CDD		BM_CONTROL

	%INCLUDE "SOURCE:[PR.OPEN]PR_OPER.HB"
	DECLARE			PR_OPER_CDD		PR_OPER_READ

	%INCLUDE "SOURCE:[WP.OPEN]WP_CONTROL.HB"
	MAP (WP_CONTROL)	WP_CONTROL_CDD		WP_CONTROL

	%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.HB"
	MAP (SB_CONTROL)	SB_CONTROL_CDD		SB_CONTROL

	%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.HB"
	MAP (BM_PRODOPER)	BM_PRODOPER_CDD		BM_PRODOPER

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP (PR_TRN_PAY)	PR_TRN_PAY_CDD	PR_TRN_PAY
	MAP (PR_HIS_PAY)	PR_TRN_PAY_CDD	PR_HIS_PAY

	!
	! Declare external functions
	!
	EXTERNAL LONG   FUNCTION WP_READ_REGLINE
	EXTERNAL LONG	FUNCTION WP_READ_REQREGISTER
	EXTERNAL REAL	FUNCTION PC_READ_COST
	EXTERNAL LONG	FUNCTION PR_READ_OPERATION

	COM (CH_WP_REGLINE_READ)	WP_REGLINE.CH%
	COM (CH_SB_BALANCE_READ)	SB_BALANCE.CH%
	COM (CH_SB_ACCOUNT_READ)	SB_ACCOUNT.CH%
	COM (CH_BM_RELATION_READ)	BM_RELATION.CH%
	COM (CH_BM_PRODOPER_READ)	BM_PRODOPER.CH%
	COM (CH_BM_CONTROL_READ)	BM_CONTROL.CH%
	COM (CH_WP_CONTROL)		WP_CONTROL.CH%
	COM (CH_PD_PRODUCT)		PD_PRODUCT.CH%

	DECLARE INTEGER CONSTANT FILE_MAX = 2000%

	!
	! Dimension
	!
	DIM DATA_FILE$(FILE_MAX)

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

	SHOW_DETAIL$ = LEFT$(UTL_REPORTX::OPTDEF(5%), 1%)

	!++
	! Abstract:FLD06
	!	^*(06) Show Detail\*
	!	.lm +5
	!	.b
	!	Should the report display list of unissued parts?
	!	.lm -5
	!
	! Index:
	!	.x Status>Select>Job Status Summary Report
	!
	!--

	DETAIL$ = LEFT(UTL_REPORTX::OPTDEF(9%), 1%)

	!++
	! Abstract:FLD10
	!	^*(10) Detail Flag\*
	!	.lm +5
	!	.b
	!	How much detail should be shown
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	1 or less = Minimum detail
	!	.te
	!	5 = Before and after looking at issues
	!	.te
	!	9 Gory debugging detail
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

355	WHEN ERROR IN
		%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.OPN"
	USE
		FILENAME$ = "BM_PRODOPER"
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

390	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
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

	TITLE$(6%) = "ReqLine   Option       Description              " + &
		"Burden      Labor       Hours      Purch        Mfg      Total"
	TITLE$(7%) = "."

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
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TESTLINE$ = SPACE$(LEN(WP_REGLINE::LLINE))

	GRAND_TOTAL_HOURS = 0.0
	GRAND_TOTAL_HOURSDOL = 0.0
	GRAND_TOTAL_PARTS = 0.0
	GRAND_TOTAL_RAWMAT = 0.0
	GRAND_TOTAL_COST = 0.0
	GRAND_TOTAL_BURDEN = 0.0

	ISSUE_HOURSDOL = 0.0
	ISSUE_HOURS = 0.0
	ISSUE_RAWMAT = 0.0
	ISSUE_PARTS = 0.0
	ISSUE_COST = 0.0
	ISSUE_BURDEN = 0.0

	COMPLETED_TOTAL_COST = 0.0

17100	!
	! Scan through regline
	!
 ReadRegLine:
	GOTO 17900 &
		IF WP_READ_REGLINE(JC_JOB::JOB, &
		TESTLINE$, "GT", WP_REGLINE_READ, QTY()) <> CMC$_NORMAL

 !	COST = PC_READ_COST(WP_REGLINE_READ::ITEMCODE, JC_JOB::LOCATION, &
 !		DATE_TODAY, "")

	TESTLINE$ = WP_REGLINE_READ::LLINE

	TOTAL_HOURS = 0.0
	TOTAL_HOURSDOL = 0.0
	TOTAL_PARTS = 0.0
	TOTAL_RAWMAT = 0.0

	!
	! Starting top level labor
	!

17200	!
	! Scan LABOR at current level
	!
	WHEN ERROR IN
		FIND #BM_PRODOPER.CH%, &
			KEY #0% EQ WP_REGLINE_READ::ITEMCODE, &
			REGARDLESS
	USE
		CONTINUE 17300
	END WHEN

17210	WHEN ERROR IN
		GET #BM_PRODOPER.CH%, REGARDLESS
	USE
		CONTINUE 17300
	END WHEN

	GOTO 17300 IF BM_PRODOPER::PRODUCT <> WP_REGLINE_READ::ITEMCODE

17220	!
	! Apply labor
	!
	TOTAL_HOURS = TOTAL_HOURS + BM_PRODOPER::HOURS * QTY(1%)

	IF PR_READ_OPERATION(BM_PRODOPER::OPERATION, &
		"", PR_OPER_READ) = CMC$_NORMAL
	THEN
		HOUR_RATE = PR_OPER_READ::HOUR_RATE
	ELSE
		HOUR_RATE = 0.0
	END IF

	TOTAL_HOURSDOL = TOTAL_HOURSDOL + &
		BM_PRODOPER::HOURS * QTY(1%) * HOUR_RATE

17290	!
	! End of labor
	!
	GOTO 17210

17300	!
	IF DETAIL$ = "Y"
	THEN
		BURDEN = QTY(1%) * WP_REGLINE_READ::COST - &
			TOTAL_PARTS - TOTAL_RAWMAT - TOTAL_HOURSDOL

		TEXT$ = "   " + &
			WP_REGLINE_READ::LLINE + " " + &
			" -PRE-        " + " " + &
			"                    " + " " + &
			FORMAT$(BURDEN, "#######.## ") + &
			FORMAT$(TOTAL_HOURSDOL, "#######.## ") + &
			FORMAT$(TOTAL_HOURS, "########.## ") + &
			FORMAT$(TOTAL_RAWMAT, "#######.## ") + &
			FORMAT$(TOTAL_PARTS, "#######.## ") + &
			FORMAT$(QTY(1%) * WP_REGLINE_READ::COST, "#######.##  ")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF


17400	!
	! Now, go through the requisitions
	!
	WHEN ERROR IN
		FIND #WP_REQREGISTER.CH%, &
			KEY #0% EQ WP_REGLINE_READ::JOB + &
			WP_REGLINE_READ::LLINE, &
			REGARDLESS
	USE
		CONTINUE 17600 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

17410	WHEN ERROR IN
		GET #WP_REQREGISTER.CH%, REGARDLESS
	USE
		CONTINUE 17600 IF ERR = 11%
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	GOTO 17600 &
		IF WP_REQREGISTER::JOB <> WP_REGLINE_READ::JOB OR &
		WP_REQREGISTER::LLINE <> WP_REGLINE_READ::LLINE

	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, &
			KEY #0% EQ WP_REQREGISTER::PRODUCT, &
			REGARDLESS
	USE
		!
		! AAaack, undefined product.
		! Assume lowest level
		!
		PD_PRODUCT::PROD_TYPE = ""
		PD_PRODUCT::DESCRIPTION = ""
	END WHEN

	SELECT WP_REQREGISTER::RECTYP
	CASE "01"

		THIS_HOURS = 0.0
		THIS_HOURSDOL = 0.0
		THIS_PARTS = 0.0
		THIS_RAWMAT = 0.0

		IF COMP_STRING(PD_PRODUCT::PROD_TYPE, &
			BM_CONTROL::PRODTYPE) <> 0%
		THEN
			TOTAL_RAWMAT = TOTAL_RAWMAT + &
				WP_REQREGISTER::AMT * WP_REQREGISTER::QTY
			THIS_RAWMAT = THIS_RAWMAT + &
				WP_REQREGISTER::AMT * WP_REQREGISTER::QTY
		ELSE
			TOTAL_PARTS = TOTAL_PARTS + &
				WP_REQREGISTER::AMT * WP_REQREGISTER::QTY
			THIS_PARTS = THIS_PARTS + &
				WP_REQREGISTER::AMT * WP_REQREGISTER::QTY
		END IF

		IF INSTR(1%, "BW,MP,DR", PD_PRODUCT::PROD_TYPE) <> 0%
		THEN
			!
			! Determine part costs here
			!
			GOSUB DropLabor
		END IF

		IF DETAIL$ = "Y"
		THEN
			TEXT$ = "   " + &
				"----" + " " + &
				WP_REQREGISTER::PRODUCT + " " + &
				LEFT(PD_PRODUCT::DESCRIPTION, 20%) + " " + &
				FORMAT$(0.0, "#######.## ") + &
				FORMAT$(THIS_HOURSDOL, "#######.## ") + &
				FORMAT$(THIS_HOURS, "########.## ") + &
				FORMAT$(THIS_RAWMAT, "#######.## ") + &
				FORMAT$(THIS_PARTS, "#######.## ")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	CASE "02"

		IF COMP_STRING(PD_PRODUCT::PROD_TYPE, &
			BM_CONTROL::PRODTYPE) <> 0%
		THEN
			ISSUE_RAWMAT = ISSUE_RAWMAT + WP_REQREGISTER::AMT
		ELSE
			ISSUE_PARTS = ISSUE_PARTS + WP_REQREGISTER::AMT
		END IF

		ISSUE_COST = ISSUE_COST + WP_REQREGISTER::AMT

	END SELECT

	!
	! End of WP_REQREGISTER
	!
	GOTO 17410

17600	!

	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, &
			KEY #0% EQ WP_REGLINE_READ::ITEMCODE, &
			REGARDLESS
	USE
		!
		! AAaack, undefined product.
		! Assume lowest level
		!
		PD_PRODUCT::DESCRIPTION = ""
	END WHEN

	BURDEN = QTY(1%) * WP_REGLINE_READ::COST - &
		TOTAL_PARTS - TOTAL_RAWMAT - TOTAL_HOURSDOL

	TEXT$ = "   " + &
		WP_REGLINE_READ::LLINE + " " + &
		WP_REGLINE_READ::ITEMCODE + " " + &
		LEFT(PD_PRODUCT::DESCRIPTION, 20%) + " " + &
		FORMAT$(BURDEN, "#######.## ") + &
		FORMAT$(TOTAL_HOURSDOL, "#######.## ") + &
		FORMAT$(TOTAL_HOURS, "########.## ") + &
		FORMAT$(TOTAL_RAWMAT, "#######.## ") + &
		FORMAT$(TOTAL_PARTS, "#######.## ") + &
		FORMAT$(QTY(1%) * WP_REGLINE_READ::COST, "#######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GRAND_TOTAL_HOURS = GRAND_TOTAL_HOURS + TOTAL_HOURS
	GRAND_TOTAL_HOURSDOL = GRAND_TOTAL_HOURSDOL + &
		TOTAL_HOURSDOL
	GRAND_TOTAL_PARTS = GRAND_TOTAL_PARTS + TOTAL_PARTS
	GRAND_TOTAL_RAWMAT = GRAND_TOTAL_RAWMAT + TOTAL_RAWMAT
	GRAND_TOTAL_COST = GRAND_TOTAL_COST + &
		QTY(1%) * WP_REGLINE_READ::COST
	GRAND_TOTAL_BURDEN = GRAND_TOTAL_BURDEN + BURDEN

17800	!
	! Finish up this regline
	!
	GOTO ReadRegLine

17900	!
	! Finish up this job
	!
	TEXT$ = "   " + &
		"    " + " " + &
		"STANDARD TO DATE" + &
		"                   " + " " + &
		FORMAT$(GRAND_TOTAL_BURDEN, "#######.## ") + &
		FORMAT$(GRAND_TOTAL_HOURSDOL, "#######.## ") + &
		FORMAT$(GRAND_TOTAL_HOURS, "########.## ") + &
		FORMAT$(GRAND_TOTAL_RAWMAT, "#######.## ") + &
		FORMAT$(GRAND_TOTAL_PARTS, "#######.## ") + &
		FORMAT$(GRAND_TOTAL_COST, "#######.##  ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Collect labor
	!
	GOSUB 18400

	ISSUE_BURDEN = FUNC_ROUND(ISSUE_HOURS * BM_CONTROL::BURDENRATE, 2%)
	ISSUE_COST = ISSUE_BURDEN + ISSUE_HOURSDOL + ISSUE_RAWMAT + &
		ISSUE_PARTS

	TEXT$ = "   " + &
		"    " + " " + &
		"COSTS TO DATE " + " " + &
		"                    " + " " + &
		FORMAT$(ISSUE_BURDEN, "#######.## ") + &
		FORMAT$(ISSUE_HOURSDOL, "#######.## ") + &
		FORMAT$(ISSUE_HOURS, "########.## ") + &
		FORMAT$(ISSUE_RAWMAT, "#######.## ") + &
		FORMAT$(ISSUE_PARTS, "#######.## ") + &
		FORMAT$(ISSUE_COST, "#######.##  ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Display pay information colleted at 18400
	!
	TEXT$ = SPACE$(8%) + &
		"Payroll Labor Between " + &
		PRNT_DATE(FROM_BATCH_NO$, 8%) + &
		" and " + &
		PRNT_DATE(TO_BATCH_NO$, 8%) + &
		"    REG HRS " + &
		FORMAT$(REG_HR, "###,###.## ") + &
		"    OVT HRS " + &
		FORMAT$(OVT_HR, "###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -3%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)


18000	!****************************************************************
	! Requition lines left over
	!
	GOTO 18090 IF SHOW_DETAIL$ <> "Y"

	NEXT_LINE$ = "   "
	HEADERFLAG% = 0%
	TOTAL_MAN = 0.0
	TOTAL_PUR = 0.0

18010	GOTO NewOrder IF WP_READ_REGLINE(JC_JOB::JOB, NEXT_LINE$, "GT", &
		WP_REGLINE_READ, QTY()) <> CMC$_NORMAL

	NEXT_LINE$ = WP_REGLINE_READ::LLINE

	REQNUMBER$ = "          "
	REQLINE$ = "    "

18050	GOTO 18010 IF WP_READ_REQREGISTER(JC_JOB::JOB, &
		WP_REGLINE_READ::LLINE, &
		REQNUMBER$ + REQLINE$, "GT", &
		WP_REQREGISTER_READ, QTY()) <> CMC$_NORMAL

	GOTO 18010 &
		IF WP_REQREGISTER_READ::LLINE <> WP_REGLINE_READ::LLINE

	REQNUMBER$ = WP_REQREGISTER_READ::REQNUM
	REQLINE$   = WP_REQREGISTER_READ::REQLIN

	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, &
			KEY #0% EQ WP_REQREGISTER_READ::PRODUCT, &
			REGARDLESS
	USE
		!
		! AAaack, undefined product.
		! Assume lowest level
		!
		PD_PRODUCT::DESCRIPTION = ""
		PD_PRODUCT::PROD_TYPE = ""
	END WHEN


	COST = PC_READ_COST(WP_REQREGISTER_READ::PRODUCT, &
		WP_REQREGISTER_READ::LOCATION, DATE_TODAY, "")
	EXTENSION = FUNC_ROUND(QTY(0%) * COST, 2%)

	IF INSTR(1%, "BW,MP,DR", PD_PRODUCT::PROD_TYPE) <> 0%
	THEN
		TOTAL_MAN = TOTAL_MAN + EXTENSION
	ELSE
		TOTAL_PUR = TOTAL_PUR + EXTENSION
	END IF

	SUBTOTAL_EXTENSION = SUBTOTAL_EXTENSION + EXTENSION

	IF (QTY(0%) <> 0.0)
	THEN
		IF HEADERFLAG% = 0%
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -2%)

			TEXT$ = "        Parts Not Yet Issued"
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 2%)
			GOTO ExitProgram IF UTL_REPORTX::STAT

			TEXT$ = "        Line   ReqNum      ReqLin " + &
				"Part           Description            " + &
				"Cat  ReqQty      IssQty    CanQty     " + &
				"BalQty       ExtCost"
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			GOTO ExitProgram IF UTL_REPORTX::STAT

			HEADERFLAG% = -1%
		END IF

		TEXT$ = "        " + &
			WP_REGLINE_READ::LLINE + "   " + &
			CONV_STRING( &
				WP_REQREGISTER_READ::REQNUM, CMC$_LEFT)	+ "  " + &
			WP_REQREGISTER_READ::REQLIN + "   " + &
			WP_REQREGISTER_READ::PRODUCT + " " + &
			LEFT(PD_PRODUCT::DESCRIPTION, 20%) + "   " + &
			PD_PRODUCT::PROD_TYPE + "  " + &
			FORMAT$(QTY(1%), "###,###") + "     " + &
			FORMAT$(QTY(2%), "###,###") + "   " + &
			FORMAT$(QTY(3%), "###,###") + "    " + &
			FORMAT$(QTY(0%), "###,###") + "    " + &
			FORMAT$(EXTENSION, "###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	GOTO 18050

 NewOrder:
	TEXT$ = SPACE$(88%) + &
		"MANUFACTURED PARTS" + &
		SPACE$(15%) + &
		FORMAT$(TOTAL_MAN, "###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -3%)

	TEXT$ = SPACE$(88%) + &
		"PURCHASED PARTS" + &
		SPACE$(18%) + &
		FORMAT$(TOTAL_PUR, "###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -3%)

	TEXT$ = SPACE$(88%) + &
		"SUB TOTAL" + &
		SPACE$(24%) + &
		FORMAT$(SUBTOTAL_EXTENSION, "###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -3%)

	SUBTOTAL_EXTENSION = 0.0

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

18090	!
	GOTO GetNextRec

 DropLabor:
18200	!*******************************************************************
	! Drop Labor
	!*******************************************************************
	!
	! Scan LABOR at current level
	!
	WHEN ERROR IN
		FIND #BM_PRODOPER.CH%, &
			KEY #0% EQ WP_REQREGISTER::PRODUCT, &
			REGARDLESS
	USE
		CONTINUE 18300
	END WHEN

18210	WHEN ERROR IN
		GET #BM_PRODOPER.CH%, REGARDLESS
	USE
		CONTINUE 18300
	END WHEN

	GOTO 18300 IF BM_PRODOPER::PRODUCT <> WP_REQREGISTER::PRODUCT

18220	!
	! Apply labor
	!
	TOTAL_HOURS = TOTAL_HOURS - BM_PRODOPER::HOURS * WP_REQREGISTER::QTY
	THIS_HOURS = THIS_HOURS - BM_PRODOPER::HOURS * WP_REQREGISTER::QTY

	IF PR_READ_OPERATION(BM_PRODOPER::OPERATION, &
		"", PR_OPER_READ) = CMC$_NORMAL
	THEN
		HOUR_RATE = PR_OPER_READ::HOUR_RATE
	ELSE
		HOUR_RATE = 0.0
	END IF

	TOTAL_HOURSDOL = TOTAL_HOURSDOL - &
		BM_PRODOPER::HOURS * WP_REQREGISTER::QTY * HOUR_RATE
	THIS_HOURSDOL = THIS_HOURSDOL - &
		BM_PRODOPER::HOURS * WP_REQREGISTER::QTY * HOUR_RATE

18290	!
	! End of labor
	!
	GOTO 18210

18300	!
	RETURN

18400	!*******************************************************************
	! Get payroll hours
	!*******************************************************************

	TO_BATCH_NO$ = DATE_TODAY
	FROM_BATCH_NO$ = DATE_INVDCODE(DATE_DAYCODE(DATE_TODAY) - 365%)
	REG_HR = 0.0
	OVT_HR = 0.0
	PAY_HR = 0.0

	CALL READ_DEVICE("PR_TRN_PAY", PR_TRN_PAY.DEV$, STAT%)
	CALL READ_DEVICE("PR_HIS_PAY", PR_HIS_PAY.DEV$, STAT%)

	CALL PR_FIND_DETAILFILE(FROM_BATCH_NO$, &
		TO_BATCH_NO$, &
		PR_TRN_PAY.DEV$, &
		PR_HIS_PAY.DEV$, &
		DATA_FILE$())

	DATA_FILE% = VAL%(DATA_FILE$(0%))

	!
	! Look up employee in all payroll files selected
	!
	FOR PR_LOOP% = 1% TO DATA_FILE%
		BATCH_NO$ = DATA_FILE$(PR_LOOP%)

		CALL ENTR_3MESSAGE(SCOPE, &
			"Processing folder " + &
			PRNT_DATE(BATCH_NO$, 8%), 1%)

18430		!
		! Open Pay folder
		!
		USE_HISTORY% = -1%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.OPN"
		USE
			CONTINUE 18440 IF ERR = 5%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		PR_TMP_PAY.CH% = PR_HIS_PAY.CH%

		GOTO 18500

18440		!
		! Open pay history folder if journal not there
		!
		USE_HISTORY% = 0%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
		USE
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		PR_TMP_PAY.CH% = PR_TRN_PAY.CH%

18500		!
		! Get pay detail information
		!
		WHEN ERROR IN
			IF (FROM_ITEM$ <> "")
			THEN
				FIND #PR_TMP_PAY.CH%, &
					KEY #1% GE JC_JOB::JOB, &
					REGARDLESS
			ELSE
				RESET #PR_TMP_PAY.CH%
			END IF
		USE
			CONTINUE 18540 IF ERR = 155%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

18510		WHEN ERROR IN
			GET #PR_TMP_PAY.CH%, REGARDLESS
		USE
			CONTINUE 18540 IF ERR = 11%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		!
		! If history then set history map to journal
		!
		IF USE_HISTORY%
		THEN
			PR_TRN_PAY = PR_HIS_PAY
		END IF

		GOTO 18510 IF PR_TRN_PAY::PTYPE = "A"

18525		!
		! Check current record
		!
		GOTO 18540 IF (PR_TRN_PAY::SUBACC <> JC_JOB::JOB)

		REG_HR = REG_HR + PR_TRN_PAY::REG_HR
		OVT_HR = OVT_HR + PR_TRN_PAY::OVT_HR

 !		IF PR_READ_OPERATION(PR_TRN_PAY::OPER, &
 !			"", PR_OPER_READ) = CMC$_NORMAL
 !		THEN
 !			HOUR_RATE = PR_OPER_READ::HOUR_RATE
 !		ELSE
 !			HOUR_RATE = 0.0
 !		END IF

		PAY_HR = PAY_HR + PR_TRN_PAY::GROSS

		GOTO 18510

18540		CLOSE #PR_TMP_PAY.CH%

	NEXT PR_LOOP%

	ISSUE_HOURS = ISSUE_HOURS + REG_HR + OVT_HR
	ISSUE_HOURSDOL = ISSUE_HOURSDOL + PAY_HR

	RETURN

	%PAGE

	!*******************************************************************
	! Exit
	!*******************************************************************

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

	END
