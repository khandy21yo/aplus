1	%TITLE "Job Status Summary Report"
	%SBTTL "WP_RPRT_JOBPROGRESS2"
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
	!	$ BAS WP_SOURCE:WP_RPRT_JOBPROGRESS2/LINE
	!	$ LINK/EXE=WP_EXE: WP_RPRT_JOBPROGRESS2, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_RPRT_JOBPROGRESS2.OBJ;*
	!
	! Author:
	!
	!	02/17/2003 - Kevin Handy
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

	!
	! Declare external functions
	!
	EXTERNAL LONG   FUNCTION WP_READ_REGLINE
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


	EXTERNAL SUB SCAN_BOM(INTEGER, REAL, REAL, REAL, REAL, &
		STRING, STRING, REAL, UTL_REPORTX_CDD, &
		STRING DIM (), INTEGER)

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

	DETAIL% = VAL%(UTL_REPORTX::OPTDEF(9%))

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
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TESTLINE$ = SPACE$(LEN(WP_REGLINe::LLINE))

	GRAND_TOTAL_HOURS = 0.0
	GRAND_TOTAL_HOURSDOL = 0.0
	GRAND_TOTAL_PARTS = 0.0
	GRAND_TOTAL_RAWMAT = 0.0
	GRAND_TOTAL_COST = 0.0
	GRAND_TOTAL_BURDEN = 0.0

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

	IF DETAIL% >= 9%
	THEN
		TEXT$ = "   " + &
			WP_REGLINE_READ::LLINE + " " + &
			WP_REGLINE_READ::TTYPE + " " + &
			WP_REGLINE_READ::ITEMCODE + " " + &
			FORMAT$(WP_REGLINE_READ::COST, "#######.## ") + &
			WP_REGLINE_READ::DESCR + " " + &
			FORMAT$(QTY(1%), "O######.##  ") + &
			FORMAT$(QTY(2%), "C######.##  ") + &
			FORMAT$(QTY(1%) * WP_REGLINE_READ::COST, "$######.##  ")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

17200	!
	! Find bill of material
	!
	CALL SCAN_BOM(1%, TOTAL_HOURS, TOTAL_HOURSDOL, TOTAL_RAWMAT, TOTAL_PARTS, &
		WP_REGLINE_READ::ITEMCODE, JC_JOB::LOCATION, &
		QTY(1%), UTL_REPORTX, TITLE$(), DETAIL%)

17300	IF DETAIL% >= 9%
	THEN
		TEXT$ = "      TOTAL HOURS IN BM " + &
			FORMAT$(TOTAL_HOURS, "########.####")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	IF DETAIL% >= 5%
	THEN
		BURDEN = QTY(1%) * WP_REGLINE_READ::COST - &
			TOTAL_PARTS - TOTAL_RAWMAT - TOTAL_HOURSDOL

		TEXT$ = "   " + &
			WP_REGLINE_READ::LLINE + " " + &
			WP_REGLINE_READ::ITEMCODE + " " + &
			FORMAT$(CURDEN, "#######.## ") + &
			FORMAT$(TOTAL_HOURSDOL, "#######.## ") + &
			FORMAT$(TOTAL_HOURS, "########.## ") + &
			FORMAT$(TOTAL_RAWMAT, "#######.## ") + &
			FORMAT$(TOTAL_PARTS, "#######.## ") + &
			FORMAT$(QTY(1%) * WP_REGLINE_READ::COST, "$######.##  ")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

17400	!
	! Now, go through the requisitions
	!
	ISSUE_TOTAL_DOLLARS = 0.0
	ISSUE_TOTAL_HOURS = 0.0

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

	GOTO 17410 &
		IF WP_REQREGISTER::RECTYP <> "02"

17420	IF DETAIL% >= 9%
	THEN
		TEXT$ = "         " + &
			WP_REQREGISTER::REQLIN + " " + &
			WP_REQREGISTER::RECTYP + " " + &
			WP_REQREGISTER::PRODUCT + " " + &
			WP_REQREGISTER::LOCATION + " " + &
			FORMAT$(WP_REQREGISTER::QTY, "######.#### ") + &
			FORMAT$(WP_REQREGISTER::AMT, "######.## ") + &
			WP_REQREGISTER::TRANDATE

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	ISSUE_TOTAL_DOLLARS = ISSUE_TOTAL_DOLLARS + WP_REQREGISTER::AMT

	!
	! End of WP_REQREGISTER
	!
	GOTO 17410

17600	!
	IF DETAIL% >= 5%
	THEN
		TEXT$ = "   " + &
			WP_REGLINE_READ::LLINE + " " + &
			"ISSUE HOURS   " + " " + &
			FORMAT$(0.0, "#######.## ") + &
			FORMAT$(0.0, "#######.## ") + &
			FORMAT$(ISSUE_TOTAL_HOURS, "########.## ") + &
			FORMAT$(0.0, "#######.## ") + &
			FORMAT$(0.0, "#######.## ") + &
			FORMAT$(0.0, "$######.##  ")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	BURDEN = QTY(1%) * WP_REGLINE_READ::COST - &
		TOTAL_PARTS - TOTAL_RAWMAT - TOTAL_HOURSDOL

	TEXT$ = "   " + &
		WP_REGLINE_READ::LLINE + " " + &
		WP_REGLINE_READ::ITEMCODE + " " + &
		FORMAT$(BURDEN, "#######.## ") + &
		FORMAT$(TOTAL_HOURSDOL, "#######.## ") + &
		FORMAT$(TOTAL_HOURS, "########.## ") + &
		FORMAT$(TOTAL_RAWMAT, "#######.## ") + &
		FORMAT$(TOTAL_PARTS, "#######.## ") + &
		FORMAT$(QTY(1%) * WP_REGLINE_READ::COST, "$######.##  ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = "   " + &
		WP_REGLINE_READ::LLINE + " " + &
		"COMPLETED     " + " " + &
		FORMAT$(0.0, "#######.## ") + &
		FORMAT$(0.0, "#######.## ") + &
		FORMAT$(0.0, "########.## ") + &
		FORMAT$(0.0, "#######.## ") + &
		FORMAT$(0.0, "#######.## ") + &
		FORMAT$(ISSUE_TOTAL_DOLLARS, "$######.##  ")

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
	COMPLETED_TOTAL_COST = COMPLETED_TOTAL_COST + ISSUE_TOTAL_DOLLARS

17800	!
	! Finish up this regline
	!
	GOTO ReadRegLine

17900	!
	! Finish up this job
	!
	TEXT$ = "   " + &
		"    " + " " + &
		"STANDARD      " + " " + &
		FORMAT$(GRAND_TOTAL_BURDEN, "#######.## ") + &
		FORMAT$(GRAND_TOTAL_HOURSDOL, "#######.## ") + &
		FORMAT$(GRAND_TOTAL_HOURS, "########.## ") + &
		FORMAT$(GRAND_TOTAL_RAWMAT, "#######.## ") + &
		FORMAT$(GRAND_TOTAL_PARTS, "#######.## ") + &
		FORMAT$(GRAND_TOTAL_COST, "$######.##  ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = "   " + &
		"    " + " " + &
		"COMPLETED     " + " " + &
		FORMAT$(0.0, "#######.## ") + &
		FORMAT$(0.0, "#######.## ") + &
		FORMAT$(COMPLETED_TOTAL_HOURS, "########.## ") + &
		FORMAT$(0.0, "#######.## ") + &
		FORMAT$(0.0, "#######.## ") + &
		FORMAT$(COMPLETED_TOTAL_COST, "$######.##  ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

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

	END PROGRAM


30000	SUB SCAN_BOM(INTEGER RECURSE, &
		REAL LABOR, REAL LABORDOL, REAL RAWMAT, REAL PARTS, &
		STRING PRODUCT, STRING LOCATION, REAL QTY, &
		UTL_REPORTX_CDD UTL_REPORTX, &
		STRING TITLE$(), INTEGER DETAIL%)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)


	!
	! Include codes
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.HB"
	MAP (BM_RELATION)	BM_RELATION_CDD		BM_RELATION

	%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.HB"
	MAP (BM_PRODOPER)	BM_PRODOPER_CDD		BM_PRODOPER

	%INCLUDE "SOURCE:[BM.OPEN]BM_CONTROL.HB"
	MAP (BM_CONTROL)	BM_CONTROL_CDD		BM_CONTROL

	%INCLUDE "SOURCE:[PR.OPEN]PR_OPER.HB"
	DECLARE			PR_OPER_CDD		PR_OPER_READ

	COM (CH_BM_RELATION_READ)	BM_RELATION.CH%
	COM (CH_BM_PRODOPER_READ)	BM_PRODOPER.CH%
	COM (CH_PD_PRODUCT)		PD_PRODUCT.CH%

	DIM BM_RELATION_CDD BM_LIST(200%)

	EXTERNAL REAL	FUNCTION PC_READ_COST
	EXTERNAL LONG	FUNCTION PR_READ_OPERATION

 ! PRINT "ENTER "; RECURSE, PRODUCT
	!
	! Initialize return values
	!
	LABOR = 0.0
	LABORDOL = 0.0
	RAWMAT = 0.0
	PARTS = 0.0

	THIS_LABOR = 0.0
	THIS_LABORDOL = 0.0
	THIS_RAWMAT = 0.0
	THIS_PARTS = 0.0

30100	!
	! Get product info
	!
	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, KEY #0% EQ PRODUCT, REGARDLESS
	USE
		!
		! AAaack, undefined product.
		! Assume lowest level
		!
		CONTINUE 30990
	END WHEN

	PROD_TYPE$ = PD_PRODUCT::PROD_TYPE

	!
	! lowest level?
	!
 !	IF COMP_STRING(PROD_TYPE$, BM_CONTROL::PRODTYPE) <> 0% OR &
 !		INSTR(1%, "BW,MP,DR", PROD_TYPE$) <> 0%
	IF INSTR(1%, "BW,MP,DR", PROD_TYPE$) <> 0%
	THEN
		!
		! Determine part costs here
		!
		GOTO 30900
	END IF

30500	!
	! Scan LABOR at current level
	!
	WHEN ERROR IN
		FIND #BM_PRODOPER.CH%, &
			KEY #0% EQ PRODUCT, &
			REGARDLESS
	USE
		CONTINUE 30600
	END WHEN

30510	WHEN ERROR IN
		GET #BM_PRODOPER.CH%, REGARDLESS
	USE
		CONTINUE 30600
	END WHEN

	GOTO 30600 IF BM_PRODOPER::PRODUCT <> PRODUCT

30520	!
	! Apply labor
	!
	LABOR = LABOR + BM_PRODOPER::THISHOURS * QTY
	THIS_LABOR = THIS_LABOR + BM_PRODOPER::THISHOURS * QTY

	IF PR_READ_OPERATION(BM_PRODOPER::OPERATION, &
		"", PR_OPER_READ) = CMC$_NORMAL
	THEN
		HOUR_RATE = PR_OPER_READ::HOUR_RATE
	ELSE
		HOUR_RATE = 0.0
	END IF

	LABORDOL = LABORDOL + BM_PRODOPER::THISHOURS * QTY * HOUR_RATE
	THIS_LABORDOL = THIS_LABORDOL + BM_PRODOPER::THISHOURS * QTY * HOUR_RATE

30590	!
	! End of labor
	!
	GOTO 30510

30600	!
	! Find bill of material
	!
	BM_LIST% = 0%

	WHEN ERROR IN
		FIND #BM_RELATION.CH%, &
			KEY #0% EQ PRODUCT, &
			REGARDLESS
	USE
		CONTINUE 30900
	END WHEN

30610	WHEN ERROR IN
		GET #BM_RELATION.CH%, REGARDLESS
	USE
		CONTINUE 30700
	END WHEN

	GOTO 30700 IF BM_RELATION::PRODUCT <> PRODUCT

	BM_LIST% = BM_LIST% + 1%
	BM_LIST(BM_LIST%) = BM_RELATION

30690	!
	! End of BM_RELATION
	!
	GOTO 30610

30700	!
	! Now that we have stashed a list of relations, we can
	! drill down to the next level
	!
	FOR LOOP% = 1% TO BM_LIST%

		CALL SCAN_BOM(RECURSE + 1%, &
			NEW_LABOR, NEW_LABORDOL, &
			NEW_RAWMAT, NEW_PARTS, &
			BM_LIST(LOOP%)::COMPONENT, LOCATION, &
			QTY * BM_LIST(LOOP%)::QUANTITY, &
			UTL_REPORTX, TITLE$(), DETAIL%)

		LABOR = LABOR + NEW_LABOR
		LABORDOL = LABORDOL + NEW_LABORDOL
		RAWMAT = RAWMAT + NEW_RAWMAT
		PARTS = PARTS + NEW_PARTS

	NEXT LOOP%

	GOTO 30990

30900	!
	! Lowest level, so apply cost
	!
	COST = PC_READ_COST(PRODUCT, LOCATION, DATE_TODAY, "")

	!
	! lowest level?
	!
	IF COMP_STRING(PROD_TYPE$, BM_CONTROL::PRODTYPE) <> 0%
	THEN
		RAWMAT = COST * QTY
		THIS_RAWMAT = COST * QTY
	ELSE
		PARTS = COST * QTY
		THIS_PARTS = COST * QTY
	END IF

30990	!
 ! PRINT "LEAVE "; RECURSE

	IF DETAIL% >= 3%
	THEN
		TEXT$ = "    " + &
			">" + FORMAT$(RECURSE, "<0>#") + "< " + &
			PRODUCT + " " + &
			FORMAT$(QTY, "Q###.####  ") + &
			FORMAT$(THIS_LABORDOL, "#######.## ") + &
			FORMAT$(THIS_LABOR, "########.## ") + &
			FORMAT$(THIS_RAWMAT, "#######.## ") + &
			FORMAT$(THIS_PARTS, "#######.## ")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	END SUB
