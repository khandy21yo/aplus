1	%TITLE "Accounts Payable Register Report By Location"
	%SBTTL "AP_RPRT_REG_BYLOC"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	! ID:AP002
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Accounts Payable Register Report By Location\* prints a listing of all
	!	Accounts Payable in order by location.
	!	The accounts are printed in location order and within each location a specific
	!	order depending on user specifications. The following fields are included in
	!	this report:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Transaction Number
	!	.le
	!	Invoice Number
	!	.le
	!	Invoice Date
	!	.le
	!	Invoice Amount
	!	.le
	!	Discount Amount
	!	.le
	!	Net Amount
	!	.le
	!	Discount Date
	!	.le
	!	Due Date
	!	.le
	!	Check Number
	!	.le
	!	Check Date
	!	.le
	!	Check Amount
	!	.le
	!	Balance Due
	!	.els
	!	.lm -5
	!
	! Index:
	!	.x Register Report By Location
	!	.x Location>Register Report By
	!	.x Report>Register By Location
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_RPRT_REG_BYLOC/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_RPRT_REG_BYLOC, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_RPRT_REG_BYLOC.OBJ;*
	!
	! Author:
	!
	!	04/19/89 - Kevin Handy
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/12/95 - Kevin Handy
	!		Open AP_CONTROL as .OPN instead of .MOD.
	!
	!	08/27/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/12/97 - Kevin Handy
	!		Open work file on UTL_WORK.DEV$
	!
	!	05/13/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/23/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	08/25/97 - Kevin Handy
	!		Lose unecessary function definitions
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/24/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!******************************************************************
	! External modules needed
	!******************************************************************

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!******************************************************************
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs, etc.)
	!******************************************************************

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.HB"
	MAP	(AP_CONTROL)	AP_CONTROL_CDD	AP_CONTROL

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP	(AP_OPEN)	AP_OPEN_CDD	AP_OPEN

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_DIST.HB"
	MAP	(AP_OPEN_DIST)	AP_OPEN_DIST_CDD	AP_OPEN_DIST

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP	(AP_VENDOR)	AP_VENDOR_CDD	AP_VENDOR

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP	(GL_PERIOD)	GL_PERIOD_CDD	GL_PERIOD

	!
	! Record structure for working with account distribution
	!
	RECORD ACCT_SUMMARY_CDD
		STRING	ACCT = 18%
		GFLOAT	AMOUNT
		GFLOAT	DISCOUNT
	END RECORD

	DIM ACCT_SUMMARY_CDD ACCOUNT_LIST(100%)

	!
	! Record for temp sort file
	!
	RECORD AP_TEMP_REG_CDD
		STRING	LOCATION = 8%
		STRING	SORTKEY = 16%
		STRING	VENNUM = 10%
		STRING	TRANKEY = 6%
		STRING	INVNUM = 15%
		STRING	INVDAT = 8%
		GFLOAT	INVAMT
		STRING	DISCDAT = 8%
		GFLOAT	DISAMT
		STRING	DUEDAT = 8%
		STRING	CKNUM = 6%
		STRING	CKDAT = 8%
		GFLOAT	CKAMT
	END RECORD

	MAP (AP_TEMP_REG) AP_TEMP_REG_CDD AP_TEMP_REG

	!
	! Dimension arrays
	!
	DIM STRING TEXT_LEFT(100%), TEXT_RIGHT(100%)

	%PAGE

	!******************************************************************
	! Take care of anything else before starting the report
	!******************************************************************

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Initialization:
	!******************************************************************
	! Get ready to begin
	!******************************************************************

	!
	! Initialize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	LOCATION$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) Location\*
	!	.b
	!	.lm +5
	!	The ^*Location\* field refers to the specific company location when there is
	!	more than one. For example, which warehouse, which office, or which store.
	!	.lm -5
	!
	! Index:
	!	.x Location>Register>By Location
	!	.x Register>By Location>Location
	!
	!--
	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field determines the item
	!	with which the report will begin printing. If the setting
	!	is blank, the report will being with the first item in the file.
	!	The value must be in agreement with the value in field (04)
	!	Sort by.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Register Report>By Location
	!	.x Register Report>By Location>From Item
	!
	!--
	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field determines the item
	!	with which the report will end. If this setting is blank,
	!	the report will end with the last item in the file.
	!	.b
	!	The value must be in agreement with field
	!	(04) Sort by.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Register Report>By Location
	!	.x Register Report>By Location>To Item
	!
	!--
	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Sort By (NU,NA,S)\*
	!	.b
	!	.lm +5
	!	The ^*Sort By (NU,NA,S)\* field determines if the
	!	report is to be printed in vendor number order, name order, or
	!	alphabetical order.
	!	.lm 15
	!	.LIST "*"
	!	.LIST ELEMENT
	!	^*NU\*#=#Vendor Number order
	!	.LIST ELEMENT
	!	^*NA\*#=#Vendor Name order
	!	.LIST ELEMENT
	!	^*S\*##=#Alphabetical order
	!	.END LIST
	!	.lm -5
	!	There are no other valid values for this field.
	!	.note
	!	The Name Order selection sorts on the name field.
	!	Hence, personal names would sort by first name first.
	!	A name field with a title would sort by the title
	!	first, i.e. "Dr. James Petersen" would be listed
	!	with the "D's".
	!	.end note
	!
	! Index:
	!	.x Sort By>Register Report>By Location
	!
	!--
	PRINT_DUE_ONLY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Print Due Only (Y/N)\*
	!	.b
	!	.lm +5
	!	The ^*Print Due Only\* field prints all the Accounts
	!	Payables with any activity or just the accounts with a balance due. A ^*Y\*
	!	causes only payment due accounts, while a ^*N\* causes all accounts
	!	with activity, to be printed.
	!	.lm -5
	!
	! Index:
	!	.x Due Only>Register>By Location
	!	.x Register>By Location>Due Only
	!
	!--
	CUT_OFF_REG$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!
	! Datatype:TEXT
	! Size:0
	!--

	SELECT SORTBY$

	CASE "NA"
		K_NUM% = 1%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AP_VENDOR::VENNAM))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AP_VENDOR::VENNAM))

	CASE "SO"
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AP_VENDOR::ALPSRT))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AP_VENDOR::ALPSRT))
		K_NUM% = 2%

	CASE ELSE
		K_NUM% = 0%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AP_VENDOR::VENNUM))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AP_VENDOR::VENNUM))

	END SELECT

	%PAGE


300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.OPN"
	USE
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

305	!
	! Open the distribution file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_DIST.OPN"
	USE
		FILENAME$ = "AP_OPEN_DIST"
		CONTINUE HelpError
	END WHEN

	!
	! Open the Accounts Payable Vendor file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.OPN"
	USE
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

315	!
	! Open temp sort file
	!
	CALL ASSG_CHANNEL(AP_TEMP.CH%, STAT%)
	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "AP_TEMP.TMP" FOR OUTPUT AS FILE AP_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP AP_TEMP_REG, &
			PRIMARY KEY (AP_TEMP_REG::LOCATION, AP_TEMP_REG::SORTKEY, &
				AP_TEMP_REG::VENNUM, AP_TEMP_REG::TRANKEY) DUPLICATES, &
			TEMPORARY, &
			BUFFER 32%, &
			ACCESS MODIFY, &
			ALLOW NONE
	USE
		FILENAME$ = "AP_TEMP_REG"
		CONTINUE HelpError
	END WHEN

	!
	! We don't need to open any more files if this is a Cutoff Ledger
	!
	GOTO ReportTitle IF CUT_OFF_REG$ <> "Y"

	!
	! Open the General Ledger Controlling file (GL_PERIOD.CTR)
	!
320	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
		CLOSE GL_PERIOD.CH%
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	!
	! Open the Accounts Payable Controlling file
	!
330	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.OPN"
		GET #AP_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "AP_CONTROL"
		CONTINUE HelpError
	END WHEN

	SELECT AP_CONTROL::CLOSEFLAG

	CASE "1"
		CALL HELP_3MESSAGE(SCOPE, "AP Close in process", &
			"ERR", "AP_CLOSE", "ERROR_CLOSE")
		GOTO ExitProgram

	CASE "2"
		CALL HELP_3MESSAGE(SCOPE, "AP Reset in process", &
			"ERR", "AP_RESET", "ERROR_RESET")
		GOTO ExitProgram

	CASE "3"
		CALL HELP_3MESSAGE(SCOPE, "AP Purge in process", &
			"ERR", "AP_PURGE", "ERROR_PURGE")
		GOTO ExitProgram

	END SELECT

	CUR_PERIOD% = AP_CONTROL::LASTPERCLOSE + 1%
	YEAR$ = AP_CONTROL::YEAR

	IF CUR_PERIOD% > GL_PERIOD::FPFY
	THEN
		CUR_PERIOD% = 1%
		YEAR$ = FORMAT$(VAL%(YEAR$) + 1%, "<0>###")
	END IF

	CUT_OFF_DATE$ = YEAR$ + FORMAT$(CUR_PERIOD%, "<0>#")

	%PAGE

 ReportTitle:
	!
	! Titles
	!
	TITLE$(1%) = "Accounts Payable Register by Location"
	TITLE$(1%) = "Accounts Payable Ledger by Location for Accounting " + &
		"Period Ended " + MID(CUT_OFF_DATE$, 5%, 2%) + ", " + &
		LEFT(CUT_OFF_DATE$, 4%) &
		IF CUT_OFF_REG$ = "Y"
	TITLE$(2%) = "For Location ????"
	TITLE$(3%) = ""

	!
	! Headers
	!
	TITLE$(4%) = "Trans#  InvoiceNum       InvDate        " + &
		"InvAmt    DiscAmt       NetAmt DiscDate DueDate  " + &
		"CkNum      CkDate        CkAmt   BalanceDue"
	TITLE$(5%) = ""

	!
	! Layouts for lines printed
	!
	LYT_LINE$ = "$TRANS:8,$INVOICE:25,DINVDATE:33,VINVAMT:47," + &
		"VDISAMT:58,VNETAMT:71,DDISDAT:80,DDUEDAT:89," + &
		"$CKNUM:98,DCKDAT:107,VCKAMT:121,VBALANCE:132"

	%PAGE

16000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	IF FROM_ITEM$ = ""
	THEN
		RESET #AP_VENDOR.CH%, KEY #K_NUM%
	ELSE
		FIND #AP_VENDOR.CH%, KEY #K_NUM% GE FROM_ITEM$, REGARDLESS
	END IF

 GetNextRecLoad:
16010	!******************************************************************
	! Main report loop starts here
	!******************************************************************

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AP_VENDOR.CH%, REGARDLESS
	USE
		CONTINUE 17000
	END WHEN

	CALL ENTR_3MESSAGE(SCOPE, AP_VENDOR::VENNUM, 1%)

	!
	! Check current record
	!
	SELECT SORTBY$

	CASE "NU"
		GOTO 17000 &
			IF (AP_VENDOR::VENNUM > TO_ITEM$) AND &
			(TO_ITEM$ <> "")
		AP_TEMP_REG::SORTKEY = AP_VENDOR::VENNUM

	CASE "NA"
		GOTO 17000 &
			IF (AP_VENDOR::VENNAM > TO_ITEM$) AND &
			(TO_ITEM$ <> "")
		AP_TEMP_REG::SORTKEY = AP_VENDOR::VENNAM

	CASE ELSE
		GOTO 17000 &
			IF (AP_VENDOR::ALPSRT > TO_ITEM$) AND &
			(TO_ITEM$ <> "")
		AP_TEMP_REG::SORTKEY = AP_VENDOR::ALPSRT

	END SELECT

	!
	! Get ready to start a sub-loop
	!
16030	WHEN ERROR IN
		FIND #AP_OPEN.CH%, KEY #0% EQ AP_VENDOR::VENNUM, REGARDLESS
	USE
		CONTINUE 16010
	END WHEN

	!
	! Get the (next) record
	!
16040	WHEN ERROR IN
		GET #AP_OPEN.CH%, REGARDLESS
	USE
		CONTINUE 16010
	END WHEN

	GOTO 16900 &
		IF (AP_VENDOR::VENNUM <> AP_OPEN::VENNUM)

	GOTO 16040 &
		IF (CUT_OFF_DATE$ < LEFT(AP_OPEN::UPDATED, 6%)) AND &
		(CUT_OFF_REG$ = "Y")

	!
	! Fill in as much of the sort record as is common
	! to all types written out
	!
	AP_TEMP_REG::VENNUM	= AP_OPEN::VENNUM
	AP_TEMP_REG::TRANKEY	= AP_OPEN::TRANKEY
	AP_TEMP_REG::INVNUM	= AP_OPEN::INVNUM
	AP_TEMP_REG::INVDAT	= AP_OPEN::INVDAT
	AP_TEMP_REG::DISCDAT	= AP_OPEN::DISCDAT
	AP_TEMP_REG::DUEDAT	= AP_OPEN::DUEDAT
	AP_TEMP_REG::CKDAT	= AP_OPEN::CKDAT

16080	!
	! Now create all of the necessary records.
	! I am creating seperate records for the invoice and the
	! check part to make things much easier for myself.
	!
	IF (AP_OPEN::INVAMT <> 0.0) OR (AP_OPEN::DISAMT <> 0.0)
	THEN
		TEST_AP% = INSTR(1%, AP_OPEN::AP_ACCT, "*")
		TEST_AP% = INSTR(1%, AP_OPEN::AP_ACCT, "?") &
			IF TEST_AP% = 0%

		IF (TEST_AP% = 0%)
		THEN
			!
			! Create one record for AP_OPEN invoice data
			!
			LOC% = INSTR(1%, AP_OPEN::AP_ACCT, "-")
			AP_TEMP_REG::LOCATION = &
				RIGHT(AP_OPEN::AP_ACCT, LOC% + 1%)
			AP_TEMP_REG::INVAMT	= AP_OPEN::INVAMT
			AP_TEMP_REG::DISAMT	= AP_OPEN::DISAMT
			AP_TEMP_REG::CKAMT	= 0.0
			AP_TEMP_REG::CKNUM	= ""

			GOSUB PutApTemp !PUT #AP_TEMP.CH%
		ELSE
			!
			! Create multiple records for AP_OPEN invoice data
			!
			GOSUB LoadDistribution
			AP_TEMP_REG::CKAMT	= 0.0

			FOR LOOP% = 1% TO ACCOUNT_LIST%
				AP_TEMP_REG::LOCATION = &
					ACCOUNT_LIST(LOOP%)::ACCT
				AP_TEMP_REG::INVAMT = &
					ACCOUNT_LIST(LOOP%)::AMOUNT
				AP_TEMP_REG::DISAMT = &
					ACCOUNT_LIST(LOOP%)::DISCOUNT
				AP_TEMP_REG::CKNUM = ""

				GOSUB PutApTemp !PUT #AP_TEMP.CH%
			NEXT LOOP%
		END IF
	END IF

	IF AP_OPEN::CKAMT <> 0.0
	THEN
		TEST_CHECK% = INSTR(1%, AP_OPEN::CASH_ACCT, "*")
		TEST_CHECK% = INSTR(1%, AP_OPEN::CASH_ACCT, "?") &
			IF TEST_CHECK% = 0%

		AP_TEMP_REG::CKNUM	= AP_OPEN::CKNUM

		IF (TEST_CHECK% = 0%)
		THEN
			!
			! Create one record for AP_OPEN payment data
			!
			LOC% = INSTR(1%, AP_OPEN::CASH_ACCT, "-")
			AP_TEMP_REG::LOCATION = RIGHT(AP_OPEN::CASH_ACCT, &
				LOC% + 1%)
			AP_TEMP_REG::INVAMT	= 0.0
			AP_TEMP_REG::DISAMT	= 0.0
			AP_TEMP_REG::CKAMT	= AP_OPEN::CKAMT

			GOSUB PutApTemp !PUT #AP_TEMP.CH%
		ELSE
			!
			! Create multiple records for AP_OPEN payment data
			!
			GOSUB LoadDistribution
			AP_TEMP_REG::INVAMT	= 0.0
			AP_TEMP_REG::DISAMT	= 0.0

			FOR LOOP% = 1% TO ACCOUNT_LIST%
				AP_TEMP_REG::LOCATION= ACCOUNT_LIST(LOOP%)::ACCT
				AP_TEMP_REG::CKAMT = ACCOUNT_LIST(LOOP%)::AMOUNT

				GOSUB PutApTemp ! PUT #AP_TEMP.CH%
			NEXT LOOP%
		END IF
	END IF

16200	!
	! End of sub-loop
	!
	GOTO 16040

16900	!
	! Try for next record
	!
	GOTO GetNextRecLoad

16990	!*******************************************************************
	! Try to print ap temp records
	!*******************************************************************
 PutApTemp:

	IF LOCATION$ <> ""
	THEN
		IF COMP_STRING(AP_TEMP_REG::LOCATION, LOCATION$) = 0%
		THEN
			RETURN
		END IF
	END IF

	PUT #AP_TEMP.CH%

	RETURN

	%PAGE

17000	!*******************************************************************
	! Start the actual printing part of this report
	!*******************************************************************
 StartReport:
	WHEN ERROR IN
		RESET #AP_TEMP.CH%
	USE
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

	THIS_VENDOR$ = ""
	THIS_LOC$ = "784931724892"		! Impossible value

 GetNextRec:
17010	!******************************************************************
	! Main report loop starts here
	!******************************************************************

	!
	! Get ready to start a sub-loop
	!
	LC_LEFT% = 0%
	LC_RIGHT% = 0%
	VENDOR_NAME_TEST% = -1%
	LOCATION_TEST% = 0%

	INV_AMT, DIS_AMT, NET_AMT, CHK_AMT, BAL_DUE = 0.0
	VEN_INV_AMT, VEN_DIS_AMT, VEN_NET_AMT, VEN_CHK_AMT = 0.0
	LOC_INV_AMT, LOC_DIS_AMT, LOC_NET_AMT, LOC_CHK_AMT = 0.0

	TRANKEY$ = "12345678901234567890"	! Un-matchable value

	!
	! Get the (next) record
	!
17040	WHEN ERROR IN
		GET #AP_TEMP.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

	IF (AP_TEMP_REG::TRANKEY <> TRANKEY$) OR &
		(THIS_VENDOR$ <> AP_TEMP_REG::VENNUM) OR &
		(AP_TEMP_REG::LOCATION <> THIS_LOC$)
	THEN
		GOSUB VendorLine
		GOTO ExitProgram IF UTL_REPORTX::STAT

		TRANKEY$ = AP_TEMP_REG::TRANKEY
	END IF

	IF (THIS_VENDOR$ <> AP_TEMP_REG::VENNUM) OR &
		(AP_TEMP_REG::LOCATION <> THIS_LOC$)
	THEN
		GOSUB VendorTotal
		GOTO ExitProgram IF UTL_REPORTX::STAT

		THIS_VENDOR$ = AP_TEMP_REG::VENNUM
		VENDOR_NAME_TEST% = -1%
	END IF

	IF (AP_TEMP_REG::LOCATION <> THIS_LOC$)
	THEN
		GOSUB LocTotal
		THIS_LOC$ = AP_TEMP_REG::LOCATION
	END IF

17080	!
	! Skip if nothing being invoiced
	!
	GOTO 17090 &
		IF (AP_TEMP_REG::INVAMT = 0.0) AND &
		(AP_TEMP_REG::DISAMT = 0.0) AND &
		(LC_LEFT% >= 1%)

	LC_LEFT% = LC_LEFT% + 1%

	IF (LC_LEFT% = 1%)
	THEN
		TEXT_LEFT(LC_LEFT%) = AP_TEMP_REG::TRANKEY + "  " + &
			AP_TEMP_REG::INVNUM + "  " + &
			PRNT_DATE(AP_TEMP_REG::INVDAT, 0%) + " "
	ELSE
		TEXT_LEFT(LC_LEFT%) = SPACE$(34%)
	END IF

	NET = AP_TEMP_REG::INVAMT - AP_TEMP_REG::DISAMT

	TEXT_LEFT(LC_LEFT%) = TEXT_LEFT(LC_LEFT%) + &
		FORMAT$(AP_TEMP_REG::INVAMT, "<%>########.## ") + &
		FORMAT$(AP_TEMP_REG::DISAMT, "<%>######.## ") + &
		FORMAT$(NET, "<%>########.## ")

	IF (LC_LEFT% = 1%)
	THEN
		TEXT_LEFT(LC_LEFT%) = TEXT_LEFT(LC_LEFT%) + &
			PRNT_DATE(AP_TEMP_REG::DISCDAT, 0%) + " " + &
			PRNT_DATE(AP_TEMP_REG::DUEDAT, 0%) + " "
	END IF

17090	IF (AP_TEMP_REG::CKAMT <> 0.0) OR (AP_TEMP_REG::CKNUM <> "")
	THEN
		LC_RIGHT% = LC_RIGHT% + 1%

		TEXT_RIGHT(LC_RIGHT%) = &
			AP_TEMP_REG::CKNUM + "   " + &
			PRNT_DATE(AP_TEMP_REG::CKDAT, 0%) + &
			FORMAT$(AP_TEMP_REG::CKAMT, " #########.## ")
	END IF

	INV_AMT = INV_AMT + AP_TEMP_REG::INVAMT
	DIS_AMT = DIS_AMT + AP_TEMP_REG::DISAMT
	NET_AMT = NET_AMT + AP_TEMP_REG::INVAMT - AP_TEMP_REG::DISAMT
	CHK_AMT = CHK_AMT + AP_TEMP_REG::CKAMT
	BAL_DUE = BAL_DUE + (AP_TEMP_REG::INVAMT - AP_TEMP_REG::DISAMT) - &
		AP_TEMP_REG::CKAMT

	!
	! End of sub-loop
	!
	GOTO 17040

	%PAGE

	!******************************************************************
	! Hendle totals and other items before EXITing
	!******************************************************************

 ExitTotal:
	!
	! Finish up printing
	!
	GOSUB VendorLine
	GOSUB VendorTotal

	!
	! Handle end of report
	!
	TEXT$ = "          Grand Total" + SPACE$(12%) + &
		FORMAT$(TOT_INV_AMT, " #########.##") + &
		FORMAT$(TOT_DIS_AMT, " #######.##") + &
		FORMAT$(TOT_NET_AMT, " #########.##") + &
		SPACE$(36%) + &
		FORMAT$(TOT_CHK_AMT, " #########.##") + &
		FORMAT$(TOT_NET_AMT - TOT_CHK_AMT, " #########.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

 ExitProgram:
	!
	! Finish up the report
	!
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

 VendorLine:
17950	!******************************************************************
	! Subroutine to print vendor register detail lines
	!******************************************************************

	RETURN IF (LC_LEFT% = 0%) AND (LC_RIGHT% = 0%)

	GET #AP_VENDOR.CH%, &
		KEY #0% EQ THIS_VENDOR$, &
		REGARDLESS &
		IF THIS_VENDOR$ <> AP_VENDOR::VENNUM

	IF LC_LEFT% > LC_RIGHT%
	THEN
		MAX_LC% = LC_LEFT%
	ELSE
		MAX_LC% = LC_RIGHT%
	END IF

	IF (PRINT_DUE_ONLY$ = "Y") AND &
		(FUNC_ROUND(BAL_DUE, 2%) <> 0.0) OR &
		(PRINT_DUE_ONLY$ <> "Y")
	THEN
		IF (VENDOR_NAME_TEST%)
		THEN
			TEXT$ = AP_VENDOR::VENNUM + " " + AP_VENDOR::VENNAM
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 5%)

			VENDOR_NAME_TEST% = 0%
		END IF

		FOR LOOP% = 1% TO MAX_LC%
			TEXT$ = ""
			TEXT$ = TEXT_LEFT(LOOP%) IF LOOP% <= LC_LEFT%
			TEXT$ = TEXT$ + SPACE$(89% - LEN(TEXT$)) + &
				TEXT_RIGHT(LOOP%) &
				IF LOOP% <= LC_RIGHT%
			TEXT$ = TEXT$ + SPACE$(120% - LEN(TEXT$)) + &
				FORMAT$(BAL_DUE, "#########.##") &
				IF LOOP% = MAX_LC%

			CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), &
				TEXT$, 0%)
		NEXT LOOP%

		VEN_INV_AMT = VEN_INV_AMT + INV_AMT
		VEN_DIS_AMT = VEN_DIS_AMT + DIS_AMT
		VEN_NET_AMT = VEN_NET_AMT + NET_AMT
		VEN_CHK_AMT = VEN_CHK_AMT + CHK_AMT

	END IF

 ComeBack1:
	INV_AMT, DIS_AMT, NET_AMT, CHK_AMT, BAL_DUE = 0.0
	LC_LEFT% = 0%
	LC_RIGHT% = 0%

	RETURN

	%PAGE

 VendorTotal:
	!******************************************************************
	! Subroutine to print Vendor total
	!******************************************************************

	IF (VENDOR_NAME_TEST% = 0%)
	THEN
		TOT_INV_AMT = TOT_INV_AMT + VEN_INV_AMT
		TOT_DIS_AMT = TOT_DIS_AMT + VEN_DIS_AMT
		TOT_NET_AMT = TOT_NET_AMT + VEN_NET_AMT
		TOT_CHK_AMT = TOT_CHK_AMT + VEN_CHK_AMT

		LOC_INV_AMT = LOC_INV_AMT + VEN_INV_AMT
		LOC_DIS_AMT = LOC_DIS_AMT + VEN_DIS_AMT
		LOC_NET_AMT = LOC_NET_AMT + VEN_NET_AMT
		LOC_CHK_AMT = LOC_CHK_AMT + VEN_CHK_AMT

		LOCATION_TEST% = -1%	! Need to print location totals

		TEXT$ = "     Vendor Total" + SPACE$(16%) + &
			FORMAT$(VEN_INV_AMT, " #########.##") + &
			FORMAT$(VEN_DIS_AMT, " #######.##") + &
			FORMAT$(VEN_NET_AMT, " #########.##") + &
			SPACE$(36%) + &
			FORMAT$(VEN_CHK_AMT, " #########.##") + &
			FORMAT$(VEN_NET_AMT - VEN_CHK_AMT, " #########.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	END IF

	VEN_INV_AMT, VEN_DIS_AMT, VEN_NET_AMT, VEN_CHK_AMT = 0.0
	RETURN

	%PAGE

 LocTotal:
	!******************************************************************
	! Subroutine to print Location total
	!******************************************************************

	IF (LOCATION_TEST% <> 0%)
	THEN
		TEXT$ = "     Location Total" + SPACE$(14%) + &
			FORMAT$(LOC_INV_AMT, " #########.##") + &
			FORMAT$(LOC_DIS_AMT, " #######.##") + &
			FORMAT$(LOC_NET_AMT, " #########.##") + &
			SPACE$(36%) + &
			FORMAT$(LOC_CHK_AMT, " #########.##") + &
			FORMAT$(LOC_NET_AMT - VEN_CHK_AMT, " #########.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TITLE$(2%) = "For Location " + AP_TEMP_REG::LOCATION
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 3000%)

		LOCATION_TEST% = 0%
	END IF

	TITLE$(2%) = "For Location " + AP_TEMP_REG::LOCATION
	LOC_INV_AMT, LOC_DIS_AMT, LOC_NET_AMT, LOC_CHK_AMT = 0.0
	TRANKEY$ = "12345678901234567890"	! Un-matchable value

	RETURN

	%PAGE

18000	!*******************************************************************
	! Load in the distribution amounts
	!*******************************************************************
 LoadDistribution:

	ACCOUNT_LIST% = 0%

	WHEN ERROR IN
		FIND #AP_OPEN_DIST.CH%, KEY #0% GE AP_OPEN::TRANKEY, REGARDLESS
	USE
		CONTINUE 18090
	END WHEN

18010	WHEN ERROR IN
		GET #AP_OPEN_DIST.CH%, REGARDLESS
	USE
		CONTINUE 18090
	END WHEN

	GOTO 18090 IF AP_OPEN_DIST::TRANKEY <> AP_OPEN::TRANKEY

	ACC_TEST% = INSTR(1%, AP_OPEN_DIST::ACCT, "-")
	ACC_TEST$ = RIGHT(AP_OPEN_DIST::ACCT, ACC_TEST% + 1%)

	!
	! Summarize
	!
	FOR LOOP% = 1% TO ACCOUNT_LIST%
		GOTO 18020 IF ACCOUNT_LIST(LOOP%)::ACCT = ACC_TEST$
	NEXT LOOP%

	LOOP%, ACCOUNT_LIST% = ACCOUNT_LIST% + 1%
	ACCOUNT_LIST(LOOP%)::ACCT = ACC_TEST$
	ACCOUNT_LIST(LOOP%)::AMOUNT = 0.0
	ACCOUNT_LIST(LOOP%)::DISCOUNT = 0.0

18020	ACCOUNT_LIST(LOOP%)::AMOUNT = &
		ACCOUNT_LIST(LOOP%)::AMOUNT + AP_OPEN_DIST::AMOUNT
	ACCOUNT_LIST(LOOP%)::DISCOUNT = &
		ACCOUNT_LIST(LOOP%)::DISCOUNT + AP_OPEN_DIST::DISCAMT

	GOTO 18010

18090	RETURN

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
