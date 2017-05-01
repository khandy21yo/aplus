1	%TITLE "Payroll Comparison Report"
	%SBTTL "PR_RPRT_TRN_COMP"
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
	! ID:PR009
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Payroll Comparison\* report option
	!	compares certain information for the current payroll
	!	folder file with the previous payroll folder file. However, any
	!	two selected payroll folder files could be compared. The following
	!	information is included:
	!	.table 3,25
	!	.te
	!	Employee Number
	!	.te
	!	Employee Name
	!	.te
	!	Current Period Hours
	!	.te
	!	Previous Period Hours
	!	.te
	!	Difference in Hours
	!	.te
	!	Current Period Units
	!	.te
	!	Previous Period Units
	!	.te
	!	Difference in Units
	!	.te
	!	Current Gross Earnings
	!	.te
	!	Previous Gross Earnings
	!	.te
	!	Difference in Earnings
	!	.te
	!	Grand Totals for Each Column
	!	.end table
	!
	! Index:
	!	.X Payroll Comparison>Report
	!	.x Report>Payroll Comparison
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_TRN_COMP/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_TRN_COMP, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_TRN_COMP.OBJ;*
	!
	! Author:
	!
	!	11/27/87 - Robert Peterson
	!
	! Modification history:
	!
	!	06/18/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		also be sent to either a spreadsheet or a DIF file.
	!
	!	06/04/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/12/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/30/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/12/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP (PR_TRN_PAY)	PR_TRN_PAY_CDD	PR_TRN_PAY
	MAP (PR_HIS_PAY)	PR_TRN_PAY_CDD	PR_HIS_PAY

	!
	! Declare variables and constants
	!
	DECLARE	STRING	LYT_LINE

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Init:
	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	.ts 55
	!	^*(01) Current Payroll Date	MMDDYYYY or MMDDYY\*
	!	.B
	!	.LM +5
	!	The ^*Current Payroll Date\* enters the
	!	payroll date which will be used in the comparison.
	!	.lm -5
	!
	! Index:
	!	.x Current Payroll Date>Payroll Comparison
	!	.x Payroll Comparison>Current Payroll Date
	!
	!--

	BATCH_NO$ = DATE_STOREDATE(BATCH_NO$)

	LAST_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	.ts 55
	!	^*(02) Last Payroll Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*Last Payroll Date\* enters the date
	!	of the last payroll which will be used in the comparison.
	!	.lm -5
	!
	! Index:
	!	.x Last Payroll Date>Payroll Comparison
	!	.x Payroll Comparison>Last Payroll Date
	!
	!--

	LAST_BATCH_NO$ = DATE_STOREDATE(LAST_BATCH_NO$)

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field enters a particular
	!	item with which the report will begin printing.
	!	The value must be in agreement with
	!	field (05).
	!	.b
	!	A blank field will cause the report to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Payroll Comparison
	!	.x Payroll Comparison>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field enters a particular
	!	item with which the report will end printing.
	!	The value must be in agreement with field (05).
	!	.b
	!	A blank field will cause the report to end with the last
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Payroll Comparison
	!	.x Payroll Comparison>To Item
	!
	!--


	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)
	!++
	! Abstract:FLD05
	!	.ts 55
	!	^*(05) Sort	NU,NA,SO,LO\*
	!	.b
	!	.lm +5
	!	The ^*Sort\* field causes the report to print
	!	in a particular order.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*NU\*	Number
	!	.te
	!	^*NA\*	Name
	!	.te
	!	^*SO\*	Alphabetical (last name first)
	!	.te
	!	^*LO\*	Location
	!	.end table
	!	An entry is required in this field.
	!
	! Index:
	!	.x Sort>Payroll Comparison
	!	.x Payroll Comparison>Sort
	!
	!--

	VAR_LESS_CUR = VAL(EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)) / 100.0
	!++
	! Abstract:FLD06
	!	^*(06) Current % < Previous\*
	!	.b
	!	.lm +5
	!	The ^*Current % < Previous\* field allows for the parameter to be established
	!	as to what differences are to be examined. For example, if 25 was entered
	!	in the field, then all current percentages that are 25% less than the
	!	previous would be printed.
	!	.lm -5
	!
	! Index:
	!	.x Current %< Previous>Payroll Comparison
	!	.x Payroll Comparison>Current %<Previous
	!
	!--

	VAR_GRTR_CUR = VAL(EDIT$(UTL_REPORTX::OPTDEF(6%), -1%)) / 100.0
	!++
	! Abstract:FLD07
	!	^*(07) Current % > Previous\*
	!	.b
	!	.lm +5
	!	The ^*Current % > Previous\* field allows for the parameter of greater
	!	comparison
	!	to be entered. For example, if all percentages that were 25% greater than
	!	the previous
	!	were desired, the number 25 would be entered in this field.
	!	.lm -5
	!
	! Index:
	!	.x Current % > Previous>Payroll Comparison
	!	.x Payroll Comparison>Current % > Previous
	!
	!--

	SELECT SORTBY$
	CASE "NU"
		K_NUM% = 0%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::EMPNUM))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::EMPNUM))

	CASE "NA"
		K_NUM% = 1%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::EMPNAME))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::EMPNAME))

	CASE "SN"
		K_NUM% = 3%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::SSN))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::SSN))

	CASE "LO"
		K_NUM% = 4%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::LOCATION))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::LOCATION))

	CASE ELSE
		K_NUM% = 2%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::SORT))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::SORT))

	END SELECT

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Pay folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
	USE
		CONTINUE 315 IF ERR = 5%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	PR_TRN_PAY_CUR.CH% = PR_TRN_PAY.CH%

	GOTO 320

315	!
	! Open Pay history folder if journal folder not there
	!
	USE_HISTORY% = -1%

	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.OPN"
	USE
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	PR_TRN_PAY_CUR.CH% = PR_HIS_PAY.CH%

320	!
	! Open Last Pay folder
	!
	TEMP_BATCH$ = BATCH_NO$
	BATCH_NO$ = LAST_BATCH_NO$
	PR_TRN_PAY.CH% = 0%

	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
	USE
		CONTINUE 325 IF ERR = 5%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	BATCH_NO$ = TEMP_BATCH$

	PR_TRN_PAY_LAST.CH% = PR_TRN_PAY.CH%

	GOTO ReportTitle

325	!
	! Open Last Pay folder
	!
	USE_LAST_HISTORY% = -1%

	PR_HIS_PAY.CH% = PR_TRN_PAY_LAST.CH%

	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.OPN"
	USE
		FILENAME$ = "PR_HIS_PAY"
		CONTINUE HelpError
	END WHEN

	BATCH_NO$ = TEMP_BATCH$

	PR_TRN_PAY_LAST.CH% = PR_HIS_PAY.CH%

	%PAGE

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "Payroll Comparison Report"
	TITLE$(2%) = "For the Payroll Folders Dated:  " + &
		PRNT_DATE(BATCH_NO$, 8%) + " and " + &
		PRNT_DATE(LAST_BATCH_NO$, 8%)
	TITLE$(3%) = ""

	!
	! Column headings
	!
	TITLE$(4%) = SPACE$(42%) + &
		"-------------Hour------------" + &
		" ------------Units------------ -------------Pay-------------"
	TITLE$(5%) = "Emp #      Name                           " + &
		"  Current  Previous       Dif" + &
		"   Current  Previous       Dif   Current  Previous       Dif"
	TITLE$(6%) = ""

	!
	! Line layouts
	!
	LYT_LINE = "$EmpNum:010,$EmpName:041,VCurHrs:051,VPrevHrs:061," + &
		"VHrDiff:071,VCurUnits:081,VPrevUnits:091,VUnitDiff:101," + &
		"VCurPay:111,VPrevPay:121,VPayDiff:131"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PR_EMP_MASTER.CH%, KEY #K_NUM%
		ELSE
			FIND #PR_EMP_MASTER.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "PR_EMP_MASTER"
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
		GET #PR_EMP_MASTER.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORTBY$
	CASE "NU"
		GOTO ExitTotal IF (PR_EMP_MASTER::EMPNUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "NA"
		GOTO ExitTotal IF (PR_EMP_MASTER::EMPNAME > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "SN"
		GOTO ExitTotal IF (PR_EMP_MASTER::SSN > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "DP"
		GOTO ExitTotal IF (PR_EMP_MASTER::DEPT > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE ELSE
		GOTO ExitTotal IF (PR_EMP_MASTER::SORT > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	END SELECT

	EMP_TOTAL(I%) = 0.0 FOR I% = 1% TO 10%

17040	WHEN ERROR IN
		FIND #PR_TRN_PAY_CUR.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17060 IF ERR = 155%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

17050	WHEN ERROR IN
		GET #PR_TRN_PAY_CUR.CH%, REGARDLESS
	USE
		CONTINUE 17060 IF ERR = 11%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	!
	! If history then set history into journal file
	!
	IF USE_HISTORY%
	THEN
		PR_TRN_PAY = PR_HIS_PAY
	END IF

	GOTO 17060 IF PR_TRN_PAY::EMPNUM <> PR_EMP_MASTER::EMPNUM

	EMP_TOTAL(1%) = EMP_TOTAL(1%) + PR_TRN_PAY::REG_HR + PR_TRN_PAY::OVT_HR
	EMP_TOTAL(4%) = EMP_TOTAL(4%) + PR_TRN_PAY::PIECE
	EMP_TOTAL(7%) = EMP_TOTAL(7%) + PR_TRN_PAY::GROSS

	GOTO 17050

17060	WHEN ERROR IN
		FIND #PR_TRN_PAY_LAST.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17100 IF ERR = 155%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

17070	WHEN ERROR IN
		GET #PR_TRN_PAY_LAST.CH%, REGARDLESS
	USE
		CONTINUE 17100 IF ERR = 11%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	!
	! If history then set history into journal file
	!
	IF USE_LAST_HISTORY%
	THEN
		PR_TRN_PAY = PR_HIS_PAY
	END IF

	GOTO 17100 IF PR_TRN_PAY::EMPNUM <> PR_EMP_MASTER::EMPNUM

	EMP_TOTAL(2%) = EMP_TOTAL(2%) + PR_TRN_PAY::REG_HR + PR_TRN_PAY::OVT_HR
	EMP_TOTAL(5%) = EMP_TOTAL(5%) + PR_TRN_PAY::PIECE
	EMP_TOTAL(8%) = EMP_TOTAL(8%) + PR_TRN_PAY::GROSS

	GOTO 17070

17100	!
	! Print employee record
	!
	EMP_TOTAL(3%) = EMP_TOTAL(1%) - EMP_TOTAL(2%)
	EMP_TOTAL(6%) = EMP_TOTAL(4%) - EMP_TOTAL(5%)
	EMP_TOTAL(9%) = EMP_TOTAL(7%) - EMP_TOTAL(8%)

	TEST% = 0%
	TEST% = -1% IF EMP_TOTAL(LOOP%) <> 0.0 FOR LOOP% = 1% TO 9%

	IF TEST% AND VAR_LESS_CUR <> 0.0 OR TEST% AND VAR_GRTR_CUR <> 0.0
	THEN
		IF VAR_LESS_CUR <> 0.0 AND VAR_GRTR_CUR <> 0.0
		THEN
			TEST% = 0% &
				IF EMP_TOTAL(8%) * (1 - VAR_LESS_CUR) < &
				EMP_TOTAL(7%) AND &
				EMP_TOTAL(7%) < (1 + VAR_GRTR_CUR) * &
				EMP_TOTAL(8%)
		ELSE
			IF VAR_GRTR_CUR <> 0.0
			THEN
				TEST% = 0% &
					IF EMP_TOTAL(7%) < &
					(1 + VAR_GRTR_CUR) * EMP_TOTAL(8%)
			ELSE
				TEST% = 0% &
					IF EMP_TOTAL(8%) * (1 - VAR_LESS_CUR) &
					< EMP_TOTAL(7%)
			END IF
		END IF
	END IF

	IF TEST%
	THEN
		TEXT$ = PR_EMP_MASTER::EMPNUM + " " + &
			PR_EMP_MASTER::EMPNAME + " " + &
			FORMAT$(EMP_TOTAL(1%), "######.## ") + &
			FORMAT$(EMP_TOTAL(2%), "######.## ") + &
			FORMAT$(EMP_TOTAL(3%), "######.## ") + &
			FORMAT$(EMP_TOTAL(4%), "######.## ") + &
			FORMAT$(EMP_TOTAL(5%), "######.## ") + &
			FORMAT$(EMP_TOTAL(6%), "######.## ") + &
			FORMAT$(EMP_TOTAL(7%), "######.## ") + &
			FORMAT$(EMP_TOTAL(8%), "######.## ") + &
			FORMAT$(EMP_TOTAL(9%), "######.##")

		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		GRAND_TOTAL(LOOP%) = GRAND_TOTAL(LOOP%) + EMP_TOTAL(LOOP%) &
			FOR LOOP% = 1% TO 9%
	END IF

	!
	! Try for next record
	!
	GOTO 17020

 ExitTotal:
	!
	! Handle end of report
	!
	TEXT$ = LEFT("Grand Total" + SPACE$(42%), 42%) + &
		FORMAT$(GRAND_TOTAL(1%), "######.## ") + &
		FORMAT$(GRAND_TOTAL(2%), "######.## ") + &
		FORMAT$(GRAND_TOTAL(3%), "######.## ") + &
		FORMAT$(GRAND_TOTAL(4%), "######.## ") + &
		FORMAT$(GRAND_TOTAL(5%), "######.## ") + &
		FORMAT$(GRAND_TOTAL(6%), "######.## ") + &
		FORMAT$(GRAND_TOTAL(7%), "######.## ") + &
		FORMAT$(GRAND_TOTAL(8%), "######.## ") + &
		FORMAT$(GRAND_TOTAL(9%), "######.##")

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
