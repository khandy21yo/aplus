1	%TITLE "PR Employee Master Dump"
	%SBTTL "PR_RPRT_EMP_DUMP"
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
	! ID:PR060
	!
	! Abstract:HELP
	!	.p
	!	The ^*Employee File Dump\* option
	!	accesses the print routine which will print an
	!	employee list.
	!	.p
	!	A short form or long form report may be printed. A short form
	!	includes employee number and employee name only for each employee
	!	listed. A long form includes every field which is contained in pages
	!	1 and 2 of the employee master file for each employee listed.
	!	.p
	!	The report may be listed in alphabetical or employee number order and
	!	includes the following fields:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Employee Number
	!	.le
	!	Employee Name
	!	.le
	!	Address One
	!	.le
	!	Address Two
	!	.le
	!	Alpa Sort
	!	.le
	!	Location
	!	.le
	!	Department
	!	.le
	!	City
	!	.le
	!	Birthdate
	!	.le
	!	State
	!	.le
	!	Social Security Number
	!	.le
	!	Zip
	!	.le
	!	Start Date
	!	.le
	!	Country
	!	.le
	!	Phone Number
	!	.le
	!	Termination Date
	!	.le
	!	Account Number
	!	.els
	!
	! Index:
	!	.x Employee File Dump>Report
	!	.x Employee>List
	!	.x Report>Employee File Dump
	!
	! Option:
	!
	! Author:
	!
	!	12/07/87 - B. Craig Larsen
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_EMP_DUMP
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_EMP_DUMP, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_EMP_DUMP.OBJ;*
	!
	! Modification history:
	!
	!	04/19/89 - Kevin Handy
	!		Added LYT_LINE$ code for short form.
	!
	!	12/12/90 - Kevin Handy
	!		Added additional exemption to report.
	!
	!	05/22/91 - Kevin Handy
	!		Modified to handle TERMDAY more consistantly.
	!
	!	04/15/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	06/27/95 - Kevin Handy
	!		Removed 1099 flag from printout.
	!
	!	08/26/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/16/97 - Kevin Handy
	!		Reformat source code.
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/05/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STATUS.HB"
	MAP	(PR_EMP_STATUS)	PR_EMP_STATUS_CDD	PR_EMP_STATUS

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_RATE.HB"
	MAP	(PR_EMP_RATE)	PR_EMP_RATE_CDD	PR_EMP_RATE

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STD_ERNDED.HB"
	MAP (PR_EMP_STD_ERNDED) PR_EMP_STD_ERNDED_CDD PR_EMP_STD_ERNDED

	%PAGE

	ON ERROR GOTO 19000


 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.p
	!	The ^*From Item\* field
	!	enters a value which will cause the
	!	report to begin with a selected item. The item must be in agreement
	!	with the specific type of sort selected, i.e., if the selection is
	!	made to print the report in employee number order, a value in this
	!	field must be an employee number; or if the selection is made to
	!	print the report in alphabetical order, the value in this field
	!	must be a name, last name first.
	!	.p
	!	Refer to field (03) Order of the Employee File Dump report
	!	setting screen for more information on sort choices.
	!
	! Index:
	!	.x From Item>Employee File Dump
	!	.x Employee File Dump>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.p
	!	The ^*To Item\* field causes the report to print
	!	ending with this particular item. A blank field will cause the report
	!	to end with the last item in the file.
	!
	! Index:
	!	.x To Item>Employee File Dump
	!	.x Employee File Dump>To Item
	!
	!--
	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Sort (NU,NA,SN,LO,SO)\*
	!	.p
	!	The ^*Sort\* field determines the order in
	!	which the report will print.
	!	.p
	!	The valid values and related sort orders are:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	NU = Number
	!	.le
	!	NA = Name
	!	.le
	!	SN = Social Security Number
	!	.le
	!	LO = Location, Department, Work Center, Alpha
	!	.le
	!	SO = Alphabetical (last name first)
	!	.els
	!	.lm -10
	!	.p
	!	This field requires an entry. Only the codes listed above are
	!	valid.
	!
	! Index:
	!	.x Sort>Employee File Dump
	!	.x Employee File Dump>Sort
	!
	!--

	FORMTYPE$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Form Type (L/S)\*
	!	.p
	!	The ^*Form Type\* field allows for choice of the long or short report. A
	!	^*L\* indicates a long form report and ^*S\* indicates the short form report.
	!
	! Index:
	!	.x Form Type>Employee File Dump
	!	.x Employee File Dump>Form Type
	!
	! Datatype:TEXT
	! Size:1
	! Valid Input: L,S,l,s
	!--

	FROMDATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(5%), -1%))
	!++
	! Abstract:FLD06
	!	^*(06) From Hire Date\*
	!	.p
	!	The ^*From Hire Date\* field causes the printing
	!	to begin with a particular Hire Date. A blank field will cause the
	!	report to start with the first Hire Date in the file.
	!
	! Index:
	!	.x From Hire Date>Employee File Dump
	!	.x Employee File Dump>From Hire Date
	!
	!--

	TODATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(6%), -1%))
	!++
	! Abstract:FLD07
	!	^*(07) To Hire Date\*
	!	.p
	!	The ^*To Hire Date\* field causes the printing
	!	to end with a particular hire date. A blank field causes the report
	!	to end with the last hire date in the file.
	!
	! Index:
	!	.x To Hire Date>Employee File Dump
	!	.x Employee File Dump>To Hire Date
	!
	!--

	INCLTERM$ = EDIT$(UTL_REPORTX::OPTDEF(7%), -1%)
	!++
	! Abstract:FLD08
	!	^*(08) Include Terminated\*
	!	.p
	!	The ^*Include Terminated\* field allows for choice as to whether the report
	!	will include the terminated employees. A ^*N\* response will not include
	!	those terminated while a ^*Y\* response will include the terminated.
	!
	! Index:
	!	.x Terminated>Employee File Dump
	!	.x Employee File Dump>Terminated
	!
	! Datatype:TEXT
	! Size:1
	! Required:Y
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
	! Open file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STATUS.OPN"
	USE
		FILENAME$ = "PR_EMP_STATUS"
		CONTINUE HelpError
	END WHEN

320	!
	! Open file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_RATE.OPN"
	USE
		FILENAME$ = "PR_EMP_RATE"
		CONTINUE HelpError
	END WHEN

330	!
	! Open file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STD_ERNDED.OPN"
	USE
		FILENAME$ = "PR_EMP_STD_ERNDED"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "EMPLOYEE DUMP REPORT"
	TITLE$(2%) = "Payroll System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Empl#      EmployeeName                   " + &
		"AlfaSort       Loc  Dept   BirthDate  SSN          " + &
		"StartDate  TermDate   Account#"

	LYT_LINE$ = "$EMPNUM:11,$EMPNAM:42,$ALPSRT:57,$LOCAT:62," + &
		"$DEPT:69,$BIRTHDATE$:80,$SSN:92,$STARTDATE$:103" + &
		"$TERMDATE:114,$ACCT$:133"

	TITLE$(5%) = SPACE$(15%) + "Address1              Address2" + &
		"              City              St ZIP        " + &
		"Country Phone"


	TITLE$(6%) = "."

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

	GOTO GetNextRec IF PR_EMP_MASTER::HIREDAY < FROMDATE$ OR &
		(PR_EMP_MASTER::HIREDAY > TODATE$ AND TODATE$ <> "")

	GOTO GetNextRec IF (PR_EMP_MASTER::TERMDAY > "00000000") AND &
		(INCLTERM$ = "N")

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
	CASE "LO"
		GOTO ExitTotal IF (PR_EMP_MASTER::LOCATION > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE ELSE
		GOTO ExitTotal IF (PR_EMP_MASTER::SORT > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	END SELECT

	!
	! Print out one line
	!
	TEXT$ = PR_EMP_MASTER::EMPNUM + " " + &
		PR_EMP_MASTER::EMPNAME + " " + &
		PR_EMP_MASTER::SORT + " " + &
		PR_EMP_MASTER::LOCATION + " " + &
		PR_EMP_MASTER::DEPT + " " + &
		PRNT_DATE(PR_EMP_MASTER::BIRTH, 8%) + " " + &
		PR_EMP_MASTER::SSN + " " + &
		PRNT_DATE(PR_EMP_MASTER::HIREDAY, 8%) + " " + &
		PRNT_DATE(PR_EMP_MASTER::TERMDAY, 8%) + " " + &
		PR_EMP_MASTER::ACCT

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 1%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = PR_EMP_MASTER::EMPNUM + SPACE$(5%) + &
		PR_EMP_MASTER::ADD1 + "  " + &
		PR_EMP_MASTER::ADD2 + "  " + &
		PR_EMP_MASTER::CITY + "  " + &
		PR_EMP_MASTER::STATE + "  " + &
		PR_EMP_MASTER:: ZIP + " " + &
		PR_EMP_MASTER::COUNTRY + "  " + &
		PRNT_PHONE(PR_EMP_MASTER::PHONE, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO PrintBlank IF FORMTYPE$ <> "L"

	!
	! For long form only
	!
	TEXT$ = PR_EMP_MASTER::EMPNUM + SPACE$(15%) + &
		"SPEC:.......... F S R C WorkPermit      HCtry  " + &
		"PF Trade  Operat   Un WCtr Skill  GR D UE TP Act"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)

	TEXT$ = PR_EMP_MASTER::EMPNUM + SPACE$(31%) + &
		PR_EMP_MASTER::REHIRE_FLAG + " " + &
		PR_EMP_MASTER::SEX + " " + &
		PR_EMP_MASTER::RACE + " " + &
		PR_EMP_MASTER::USCIT + " " + &
		PR_EMP_MASTER::WRKPERMIT + " " + &
		PR_EMP_MASTER::HOMCNTRY + " " + &
		FORMAT$(PR_EMP_MASTER::PAYFREQ, "##") + " " + &
		PR_EMP_MASTER::TRADE + " " + &
		PR_EMP_MASTER::OPER + " " + &
		PR_EMP_MASTER::UNION + " " + &
		PR_EMP_MASTER::WORK_CENTER + " " + &
		PR_EMP_MASTER::EMP_SKILL + " " + &
		PR_EMP_MASTER::EMP_GRADE + " " + &
		PR_EMP_MASTER::DISABLED + " " + &
		PR_EMP_MASTER::SUI_SW + " " + &
		PR_EMP_MASTER::TAX_PKG + " " + &
		PR_EMP_MASTER::ACTIVE_FLAG

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

 StatusTop:
17040	!
	! Status loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		FIND #PR_EMP_STATUS.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE RateTop
	END WHEN

	STATUS_FLAG% = -1%
	TEXT$, TEXT1$ = ""

 StatusNext:
	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_EMP_STATUS.CH%, REGARDLESS
	USE
		CONTINUE RateTop
	END WHEN

	GOTO RateTop IF PR_EMP_STATUS::EMPNUM <> PR_EMP_MASTER::EMPNUM

	IF STATUS_FLAG%
	THEN
		TEXT$ = PR_EMP_MASTER::EMPNUM + SPACE$(15%) + &
			"STATUS:........ Type Code Status Exemption" + &
			"    Type Code Status Exemption"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)
		STATUS_FLAG% = 0%
	END IF

	TEXT$ = TEXT1$ + &
		PR_EMP_STATUS::STTYPE + "   " + &
		PR_EMP_STATUS::CODE + "   " + &
		PR_EMP_STATUS::STSTATUS + "       " + &
		FORMAT$(PR_EMP_STATUS::EXEMPT, "###") + "  " + &
		FORMAT$(PR_EMP_STATUS::ADDEXEMPT, "###")

	IF TEXT1$ <> ""
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		TEXT1$ = ""
	ELSE
		TEXT1$ = PR_EMP_MASTER::EMPNUM + SPACE$(31%) + TEXT$
	END IF

	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO StatusNext

 RateTop:

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%) &
		IF TEXT1$ <> ""

17080	!
	! Rate loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		FIND #PR_EMP_RATE.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE ErndedTop
	END WHEN

	RATE_FLAG% = -1%

 RateNext:
	!
	! Get next record
	!
	GET #PR_EMP_RATE.CH%, REGARDLESS

	GOTO ErndedTop IF PR_EMP_RATE::EMPNUM <> PR_EMP_MASTER::EMPNUM

	IF RATE_FLAG%
	THEN
		TEXT$ = PR_EMP_MASTER::EMPNUM + SPACE$(15%) + &
			"RATE:.......... Oper     EffDate     Type Code Hr Rate    " + &
			"Piece   OT Percent  EffRate EvalDate"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
		RATE_FLAG% = 0%
	END IF

	TEXT$ = PR_EMP_MASTER::EMPNUM + SPACE$(31%) + &
		PR_EMP_RATE::OPER + " " + &
		PRNT_DATE(PR_EMP_RATE::EFFDAT, 8%) + "  " + &
		PR_EMP_RATE::RATE_TYPE + "    " + &
		PR_EMP_RATE::RATE_CDE + "  " + &
		FORMAT$(PR_EMP_RATE::HOUR_RATE, "####.###") + " " + &
		FORMAT$(PR_EMP_RATE::PIECE_RATE, "###.####") + "      " + &
		FORMAT$(PR_EMP_RATE::FACTOR, "###%") + "    " + &
		FORMAT$(PR_EMP_RATE::STDEFF, "####.##%") + " " + &
		PRNT_DATE(PR_EMP_RATE::EVAL_DATE, 8%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO RateNext

 ErndedTop:
17120	!
	! Ernded loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		FIND #PR_EMP_STD_ERNDED.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE PrintBlank
	END WHEN

	ERNDED_FLAG% = -1%

 ErndedNext:
	!
	! Get next record
	!
	GET #PR_EMP_STD_ERNDED.CH%, REGARDLESS

	GOTO PrintBlank IF PR_EMP_STD_ERNDED::EMPNUM <> PR_EMP_MASTER::EMPNUM

	IF ERNDED_FLAG%
	THEN
		TEXT$ = PR_EMP_MASTER::EMPNUM+ SPACE$(15%) + &
		"STD ERNDED:.... Type Code      Rate     Limit   To Date   " + &
			"Accrued End Date     Freq M User"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
		ERNDED_FLAG% = 0%
	END IF

	TEXT$ = PR_EMP_MASTER::EMPNUM + SPACE$(31%) + &
		PR_EMP_STD_ERNDED::RTYPE + "    " + &
		PR_EMP_STD_ERNDED::CODE + "   " + &
		FORMAT$(PR_EMP_STD_ERNDED::RATE, "#####.###") + " " + &
		FORMAT$(PR_EMP_STD_ERNDED::LIMIT, "######.##") + " " + &
		FORMAT$(PR_EMP_STD_ERNDED::CTDBAL, "######.##") + " " + &
		FORMAT$(PR_EMP_STD_ERNDED::ACCRUED, "######.##") + " " + &
		PRNT_DATE(PR_EMP_STD_ERNDED::ENDDAT, 8%) + " " + &
		PR_EMP_STD_ERNDED::FREQ + " " + &
		PR_EMP_STD_ERNDED::METHOD + " " + &
		LEFT(PR_EMP_STD_ERNDED::USERDEF, 9%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO ErndedNext

 PrintBlank:
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!

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
