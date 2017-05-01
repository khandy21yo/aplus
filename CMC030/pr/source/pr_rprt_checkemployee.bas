1	%TITLE "PR Employee Master Dump"
	%SBTTL "PR_RPRT_CHECKEMPLOYEE"
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
	! ID:PR060
	!
	! Abstract:HELP
	!	.p
	!	This program searches for problems in the employee master
	!	file such as missing required fields, and garbage fields.
	!	.P
	!	The problems currently searched for include:
	!	.lm +5
	!	.b
	!	.list 0,"*"
	!	.le
	!	Missing name
	!	.le
	!	Missing city
	!	.le
	!	Missing state
	!	.le
	!	Missing zip code
	!	.le
	!	Missing Social Security Number
	!	.le
	!	Social Security Number is Wrong Length
	!	.le
	!	Badly Formatted Social Security Number
	!	.le
	!	Invalid Social Security Number
	!	.le
	!	Missing GL Account
	!	.le
	!	Missing Pay Frequency
	!	.le
	!	Missing SUI State code
	!	.le
	!	Missing Hire Date
	!	.els
	!	.lm -5
	!
	! Index:
	!	.x Report>Check Employee
	!	.x Check Employee>Report
	!
	! Option:
	!
	! Author:
	!
	!	05/25/93 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_CHECKEMPLOYEE
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_CHECKEMPLOYEE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_CHECKEMPLOYEE.OBJ;*
	!
	! Modification history:
	!
	!	06/11/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/02/93 - Kevin Handy
	!		Added check for workmans comp code.
	!
	!	01/18/94 - Kevin Handy
	!		Added checks for: Address and tax package
	!
	!	02/01/94 - Kevin Handy
	!		Check for invalid SSN's in 729-799, 800-999
	!		ranges (invalid).
	!
	!	02/08/94 - Kevin Handy
	!		Make a SSN of "   -  -    " display "blank" instead
	!		of "wrong length".
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/26/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/17/2000 - Kevin Handy
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

	!
	! Dimension Statements
	!
	DIM PROBLEM$(50%)

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
	!	enters a value which causes the
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
	!	The ^*To Item\* field causes the report to
	!	end with this particular item. A blank field will cause the report
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

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "CHECK EMPLOYEE INFORMATION REPORT"
	TITLE$(2%) = "Payroll System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Empl#      EmployeeName"

	LYT_LINE$ = "$EMPNUM:11,$EMPNAM:42"

	TITLE$(5%) = ""
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

	PROBLEM% = 0%

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

17100	!
	! Look for various problems in master file
	!
	IF PR_EMP_MASTER::EMPNAME = ""
	THEN
		PROBLEM% = PROBLEM% + 1%
		PROBLEM$(PROBLEM%) = "Missing name"
	END IF

	IF (PR_EMP_MASTER::ADD1 = "") AND (PR_EMP_MASTER::ADD2 = "")
	THEN
		PROBLEM% = PROBLEM% + 1%
		PROBLEM$(PROBLEM%) = "Missing address"
	END IF

	IF PR_EMP_MASTER::CITY = ""
	THEN
		PROBLEM% = PROBLEM% + 1%
		PROBLEM$(PROBLEM%) = "Missing city"
	END IF

	IF PR_EMP_MASTER::STATE = ""
	THEN
		PROBLEM% = PROBLEM% + 1%
		PROBLEM$(PROBLEM%) = "Missing state"
	END IF

	IF PR_EMP_MASTER::ZIP = ""
	THEN
		PROBLEM% = PROBLEM% + 1%
		PROBLEM$(PROBLEM%) = "Missing zip code"
	END IF

	IF (PR_EMP_MASTER::SSN = "") OR (PR_EMP_MASTER::SSN = "   -  -")
	THEN
		PROBLEM% = PROBLEM% + 1%
		PROBLEM$(PROBLEM%) = "Missing Social Security Number"
	ELSE
		!
		! Must be aaa-bb-cccc or aaabbcccc
		!
		X% = LEN(TRM$(PR_EMP_MASTER::SSN))
		IF (X% <> 9%) AND (X% <> 11%)
		THEN
			PROBLEM% = PROBLEM% + 1%
			PROBLEM$(PROBLEM%) = &
				"Social Security Number is Wrong Length"
		END IF

		!
		! SSN's of 000, 800 to 999, 729 to 799 are not
		! valid.
		!
		XTEST$ = LEFT(PR_EMP_MASTER::SSN, 3%)
		IF ((XTEST$ >= "800") AND (XTEST$ <= "999")) OR &
			((XTEST$ >= "729") AND (XTEST$ <= "799")) OR &
			(XTEST$ = "000")
		THEN
			PROBLEM% = PROBLEM% + 1%
			PROBLEM$(PROBLEM%) = "Invalid Social Security Number"
		END IF
	END IF

	IF PR_EMP_MASTER::ACCT = ""
	THEN
		PROBLEM% = PROBLEM% + 1%
		PROBLEM$(PROBLEM%) = "Missing GL Account"
	END IF

	IF PR_EMP_MASTER::PAYFREQ = 0%
	THEN
		PROBLEM% = PROBLEM% + 1%
		PROBLEM$(PROBLEM%) = "Missing Pay Frequency"
	END IF

	IF PR_EMP_MASTER::SUI_SW = ""
	THEN
		PROBLEM% = PROBLEM% + 1%
		PROBLEM$(PROBLEM%) = "Missing SUI State code"
	END IF

	IF PR_EMP_MASTER::HIREDAY <= "00000000"
	THEN
		PROBLEM% = PROBLEM% + 1%
		PROBLEM$(PROBLEM%) = "Missing Hire Date"
	END IF

	IF PR_EMP_MASTER::TAX_PKG = ""
	THEN
		PROBLEM% = PROBLEM% + 1%
		PROBLEM$(PROBLEM%) = "Missing Tax Package"
	END IF

	IF PR_EMP_MASTER::WC = ""
	THEN
		PROBLEM% = PROBLEM% + 1%
		PROBLEM$(PROBLEM%) = "Missing Workmans Comp Code"
	END IF

17200	!
	! Games with SSN
	!
	JUNK$ = PR_EMP_MASTER::SSN

	WHEN ERROR IN
		IF INSTR(1%, JUNK$, "-") = 0%
		THEN
			V = VAL(JUNK$)
		ELSE
			IF (MID(JUNK$, 4%, 1%) <> "-") OR (MID(JUNK$,  7%, 1%) <> "-")
			THEN
				PROBLEM% = PROBLEM% + 1%
				PROBLEM$(PROBLEM%) = &
					"Badly Formatted Social Security Number"
			ELSE
				V% = VAL%(MID(JUNK$, 1%, 3%))
				V% = VAL%(MID(JUNK$, 5%, 2%))
				V% = VAL%(RIGHT(JUNK$, 8%))
			END IF
		END IF
	USE
		PROBLEM% = PROBLEM% + 1%
		PROBLEM$(PROBLEM%) = "Bad digit in Social Security Number"
	END WHEN

17210	!

17900	!
	! Print out one line
	!
	IF PROBLEM%
	THEN
		TEXT$ = PR_EMP_MASTER::EMPNUM + " " + &
			PR_EMP_MASTER::EMPNAME + " "

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 5%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		FOR I% = 1% TO PROBLEM%
			TEXT$ = SPACE$(15%) + PROBLEM$(I%)
			CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), &
				TEXT$, 0%)
			GOTO ExitProgram IF UTL_REPORTX::STAT
		NEXT I%

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), "", -1%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

	END IF

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
