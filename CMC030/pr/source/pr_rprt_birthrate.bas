1	%TITLE "PR Employee Birth Date Report"
	%SBTTL "PR_RPRT_BIRTHRATE"
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
	! ID:PR025
	!
	! Abstract:HELP
	!	.p
	!	The ^*Employee Birthdate Report\* option
	!	prints a report which lists employees in
	!	order of date of birth.
	!	.p
	!	The report includes the following column headings:
	!	.b
	!	.ls 0,"o"
	!	.lm +15
	!	.le
	!	#Birthdate
	!	.le
	!	#Employee Number
	!	.le
	!	#Employee Name
	!	.le
	!	#Age (Based on the report date)
	!	.le
	!	#Sex
	!	.le
	!	#Rate
	!	.els
	!
	! Index:
	!	.x Employee Birthdate>Report
	!	.x Report>Employee Birthdate
	!
	! Option:
	!
	! Author:
	!
	!	04/02/91 - Kevin Handy
	!		Taken from PR_RPRT_BIRTH
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_BIRTHRATE
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_BIRTHRATE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_BIRTHRATE.OBJ;*
	!
	! Modification history:
	!
	!	04/24/91 - Kevin Handy
	!		Added employee status (read from PR_EMP_DATES).
	!
	!	05/22/91 - Kevin Handy
	!		Modified to handle TERMDAY more consistantly.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	04/25/96 - Kevin Handy
	!		Reformat source code
	!
	!	04/29/96 - Kevin Handy
	!		Modified to display exemptions.
	!
	!	04/29/96 - Kevin Handy
	!		Modified to be able to sort excluding birth day.
	!
	!	10/25/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/22/2000 - Kevin Handy
	!		Add EFF_DATE parameter to PR_READ_RATE
	!
	!	11/16/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_DATES.HB"

	RECORD PR_TEMP_CDD
		STRING BIRTH = 4%
		STRING BIRTHDAY = 8%
		STRING EMPNUM = 10%
		STRING EMPNAME = 30%
		STRING SORT = 15%
		STRING LOCATION = 4%
		STRING SEX = 1%
		REAL   RATE
		STRING HIREDAY = 8%
		STRING DEPT = 6%
		STRING PAYCODE = 2%
		STRING FSTATUS = 30%
		WORD   EXEMPTIONS
	END RECORD

	MAP (PR_TEMP) PR_TEMP_CDD PR_TEMP

	DECLARE PR_EMP_DATES_CDD CURRENT_DATES

	%PAGE

	ON ERROR GOTO 19000

	CALL ASSG_CHANNEL(PR_TEMP.CH%, STAT%)
	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	.ts 55
	!	^*(01) From Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*From Date\* field causes the printing
	!	to begin with a particular month and day.
	!	.lm -5
	!
	! Index:
	!	.x From Date>Employee Birthdate Report
	!	.x Employee Birthdate Report>From Date
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	.ts 55
	!	^*(02) To Date	MMDD\*
	!	.b
	!	.lm +5
	!	The ^*To Date\* field causes the printing
	!	to end with a particular month and day.
	!	.lm -5
	!
	! Index:
	!	.x To Date>Employee Birthdate Report
	!	.x Employee Birthdate Report>To Date
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)
	!++
	! Abstract:FLD03
	!	.ts 55
	!	^*(03) Sort by	NU, NA, SO, LO\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field enters a code which
	!	causes the report to be sorted in the indicated manner.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*NU\*	Number
	!	.te
	!	^*NA\*	Name
	!	.te
	!	^*SA\*	Birthdate, Alphabetical (last name first)
	!	.te
	!	^*LO\*	Location, Department, Birthdate
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort By>Employee Birthdate Report
	!	.x Employee Birthdate Report>Sort By
	!
	!--

	DATE_TERM_EMP$  = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(3%), 132%))
	!++
	! Abstract:FLD04
	!	^*(04) Exclude to Termination Date\*
	!	.p
	!	The ^*Exclude To Termination Date\* allows for the input of a parameter as to
	!	which terminated employees should be included in the ^*Birthday Report\*. For
	!	example, if the date 051590 were entered, any employees terminated before that
	!	date would not be included.
	!
	! Index:
	!	.x Terimation Date>Exclude to>Employee Birthdate Report
	!	.x Employee Birthdate Report>Termination Date>Exclude to
	!
	! Datatype:DATE
	! Size:8
	!--

	DATE_SORT$ = LEFT$(UTL_REPORTX::OPTDEF(4%), 1%)
	!++
	! Abstract:FLD05
	!	^*(05) Sort By Birth Day\*
	!	.p
	!	This field specifies if the employees should print out
	!	in Birth Day order (Month and day only, not year)
	!	followed by the "Sort By" order,
	!	or in the "Sort By" order only.
	!
	! Index:
	!	.x Birth Day>Employee Birthdate Report
	!	.x Employee Birthdate Report>Birth Day
	!
	!--

	FROM_AGE% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(5%), 132%))
	!++
	! Abstract:FLD06
	!	^*(06) From Age\*
	!	.p
	!	The ^*From Age\* setting causes the printing
	!	to begin with a particular age.
	!	.p
	!	A blank field will cause the report to start with the first item in the file.
	!
	! Index:
	!	.x From Age>Employee Birthdate Report
	!	.x Employee Birthdate Report>From Age
	!
	!--

	TO_AGE% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(6%), 132%))
	!++
	! Abstract:FLD07
	!	^*(07) To Age\*
	!	.p
	!	The ^*To Age\* setting causes the printing
	!	to end with a particular age.
	!	.p
	!	A blank field causes the report to end with the last age in the file.
	!
	! Index:
	!	.x To Age>Employee Birthdate Report
	!	.x Employee Birthdate Report>To Age
	!
	!--

	TO_AGE% = 999% IF TO_AGE% = 0%

300	!
	! Open employee master file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

310	!
	! Open employee status file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STATUS.OPN"
	USE
		FILENAME$ = "PR_EMP_STATUS"
		CONTINUE HelpError
	END WHEN

350	CALL ENTR_3MESSAGE(SCOPE, "Creating work file. ", 17%)

	SELECT SORTBY$
	CASE "NU"
		OPEN UTL_WORK.DEV$ + "PR_TEMP.TMP" FOR OUTPUT &
			AS FILE PR_TEMP.CH%, ORGANIZATION INDEXED FIXED, &
			MAP PR_TEMP, &
			BUFFER 32%, &
			TEMPORARY, &
			PRIMARY KEY (PR_TEMP::BIRTH,PR_TEMP::EMPNUM) DUPLICATES, &
			ACCESS MODIFY, ALLOW NONE

	CASE "NA"
		OPEN UTL_WORK.DEV$ + "PR_TEMP.TMP" FOR OUTPUT &
			AS FILE PR_TEMP.CH%, ORGANIZATION INDEXED FIXED, &
			MAP PR_TEMP, &
			BUFFER 32%, &
			TEMPORARY, &
			PRIMARY KEY (PR_TEMP::BIRTH,PR_TEMP::EMPNAME) DUPLICATES, &
			ACCESS MODIFY, ALLOW NONE

	CASE "SA"
		OPEN UTL_WORK.DEV$ + "PR_TEMP.TMP" FOR OUTPUT &
			AS FILE PR_TEMP.CH%, ORGANIZATION INDEXED FIXED, &
			MAP PR_TEMP, &
			BUFFER 32%, &
			TEMPORARY, &
			PRIMARY KEY (PR_TEMP::BIRTH,PR_TEMP::SORT) DUPLICATES, &
			ACCESS MODIFY, ALLOW NONE

	CASE "LO"
		OPEN UTL_WORK.DEV$ + "PR_TEMP.TMP" FOR OUTPUT &
			AS FILE PR_TEMP.CH%, ORGANIZATION INDEXED FIXED, &
			MAP PR_TEMP, &
			BUFFER 32%, &
			TEMPORARY, &
			PRIMARY KEY (PR_TEMP::LOCATION,PR_TEMP::DEPT,PR_TEMP::BIRTH) DUPLICATES, &
			ACCESS MODIFY, ALLOW NONE
	END SELECT

400	RESET #PR_EMP_MASTER.CH%, KEY #0%

	EFFDAT$ = DATE_TODAY

410	!
	! Get next employee record
	!
	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, REGARDLESS
	USE
		CONTINUE ReportTitle IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	GOTO 410 IF (PR_EMP_MASTER::TERMDAY < DATE_TERM_EMP$) AND &
		(PR_EMP_MASTER::TERMDAY > "00000000")

420	!
	! Get Federal exemptions
	!
	EXEMPT% = 0%
	WHEN ERROR IN
		GET #PR_EMP_STATUS.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM + "FW", &
			REGARDLESS
	USE
		CONTINUE 430
	END WHEN

	EXEMPT% = PR_EMP_STATUS::EXEMPT + PR_EMP_STATUS::ADDEXEMPT

430	TEMP$ = MID(PR_EMP_MASTER::BIRTH, 5%, 4%)

	IF (TEMP$ >= FROM_ITEM$ AND FROM_ITEM$ <> "") AND &
		(TEMP$ <= TO_ITEM$ AND TO_ITEM$   <> "") OR &
		(FROM_ITEM$ = "" AND TO_ITEM$ = "")
	THEN
		CALL PR_READ_DATES(PR_EMP_MASTER::EMPNUM, &
			"AC", DATE_TODAY, 1%, CURRENT_DATES)

		IF (DATE_SORT$ = "N")
		THEN
			PR_TEMP::BIRTH		= ""
		ELSE
			PR_TEMP::BIRTH		= TEMP$
		END IF
		PR_TEMP::EMPNUM		= PR_EMP_MASTER::EMPNUM
		PR_TEMP::EMPNAME	= PR_EMP_MASTER::EMPNAME
		PR_TEMP::SORT		= PR_EMP_MASTER::SORT
		PR_TEMP::LOCATION	= PR_EMP_MASTER::LOCATION
		IF PR_EMP_MASTER::BIRTH = "00000000"
		THEN
			PR_TEMP::BIRTHDAY	= ""
		ELSE
			PR_TEMP::BIRTHDAY	= PR_EMP_MASTER::BIRTH
		END IF
		PR_TEMP::SEX		= PR_EMP_MASTER::SEX
		IF PR_EMP_MASTER::HIREDAY = "00000000"
		THEN
			PR_TEMP::HIREDAY	= ""
		ELSE
			PR_TEMP::HIREDAY	= PR_EMP_MASTER::HIREDAY
		END IF
		PR_TEMP::DEPT		= PR_EMP_MASTER::DEPT
		PR_TEMP::FSTATUS	= CURRENT_DATES::DESCR
		PR_TEMP::EXEMPTIONS	= EXEMPT%

		CALL PR_READ_RATE(PR_EMP_MASTER::EMPNUM, &
			PR_EMP_MASTER::OPER, &
			EFFDAT$, &
			PR_TEMP::PAYCODE, &
			RATE_CODE$, &
			PR_TEMP::RATE, &
			PIECE_RATE, &
			FACTOR%, &
			STDEFF, &
			EVALDATE$, &
			EFF_DATE$)

		PUT #PR_TEMP.CH%
	END IF

	GOTO 410

 ReportTitle:
	!
	! Titles
	!
	TITLE$(1%) = "Employee Birthdate Report"
	TITLE$(2%) = ""

	!
	! Column headings
	!
	TITLE$(3%) = "Employee # " + &
		"Employee Name                  Loc  Dept   Birthdate Age  " + &
		"HireDate     Sex   Code Rate  Exempt Status"
	TITLE$(4%) = ""

	!
	! Line layouts
	!
	LYT_LINE$ = "$Line:132"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	RESET #PR_TEMP.CH%

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_TEMP.CH%
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

	!
	! Print out one line
	!
	THE_AGE% = VAL%(LEFT(DATE_TODAY, 4%)) - &
		VAL%(LEFT(PR_TEMP::BIRTHDAY, 4%))
	THE_AGE% = 0% IF THE_AGE% = VAL%(LEFT(DATE_TODAY, 4%))

	GOTO GetNextRec IF (FROM_AGE%>THE_AGE%) OR (THE_AGE%>TO_AGE%)

	LIN% = 0%
	IF SORTBY$ = "LO"
	THEN
		LIN% = 999% IF (PR_TEMP::LOCATION <> TEST_LOCATION$) AND &
			RUN.LOOP% <> 0%
		TEST_LOCATION$ = PR_TEMP::LOCATION
		RUN.LOOP% = -1%
	END IF

	TEXT$ = PR_TEMP::EMPNUM + " " + &
		PR_TEMP::EMPNAME + " " + &
		PR_TEMP::LOCATION + " " + &
		PR_TEMP::DEPT + " " + &
		PRNT_DATE(PR_TEMP::BIRTHDAY, 8%) + " " + &
		FORMAT$(THE_AGE%, "###  ") + &
		PRNT_DATE(PR_TEMP::HIREDAY, 8%) + "   " + &
		PR_TEMP::SEX + "   " + &
		PR_TEMP::PAYCODE + " " + &
		FORMAT$(PR_TEMP::RATE, "#####.## ") + &
		FORMAT$(PR_TEMP::EXEMPTIONS, "###### ") + &
		PR_TEMP::FSTATUS

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, LIN%)

	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!

 ExitProgram:
	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Close the channel
	!
	CLOSE #PR_TEMP.CH%

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
