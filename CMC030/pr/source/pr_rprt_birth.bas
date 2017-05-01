1	%TITLE "PR Employee Birth Date Report"
	%SBTTL "PR_RPRT_BIRTH"
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
	! ID:PR025
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Employee Birthdate Report\* option
	!	prints a report which lists
	!	employees in order of date of birth.
	!	.b
	!	The report includes the following information:
	!	.table 3,25
	!	.te
	!	Birthdate
	!	.te
	!	Employee Number
	!	.te
	!	Employee Name
	!	.te
	!	Age (Based from the report date)
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Employee Birthdate>Report
	!	.x Report>Employee Birthdate
	!
	! Option:
	!
	! Author:
	!
	!	12/07/87 - B. Craig Larsen
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_BIRTH
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_BIRTH, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_BIRTH.OBJ;*
	!
	! Modification history:
	!
	!	04/05/89 - J. Shad Rydalch
	!		Added the "FROM AGE" and "TO AGE" options.
	!
	!	06/15/90 - Aaron Redd
	!		Added line layouts so that the report could be
	!		sent to spreadsheets or DIF files.
	!
	!	05/22/91 - Kevin Handy
	!		Modified to handle TERMDAY more consistantly.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	10/02/95 - Kevin Handy
	!		Modified to have SO sort (By last name)
	!
	!	10/02/95 - Kevin Handy
	!		Modified so that sortby has a default case.
	!
	!	09/10/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/29/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/19/2000 - Kevin Handy
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
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	MAP (PR_TEMP) &
		PR_TEMP.BIRTH$ = 4%, &
		PR_TEMP.BIRTHDAY$ = 8%, &
		PR_TEMP.EMPNUM$ = 10%, &
		PR_TEMP.EMPNAME$ = 30%, &
		PR_TEMP.SORT$ = 15%, &
		PR_TEMP.LOC$ = 4%

	%PAGE

	ON ERROR GOTO 19000

	CALL ASSG_CHANNEL(PR_TEMP.CH%, STAT%)
	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	.ts 55
	!	^*(01) From Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*From Date\* field causes the printing
	!	to begin with a particular date.
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
	!	^*(02) To Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*To Date\* field causes the printing
	!	to end with a particular date.
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
	!	^*(03) Sort by	NU,NA,SO,LO\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field enters a code which will
	!	cause the report to be sorted in the indicated manner.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*NU\*	Birthdate, Employee Number
	!	.te
	!	^*NA\*	Birthdate, Employee Name
	!	.te
	!	^*SA\*	Birthdate, Alphabetical (last name first)
	!	.te
	!	^*SO\*	Alphabetical (last name first)
	!	.te
	!	^*LO\*	Location, Birthdate
	!	.end table
	!	An entry is required in this field
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
	!	.ts 55
	!	^*(04) Exclude to Termination Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*Exclude to Termination Date\* inputs a parameter as to
	!	which terminated employees should be included in the ^*Birthday Report\*.
	!	.b
	!	For example, if the date 051590 were entered, any employees terminated
	!	before that date would not be included.
	!	.lm -5
	!
	! Index:
	!	.x Terimation Date>Exclude to>Employee Birthdate Report
	!	.x Employee Birthdate Report>Termination Date>Exclude to
	!
	!--

	FROM_AGE% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(5%), 132%))
	!++
	! Abstract:FLD06
	!	^*(06) From Age\*
	!	.b
	!	.lm +5
	!	The ^*From Age\* field cause the printing
	!	to begin with a particular age.
	!	.b
	!	A blank field will cause the report to start with the first item in the file.
	!	.lm -5
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
	!	.b
	!	.lm +5
	!	The ^*To Age\* field causes the printing
	!	to end with a particular age.
	!	.b
	!	A blank field causes the report to end with the last age in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Age>Employee Birthdate Report
	!	.x Employee Birthdate Report>To Age
	!
	!--

	TO_AGE% = 999% IF TO_AGE% = 0%

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

310	CALL ENTR_3MESSAGE(SCOPE, "Creating work file. ", 17%)

	SELECT SORTBY$
	CASE "NU"
		OPEN UTL_WORK.DEV$ + "PR_TEMP.TMP" FOR OUTPUT &
			AS FILE PR_TEMP.CH%, ORGANIZATION INDEXED FIXED, &
			MAP PR_TEMP, &
			BUFFER 32%, &
			TEMPORARY, &
			PRIMARY KEY (PR_TEMP.BIRTH$, PR_TEMP.EMPNUM$) DUPLICATES, &
			ACCESS MODIFY, ALLOW NONE
	CASE "SA"
		OPEN UTL_WORK.DEV$ + "PR_TEMP.TMP" FOR OUTPUT &
			AS FILE PR_TEMP.CH%, ORGANIZATION INDEXED FIXED, &
			MAP PR_TEMP, &
			BUFFER 32%, &
			TEMPORARY, &
			PRIMARY KEY (PR_TEMP.BIRTH$, PR_TEMP.SORT$) DUPLICATES, &
			ACCESS MODIFY, ALLOW NONE
	CASE "SO"
		OPEN UTL_WORK.DEV$ + "PR_TEMP.TMP" FOR OUTPUT &
			AS FILE PR_TEMP.CH%, ORGANIZATION INDEXED FIXED, &
			MAP PR_TEMP, &
			BUFFER 32%, &
			TEMPORARY, &
			PRIMARY KEY (PR_TEMP.SORT$) DUPLICATES, &
			ACCESS MODIFY, ALLOW NONE
	CASE "LO"
		OPEN UTL_WORK.DEV$ + "PR_TEMP.TMP" FOR OUTPUT &
			AS FILE PR_TEMP.CH%, ORGANIZATION INDEXED FIXED, &
			MAP PR_TEMP, &
			BUFFER 32%, &
			TEMPORARY, &
			PRIMARY KEY (PR_TEMP.LOC$, PR_TEMP.BIRTH$) DUPLICATES, &
			ACCESS MODIFY, ALLOW NONE
	CASE ELSE
		OPEN UTL_WORK.DEV$ + "PR_TEMP.TMP" FOR OUTPUT &
			AS FILE PR_TEMP.CH%, ORGANIZATION INDEXED FIXED, &
			MAP PR_TEMP, &
			BUFFER 32%, &
			TEMPORARY, &
			PRIMARY KEY (PR_TEMP.BIRTH$, PR_TEMP.EMPNAME$) DUPLICATES, &
			ACCESS MODIFY, ALLOW NONE
	END SELECT

400	RESET #PR_EMP_MASTER.CH%, KEY #0%

410	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, REGARDLESS
	USE
		CONTINUE ReportTitle IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	GOTO 410 IF (PR_EMP_MASTER::TERMDAY < DATE_TERM_EMP$) AND &
		(PR_EMP_MASTER::TERMDAY > "000000")

	TEMP$ = MID(PR_EMP_MASTER::BIRTH, 5%, 4%)

	IF (TEMP$ >= FROM_ITEM$ AND FROM_ITEM$ <> "") AND &
		(TEMP$ <= TO_ITEM$ AND TO_ITEM$ <> "") OR &
		(FROM_ITEM$ = "" AND TO_ITEM$ = "")
	THEN
		PR_TEMP.BIRTH$ = TEMP$
		PR_TEMP.EMPNUM$ = PR_EMP_MASTER::EMPNUM
		PR_TEMP.EMPNAME$ = PR_EMP_MASTER::EMPNAME
		PR_TEMP.SORT$ = PR_EMP_MASTER::SORT
		PR_TEMP.LOC$ = PR_EMP_MASTER::LOCATION
		PR_TEMP.BIRTHDAY$ = PR_EMP_MASTER::BIRTH

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
	TITLE$(3%) = "Birthdate           Employee # " + &
		"Employee Name                  Loc  Age"
	TITLE$(4%) = ""

	!
	! Line layouts
	!
	LYT_LINE$ = "DBirthDate:010,$EmpNum:030,$EmpName:061," + &
		"$Location:066,VPresentAge:070"

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
		VAL%(LEFT(PR_TEMP.BIRTHDAY$, 4%))
	THE_AGE% = 0% IF THE_AGE% = VAL%(LEFT(DATE_TODAY, 4%))

	GOTO GetNextRec IF (FROM_AGE% > THE_AGE%) OR (THE_AGE% > TO_AGE%)

	LIN% = 0%
	IF SORTBY$ = "LO"
	THEN
		LIN% = 999% IF PR_TEMP.LOC$ <> TEST_LOCATION$ AND &
			RUN_LOOP% <> 0%
		TEST_LOCATION$ = PR_TEMP.LOC$
		RUN_LOOP% = -1%
	END IF

	TEXT$ = PRNT_DATE(PR_TEMP.BIRTHDAY$, 8%) + SPACE$(10%) + &
		PR_TEMP.EMPNUM$ + " " + &
		PR_TEMP.EMPNAME$ + " " + &
		PR_TEMP.LOC$ + " " + &
		FORMAT$(THE_AGE%, "###")

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
