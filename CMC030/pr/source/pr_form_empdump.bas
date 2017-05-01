1	%TITLE "Employee Labels"
	%SBTTL "PR_FORM_EMPDUMP"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1992 BY
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho.  83402
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
	! ID:PREDMP
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Employee Labels\* provides the option for printing
	!	labels containing information for each employee.
	!	.lm -5
	!	.lm -5
	!
	! Index:
	!	.x Employee Labels>Report
	!	.x Report>Employee Labels
	!	.x Labels>Report
	!	.x Report>Labels
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_FORM_EMPDUMP
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_FORM_EMPDUMP, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_FORM_EMPDUMP.OBJ;*
	!
	! Author:
	!
	!	04/14/92 - Kevin Handy
	!		Taken from PR_FORM_LABEL with massive modifications.
	!
	! Modification history:
	!
	!	04/28/92 - Kevin Handy
	!		Clean up (check)
	!
	!	06/16/92 - Kevin Handy
	!		Modified to use OUTP_INITFORM function.
	!
	!	08/13/92 - Kevin Handy
	!		Added Ability to access individual quarters
	!		instead of just this qtr and ytd.
	!
	!	04/13/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/29/94 - Kevin Handy
	!		Added ability to access PR_EMP_STD_ERNDED::METHOD
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/28/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING(...,ASCII(" ")) to "" in several places.
	!
	!	09/09/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/10/97 - Kevin Handy
	!		Lose PRNT.CH% variable
	!
	!	09/09/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/26/98 - Kevin Handy
	!		Don't erase SMG_SCREEN_DATA%, since it is
	!		never created.
	!
	!	06/09/99 - Kevin Handy
	!		Lose line 565 (Dead code)
	!
	!	12/04/2000 - Kevin Handy
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

	!
	! Define maps
	!
	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_DEPARTMENT.HB"
	MAP	(UTL_DEPARTMENT)	UTL_DEPARTMENT_CDD	UTL_DEPARTMENT

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_RATE.HB"
	MAP	(PR_EMP_RATE)		PR_EMP_RATE_CDD	PR_EMP_RATE

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STD_ERNDED.HB"
	MAP	(PR_EMP_STD_ERNDED)	PR_EMP_STD_ERNDED_CDD	PR_EMP_STD_ERNDED

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STATUS.HB"
	MAP	(PR_EMP_STATUS)		PR_EMP_STATUS_CDD	PR_EMP_STATUS

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.HB"
	MAP	(PR_REG_ERNDED)		PR_REG_ERNDED_CDD	PR_REG_ERNDED

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.HB"
	MAP	(PR_REG_TAXES)		PR_REG_TAXES_CDD	PR_REG_TAXES

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]FORM_GROUP.HB"
	DIM			FORM_GROUP_CDD		FORM_GROUP(20%)
			! Maximum of 10 groups

	MAP (TRANS_FORM) &
		HOUR_RATE, &
		PIECE_RATE, &
		FACTOR%, &
		STDEFF, &
		PAGE_COUNT, &
		REPDATE$, &
		PR_EMP_MASTER.ADDLINE$(3%) = 50%, &
		QTR%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION OUTP_FORMINIT
	EXTERNAL LONG   FUNCTION OUTP_FORMPRINT
	EXTERNAL LONG   FUNCTION OUTP_INITFORM

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	REPORT$ = "PREDMP"

	!
	! Look up device
	!
	CALL READ_DEVICE("PR_FORM", PR_FORM.DEV$, STAT%)

	!***************************************************************
	! Open all of the files
	!***************************************************************

	!
	! Open Payroll Employee master file
	!
300	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	!
	! Open rate file
	!
305	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_RATE.OPN"
	USE
		CONTINUE 310
	END WHEN

	!
	! Open Status File
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STATUS.OPN"
	USE
		CONTINUE 315
	END WHEN

	!
	! Open ERNDED File
	!
315	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STD_ERNDED.OPN"
	USE
		CONTINUE 320
	END WHEN

	!
	! Open Department file
	!
320	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_DEPARTMENT.OPN"
	USE
		CONTINUE 330 IF ERR = 5%
		FILENAME$ = "UTL_DEPARTMENT"
		CONTINUE HelpError
	END WHEN

	!
	! Open REPORT file
	!
330	GOTO ExitProgram &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "") <> CMC$_NORMAL

	!
	! Set Page Length
	!
	FORM_GROUP(FRM_LENGTH%)::NUMBER = UTL_REPORTX::PAGELEN

	!
	! Get stuff from report screen
	!

	REPDATE$ = UTL_REPORTX::REPDATE

	SHOW_TERM$ = LEFT(EDIT$(UTL_REPORTX::OPTDEF(0%), 132%), 1%)
	!++
	!
	! Abstract:FLD01
	!	.x Print Terminated>Employee Labels Report
	!	^*(01) Print Terminated\*
	!	.B
	!	.LM +5
	!	The ^*Print Terminated\* field will cause
	!	the printing of terminated employees if the value is set to "^*Yes\*",
	!	otherwise they will not be printed.
	!	.lm -5
	!
	! Index:
	!	.x Employee Labels Report>Print Terminated
	!	.x Terminated>Employee Labels Report
	!	.x Employee Labels Report>Terminated
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	!++
	!
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes the printing
	!	to begin with a particular item.
	!	.b
	!	A blank field will cause the report to start with the first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Employee Labels Report
	!	.x Employee Labels Report>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)
	!++
	!
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes printing
	!	to end with a particular item.
	!	.b
	!	A blank field will cause the report to end with the last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Employee Labels Report
	!	.x Employee Labels Report>To Item
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)
	!++
	!
	! Abstract:FLD04
	!	.x Sort>Employee Labels Report
	!	^*(04) Sort\*
	!	.b
	!	.lm +5
	!	The ^*Sort\* field enters a code which causes the
	!	report to be sorted in the indicated manner.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*NU\*  - Number
	!	.te
	!	^*NA\*  - Name
	!	.te
	!	^*SO\*  - Alphabetical (last name first)
	!	.te
	!	^*LO\*  - Location
	!	.end table
	!	An entry is required in this field.
	!
	! Index:
	!	.x Employee Labels Report>Sort
	!
	!--

	DEPT$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)
	!++
	!
	! Abstract:FLD05
	!	^*(05) Department\*
	!	.b
	!	.lm +5
	!	The ^*Department\* field signifies which department is to be examined by
	!	inserting the department number. If all departments are to be examined, leave
	!	the field blank or insert an _*.
	!	.lm -5
	!
	! Index:
	!	.x Department>Employee Labels Report
	!	.x Employee Labels Report>Department
	!
	!--

	REPORT$ = REPORT$ + "$" + TRM$(UTL_REPORTX::OPTDEF(5%))
	!++
	!
	! Abstract:FLD06
	!	^*(06) Form\*
	!	.b
	!	.lm +5
	!	The ^*Form\* can vary to fit the need of the user. For example,
	!	the report for the factory workers may include the pay rate. Yet the form
	!	for the executives would be one not to include the pay rate.
	!	.lm -5
	!
	! Index:
	!	.x Form>Employee Labels Report
	!	.x Employee Labels Report>Form
	!
	!--

	YYYY$ = EDIT$(UTL_REPORTX::OPTDEF(6%), -1%)
	!++
	!
	! Abstract:FLD07
	!	^*(07) Year\*
	!
	! Index:
	!	.x Year>Employee Labels Report
	!	.x Employee Labels Report>Year
	!
	!--

	QTR% = VAL%(UTL_REPORTX::OPTDEF(7%)) - 1%
	!++
	!
	! Abstract:FLD08
	!	^*(08) Quarter\*
	!
	! Index:
	!	.x Quarter>Employee Labels Report
	!	.x Employee Labels Report>Quarter
	!
	!--

	!
	! Quarterly Pay File
	!
530	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.OPN"
	USE
		CONTINUE 535
	END WHEN

	!
	! Quarterly Tax file
	!
535	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.OPN"
	USE
		CONTINUE 540
	END WHEN

540	!
	! Decide how to sort through file
	!
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

	CASE "LN"
		!
		! Sort Payroll Employee master file
		!
550		CALL ENTR_3MESSAGE(SCOPE, "Creating temporary file", 17%)
		CALL ASSG_CHANNEL(PR_EMP_MASTER_TEMP.CH%, STAT%)

		OPEN "TEMP.TEMP" FOR OUTPUT AS FILE PR_EMP_MASTER_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP PR_EMP_MASTER, &
			BUFFER 32%, &
			TEMPORARY, &
			PRIMARY KEY &
			( &
				PR_EMP_MASTER::LOCATION, &
				PR_EMP_MASTER::DEPT, &
				PR_EMP_MASTER::WORK_CENTER, &
				PR_EMP_MASTER::EMPNUM &
			), &
			ACCESS MODIFY, ALLOW NONE

		RESET #PR_EMP_MASTER.CH%

560		GET #PR_EMP_MASTER.CH%,REGARDLESS
		PUT #PR_EMP_MASTER_TEMP.CH%
		GOTO 560

 !565		PR_EMP_MASTER.CH% = PR_EMP_MASTER_TEMP.CH%
 !		K_NUM% = 0%

	CASE ELSE
		K_NUM% = 2%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::SORT))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::SORT))

	END SELECT

	!
	! Restore original values for the help message
	!
	SCOPE::PRG_IDENT = TEMP_IDENT$
	SCOPE::PRG_PROGRAM = TEMP_PROGRAM$

590	!
	! Load in the form
	!
	GOSUB LoadForm

	!
	! GOTO alignment routine
	!
	GOSUB Alignment

	%PAGE

2000	!*******************************************************************
	! Read through Payroll master file
	!*******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PR_EMP_MASTER.CH%, KEY #K_NUM%
		ELSE
			FIND #PR_EMP_MASTER.CH%, KEY #K_NUM% GE FROM_ITEM$
		END IF
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	PAGE_COUNT = 1%
	UTL_REPORTX::LINENO = 0%
	TEST_COUNT% = 0%

2010	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	IF TO_ITEM$ <> ""
	THEN
		SELECT SORTBY$

		CASE "NU"
			GOTO ExitTotal IF PR_EMP_MASTER::EMPNUM > TO_ITEM$

		CASE "NA"
			GOTO ExitTotal IF PR_EMP_MASTER::EMPNAME > TO_ITEM$

		CASE "SN"
			GOTO ExitTotal IF PR_EMP_MASTER::SSN > TO_ITEM$

		CASE "LO"
			GOTO ExitTotal IF PR_EMP_MASTER::LOCATION > TO_ITEM$

		CASE "LN"

		CASE ELSE
			GOTO ExitTotal IF PR_EMP_MASTER::SORT > TO_ITEM$

		END SELECT
	END IF

	!
	! Is the employee terminated?
	!
	IF SHOW_TERM$ <> "Y"
	THEN
		GOTO 2010 IF PR_EMP_MASTER::TERMDAY > "00000000"
	END IF

	!
	! Is employee in the specified department?
	!
	GOTO 2010 IF (COMP_STRING(PR_EMP_MASTER::DEPT, DEPT$) = 0%) &
		AND (DEPT$ <> "")

	!
	! Is employee in the correct state?
	!
	GOTO 2010 IF (COMP_STRING(PR_EMP_MASTER::STATE, STATE$) = 0%) &
		AND (STATE$ <> "")

	!
	! Create an address line format that reduces white space
	!
	I% = 0%

	IF EDIT$(PR_EMP_MASTER::ADD1, -1%) <> ""
	THEN
		I% = I% + 1%
		PR_EMP_MASTER.ADDLINE$(I%) = &
			EDIT$(PR_EMP_MASTER::ADD1, 8% + 16% + 32% + 128%)
	END IF

	IF EDIT$(PR_EMP_MASTER::ADD2, -1%) <> ""
	THEN
		I% = I% + 1%
		PR_EMP_MASTER.ADDLINE$(I%) = &
			EDIT$(PR_EMP_MASTER::ADD2, 8% + 16% + 32% + 128%)
	END IF

	I% = I% + 1%
	PR_EMP_MASTER.ADDLINE$(I%) = &
		EDIT$(EDIT$(PR_EMP_MASTER::CITY, 128%) + ", " + &
		PR_EMP_MASTER::STATE + " " + PR_EMP_MASTER::ZIP + " " + &
		PR_EMP_MASTER::COUNTRY, 8% + 16% + 32% + 128%)

	PR_EMP_MASTER.ADDLINE$(LOOP%) = "" &
		FOR LOOP% = I% + 1% TO 3%	! Blank 'em out

	!
	! Get the coresponding UTL_DEPARTMENT record for the employee.
	!
	UTL_DEPARTMENT::SUPERVISOR = ""

2050	WHEN ERROR IN
		GET #UTL_DEPARTMENT.CH%, &
			KEY #0% EQ PR_EMP_MASTER::LOCATION + &
			PR_EMP_MASTER::DEPT
	USE
		CONTINUE 2060 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "UTL_DEPARTMENT"
		CONTINUE HelpError
	END WHEN

	!*******************************************************************
	! Print the body of the Form
	!*******************************************************************

2060	FOR FORMLOOP% = 1% TO FORM_GROUP%

		SELECT FORM_GROUP(FORMLOOP%)::FGROUP

		!
		! Top (main part) of the form
		!
		CASE "TOP"
			TEST_COUNT% = TEST_COUNT% + &
				OUTP_FORMPRINT(UTL_REPORTX, &
				FORMLOOP%, &
				FORM_TEXT$, &
				FORM_GROUPS%, &
				FORM_GROUP(), &
				0%)

			GOTO ExitProgram IF UTL_REPORTX::STAT

		CASE "RATE-HEAD"
			GOSUB RateHead

		CASE "RATE"
			GOSUB RateBody

		CASE "STATUS-HEAD"
			GOSUB StatusHead

		CASE "STATUS"
			GOSUB StatusBody

		CASE "ERNDED-HEAD"
			GOSUB ErndedHead

		CASE "ERNDED"
			GOSUB ErndedBody

		CASE "PAY-HEAD"
			GOSUB PayHead

		CASE "PAY"
			GOSUB PayBody

		CASE "TAX-HEAD"
			GOSUB TaxHead

		CASE "TAX"
			GOSUB TaxBody

		!
		! Anything that is unknown
		!
		CASE ELSE
			TEST_COUNT% = TEST_COUNT% + &
				OUTP_FORMPRINT(UTL_REPORTX, &
				FORMLOOP%, &
				FORM_TEXT$, &
				FORM_GROUPS%, &
				FORM_GROUP(), &
				0%)

			GOTO ExitProgram IF UTL_REPORTX::STAT

		END SELECT

	NEXT FORMLOOP%

	CALL OUTP_NEWPAGE(UTL_REPORTX)

	GOTO 2010

	%Page

3000	!*******************************************************************
	! See if employee has any rates, and if so print rate-head
	!*******************************************************************
 RateHead:
	WHEN ERROR IN
		GET #PR_EMP_RATE.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 3090
	END WHEN

	TEST_COUNT% = TEST_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FORMLOOP%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

3090	RETURN

	%PAGE

3100	!*******************************************************************
	! See if employee has any rates, and if so print rate-head
	!*******************************************************************
 RateBody:
	WHEN ERROR IN
		GET #PR_EMP_RATE.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 3190
	END WHEN

3120	IF PR_EMP_MASTER::EMPNUM = PR_EMP_RATE::EMPNUM
	THEN
		TEST_COUNT% = TEST_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
			FORMLOOP%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT

		WHEN ERROR IN
			GET #PR_EMP_RATE.CH%
		USE
			CONTINUE 3190
		END WHEN

		GOTO 3120
	END IF

3190	RETURN

	%PAGE

3200	!*******************************************************************
	! See if employee has any rates, and if so print rate-head
	!*******************************************************************
 StatusHead:
	WHEN ERROR IN
		GET #PR_EMP_STATUS.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 3290
	END WHEN

	TEST_COUNT% = TEST_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FORMLOOP%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

3290	RETURN

	%PAGE

3300	!*******************************************************************
	! See if employee has any Status, and if so print rate-head
	!*******************************************************************
 StatusBody:
	WHEN ERROR IN
		GET #PR_EMP_STATUS.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 3390
	END WHEN

3320	IF PR_EMP_MASTER::EMPNUM = PR_EMP_STATUS::EMPNUM
	THEN
		TEST_COUNT% = TEST_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
			FORMLOOP%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT

		WHEN ERROR IN
			GET #PR_EMP_STATUS.CH%
		USE
			CONTINUE 3390
		END WHEN

		GOTO 3320
	END IF

3390	RETURN

	%PAGE

3400	!*******************************************************************
	! See if employee has any rates, and if so print rate-head
	!*******************************************************************
 ErndedHead:
	WHEN ERROR IN
		GET #PR_EMP_STD_ERNDED.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 3490
	END WHEN

	TEST_COUNT% = TEST_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FORMLOOP%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

3490	RETURN

	%PAGE

3500	!*******************************************************************
	! See if employee has any Status, and if so print rate-head
	!*******************************************************************
 ErndedBody:
	WHEN ERROR IN
		GET #PR_EMP_STD_ERNDED.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 3590
	END WHEN

3520	IF PR_EMP_MASTER::EMPNUM = PR_EMP_STD_ERNDED::EMPNUM
	THEN
		TEST_COUNT% = TEST_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
			FORMLOOP%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT

		WHEN ERROR IN
			GET #PR_EMP_STD_ERNDED.CH%
		USE
			CONTINUE 3590
		END WHEN

		GOTO 3520
	END IF

3590	RETURN

	%PAGE

3600	!*******************************************************************
	! See if employee has any rates, and if so print rate-head
	!*******************************************************************
 PayHead:
	WHEN ERROR IN
		GET #PR_REG_ERNDED.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 3690
	END WHEN

	TEST_COUNT% = TEST_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FORMLOOP%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

3690	RETURN

	%PAGE

3700	!*******************************************************************
	! See if employee has any Status, and if so print rate-head
	!*******************************************************************
 PayBody:
	WHEN ERROR IN
		GET #PR_REG_ERNDED.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 3790
	END WHEN

3720	IF PR_EMP_MASTER::EMPNUM = PR_REG_ERNDED::EMPNUM
	THEN
		TEST_COUNT% = TEST_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
			FORMLOOP%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT

		WHEN ERROR IN
			GET #PR_REG_ERNDED.CH%
		USE
			CONTINUE 3790
		END WHEN

		GOTO 3720
	END IF

3790	RETURN

	%PAGE

3800	!*******************************************************************
	! See if employee has any rates, and if so print rate-head
	!*******************************************************************
 TaxHead:
	WHEN ERROR IN
		GET #PR_REG_TAXES.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 3890
	END WHEN

	TEST_COUNT% = TEST_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FORMLOOP%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

3890	RETURN

	%PAGE

3900	!*******************************************************************
	! See if employee has any Status, and if so print rate-head
	!*******************************************************************
 TaxBody:
	WHEN ERROR IN
		GET #PR_REG_TAXES.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 3990
	END WHEN

3920	IF PR_EMP_MASTER::EMPNUM = PR_REG_TAXES::EMPNUM
	THEN
		TEST_COUNT% = TEST_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
			FORMLOOP%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT

		WHEN ERROR IN
			GET #PR_REG_TAXES.CH%
		USE
			CONTINUE 3990
		END WHEN

		GOTO 3920
	END IF

3990	RETURN

	%PAGE

 ExitTotal:
	!
	! Print to the bottom of the body
	!
	GOTO Exitprogram

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************

	CALL OUTP_FINISHNOTITLE(UTL_REPORTX)

	CLOSE PR_EMP_MASTER.CH%

	!
	! Erase Display
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	!
	! Change the width
	!
	SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID, 80%)

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 Alignment:
	!*******************************************************************
	! Print alignment form, if desireable
	!*******************************************************************

	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	UTL_REPORTX::LINENO = 0%
	UTL_REPORTX::PAGENO = 0%
	TEST_COUNT% = 0%

	SCOPE::PRG_ITEM = "ALIGNMENT"
	JUNK$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", &
		"Do you want an alignment form?  Confirm then press <Do> ", &
		"N", 0%, "'E", "")

	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)

	SELECT SCOPE::SCOPE_EXIT

	!
	! An exit key was typed
	!
	CASE 3%, SMG$K_TRM_CTRLZ, SMG$K_TRM_F10
		GOTO ExitProgram

	!
	! Return, etc. act as next screen
	!
	CASE 10%, 12%, 13%, SMG$K_TRM_F7, SMG$K_TRM_DO

	!
	! Case else
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO Alignment

	END SELECT

	GOTO AlignmentReturn IF JUNK$ <> "Y"

	!
	! Print the body of the form
	!
	FOR I% = 1% TO FORM_GROUP%

		TEST_COUNT% = TEST_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
			I%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			0%)

	NEXT I%

	CALL OUTP_NEWPAGE(UTL_REPORTX)

	!
	! Do they need another?
	!
	GOTO Alignment

 AlignmentReturn:
	RETURN

	%PAGE

 LoadForm:
	!*******************************************************************
	! Initilize Transmittal form
	!*******************************************************************

	!
	! Get form from the PR form library
	!
	SMG_STATUS% = OUTP_FORMINIT(PR_FORM.DEV$ + "PR_FORM", REPORT$, &
		FORM_TEXT$, FORM_GROUP%, FORM_GROUP())

	!
	! Was there an error?
	!
	IF SMG_STATUS% <> 0%
	THEN
		CALL HELP_34MESSAGE(SCOPE, "transmittal form is missing", &
			"E", SCOPE::PRG_PROGRAM, REPORT$, NUM1$(SMG_STATUS%))
		GOTO ExitProgram
	END IF

	!
	! Search for the desired parts of the form
	!
	FRM_BODY% = 0%
	FRM_LENGTH% = 0%

	FOR I% = 1% TO FORM_GROUP%

		SELECT FORM_GROUP(I%)::FGROUP

		CASE "FRM-BODY"
			FRM_BODY% = I%

		CASE "FRM-LENGTH"
			FRM_LENGTH% = I%

		END SELECT

	NEXT I%

	RETURN

	%Page

19000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME 19990

 HelpError:
19990	!
	! This moved from inside error to outside so that errors occuring
	! at lower levels could be trapped.  Basic will not allow any
	! error to occur inside of an error no matter if it is in a
	! different module.
	!
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

	!
	! Handle the cases where a file couldn't be opened
	!
	END

20000	SUB FORM_LOADVAR(VARNAME$, REALVALUE, TEXTVALUE$)

	!*******************************************************************
	! This function is used to return the value of a field back to the
	! form printer.
	!*******************************************************************

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	!
	! Define maps
	!
	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_DEPARTMENT.HB"
	MAP	(UTL_DEPARTMENT)	UTL_DEPARTMENT_CDD	UTL_DEPARTMENT

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_RATE.HB"
	MAP (PR_EMP_RATE)	PR_EMP_RATE_CDD		PR_EMP_RATE

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STATUS.HB"
	MAP (PR_EMP_STATUS)	PR_EMP_STATUS_CDD	PR_EMP_STATUS

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STD_ERNDED.HB"
	MAP (PR_EMP_STD_ERNDED)	PR_EMP_STD_ERNDED_CDD	PR_EMP_STD_ERNDED

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.HB"
	MAP (PR_REG_ERNDED)	PR_REG_ERNDED_CDD	PR_REG_ERNDED

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.HB"
	MAP (PR_REG_TAXES)	PR_REG_TAXES_CDD	PR_REG_TAXES

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	MAP (TRANS_FORM) &
		HOUR_RATE, &
		PIECE_RATE, &
		FACTOR%, &
		STDEFF, &
		PAGE_COUNT, &
		REPDATE$, &
		PR_EMP_MASTER.ADDLINE$(3%) = 50%, &
		QTR%

	%PAGE

	!
	! Set up default values
	!
	REALVALUE = 0.0
	TEXTVALUE$ = "?????????????????????????????????????"

	!
	! Pick by variable
	!
	SELECT VARNAME$

	!************************************************************
	! Fields for the PR_EMP_MASTER file
	!************************************************************

	CASE "PR_EMP_MASTER::EMPNUM"
		TEXTVALUE$ = PR_EMP_MASTER::EMPNUM

	CASE "PR_EMP_MASTER::EMPNAME"
		TEXTVALUE$ = PR_EMP_MASTER::EMPNAME

	CASE "PR_EMP_MASTER::ADD1"
		TEXTVALUE$ = PR_EMP_MASTER::ADD1

	CASE "PR_EMP_MASTER::ADD2"
		TEXTVALUE$ = PR_EMP_MASTER::ADD2

	CASE "PR_EMP_MASTER::CITY"
		TEXTVALUE$ = PR_EMP_MASTER::CITY

	CASE "PR_EMP_MASTER::STATE"
		TEXTVALUE$ = PR_EMP_MASTER::STATE

	CASE "PR_EMP_MASTER::ZIP"
		TEXTVALUE$ = PR_EMP_MASTER::ZIP

	CASE "PR_EMP_MASTER::COUNTRY"
		TEXTVALUE$ = PR_EMP_MASTER::COUNTRY

	CASE "PR_EMP_MASTER.ADDLINE1"	! Substitute Employee Address
		TEXTVALUE$ = PR_EMP_MASTER.ADDLINE$(1%)

	CASE "PR_EMP_MASTER.ADDLINE2"	! Substitute Employee Address
		TEXTVALUE$ = PR_EMP_MASTER.ADDLINE$(2%)

	CASE "PR_EMP_MASTER.ADDLINE3"	! Substitute Employee Address
		TEXTVALUE$ = PR_EMP_MASTER.ADDLINE$(3%)

	CASE "PR_EMP_MASTER::PHONE"
		TEXTVALUE$ = PRNT_PHONE(PR_EMP_MASTER::PHONE, 0%)

	CASE "PR_EMP_MASTER::SSN"
		TEXTVALUE$ = PRNT_SSN(PR_EMP_MASTER::SSN, 0%)

	CASE "PR_EMP_MASTER::SORT"
		TEXTVALUE$ = PR_EMP_MASTER::SORT

	CASE "PR_EMP_MASTER::SUBACC"
		TEXTVALUE$ = PR_EMP_MASTER::SUBACC

	CASE "PR_EMP_MASTER::ACCT"
		TEXTVALUE$ = PR_EMP_MASTER::ACCT

	CASE "PR_EMP_MASTER::TRADE"
		TEXTVALUE$ = PR_EMP_MASTER::TRADE

	CASE "PR_EMP_MASTER::OPER"
		TEXTVALUE$ = PR_EMP_MASTER::OPER

	CASE "PR_EMP_MASTER::UNION"
		TEXTVALUE$ = PR_EMP_MASTER::UNION

	CASE "PR_EMP_MASTER::LOCATION"
		TEXTVALUE$ = PR_EMP_MASTER::LOCATION

	CASE "PR_EMP_MASTER::DEPT"
		TEXTVALUE$ = PR_EMP_MASTER::DEPT

	CASE "PR_EMP_MASTER::WORK_CENTER"
		TEXTVALUE$ = PR_EMP_MASTER::WORK_CENTER

	CASE "PR_EMP_MASTER::EMP_SKILL"
		TEXTVALUE$ = PR_EMP_MASTER::EMP_SKILL

	CASE "PR_EMP_MASTER::EMP_GRADE"
		TEXTVALUE$ = PR_EMP_MASTER::EMP_GRADE

	CASE "PR_EMP_MASTER::DISABLED"
		TEXTVALUE$ = PR_EMP_MASTER::DISABLED

	CASE "PR_EMP_MASTER::PAYFREQ"
		REALVALUE = PR_EMP_MASTER::PAYFREQ

	CASE "PR_EMP_MASTER::SUI_SW"
		TEXTVALUE$ = PR_EMP_MASTER::SUI_SW

	CASE "PR_EMP_MASTER::TAX_PKG"
		TEXTVALUE$ = PR_EMP_MASTER::TAX_PKG

	CASE "PR_EMP_MASTER::W2_1099"
		TEXTVALUE$ = PR_EMP_MASTER::W2_1099

	CASE "PR_EMP_MASTER::BIRTH"
		TEXTVALUE$ = PRNT_DATE(PR_EMP_MASTER::BIRTH, 8%)

	CASE "PR_EMP_MASTER::HIREDAY"
		TEXTVALUE$ = PRNT_DATE(PR_EMP_MASTER::HIREDAY, 8%)

	CASE "PR_EMP_MASTER::TERMDAY"
		TEXTVALUE$ = PRNT_DATE(PR_EMP_MASTER::TERMDAY, 8%)

	CASE "PR_EMP_MASTER::SEX"
		TEXTVALUE$ = PR_EMP_MASTER::SEX

	CASE "PR_EMP_MASTER::RACE"
		TEXTVALUE$ = PR_EMP_MASTER::RACE

	CASE "PR_EMP_MASTER::USCIT"
		TEXTVALUE$ = PR_EMP_MASTER::USCIT

	CASE "PR_EMP_MASTER::WRKPERMIT"
		TEXTVALUE$ = PR_EMP_MASTER::WRKPERMIT

	CASE "PR_EMP_MASTER::HOMCNTRY"
		TEXTVALUE$ = PR_EMP_MASTER::HOMCNTRY

	CASE "PR_EMP_MASTER::ACTIVE_FLAG"
		TEXTVALUE$ = PR_EMP_MASTER::ACTIVE_FLAG

	CASE "PR_EMP_MASTER::RATE_TYPE"
		TEXTVALUE$ = PR_EMP_MASTER::RATE_TYPE

	CASE "PR_EMP_MASTER::RATE_CDE"
		TEXTVALUE$ = PR_EMP_MASTER::RATE_CDE

	CASE "PR_EMP_MASTER::WC"
		TEXTVALUE$ = PR_EMP_MASTER::WC

	!************************************************************
	! Fields for the UTL_DEPARTMENT file
	!************************************************************

	CASE "UTL_DEPARTMENT::LOCATION"
		TEXTVALUE$ = UTL_DEPARTMENT::LOCATION

	CASE "UTL_DEPARTMENT::DEPT_NUM"
		TEXTVALUE$ = UTL_DEPARTMENT::DEPT_NUM

	CASE "UTL_DEPARTMENT::DESCRIPTION"
		TEXTVALUE$ = UTL_DEPARTMENT::DESCRIPTION

	CASE "UTL_DEPARTMENT::DEPGROUP"
		TEXTVALUE$ = UTL_DEPARTMENT::DEPGROUP

	CASE "UTL_DEPARTMENT::PHONE"
		TEXTVALUE$ = UTL_DEPARTMENT::PHONE

	CASE "UTL_DEPARTMENT::SUPERVISOR"
		TEXTVALUE$ = UTL_DEPARTMENT::SUPERVISOR


	!************************************************************
	! PR_EMP_RATE fields
	!************************************************************

	CASE "PR_EMP_RATE::OPER"
		TEXTVALUE$ = PR_EMP_RATE::OPER

	CASE "PR_EMP_RATE::EFFDAT"
		TEXTVALUE$ = PRNT_DATE(PR_EMP_RATE::EFFDAT, 8%)

	CASE "PR_EMP_RATE::RATE_TYPE"
		TEXTVALUE$ = PR_EMP_RATE::RATE_TYPE

	CASE "PR_EMP_RATE::RATE_CDE"
		TEXTVALUE$ = PR_EMP_RATE::RATE_CDE

	CASE "PR_EMP_RATE::HOUR_RATE"
		REALVALUE = PR_EMP_RATE::HOUR_RATE

	CASE "PR_EMP_RATE::PIECE_RATE"
		REALVALUE = PR_EMP_RATE::PIECE_RATE

	CASE "PR_EMP_RATE::FACTOR"
		REALVALUE = PR_EMP_RATE::FACTOR

	CASE "PR_EMP_RATE::STDEFF"
		REALVALUE = PR_EMP_RATE::STDEFF

	CASE "PR_EMP_RATE::EVAL_DATE"
		TEXTVALUE$ = PRNT_DATE(PR_EMP_RATE::EVAL_DATE, 8%)

	!************************************************************
	! Status
	!************************************************************

	CASE "PR_EMP_STATUS::STTYPE"
		TEXTVALUE$ = PR_EMP_STATUS::STTYPE

	CASE "PR_EMP_STATUS::CODE"
		TEXTVALUE$ = PR_EMP_STATUS::CODE

	CASE "PR_EMP_STATUS::STSTATUS"
		TEXTVALUE$ = PR_EMP_STATUS::STSTATUS

	CASE "PR_EMP_STATUS::EXEMPT"
		REALVALUE = PR_EMP_STATUS::EXEMPT

	!************************************************************
	! Standard earn/ded file
	!************************************************************

	CASE "PR_EMP_STD_ERNDED::RTYPE"
		TEXTVALUE$ = PR_EMP_STD_ERNDED::RTYPE

	CASE "PR_EMP_STD_ERNDED::CODE"
		TEXTVALUE$ = PR_EMP_STD_ERNDED::CODE

	CASE "PR_EMP_STD_ERNDED::RATE"
		REALVALUE = PR_EMP_STD_ERNDED::RATE

	CASE "PR_EMP_STD_ERNDED::LIMIT"
		REALVALUE = PR_EMP_STD_ERNDED::LIMIT

	CASE "PR_EMP_STD_ERNDED::CTDBAL"
		REALVALUE = PR_EMP_STD_ERNDED::CTDBAL

	CASE "PR_EMP_STD_ERNDED::ACCRUED"
		REALVALUE = PR_EMP_STD_ERNDED::ACCRUED

	CASE "PR_EMP_STD_ERNDED::ENDDAT"
		TEXTVALUE$ = PRNT_DATE(PR_EMP_STD_ERNDED::ENDDAT, 8%)

	CASE "PR_EMP_STD_ERNDED::FREQ"
		TEXTVALUE$ = PR_EMP_STD_ERNDED::FREQ

	CASE "PR_EMP_STD_ERNDED::METHOD"
		TEXTVALUE$ = PR_EMP_STD_ERNDED::METHOD

	CASE "PR_EMP_STD_ERNDED::USERDEF"
		TEXTVALUE$ = PR_EMP_STD_ERNDED::USERDEF

	!************************************************************
	! Ernded Register
	!*******************************************************************

	CASE "PR_REG_ERNDED::ETYPE"
		TEXTVALUE$ = PR_REG_ERNDED::ETYPE

	CASE "PR_REG_ERNDED::CODE"
		TEXTVALUE$ = PR_REG_ERNDED::CODE

	CASE "PR_REG_ERNDED::QTR_DOLL"
		REALVALUE = PR_REG_ERNDED::QTR_DOLL(QTR%)

	CASE "PR_REG_ERNDED::QTR1_DOLL"
		REALVALUE = PR_REG_ERNDED::QTR_DOLL(0%)

	CASE "PR_REG_ERNDED::QTR2_DOLL"
		REALVALUE = PR_REG_ERNDED::QTR_DOLL(1%) IF QTR% >= 1%

	CASE "PR_REG_ERNDED::QTR3_DOLL"
		REALVALUE = PR_REG_ERNDED::QTR_DOLL(2%) IF QTR% >= 2%

	CASE "PR_REG_ERNDED::QTR4_DOLL"
		REALVALUE = PR_REG_ERNDED::QTR_DOLL(3%) IF QTR% >= 3%

	CASE "PR_REG_ERNDED::YEA_DOLL"
		TEMP = 0.0
		TEMP = TEMP + PR_REG_ERNDED::QTR_DOLL(I%) &
			FOR I% = 0% TO QTR%
		REALVALUE = TEMP

	CASE "PR_REG_ERNDED::QTR_REG_HRS"
		REALVALUE = PR_REG_ERNDED::REG_HRS(QTR%)

	CASE "PR_REG_ERNDED::QTR1_REG_HRS"
		REALVALUE = PR_REG_ERNDED::REG_HRS(0%)

	CASE "PR_REG_ERNDED::QTR2_REG_HRS"
		REALVALUE = PR_REG_ERNDED::REG_HRS(1%) IF QTR% >= 1%

	CASE "PR_REG_ERNDED::QTR3_REG_HRS"
		REALVALUE = PR_REG_ERNDED::REG_HRS(2%) IF QTR% >= 2%

	CASE "PR_REG_ERNDED::QTR4_REG_HRS"
		REALVALUE = PR_REG_ERNDED::REG_HRS(3%) IF QTR% >= 3%

	CASE "PR_REG_ERNDED::YEA_REG_HRS"
		TEMP = 0.0
		TEMP = TEMP + PR_REG_ERNDED::REG_HRS(I%) &
			FOR I% = 0% TO QTR%
		REALVALUE = TEMP

	CASE "PR_REG_ERNDED::QTR_PRE_HRS"
		REALVALUE = PR_REG_ERNDED::PRE_HRS(QTR%)

	CASE "PR_REG_ERNDED::QTR1_PRE_HRS"
		REALVALUE = PR_REG_ERNDED::PRE_HRS(0%)

	CASE "PR_REG_ERNDED::QTR2_PRE_HRS"
		REALVALUE = PR_REG_ERNDED::PRE_HRS(1%) IF QTR% >= 1%

	CASE "PR_REG_ERNDED::QTR3_PRE_HRS"
		REALVALUE = PR_REG_ERNDED::PRE_HRS(2%) IF QTR% >= 2%

	CASE "PR_REG_ERNDED::QTR4_PRE_HRS"
		REALVALUE = PR_REG_ERNDED::PRE_HRS(3%) IF QTR% >= 3%

	CASE "PR_REG_ERNDED::YEA_PRE_HRS"
		TEMP = 0.0
		TEMP = TEMP + PR_REG_ERNDED::PRE_HRS(I%) &
			FOR I% = 0% TO QTR%
		REALVALUE = TEMP

	CASE "PR_REG_ERNDED::QTR_UNITS"
		REALVALUE = PR_REG_ERNDED::UNITS(QTR%)

	CASE "PR_REG_ERNDED::QTR1_UNITS"
		REALVALUE = PR_REG_ERNDED::UNITS(0%)

	CASE "PR_REG_ERNDED::QTR2_UNITS"
		REALVALUE = PR_REG_ERNDED::UNITS(1%) IF QTR% >= 1%

	CASE "PR_REG_ERNDED::QTR3_UNITS"
		REALVALUE = PR_REG_ERNDED::UNITS(2%) IF QTR% >= 2%

	CASE "PR_REG_ERNDED::QTR4_UNITS"
		REALVALUE = PR_REG_ERNDED::UNITS(3%) IF QTR% >= 3%

	CASE "PR_REG_ERNDED::YEA_UNITS"
		TEMP = 0.0
		TEMP = TEMP + PR_REG_ERNDED::UNITS(I%) &
			FOR I% = 0% TO QTR%
		REALVALUE = TEMP

	!************************************************************
	! Register Taxes
	!************************************************************

	CASE "PR_REG_TAXES::TTYPE"
		TEXTVALUE$ = PR_REG_TAXES::TTYPE

	CASE "PR_REG_TAXES::CODE"
		TEXTVALUE$ = PR_REG_TAXES::CODE

	CASE "PR_REG_TAXES::QTR_TAX"
		REALVALUE = PR_REG_TAXES::TAX(QTR%)

	CASE "PR_REG_TAXES::QTR1_TAX"
		REALVALUE = PR_REG_TAXES::TAX(0%)

	CASE "PR_REG_TAXES::QTR2_TAX"
		REALVALUE = PR_REG_TAXES::TAX(1%) IF QTR% >= 1%

	CASE "PR_REG_TAXES::QTR3_TAX"
		REALVALUE = PR_REG_TAXES::TAX(2%) IF QTR% >= 2%

	CASE "PR_REG_TAXES::QTR4_TAX"
		REALVALUE = PR_REG_TAXES::TAX(3%) IF QTR% >= 3%

	CASE "PR_REG_TAXES::YEA_TAX"
		TEMP = 0.0
		TEMP = TEMP + PR_REG_TAXES::TAX(I%) &
			FOR I% = 0% TO QTR%
		REALVALUE = TEMP

	CASE "PR_REG_TAXES::QTR_REPORTABLE"
		REALVALUE = PR_REG_TAXES::REPORTABLE(QTR%)

	CASE "PR_REG_TAXES::QTR1_REPORTABLE"
		REALVALUE = PR_REG_TAXES::REPORTABLE(0%)

	CASE "PR_REG_TAXES::QTR2_REPORTABLE"
		REALVALUE = PR_REG_TAXES::REPORTABLE(1%) IF QTR% >= 1%

	CASE "PR_REG_TAXES::QTR3_REPORTABLE"
		REALVALUE = PR_REG_TAXES::REPORTABLE(2%) IF QTR% >= 2%

	CASE "PR_REG_TAXES::QTR4_REPORTABLE"
		REALVALUE = PR_REG_TAXES::REPORTABLE(3%) IF QTR% >= 3%

	CASE "PR_REG_TAXES::YEA_REPORTABLE"
		TEMP = 0.0
		TEMP = TEMP + PR_REG_TAXES::REPORTABLE(I%) &
			FOR I% = 0% TO QTR%
		REALVALUE = TEMP

	CASE "PR_REG_TAXES::QTR_TAXABLE"
		REALVALUE = PR_REG_TAXES::TAXABLE(QTR%)

	CASE "PR_REG_TAXES::QTR1_TAXABLE"
		REALVALUE = PR_REG_TAXES::TAXABLE(0%)

	CASE "PR_REG_TAXES::QTR2_TAXABLE"
		REALVALUE = PR_REG_TAXES::TAXABLE(1%) IF QTR% >= 1%

	CASE "PR_REG_TAXES::QTR3_TAXABLE"
		REALVALUE = PR_REG_TAXES::TAXABLE(2%) IF QTR% >= 2%

	CASE "PR_REG_TAXES::QTR4_TAXABLE"
		REALVALUE = PR_REG_TAXES::TAXABLE(3%) IF QTR% >= 3%

	CASE "PR_REG_TAXES::YEA_TAXABLE"
		TEMP = 0.0
		TEMP = TEMP + PR_REG_TAXES::TAXABLE(I%) &
			FOR I% = 0% TO QTR%
		REALVALUE = TEMP

	!************************************************************
	! Misc. feilds
	!************************************************************

	CASE "REPDATE"
		TEXTVALUE$ = REPDATE$

	END SELECT

	END SUB
