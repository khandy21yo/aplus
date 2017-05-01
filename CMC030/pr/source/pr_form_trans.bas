1	%TITLE "WRKSHT - Print Transmittal Worksheet"
	%SBTTL "PR_FORM_TRANS"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 1987, 1988 BY
	!	Computer Management Center, Inc.
	!	Idaho Falls, Idaho.
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
	! ID:PRTRAN
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Transmittal Worksheet\* option
	!	prints a payroll transmittal form which can be used to manually record
	!	various pay and payroll deduction information. A completed Transmittal
	!	Worksheet would then be used as the document from which the payroll
	!	data would be entered in the Timekeeper routine.
	!	.b
	!	The format of the Transmittal Worksheet can be user defined.
	!	.lm -5
	!
	! Index:
	!	.x Transmittal>Worksheet
	!	.x Report>Transmittal Worksheet
	!	.x Worksheet>Transmittal
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_FORM_TRANS
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_FORM_TRANS, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_FORM_TRANS.OBJ;*
	!
	! Author:
	!
	!	12/05/87 - Robert Peterson
	!
	! Modification history:
	!
	!	03/18/91 - Kevin Handy
	!		Added eval_date field to pr_read_rate.
	!
	!	03/21/91 - Kevin Handy
	!		Added default form length.
	!
	!	05/22/91 - Kevin Handy
	!		Modifications to handle TERMDAY more consistantly.
	!
	!	06/03/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	06/06/91 - J. Shad Rydalch
	!		Added form fields for address that will keep
	!		post office happy.
	!
	!	06/19/91 - Craig Tanner
	!		Added section to update UTL_REPORT from master.
	!
	!	07/14/91 - Kevin Handy
	!		Removed error trapping for 4000, which doesn't exist.
	!
	!	03/03/92 - Kevin Handy
	!		Fixed minor paging problem, modified to
	!		use OUTP_FORMFF to skip to bottom of page.
	!
	!	03/04/92 - Kevin Handy
	!		Fixed bug where it used FRM_BODY% to determine
	!		length of body part of form to FORM_LENGTH%.
	!
	!	03/05/92 - Kevin Handy
	!		Still trying to get rid of paging bugs.
	!
	!	03/10/92 - Dan Perkins
	!		Changed call OUTP_FORMFF to OUTP_NEWPAGE.
	!
	!	06/08/92 - Kevin Handy
	!		Modified to use OUTP_INITFORM function.
	!
	!	08/12/92 - Kevin Handy
	!		Modified to allow entry of report date "RD".
	!
	!	10/15/92 - Kevin Handy
	!		Added "FRM-NOPAGE" option in the-form.
	!
	!	04/13/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/16/95 - Kevin Handy
	!		(V3.6)
	!		Added sort by work center (WC)
	!
	!	04/12/95 - Kevin Handy
	!		Update to V3.6 coding standards.
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
	!		Don't erase SMG_SCREEN_DATA, which is
	!		never created
	!
	!	06/09/99 - Kevin Handy
	!		Lose line 585 (Dead code)
	!
	!	03/22/2000 - Kevin Handy
	!		Added EFF_ADTE parameter to PR_READ_RATE
	!
	!	09/21/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	%PAGE

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
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]FORM_GROUP.HB"
	DIM FORM_GROUP_CDD FORM_GROUP(10%)	! Max of 10 groups

	MAP (TRANS_FORM) &
		RATE_TYPE$ = 1%, &
		RATE_CDE$ = 2%, &
		HOUR_RATE, &
		PIECE_RATE, &
		FACTOR%, &
		STDEFF, &
		PR_END_DATE$ = 10%, &
		PAGE_COUNT, &
		PR_EMP_MASTER.ADDLINE$(3%) = 50%

	!
	! External functions
	!
	EXTERNAL LONG  FUNCTION OUTP_FORMINIT
	EXTERNAL LONG  FUNCTION OUTP_FORMPRINT
	EXTERNAL LONG  FUNCTION OUTP_INITFORM

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	REPORT$,REPORT1$ = "PRTRAN"

	!
	! Look up device
	!
	CALL READ_DEVICE("PR_FORM", PR_FORM.DEV$, STAT%)

	!***************************************************************
	! Open all of the files
	!***************************************************************

300	!
	! Open Payroll Employee master file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

340	!
	! Get Report
	!
	OPTN$ = "RD"
	GOTO ExitProgram &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT1$, OPTN$) <> CMC$_NORMAL

	!
	! Set Page Length
	!
	PR_END_DATE$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	.ts 55
	!	^*(01) Payroll Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*Payroll Date\* field enters the
	!	payroll folder file date relative to the subject
	!	Transmittal form.
	!	.lm -5
	!
	! Index:
	!	.x Date>Transmittal Report
	!	.x Date>Worksheet Report
	!	.x Transmittal>Date
	!	.x Worksheet>Date
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field enters a value which causes the
	!	Transmittal to begin printing with a selected item.
	!	The item must be in agreement with the specific type of sort
	!	selection. For example, if the selection is made to print the
	!	transmittal in employee number order, a value in this field
	!	must be an employee number; if the selection is made to print
	!	the transmittal in alphabetical order, the value in this field
	!	must be the name of an employee, last name first.
	!	.b
	!	If this field contains blanks, the report will begin with the first
	!	record in the file.
	!	.b
	!	Refer to field (04) Sort of the Payroll Transmittal report
	!	setting screen for more information on sort choices.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Transmittal
	!	.x From Item>Worksheet
	!	.x Worksheet>From Item
	!	.x Transmittal>From Item
	!
	!--


	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field enters a value which
	!	will cause the Transmittal to end printing with a
	!	selected item. The item must be in agreement with the specific type
	!	of sort selected. For example, if the selection is made to print the
	!	transmittal in location number order, a value in this field
	!	must be a location code; if the selection is made to print
	!	the transmittal in alphabetical order,
	!	the value in this field must be the name of an employee, last
	!	name first.
	!	.b
	!	If this field is left blank, the report will end with the
	!	last record in the file.
	!	.b
	!	Refer to field (04) Sort of the Payroll Transmittal report
	!	setting screen for more information on sort choices.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Transmittal
	!	.x To Item>Worksheet
	!	.x Transmittal>To Item
	!	.x Worksheet>To Item
	!
	!--


	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	.TS 55
	!	^*(04) Sort	NU,LN,LE,LO,SO\*
	!	.B
	!	.LM +5
	!	The ^*Sort\* field selects the manner
	!	in which the transmittal will be sorted and printed.
	!	.b
	!	Valid selections are:
	!	.TABLE 3,25
	!	.TE
	!	^*NU\*	Employee Number
	!	.TE
	!	^*LN\*	Location, Department, Work Center, Number is secondary sort
	!	.TE
	!	^*LE\*	Location, Employee Number is secondary sort.
	!	.TE
	!	^*LO\*	Location, Alpha is secondary sort
	!	.TE
	!	^*SO\*	Alphabetical (last name first)
	!	.te
	!	*W*C	Work Center
	!	.END TABLE
	!	If a ^*LN\*, *L*E, or *W*C selection is made, there will be a slight delay while
	!	the system creates a sorted file.
	!	.LM -5
	!
	! Index:
	!	.x Sort>Transmittal
	!	.x Transmittal>Sort
	!	.x Worksheet>Sort
	!	.x Sort>Worksheet
	!
	!--


	PAGEBREAK$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	.TS 55
	!	^*(05) Page After Sort Item	Y or N\*
	!	.b
	!	.lm +5
	!	When either an ^*LN\* or ^*LO\* sort selection is made in field
	!	(04), the ^*Page After Sort Item\* field causes a page
	!	break to occur after printing each location.
	!	.table 3,25
	!	.te
	!	^*Y\*	Page Break
	!	.te
	!	^*N\*	No Page Break
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Paging>Transmittal
	!	.x Transmittal>Paging
	!	.x Paging>Worksheet
	!	.x Worksheet>Paging
	!
	!--


	REPORT$ = REPORT$ + "$" + TRM$(UTL_REPORTX::OPTDEF(5%))

	!++
	! Abstract:FLD06
	!	^*(06) Form\*
	!	.b
	!	.lm +5
	!	The ^*Form\* field selects a specific
	!	Worksheet format.
	!	.b
	!	Different formats can be created in the Payroll -> Utility -> Format
	!	menu option. Several formats may be created, each with a unique name
	!	designation. The specific format needed can be accessed by entering the
	!	name which has been designated for the specific format.
	!	.lm -5
	!
	! Index:
	!	.x Form>Transmittal
	!	.x Form>Worksheet
	!	.x Transmittal>Form
	!	.x Worksheet>Form
	!
	!--


	DEPT$ = EDIT$(UTL_REPORTX::OPTDEF(6%), -1%)

	!++
	! Abstract:FLD07
	!	^*(07) Department\*
	!	.b
	!	.lm +5
	!	The ^*Department\* field selects a
	!	specific department for which worksheets are to be printed.
	!	.b
	!	Wildcard techniques may be used in the department selection.
	!	.lm -5
	!
	! Index:
	!	.x Department>Worksheet
	!	.x Department>Transmittal
	!	.x Worksheet>Department
	!	.x Transmittal>Department
	!
	!--

	EFFDAT$ = DATE_STOREDATE(PR_END_DATE$)

	SELECT SORTBY$
	CASE "NU"
		K_NUM% = 0%

	CASE "NA"
		K_NUM% = 1%

	CASE "SN"
		K_NUM% = 3%

	CASE "LO"
		K_NUM% = 4%

	CASE "WC"
530		!
		! Sort Payroll Employee master file
		!
		CALL ENTR_3MESSAGE(SCOPE, "Creating temporary file", 17%)
		CALL ASSG_CHANNEL(PR_EMP_MASTER_TEMP.CH%, STAT%)

		OPEN "TEMP.TEMP" FOR OUTPUT AS FILE PR_EMP_MASTER_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP PR_EMP_MASTER, &
			BUFFER 32%, &
			TEMPORARY, &
			PRIMARY KEY &
			( &
				PR_EMP_MASTER::WORK_CENTER, &
				PR_EMP_MASTER::EMPNUM &
			), &
			ACCESS MODIFY, ALLOW NONE

		RESET #PR_EMP_MASTER.CH%

540		WHEN ERROR IN
			GET #PR_EMP_MASTER.CH%,REGARDLESS
			PUT #PR_EMP_MASTER_TEMP.CH%
		USE
			CONTINUE 545 IF ERR = 11%
			FILENAME$ = "PR_EMP_MASTER"
			CONTINUE HelpError
		END WHEN

		GOTO 540

545		PR_EMP_MASTER.CH% = PR_EMP_MASTER_TEMP.CH%
		K_NUM% = 0%

	CASE "LN"
550		!
		! Sort Payroll Employee master file
		!
		CALL ENTR_3MESSAGE(SCOPE, "Creating temporary file", 17%)
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

560		WHEN ERROR IN
			GET #PR_EMP_MASTER.CH%,REGARDLESS
			PUT #PR_EMP_MASTER_TEMP.CH%
		USE
			CONTINUE 565 IF ERR = 11%
			FILENAME$ = "PR_EMP_MASTER"
			CONTINUE HelpError
		END WHEN

		GOTO 560

565		PR_EMP_MASTER.CH% = PR_EMP_MASTER_TEMP.CH%
		K_NUM% = 0%

	CASE "LE"
570		!
		! Sort Payroll Employee master file
		!
		CALL ENTR_3MESSAGE(SCOPE, "Creating temporary file", 17%)
		CALL ASSG_CHANNEL(PR_EMP_MASTER_TEMP.CH%, STAT%)

		OPEN "TEMP.TEMP" FOR OUTPUT AS FILE PR_EMP_MASTER_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP PR_EMP_MASTER, &
			BUFFER 32%, &
			TEMPORARY, &
			PRIMARY KEY &
			( &
				PR_EMP_MASTER::LOCATION, &
				PR_EMP_MASTER::EMPNUM &
			), &
			ACCESS MODIFY, ALLOW NONE

		RESET #PR_EMP_MASTER.CH%

580		GET #PR_EMP_MASTER.CH%,REGARDLESS
		PUT #PR_EMP_MASTER_TEMP.CH%
		GOTO 580

	CASE ELSE
		K_NUM% = 2%
	END SELECT

590	!
	! Load in the form
	!
	GOSUB LoadForm

	!
	! GOTO aligment routine
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
			FIND #PR_EMP_MASTER.CH%, KEY #K_NUM% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	PAGE_COUNT = 1%
	UTL_REPORTX::LINENO = 0%
	TEST_COUNT% = 0%

	!
	! Print the top of the form
	!
	HEAD_COUNT% = OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_TOP%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

2010	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	GOTO 2010 &
		IF COMP_STRING(PR_EMP_MASTER::DEPT, DEPT$) = 0% AND DEPT$ <> ""

	IF PR_EMP_MASTER::TERMDAY <= "00000000"
	THEN
		IF PAGEBREAK$ = "Y" AND TEST_PRINT%
		THEN
			SELECT SORTBY$

			CASE "NU"
				GOTO ExitTotal IF TO_ITEM$ <> "" AND &
					PR_EMP_MASTER::EMPNUM > TO_ITEM$
				IF TEST_EMPNUM$ <> PR_EMP_MASTER::EMPNUM
				THEN
					GOSUB PageBreak
				END IF

			CASE "NA"
				GOTO ExitTotal IF TO_ITEM$ <> "" AND &
					PR_EMP_MASTER::EMPNAME > TO_ITEM$
				IF TEST_EMPNAME$ <> PR_EMP_MASTER::EMPNAME
				THEN
					GOSUB PageBreak
				END IF

			CASE "SN"
				GOTO ExitTotal IF TO_ITEM$ <> "" AND &
					PR_EMP_MASTER::SSN > TO_ITEM$
				IF TEST_SSN$ <> PR_EMP_MASTER::SSN
				THEN
					GOSUB PageBreak
				END IF

			CASE "LO", "LN", "LE"
				GOTO ExitTotal IF TO_ITEM$ <> "" AND &
					PR_EMP_MASTER::LOCATION > TO_ITEM$
				IF TEST_LOCATION$ <> PR_EMP_MASTER::LOCATION
				THEN
					GOSUB PageBreak
				END IF

			CASE "WC"
				GOTO ExitTotal IF TO_ITEM$ <> "" AND &
					PR_EMP_MASTER::WORK_CENTER > TO_ITEM$
				IF TEST_LOCATION$ <> PR_EMP_MASTER::WORK_CENTER
				THEN
					GOSUB PageBreak
				END IF

			CASE ELSE
				GOTO ExitTotal IF TO_ITEM$ <> "" AND &
					PR_EMP_MASTER::SORT > TO_ITEM$
				IF TEST_SORT$ <> PR_EMP_MASTER::SORT
				THEN
					GOSUB PageBreak
				END IF

			END SELECT

		END IF

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
			PR_EMP_MASTER::STATE + " " + &
			PR_EMP_MASTER::ZIP + " " + &
			PR_EMP_MASTER::COUNTRY, 8% + 16% + 32% + 128%)

		PR_EMP_MASTER.ADDLINE$(LOOP%) = "" &
			FOR LOOP% = I% + 1% TO 3%	! Blank 'em out

		!
		! Set Test Variables
		!
		TEST_EMPNUM$ = PR_EMP_MASTER::EMPNUM
		TEST_EMPNAME$ = PR_EMP_MASTER::EMPNAME
		TEST_SSN$ = PR_EMP_MASTER::SSN
		TEST_LOCATION$ = PR_EMP_MASTER::LOCATION
		TEST_SORT$ = PR_EMP_MASTER::SORT

		TEST_PRINT% = -1%

		CALL PR_READ_RATE(PR_EMP_MASTER::EMPNUM, &
			PR_EMP_MASTER::OPER, &
			EFFDAT$, &
			RATE_TYPE$, &
			RATE_CDE$, &
			HOUR_RATE, &
			PIECE_RATE, &
			FACTOR%, &
			STDEFF, &
			EVALDATE$, &
			EFF_DATE$)

		!
		! Increment body counter, check to see if need to page
		!
		IF TEST_COUNT% + 1% >= BODY_LENGTH%
		THEN
			GOSUB PageBreak
		END IF

		!
		! Print the body of the Form
		!
		TEST_COUNT% = TEST_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_BODY%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			0%)

	END IF

	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO 2010

	%Page

 ExitTotal:
	!
	! Print to the bottom of the body
	!
	CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
		FOR LOOP% = HEAD_COUNT% + TEST_COUNT% TO FORM_LENGTH%

	!
	! Print the bottom of the form
	!
	SMG_STATUS% = OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_BOT%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	!
	! Print to the bottom of the form
	!
	CALL OUTP_NEWPAGE(UTL_REPORTX) &
		IF NOPAGE% = 0%

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

	GOTO AlignmentReturn &
		IF JUNK$ <> "Y"

	HEAD_COUNT% = OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_TOP%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		1%)

	!
	! Print the body of the form
	!
	TEST_COUNT% = TEST_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_BODY%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		1%) &
		FOR I% = 1% TO 3%

	!
	! Print lines to bottom of the form
	!
	CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
		FOR LOOP% = HEAD_COUNT% + TEST_COUNT% TO FORM_LENGTH%

	!
	! Print the bottom of the form
	!
	SMG_STATUS% = OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_BOT%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		1%)

	!
	! Print to the bottom of the form
	!
	CALL OUTP_NEWPAGE(UTL_REPORTX) &
		IF NOPAGE% = 0%

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
	FRM_TOP% = 0%
	FRM_BOT% = 0%
	FRM_BODY% = 0%
	FORM_LENGTH% = 0%
	NOPAGE% = 0%

	FOR I% = 1% TO FORM_GROUP%

		SELECT FORM_GROUP(I%)::FGROUP

		CASE "FRM-TOP"
			FRM_TOP% = I%

		CASE "FRM-BOT"
			FRM_BOT% = I%

		CASE "FRM-BODY"
			FRM_BODY% = I%
			BODY_LENGTH% = FORM_GROUP(I%)::NUMBER

		CASE "FRM-LENGTH"
			FORM_LENGTH% = FORM_GROUP(I%)::NUMBER

		CASE "FRM-NOPAGE"
			NOPAGE% = I%

		END SELECT

	NEXT I%

	FORM_LENGTH% = 50% &
		IF FORM_LENGTH% = 0%
	BODY_LENGTH% = FORM_LENGTH% = 15% &
		IF BODY_LENGTH% = 0%

	RETURN

 PageBreak:
	!*******************************************************************
	! Page Break
	!*******************************************************************

	!
	! Print to the bottom of the body
	!
	CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
		FOR LOOP% = HEAD_COUNT% + TEST_COUNT% + 6% TO FORM_LENGTH%

	!
	! Print the bottom of the form
	!
	SMG_STATUS% = OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_BOT%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

 PageB:
	CALL OUTP_NEWPAGE(UTL_REPORTX) &
		IF NOPAGE% = 0%

	PAGE_COUNT = PAGE_COUNT + 1%
	UTL_REPORTX::LINENO = 0%
	TEST_COUNT% = 0%

	!
	! Print the top of the form
	!
	HEAD_COUNT% = OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_TOP%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	RETURN

	%Page


19000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

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

	MAP (TRANS_FORM) &
		RATE_TYPE$ = 1%, &
		RATE_CDE$ = 2%, &
		HOUR_RATE, &
		PIECE_RATE, &
		FACTOR%, &
		STDEFF, &
		PR_END_DATE$ = 10%, &
		PAGE_COUNT, &
		PR_EMP_MASTER.ADDLINE$(3%) = 50%

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
	! Non fielded values
	!************************************************************

	CASE "RATE_TYPE"
		TEXTVALUE$ = RATE_TYPE$

	CASE "RATE_CDE"
		TEXTVALUE$ = RATE_CDE$

	CASE "HOUR_RATE"
		REALVALUE = HOUR_RATE

	CASE "PIECE_RATE"
		REALVALUE = PIECE_RATE

	CASE "FACTOR"
		REALVALUE = FACTOR%

	CASE "STDEFF"
		REALVALUE = STDEFF

	CASE "PR_END_DATE"
		TEXTVALUE$ = PR_END_DATE$

	CASE "PAGE"
		REALVALUE = PAGE_COUNT
		TEXTVALUE$ = NUM1$(PAGE_COUNT)

	END SELECT


	END SUB
