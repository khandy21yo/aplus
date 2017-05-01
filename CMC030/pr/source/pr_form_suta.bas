1	%TITLE "Print Payroll Transmittal Using a Form"
	%SBTTL "PR_FORM_SUTA"
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
	! ID:PRSUTA
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
	!	$ BAS PR_SOURCE:PR_FORM_SUTA
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_FORM_SUTA, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_FORM_SUTA.OBJ;*
	!
	! Author:
	!
	!	09/23/91 - JEFF BEARD
	!
	! Modification history:
	!
	!	10/30/91 - Kevin Handy
	!		Lots of bug fixes, formatting changes, additional
	!		information, ...
	!
	!	10/31/91 - Kevin Handy
	!		Modified to use SUTA code from master file if it
	!		is defined.
	!
	!	10/31/91 - Kevin Handy
	!		Modified to also use SUTA code from master file
	!		in calculating the grand total that gets printed
	!		on each page in findtotal.
	!
	!	03/24/92 - Kevin Handy
	!		Allowed Access to new "SUTA ACCOUNT" field in
	!		state tax profile.
	!
	!	06/24/92 - Kevin Handy
	!		Modified to use OUTP_INITFORM function.
	!
	!	07/08/92 - Kevin Handy
	!		Modified to calculate correct final total.
	!		It was stoppong after first TAX record found
	!		was not a "SW" record.
	!
	!	04/13/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	10/31/94 - Kevin Handy
	!		Ripped code out of PR_RPRT_SUTA to make this report
	!		print SUTA taxable instead of STATE taxable.
	!
	!	12/07/94 - Kevin Handy
	!		Fixed bug where error at 2016 went to 2010 instead
	!		of 2020, where it should have.
	!
	!	12/07/94 - Kevin Handy
	!		Redid way that the FORM total is calculated.
	!		Maybe now it is correct.
	!
	!	12/07/94 - Kevin Handy
	!		Added rounding to the form total, just in case.
	!
	!	01/18/95 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	04/25/95 - Kevin Handy
	!		Lose extra externals no longer needed.
	!
	!	09/09/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/10/97 - Kevin Handy
	!		Lose PRNT.CH% variable
	!		Use integer for #key
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	09/09/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	05/29/98 - Kevin Handy
	!		Handle 'F' final deduction codes.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/26/98 - Kevin Handy
	!		Don't erase SMG_SCREEN_DATA%, which is
	!		never created.
	!
	!	01/18/99 - Kevin Handy
	!		Lose an extra GET on PR_EMP_MASTER which was
	!		really confising things
	!
	!	11/03/2000 - Kevin Handy
	!		Use A"x"B
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.HB"
	MAP (PR_REG_TAXES)	PR_REG_TAXES_CDD	PR_REG_TAXES

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.HB"
	MAP (PR_REG_ERNDED) PR_REG_ERNDED_CDD PR_REG_ERNDED

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.HB"
	MAP (PR_TAX_PROFILE_F)	PR_TAX_PROFILE_F_CDD	PR_TAX_PROFILE_F
	MAP (PR_TAX_PROFILE_F1)	PR_TAX_PROFILE_F_CDD	PR_TAX_PROFILE_F1

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_S.HB"
	MAP (PR_TAX_PROFILE_F)	PR_TAX_PROFILE_S_CDD	PR_TAX_PROFILE_S
	MAP (PR_TAX_PROFILE_S1)	PR_TAX_PROFILE_S_CDD	PR_TAX_PROFILE_S1

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP (PR_ERNDED_DEF) PR_ERNDED_DEF_CDD PR_ERNDED_DEF

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.HB"
	MAP (PR_TAX_PKG)	PR_TAX_PKG_CDD	PR_TAX_PKG

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.HB"
	MAP (UTL_REPORT) UTL_REPORT_CDD UTL_REPORT
	DECLARE UTL_REPORT_CDD UTL_REPORT_SYS

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.HB"
	MAP (UTL_PROFILE)	UTL_PROFILE_CDD		UTL_PROFILE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[UTL.OPEN]FORM_GROUP.HB"
	DIM FORM_GROUP_CDD FORM_GROUP(10%)	! Max of 10 groups

	RECORD PR_TEMP_CDD
		STRING	STCODE = 2%
		STRING	SORTKEY = 20%
		STRING	EMPNUM = 10%
		REAL	WAGES(3%)
		WORD	WKWRK(3%)
	END RECORD

	DECLARE PR_TEMP_CDD PR_TEMP_BUFFER(52%)

	!
	! COMMON VARIABLES
	!
	COMMON (VARIABLES) REAL &
			QTR_WAGES, FORM_TOTAL, TOTAL_PAGE, &
		STRING &
			MAGNETIC_MEDIA, YYYY, &
		INTEGER &
			QTR


	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION OUTP_FORMINIT
	EXTERNAL LONG    FUNCTION OUTP_FORMPRINT
	EXTERNAL LONG   FUNCTION OUTP_INITFORM

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	REPORT$ = "PRSUTA"

	!
	! Look up device
	!
	CALL  READ_DEVICE("PR_FORM", PR_FORM.DEV$, STAT%)

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

310	!
	! Open tax Profile
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.OPN"

		GET #PR_TAX_PROFILE.CH%, KEY #0% EQ "F"
		PR_TAX_PROFILE_F1 = PR_TAX_PROFILE_F
	USE
		FILENAME$ = "PR_TAX_PROFILE"
		CONTINUE HelpError
	END WHEN

315	!
	! Open tax package file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.OPN"
	USE
		FILENAME$ = "PR_TAX_PKG"
		CONTINUE HelpError
	END WHEN

317	!
	! Open Company Profile
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.OPN"
		GET #UTL_PROFILE.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "UTL_PROFILE"
		CONTINUE HelpError
	END WHEN

320	!
	! Open LOCATION file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"

		GET #UTL_LOCATION.CH%, &
			KEY #0% EQ UTL_PROFILE::MAINLOCATION, &
			REGARDLESS
	USE
		UTL_LOCATION::LOCNAME = ""
		UTL_LOCATION::ADDRESS1 = ""
		UTL_LOCATION::ADDRESS2 = ""
		UTL_LOCATION::CITY = ""
		UTL_LOCATION::STATE = ""
		UTL_LOCATION::ZIP = ""

		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

342	!
	! Open ERNDED_DEF file
	!
	ADJUST_NEGATIVE$ = ""
	ADJUST_POSITIVE$ = ""

	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"
		RESET #PR_ERNDED_DEF.CH%
	USE
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

344	WHEN ERROR IN
		GET #PR_ERNDED_DEF.CH%, REGARDLESS
	USE
		CONTINUE 350 IF ERR = 11%
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

	IF (PR_ERNDED_DEF::REPORTABLE_SWH <> PR_ERNDED_DEF::REPORTABLE_SUI)
	THEN
		IF (PR_ERNDED_DEF::REPORTABLE_SWH = "N" XOR &
			(PR_ERNDED_DEF::ETYPE <> "D" AND &
			PR_ERNDED_DEF::ETYPE <> "F"))
		THEN
			ADJUST_NEGATIVE$ = ADJUST_NEGATIVE$ + &
				"," + &
				PR_ERNDED_DEF::ETYPE + PR_ERNDED_DEF::CODE
		ELSE
			ADJUST_POSITIVE$ = ADJUST_POSITIVE$ + &
				"," + &
				PR_ERNDED_DEF::ETYPE + PR_ERNDED_DEF::CODE
		END IF
	END IF

	GOTO 344

350	!
	! Open REPORT file
	!
	GOTO ExitProgram &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "") <> CMC$_NORMAL

	!
	! Set Page Length
	!
	FORM_LENGTH% = UTL_REPORTX::PAGELEN

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field
	!	enters a value which causes the
	!	Transmittal to begin with a selected item. The item must be in
	!	agreement with the specific type of sort selection. For example, if the
	!	selection is made to print the transmittal in employee number order,
	!	a value in this field must be an employee number; if the selection
	!	is made to print the transmittal in alphabetical order, the value in
	!	this field must be the name of an employee, last name first.
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


	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field
	!	enters a value which causes the
	!	Transmittal to end with a selected item. The item must be in
	!	agreement with the specific type of sort selected. For example, if the
	!	selection is made to print the transmittal in location number
	!	order, a value in this field must be a location code; if the
	!	selection is made to print the transmittal in alphabetical order,
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


	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	.x Sort>Transmittal
	!	^*(03) Sort\*
	!	.b
	!	.lm +5
	!	The ^*Sort\* field
	!	selects the manner in which the transmittal
	!	will be sorted and printed.
	!	.b
	!	Valid sort codes are:
	!	.table 3,25
	!	.te
	!	^*NU\*  - Employee Number
	!	.te
	!	^*LN\*  - Location Number
	!	.te
	!	^*LO\*  - Location Alpha
	!	.te
	!	^*SO\*  - Alphabetical (last name first)
	!	.end table
	!	^*NOTE:\* If an ^*LN\* selection is made, there will be a slight delay while
	!	the system creates a sorted file.
	!	.lm -5
	!
	! Index:
	!	.x Transmittal>Sort
	!	.x Worksheet>Sort
	!	.x Sort>Worksheet
	!
	!--

	REPORT$ = REPORT$ + "$" + TRM$(UTL_REPORTX::OPTDEF(3%))

	!++
	! Abstract:FLD04
	!	^*(04) Form\*
	!	.b
	!	.lm +5
	!	The ^*Form\* field
	!	selects a specific Worksheet format.
	!	.b
	!	Different formats can be created in the Payroll -> Utility -> Format
	!	menu option. Several formats may be created, each with a unique name
	!	designation. The specific format needed can be accessed by entering the
	!	selected name for the specific format.
	!	.lm -5
	!
	! Index:
	!	.x Form>Transmittal
	!	.x Form>Worksheet
	!	.x Transmittal>Form
	!	.x Worksheet>Form
	!
	!--


	DEPT$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Department\*
	!	.b
	!	.lm +5
	!	The ^*Department\* field
	!	selects a specific department for which
	!	worksheets are to be printed.
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

	YYYY = EDIT$(UTL_REPORTX::OPTDEF(5%), 132%)
	!
	! SET VARIABLE "YYYY$" FOR THE PR_REG_TAXES FILE
	!
	YYYY$ = YYYY

	!++
	! Abstract:FLD06
	!	.x Year>SUTA Report
	!	^*(06) Year\*
	!	.b
	!	.lm +5
	!	The ^*Year\* field enters the year which
	!	is to be printed.
	!	.b
	!	The format for entry is YYYY.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x SUTA Report>Year
	!
	!--


	QTR = VAL%(EDIT$(UTL_REPORTX::OPTDEF(6%), 132%))

	!++
	! Abstract:FLD07
	!	.ts 55
	!	^*(07) Quarter	1 Digit\*
	!	.b
	!	.lm +5
	!	The ^*Quarter\* field enters the payroll
	!	quarter for which this report will print.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Quarter>SUTA Report
	!	.x SUTA Report>Quarter
	!
	!--

	IF LEFT(EDIT$(UTL_REPORTX::OPTDEF(7%), 132%), 1%) = "Y"
	THEN
		MAGNETIC_MEDIA = "X"
	ELSE
		MAGNETIC_MEDIA = " "
	END IF

	!++
	! Abstract:FLD08
	!	.ts 55
	!	^*(08) Quarter	1,2,3,4\*
	!	.b
	!	.lm +5
	!	The ^*Quarter\* field enters the payroll
	!	quarter for which this report will print.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Quarter>SUTA Report
	!	.x SUTA Report>Quarter
	!
	!--


	STATE_CODE$ = EDIT$(UTL_REPORTX::OPTDEF(8%), 132%)

	!++
	! Abstract:FLD09
	!	.ts 55
	!	^*(09) State to Print	2 Digits\*
	!	.b
	!	.lm +5
	!	The ^*State to Print\* field enters the
	!	state the forms will be printed for. Only one state
	!	can be printed at a time.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Quarter>SUTA Report
	!	.x SUTA Report>Quarter
	!
	!--


535	GET #PR_TAX_PROFILE.CH%, KEY #0% EQ "S" + STATE_CODE$
	PR_TAX_PROFILE_S1 = PR_TAX_PROFILE_S

537	!
	! Open earnings/deduction file in case we need it
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.OPN"
	USE
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

540	!
	! Open tax register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.OPN"
	USE
		FILENAME$ = "PR_REG_TAXES"
		CONTINUE HelpError
	END WHEN

	SELECT SORTBY$

	CASE "NU"
		K_NUM% = 0%

	CASE "NA"
		K_NUM% = 1%

	CASE "SN"
		K_NUM% = 3%

	CASE "LO"
		K_NUM% = 4%

	CASE "LN"
550		!
		! Sort Payroll Employee master file
		!
		CALL ENTR_3MESSAGE(SCOPE, "Creating temporary file", 17%)
		CALL ASSG_CHANNEL(PR_EMP_MASTER_TEMP.CH%, STAT%)

		WHEN ERROR IN
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
		USE
			FILENAME$ = "TEMP"
			CONTINUE HelpError
		END WHEN

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

	CASE ELSE
		K_NUM% = 2%

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
	! GOTO aligment routine
	!
	GOSUB Alignment

	GOSUB FindTotal

	%PAGE

2000	!*******************************************************************
	! Read through Payroll master file
	!*******************************************************************

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

	PAGE_COUNT = 1%
	UTL_REPORTX::LINENO = 0%
	TEST_COUNT% = 0%

	!
	! Print the top of the form
	!
	SMG_STATUS% = OUTP_FORMPRINT(UTL_REPORTX, &
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

	QTR_WAGES = 0.0

	GOTO 2010 IF COMP_STRING(PR_EMP_MASTER::DEPT, DEPT$) = 0% &
		AND DEPT$ <> ""

	SELECT SORTBY$

	CASE "NU"
		GOTO ExitTotal IF TO_ITEM$ <> "" AND &
			PR_EMP_MASTER::EMPNUM > TO_ITEM$

	CASE "NA"
		GOTO ExitTotal IF TO_ITEM$ <> "" AND &
			PR_EMP_MASTER::EMPNAME > TO_ITEM$

	CASE "SN"
		GOTO ExitTotal IF TO_ITEM$ <> "" AND &
			PR_EMP_MASTER::SSN > TO_ITEM$

	CASE "LO", "LN"
		GOTO ExitTotal IF TO_ITEM$ <> "" AND &
			PR_EMP_MASTER::LOCATION > TO_ITEM$

	CASE ELSE
		GOTO ExitTotal IF TO_ITEM$ <> "" AND &
			PR_EMP_MASTER::SORT > TO_ITEM$

	END SELECT


 !*******************************************************************
 !
 !2015	FIND #PR_REG_TAXES.CH%, &
 !		KEY #0% GE PR_EMP_MASTER::EMPNUM + "SW", &
 !		REGARDLESS
 !
 !
 !2020	GET #PR_REG_TAXES.CH%, REGARDLESS
 !
 !	IF (PR_REG_TAXES::EMPNUM = PR_EMP_MASTER::EMPNUM) AND &
 !		(PR_REG_TAXES::TTYPE = "SW")
 !	THEN
 !		IF PR_EMP_MASTER::SUI_SW <> "  "
 !		THEN
 !			TEST_CODE$ = PR_EMP_MASTER::SUI_SW
 !		ELSE
 !			TEST_CODE$ = PR_REG_TAXES::CODE
 !		END IF
 !
 !		QTR_WAGES = FUNC_ROUND(QTR_WAGES + &
 !			PR_REG_TAXES::REPORTABLE(QTR - 1%), 2%) &
 !			IF TEST_CODE$ = STATE_CODE$
 !
 !		GOTO 2020
 !	END IF
 !
 !*******************************************************************

2015	WHEN ERROR IN
		FIND #PR_REG_TAXES.CH%, &
			KEY #0% GE PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 2010 IF ERR = 155%
		FILENAME$ = "PR_REG_TAXES"
		CONTINUE HelpError
	END WHEN

	PR_TEMP_BUFFER% = 0%

2016	WHEN ERROR IN
		GET #PR_REG_TAXES.CH%, REGARDLESS
	USE
		CONTINUE 2020 IF ERR = 11%
		FILENAME$ = "PR_REG_TAXES"
		CONTINUE HelpError
	END WHEN

	GOTO 2020 IF PR_REG_TAXES::EMPNUM <> PR_EMP_MASTER::EMPNUM

	GOTO 2016 IF PR_REG_TAXES::TTYPE <> "SW"

2017	PR_EMP_MASTER::SUI_SW = "  "
	PR_EMP_MASTER::EMPNUM = PR_REG_TAXES::EMPNUM

 !
 ! What the heck is this doing here? We already have the employee record.
 !
 !	GET #PR_EMP_MASTER.CH%, KEY #0% EQ PR_REG_TAXES::EMPNUM, REGARDLESS

2018	PR_TEMP_BUFFER% = PR_TEMP_BUFFER% + 1%

	IF PR_EMP_MASTER::SUI_SW <> "  "
	THEN
		PR_TEMP_BUFFER(PR_TEMP_BUFFER%)::STCODE = &
			PR_EMP_MASTER::SUI_SW
	ELSE
		PR_TEMP_BUFFER(PR_TEMP_BUFFER%)::STCODE = PR_REG_TAXES::CODE
	END IF

	PR_TEMP_BUFFER(PR_TEMP_BUFFER%)::EMPNUM = PR_REG_TAXES::EMPNUM
	FOR I% = 0% TO 3%
		PR_TEMP_BUFFER(PR_TEMP_BUFFER%)::WAGES(I%) = &
			PR_REG_TAXES::REPORTABLE(I%)
		PR_TEMP_BUFFER(PR_TEMP_BUFFER%)::WKWRK(I%) = &
			PR_REG_TAXES::WKWRK(I%)
	NEXT I%

	GOTO 2016

2020	!*******************************************************************
	! Output all records buffered
	!*******************************************************************

	IF (ADJUST_POSITIVE$ <> "") OR (ADJUST_NEGATIVE$ <> "")
	THEN
		GOSUB CalculateAdjustment IF PR_TEMP_BUFFER%
	END IF

	FOR I% = 1% TO PR_TEMP_BUFFER%

		IF PR_TEMP_BUFFER(I%)::STCODE = STATE_CODE$
		THEN
			QTR_WAGES = PR_TEMP_BUFFER(I%)::WAGES(QTR - 1%)
		END IF
	NEXT I%

	PR_TEMP_BUFFER% = 0%

2025	GOTO 2010 IF QTR_WAGES = 0.0

	!
	! Increment body counter, check to see if need to page
	!
	IF TEST_COUNT% >= FRM_BODY_LENGTH%
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

	TOTAL_PAGE = TOTAL_PAGE + QTR_WAGES

	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO 2010

	%PAGE

 CalculateAdjustment:
2600	!*******************************************************************
	! Calculate any adjustment for this employee
	!*******************************************************************

	ADJUSTMENT(I%) = 0.0 FOR I% = 0% TO 3%
	ADJUSTMENT% = 0%

	!
	! Load up any adjustments
	!
	WHEN ERROR IN
		GET #PR_REG_ERNDED.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 2620
	END WHEN

2610	WHILE PR_REG_ERNDED::EMPNUM = PR_EMP_MASTER::EMPNUM

		IF PR_REG_ERNDED::ETYPE = "O"
		THEN
			ETYPE$ = "P"
		ELSE
			ETYPE$ = PR_REG_ERNDED::ETYPE
		END IF

		!
		! Positive adjustments
		!
		IF INSTR(1%, ADJUST_POSITIVE$, ETYPE$ + PR_REG_ERNDED::CODE)
		THEN
			IF ETYPE$ = "D" OR ETYPE$ = "F"
			THEN
				XSIGN = -1.0
			ELSE
				XSIGN = 1.0
			END IF

			ADJUSTMENT(I%) = FUNC_ROUND(ADJUSTMENT(I%) + &
				XSIGN * PR_REG_ERNDED::QTR_DOLL(I%), 2%) &
				FOR I% = 0% TO 3%
			ADJUSTMENT% = -1%
		END IF

		!
		! Negative adjustments
		!
		IF INSTR(1%, ADJUST_NEGATIVE$, ETYPE$ + PR_REG_ERNDED::CODE)
		THEN
			IF ETYPE$ = "D" OR ETYPE$ = "F"
			THEN
				XSIGN = 1.0
			ELSE
				XSIGN = -1.0
			END IF


			ADJUSTMENT(I%) = FUNC_ROUND(ADJUSTMENT(I%) + &
				XSIGN * PR_REG_ERNDED::QTR_DOLL(I%), 2%) &
				FOR I% = 0% TO 3%
			ADJUSTMENT% = -1%
		END IF

		WHEN ERROR IN
			GET #PR_REG_ERNDED.CH%, REGARDLESS
		USE
			CONTINUE 2620
		END WHEN
	NEXT

2620	!
	! Add adjustments into totals
	!
	RETURN IF ADJUSTMENT% = 0%

	FOR QTR1% = 0% TO 3%
		!
		! Calculate rate to spread out with
		!
		TOTAL = 0.0
		TOTAL = TOTAL + PR_TEMP_BUFFER(I%)::WAGES(QTR1%) &
			FOR I% = 1% TO PR_TEMP_BUFFER%

		IF TOTAL = 0.0
		THEN
			FACTOR = 0.0
		ELSE
			FACTOR = ADJUSTMENT(QTR1%) / TOTAL
		END IF

		!
		! Spread it out for all but last state
		!
		FOR I% = 1% TO PR_TEMP_BUFFER% - 1%
			CALC = FUNC_ROUND(PR_TEMP_BUFFER(I%)::WAGES(QTR1%) * &
				FACTOR, 2%)
			PR_TEMP_BUFFER(I%)::WAGES(QTR1%) = &
				PR_TEMP_BUFFER(I%)::WAGES(QTR1%) + &
				CALC
			ADJUSTMENT(QTR1%) = ADJUSTMENT(QTR1%) - CALC
		NEXT I%

		!
		! Slap anything left over in last state
		!
		PR_TEMP_BUFFER(PR_TEMP_BUFFER%)::WAGES(QTR1%) = &
			PR_TEMP_BUFFER(PR_TEMP_BUFFER%)::WAGES(QTR1%) + &
			ADJUSTMENT(QTR1%)

	NEXT QTR1%

	RETURN

	%PAGE

 ExitTotal:
	!
	! Print to the bottom of the body
	!
	CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
		FOR LOOP% = TEST_COUNT% + 1% TO FRM_BODY_LENGTH%

	TEST_COUNT% = 0%

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
	LINE_COUNT% = UTL_REPORTX::LINENO
	CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
		FOR LOOP% = LINE_COUNT% + 1% TO FORM_LENGTH%

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
	JUNK$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", "Do you want an alignment form?  " + &
		"Confirm then press <Do> ", &
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

	SMG_STATUS% = OUTP_FORMPRINT(UTL_REPORTX, &
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
		UNTIL TEST_COUNT% + 8% > FRM_BODY_LENGTH%

	!
	! Print lines to bottom of the form
	!
	CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
		FOR LOOP% = TEST_COUNT% + 1% TO FRM_BODY_LENGTH%

	TEST_COUNT% = 0%

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
	LINE_COUNT% = UTL_REPORTX::LINENO

	CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
		FOR LOOP% = LINE_COUNT% + 1% TO FORM_LENGTH%

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
	SMG_STATUS% = OUTP_FORMINIT(	PR_FORM.DEV$ + "PR_FORM", REPORT$, &
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

	FOR I% = 1% TO FORM_GROUP%

		SELECT FORM_GROUP(I%)::FGROUP

		CASE "FRM-TOP"
			FRM_TOP% = I%

		CASE "FRM-BOT"
			FRM_BOT% = I%

		CASE "FRM-BODY"
			FRM_BODY% = I%
			FRM_BODY_LENGTH% = FORM_GROUP(I%)::NUMBER

		CASE "FRM-LENGTH"
			FORM_LENGTH% = FORM_GROUP(I%)::NUMBER
		END SELECT

	NEXT I%

	FORM_LENGTH% = 60% IF FORM_LENGTH% = 0%

	RETURN

 PageBreak:
	!*******************************************************************
	! Page Break
	!*******************************************************************

	!
	! Print to the bottom of the body
	!
	CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
		FOR LOOP% = TEST_COUNT% + 1% TO FRM_BODY_LENGTH%

	TEST_COUNT% = 0%

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
 !	LINE_COUNT% = UTL_REPORTX::LINENO
 !	CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
 !		FOR LOOP% = LINE_COUNT% + 1% TO FORM_LENGTH%
	CALL OUTP_FORMFF(UTL_REPORTX)

	PAGE_COUNT = PAGE_COUNT + 1%
	UTL_REPORTX::LINENO = 0%
	TEST_COUNT% = 0%
	TOTAL_PAGE = 0.0

	!
	! Print the top of the form
	!
	SMG_STATUS% = OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_TOP%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	RETURN

	%PAGE

3000	!*******************************************************************
	! Calculate form totals
	!*******************************************************************
 FindTotal:

	FORM_TOTAL = 0.0

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
		CONTINUE ExitTotal IF ERR = 155% OR ERR = 11%
		CONTINUE HelpError
	END WHEN

3010	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, REGARDLESS
	USE
		CONTINUE 3090 IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	QTR_WAGES = 0.0

	GOTO 3010 IF COMP_STRING(PR_EMP_MASTER::DEPT, DEPT$) = 0% &
		AND DEPT$ <> ""

	SELECT SORTBY$

	CASE "NU"
		GOTO 3090 IF TO_ITEM$ <> "" AND &
			PR_EMP_MASTER::EMPNUM > TO_ITEM$

	CASE "NA"
		GOTO 3090 IF TO_ITEM$ <> "" AND &
			PR_EMP_MASTER::EMPNAME > TO_ITEM$

	CASE "SN"
		GOTO 3090 IF TO_ITEM$ <> "" AND &
			PR_EMP_MASTER::SSN > TO_ITEM$

	CASE "LO", "LN"
		GOTO 3090 IF TO_ITEM$ <> "" AND &
			PR_EMP_MASTER::LOCATION > TO_ITEM$

	CASE ELSE
		GOTO 3090 IF TO_ITEM$ <> "" AND &
			PR_EMP_MASTER::SORT > TO_ITEM$

	END SELECT

3015	WHEN ERROR IN
		FIND #PR_REG_TAXES.CH%, &
			KEY #0% GE PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 3010 IF ERR = 155%
		FILENAME$ = "PR_REG_TAXES"
		CONTINUE HelpError
	END WHEN

	PR_TEMP_BUFFER% = 0%

3016	WHEN ERROR IN
		GET #PR_REG_TAXES.CH%, REGARDLESS
	USE
		CONTINUE 3020 IF ERR = 11%
		FILENAME$ = "PR_REG_TAXES"
		CONTINUE HelpError
	END WHEN

	GOTO 3016 IF PR_REG_TAXES::TTYPE <> "SW"

	GOTO 3020 IF PR_REG_TAXES::EMPNUM <> PR_EMP_MASTER::EMPNUM

3017	PR_EMP_MASTER::SUI_SW = "  "
	PR_EMP_MASTER::EMPNUM = PR_REG_TAXES::EMPNUM

 !
 ! And here it is again? We already have the employee record
 !
 !	GET #PR_EMP_MASTER.CH%, KEY #0% EQ PR_REG_TAXES::EMPNUM, REGARDLESS

3018	PR_TEMP_BUFFER% = PR_TEMP_BUFFER% + 1%

	IF PR_EMP_MASTER::SUI_SW <> "  "
	THEN
		PR_TEMP_BUFFER(PR_TEMP_BUFFER%)::STCODE = &
			PR_EMP_MASTER::SUI_SW
	ELSE
		PR_TEMP_BUFFER(PR_TEMP_BUFFER%)::STCODE = PR_REG_TAXES::CODE
	END IF

	PR_TEMP_BUFFER(PR_TEMP_BUFFER%)::EMPNUM = PR_REG_TAXES::EMPNUM
	FOR I% = 0% TO 3%
		PR_TEMP_BUFFER(PR_TEMP_BUFFER%)::WAGES(I%) = &
			PR_REG_TAXES::REPORTABLE(I%)
		PR_TEMP_BUFFER(PR_TEMP_BUFFER%)::WKWRK(I%) = &
			PR_REG_TAXES::WKWRK(I%)
	NEXT I%

	GOTO 3016

3020	!*******************************************************************
	! Output all records buffered
	!*******************************************************************

	IF (ADJUST_POSITIVE$ <> "") OR (ADJUST_NEGATIVE$ <> "")
	THEN
		GOSUB CalculateAdjustment IF PR_TEMP_BUFFER%
	END IF

	FOR I% = 1% TO PR_TEMP_BUFFER%

		IF PR_TEMP_BUFFER(I%)::STCODE = STATE_CODE$
		THEN
			FORM_TOTAL = FUNC_ROUND(FORM_TOTAL + &
				PR_TEMP_BUFFER(I%)::WAGES(QTR - 1%), 2%)
		END IF
	NEXT I%

	PR_TEMP_BUFFER% = 0%

	GOTO 3010

3090	RETURN

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

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.HB"
	MAP (PR_REG_TAXES)	PR_REG_TAXES_CDD	PR_REG_TAXES

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.HB"
	MAP (PR_TAX_PROFILE_F1)	PR_TAX_PROFILE_F_CDD	PR_TAX_PROFILE_F1

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_S.HB"
	MAP (PR_TAX_PROFILE_S1)	PR_TAX_PROFILE_S_CDD	PR_TAX_PROFILE_S1

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION


	!
	! COMMON VARIABLES
	!
	COMMON (VARIABLES) REAL QTR_WAGES, FORM_TOTAL, TOTAL_PAGE, &
		STRING MAGNETIC_MEDIA, YYYY, &
		INTEGER QTR

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

	CASE "PR_EMP_MASTER::PHONE"
		TEXTVALUE$ = PRNT_PHONE(PR_EMP_MASTER::PHONE, 0%)

	CASE "PR_EMP_MASTER::SSN"
		TEXTVALUE$ = PRNT_SSN(PR_EMP_MASTER::SSN, 0%)

	CASE "PR_EMP_MASTER::SSN_1"
		TEXTVALUE$ = MID(PRNT_SSN(PR_EMP_MASTER::SSN, 0%), 1%, 3%)

	CASE "PR_EMP_MASTER::SSN_2"
		TEXTVALUE$ = MID(PRNT_SSN(PR_EMP_MASTER::SSN, 0%), 5%, 2%)

	CASE "PR_EMP_MASTER::SSN_3"
		TEXTVALUE$ = MID(PRNT_SSN(PR_EMP_MASTER::SSN, 0%), 8%, 4%)

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

	CASE "PR_EMP_MASTER::REHIRE_FLAG"
		TEXTVALUE$ = PR_EMP_MASTER::REHIRE_FLAG

	!************************************************************
	! PR TAX PROFILE (FEDERAL)
	!************************************************************

	CASE "PR_TAX_PROFILE_F::REPNO"
		TEXTVALUE$ = PR_TAX_PROFILE_F1::REPNO

	CASE "PR_TAX_PROFILE_F::MIN_WAGE"
		REALVALUE = PR_TAX_PROFILE_F1::MIN_WAGE

	!************************************************************
	! PR TAX PROFILE (STATE)
	!************************************************************

	CASE "PR_TAX_PROFILE_S::REPNO"
		TEXTVALUE$ = PR_TAX_PROFILE_S1::REPNO

	CASE "PR_TAX_PROFILE_S::SUTANO"
		TEXTVALUE$ = PR_TAX_PROFILE_S1::SUTANO

	!
	! Idaho gives you a lot of dashed numbers, but doesnt want
	! you to give them all back for some dumb reason.
	!
	CASE "STATE_ID_NUMBER"
		TEMP$ = TRM$(PR_TAX_PROFILE_S1::REPNO)
		I% = INSTR(1%, TEMP$ + "-", "-")
		TEXTVALUE$ = STRING$(9% - I%, A"0"B) + &
			LEFT(TEMP$, I% - 1%)

	!************************************************************
	! Location file
	!************************************************************
	CASE "UTL_LOCATION::LOCNAME"
		TEXTVALUE$ = UTL_LOCATION::LOCNAME

	CASE "UTL_LOCATION::ADDRESS1"
		TEXTVALUE$ = UTL_LOCATION::ADDRESS1

	CASE "UTL_LOCATION::ADDRESS2"
		TEXTVALUE$ = UTL_LOCATION::ADDRESS2

	CASE "UTL_LOCATION::CITY"
		TEXTVALUE$ = UTL_LOCATION::CITY

	CASE "UTL_LOCATION::STATE"
		TEXTVALUE$ = UTL_LOCATION::STATE

	CASE "UTL_LOCATION::ZIP"
		TEXTVALUE$ = UTL_LOCATION::ZIP

	!************************************************************
	! Non fielded values
	!************************************************************

	CASE "PAGE"
		REALVALUE = PAGE_COUNT
		TEXTVALUE$ = NUM1$(PAGE_COUNT)

	CASE "QTR_WAGES"
		!
		! Figure the Quarter Wages
		!
		REALVALUE = QTR_WAGES

	CASE "TOTAL"
		REALVALUE = FORM_TOTAL

	CASE "TOTAL_PAGE"
		REALVALUE = TOTAL_PAGE

	CASE "MAGNETIC_MEDIA"
		TEXTVALUE$ = MAGNETIC_MEDIA

	CASE "QTR"
		TEXTVALUE$ = NUM1$(QTR)
		REALVALUE = QTR

	CASE "YEAR"
		TEXTVALUE$ = SEG$(YYYY, 3%, 4%)

	CASE "YEAR_ALL"
		TEXTVALUE$ = YYYY

	END SELECT


	END SUB
