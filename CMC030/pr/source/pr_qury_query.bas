1	%TITLE "QUERY - Payroll Query"
	%SBTTL "PR_QURY_QUERY"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 1988 BY
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
	! ID:PRQURY
	!
	! Abstract:HELP
	!	.p
	!	The ^*Payroll Query\* option
	!	makes on-line inquiries on any selected employee.
	!
	! Index:
	!	.x Query>Payroll
	!	.X Payroll>Query
	!
	! Option:
	!	PR_QURY_QUERY$PAY$HELP
	!	PR_QURY_QUERY$TAX$HELP
	!	PR_QURY_QUERY$CHK$HELP
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_QURY_QUERY/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_QURY_QUERY,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_QURY_QUERY.OBJ;*
	!
	! Author:
	!
	!	02/22/88 - Robert Peterson
	!
	! Modification history:
	!
	!	03/27/89 - Kevin Handy
	!		Fixed bug where it was using CHW instead of CWH
	!		for the city witholding table.
	!
	!	04/03/89 - Kevin Handy
	!		Added code for SI type.  Changed loop from hard-coded
	!		to use length of SUBJECT_TYPE_TABLE$.
	!
	!	05/19/89 - Kevin Handy
	!		Fixed the "Taxes" query so it would show more
	!		than just "FW" and "FI" types.
	!
	!	06/22/89 - Kevin Handy
	!		Fixed problem where it tried to use cross between
	!		pay gross and tax gross instead of using the gross
	!		stored for each tax (tax report).
	!
	!	11/20/89 - Kevin Handy
	!		Modified so SU types will show up in the query.
	!
	!	01/05/90 - Kevin Handy
	!		Cleaned up error trapping.
	!
	!	01/05/90 - Kevin Handy
	!		NOTE: If there is nothing in an employee's
	!		pay file, then the Check option will not
	!		display the employee.
	!
	!	04/06/90 - Kevin Handy
	!		Fixed bug where error at 7100 would not trap
	!		correctly, causing it to stop looking through
	!		folders and come up with a nasty error message.
	!
	!	01/01/91 - Kevin Handy
	!		Modified to handle to layout for PR_REG_TAXES.
	!
	!	01/11/91 - Craig Tanner
	!		Added YYYY$ to some filename$ in error trapping.
	!
	!	01/23/91 - Kevin Handy
	!		Removed unecessary open of PR_EMP_STATUS.MAS.
	!
	!	04/08/91 - Kevin Handy
	!		Added PR_READ_DATES information on the bottom of
	!		the screen.
	!
	!	04/15/91 - Kevin Handy
	!		Modification to pull Repaint out of error trapping
	!		so that PR_READ_DATES would not crash horribly.
	!
	!	06/03/91 - Kevin Handy
	!		Removed stupid code in error trapping.
	!
	!	09/19/91 - Kevin Handy
	!		Increased number of files from 200 to 300.
	!
	!	12/18/91 - Kevin Handy
	!		Removed code for PR_EMP_STD_ERNDED which was opened
	!		and never used.
	!
	!	12/18/91 - Kevin Handy
	!		Modified to ignore "A" types in PR_PAY files.
	!
	!	03/13/92 - Kevin Handy
	!		Removed duplicate error trap (check)
	!
	!	04/22/92 - Kevin Handy
	!		Increased max from 300 to 600.
	!
	!	04/14/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	09/20/93 - Kevin Handy
	!		Increased file dimension from 600 to 1000. (dwi)
	!
	!	04/12/95 - Kevin Handy
	!		Update to V3.6 source standards.
	!		Change SMG_QUERY to SMG_QUERY%
	!
	!	04/19/95 - Kevin Handy
	!		Modify to fix in 80 columns better.
	!
	!	07/10/95 - Kevin Handy
	!		Fix problems in the CheckReg option where it
	!		would open files multiple times for one
	!		employee, when it didn't need to.
	!
	!	07/10/95 - Kevin Handy
	!		Moved handling of "A" record types in
	!		CheckReg option.
	!
	!	11/08/95 - Kevin Handy
	!		Modified file dimension from 1000 to 2000.
	!
	!	07/15/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/13/96 - Kevin Handy
	!		Lose extra '&' before 'than'.
	!
	!	03/03/97 - Kevin Handy
	!		Increase dimension from 2000 to 3000.
	!
	!	03/12/97 - Kevin Handy
	!		Lose code testing TAX_TYPE$, which is never set
	!		to anything.
	!		Don't need SUBJECT_TYPE_TABLE Variable.
	!		Dimension several variables.
	!		Handle FH Code.
	!		Fix bug in looking at ',2' in fica instead of ',1'.
	!
	!	05/08/97 - Kevin Handy
	!		Use OUTP_INITFORM function
	!
	!	05/08/97 - Kevin Handy
	!		Fix bug in zeroing arrays at 6010
	!
	!	05/09/97 - Kevin Handy
	!		Lose several SMG_BLANK pop's
	!
	!	08/25/97 - Kevin Handy
	!		Clean up (Check)
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"

	!
	! Map file
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP (PR_TRN_PAY)	PR_TRN_PAY_CDD	PR_TRN_PAY
	MAP (PR_HIS_PAY)	PR_TRN_PAY_CDD	PR_HIS_PAY

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.HB"
	MAP (PR_TRN_DED)	PR_TRN_DED_CDD	PR_TRN_DED
	MAP (PR_HIS_DED)	PR_TRN_DED_CDD	PR_HIS_DED

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.HB"
	MAP (PR_TRN_CHECK)	PR_TRN_CHECK_CDD	PR_TRN_CHECK
	MAP (PR_HIS_CHECK)	PR_TRN_CHECK_CDD	PR_HIS_CHECK

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.HB"
	MAP (PR_REG_TAXES)	PR_REG_TAXES_CDD	PR_REG_TAXES

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.HB"
	MAP (PR_REG_ERNDED)	PR_REG_ERNDED_CDD	PR_REG_ERNDED

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP (PR_ERNDED_DEF)	PR_ERNDED_DEF_CDD	PR_ERNDED_DEF

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_RATE.HB"
	MAP	(PR_EMP_RATE)	PR_EMP_RATE_CDD	PR_EMP_RATE

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_DATES.HB"

	!
	! This common area must be mapped in both the main program and
	! in PR_MAIN_EMPLOYEE.
	!
	COM (CH_PR_EMP_MASTER) &
		PR_EMP_MASTER.CH%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION OUTP_INITFORM

	!
	! Declare constants
	!
	DECLARE INTEGER CONSTANT MAX_CODE = 10%
	DECLARE INTEGER CONSTANT MAX_ITEM = 10%
	DECLARE PR_EMP_DATES_CDD CURRENT_DATES

	!
	! Dimension
	!
	DIM DATA_FILE$(3000%)
	DIM CODE%(MAX_CODE), CODE$(MAX_CODE, MAX_ITEM), &
		EMP_YTD_WAGES(MAX_CODE, MAX_ITEM), &
		EMP_QTD_WAGES(MAX_CODE, MAX_ITEM), &
		EMP_YTD_TAXES(MAX_CODE, MAX_ITEM), &
		EMP_QTD_TAXES(MAX_CODE, MAX_ITEM), &
		EMP_YTD_WKWRK(MAX_CODE, MAX_ITEM), &
		EMP_QTD_WKWRK(MAX_CODE, MAX_ITEM)
	DIM LINE_TOTAL(10%)

	%PAGE

	ON ERROR GOTO 19000

 !	SUBJECT_TYPE_TABLE$ = "FWH*FIE*FHE*SWH*OST*CWH*DWH*EWH*SWC*SUI*"
	TAX_TYPE_TABLE$ = "FW!FI!FH!SW!SX!CW!DW!EW!SI!SU"

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	RESTORE_PROGRAM$ = SCOPE::PRG_PROGRAM
	RESTORE_IDENT$ = SCOPE::PRG_IDENT
	RESTORE_ITEM$ = SCOPE::PRG_ITEM

	REPORT$(1%) = "PRQUR1"
	REPORT$(2%) = "PRQUR2"
	REPORT$(3%) = "PRQUR3"

	!
	! Look up device transaction file
	!
	CALL READ_DEVICE("PR_TRN_PAY", PR_TRN_PAY.DEV$, STAT%)
	CALL READ_DEVICE("PR_HIS_PAY", PR_HIS_PAY.DEV$, STAT%)

	!
	! Look up all journal payroll files
	!
	CALL PR_FIND_DETAILFILE("00000101", &
		"99991231", &
		PR_TRN_PAY.DEV$, &
		PR_HIS_PAY.DEV$, &
		DATA_FILE$())

	DATA_FILE% = VAL%(DATA_FILE$(0%))

300	!
	! Open Employee master File
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

330	!
	! Open Employee Rate File
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_RATE.OPN"
	USE
		CONTINUE 340 IF ERR = 5%
		FILENAME$ = "PR_EMP_RATE"
		CONTINUE HelpError
	END WHEN

340	!
	! Open Pay Deduction definition File
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"
	USE
		CONTINUE 400 IF ERR = 5%
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

400	!*****************************************************************

	GOSUB Initialize

900	!
	! Create a display window
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		78%, &
		SMG_QUERY%, &
		SMG$M_BORDER &
	)

	!
	! Label the display
	!
	SMG_STATUS% = SMG$LABEL_BORDER(SMG_QUERY%, &
		"PR Query for " + TRM$(SCOPE::PRG_COMPANY))

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, "Employee #", 2%, 2%)

	GOSUB Repaint

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_QUERY%, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)

	%PAGE

1000	!******************************************************************
	! Main option menu
	!******************************************************************

1100	!
	! Enter options
	!
	SCOPE::PRG_ITEM = RESTORE_ITEM$
	SCOPE::PRG_IDENT = RESTORE_IDENT$
	SCOPE::PRG_PROGRAM = RESTORE_PROGRAM$

	GOSUB Repaint

	OPTLIST$ = "Find Next Help Payded Taxes Checkreg eXit"

	OPT$ = ENTR_3OPTION(SCOPE, "COMMAND", OPTLIST$, OPT%, 0%)

	SELECT SCOPE::SCOPE_EXIT

	!
	! Control c
	!
	CASE 3%
		GOTO 1000

	!
	! Exit key
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	END SELECT

	SELECT OPT$

	CASE "F"

1120		SCOPE::PRG_ITEM = "FLD001"

	!++
	! Abstract:FLD001
	!	^*Employee Number\*
	!	.p
	!	The ^*Employee Number\* is the number assigned to each employee to represent
	!	them when dealing with the company. It may contain up to ten (10) alphanumeric
	!	characters.
	!
	! Index:
	!	.x Employee Number>Payroll Query
	!	.x Payroll Query>Employee Number
	!
	! Required:
	!--

		PR_EMP_MASTER::EMPNUM = ENTR_3STRING(SCOPE, SMG_QUERY%, "2;12", &
			"Employee #", PR_EMP_MASTER::EMPNUM, FLAG%, "'E", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT
		!
		! List Choices
		!
		CASE SMG$K_TRM_F14
			TEMP$ = PR_EMP_MASTER::EMPNUM

			IF MAIN_WINDOW(PR_MAIN_EMPLOYEE.ID, "VX  ") <> 1%
			THEN
				PR_EMP_MASTER::EMPNUM = TEMP$

				GOTO 1120
			ELSE
				GOTO 2000
			END IF

		!
		! Control c
		!
		CASE 3%
			GOTO 1120

		!
		! Exit key
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT

		IF PR_EMP_MASTER::EMPNUM = "??????????"
		THEN
			GOTO 1000
		END IF

2000		WHEN ERROR IN
			FIND #PR_EMP_MASTER.CH%, &
				KEY #0% GE PR_EMP_MASTER::EMPNUM, &
				REGARDLESS

			GET #PR_EMP_MASTER.CH%, REGARDLESS
		USE
 !			GOSUB Initialize

			PR_EMP_MASTER::EMPNUM	= STRING$(10%, A"?"B)
			PR_EMP_MASTER::EMPNAME	= STRING$(40%, A"?"B)
			PR_EMP_MASTER::ADD1	= STRING$(25%, A"?"B)
			PR_EMP_MASTER::ADD2	= STRING$(21%, A"?"B)
			PR_EMP_MASTER::CITY	= STRING$(15%, A"?"B)
			PR_EMP_MASTER::STATE	= STRING$(2%, A"?"B)
			PR_EMP_MASTER::COUNTRY	= STRING$(8%, A"?"B)
			PR_EMP_MASTER::ZIP	= STRING$(10%, A"?"B)
			PR_EMP_MASTER::PHONE	= STRING$(10%, A"?"B)
			PR_EMP_MASTER::SORT	= STRING$(15%, A"?"B)

			CONTINUE 1100
		END WHEN

	CASE "N"
3000		WHEN ERROR IN
			GET #PR_EMP_MASTER.CH%, REGARDLESS
		USE
			IF ERR = 11%
			THEN
				CALL ENTR_3MESSAGE(SCOPE, "End of File", 0%)
				CONTINUE 1100
			END IF
			FILENAME$ = "PR_EMP_MASTER"
			CONTINUE HelpError
		END WHEN

	CASE "H"
		CALL HELP_34MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, &
			SCOPE::PRG_PROGRAM, "", SCOPE::PRG_ITEM)

	CASE "P"
		REPORT_LOOP% = 1%
		SCOPE::PRG_PROGRAM = TRM$(SCOPE::PRG_PROGRAM) + "$PAY"

		GOSUB ReportSetting

		IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F10) OR &
			(SCOPE::SCOPE_EXIT = SMG$K_TRM_CTRLC) OR &
			(SCOPE::SCOPE_EXIT = SMG$K_TRM_F8)
		THEN
			GOTO 1100
		END IF

		IF UTL_REPORTX::STAT = 0%
		THEN
			GOSUB PayDed
		END IF

		!
		! Finish up
		!
		CALL OUTP_FINISH(UTL_REPORTX)

		GOTO 900

	CASE "T"
		REPORT_LOOP% = 2%
		SCOPE::PRG_PROGRAM = TRM$(SCOPE::PRG_PROGRAM) + "$TAX"

		GOSUB ReportSetting

		IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F10) OR &
			(SCOPE::SCOPE_EXIT = SMG$K_TRM_CTRLC) OR &
			(SCOPE::SCOPE_EXIT = SMG$K_TRM_F8)
		THEN
			GOTO 1100
		END IF

		IF UTL_REPORTX::STAT = 0%
		THEN
			GOSUB Taxes
		END IF

		!
		! Finish up
		!
		CALL OUTP_FINISH(UTL_REPORTX)

		GOTO 900

	CASE "C"
		REPORT_LOOP% = 3%
		SCOPE::PRG_PROGRAM = TRM$(SCOPE::PRG_PROGRAM) + "$CHK"

		GOSUB ReportSetting

		IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F10) OR &
			(SCOPE::SCOPE_EXIT = SMG$K_TRM_CTRLC) OR &
			(SCOPE::SCOPE_EXIT = SMG$K_TRM_F8)
		THEN
			GOTO 1100
		END IF

		IF UTL_REPORTX::STAT = 0%
		THEN
			GOSUB Check
		END IF

		!
		! Finish up
		!
		CALL OUTP_FINISH(UTL_REPORTX)

		GOTO 900

	CASE "X"
		GOTO ExitProgram

	END SELECT

	GOTO 1100

	%PAGE

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

 Repaint:
3500	!**************************************************************
	! Repaint employee name
	!**************************************************************

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		PR_EMP_MASTER::EMPNUM, 2%, 12%,, SMG$M_BOLD)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		PR_EMP_MASTER::EMPNAME, 6%, 3%,, SMG$M_BOLD)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		PR_EMP_MASTER::ADD1, 7%, 3%,, SMG$M_BOLD)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		PR_EMP_MASTER::ADD2, 8%, 3%,, SMG$M_BOLD)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		PR_EMP_MASTER::CITY + " " + PR_EMP_MASTER::STATE + " " + &
		PR_EMP_MASTER::ZIP, 9%, 3%,, SMG$M_BOLD)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		PR_EMP_MASTER::COUNTRY, 10%, 3%,, SMG$M_BOLD)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		PRNT_PHONE(PR_EMP_MASTER::PHONE, 0%), 11%, 3%,, SMG$M_BOLD)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		PR_EMP_MASTER::SORT, 12%, 3%,, SMG$M_BOLD)

	CALL PR_READ_DATES(PR_EMP_MASTER::EMPNUM, &
		"AC", DATE_TODAY, 1%, CURRENT_DATES)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		"Chronicle AC Start: " + &
		PRNT_DATE(CURRENT_DATES::DATEBEGIN, 8%) + "  End: " + &
		PRNT_DATE(CURRENT_DATES::DATEEND, 8%) + "  Descr: " + &
		CURRENT_DATES::DESCR, &
		18%, 1%)

	RETURN

	%Page

 Initialize:
3600	!*******************************************************************
	! Set Initialize values
	!*******************************************************************
	PR_EMP_MASTER::EMPNUM	= STRING$(10%, A"?"B)
	PR_EMP_MASTER::EMPNAME	= STRING$(40%, A"?"B)
	PR_EMP_MASTER::ADD1	= STRING$(25%, A"?"B)
	PR_EMP_MASTER::ADD2	= STRING$(21%, A"?"B)
	PR_EMP_MASTER::CITY	= STRING$(15%, A"?"B)
	PR_EMP_MASTER::STATE	= STRING$(2%, A"?"B)
	PR_EMP_MASTER::COUNTRY	= STRING$(8%, A"?"B)
	PR_EMP_MASTER::ZIP	= STRING$(10%, A"?"B)
	PR_EMP_MASTER::PHONE	= STRING$(10%, A"?"B)
	PR_EMP_MASTER::SORT	= STRING$(15%, A"?"B)

	RETURN

	%Page

 ReportSetting:
	!****************************************************************
	! Ask for report settings
	!****************************************************************
	CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)

4000	!******************************************************************
	! Set up the report settings screen
	!******************************************************************

	GOTO ReportSetting1 &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$(REPORT_LOOP%), "") <> &
		CMC$_NORMAL

	!
	! Erase option and message window
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

 ReportSetting1:

	RETURN

	%PAGE

 PayDed:

	!++
	! Abstract:PAY$HELP
	!	^*Payded\*
	!	.p
	!	The ^*Payded\* function
	!	accesses the Payments and Deductions section of the Payroll Query file,
	!	which contains information on every type of payment (including non compensation
	!	and memo payments) and every type of deduction for each employee by calendar
	!	quarter. The data stored includes:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Amount (paid or deducted)
	!	.le
	!	Regular hours worked (per payment type)
	!	.le
	!	Overtime hours worked (per payment type)
	!	.le
	!	Units produced (per payment type)
	!	.els
	!	.lm -15
	!
	! Index:
	!	.x Payded>Query
	!	.x Query>Payded
	!	.x Payment>Query
	!	.x Deduction>Query
	!
	!--

	WORK_EMPNUM$ = PR_EMP_MASTER::EMPNUM

	WORK_EMPNAME$ = PR_EMP_MASTER::EMPNAME

	TEST_TYPE$ = ""

	TOTAL(LOOP%) = 0.0 FOR LOOP% = 1% TO 6%

	YYYY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	!++
	! Abstract:PAY$FLD01
	!	^*(01) Payroll Year\*
	!	.p
	!	The ^*Payroll Year\* field specifies the year for which this report is
	!	to print. The format for entry is YYYY.
	!
	! Index:
	!	.x Payroll Year>Query
	!	.x Year>Query
	!	.x Query>Payroll Year
	!
	!--

	QTR% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(1%), 132%))
	!++
	! Abstract:PAY$FLD02
	!	^*(02) Quarter\*
	!	.p
	!	The ^*Quarter\* field specifies the accounting quarter
	!	for which this report will print. This field requires an entry and will
	!	accommodate a one digit number.
	!
	! Index:
	!	.x Quarter>Query
	!	.x Query>Quarter
	!
	!--

	CLOSE PR_REG_ERNDED.CH%

	CALL ASSG_FREECHANNEL(PR_REG_ERNDED.CH%)

5000	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.OPN"
	USE
		CONTINUE 5350 IF ERR = 5%
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

	!
	! Set up titles and whatnot
	!
	TITLE$(1%) = "Pay Deduction Dump - " + NUM1$(QTR%) + &
		MID("stndrdth", QTR% * 2% - 1%, 2%) + " Quarter"
	TITLE$(2%) = "For the year of " + YYYY$
	TITLE$(3%) = ""

	TITLE$(4%) = "                                                      " + &
		"   -----------Year To Date----------         " + &
		" --------Quarter to Date---------"

	TITLE$(5%) = "Emp #      Name                           Type  Code  " + &
		"      Dollars       Hours      Units         " + &
		"    Dollars      Hours      Units"

	TITLE$(6%) = ""

5040	WHEN ERROR IN
		FIND #PR_REG_ERNDED.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 5350 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

5050	WHEN ERROR IN
		GET #PR_REG_ERNDED.CH%, REGARDLESS
	USE
		CONTINUE 5090 IF ERR = 11%
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

	GOTO 5090 IF PR_EMP_MASTER::EMPNUM <> PR_REG_ERNDED::EMPNUM

	IF TEST_TYPE$ <> PR_REG_ERNDED::ETYPE AND TEST_TYPE$ <> ""
	THEN
		TEXT$ = SPACE$(50%) + "Total  " + &
			FORMAT$(TOTAL(1%), "###,###.##  ") + &
			FORMAT$(TOTAL(2%), "###,###.## ") + &
			FORMAT$(TOTAL(3%), "###,###.##          ") + &
			FORMAT$(TOTAL(4%), "###,###.## ") + &
			FORMAT$(TOTAL(5%), "###,###.## ") + &
			FORMAT$(TOTAL(6%), "###,###.## ")

		TOTAL(LOOP%) = 0.0 FOR LOOP% = 1% TO 6%

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		GOTO 5350 IF UTL_REPORTX::STAT
	END IF

	TEST_TYPE$ = PR_REG_ERNDED::ETYPE

	EMP(LOOP%) = 0.0 FOR LOOP% = 1% TO 6%

	EMP(1%) = EMP(1%) + PR_REG_ERNDED::QTR_DOLL(LOOP%) &
		FOR LOOP% = 0% TO (QTR% - 1%)
	EMP(2%) = EMP(2%) + PR_REG_ERNDED::REG_HRS(LOOP%) + PR_REG_ERNDED::PRE_HRS(LOOP%) &
		FOR LOOP% = 0% TO (QTR% - 1%)
	EMP(3%) = EMP(3%) + PR_REG_ERNDED::UNITS(LOOP%) &
		FOR LOOP% = 0% TO (QTR% - 1%)
	EMP(4%) = PR_REG_ERNDED::QTR_DOLL(QTR% - 1%)
	EMP(5%) = PR_REG_ERNDED::REG_HRS(QTR% - 1%) + &
		PR_REG_ERNDED::PRE_HRS(QTR% - 1%)
	EMP(6%) = PR_REG_ERNDED::UNITS(QTR% - 1%)

	TEXT$ = LEFT(WORK_EMPNUM$ + SPACE$(10%), 10%) + " " + &
		LEFT(WORK_EMPNAME$ + SPACE$(30%), 30%) + "  " + &
		PR_REG_ERNDED::ETYPE + "     " + &
		PR_REG_ERNDED::CODE + "      " + &
		FORMAT$(EMP(1%), "###,###.##  ") + &
		FORMAT$(EMP(2%), "###,###.## ") + &
		FORMAT$(EMP(3%), "###,###.##          ") + &
		FORMAT$(EMP(4%), "###,###.## ") + &
		FORMAT$(EMP(5%), "###,###.## ") + &
		FORMAT$(EMP(6%), "###,###.## ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO 5350 IF UTL_REPORTX::STAT

	TOTAL(LOOP%) = TOTAL(LOOP%) + EMP(LOOP%) FOR LOOP% = 1% TO 6%

	WORK_EMPNUM$, WORK_EMPNAME$ = ""

	GOTO 5050

5090	IF TEST_TYPE$ <> ""
	THEN
		TEXT$ = SPACE$(50%) + "Total  " + &
			FORMAT$(TOTAL(1%), "###,###.##  ") + &
			FORMAT$(TOTAL(2%), "###,###.## ") + &
			FORMAT$(TOTAL(3%), "###,###.##          ") + &
			FORMAT$(TOTAL(4%), "###,###.## ") + &
			FORMAT$(TOTAL(5%), "###,###.## ") + &
			FORMAT$(TOTAL(6%), "###,###.## ")

		TOTAL(LOOP%) = 0.0 FOR LOOP% = 1% TO 6%

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		GOTO 5350 IF UTL_REPORTX::STAT
	END IF

5350	RETURN

	%Page

 Taxes:
	!*****************************************************************
	! Print taxes report
	!*****************************************************************

	!++
	! Abstract:TAX$HELP
	!	^*Taxes\*
	!	.p
	!	The ^*Taxes\* function
	!	accesses the Taxes section of the Payroll Query file which contains
	!	information on every possible type of tax for each employee by calendar
	!	quarter. The data stored includes:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Tax Type (FICA, Federal Withholding, etc.)
	!	.le
	!	Tax Codes (for State and Local Taxes)
	!	.le
	!	Subject Wages (for each tax type)
	!	.le
	!	Tax Amount Withheld (for each tax type)
	!	.le
	!	Weeks worked (for each applicable tax type)
	!	.els
	!
	! Index:
	!	.x Taxes>Query
	!	.x Query>Taxes
	!
	!--

	WORK_EMPNUM$ = PR_EMP_MASTER::EMPNUM

	WORK_EMPNAME$ = PR_EMP_MASTER::EMPNAME

	!
	! Other Variables
	!

	YYYY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	!++
	! Abstract:TAX$FLD01
	!	^*(01) Payroll Year\*
	!	.p
	!	The ^*Payroll Year\* field specifies the year for which this report
	!	is to print.
	!	.p
	!	The format for entry is YYYY.
	!
	! Index:
	!	.x Payroll Year>Query
	!	.x Year>Query
	!	.x Query>Payroll Year
	!
	!--

	QTR% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(1%), 132%))
	!++
	! Abstract:TAX$FLD02
	!	^*(02) Quarter\*
	!	.p
	!	The ^*Quarter\* field specifies the accounting quarter for which
	!	this report will print.
	!	.p
	!	This field requires an entry. The field will accommodate a 1 digit number.
	!
	! Index:
	!	.x Quarter>Query
	!	.x Query>Quarter
	!
	!--

	CLOSE PR_REG_ERNDED.CH%, PR_REG_TAXES.CH%

	CALL ASSG_FREECHANNEL(PR_REG_ERNDED.CH%)
	CALL ASSG_FREECHANNEL(PR_REG_TAXES.CH%)

6000	!
	! Open Earnings and Deduction register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.OPN"
	USE
		CONTINUE 6350 IF ERR = 5%
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

6010	!
	! Open Tax withholding register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.OPN"
	USE
		CONTINUE 6350 IF ERR = 5%
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		CONTINUE HelpError
	END WHEN

	!
	! Set up titles and whatnot
	!
	TITLE$(1%) = "Tax Withholding Dump - " + &
		NUM1$(QTR%) + MID("stndrdth", QTR% * 2% - 1%, 2%) + &
		" Quarter"
	TITLE$(2%) = "For the year of " + YYYY$
	TITLE$(3%) = ""

	TITLE$(4%) = "                                          W/H   " + &
		"      -------------Year To Date-------------    -----" + &
		"-----Quarter to Date-----------"

	TITLE$(5%) = "Emp #      Name                           Type  " + &
		"Code          Wages         Taxes    Wks Wkd         " + &
		" Wages         Taxes    Wks Wkd"

	TITLE$(6%) = ""

	FOR I% = 1% TO MAX_CODE
		CODE%(I%) = 0%
		FOR J% = 0% TO MAX_ITEM
			EMP_YTD_WAGES(I%, J%) = 0.0
			EMP_YTD_TAXES(I%, J%) = 0.0
			EMP_YTD_WKWRK(I%, J%) = 0.0
			EMP_QTD_WAGES(I%, J%) = 0.0
			EMP_QTD_TAXES(I%, J%) = 0.0
			EMP_QTD_WKWRK(I%, J%) = 0.0
		NEXT J%
	NEXT I%

6040	WHEN ERROR IN
		FIND #PR_REG_TAXES.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 6100 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		CONTINUE HelpError
	END WHEN

6050	WHEN ERROR IN
		GET #PR_REG_TAXES.CH%, REGARDLESS
	USE
		CONTINUE 6100 IF ERR = 11%
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		CONTINUE HelpError
	END WHEN

	GOTO 6100 IF PR_EMP_MASTER::EMPNUM <> PR_REG_TAXES::EMPNUM

	TAX_TYPE% = (INSTR(1%, TAX_TYPE_TABLE$, PR_REG_TAXES::TTYPE) + 2%) / 3%

	IF TAX_TYPE% <> 0%
	THEN
		IF TAX_TYPE% > 3%
		THEN
			TEMP_LOOP%, CODE%(TAX_TYPE%) = CODE%(TAX_TYPE%) + 1%
			CODE$(TAX_TYPE%, TEMP_LOOP%) = &
				PR_REG_TAXES::CODE
		ELSE
			TEMP_LOOP% = 1%
		END IF

		EMP_YTD_WAGES(TAX_TYPE%, 0%) = &
			FUNC_ROUND(EMP_YTD_WAGES(TAX_TYPE%, 0%) + &
			PR_REG_TAXES::TAXABLE(LOOP%), 2%) &
				FOR LOOP% = 0% TO QTR% - 1%

		EMP_YTD_WAGES(TAX_TYPE%, TEMP_LOOP%) = &
			FUNC_ROUND(EMP_YTD_WAGES(TAX_TYPE%, TEMP_LOOP%) + &
			PR_REG_TAXES::TAXABLE(LOOP%), 2%) &
				FOR LOOP% = 0% TO QTR% - 1%

		EMP_YTD_TAXES(TAX_TYPE%, TEMP_LOOP%) = &
			FUNC_ROUND(EMP_YTD_TAXES(TAX_TYPE%, TEMP_LOOP%) + &
			PR_REG_TAXES::TAX(LOOP%), 2%) &
				FOR LOOP% = 0% TO QTR% - 1%

		EMP_YTD_WKWRK(TAX_TYPE%, TEMP_LOOP%) = &
			FUNC_ROUND(EMP_YTD_WKWRK(TAX_TYPE%, TEMP_LOOP%) + &
			PR_REG_TAXES::WKWRK(LOOP%), 2%) &
				FOR LOOP% = 0% TO QTR% - 1%

		EMP_QTD_WAGES(TAX_TYPE%, 0%) = &
			FUNC_ROUND(EMP_QTD_WAGES(TAX_TYPE%, 0%) + &
			PR_REG_TAXES::TAXABLE(QTR% - 1%), 2%)

		EMP_QTD_WAGES(TAX_TYPE%, TEMP_LOOP%) = &
			FUNC_ROUND(EMP_QTD_WAGES(TAX_TYPE%, TEMP_LOOP%) + &
			PR_REG_TAXES::TAXABLE(QTR% - 1%), 2%)

		EMP_QTD_TAXES(TAX_TYPE%, TEMP_LOOP%) = &
			FUNC_ROUND(EMP_QTD_TAXES(TAX_TYPE%, TEMP_LOOP%) + &
			PR_REG_TAXES::TAX(QTR% - 1%), 2%)

		EMP_QTD_WKWRK(TAX_TYPE%, TEMP_LOOP%) = &
			FUNC_ROUND(EMP_QTD_WKWRK(TAX_TYPE%, TEMP_LOOP%) + &
			PR_REG_TAXES::WKWRK(QTR% - 1%), 2%)

	END IF

	GOTO 6050

	%Page

6100	!
	! Print fed total
	!
 !	GOTO 6110 IF COMP_STRING("FW", TAX_TYPE$) = 0% AND TAX_TYPE$ <> ""

	IF FUNC_ROUND(EMP_YTD_TAXES(1%, 1%) + EMP_QTD_TAXES(1%, 1%), 2%) <> 0.0
	THEN
		TEXT$ = LEFT(WORK_EMPNUM$ + SPACE$(10%), 10%) + " " + &
			LEFT(WORK_EMPNAME$ + SPACE$(30%), 30%) + "  FW         " + &
			FORMAT$(EMP_YTD_WAGES(1%, 1%), "##,###,###.## ") + &
			FORMAT$(EMP_YTD_TAXES(1%, 1%), "##,###,###.## ") + &
			"            " + &
			FORMAT$(EMP_QTD_WAGES(1%, 1%), "##,###,###.## ") + &
			FORMAT$(EMP_QTD_TAXES(1%, 1%), "##,###,###.## ")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		GOTO 6350 IF UTL_REPORTX::STAT

		WORK_EMPNUM$ = ""
		WORK_EMPNAME$ = ""

	END IF

6110	!
	! Print Fica total
	!
 !	GOTO 6120 IF COMP_STRING("FI", TAX_TYPE$) = 0% AND TAX_TYPE$ <> ""

	IF FUNC_ROUND(EMP_YTD_TAXES(2%, 1%) + EMP_QTD_TAXES(2%, 1%), 2%) <> 0.0
	THEN
		TEXT$ = LEFT(WORK_EMPNUM$ + SPACE$(10%), 10%) + " " + &
			LEFT(WORK_EMPNAME$ + SPACE$(30%), 30%) + "  FI         " + &
			FORMAT$(EMP_YTD_WAGES(2%, 1%), "##,###,###.## ") + &
			FORMAT$(EMP_YTD_TAXES(2%, 1%), "##,###,###.## ") + &
			"            " + &
			FORMAT$(EMP_QTD_WAGES(2%, 1%), "##,###,###.## ") + &
			FORMAT$(EMP_QTD_TAXES(2%, 1%), "##,###,###.## ")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		GOTO 6350 IF UTL_REPORTX::STAT

		WORK_EMPNUM$ = ""
		WORK_EMPNAME$ = ""

	END IF

6115	!
	! Print Fica total
	!
 !	GOTO 6120 IF COMP_STRING("FH", TAX_TYPE$) = 0% AND TAX_TYPE$ <> ""

	IF FUNC_ROUND(EMP_YTD_TAXES(3%, 1%) + EMP_QTD_TAXES(3%, 1%), 2%) <> 0.0
	THEN
		TEXT$ = LEFT(WORK_EMPNUM$ + SPACE$(10%), 10%) + " " + &
			LEFT(WORK_EMPNAME$ + SPACE$(30%), 30%) + &
			"  FH         " + &
			FORMAT$(EMP_YTD_WAGES(3%, 1%), "##,###,###.## ") + &
			FORMAT$(EMP_YTD_TAXES(3%, 1%), "##,###,###.## ") + &
			"            " + &
			FORMAT$(EMP_QTD_WAGES(3%, 1%), "##,###,###.## ") + &
			FORMAT$(EMP_QTD_TAXES(3%, 1%), "##,###,###.## ")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		GOTO 6350 IF UTL_REPORTX::STAT

		WORK_EMPNUM$ = ""
		WORK_EMPNAME$ = ""

	END IF

6120	SUM% = 0%
	SUM% = SUM% + CODE%(LOOP%) FOR LOOP% = 3% TO MAX_CODE
	GOTO 6350 IF SUM% = 0%

	!
	! Print tax report for SW SX CW DW EW
	!
	FOR TAX_TYPE% = 4% TO LEN(TAX_TYPE_TABLE$) / 3%

		CODE$ = MID(TAX_TYPE_TABLE$, (TAX_TYPE% - 1%) * 3% + 1%, 2%)

		TEST% = 0%

		FOR LOOP% = 1% TO CODE%(TAX_TYPE%)

 !			GOTO 6130 IF COMP_STRING(CODE$, TAX_TYPE$) = 0% &
 !				AND TAX_TYPE$ <> "" &
 !				OR COMP_STRING(CODE$(TAX_TYPE%, LOOP%), &
 !				TAX_CODE$) = 0% AND TAX_CODE$ <> ""

			TEXT$ = LEFT(WORK_EMPNUM$ + SPACE$(10%), 10%) + " " + &
				LEFT(WORK_EMPNAME$ + SPACE$(30%), 30%) + "  " + &
				CODE$ + "    " + CODE$(TAX_TYPE%, LOOP%) + "   " + &
				FORMAT$(EMP_YTD_WAGES(TAX_TYPE%, LOOP%), "##,###,###.## ") + &
				FORMAT$(EMP_YTD_TAXES(TAX_TYPE%, LOOP%), "##,###,###.## ") + &
				FORMAT$(EMP_YTD_WKWRK(TAX_TYPE%, LOOP%), "   ###,###") + &
				"  " + &
				FORMAT$(EMP_QTD_WAGES(TAX_TYPE%, LOOP%), "##,###,###.## ") + &
				FORMAT$(EMP_QTD_TAXES(TAX_TYPE%, LOOP%), "##,###,###.## ") + &
				FORMAT$(EMP_QTD_WKWRK(TAX_TYPE%, LOOP%), "   ###,###")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

			GOTO 6350 IF UTL_REPORTX::STAT

			WORK_EMPNUM$ = ""
			WORK_EMPNAME$ = ""

			TEST% = -1%

6130		NEXT LOOP%

		IF TEST%
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

			GOTO 6350 IF UTL_REPORTX::STAT
		END IF

	NEXT TAX_TYPE%

6350	RETURN

	%Page

 Check:
	!*****************************************************************
	! Print check register report
	!*****************************************************************

	!++
	! Abstract:CHK$HELP
	!	^*Check Register\*
	!	.p
	!	The ^*Check Register\* report option prints, for a specified
	!	payroll folder date, a payroll register in check number sequence. The report
	!	prints current information only.
	!	.p
	!	It is recommended that the ^*Check Register\* report be filed permanently.
	!	.p
	!	The fields in this report include:
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Check Number
	!	.le
	!	Employee Number
	!	.le
	!	Employee Name
	!	.le
	!	Pay Date
	!	.le
	!	Hours
	!	.le
	!	Units
	!	.le
	!	Gross Pay
	!	.le
	!	Non-Compensation Pay
	!	.le
	!	Miscellaneous Deductions (includes local taxes)
	!	.le
	!	State Taxes Withheld
	!	.le
	!	Federal Withholding Taxes
	!	.le
	!	FICA Taxes Withheld
	!	.le
	!	Net Check Amount
	!	.els
	!
	! Index:
	!	.x Check Register>Query
	!	.x Check>Query
	!	.x Query>Check Register
	!
	!--

	FROM_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	FROM_BATCH_NO$ = DATE_STOREDATE(FROM_BATCH_NO$)

	!++
	! Abstract:CHK$FLD01
	!	^*(01) From Date\*
	!	.p
	!	The ^*From Date\* field causes the printing
	!	to begin with a particular date.
	!	.b
	!	A blank field will cause the report to begin with the
	!	first date in the file.
	!
	! Index:
	!	.x From Date>Query
	!	.x Query>From Date
	!
	!--

	TO_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	TO_BATCH_NO$ = DATE_STOREDATE(TO_BATCH_NO$)

	!++
	! Abstract:CHK$FLD02
	!	^*(02) To Date\*
	!	.p
	!	The ^*To Date\* field causes the printing
	!	to end with a particular date.
	!	.b
	!	A blank field will cause the report to end with the last
	!	date in the file.
	!
	! Index:
	!	.x To Date>Query
	!	.x Query>To Date
	!
	!--

	!
	! Set up titles and whatnot
	!
	TITLE$(1%) = "Payroll Net Check Register"
	TITLE$(2%) = "For the Payroll Folders Dated From: " + &
		MID(FROM_BATCH_NO$, 5%, 2%) + "/" + &
		MID(FROM_BATCH_NO$, 7%, 2%) + "/" + &
		LEFT(FROM_BATCH_NO$, 4%) + " To: " + &
		MID(TO_BATCH_NO$, 5%, 2%) + "/" + &
		MID(TO_BATCH_NO$, 7%, 2%) + "/" + &
		LEFT(TO_BATCH_NO$, 4%)

	TITLE$(3%) = ""
	TITLE$(4%) = "Emp #      Name                     Date    " + &
		"    Hrs    Units Gross Pay Non-Comp     FICA  Federal    State" + &
		" Misc/Ded Net Check Ck #  "

	TITLE$(5%) = ""

7020	!
	! Main loop starts here
	!
	WORK_EMPNUM$ = PR_EMP_MASTER::EMPNUM
	WORK_EMPNAME$ = PR_EMP_MASTER::EMPNAME

	NUMBER_OF_LINES% = 0%

	EMP_TOTAL(I%) = 0.0 FOR I% = 1% TO 10%

	!
	! Look up employee in all payroll files selected
	!
	FOR PR_LOOP% = 1% TO DATA_FILE%

		EOF% = 0%
		BATCH_NO$ = DATA_FILE$(PR_LOOP%)

		IF (FROM_BATCH_NO$ > BATCH_NO$) OR (TO_BATCH_NO$ < BATCH_NO$)
		THEN
			GOTO 7400
		END IF

		CLOSE PR_TMP_PAY.CH%
		CALL ASSG_FREECHANNEL(PR_TRN_PAY.CH%)
		CALL ASSG_FREECHANNEL(PR_HIS_PAY.CH%)

		USE_HISTORY% = 0%

7030		!
		! Open Pay folder
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
		USE
			CONTINUE 7040 IF ERR = 5%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		PR_TMP_PAY.CH% = PR_TRN_PAY.CH%

		GOTO 7100

7040		!
		! Open pay history folder if journal not there
		!
		USE_HISTORY% = -1%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.OPN"
		USE
			CONTINUE 7050 IF ERR = 5%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		PR_TMP_PAY.CH% = PR_HIS_PAY.CH%

7050		!
		! Close deduction folder
		! This takes alot of time but it doesn't take disk storage
		!
		CLOSE PR_TRN_DED.CH%
		CALL ASSG_FREECHANNEL(PR_TRN_DED.CH%)

		!
		! Open Deduction folder
		!
		IF USE_HISTORY%
		THEN
			WHEN ERROR IN
				%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_DED.OPN"
			USE
				CONTINUE 7060 IF ERR = 5%
				FILENAME$ = "PR_TRN_DED"
				CONTINUE HelpError
			END WHEN

			PR_TRN_DED.CH% = PR_HIS_DED.CH%
		ELSE
			WHEN ERROR IN
				%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.OPN"
			USE
				CONTINUE 7060 IF ERR = 5%
				FILENAME$ = "PR_TRN_DED"
				CONTINUE HelpError
			END WHEN

		END IF

7060		!
		! Close check folder
		! This takes alot of time but it doesn't take disk storage
		!
		CLOSE PR_TRN_CHECK.CH%
		CALL ASSG_FREECHANNEL(PR_TRN_CHECK.CH%)

		!
		! Open Check folder
		!
		IF USE_HISTORY%
		THEN
			WHEN ERROR IN
				%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_CHECK.CRE"
			USE
				CONTINUE 7350 IF ERR = 5%
				FILENAME$ = "PR_TRN_CHECK"
				CONTINUE HelpError
			END WHEN

			PR_TRN_CHECK.CH% = PR_HIS_CHECK.CH%
		ELSE
			WHEN ERROR IN
				%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.CRE"
			USE
				CONTINUE 7350 IF ERR = 5%
				FILENAME$ = "PR_TRN_CHECK"
				CONTINUE HelpError
			END WHEN

		END IF

7100		!
		! Get pay detail information
		!
		WHEN ERROR IN
			FIND #PR_TMP_PAY.CH%, &
				KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
				REGARDLESS
		USE
			CONTINUE 7400 IF ERR = 155%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		LINE_TOTAL(I%) = 0.0 FOR I% = 1% TO 10%

		TEST_PR_END_DATE$ = ""

7110		WHEN ERROR IN
			GET #PR_TMP_PAY.CH%, REGARDLESS
		USE
			IF ERR = 11%
			THEN
				EOF% = -1%
				CONTINUE 7200
			END IF
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		!
		! If history then set history map to journal
		!
		IF USE_HISTORY%
		THEN
			PR_TRN_PAY = PR_HIS_PAY
		END IF

		GOTO 7110 IF PR_TRN_PAY::PTYPE = "A"

		GOTO 7200 &
			IF (PR_EMP_MASTER::EMPNUM <> PR_TRN_PAY::EMPNUM) OR &
			((PR_TRN_PAY::PR_END_DATE <> TEST_PR_END_DATE$) AND &
			(TEST_PR_END_DATE$ <> ""))

7120		TEST_PR_END_DATE$ = PR_TRN_PAY::PR_END_DATE

		LINE_TOTAL(1%) = FUNC_ROUND(LINE_TOTAL(1%) + &
			PR_TRN_PAY::REG_HR + PR_TRN_PAY::OVT_HR, 2%)
		LINE_TOTAL(2%) = FUNC_ROUND(LINE_TOTAL(2%) + &
			PR_TRN_PAY::PIECE, 2%)
		LINE_TOTAL(3%) = FUNC_ROUND(LINE_TOTAL(3%) + &
			PR_TRN_PAY::GROSS, 2%) &

		GOTO 7110

7200		WHEN ERROR IN
			FIND #PR_TRN_DED.CH%, &
				KEY #0% EQ PR_EMP_MASTER::EMPNUM + TEST_PR_END_DATE$, &
				REGARDLESS
		USE
			CONTINUE 7300 IF ERR = 9% OR ERR = 155%
			FILENAME$ = "PR_TRN_DED"
			CONTINUE HelpError
		END WHEN

7210		WHEN ERROR IN
			GET #PR_TRN_DED.CH%, REGARDLESS
		USE
			CONTINUE 7300 IF ERR = 11%
			FILENAME$ = "PR_TRN_DED"
			CONTINUE HelpError
		END WHEN

		!
		! If journal net there the set history map to journal
		!
		IF USE_HISTORY%
		THEN
			PR_TRN_DED = PR_HIS_DED
		END IF

		GOTO 7300 IF (PR_EMP_MASTER::EMPNUM <> PR_TRN_DED::EMPNUM) OR &
			(PR_TRN_DED::PR_END_DATE <> TEST_PR_END_DATE$ AND &
			TEST_PR_END_DATE$ <> "")

		IF PR_TRN_DED::DTYPE = "M" OR PR_TRN_DED::DTYPE = "T"
		THEN
			LINE_TOTAL(4%) = FUNC_ROUND(LINE_TOTAL(4%) + &
				PR_TRN_DED::AMOUNT, 2%)
		ELSE
			SELECT PR_TRN_DED::CODE

			CASE "FI"
				LINE_TOTAL(5%) = FUNC_ROUND(LINE_TOTAL(5%) + &
					PR_TRN_DED::AMOUNT, 2%)

			CASE "FW"
				LINE_TOTAL(6%) = FUNC_ROUND(LINE_TOTAL(6%) + &
					PR_TRN_DED::AMOUNT, 2%)

			CASE "SW"
				LINE_TOTAL(7%) = FUNC_ROUND(LINE_TOTAL(7%) + &
					PR_TRN_DED::AMOUNT, 2%)

			CASE ELSE
				LINE_TOTAL(8%) = FUNC_ROUND(LINE_TOTAL(8%) + &
					PR_TRN_DED::AMOUNT, 2%)

			END SELECT
		END IF

		GOTO 7210

7300		!
		! Get the check number now
		!
		PR_TRN_CHECK::PR_END_DATE	= ""
		PR_TRN_CHECK::CHECK		= ""
		PR_TRN_CHECK::CHECK_DATE	= ""
		PR_TRN_CHECK::PAYFREQ		= 0%


		WHEN ERROR IN
			GET #PR_TRN_CHECK.CH%, KEY #0% EQ PR_EMP_MASTER::EMPNUM + &
				TEST_PR_END_DATE$, REGARDLESS
		USE
			CONTINUE 7350 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "PR_TRN_CHECK"
			CONTINUE HelpError
		END WHEN

		!
		! If history then set history map to journal
		!
		IF USE_HISTORY%
		THEN
			PR_TRN_CHECK = PR_HIS_CHECK
		END IF


7350		!********************************************************
		! Print one line of employee history
		!********************************************************
		SUM = 0.0
		SUM = FUNC_ROUND(SUM + LINE_TOTAL(LOOP%), 2%) &
			FOR LOOP% = 1% TO 8%

		IF SUM <> 0.0
		THEN
			LINE_TOTAL(9%) = FUNC_ROUND(LINE_TOTAL(3%) - &
				LINE_TOTAL(5%) - &
				LINE_TOTAL(6%) - LINE_TOTAL(7%) - &
				LINE_TOTAL(8%), 2%)

			EMP_TOTAL(LOOP%) = EMP_TOTAL(LOOP%) + &
				LINE_TOTAL(LOOP%) &
				FOR LOOP% = 1% TO 9%

			TEXT$ = LEFT(WORK_EMPNUM$ + SPACE$(10%), 10%) + " " + &
				LEFT(WORK_EMPNAME$ + SPACE$(22%), 22%) + " " + &
				PRNT_DATE(BATCH_NO$, 6%) + " " + &
				FORMAT$(LINE_TOTAL(1%), "#####.## ") + &
				FORMAT$(LINE_TOTAL(2%), "#####.## ") + &
				FORMAT$(LINE_TOTAL(3%), "######.## ") + &
				FORMAT$(LINE_TOTAL(4%), "#####.##") + &
				FORMAT$(LINE_TOTAL(5%), "######.## ") + &
				FORMAT$(LINE_TOTAL(6%), "#####.## ") + &
				FORMAT$(LINE_TOTAL(7%), "#####.## ") + &
				FORMAT$(LINE_TOTAL(8%), "#####.##") + &
				FORMAT$(LINE_TOTAL(9%), "#######.## ") + &
				PR_TRN_CHECK::CHECK

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

			WORK_EMPNUM$, WORK_EMPNAME$ = ""

			NUMBER_OF_LINES% = NUMBER_OF_LINES% + 1%

		END IF

		GOTO CheckExit IF UTL_REPORTX::STAT

		LINE_TOTAL(I%) = 0.0 FOR I% = 1% TO 10%

		IF (PR_TRN_PAY::EMPNUM = PR_EMP_MASTER::EMPNUM) AND (EOF% = 0%)
		THEN
			GOTO 7120
		END IF

7400	!
	! Get data from next payroll folder
	!
	NEXT PR_LOOP%

	!
	! Print totals for one employee
	!
	IF NUMBER_OF_LINES%
	THEN
		!***********************************************************
		! Print Total for employee
		!***********************************************************

		IF NUMBER_OF_LINES% > 1%
		THEN
			TEXT$ = SPACE$(16%) + "Emp Total" + SPACE$(18%) + &
				FORMAT$(EMP_TOTAL(1%), "#####.## ") + &
				FORMAT$(EMP_TOTAL(2%), "#####.## ") + &
				FORMAT$(EMP_TOTAL(3%), "######.## ") + &
				FORMAT$(EMP_TOTAL(4%), "#####.##") + &
				FORMAT$(EMP_TOTAL(5%), "######.## ") + &
				FORMAT$(EMP_TOTAL(6%), "#####.## ") + &
				FORMAT$(EMP_TOTAL(7%), "#####.## ") + &
				FORMAT$(EMP_TOTAL(8%), "#####.##") + &
				FORMAT$(EMP_TOTAL(9%), "#######.## ")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		END IF

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		GOTO CheckExit IF UTL_REPORTX::STAT
	END IF

 CheckExit:
	RETURN

	%Page

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

	%Page

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

19999	END

20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"

	EXTERNAL LONG FUNCTION PR_MAIN_EMPLOYEE

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	!
	! (Special) Employee master file
	!
	CASE PR_MAIN_EMPLOYEE.ID

		MAINT_GROUP = PR_MAIN_EMPLOYEE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
