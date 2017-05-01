1	%TITLE "Payroll New York SDI Report"
	%SBTTL "PR_RPRT_NY_SDI"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1989 BY
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
	! ID:PR087
	!
	! Abstract:HELP
	!	.p
	!	The ^*New York SDI Report\* option
	!	prints the
	!	required information for State Disability Insurance reporting.
	!	This report contains the following fields:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Employee Number
	!	.le
	!	Employee Name
	!	.le
	!	Date
	!	.le
	!	Gross Pay
	!	.le
	!	Taxable
	!	.le
	!	Tax
	!	.els
	!
	! Index:
	!	.x SDI>Report>NY
	!	.x Report>SDI>NY
	!	.x State Disability Insurance>NY>Report
	!	.x Report>State Disability Insurance>NY
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_NY_SDI/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_NY_SDI, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_NY_SDI.OBJ;*
	!
	! Author:
	!
	!	07/14/89 - B. Craig Larsen
	!
	! Modification history:
	!
	!	01/15/90 - Kevin Handy
	!		Fixed bug where one folder may be missed after
	!		the first loop through all of the folders.
	!
	!	06/15/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		be sent to a spreadsheet or to a DIF file.
	!
	!	12/18/91 - Kevin Handy
	!		Modified to ignore "A" records in PR_PAY.
	!
	!	04/10/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Remove SUBJECT% parameter from PR_READ_SUBJTAX.
	!		Removed disable_unsolicited_input.
	!
	!	09/10/96 - Kevin Handy
	!		Reformat source code.
	!
	!	03/13/97 - Kevin Handy
	!		Clean up coding so doesn't look like it refers
	!		to FICA, and does fewer INSTR's.
	!
	!	05/30/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/19/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STATUS.HB"
	MAP (PR_EMP_STATUS)	PR_EMP_STATUS_CDD	PR_EMP_STATUS

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP (PR_TRN_PAY)	PR_TRN_PAY_CDD	PR_TRN_PAY
	MAP (PR_HIS_PAY)	PR_TRN_PAY_CDD	PR_HIS_PAY

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.HB"
	MAP (PR_TRN_DED)	PR_TRN_DED_CDD	PR_TRN_DED
	MAP (PR_HIS_DED)	PR_TRN_DED_CDD	PR_HIS_DED

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP	(PR_ERNDED_DEF)	PR_ERNDED_DEF_CDD	PR_ERNDED_DEF

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_S.HB"
	MAP	(PR_TAX_PROFILE_F)	PR_TAX_PROFILE_S_CDD	PR_TAX_PROFILE_S

	!
	! Need to include _F version so that variable length record
	! business will work.
	!
	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.HB"
	MAP	(PR_TAX_PROFILE_F)	PR_TAX_PROFILE_F_CDD	PR_TAX_PROFILE_F

	DECLARE INTEGER CONSTANT OPEN_MAX = 24
	DECLARE INTEGER CONSTANT FILE_MAX = 2000

	!
	! Dimension
	!
	DIM	DATA_FILE$(FILE_MAX), &
		USE_HISTORY%(OPEN_MAX), &
		PR_TMP_PAY.CH%(OPEN_MAX)

	%PAGE

	ON ERROR GOTO 19000

	!
	! Other Variables
	!
	SUBJECT_CODE$ = "OST"
	TAX_TYPE_TABLE$ = "FI!FW!SW!SX!SU!CW!DW!EW!SI!"
	TAX_STATE$ = "NY"

 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	FROM_BATCH_NO$ = DATE_STOREDATE(FROM_BATCH_NO$)
	!++
	! Abstract:FLD01
	!	^*(01) Start Payroll Date\*
	!	.p
	!	The ^*Start Payroll Date\* field enters the date with
	!	which the report will begin printing.  A blank field causes the report to begin
	!	with the first date in the file.
	!
	! Index:
	!	.x Start Payroll Date
	!
	!--

	TO_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	TO_BATCH_NO$ = DATE_STOREDATE(TO_BATCH_NO$)
	!++
	! Abstract:FLD02
	!	^*(02) To Payroll Date\*
	!	.p
	!	The ^*To Payroll Date\* field enters the payroll date
	!	with which the report will end printing.  A blank field causes the report
	!	to end with the last date in the file.
	!
	! Index:
	!	.x To Payroll Date
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)
	!++
	! Abstract:FLD03
	!	^*(03) From Item\*
	!	.p
	!	The ^*From Item\* field enters the item with which the
	!	report will begin printing.  A blank field causes the report to begin with
	!	the first item in the file.
	!
	! Index:
	!	.x From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)
	!++
	! Abstract:FLD04
	!	^*(04) To Item\*
	!	.p
	!	The ^*To Item\* field enters the item with which the report
	!	will conclude printing.  A blank field causes the report to  end with the
	!	last item in the file.
	!
	! Index:
	!	.x To Item
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)
	!++
	! Abstract:FLD05
	!	^*(05) Sort (NU,NA,LO,SO)
	!	.p
	!	The ^*Sort\* field enter a code which causes the report
	!	to be sorted in the indicated manner.
	!	.p
	!	Valid codes for entry are:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	NU = Number
	!	.le
	!	NA = Name
	!	.le
	!	LO = Location
	!	.le
	!	SO = Alphabetical (Last name first)
	!	.els
	!	.lm -10
	!	.p
	!	An entry is required in this field and only the above codes are valid.
	!
	! Index:
	!	.x Sort
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

	CALL READ_DEVICE("PR_TRN_PAY", PR_TRN_PAY.DEV$, STAT%)
	CALL READ_DEVICE("PR_HIS_PAY", PR_HIS_PAY.DEV$, STAT%)

	CALL PR_FIND_DETAILFILE(FROM_BATCH_NO$, &
		TO_BATCH_NO$, &
		PR_TRN_PAY.DEV$, &
		PR_HIS_PAY.DEV$, &
		DATA_FILE$())

	DATA_FILE% = VAL%(DATA_FILE$(0%))

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	!
	! Open Employee status file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STATUS.OPN"
	USE
		FILENAME$ = "PR_EMP_STATUS"
		CONTINUE HelpError
	END WHEN

	!
	! Open up the pay file.  If there are more than OPEN_MAX files
	! then skip to slower logic - open close files
	!
	TMP_DATA_FILE% = OPEN_MAX
	TMP_DATA_FILE% = DATA_FILE% IF DATA_FILE% <= OPEN_MAX

	FOR PR_LOOP% = 1% TO TMP_DATA_FILE%
		BATCH_NO$ = DATA_FILE$(PR_LOOP%)

		USE_HISTORY%(PR_LOOP%) = 0%

310		!
		! Open Pay folder
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
		USE
			CONTINUE 320 IF ERR = 5%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		PR_TMP_PAY.CH%(PR_LOOP%) = PR_TRN_PAY.CH%

		GOTO 330

320		!
		! Open pay history folder if journal not there
		!
		CALL ASSG_FREECHANNEL(PR_TRN_PAY.CH%)
		USE_HISTORY%(PR_LOOP%) = -1%

		%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.OPN"

		PR_TMP_PAY.CH%(PR_LOOP%) = PR_HIS_PAY.CH%

330		PR_TRN_PAY.CH%, PR_HIS_PAY.CH% = 0%
	NEXT PR_LOOP%

340	!
	! Open ERNDED Definition file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"
	USE
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

360	!
	! Open Profile
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.OPN"

		GET #PR_TAX_PROFILE.CH%, KEY #0% EQ "S" + TAX_STATE$, REGARDLESS

		OST_DED_MAX = PR_TAX_PROFILE_S::OST_DEDMAX

		CLOSE #PR_TAX_PROFILE.CH%
	USE
		FILENAME$ = "PR_TAX_PROFILE"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Set up titles and whatnot
	!
	TITLE$(1%) = "Payroll New York SDI Report"
	TITLE$(2%) = "From:  " + PRNT_DATE(FROM_BATCH_NO$, 8%) + &
		"  To:  " + PRNT_DATE(TO_BATCH_NO$, 8%)

	SELECT SORTBY$
	CASE "NU"
		TITLE$(3%) = "By Employee Number"

	CASE "NA"
		TITLE$(3%) = "By Employee Name"

	CASE "SN"
		TITLE$(3%) = "By Soc. Sec. Number"

	CASE "LO"
		TITLE$(3%) = "By Location"

	CASE ELSE
		TITLE$(3%) = "By Alpha Sort"
	END SELECT

	TITLE$(4%) = ""

	!
	! Column headings
	!
	TITLE$(5%) = "EmpNum     EmpName                  Date     " + &
		"Gross Pay     Taxable         Tax"
	TITLE$(6%) = ""

	!
	! Line layouts
	!
	LYT_LINE$ = "$EmpNum:010,$EmpName:033,DPRDate:042,VPay:055," + &
		"VTaxable:067,VTax:079"

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
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	LOCAL_FLAG% = 0%

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

	TAXX$ = "SX"

17024	EXEMPT_EMP% =  0%

	WHEN ERROR IN
		GET #PR_EMP_STATUS.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM + TAXX$ + TAX_STATE$, &
			REGARDLESS
	USE
		CONTINUE 17026 IF ERR = 155%
		FILENAME$ = "PR_EMP_STATUS"
		CONTINUE HelpError
	END WHEN

	EXEMPT_EMP% = -1% IF PR_EMP_STATUS::STSTATUS = "E"

17026	!
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

		GOSUB LocalTotal IF (PR_EMP_MASTER::LOCATION <> THIS_LOCATION$)

	CASE ELSE
		GOTO ExitTotal IF (PR_EMP_MASTER::SORT > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	END SELECT

	NUMBER_OF_LINES%, NEW_EMP%, NEWYORK% = 0%

	EMP_TOT_PAY, EMP_TOT_TAXABLE, EMP_TOT_TAX = 0.0

	!
	! Open all payroll folder files
	!
	FOR PR_LOOP% = 1% TO DATA_FILE%
		BATCH_NO$ = DATA_FILE$(PR_LOOP%)

		USE_HISTORY% = 0%

		PAY, EMP_TAXABLE, EMP_TAX = 0.0

		!
		! To optimize performance open_max files are open all
		! of the time to test for an employee number
		!
		IF PR_LOOP% <= OPEN_MAX
		THEN
			USE_HISTORY% = USE_HISTORY%(PR_LOOP%)
			PR_TMP_PAY.CH% = PR_TMP_PAY.CH%(PR_LOOP%)
			GOTO 17100
		END IF

17030		!
		! Open Pay folder
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
		USE
			CONTINUE 17040 IF ERR = 5%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		PR_TMP_PAY.CH% = PR_TRN_PAY.CH%

		GOTO 17100

17040		!
		! Open pay history folder if journal not there
		!
		USE_HISTORY% = -1%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.OPN"
		USE
			CONTINUE 17100 IF ERR = 5%
			FILENAME$ = "PR_HIS_PAY"
			CONTINUE HelpError
		END WHEN

		PR_TMP_PAY.CH% = PR_HIS_PAY.CH%

17100		!
		! Get pay detail information
		!
		WHEN ERROR IN
			FIND #PR_TMP_PAY.CH%, &
				KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
				REGARDLESS
		USE
			CONTINUE 17200 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

17110		WHEN ERROR IN
			GET #PR_TMP_PAY.CH%, REGARDLESS
		USE
			CONTINUE 17200 IF ERR = 11%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		!
		! If history then set history map to journal
		!
		PR_TRN_PAY = PR_HIS_PAY IF USE_HISTORY%

		GOTO 17200 IF PR_EMP_MASTER::EMPNUM <> PR_TRN_PAY::EMPNUM

		GOTO 17110 IF PR_TRN_PAY::TAX_PKG <> TAX_STATE$

		GOTO 17110 IF PR_TRN_PAY::PTYPE = "A"

		NEWYORK% = -1%

		!
		! See if taxable
		!
		CALL PR_READ_SUBJTAX(PR_ERNDED_DEF.CH%, &
			SUBJECT_CODE$, &
			"P", &
			PR_TRN_PAY::CODE, &
			TAXABLE%, &
			REPORTABLE%)

		GOTO 17110 IF TAXABLE%

		EMP_TAXABLE = FUNC_ROUND(EMP_TAXABLE + PR_TRN_PAY::GROSS, 2%)

		PAY = FUNC_ROUND(PAY + PR_TRN_PAY::GROSS, 2%)

		GOTO 17110

17200		GOTO 17420 IF NEWYORK% = 0%

		!
		! Close deduction folder
		! This takes alot of time but it doesn't take disk storage
		!
		CLOSE PR_TRN_DED.CH%
		CALL ASSG_FREECHANNEL(PR_TRN_DED.CH%)

		!
		! Open Deduction folder
		!
		WHEN ERROR IN
			IF USE_HISTORY% = 0%
			THEN
				%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.OPN"
			ELSE
				%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_DED.OPN"

				PR_TRN_DED.CH% = PR_HIS_DED.CH%
			END IF

			FIND #PR_TRN_DED.CH%, &
				KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
				REGARDLESS
		USE
			CONTINUE 17300 IF ERR = 155% OR ERR = 5%
			FILENAME$ = "PR_TRN_DED"
			CONTINUE HelpError
		END WHEN

17210		WHEN ERROR IN
			GET #PR_TRN_DED.CH%, REGARDLESS
		USE
			CONTINUE 17300 IF ERR = 11%
			FILENAME$ = "PR_TRN_DED"
			CONTINUE HelpError
		END WHEN

		!
		! If journal not there then set history map to journal
		!
		PR_TRN_DED = PR_HIS_DED IF USE_HISTORY%

		GOTO 17300 IF PR_EMP_MASTER::EMPNUM <> PR_TRN_DED::EMPNUM

		!
		! See if this is a tax
		!
		EMP_TAX = FUNC_ROUND(EMP_TAX + PR_TRN_DED::AMOUNT, 2%) &
			IF PR_TRN_DED::CODE = TAXX$ AND &
			PR_TRN_DED::TAX_CODE = TAX_STATE$

		GOTO 17210

17300		TAXABLE_TEXT$ = ""

		!
		! Tax Basis
		!
		GOTO 17420 IF EMP_TAX <= 0.0 AND EMP_TAXABLE <= 0.0

		IF NEW_EMP% = 0%
		THEN
			TAXABLE_TEXT$ = PR_EMP_MASTER::EMPNUM + &
				" " + LEFT( &
				PR_EMP_MASTER::EMPNAME, 22%) + &
				" " + PRNT_DATE(DATA_FILE$( &
				PR_LOOP%), 6%) + "   " + &
				FORMAT$(PAY, "######.##   ")
		ELSE
			TAXABLE_TEXT$ = SPACE$(34%) + &
				PRNT_DATE(DATA_FILE$( &
				PR_LOOP%), 6%) + "   " + &
				FORMAT$(PAY, "######.##   ")
		END IF

		PER_LIMIT = FUNC_ROUND( OST_DED_MAX / &
			PR_EMP_MASTER::PAYFREQ, 2%)

		EMP_TAXABLE = PER_LIMIT	IF EMP_TAXABLE > PER_LIMIT AND &
				PER_LIMIT > 0.0

		TAXABLE_TEXT$ = TAXABLE_TEXT$ + FORMAT$( &
			EMP_TAXABLE, "######.##   ")

		TAXABLE_TEXT$ = TAXABLE_TEXT$ + FORMAT$(EMP_TAX, "######.##")

		!
		! Print Taxes
		!
		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), &
			LEFT(TAXABLE_TEXT$, 80%), 0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT

		LOCAL_FLAG%, NEW_EMP% = -1%

		NUMBER_OF_LINES% = NUMBER_OF_LINES% + 1%

		EMP_TOT_PAY = EMP_TOT_PAY + PAY
		EMP_TOT_TAXABLE = EMP_TOT_TAXABLE + EMP_TAXABLE
		EMP_TOT_TAX = EMP_TOT_TAX + EMP_TAX

17420	!
	! Get data from next payroll folder
	!
		IF PR_LOOP% > OPEN_MAX
		THEN
			CLOSE PR_TMP_PAY.CH%
			CALL ASSG_FREECHANNEL(PR_TMP_PAY.CH%)
		END IF

	NEXT PR_LOOP%

	GOSUB PrintEmployeeTotal

	!
	! Go to next record
	!
	GOTO 17020

 ExitTotal:
	GOSUB LocalTotal IF (SORTBY$ = "LO")

	!
	! Handle end of report
	!
	TAXABLE_TEXT$ = "Total Taxable" + SPACE$(32%)

	TAXABLE_TEXT$ = TAXABLE_TEXT$ +	FORMAT$(TOT_TOT_PAY, "######.##   ")

	!
	! Print Taxable wages
	!
	TAXABLE_TEXT$ = TAXABLE_TEXT$ +	FORMAT$(TOT_TOT_TAXABLE, "######.##   ")


	TAXABLE_TEXT$ = TAXABLE_TEXT$ + FORMAT$(TOT_TOT_TAX, "######.##")

	!
	! Print Taxes
	!
	COUNT_TEXT$ = "     Employee Count "

	COUNT_TEXT$ = COUNT_TEXT$ + FORMAT$(TOT_EMP_MALE%, &
		"  Male:  #######  ")

	COUNT_TEXT$ = COUNT_TEXT$ + FORMAT$(TOT_EMP_FEMALE%, &
		"Female:  #######  ")

	COUNT_TEXT$ = COUNT_TEXT$ + FORMAT$(TOT_EMP_MALE% + &
		TOT_EMP_FEMALE%, " Total:  ########")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), LEFT(TAXABLE_TEXT$, 80%), 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), LEFT(COUNT_TEXT$, 80%), 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)


	!
	! Handle end of report
	!
	TAXABLE_TEXT$ = "Total Exempt " + SPACE$(32%)

	TAXABLE_TEXT$ = TAXABLE_TEXT$ +	FORMAT$(XTOT_TOT_PAY, "######.##   ")

	!
	! Print Taxable wages
	!
	TAXABLE_TEXT$ = TAXABLE_TEXT$ +	FORMAT$(XTOT_TOT_TAXABLE, "######.##   ")


	TAXABLE_TEXT$ = TAXABLE_TEXT$ + FORMAT$(XTOT_TOT_TAX, "######.##")

	!
	! Print Taxes
	!
	COUNT_TEXT$ = "     Employee Count "

	COUNT_TEXT$ = COUNT_TEXT$ + FORMAT$(XTOT_EMP_MALE%, &
		"  Male:  #######  ")

	COUNT_TEXT$ = COUNT_TEXT$ + FORMAT$(XTOT_EMP_FEMALE%, &
		"Female:  #######  ")

	COUNT_TEXT$ = COUNT_TEXT$ + FORMAT$(XTOT_EMP_MALE% + &
		XTOT_EMP_FEMALE%, " Total:  ########")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), LEFT(TAXABLE_TEXT$, 80%), 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), LEFT(COUNT_TEXT$, 80%), 0%)

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

 PrintEmployeeTotal:
	!******************************************************************
	! Print Total for employee
	!******************************************************************

	IF NUMBER_OF_LINES% > 1%
	THEN
		TEXT$ = SPACE$(18%) + "Emp Total" + SPACE$(18%) + &
			FORMAT$(EMP_TOT_PAY, "######.##   ") + &
			FORMAT$(EMP_TOT_TAXABLE, "######.##   ") + &
			FORMAT$(EMP_TOT_TAX, "######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	IF NUMBER_OF_LINES% > 0%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT

		SELECT	EXEMPT_EMP%
		! Not Exempt
		CASE 0%
			TOT_TOT_PAY = TOT_TOT_PAY + EMP_TOT_PAY
			TOT_TOT_TAXABLE = TOT_TOT_TAXABLE + EMP_TOT_TAXABLE
			TOT_TOT_TAX = TOT_TOT_TAX + EMP_TOT_TAX

			LOC_TOT_PAY = LOC_TOT_PAY + EMP_TOT_PAY
			LOC_TOT_TAXABLE = LOC_TOT_TAXABLE + EMP_TOT_TAXABLE
			LOC_TOT_TAX = LOC_TOT_TAX + EMP_TOT_TAX

			SELECT	PR_EMP_MASTER::SEX
			CASE "M"
				TOT_EMP_MALE% = TOT_EMP_MALE% + 1%
				LOC_EMP_MALE% = LOC_EMP_MALE% + 1%

			CASE ELSE
				TOT_EMP_FEMALE% = TOT_EMP_FEMALE% + 1%
				LOC_EMP_FEMALE% = LOC_EMP_FEMALE% + 1%

			END SELECT

		! Exempt
		CASE -1%
			XTOT_TOT_PAY = XTOT_TOT_PAY + EMP_TOT_PAY
			XTOT_TOT_TAXABLE = XTOT_TOT_TAXABLE + EMP_TOT_TAXABLE
			XTOT_TOT_TAX = XTOT_TOT_TAX + EMP_TOT_TAX

			XLOC_TOT_PAY = XLOC_TOT_PAY + EMP_TOT_PAY
			XLOC_TOT_TAXABLE = XLOC_TOT_TAXABLE + EMP_TOT_TAXABLE
			XLOC_TOT_TAX = XLOC_TOT_TAX + EMP_TOT_TAX

			SELECT	PR_EMP_MASTER::SEX
			CASE "M"
				XTOT_EMP_MALE% = XTOT_EMP_MALE% + 1%
				XLOC_EMP_MALE% = XLOC_EMP_MALE% + 1%

			CASE ELSE
				XTOT_EMP_FEMALE% = XTOT_EMP_FEMALE% + 1%
				XLOC_EMP_FEMALE% = XLOC_EMP_FEMALE% + 1%

			END SELECT

		END SELECT

		EMP_TOT_PAY, EMP_TOT_TAXABLE, EMP_TOT_TAX = 0.0
	END IF

	RETURN

	%Page

 LocalTotal:
	GOTO 18590 IF LOCAL_FLAG% = 0%

	TAXABLE_TEXT$ = "Total Local Taxable" + SPACE$(26%)

	TAXABLE_TEXT$ = TAXABLE_TEXT$ + FORMAT$(LOC_TOT_PAY, "######.##   ")

	!
	! Print Taxable wages
	!
	TAXABLE_TEXT$ = TAXABLE_TEXT$ +	FORMAT$(LOC_TOT_TAXABLE, "######.##   ")


	TAXABLE_TEXT$ = TAXABLE_TEXT$ + FORMAT$(LOC_TOT_TAX, "######.##")

	!
	! Print Taxes
	!
	COUNT_TEXT$ = "     Employee Count "

	COUNT_TEXT$ = COUNT_TEXT$ + FORMAT$(LOC_EMP_MALE%, &
		"  Male:  #######  ")

	COUNT_TEXT$ = COUNT_TEXT$ + FORMAT$(LOC_EMP_FEMALE%, &
		"Female:  #######  ")

	COUNT_TEXT$ = COUNT_TEXT$ + FORMAT$(LOC_EMP_MALE% + &
		LOC_EMP_FEMALE%, " Total:  ########")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), LEFT(TAXABLE_TEXT$, 80%), 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), LEFT(COUNT_TEXT$, 80%), 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)


	TAXABLE_TEXT$ = "Total Local Exempt" + SPACE$(27%)

	TAXABLE_TEXT$ = TAXABLE_TEXT$ + FORMAT$(XLOC_TOT_PAY, "######.##   ")

	!
	! Print Taxable wages
	!
	TAXABLE_TEXT$ = TAXABLE_TEXT$ +	FORMAT$(XLOC_TOT_TAXABLE, "######.##   ")


	TAXABLE_TEXT$ = TAXABLE_TEXT$ + FORMAT$(XLOC_TOT_TAX, "######.##")

	!
	! Print Taxes
	!
	COUNT_TEXT$ = "     Employee Count "

	COUNT_TEXT$ = COUNT_TEXT$ + FORMAT$(XLOC_EMP_MALE%, &
		"  Male:  #######  ")

	COUNT_TEXT$ = COUNT_TEXT$ + FORMAT$(XLOC_EMP_FEMALE%, &
		"Female:  #######  ")

	COUNT_TEXT$ = COUNT_TEXT$ + FORMAT$(XLOC_EMP_MALE% + &
		XLOC_EMP_FEMALE%, " Total:  ########")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), LEFT(TAXABLE_TEXT$, 80%), 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), LEFT(COUNT_TEXT$, 80%), 0%)

	TITLE$(3%) = "For Location " + PR_EMP_MASTER::LOCATION
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 3000%)

18590	!
	! Set up for next one
	!
	LOCAL_FLAG% = 0%

	TITLE$(3%) = "For Location " + PR_EMP_MASTER::LOCATION
	THIS_LOCATION$ = PR_EMP_MASTER::LOCATION

	LOC_TOT_PAY, LOC_TOT_TAXABLE, LOC_TOT_TAX = 0.0
	LOC_EMP_MALE%, LOC_EMP_FEMALE% = 0%

	XLOC_TOT_PAY, XLOC_TOT_TAXABLE, XLOC_TOT_TAX = 0.0
	XLOC_EMP_MALE%, XLOC_EMP_FEMALE% = 0%

	RETURN

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
