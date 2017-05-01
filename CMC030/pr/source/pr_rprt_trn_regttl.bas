1	%TITLE "Payroll Register Report"
	%SBTTL "PR_RPRT_TRN_REGTTL"
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
	! Computer Management Center.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	! ID:PR014
	!
	! Abstract:HELP
	!	.p
	!	This program prints out the management payroll summary report.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_TRN_REGTTL/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_TRN_REGTTL, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_TRN_REGTTL.OBJ;*
	!
	! Author:
	!
	!	11/27/87 - Robert Peterson
	!
	! Modification history:
	!
	!	02/23/89 - Kevin Handy
	!		Taken from PR_RPRT_TRN_REG and modified to
	!		create PR_RPRT_TRN_REGTTL.
	!
	!	03/07/89 - Kevin Handy
	!		Fixed problem with overtime not being zeroed,
	!		and all deductions being totaled under first
	!		code.
	!
	!	03/07/89 - Kevin Handy
	!		Added SU as a tax type.
	!
	!	04/10/89 - Frank Starman
	!		Fixed problem with more then one check per employee
	!		and percentage calculation
	!
	!	01/31/91 - Kevin Handy
	!		Modified to print earnings code instead of an
	!		array that was never set up.
	!
	!	12/18/91 - Kevin Handy
	!		Modified to ignore "A" records in PR_PAY.
	!
	!	04/11/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Removed unsolicited_input stuff.
	!
	!	10/25/96 - Kevin Handy
	!		Reformat source code
	!
	!	03/13/97 - Kevin Handy
	!		Handle FH code.
	!		Use integer for #key
	!
	!	05/29/98 - Kevin Handy
	!		Handle new 'F' deduction code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/28/2000 - Kevin Handy
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
	DECLARE				UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)		PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP	(PR_TRN_PAY)		PR_TRN_PAY_CDD		PR_TRN_PAY
	MAP	(PR_HIS_PAY)		PR_TRN_PAY_CDD		PR_HIS_PAY

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.HB"
	MAP	(PR_TRN_DED)		PR_TRN_DED_CDD		PR_TRN_DED
	MAP	(PR_HIS_DED)		PR_TRN_DED_CDD		PR_HIS_DED

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP	(PR_ERNDED_DEF)		PR_ERNDED_DEF_CDD	PR_ERNDED_DEF
	MAP	(PR_ERNDED_DEF.CH%)	PR_ERNDED_DEF.CH%

	!
	! Dimension
	!
	RECORD ERN_RECORD
		STRING CODE = 2%
		REAL REGHRS
		REAL OVTHRS
		REAL UNITS
		REAL CURRENT
		REAL OVERTIME
	END RECORD

	RECORD DED_RECORD
		STRING ETYPE = 1%
		STRING CODE = 2%
		REAL CURRENT
	END RECORD

	RECORD TAX_RECORD
		STRING CODE = 2%
		STRING TAX_CODE = 2%
		REAL CURRENT
	END RECORD

	RECORD NON_RECORD
		STRING DTYPE = 2%
		STRING CODE = 2%
		REAL CURRENT
	END RECORD

	DIM ERN_RECORD EMP_ERN(30%)
	DIM DED_RECORD EMP_DED(30%)
	DIM TAX_RECORD EMP_TAX(30%)
	DIM NON_RECORD EMP_NON(30%)

	DIM ERN_RECORD DEP_ERN(100%)
	DIM DED_RECORD DEP_DED(100%)
	DIM TAX_RECORD DEP_TAX(100%)
	DIM NON_RECORD DEP_NON(100%)

	DIM ERN_RECORD LOC_ERN(100%)
	DIM DED_RECORD LOC_DED(100%)
	DIM TAX_RECORD LOC_TAX(100%)
	DIM NON_RECORD LOC_NON(100%)

	DIM ERN_RECORD GRAND_ERN(100%)
	DIM DED_RECORD GRAND_DED(100%)
	DIM TAX_RECORD GRAND_TAX(100%)
	DIM NON_RECORD GRAND_NON(100%)

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	!++
	! Abstract:FLD01
	!
	!
	! Index:
	! Datatype:DATE
	! Size:8
	! Required:Y
	!--
	BATCH_NO$ = DATE_STOREDATE(BATCH_NO$) ! Reformat to (YYYYMMDD)
	YYYY$ = LEFT(BATCH_NO$, 4%)

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	!++
	! Abstract:FLD02
	!
	!
	! Index:
	! Datatype:TEXT
	! Size:4
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)
	!++
	! Abstract:FLD03
	!
	!
	! Index:
	! Datatype:TEXT
	! Size:4
	!--

	BY_DEPT$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)
	!++
	! Abstract:FLD04
	!
	!
	! Index:
	! Datatype:TEXT
	! Size:20
	!--

	K_NUM% = 4%
	FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::LOCATION))
	TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::LOCATION))

	IF (BY_DEPT$ = "") OR &
		(INSTR(1%, BY_DEPT$, "?") <> 0%) OR &
		(INSTR(1%, BY_DEPT$, "%") <> 0%) OR &
		(INSTR(1%, BY_DEPT$, "*") <> 0%)
	THEN
		BY_DEPT% = -1%		! BY_DEPT$ may have several depts
	ELSE
		BY_DEPT% = 0%		! BY_DEPT$ is only one dept
	END IF

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

	GOTO 320

315	!
	! Open Pay history if pay journal no found
	!
	USE_HISTORY% = -1%

	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.OPN"
	USE
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	PR_TRN_PAY.CH% = PR_HIS_PAY.CH%

320	!
	! Open Deduction folder
	!
	IF USE_HISTORY% = 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.OPN"
		USE
			FILENAME$ = "PR_TRN_DED"
			CONTINUE HelpError
		END WHEN
	ELSE
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_DED.OPN"
		USE
			FILENAME$ = "PR_HIS_DED"
			CONTINUE HelpError
		END WHEN

		PR_TRN_DED.CH% = PR_HIS_DED.CH%
	END IF

350	!
	! Open ERNDED Definition file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"
	USE
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
360	!
	! Set up titles and whatnot
	!
	TITLE$(1%) = "Management Payroll Summary"
	TITLE$(2%) = "For the Payroll Folder Dated: " + &
		MID(BATCH_NO$, 5%, 2%) + "/" + &
		MID(BATCH_NO$, 7%, 2%) + "/" + &
		LEFT(BATCH_NO$, 4%)

	TITLE$(3%) = ""
	TITLE$(4%) = ""
	TITLE$(5%) = ""

	%PAGE

16000	!******************************************************************
	! Generate a grand total so we can show percentages.
	!******************************************************************

	CALL ENTR_3MESSAGE(SCOPE, "Generate a grand total", 17%)
	GRAND_ERN = 0.0

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

16020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, REGARDLESS
	USE
		CONTINUE 17000 IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	!
	! Skip if not in a valid department
	!
	IF BY_DEPT$ <> ""
	THEN
		GOTO 16020 IF COMP_STRING(PR_EMP_MASTER::DEPT, BY_DEPT$) = 0%
	END IF

	!
	! Check current record
	!
	GOTO 17000 IF (PR_EMP_MASTER::LOCATION > TO_ITEM$) AND &
		TO_ITEM$ <> ""

16100	!
	! Get pay detail information
	!
	WHEN ERROR IN
		FIND #PR_TRN_PAY.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 16200 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

16110	WHEN ERROR IN
		GET #PR_TRN_PAY.CH%, REGARDLESS
	USE
		IF ERR = 11%
		THEN
			PR_TRN_PAY::EMPNUM = ""
			CONTINUE 16200
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

	GOTO 16110 IF PR_TRN_PAY::PTYPE = "A"

	GOTO 16200 IF (PR_EMP_MASTER::EMPNUM <> PR_TRN_PAY::EMPNUM)

16120	TEST_PR_END_DATE$ = PR_TRN_PAY::PR_END_DATE

	GRAND_ERN = GRAND_ERN + PR_TRN_PAY::GROSS

	GOTO 16110

16200	!
	! Get next employee
	!
	GOTO 16020

	%Page

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	GRAND_NUM_EMPLOYEE% = 0%
	LOC_NUM_EMPLOYEE% = 0%
	THIS_LOC$ = "$%^$#^@&$#%^@"

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
	! Skip if not in a valid department
	!
	IF BY_DEPT$ <> ""
	THEN
		GOTO 17020 IF COMP_STRING(PR_EMP_MASTER::DEPT, BY_DEPT$) = 0%
	END IF

	!
	! Check current record
	!
	GOTO ExitTotal IF (PR_EMP_MASTER::LOCATION > TO_ITEM$) AND &
		TO_ITEM$ <> ""

17100	!
	! Get pay detail information
	!
	WHEN ERROR IN
		FIND #PR_TRN_PAY.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17350 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	EMP_ERN%, EMP_TAX%, EMP_DED%, EMP_NON% = 0%
	TEST_PR_END_DATE$ = ""

17110	WHEN ERROR IN
		GET #PR_TRN_PAY.CH%, REGARDLESS
	USE
		IF ERR = 11%
		THEN
			PR_TRN_PAY::EMPNUM = ""
			CONTINUE 17200
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

	GOTO 17200 IF (PR_EMP_MASTER::EMPNUM <> PR_TRN_PAY::EMPNUM) OR &
		(PR_TRN_PAY::PR_END_DATE <> TEST_PR_END_DATE$ AND &
		TEST_PR_END_DATE$ <> "")

17120	TEST_PR_END_DATE$ = PR_TRN_PAY::PR_END_DATE

	GOTO 17130 IF EMP_ERN(LOOP%)::CODE = PR_TRN_PAY::CODE &
		FOR LOOP% = 1% TO EMP_ERN%
	EMP_ERN%, LOOP% = EMP_ERN% + 1%
	EMP_ERN(LOOP%)::CODE = PR_TRN_PAY::CODE
	EMP_ERN(LOOP%)::REGHRS  = 0.0
	EMP_ERN(LOOP%)::OVTHRS  = 0.0
	EMP_ERN(LOOP%)::UNITS   = 0.0
	EMP_ERN(LOOP%)::CURRENT = 0.0
	EMP_ERN(LOOP%)::OVERTIME = 0.0

17130	EMP_ERN(LOOP%)::REGHRS  = FUNC_ROUND(EMP_ERN(LOOP%)::REGHRS  + &
		PR_TRN_PAY::REG_HR, 2%)
	EMP_ERN(LOOP%)::OVTHRS  = FUNC_ROUND(EMP_ERN(LOOP%)::OVTHRS  + &
		PR_TRN_PAY::OVT_HR, 2%)
	EMP_ERN(LOOP%)::UNITS   = FUNC_ROUND(EMP_ERN(LOOP%)::UNITS   + &
		PR_TRN_PAY::PIECE, 2%)

	TEMP1 = FUNC_ROUND(PR_TRN_PAY::OVT_HR * PR_TRN_PAY::HOUR_RATE * &
		PR_TRN_PAY::FACTOR / 100.0, 2%)
	TEMP = PR_TRN_PAY::GROSS - TEMP1

	EMP_ERN(LOOP%)::CURRENT = FUNC_ROUND(EMP_ERN(LOOP%)::CURRENT + TEMP, 2%)
	EMP_ERN(LOOP%)::OVERTIME = FUNC_ROUND(EMP_ERN(LOOP%)::OVERTIME + &
		TEMP1, 2%)

	GOTO 17110

17200	!
	! Get Tax/Ded detail information
	!
	WHEN ERROR IN
		FIND #PR_TRN_DED.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM + TEST_PR_END_DATE$, &
			REGARDLESS
	USE
		CONTINUE 17300 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

17210	WHEN ERROR IN
		GET #PR_TRN_DED.CH%, REGARDLESS
	USE
		CONTINUE 17300 IF ERR = 11%
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

	!
	! If history then set history map to journal
	!
	IF USE_HISTORY%
	THEN
		PR_TRN_DED = PR_HIS_DED
	END IF

	GOTO 17300 IF (PR_EMP_MASTER::EMPNUM <> PR_TRN_DED::EMPNUM) OR &
		(PR_TRN_DED::PR_END_DATE <> TEST_PR_END_DATE$)

17220	GOTO 17250 IF INSTR(1%, "!FI!FH!FW!SW!SX!CW!DW!EW!SU!SI!", &
		PR_TRN_DED::CODE)

	GOTO 17270 IF PR_TRN_DED::DTYPE = "M" OR PR_TRN_DED::DTYPE = "T"

	!
	! Add Deductions
	!
	GOTO 17230 IF EMP_DED(LOOP%)::CODE = PR_TRN_DED::CODE AND &
		EMP_DED(LOOP%)::ETYPE = PR_TRN_DED::DTYPE &
		FOR LOOP% = 1% TO EMP_DED%

	EMP_DED%, LOOP% = EMP_DED% + 1%
	EMP_DED(LOOP%)::ETYPE = PR_TRN_DED::DTYPE
	EMP_DED(LOOP%)::CODE = PR_TRN_DED::CODE
	EMP_DED(LOOP%)::CURRENT = 0.0

17230	EMP_DED(LOOP%)::CURRENT = FUNC_ROUND(EMP_DED(LOOP%)::CURRENT + &
		PR_TRN_DED::AMOUNT, 2%)

	GOTO 17290

17250	!
	! Add taxes
	!
	GOTO 17260 IF (EMP_TAX(LOOP%)::CODE = PR_TRN_DED::CODE) AND &
		(EMP_TAX(LOOP%)::TAX_CODE = PR_TRN_DED::TAX_CODE) &
		FOR LOOP% = 1% TO EMP_TAX%

	EMP_TAX%, LOOP% = EMP_TAX% + 1%
	EMP_TAX(LOOP%)::CODE = PR_TRN_DED::CODE
	EMP_TAX(LOOP%)::TAX_CODE = PR_TRN_DED::TAX_CODE
	EMP_TAX(LOOP%)::CURRENT = 0.0

17260	EMP_TAX(LOOP%)::CURRENT = FUNC_ROUND(EMP_TAX(LOOP%)::CURRENT + &
		PR_TRN_DED::AMOUNT, 2%)

	GOTO 17290

17270	!
	! Add noncompensation items
	!
	GOTO 17280 IF (EMP_NON(LOOP%)::DTYPE = PR_TRN_DED::DTYPE) AND &
		(EMP_NON(LOOP%)::CODE = PR_TRN_DED::CODE) &
		FOR LOOP% = 1% TO EMP_NON%

	EMP_NON%, LOOP% = EMP_NON% + 1%
	EMP_NON(LOOP%)::DTYPE = PR_TRN_DED::CODE
	EMP_NON(LOOP%)::CODE = PR_TRN_DED::CODE
	EMP_NON(LOOP%)::CURRENT = 0.0

17280	EMP_NON(LOOP%)::CURRENT = FUNC_ROUND(EMP_NON(LOOP%)::CURRENT + &
		PR_TRN_DED::AMOUNT, 2%)

17290	!
	! Loop back for next deduction record
	!
	GOTO 17210

17300	GOSUB PrintEmployee

	GOTO ExitTotal IF UTL_REPORTX::STAT

	IF PR_TRN_PAY::EMPNUM = PR_EMP_MASTER::EMPNUM
	THEN
		EMP_ERN% = 0%
		EMP_DED% = 0%
		EMP_TAX% = 0%
		EMP_NON% = 0%
		GOTO 17120
	END IF


17350	!
	! End of report
	!
	GOTO 17020

	%Page

 ExitTotal:
	!
	! Handle end of report
	!
	GOSUB PrintDeptTotal

	GOSUB PrintLocalTotal

	GOSUB PrintGrandTotal

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

 PrintEmployee:
18300	!********************************************************************
	! Now we are finally ready to print the employee data
	!********************************************************************

	!
	! Print department total if necessary
	!
	GOSUB PrintDeptTotal &
		IF ((THIS_LOC$ <> PR_EMP_MASTER::LOCATION) OR &
			(THIS_DEPT$ <> PR_EMP_MASTER::DEPT))

	!
	! Print location total if necessary
	!
	GOSUB PrintLocalTotal &
		IF (THIS_LOC$ <> PR_EMP_MASTER::LOCATION)

	!
	! Count employees
	!
	IF THIS_EMP$ <> PR_EMP_MASTER::EMPNUM
	THEN
		GRAND_NUM_EMPLOYEE% = GRAND_NUM_EMPLOYEE% + 1%
		LOC_NUM_EMPLOYEE% = LOC_NUM_EMPLOYEE% + 1%
		DEP_NUM_EMPLOYEE% = DEP_NUM_EMPLOYEE% + 1%
	END IF

	THIS_EMP$ = PR_EMP_MASTER::EMPNUM

	!*******************************************************************
	! Summarize everything for the location totals
	!*******************************************************************

	CALL SUMMARIZE_TRN( &
		EMP_ERN%, EMP_ERN(), &
		EMP_DED%, EMP_DED(), &
		EMP_TAX%, EMP_TAX(), &
		EMP_NON%, EMP_NON(), &
		DEP_ERN%, DEP_ERN(), &
		DEP_DED%, DEP_DED(), &
		DEP_TAX%, DEP_TAX(), &
		DEP_NON%, DEP_NON() &
	)

	CALL SUMMARIZE_TRN( &
		EMP_ERN%, EMP_ERN(), &
		EMP_DED%, EMP_DED(), &
		EMP_TAX%, EMP_TAX(), &
		EMP_NON%, EMP_NON(), &
		LOC_ERN%, LOC_ERN(), &
		LOC_DED%, LOC_DED(), &
		LOC_TAX%, LOC_TAX(), &
		LOC_NON%, LOC_NON() &
	)

	CALL SUMMARIZE_TRN( &
		EMP_ERN%, EMP_ERN(), &
		EMP_DED%, EMP_DED(), &
		EMP_TAX%, EMP_TAX(), &
		EMP_NON%, EMP_NON(), &
		GRAND_ERN%, GRAND_ERN(), &
		GRAND_DED%, GRAND_DED(), &
		GRAND_TAX%, GRAND_TAX(), &
		GRAND_NON%, GRAND_NON() &
	)

	RETURN

	%Page

 PrintGrandTotal:
18400	!********************************************************************
	! Print the grand totals
	!********************************************************************

	THIS_ERN = 0.0
	THIS_ERN = THIS_ERN + GRAND_ERN(I%)::CURRENT + GRAND_ERN(I%)::OVERTIME &
		FOR I% = 1% TO GRAND_ERN%

	IF (THIS_ERN = 0.0) OR (GRAND_ERN = 0.0)
	THEN
		THIS_ERN = 0.0
	ELSE
		THIS_ERN = (THIS_ERN / GRAND_ERN) * 100.0
	END IF

	TEXT$ = "***GRAND TOTAL***  Number of employees: " + &
		NUM1$(GRAND_NUM_EMPLOYEE%) + &
		"   Payroll End Date: " + PRNT_DATE(TEST_PR_END_DATE$, 6%) + &
		"   Earnings %: " + FORMAT$(THIS_ERN, "###.##") + "%"

	CALL PRINT_TRN( &
		UTL_REPORTX, &
		TITLE$(), &
		TEXT$, &
		GRAND_ERN%, GRAND_ERN(), &
		GRAND_DED%, GRAND_DED(), &
		GRAND_TAX%, GRAND_TAX(), &
		GRAND_NON%, GRAND_NON() &
	)

18490	RETURN

	%Page

 PrintLocalTotal:
18500	!********************************************************************
	! Print the LOC totals
	!********************************************************************

	GOTO 18590 IF LOC_NUM_EMPLOYEE% = 0%
	GOTO 18590 IF (BY_DEPT% = 0%)

	THIS_ERN = 0.0
	THIS_ERN = THIS_ERN + LOC_ERN(I%)::CURRENT + LOC_ERN(I%)::OVERTIME &
		FOR I% = 1% TO LOC_ERN%

	IF (THIS_ERN = 0.0) OR (GRAND_ERN = 0.0)
	THEN
		THIS_ERN = 0.0
	ELSE
		THIS_ERN = (THIS_ERN / GRAND_ERN) * 100.0
	END IF

	TEXT$ = "**Location Totals**  Number of employees: " + &
		NUM1$(LOC_NUM_EMPLOYEE%) + &
		"   Payroll End Date: " + PRNT_DATE(TEST_PR_END_DATE$, 6%) + &
		"   Location: " + THIS_LOC$ + &
		"   Earnings %: " + FORMAT$(THIS_ERN, "###.###") + "%"

	CALL PRINT_TRN( &
		UTL_REPORTX, &
		TITLE$(), &
		TEXT$, &
		LOC_ERN%, LOC_ERN(), &
		LOC_DED%, LOC_DED(), &
		LOC_TAX%, LOC_TAX(), &
		LOC_NON%, LOC_NON() &
	)

	!
	! Start on a new page
	!
	TITLE$(3%) = "Location: " + PR_EMP_MASTER::LOCATION

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 300%)

18590	!
	! Initilize for start of this new location
	!
	THIS_LOC$ = PR_EMP_MASTER::LOCATION

	LOC_NUM_EMPLOYEE% = 0%
	LOC_ERN% = 0%
	LOC_TAX% = 0%
	LOC_DED% = 0%
	LOC_NON% = 0%

	TITLE$(3%) = "Location: " + PR_EMP_MASTER::LOCATION
					! Needed here for
					! first time through.

	RETURN

	%Page

 PrintDeptTotal:
18600	!********************************************************************
	! Print the LOC totals
	!********************************************************************

	GOTO 18690 IF DEP_NUM_EMPLOYEE% = 0%

	THIS_ERN = 0.0
	THIS_ERN = THIS_ERN + DEP_ERN(I%)::CURRENT + DEP_ERN(I%)::OVERTIME &
		FOR I% = 1% TO DEP_ERN%

	IF (THIS_ERN = 0.0) OR (GRAND_ERN = 0.0)
	THEN
		THIS_ERN = 0.0
	ELSE
		THIS_ERN = (THIS_ERN / GRAND_ERN) * 100.0
	END IF

	TEXT$ = "Number of employees: " + NUM1$(DEP_NUM_EMPLOYEE%) + &
		"   Payroll End Date: " + PRNT_DATE(TEST_PR_END_DATE$, 6%) + &
		"   Location:" + THIS_LOC$ + "  Department: " + THIS_DEPT$ + &
		"   Earnings %: " + FORMAT$(THIS_ERN, "###.###") + "%"

	CALL PRINT_TRN( &
		UTL_REPORTX, &
		TITLE$(), &
		TEXT$, &
		DEP_ERN%, DEP_ERN(), &
		DEP_DED%, DEP_DED(), &
		DEP_TAX%, DEP_TAX(), &
		DEP_NON%, DEP_NON() &
	)

18690	!
	! Initilize for start of this new location
	!
	THIS_DEPT$ = PR_EMP_MASTER::DEPT

	DEP_NUM_EMPLOYEE% = 0%
	DEP_ERN% = 0%
	DEP_TAX% = 0%
	DEP_DED% = 0%
	DEP_NON% = 0%

	RETURN

	%Page

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

	%Page

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

	END

20000	!*******************************************************************
	! Subroutine used to summarize all of an employees data into
	! the grand totals.
	!*******************************************************************

	SUB SUMMARIZE_TRN( &
		LONG EMP_ERN, ERN_RECORD EMP_ERN(), &
		LONG EMP_DED, DED_RECORD EMP_DED(), &
		LONG EMP_TAX, TAX_RECORD EMP_TAX(), &
		LONG EMP_NON, NON_RECORD EMP_NON(), &
		LONG GRAND_ERN, ERN_RECORD GRAND_ERN(), &
		LONG GRAND_DED, DED_RECORD GRAND_DED(), &
		LONG GRAND_TAX, TAX_RECORD GRAND_TAX(), &
		LONG GRAND_NON, NON_RECORD GRAND_NON() &
	)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	RECORD ERN_RECORD
		STRING CODE = 2%
		REAL REGHRS
		REAL OVTHRS
		REAL UNITS
		REAL CURRENT
		REAL OVERTIME
	END RECORD

	RECORD DED_RECORD
		STRING ETYPE = 1%
		STRING CODE = 2%
		REAL CURRENT
	END RECORD

	RECORD TAX_RECORD
		STRING CODE = 2%
		STRING TAX_CODE = 2%
		REAL CURRENT
	END RECORD

	RECORD NON_RECORD
		STRING DTYPE = 2%
		STRING CODE = 2%
		REAL CURRENT
	END RECORD


	!
	! Earn totals
	!
	FOR I% = 1% TO EMP_ERN
		FOR J% = 1% TO GRAND_ERN
			IF EMP_ERN(I%)::CODE = GRAND_ERN(J%)::CODE
			THEN
				GRAND_ERN(J%)::REGHRS = GRAND_ERN(J%)::REGHRS + &
					EMP_ERN(I%)::REGHRS
				GRAND_ERN(J%)::OVTHRS = GRAND_ERN(J%)::OVTHRS + &
					EMP_ERN(I%)::OVTHRS
				GRAND_ERN(J%)::UNITS = GRAND_ERN(J%)::UNITS + &
					EMP_ERN(I%)::UNITS
				GRAND_ERN(J%)::CURRENT = GRAND_ERN(J%)::CURRENT + &
					EMP_ERN(I%)::CURRENT
				GRAND_ERN(J%)::OVERTIME = GRAND_ERN(J%)::OVERTIME + &
					EMP_ERN(I%)::OVERTIME
				GOTO LocTtlA
			END IF
		NEXT J%
		GRAND_ERN, J% = GRAND_ERN + 1%
		GRAND_ERN(J%) = EMP_ERN(I%)

 LocTtlA:
	NEXT I%

	!
	! Tax totals
	!
	FOR I% = 1% TO EMP_TAX
		FOR J% = 1% TO GRAND_TAX
			IF (EMP_TAX(I%)::CODE = GRAND_TAX(J%)::CODE) AND &
				(EMP_TAX(I%)::TAX_CODE = GRAND_TAX(J%)::TAX_CODE)
			THEN
				GRAND_TAX(J%)::CURRENT = &
					GRAND_TAX(J%)::CURRENT + &
					EMP_TAX(I%)::CURRENT
				GOTO LocTtlB
			END IF
		NEXT J%
		GRAND_TAX, J% = GRAND_TAX + 1%
		GRAND_TAX(J%) = EMP_TAX(I%)
 LocTtlB:
	NEXT I%

	!
	! Ded totals
	!
	FOR I% = 1% TO EMP_DED
		FOR J% = 1% TO GRAND_DED
			IF EMP_DED(I%)::CODE = GRAND_DED(J%)::CODE AND &
				EMP_DED(I%)::ETYPE = GRAND_DED(J%)::ETYPE
			THEN
				GRAND_DED(J%)::CURRENT = &
					GRAND_DED(J%)::CURRENT + &
					EMP_DED(I%)::CURRENT
				GOTO LocTtlC
			END IF
		NEXT J%
		GRAND_DED, J% = GRAND_DED + 1%
		GRAND_DED(J%) = EMP_DED(I%)
 LocTtlC:
	NEXT I%

	!
	! Non-comp totals
	!
	FOR I% = 1% TO EMP_NON
		FOR J% = 1% TO GRAND_NON
			IF (EMP_NON(I%)::DTYPE = GRAND_NON(J%)::DTYPE) AND &
				(EMP_NON(I%)::CODE = GRAND_NON(J%)::CODE)
			THEN
				GRAND_NON(J%)::CURRENT = &
					GRAND_NON(J%)::CURRENT + &
					EMP_NON(I%)::CURRENT
				GOTO LocTtlE
			END IF
		NEXT J%
		GRAND_NON, J% = GRAND_NON + 1%
		GRAND_NON(J%) = EMP_NON(I%)
 LocTtlE:
	NEXT I%

	END SUB

21000	!*******************************************************************
	! Print out an employee, local total, grand total, ...
	!*******************************************************************

	SUB PRINT_TRN( &
		UTL_REPORTX_CDD UTL_REPORTX, &
		STRING TITLE(), &
		STRING TEXT, &
		LONG EMP_ERN, ERN_RECORD EMP_ERN(), &
		LONG EMP_DED, DED_RECORD EMP_DED(), &
		LONG EMP_TAX, TAX_RECORD EMP_TAX(), &
		LONG EMP_NON, NON_RECORD EMP_NON() &
	)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Map's
	!
	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP	(PR_ERNDED_DEF)		PR_ERNDED_DEF_CDD	PR_ERNDED_DEF
	MAP	(PR_ERNDED_DEF.CH%)	PR_ERNDED_DEF.CH%

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	!
	! Dimensions
	!
	RECORD ERN_RECORD
		STRING CODE = 2%
		REAL REGHRS
		REAL OVTHRS
		REAL UNITS
		REAL CURRENT
		REAL OVERTIME
	END RECORD

	RECORD DED_RECORD
		STRING ETYPE = 1%
		STRING CODE = 2%
		REAL CURRENT
	END RECORD

	RECORD TAX_RECORD
		STRING CODE = 2%
		STRING TAX_CODE = 2%
		REAL CURRENT
	END RECORD

	RECORD NON_RECORD
		STRING DTYPE = 2%
		STRING CODE = 2%
		REAL CURRENT
	END RECORD

	!
	! Set up
	!
	ON ERROR GOTO 21900

	SUB_REGHRS = 0.0
	SUB_OVTHRS = 0.0
	SUB_UNITS = 0.0
	SUB_CURRENT = 0.0
	SUB_OVERTIME = 0.0
	SUB_TAX = 0.0
	SUB_DED = 0.0
	SUB_NON = 0.0

	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT, 10%)

	TEXT$ = "--------Earnings--------   -----Hours-----   -----Amounts----- |" + &
		" -------Taxes------- | ---Miscellaneous Deductions---"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	TEXT$ = "Cd Description             Reg   Ovt Units   Current  Overtime |" + &
		"Taxes        Amount  |Cd Deduction           Amount"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	TEXT$ = "---------------------------------------------------------------|" + &
		"---------------------|-------------------------------"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	WORK_LOOP% = EMP_ERN
	WORK_LOOP% = EMP_TAX IF WORK_LOOP% < EMP_TAX
	WORK_LOOP% = EMP_DED IF WORK_LOOP% < EMP_DED

	FOR I% = 1% TO WORK_LOOP%

		TEXT$ = ""

		! Print earnings data
		IF I% <= EMP_ERN
		THEN
			E_TYPE$ = "P"
			CODE$ = EMP_ERN(I%)::CODE
			GOSUB LookUpED
			TEXT$ = LEFT(CODE$ + "   ", 3%) + &
				LEFT(DESCR$ + SPACE$(20%), 20%) + &
				FORMAT$(EMP_ERN(I%)::REGHRS, "####.##") + &
				FORMAT$(EMP_ERN(I%)::OVTHRS, "###.##") + &
				FORMAT$(EMP_ERN(I%)::UNITS, "####.# ") + &
				FORMAT$(EMP_ERN(I%)::CURRENT, "######.## ") + &
				FORMAT$(EMP_ERN(I%)::OVERTIME, "######.## ")

			SUB_REGHRS = SUB_REGHRS + EMP_ERN(I%)::REGHRS
			SUB_OVTHRS = SUB_OVTHRS + EMP_ERN(I%)::OVTHRS
			SUB_UNITS = SUB_UNITS + EMP_ERN(I%)::UNITS
			SUB_CURRENT = SUB_CURRENT + EMP_ERN(I%)::CURRENT
			SUB_OVERTIME = SUB_OVERTIME + EMP_ERN(I%)::OVERTIME
		END IF

		TEXT$ = LEFT(TEXT$ + SPACE$(63%), 63%) + "|"

		! Print Tax data
		IF I% <= EMP_TAX
		THEN
			E_TYPE$ = EMP_TAX(I%)::CODE
			CODE$ = EMP_TAX(I%)::TAX_CODE

			SELECT E_TYPE$
			CASE "FI"
				DESCR$ = "FICA"
			CASE "FH"
				DESCR$ = "FICA (HI)"
			CASE "FW"
				DESCR$ = "FED/WH"
			CASE "SW"
				DESCR$ = "STATE/WH " + CODE$
			CASE "SX"
				DESCR$ = "OST " + CODE$
			CASE "CW"
				DESCR$ = "CITY/WH " + CODE$
			CASE "DW"
				DESCR$ = "COUNTY/WH " + CODE$
			CASE "EW"
				DESCR$ = "SCHOOL/WH " + CODE$
			CASE "SU"
				DESCR$ = "STATE/UN " + CODE$
			CASE "SI"
				DESCR$ = "WC/INS " + CODE$
			CASE ELSE
				DESCR$ = "?????????????????????????????"
			END SELECT

			TEXT1$ = LEFT(DESCR$ + SPACE$(12%), 12%) + &
				FORMAT$(EMP_TAX(I%)::CURRENT, "#####.## ")

			SUB_TAX = SUB_TAX + EMP_TAX(I%)::CURRENT
		ELSE
			TEXT1$ = SPACE$(21%)
		END IF

		TEXT$ = TEXT$ + TEXT1$ + "|"

		! Print Deduction data
		IF I% <= EMP_DED
		THEN
			E_TYPE$ = EMP_DED(I%)::ETYPE
			CODE$ = EMP_DED(I%)::CODE
			GOSUB LookUpED
			TEXT1$ = &
				LEFT(EMP_DED(I%)::CODE + "   ", 3%) + &
				LEFT(DESCR$ + SPACE$(20%), 20%) + &
				FORMAT$(EMP_DED(I%)::CURRENT, "#####.## ")

			SUB_DED = SUB_DED + EMP_DED(I%)::CURRENT
		ELSE
			TEXT1$ = ""
		END IF

		TEXT$ = TEXT$ + TEXT1$

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		GOTO 21090 IF UTL_REPORTX::STAT

	NEXT I%

	TEXT$ = SPACE$(63%) + "|" + &
		SPACE$(21%) + "|"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
	GOTO 21090 IF UTL_REPORTX::STAT

	TEXT$ = "TOTAL" + SPACE$(18%) + &
		FORMAT$(SUB_REGHRS, "####.##") + &
		FORMAT$(SUB_OVTHRS, "###.##") + &
		FORMAT$(SUB_UNITS, "####.# ") + &
		FORMAT$(SUB_CURRENT, "######.## ") + &
		FORMAT$(SUB_OVERTIME, "######.## ") + &
		"|" + &
		SPACE$(12%) + &
		FORMAT$(SUB_TAX, "#####.## ") + &
		"|" + &
		SPACE$(23%) + &
		FORMAT$(SUB_DED, "#####.## ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
	GOTO 21090 IF UTL_REPORTX::STAT

	TEXT$ = SPACE$(63%) + "|" + &
		SPACE$(21%) + "|   Net             " + &
		FORMAT$(SUB_CURRENT + &
			SUB_OVERTIME - &
			SUB_TAX - &
			SUB_DED, "#########.## ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
	GOTO 21090 IF UTL_REPORTX::STAT

	IF EMP_NON
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), "", 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), &
			"Memo/Non-pay compensation", 0%)
		GOTO 21090 IF UTL_REPORTX::STAT

		FOR I% = 1% TO EMP_NON
			E_TYPE$ = EMP_NON(I%)::DTYPE
			CODE$ = EMP_NON(I%)::CODE
			GOSUB LookUpED

			TEXT$ = LEFT(CODE$ + "   ", 3%) + &
				LEFT(DESCR$ + SPACE$(20%), 20%) + &
				SPACE$(19%) + &
				FORMAT$(EMP_NON(I%)::CURRENT, "######.## ")

			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
			GOTO 21090 IF UTL_REPORTX::STAT
		NEXT I%
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), "", -2%)

21090	EXIT SUB

 LookUpED:
	!********************************************************************
	! Look up ernded description
	!********************************************************************
	DESCR$ = "?????????????????????????????????"

21100	WHEN ERROR IN
		GET #PR_ERNDED_DEF.CH%, KEY #0% EQ E_TYPE$ + CODE$, REGARDLESS
	USE
		CONTINUE 21110
	END WHEN

	DESCR$ = PR_ERNDED_DEF::DESCR

21110	RETURN

21900	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

	END SUB
