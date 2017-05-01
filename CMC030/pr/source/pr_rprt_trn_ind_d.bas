1	%TITLE "Payroll Indirect Dept Register Report"
	%SBTTL "PR_RPRT_TRN_IND_D"
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
	! Computer Management Center, Inc.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	! ID:PR013
	!
	! Abstract:HELP
	!	.p
	!	This program prints a payroll Indirect Register report
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_TRN_IND_D/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_TRN_IND_D, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_TRN_IND_D.OBJ;*
	!
	! Author:
	!
	!	11/27/87 - Robert Peterson
	!
	! Modification history:
	!
	!	03/18/1988 - B. Craig Larsen
	!		This program could use some work on the LD sort
	!		option.  It will only sort by dept within
	!		each location - so it is not strictly by dept.
	!
	!	03/17/89 - Kevin Handy
	!		Modified to check taxable/nontaxable flag correctly.
	!
	!	03/21/89 - Kevin Handy
	!		Fixed problem handling deductions taxable/nontaxable.
	!
	!	05/29/90 - Kevin Handy
	!		Modifications to the way the FACTOR calculations are
	!		done to spread non-taxable amounts.
	!
	!	06/04/90 - Kevin Handy
	!		Fixed minor bug in 05/29/90 modification.
	!
	!	01/15/91 - Craig Tanner
	!		Added YYYY$ to some filename$ in error trapping.
	!
	!	06/04/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	11/05/91 - Kevin Handy
	!		Modified so that SUI_MAX of 0.0 means unlimited,
	!		not a limit of 0.0
	!
	!	12/18/91 - Kevin Handy
	!		Modified to only look at "D" and "P" types in
	!		PR_REG_ERNDED (Ignore "A" types).
	!
	!	12/18/91 - Kevin Handy
	!		Modified to ignore "A" records in PR_PAY.
	!
	!	03/22/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	04/10/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 standards.
	!		Remoce SUBJECT% parameter from PR_READ_SUBJTAX.
	!		Take out disable_unsolicited_input call.
	!
	!	09/12/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/12/97 - Kevin Handy
	!		Use one more digit for fica rates.
	!		Use integer for #key
	!
	!	07/24/97 - Kevin Handy
	!		Allow old FICA rates to work until I can get
	!		everyone switched over
	!
	!	05/29/98 - Kevin Handy
	!		Handle new 'F' final deduction type
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/06/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.HB"
	MAP	(PR_REG_TAXES)		PR_REG_TAXES_CDD	PR_REG_TAXES

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.HB"
	MAP	(PR_REG_ERNDED)		PR_REG_ERNDED_CDD	PR_REG_ERNDED

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP	(PR_ERNDED_DEF)		PR_ERNDED_DEF_CDD	PR_ERNDED_DEF

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.HB"
	MAP	(PR_TAX_TABLE)		PR_TAX_TABLE_CDD	PR_TAX_TABLE

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_S.HB"
	MAP	(PR_TAX_PROFILE_F)	PR_TAX_PROFILE_S_CDD	PR_TAX_PROFILE_S

	!
	! Need to include _F version so that variable length record
	! business will work.
	!
	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.HB"
	MAP	(PR_TAX_PROFILE_F)	PR_TAX_PROFILE_F_CDD	PR_TAX_PROFILE_F

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.HB"
	MAP	(PR_TAX_PKG)		PR_TAX_PKG_CDD		PR_TAX_PKG

	!
	! Dimension
	!
	DIM EMP_NT_CUR(10%), &
		EMP_NT_YTD(10%), &
		EMP_WH_CODE$(10%, 3%), &
		EMP_WH_CODE%(3%), &
		SUI_CODE$(30%), &
		TOTAL_SUI_WAGES(30%), &
		SUI_PCT(30%), &
		SUI_MAX(30%)


	%PAGE

	ON ERROR GOTO 19000

	!
	! Other Variables
	!
	SUBJECT_TYPE_TABLE$ = "FIR!SUI!FUI!"

	TAX_TYPE_TABLE$ = "FI!FW!SW!SX!CW!DW!EW!SU!"

 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	BATCH_NO$ = DATE_STOREDATE(BATCH_NO$)
	YYYY$ = LEFT(BATCH_NO$, 4%)
	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

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

	CASE "LO", "LD"
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
	! Open Pay history folder if journal not found
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
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.OPN"
	ELSE
		%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_DED.OPN"

		PR_TRN_DED.CH% = PR_HIS_DED.CH%
	END IF

330	!
	! Open ERNDED Definition file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"
	USE
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

340	!
	! Open TaxWH register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.OPN"
	USE
		CONTINUE 350 IF ERR = 5%
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		CONTINUE HelpError
	END WHEN

350	!
	! Open ERNDED register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.OPN"
	USE
		CONTINUE 360 IF ERR = 5%
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

360	!
	! Open Tax Table file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.OPN"

		GET #PR_TAX_TABLE.CH%, KEY #0% EQ "F", REGARDLESS
	USE
		CONTINUE 370 IF ERR = 5%
		CONTINUE 370 IF ERR = 155%
		FILENAME$ = "PR_TAX_TABLE_" + YYYY$
		CONTINUE HelpError
	END WHEN

	FICA_EMPR_PCT = (PR_TAX_TABLE::FICA_EMPR_PCT) / 10000.0
	FICA_LIMIT = PR_TAX_TABLE::FICA_LIMIT

	IF FICA_EMPR_PCT > 0.100
	THEN
		FICA_EMPR_PCT = FICA_EMPR_PCT / 10.0
	END IF

	CLOSE PR_TAX_TABLE.CH%

370	!
	! Open Profile
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.OPN"
	USE
		CONTINUE 380 IF ERR = 5%
		FILENAME$ = "PR_TAX_PROFILE"
		CONTINUE HelpError
	END WHEN

375	WHEN ERROR IN
		GET #PR_TAX_PROFILE.CH%, KEY #0% EQ "F  ", REGARDLESS
	USE
		CONTINUE 380 IF ERR = 155%
		FILENAME$ = "PR_TAX_PROFILE"
		CONTINUE HelpError
	END WHEN

	FUI_PCT = PR_TAX_PROFILE_F::FUI_PCT / 100.0
	FUI_MAX = PR_TAX_PROFILE_F::FUI_MAX

380	!
	! Open Tax package
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.OPN"
	USE
		FILENAME$ = "PR_TAX_PKG"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
 !	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SMG_PBID)

	!
	! Set up titles and whatnot
	!
	TITLE$(1%) = "Employer Payroll Tax Expense Report"
	TITLE$(2%) = "For the Payroll Folder Dated: " + &
		MID(BATCH_NO$, 5%, 2%) + "/" + &
		MID(BATCH_NO$, 7%, 2%) + "/" + &
		LEFT(BATCH_NO$, 4%)

	TITLE$(3%) = ""

	TITLE$(4%) = "                                            -----------Subject Pay----------"

	IF SORTBY$ = "LD"
	THEN
		TITLE$(5%) = "Dept   Emp #      Name                            FICA        FUI        SUI"
	ELSE
		TITLE$(5%) = "Emp #      Name                                   FICA        FUI        SUI"
	END IF

	TITLE$(6%) = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	FIRST_TIME% = -1%

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
		GOTO ExitTotal &
			IF (PR_EMP_MASTER::LOCATION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	CASE "LD"
		GOTO ExitTotal IF (PR_EMP_MASTER::DEPT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	CASE ELSE
		GOTO ExitTotal IF (PR_EMP_MASTER::SORT > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	END SELECT

	EMP_SUBJ_CUR(I%, J%) = 0.0 FOR J% = 0% TO 3% FOR I% = 1% TO 10%
	EMP_SUBJ_YTD(I%, J%) = 0.0 FOR J% = 1% TO 3% FOR I% = 1% TO 10%
	EMP_NT_CUR(I%) = 0.0 FOR I% = 1% TO 3%
	EMP_NT_YTD(I%) = 0.0 FOR I% = 1% TO 3%
	EMP_WH_CODE$(I%, J%) = "" FOR J% = 1% TO 3% FOR I% = 1% TO 10%

	EMP_WH_CODE%(I%) = 0% FOR I% = 1% TO 3%

	!
	! Set up for fica and fui
	!
	EMP_WH_CODE%(1%) = 1%
	EMP_WH_CODE%(2%) = 1%

17100	!
	! Get pay detail information
	!
	WHEN ERROR IN
		FIND #PR_TRN_PAY.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17550 IF ERR = 155%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

17110	WHEN ERROR IN
		GET #PR_TRN_PAY.CH%, REGARDLESS
	USE
		CONTINUE 17200 IF ERR = 11%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	!
	! If history the set history map into journal
	!
	IF USE_HISTORY%
	THEN
		PR_TRN_PAY = PR_HIS_PAY
	END IF

	GOTO 17110 IF PR_TRN_PAY::PTYPE = "A"

	GOTO 17200 IF (PR_EMP_MASTER::EMPNUM <> PR_TRN_PAY::EMPNUM)

17120	PKG_WH_CODE$(3%) = EDIT$(PR_EMP_MASTER::SUI_SW, -1%)

	IF PKG_WH_CODE$(3%) = ""
	THEN
		WHEN ERROR IN
			GET #PR_TAX_PKG.CH%, &
				KEY #0% EQ PR_TRN_PAY::TAX_PKG + "SW", &
				REGARDLESS
		USE
			CONTINUE 17140 IF ERR = 155% OR ERR = 5%
			FILENAME$ = "PR_TAX_PKG"
			CONTINUE HelpError
		END WHEN

		PKG_WH_CODE$(3%) = PR_TAX_PKG::CODE
	END IF

17140	FOR TAX_TYPE% = 1% TO LEN(SUBJECT_TYPE_TABLE$) / 4%
		!
		! See if taxable
		!
		SUBJECT_CODE$ = &
			MID(SUBJECT_TYPE_TABLE$, (TAX_TYPE% - 1%) * 4% + 1%, 3%)

		CALL PR_READ_SUBJTAX(PR_ERNDED_DEF.CH%, &
			SUBJECT_CODE$, &
			"P", &
			PR_TRN_PAY::CODE, &
			TAXABLE%, &
			REPORTABLE%)

		GOTO 17170 IF TAXABLE%

		WH_LOOP% = 1%

		IF TAX_TYPE% > 2%
		THEN
			GOTO 17150 &
				IF PKG_WH_CODE$(TAX_TYPE%) = &
				EMP_WH_CODE$(TAX_TYPE%, WH_LOOP%) &
				FOR WH_LOOP% = 1% TO EMP_WH_CODE%(TAX_TYPE%)

			EMP_WH_CODE%(TAX_TYPE%), WH_LOOP% = &
				EMP_WH_CODE%(TAX_TYPE%) + 1%
			EMP_WH_CODE$(TAX_TYPE%, WH_LOOP%) = &
				PKG_WH_CODE$(TAX_TYPE%)

		END IF

17150		IF TAX_TYPE% < 3% OR PKG_WH_CODE$(TAX_TYPE%) <> ""
		THEN
			EMP_SUBJ_CUR(TAX_TYPE%, WH_LOOP%) = &
				FUNC_ROUND(EMP_SUBJ_CUR(TAX_TYPE%, WH_LOOP%) + &
				PR_TRN_PAY::GROSS, 2%)

			IF (PR_TRN_PAY::UPDATE_FLAG AND 1%) = 1%
			THEN
				EMP_SUBJ_YTD(TAX_TYPE%, WH_LOOP%) = &
					FUNC_ROUND(EMP_SUBJ_YTD(TAX_TYPE%, WH_LOOP%) - &
					PR_TRN_PAY::GROSS, 2%)
			END IF
		END IF


		EMP_SUBJ_CUR(TAX_TYPE%, 0%) = &
			FUNC_ROUND(EMP_SUBJ_CUR(TAX_TYPE%, 0%) + &
			PR_TRN_PAY::GROSS, 2%)

17170	NEXT TAX_TYPE%

	GOTO 17110

17200	!-------------------------------------------------------------------
	! Get Tax/Ded detail information
	!-------------------------------------------------------------------

	WHEN ERROR IN
		FIND #PR_TRN_DED.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
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
	! If history the set history map into journal
	!
	IF USE_HISTORY%
	THEN
		PR_TRN_DED = PR_HIS_DED
	END IF

	GOTO 17300 IF PR_EMP_MASTER::EMPNUM <> PR_TRN_DED::EMPNUM

	!
	! See if this is a tax
	!
	GOTO 17290 IF (INSTR(1%, TAX_TYPE_TABLE$, PR_TRN_DED::CODE) + 2%) /3%

	!
	! Check Deductions
	!
	GOTO 17270 IF PR_TRN_DED::DTYPE = "T" OR PR_TRN_DED::DTYPE = "M"

	FOR TAX_TYPE% = 1% TO LEN(SUBJECT_TYPE_TABLE$) / 4%

		!
		! See if taxable and reportable
		!
		SUBJECT_CODE$ = &
			MID(SUBJECT_TYPE_TABLE$, (TAX_TYPE% - 1%) * 4% + 1%, 3%)

		CALL PR_READ_SUBJTAX(PR_ERNDED_DEF.CH%, &
			SUBJECT_CODE$, &
			PR_TRN_DED::DTYPE, &
			PR_TRN_DED::CODE, &
			TAXABLE%, &
			REPORTABLE%)

		IF (TAXABLE% = 0% AND PR_TRN_DED::DTYPE <> "D" AND &
			PR_TRN_DED::DTYPE <> "F") OR &
			(TAXABLE% <> 0% AND (PR_TRN_DED::DTYPE = "D" OR &
			PR_TRN_DED::DTYPE = "F"))
		THEN
			IF TAX_TYPE% < 3%
			THEN
				EMP_SUBJ_CUR(TAX_TYPE%, 1%) = &
					FUNC_ROUND(EMP_SUBJ_CUR(TAX_TYPE%, 1%) - &
						PR_TRN_DED::AMOUNT, 2%)

				IF (PR_TRN_DED::UPDATE_FLAG AND 1%) = 1%
				THEN
					EMP_SUBJ_YTD(TAX_TYPE%, WH_LOOP%) = &
						FUNC_ROUND(EMP_SUBJ_YTD(TAX_TYPE%, WH_LOOP%) - &
						PR_TRN_DED::AMOUNT, 2%)
				END IF
			ELSE
				EMP_NT_CUR(TAX_TYPE%) = FUNC_ROUND(EMP_NT_CUR(TAX_TYPE%) + &
					PR_TRN_DED::AMOUNT, 2%)

				IF (PR_TRN_DED::UPDATE_FLAG AND 1%) = 1%
				THEN
					EMP_NT_YTD(TAX_TYPE%) = FUNC_ROUND(EMP_NT_YTD(TAX_TYPE%) - &
						PR_TRN_DED::AMOUNT, 2%)
				END IF
			END IF
		END IF

	NEXT TAX_TYPE%

	GOTO 17290

17270	!
	! Add noncompensation items
	!


17290	!
	! Loop back for next deduction record
	!
	GOTO 17210

17300	!****************************************************************
	! Look up fica tax and state taxable wages
	!****************************************************************

	WHEN ERROR IN
		FIND #PR_REG_TAXES.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17400 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		CONTINUE HelpError
	END WHEN

17320	WHEN ERROR IN
		GET #PR_REG_TAXES.CH%, REGARDLESS
	USE
		CONTINUE 17400 IF ERR = 11%
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		CONTINUE HelpError
	END WHEN

	GOTO 17400 IF PR_REG_TAXES::EMPNUM <> PR_EMP_MASTER::EMPNUM

	IF PR_REG_TAXES::TTYPE = "FI"
	THEN
		GOTO 17320
	END IF

	GOTO 17320 IF PR_REG_TAXES::TTYPE <> "SW"

	PR_REG_TAXES::CODE = PR_EMP_MASTER::SUI_SW &
		IF EDIT$(PR_EMP_MASTER::SUI_SW, -1%) <> ""

	GOTO 17330 IF PR_REG_TAXES::CODE = EMP_WH_CODE$(3%,WH_LOOP%) &
		FOR WH_LOOP% = 1% TO EMP_WH_CODE%(TAX_TYPE%)

	EMP_WH_CODE%(TAX_TYPE%), WH_LOOP% = EMP_WH_CODE%(TAX_TYPE%) + 1%
	EMP_WH_CODE$(TAX_TYPE%, WH_LOOP%) = PKG_WH_CODE$(TAX_TYPE%)

17330	EMP_SUBJ_YTD(3%, WH_LOOP%) = FUNC_ROUND(EMP_SUBJ_YTD(3%, WH_LOOP%) + &
		PR_REG_TAXES::REPORTABLE(I%), 2%) FOR I% = 0% TO 3%


	GOTO 17320

17400	!--------------------------------------------------------------------
	! Get subject wages from ernded file for FICA and FUI.
	! Get Not taxable wages for SUI
	!--------------------------------------------------------------------
	WHEN ERROR IN
		FIND #PR_REG_ERNDED.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17500 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

17420	WHEN ERROR IN
		GET #PR_REG_ERNDED.CH%, REGARDLESS
	USE
		CONTINUE 17500 IF ERR = 11%
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

	GOTO 17500 IF PR_REG_ERNDED::EMPNUM <> PR_EMP_MASTER::EMPNUM

	GOTO 17420 &
		IF PR_REG_ERNDED::ETYPE <> "P" AND &
		PR_REG_ERNDED::ETYPE <> "D" AND &
		PR_REG_ERNDED::ETYPE <> "F"

	FOR TAX_TYPE% = 1% TO LEN(SUBJECT_TYPE_TABLE$) / 4%

		!
		! See if taxable and reportable
		!
		SUBJECT_CODE$ = MID(SUBJECT_TYPE_TABLE$, &
			(TAX_TYPE% - 1%) * 4% + 1%, 3%)

		CALL PR_READ_SUBJTAX(PR_ERNDED_DEF.CH%, &
			SUBJECT_CODE$, &
			PR_REG_ERNDED::ETYPE, &
			PR_REG_ERNDED::CODE, &
			TAXABLE%, &
			REPORTABLE%)

		TOTAL = 0.0

		TOTAL = FUNC_ROUND(TOTAL + PR_REG_ERNDED::QTR_DOLL(LOOP%), 2%) &
			FOR LOOP% = 0% TO 3%

		IF TAX_TYPE% < 3%
		THEN
			EMP_SUBJ_YTD(TAX_TYPE%, 1%) = &
				FUNC_ROUND(EMP_SUBJ_YTD(TAX_TYPE%, 1%) + &
				TOTAL, 2%) &
				IF PR_REG_ERNDED::ETYPE = "P" AND TAXABLE%

			EMP_SUBJ_YTD(TAX_TYPE%, 1%) = &
				FUNC_ROUND(EMP_SUBJ_YTD(TAX_TYPE%, 1%) - TOTAL, 2%) &
				IF (PR_REG_ERNDED::ETYPE = "D" OR &
				PR_REG_ERNDED::ETYPE = "F") AND &
				TAXABLE% = 0%

		ELSE
			EMP_SUBJ_YTD(TAX_TYPE%, 0%) = &
				FUNC_ROUND(EMP_SUBJ_YTD(TAX_TYPE%, 0%) + &
				TOTAL, 2%) &
				IF PR_REG_ERNDED::ETYPE = "P" AND &
				TAXABLE%

			EMP_NT_YTD(TAX_TYPE%) = &
				FUNC_ROUND(EMP_NT_YTD(TAX_TYPE%) + TOTAL, 2%) &
				IF (PR_REG_ERNDED::ETYPE = "D" OR &
				PR_REG_ERNDED::ETYPE = "F") AND &
				TAXABLE% = 0%

		END IF

	NEXT TAX_TYPE%

	GOTO 17420

17500	!
	! Allocate taxable wages for the current period
	!
	TAX_TYPE% = 3%

	FACTOR = 1.0
	FACTOR = 1.0 - (EMP_NT_CUR(TAX_TYPE%) / EMP_SUBJ_CUR(TAX_TYPE%, 0%)) &
		IF EMP_SUBJ_CUR(TAX_TYPE%, 0%) <> 0.0

	TOTAL_TO_DIST = FUNC_ROUND(EMP_SUBJ_CUR(TAX_TYPE%, 0%) - &
		EMP_NT_CUR(TAX_TYPE%), 2%)

	FOR LOOP% = 1% TO EMP_WH_CODE%(TAX_TYPE%) - 1%
		EMP_SUBJ_CUR(TAX_TYPE%, LOOP%) = &
			FUNC_ROUND(EMP_SUBJ_CUR(TAX_TYPE%, LOOP%) * &
			FACTOR, 2%)
		TOTAL_TO_DIST = &
			FUNC_ROUND(TOTAL_TO_DIST - &
			EMP_SUBJ_CUR(TAX_TYPE%, LOOP%), 2%)
	NEXT LOOP%

	EMP_SUBJ_CUR(TAX_TYPE%, EMP_WH_CODE%(TAX_TYPE%)) = &
		FUNC_ROUND(TOTAL_TO_DIST, 2%)

	!
	! Allocate taxable wages YTD

	TAX_TYPE% = 3%

	FACTOR = 1.0
	FACTOR = 1.0 - (EMP_NT_YTD(TAX_TYPE%) / EMP_SUBJ_YTD(TAX_TYPE%, 0%)) &
		IF EMP_SUBJ_YTD(TAX_TYPE%, 0%) <> 0.0

	TOTAL_TO_DIST = FUNC_ROUND(EMP_SUBJ_YTD(TAX_TYPE%, 0%) - &
		EMP_NT_YTD(TAX_TYPE%), 2%)

	FOR LOOP% = 1% TO EMP_WH_CODE%(TAX_TYPE%) - 1%
		EMP_SUBJ_YTD(TAX_TYPE%, LOOP%) = &
			FUNC_ROUND(EMP_SUBJ_YTD(TAX_TYPE%, LOOP%) * &
			FACTOR, 2%)
		TOTAL_TO_DIST = &
			FUNC_ROUND(TOTAL_TO_DIST - &
			EMP_SUBJ_YTD(TAX_TYPE%, LOOP%), 2%)
	NEXT LOOP%

	EMP_SUBJ_YTD(TAX_TYPE%, EMP_WH_CODE%(TAX_TYPE%)) = &
		FUNC_ROUND(TOTAL_TO_DIST, 2%)

	SELECT SORTBY$
	CASE "LD"
		IF FIRST_TIME%
		THEN
			FIRST_TIME% = 0%
			OLD_DEPT$ = PR_EMP_MASTER::DEPT
		ELSE
			IF PR_EMP_MASTER::DEPT <> OLD_DEPT$
			THEN
				GOSUB SubTotals
				OLD_DEPT$ = PR_EMP_MASTER::DEPT
			END IF
		END IF

	END SELECT

17510	!--------------------------------------------------------------------
	! Determine FICA Taxes
	!--------------------------------------------------------------------

	FICA_WAGES = 0.0

	EMP_SUBJ_YTD(1%, 1%) = EMP_SUBJ_YTD(1%, 1%) + EMP_SUBJ_CUR(1%, 1%)

	IF EMP_SUBJ_YTD(1%, 1%) - EMP_SUBJ_CUR(1%, 1%) < FICA_LIMIT
	THEN
		FICA_EXCESS = EMP_SUBJ_YTD(1%, 1%) - FICA_LIMIT
		FICA_EXCESS = 0.0 IF FICA_EXCESS < 0.0
		FICA_WAGES = EMP_SUBJ_CUR(1%, 1%) - FICA_EXCESS
	END IF

	TOTAL_FICA_WAGES = TOTAL_FICA_WAGES + FICA_WAGES

17520	!--------------------------------------------------------------------
	! Determine FUI Taxes
	!--------------------------------------------------------------------

	FUI_WAGES = 0.0

	EMP_SUBJ_YTD(2%, 1%) = EMP_SUBJ_YTD(2%, 1%) + EMP_SUBJ_CUR(2%, 1%)

	IF EMP_SUBJ_YTD(2%, 1%) - EMP_SUBJ_CUR(2%, 1%) < FUI_MAX
	THEN
		FUI_EXCESS = EMP_SUBJ_YTD(2%, 1%) - FUI_MAX
		FUI_EXCESS = 0.0 IF FUI_EXCESS < 0.0
		FUI_WAGES = EMP_SUBJ_CUR(2%, 1%) - FUI_EXCESS
	END IF

	TOTAL_FUI_WAGES = TOTAL_FUI_WAGES + FUI_WAGES

17530	!-------------------------------------------------------------------
	! Detemine SUI taxes
	!-------------------------------------------------------------------

	SUI_WAGES = 0.0

	FOR LOOP% = 1% TO EMP_WH_CODE%(3%)
		GOTO 17540 IF SUI_CODE$(I%) = EMP_WH_CODE$(3%, LOOP%) &
			FOR I% = 1% TO SUI_LOOP%

		SUI_LOOP%,I% = SUI_LOOP% + 1%
		SUI_CODE$(I%) = EMP_WH_CODE$(3%, LOOP%)
		SUI_PCT(I%) = 0.0
		SUI_MAX(I%) = 0.0

		WHEN ERROR IN
			GET #PR_TAX_PROFILE.CH%, &
				KEY #0% EQ "S" + EMP_WH_CODE$(3%, LOOP%), &
				REGARDLESS
		USE
			CONTINUE 17540 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "PR_TAX_PROFILE"
			CONTINUE HelpError
		END WHEN

		SUI_PCT(I%) = PR_TAX_PROFILE_S::SUI_PCT / 100.0
		SUI_MAX(I%) = PR_TAX_PROFILE_S::SUI_MAX

17540		EMP_SUBJ_YTD(3%, LOOP%) = EMP_SUBJ_YTD(3%, LOOP%) + &
			EMP_SUBJ_CUR(3%, LOOP%)

		IF (EMP_SUBJ_YTD(3%, LOOP%) - &
			EMP_SUBJ_CUR(3%, LOOP%) < SUI_MAX(I%)) OR &
			(SUI_MAX(I%) = 0.0)
		THEN
			IF SUI_MAX(I%) = 0.0
			THEN
				SUI_EXCESS = 0.0
			ELSE
				SUI_EXCESS = EMP_SUBJ_YTD(3%, LOOP%) - &
					SUI_MAX(I%)
				SUI_EXCESS = 0.0 IF SUI_EXCESS < 0.0
			END IF
			SUI_WAGES = FUNC_ROUND(SUI_WAGES + &
				EMP_SUBJ_CUR(3%, LOOP%) - &
				SUI_EXCESS, 2%)
			TOTAL_SUI_WAGES(I%) = FUNC_ROUND(TOTAL_SUI_WAGES(I%) + &
				EMP_SUBJ_CUR(3%, LOOP%) - SUI_EXCESS, 2%)
			TOTAL_SUI_WAGES = FUNC_ROUND(TOTAL_SUI_WAGES + &
				EMP_SUBJ_CUR(3%, LOOP%) - SUI_EXCESS, 2%)
		END IF


	NEXT LOOP%

	!--------------------------------------------------------------------
	! Print subject data
	!--------------------------------------------------------------------

	SELECT SORTBY$
	CASE "LD"
		TEXT$ = PR_EMP_MASTER::DEPT + " " + &
			PR_EMP_MASTER::EMPNUM + " " + &
			LEFT(PR_EMP_MASTER::EMPNAME, 25%) + " " + &
			FORMAT$(FICA_WAGES, "#######.## ") + &
			FORMAT$(FUI_WAGES, "#######.## ") + &
			FORMAT$(SUI_WAGES, "#######.##")

	CASE ELSE
		TEXT$ = PR_EMP_MASTER::EMPNUM + " " + &
			LEFT(PR_EMP_MASTER::EMPNAME, 25%) + " " + &
			FORMAT$(FICA_WAGES, "#######.## ") + &
			FORMAT$(FUI_WAGES, "#######.## ") + &
			FORMAT$(SUI_WAGES, "#######.##")
	END SELECT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

17550	!
	! Next employee
	!
	GOTO 17020

	%Page

 ExitTotal:
	GOSUB SubTotals

	!
	! Handle end of report
	!
	TEXT$ = "Grand Total Subject Wages " + SPACE$(18%) + &
		FORMAT$(G_TOTAL_FICA_WAGES, "#######.## ") + &
		FORMAT$(G_TOTAL_FUI_WAGES, "#######.## ") + &
		FORMAT$(G_TOTAL_SUI_WAGES, "#######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = "Grand FICA Expense     " + &
		FORMAT$(FUNC_ROUND(FICA_EMPR_PCT * G_TOTAL_FICA_WAGES, 2%), &
		"###,###.## ") + &
		FORMAT$(G_TOTAL_FICA_WAGES, "###,###.## ") + &
		FORMAT$(FICA_EMPR_PCT * 100.0, "###.##%")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = "Grand FUI Expense      " + &
		FORMAT$(FUNC_ROUND(FUI_PCT * G_TOTAL_FUI_WAGES, 2%), &
		"###,###.## ") + &
		FORMAT$(G_TOTAL_FUI_WAGES, "###,###.## ") + &
		FORMAT$(FUI_PCT * 100.0, "###.##%")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FOR LOOP% = 1% TO SUI_LOOP%

		TEXT$ = "Grand SUI Expense    " + &
			LEFT(SUI_CODE$(LOOP%) + "  ", 2%) + &
			FORMAT$(FUNC_ROUND(SUI_PCT(LOOP%) * &
			G_TOTAL_SUI_WAGES(LOOP%), 2%), "###,###.## ") + &
			FORMAT$(G_TOTAL_SUI_WAGES(LOOP%), "###,###.## ") + &
			FORMAT$(SUI_PCT(LOOP%) * 100.0, "###.##%")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT

	NEXT LOOP%


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

 SubTotals:
	TEXT$ = "Total Subject Wages " + SPACE$(24%) + &
		FORMAT$(TOTAL_FICA_WAGES, "#######.## ") + &
		FORMAT$(TOTAL_FUI_WAGES, "#######.## ") + &
		FORMAT$(TOTAL_SUI_WAGES, "#######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = "FICA Expense           " + &
		FORMAT$(FUNC_ROUND(FICA_EMPR_PCT * TOTAL_FICA_WAGES, 2%), &
		"###,###.## ") + &
		FORMAT$(TOTAL_FICA_WAGES, "###,###.## ") + &
		FORMAT$(FICA_EMPR_PCT * 100.0, "###.##%")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = "FUI Expense            " + &
		FORMAT$(FUNC_ROUND(FUI_PCT * TOTAL_FUI_WAGES, 2%), &
		"###,###.## ") + &
		FORMAT$(TOTAL_FUI_WAGES, "###,###.## ") + &
		FORMAT$(FUI_PCT * 100.0, "###.##%")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FOR LOOP% = 1% TO SUI_LOOP%

		TEXT$ = "SUI Expense          " + &
			LEFT(SUI_CODE$(LOOP%) + "  ", 2%) + &
			FORMAT$(FUNC_ROUND(SUI_PCT(LOOP%) * &
				TOTAL_SUI_WAGES(LOOP%), 2%), "###,###.## ") + &
			FORMAT$(TOTAL_SUI_WAGES(LOOP%), "###,###.## ") + &
			FORMAT$(SUI_PCT(LOOP%) * 100.0, "###.##%")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT

	NEXT LOOP%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	G_TOTAL_FICA_WAGES = G_TOTAL_FICA_WAGES + TOTAL_FICA_WAGES
	G_TOTAL_FUI_WAGES = G_TOTAL_FUI_WAGES + TOTAL_FUI_WAGES
	G_TOTAL_SUI_WAGES = G_TOTAL_SUI_WAGES + TOTAL_SUI_WAGES
	TOTAL_FICA_WAGES, TOTAL_FUI_WAGES, TOTAL_SUI_WAGES = 0.0

	RETURN

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
