1	%TITLE "Post Final Payroll Program"
	%SBTTL "PR_SPEC_FOLDERFROMGL"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1998 BY
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! ID:PRFINL
	!
	! Abstract:HELP
	!	.p
	!	Create a Payroll folder from a GL journal
	!
	! Index:
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_SPEC_FOLDERFROMGL
	!	$ LINK/EXECUTABLE=PR_EXE: PR_SPEC_FOLDERFROMGL, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_SPEC_FOLDERFROMGL.OBJ;*
	!
	! Todo:
	!	- Get GL accounts from definition tables
	!	- Run as a report
	!	- Ask for a batch number
	!
	! Author:
	!
	!	03/10/98 - Kevin Handy
	!
	! Modification history:
	!
	!	05/29/98 - Kevin Handy
	!		Handle new 'F' deduction code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/11/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP) GL_YYYY_PP_CDD GL_YYYY_PP

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.HB"
	MAP (PR_TRN_CHECK) PR_TRN_CHECK_CDD PR_TRN_CHECK

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.HB"
	MAP (PR_TRN_DED) PR_TRN_DED_CDD PR_TRN_DED

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP (PR_TRN_PAY) PR_TRN_PAY_CDD PR_TRN_PAY

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER) PR_EMP_MASTER_CDD PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.HB"
	MAP (PR_TAX_PROFILE_F) PR_TAX_PROFILE_F_CDD PR_TAX_PROFILE_F

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_S.HB"
	MAP (PR_TAX_PROFILE_F) PR_TAX_PROFILE_S_CDD PR_TAX_PROFILE_S

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP (PR_ERNDED_DEF) PR_ERNDED_DEF_CDD PR_ERNDED_DEF

	DIM ACCOUNT$(100%), CODE$(100%)

200	!*******************************************************************
	! Get GL Date and open it
	!*******************************************************************

	PRINT "GL Period (YYYY_PP)";
	LINPUT YYYY_PP$
	YYYY_PP$ = EDIT$(YYYY_PP$, 4%)
	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"

300	!*******************************************************************
	! Get payroll date and create folders
	!*******************************************************************

	PRINT "Folder Date (YYYYMMDD)";
	LINPUT BATCH_NO$
	BATCH_NO$ = EDIT$(BATCH_NO$, 4%)

310	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.CRE"

320	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.CRE"

330	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.CRE"

340	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.OPN"

350	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"

400	!*******************************************************************
	! Open up other files
	!*******************************************************************

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"

900	!*******************************************************************
	! Get batch number to process
	!*******************************************************************

1000	!*******************************************************************
	! Load in account table info
	!
	! TODO: Modify to pull accounts from the payroll tables
	!*******************************************************************

	ACCOUNT% = 0%

1010	!
	! Look at the profile
	!
	WHEN ERROR IN
		RESET #PR_TAX_PROFILE.CH%
	USE
		CONTINUE 1090
	END WHEN

1020	WHEN ERROR IN
		GET #PR_TAX_PROFILE.CH%, REGARDLESS
	USE
		CONTINUE 1090
	END WHEN

	SELECT PR_TAX_PROFILE_F::AUTH

	CASE "F"
		!
		! Federal Witholdings
		!
		ACCOUNT% = ACCOUNT% + 1%
		ACCOUNT$(ACCOUNT%) = PR_TAX_PROFILE_F::WH_ACCT
		CODE$(ACCOUNT%) = "CFW  "

		!
		! FICA Witholdings
		!
		ACCOUNT% = ACCOUNT% + 1%
		ACCOUNT$(ACCOUNT%) = PR_TAX_PROFILE_F::FICA_EX_ACCT
		CODE$(ACCOUNT%) = "CFI  "

		!
		! Check
		!
		ACCOUNT% = ACCOUNT% + 1%
		ACCOUNT$(ACCOUNT%) = PR_TAX_PROFILE_F::CASH_ACCT
		CODE$(ACCOUNT%) = "XCK  "

	CASE "S"
		!
		! State Witholdings
		!
		ACCOUNT% = ACCOUNT% + 1%
		ACCOUNT$(ACCOUNT%) = PR_TAX_PROFILE_S::WH_ACCT
		CODE$(ACCOUNT%) = "CSW" + PR_TAX_PROFILE_S::CODE

	END SELECT

	GOTO 1020

1090	CLOSE PR_TAX_PROFILE_F.CH%

1100	!
	! Handle PR_ERNDED_DEF table now
	!
	WHEN ERROR IN
		RESET #PR_ERNDED_DEF.CH%
	USE
		CONTINUE 1190
	END WHEN

1110	WHEN ERROR IN
		GET #PR_ERNDED_DEF.CH%
	USE
		CONTINUE 1190
	END WHEN

	IF TRM$(PR_ERNDED_DEF::DRCR_ACCT) <> ""
	THEN
		!
		! Pay/deduction
		!
		ACCOUNT% = ACCOUNT% + 1%
		ACCOUNT$(ACCOUNT%) = PR_ERNDED_DEF::DRCR_ACCT
		CODE$(ACCOUNT%) = PR_ERNDED_DEF::ETYPE + &
			PR_ERNDED_DEF::CODE + "  "
	ELSE
		IF TRM$(PR_ERNDED_DEF::ACCRUAL_ACCT) <> ""
		THEN
			!
			! Pay/deduction
			!
			ACCOUNT% = ACCOUNT% + 1%
			ACCOUNT$(ACCOUNT%) = PR_ERNDED_DEF::ACCRUAL_ACCT
			CODE$(ACCOUNT%) = PR_ERNDED_DEF::ETYPE + &
				PR_ERNDED_DEF::CODE + "  "
		END IF
	END IF

	GOTO 1110

1190	PRINT "Account Translation Table"
	PRINT CODE$(I%), ACCOUNT$(I%) FOR I% = 1% TO ACCOUNT%
	PRINT

2000	!*******************************************************************
	! Loop through the general ledger data
	!*******************************************************************

	PRINT
	PRINT "GL Batch Number (XXXXXX)";
	LINPUT GLBATCH$
	GLBATCH$ = EDIT$(GLBATCH$, 4%)
	GOTO 2900 IF GLBATCH$ = ""

	FIND #GL_YYYY_PP.CH%, KEY #4% GE GLBATCH$, REGARDLESS

2100	GET #GL_YYYY_PP.CH%, REGARDLESS

	!
	! Ignore things that are not employee records
	!
	GOTO 2000 IF GL_YYYY_PP::BTHNUM <> GLBATCH$
	GOTO 2100 IF GL_YYYY_PP::XREFNO = ""
	GOTO 2100 IF LEFT(GL_YYYY_PP::XREFNO, 1%) = "-"

2110	!
	! Is this really an employee
	!
	IF PR_EMP_MASTER::EMPNUM <> GL_YYYY_PP::XREFNO
	THEN
		WHEN ERROR IN
			GET #PR_EMP_MASTER.CH%, &
				KEY #0% EQ GL_YYYY_PP::XREFNO, &
				REGARDLESS
		USE
			CONTINUE 2100
		END WHEN
	END IF

2150	!
	! Look for some kind of match with the account table we created
	! a while back.
	!
	FOR CODING% = 1% TO ACCOUNT%

		IF COMP_STRING(GL_YYYY_PP::ACCT, ACCOUNT$(CODING%))
		THEN
			SELECT LEFT(CODE$(CODING%), 1%)
			CASE "P"
				!
				! Payment record
				!
				GOSUB 3300
			CASE "D", "F"
				!
				! Deduction Record
				!
				GOSUB 3200
			CASE "C"
				!
				! Calculated Tax Record
				!
				GOSUB 3400
			CASE "X"
				!
				! Check
				!
				GOSUB 3100
			END SELECT

			PRINT IF CCPOS(0%) >= 50%

			!
			! Only one match to a customer
			!
			GOTO 2100
		END IF
	NEXT CODING%


	GOTO 2100

2900	CLOSE GL_YYYY_PP.CH%
	CLOSE PR_TRN_CHECK.CH%
	CLOSE PR_TRN_DED.CH%
	CLOSE PR_TRN_PAY.CH%

	GOTO 32767

	!*******************************************************************
	! Check record?
	!*******************************************************************

3100	PR_TRN_CHECK::EMPNUM = GL_YYYY_PP::XREFNO
	PR_TRN_CHECK::PR_END_DATE = BATCH_NO$
	PR_TRN_CHECK::CHECK = GL_YYYY_PP::CKNO
	PR_TRN_CHECK::CHECK_DATE = GL_YYYY_PP::TRANDAT
	PR_TRN_CHECK::PAYFREQ = PR_EMP_MASTER::PAYFREQ
	PR_TRN_CHECK::UPDATE_FLAG = 7%
	PR_TRN_CHECK::BATCH = GLBATCH$

	WHEN ERROR IN
		PUT #PR_TRN_CHECK.CH%
	USE
		PRINT "*";
		CONTINUE 3190
	END WHEN

3190	PRINT "X";

	RETURN

	!*******************************************************************
	! Deduction record?
	!*******************************************************************

3200	PR_TRN_DED::EMPNUM = GL_YYYY_PP::XREFNO
	PR_TRN_DED::PR_END_DATE = BATCH_NO$
	PR_TRN_DED::DTYPE = MID(CODE$(CODING%), 1%, 1%)
	PR_TRN_DED::CODE = MID(CODE$(CODING%), 2%, 2%)
	PR_TRN_DED::AMOUNT = -GL_YYYY_PP::AMOUNT
	PR_TRN_DED::TAX_CODE = MID(CODE$(CODING%), 4%, 2%)
	PR_TRN_DED::SSTATUS = ""
	PR_TRN_DED::EXEMPT = 0%
	PR_TRN_DED::UPDATE_FLAG = 7%
	PR_TRN_DED::BATCH = GLBATCH$
	PR_TRN_DED::TAXABLE = 0.0
	PR_TRN_DED::REPORTABLE = 0.0
	PR_TRN_DED::ADDEXEMPT = 0%

	WHEN ERROR IN
		PUT #PR_TRN_DED.CH%
	USE
		PRINT "*";
		CONTINUE 3290
	END WHEN

3290	PRINT "D";

	RETURN

	!*******************************************************************
	! Pay record?
	!*******************************************************************

3300	HRS = GL_YYYY_PP::HOURS
	OVT = 0.0

	IF (HRS > 40)
	THEN
		OVT = HRS - 40
		HRS = 40
	END IF

	PR_TRN_PAY::EMPNUM = GL_YYYY_PP::XREFNO
	PR_TRN_PAY::PR_END_DATE = BATCH_NO$
	PR_TRN_PAY::EMP_SKILL = PR_EMP_MASTER::EMP_SKILL
	PR_TRN_PAY::EMP_GRADE = PR_EMP_MASTER::EMP_GRADE
	PR_TRN_PAY::ACCT = GL_YYYY_PP::ACCT
	PR_TRN_PAY::SUBACC = GL_YYYY_PP::SUBACC
	PR_TRN_PAY::OPER = PR_EMP_MASTER::OPER
	PR_TRN_PAY::LOCATION = PR_EMP_MASTER::LOCATION
	PR_TRN_PAY::DEPT = PR_EMP_MASTER::DEPT
	PR_TRN_PAY::WORK_CENTER = PR_EMP_MASTER::WORK_CENTER
	PR_TRN_PAY::UNION = PR_EMP_MASTER::UNION
	PR_TRN_PAY::PTYPE = MID(CODE$(CODING%), 1%, 1%)
	IF GL_YYYY_PP::HOURS <> 0.0
	THEN
		PR_TRN_PAY::RTYPE = "H"
	ELSE
		PR_TRN_PAY::RTYPE = "O"
	END IF
	PR_TRN_PAY::CODE = MID(CODE$(CODING%), 2%, 2%)
	PR_TRN_PAY::PIECE_RATE = 0.0
	PR_TRN_PAY::HOUR_RATE = 0.0
	IF GL_YYYY_PP::HOURS <> 0.0
	THEN
		PR_TRN_PAY::HOUR_RATE = &
			GL_YYYY_PP::AMOUNT / GL_YYYY_PP::HOURS
	END IF
	PR_TRN_PAY::REG_HR = HRS
	PR_TRN_PAY::OVT_HR = OVT
	PR_TRN_PAY::PIECE = GL_YYYY_PP::UNITS
	PR_TRN_PAY::FACTOR = 150%
	PR_TRN_PAY::GROSS = GL_YYYY_PP::AMOUNT
	PR_TRN_PAY::TAX_PKG = PR_EMP_MASTER::TAX_PKG
	PR_TRN_PAY::BATCH_ENTRY = ""
	PR_TRN_PAY::UPDATE_FLAG = 7%
	PR_TRN_PAY::SEQNUM = ""
	PR_TRN_PAY::BATCH = GL_YYYY_PP::BTHNUM
	PR_TRN_PAY::WORKDATE = GL_YYYY_PP::TRANDAT
	PR_TRN_PAY::EQUIPMENT = ""
	PR_TRN_PAY::EQUIPHOUR = 0

	WHEN ERROR IN
		PUT #PR_TRN_PAY.CH%
	USE
		PRINT "*";
		CONTINUE 3390
	END WHEN

3390	PRINT "P";

	RETURN

	!*******************************************************************
	! Calculated Tax record?
	!*******************************************************************

3400	PR_TRN_DED::EMPNUM = GL_YYYY_PP::XREFNO
	PR_TRN_DED::PR_END_DATE = BATCH_NO$
	PR_TRN_DED::DTYPE = MID(CODE$(CODING%), 1%, 1%)
	PR_TRN_DED::CODE = MID(CODE$(CODING%), 2%, 2%)
	PR_TRN_DED::AMOUNT = -GL_YYYY_PP::AMOUNT
	PR_TRN_DED::TAX_CODE = MID(CODE$(CODING%), 4%, 2%)
	PR_TRN_DED::SSTATUS = ""
	PR_TRN_DED::EXEMPT = 0%
	PR_TRN_DED::UPDATE_FLAG = 7%
	PR_TRN_DED::BATCH = GLBATCH$
	PR_TRN_DED::TAXABLE = 0.0
	PR_TRN_DED::REPORTABLE = 0.0
	PR_TRN_DED::ADDEXEMPT = 0%

	WHEN ERROR IN
		PUT #PR_TRN_DED.CH%
	USE
		PRINT "*";
		CONTINUE 3490
	END WHEN

3490	PRINT "C";

	RETURN

32767	END
