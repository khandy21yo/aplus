1	%TITLE "Get Taxable/Reportable Wages"
	%SBTTL "PR_READ_SUBJWAGE"
	%IDENT "V3.6a Calico"

	SUB PR_READ_SUBJWAGE( &
		EMPNUM$, &
		YYYY$, &
		SUBJECT_CODE$, &
		TAX_CODE$(), &
		TAXABLE(,), &
		CODE_LOOP%)

	!
	!	COPYRIGHT (C) 1987 BY
	!	Computer Management Center
	!	Idaho Falls, Idaho
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
	! Computer Management Center
	!
	! Computer Management Center assumes no responsibility for the use
	! or reliability of its software on equipment which is not supported
	! by Computer Management Center.
	!
	!++
	!
	! Abstract:HELP
	!	.p
	!	This function returns the reportable, taxable wages and
	!	the taxes by employee for a given year
	!
	! Index:
	!
	! Option:
	!
	! Input:
	!
	!	EMPNUM$ = Employee number min=1; max=10
	!	YYYY$ = Tax year, format=YYYY
	!	SUBJECT_CODE$ = The tax subject code
	!		FWH = Federal Withholding
	!		FIE = FICA Employee
	!		FHE = FICA Employee
	!		FIR = FICA Employer
	!		FHR = FICA Employer
	!		FUI = Federal Unemployment
	!		SWH = State Withholding
	!		SUI = State Unemployment
	!		OST = Other State Taxes
	!		CWH = City Withholding
	!		DWH = County Withholding
	!		EWH = School Withholding
	!
	! Output:
	!
	!	TAX_CODE() = Tax code for state, city, county
	!	TAXABLE(,) = Taxable pay, first element in the array
	!		is the tax code item, The second
	!		element is the quarter.  The quarter
	!		is from 1 to 4.
	!	CODE_LOOP% = Number of elements in tax code array.
	!
	! Example:
	!
	!	CALL PR_READ_SUBJWAGE("10015", &
	!		"1988", &
	!		TAX_CODE$(), &
	!		TAXABLE(,), &
	!		CODE_LOOP%)
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_READ_SUBJWAGE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PR_READ_SUBJWAGE
	!	$ DELETE PR_READ_SUBJWAGE.OBJ;*
	!
	! Author:
	!
	!	03/15/88 - Robert Peterson
	!
	! Modification history:
	!
	!	04/03/89 - Kevin Handy
	!		Added code for SI tax type.
	!
	!	08/08/90 - Kevin Handy
	!		Changes intended to speed up function a little bit.
	!		Use MAT ZER, select, unroll error trapping.
	!
	!	08/09/90 - Kevin Handy
	!		Removed strange code that looked for "FIR~", since
	!		only three characters are passed at one time.
	!
	!	01/01/91 - Kevin Handy
	!		Modified to use new layout of PR_REG_TAXES file.
	!		Greatly simplified/speed up code.
	!
	!	07/13/91 - Kevin Handy
	!		Removed COM area PR_READ_SUBJWAGE, and modified
	!		to pass PR_REG_TAXES.CH through parameter list.
	!
	!	07/13/91 - Kevin Handy
	!		Removed PR_ERNDED_DEF file which is no longer
	!		used in this program.
	!
	!	07/13/91 - Kevin Handy
	!		Removed PR_REG_ERNDED file which is no longer
	!		used in this program.
	!
	!	07/01/93 - Kevin Handy
	!		Modified to zero WKWRK and TAXWH.
	!		Fixed bug in figuring WKWRK where was summing
	!		up TAXWH_WKWRK which should have been WKWRK.
	!
	!	07/01/93 - Kevin Handy
	!		Lost several parameters: REPORTABLE(),
	!		WKWRK(), and TAXWH() which were not used in
	!		any program calling this function.
	!
	!	05/03/94 - Kevin Handy
	!		Pulled channel for PR_REG_TAXES.CH% from parameter
	!		list into common area, and added open for file
	!		if it isn't open.
	!
	!	05/04/94 - Kevin Handy
	!		Added comments. Disabled edit$() of subject_code$
	!		since it should be coming in ligit.
	!
	!	05/04/94 - Kevin Handy
	!		Added code to handle FUTA/SUTA business.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/25/96 - Kevin Handy
	!		Reformat source code
	!
	!	03/05/98 - Kevin Handy
	!		Fix bug with ADJUSTMENT going from 0 to 3,
	!		and TAXABLE going from 1 to 4.
	!
	!	05/29/98 - Kevin Handy
	!		Handle new 'F' deduction type
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/03/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%PAGE

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.HB"
	MAP (PR_REG_TAXES)	PR_REG_TAXES_CDD	PR_REG_TAXES

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.HB"
	MAP (PR_REG_ERNDED)	PR_REG_ERNDED_CDD	PR_REG_ERNDED

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP (PR_ERNDED_DEF) PR_ERNDED_DEF_CDD PR_ERNDED_DEF

	COM (CH_READ_PR_REG_TAXES) &
		PR_REG_TAXES.CH%, &
		PR_REG_TAXES.PROTECT%, &
		PR_REG_TAXES.YEAR$ = 4%, &
		FUNEG$ = 90%, &
		SUNEG$ = 90%, &
		FUPOS$ = 90%, &
		SUPOS$ = 90%

	COM (CH_PR_REG_ERNDED) &
		PR_REG_ERNDED.CH%

	COM (CH_PR_ERNDED_DEF) &
		PR_ERNDED_DEF.CH%

	%Page

	!
	! Initialize variables
	!
	MAT TAXABLE = ZER

410	!
	! Open TaxWH register if necessary
	!
	IF (PR_REG_TAXES.CH% = 0%) OR (PR_REG_TAXES.YEAR$ <> YYYY$)
	THEN
		!
		! Close previous version
		!
		CLOSE PR_REG_TAXES.CH% IF PR_REG_TAXES.CH%
		CLOSE PR_REG_ERNDED.CH% IF PR_REG_ERNDED.CH%

		!
		! Remember year
		!
		PR_REG_TAXES.YEAR$ = YYYY$

		!
		! Open current version
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.OPN"
			%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.OPN"
		USE
			CONTINUE ExitSub
		END WHEN
	END IF

	%Page

1000	!*****************************************************************
	! Find taxes withheld and total amount recorded as being subject
	! to taxes based on the subject code passed to this function.
	!*****************************************************************

	!
	! Find this employee in the tax register
	!
	SELECT SUBJECT_CODE$
	CASE "OST"
		TAX_TYPE$ = "SX"
	CASE "SWC"
		TAX_TYPE$ = "SI"
	CASE "SUI"
		TAX_TYPE$ = "SW"
	CASE "FUI"
		TAX_TYPE$ = "FW"
	CASE ELSE
		TAX_TYPE$ = LEFT(SUBJECT_CODE$, 2%)
	END SELECT

	!
	! Search for base tax record
	!
	WHEN ERROR IN
		FIND #PR_REG_TAXES.CH%, KEY #0% EQ EMPNUM$ + TAX_TYPE$, REGARDLESS
	USE
		CONTINUE ExitSub IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_REG_TAXES"
		CONTINUE HelpError
	END WHEN

1030	WHEN ERROR IN
		GET #PR_REG_TAXES.CH%, REGARDLESS
	USE
		CONTINUE 2000 IF ERR = 11%
	END WHEN

	GOTO 2000 IF EMPNUM$ <> PR_REG_TAXES::EMPNUM OR &
		TAX_TYPE$ <> PR_REG_TAXES::TTYPE

	!
	! Search for matching code.  This is required for federal
	! taxes, state withholding and OST.
	!
	GOTO 1040 &
		IF TAX_CODE$(LOOP%) = PR_REG_TAXES::CODE &
		FOR LOOP% = 1% TO CODE_LOOP%

	CODE_LOOP%, LOOP% = CODE_LOOP% + 1%

	TAX_CODE$(CODE_LOOP%) = PR_REG_TAXES::CODE

1040	!
	! Add to array
	!
	FOR QTR% = 1% TO 4%
		TAXABLE(LOOP%, QTR%) = &
			FUNC_ROUND(TAXABLE(LOOP%, QTR%) + &
			PR_REG_TAXES::TAXABLE(QTR% - 1%), 2%)

	NEXT QTR%

	GOTO 1030

2000	!*******************************************************************
	!
	! Do we need to go through all that gory business to find out
	! the SUTA and FUTA amounts?
	!
	GOTO ExitSub UNLESS SUBJECT_CODE$ = "FUI" OR SUBJECT_CODE$ = "SUI"

	GOTO 3000 IF (PR_ERNDED_DEF.CH% <> 0%)

2010	!
	! Open ERNDED_DEF file
	!
	FUADJUST_NEGATIVE$ = ""
	FUADJUST_POSITIVE$ = ""

	SUADJUST_NEGATIVE$ = ""
	SUADJUST_POSITIVE$ = ""

	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"
		RESET #PR_ERNDED_DEF.CH%
	USE
		CONTINUE ExitSub
	END WHEN

2020	WHEN ERROR IN
		GET #PR_ERNDED_DEF.CH%, REGARDLESS
	USE
		CONTINUE 2030
	END WHEN

	IF (PR_ERNDED_DEF::REPORTABLE_SWH <> PR_ERNDED_DEF::REPORTABLE_SUI)
	THEN
		IF PR_ERNDED_DEF::REPORTABLE_SWH = "N"
		THEN
			SUADJUST_NEGATIVE$ = SUADJUST_NEGATIVE$ + &
				"," + &
				PR_ERNDED_DEF::ETYPE + PR_ERNDED_DEF::CODE
		ELSE
			SUADJUST_POSITIVE$ = SUADJUST_POSITIVE$ + &
				"," + &
				PR_ERNDED_DEF::ETYPE + PR_ERNDED_DEF::CODE
		END IF

	END IF

	IF (PR_ERNDED_DEF::REPORTABLE_FWH <> PR_ERNDED_DEF::REPORTABLE_FUI)
	THEN
		IF PR_ERNDED_DEF::REPORTABLE_FWH = "N"
		THEN
			FUADJUST_NEGATIVE$ = FUADJUST_NEGATIVE$ + &
				"," + &
				PR_ERNDED_DEF::ETYPE + PR_ERNDED_DEF::CODE
		ELSE
			FUADJUST_POSITIVE$ = FUADJUST_POSITIVE$ + &
				"," + &
				PR_ERNDED_DEF::ETYPE + PR_ERNDED_DEF::CODE
		END IF

	END IF

	GOTO 2020

2030	FUNEG$ = FUADJUST_NEGATIVE$
	SUNEG$ = SUADJUST_NEGATIVE$

	FUPOS$ = FUADJUST_POSITIVE$
	SUPOS$ = SUADJUST_POSITIVE$


3000	!*******************************************************************
	! Calculate any adjustment for this employee
	!*******************************************************************

	IF SUBJECT_CODE$ = "FUI"
	THEN
		ADJUST_NEGATIVE$ = FUNEG$
		ADJUST_POSITIVE$ = FUPOS$
	ELSE
		ADJUST_NEGATIVE$ = SUNEG$
		ADJUST_POSITIVE$ = SUPOS$
	END IF

	GOTO ExitSub IF ADJUST_NEGATIVE$ = "" AND ADJUST_POSITIVE$ = ""

	ADJUSTMENT(I%) = 0.0 FOR I% = 0% TO 3%
	ADJUSTMENT% = 0%

	!
	! Load up any adjustments
	!
	WHEN ERROR IN
		GET #PR_REG_ERNDED.CH%, KEY #0% EQ EMPNUM$, REGARDLESS
	USE
		CONTINUE 3020
	END WHEN

3010	WHILE PR_REG_ERNDED::EMPNUM = EMPNUM$

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
			CONTINUE 3020
		END WHEN
	NEXT

3020	!
	! Add adjustments into totals
	!
	GOTO ExitSub IF ADJUSTMENT% = 0%

	FOR QTR1% = 1% TO 4%
		!
		! Calculate rate to spread out with
		!
		TOTAL = 0.0
		TOTAL = TOTAL + TAXABLE(I%, QTR1%) &
			FOR I% = 1% TO CODE_LOOP%

		IF TOTAL = 0.0
		THEN
			FACTOR = 0.0
		ELSE
			FACTOR = ADJUSTMENT(QTR1% - 1%) / TOTAL
		END IF

		!
		! Spread it out for all but last state
		!
		FOR I% = 1% TO CODE_LOOP% - 1%
			CALC = FUNC_ROUND(TAXABLE(I%, QTR1%) * &
				FACTOR, 2%)
			TAXABLE(I%, QTR1%) = &
				TAXABLE(I%, QTR1%) + &
				CALC
			ADJUSTMENT(QTR1% - 1%) = ADJUSTMENT(QTR1% - 1%) - CALC
		NEXT I%

		!
		! Slap anything left over in last state
		!
		TAXABLE(CODE_LOOP%, QTR1%) = &
			TAXABLE(CODE_LOOP%, QTR1%) + &
			ADJUSTMENT(QTR1% - 1%)

	NEXT QTR1%


 ExitSub:

	EXIT SUB

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitSub

	END SUB
