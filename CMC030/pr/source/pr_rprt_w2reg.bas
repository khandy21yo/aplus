1	%TITLE "Payroll W2 Register Report"
	%SBTTL "PR_RPRT_W2REG"
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
	! ID:PRW2RG
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*W-2 Register\* option
	!	prints and reviews for accuracy a W-2 Register report prior
	!	to printing the actual W-2 Forms.
	!	.b
	!	The register can be printed in employee number or alphabetical
	!	order and lists the following fields:
	!	.table 3,25
	!	.te
	!	State
	!	.te
	!	Employee Number
	!	.te
	!	Social Security Number
	!	.te
	!	Taxable State
	!	.te
	!	Taxes-State
	!	.te
	!	Other State Tax
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x W-2 Register
	!	.x Report>W-2 Register
	!	.x Report>Annual>W-2 Register
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_W2REG/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_W2REG, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_W2REG.OBJ;*
	!
	! Author:
	!
	!	11/27/87 - Robert Peterson
	!
	! Modification history:
	!
	!	06/23/89 - Kevin Handy
	!		Opened PR_TEMP file TEMPORARY instead of trying
	!		to remember to delete it.
	!
	!	01/29/90 - Kevin Handy
	!		Completely rewrote section that calculates SU wages.
	!
	!	02/06/90 - Kevin Handy
	!		Added Maximum FICA Limit, so totals would match
	!		up to W2's and other reports.
	!
	!	02/15/90 - Kevin Handy
	!		Added total count of employees.
	!
	!	02/15/90 - Kevin Handy
	!		Removed printing of Zero employees.
	!
	!	01/14/91 - Craig Tanner
	!		Added YYYY$ to some filename$ in error trapping.
	!
	!	03/20/91 - Kevin Handy
	!		Removed an extra '$' on 'PR_TEMP::CODE$' so that
	!		the program would compile.
	!
	!	07/13/91 - Kevin Handy
	!		Removed PR_ERNDED_DEF.CH and PR_REG_ERNDED.CH
	!		from PR_FUNC_READTAXES.
	!
	!	07/13/91 - Kevin Handy
	!		Removed COM area PR_READ_SUBJWAGE.
	!
	!	07/13/91 - Kevin Handy
	!		Removed PR_ERNDED_DEF file which is no longer
	!		used in this program.
	!
	!	07/13/91 - Kevin Handy
	!		Removed PR_REG_ERNDED file which is no longer
	!		used in this program.
	!
	!	09/17/91 - Kevin Handy
	!		Removed unused arrays OT_???.
	!
	!	01/04/92 - Kevin Handy
	!		Modified to show HI and OASDI as seperate units.
	!
	!	03/30/92 - Kevin Handy
	!		Modified to not split HI and OASDI up when HI Rate
	!		is zero (pre 1991).
	!
	!	06/14/93 - Kevin Handy
	!		Added REGARDLESS to get on PR_TAX_TABLE.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/10/95 - Kevin Handy
	!		Added quarter option to the report settings screen,
	!		as per Double L request.
	!
	!	10/25/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	12/26/96 - Kevin Handy
	!		Add line number 430.
	!		Reformat source code.
	!
	!	03/13/97 - Kevin Handy
	!		Modified to handle FH taxes
	!
	!	05/12/97 - Kevin Handy
	!		Use one more digit for fica rates
	!		Use integer for #key
	!
	!	07/24/97 - Kevin Handy
	!		Allow old FICA rates to work until I get
	!		everyone converted.
	!
	!	08/29/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	02/08/99 - Kevin Handy
	!		Fix up so older files won't be goofy on reports.
	!
	!	12/01/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PR_STRUCTURE.INC"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.HB"
	MAP	(PR_REG_TAXES)	PR_REG_TAXES_CDD	PR_REG_TAXES

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.HB"
	MAP (PR_TAX_TABLE) PR_TAX_TABLE_CDD PR_TAX_TABLE

	RECORD PR_TEMP_CDD
		STRING	TTYPE = 2%
		STRING	CODE = 2%
		STRING	EMPNUM = 10%
		REAL	TAXABLE_WAGE(3%)
		REAL	TAXES(3%)
	END RECORD

	MAP (PR_TEMP) PR_TEMP_CDD PR_TEMP

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION PR_FUNC_READTAXES

	!
	! Dimensions
	!
	DIM PR_TAXES_STRUCT PR_TAXES(50%)

	%PAGE

	ON ERROR GOTO 19000

	!
	! Declare channels
	!
	CALL ASSG_CHANNEL(PR_TEMP.CH%, STAT%)

 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	YYYY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	.ts 55
	!	^*(01) Year	YYYY\*
	!	.b
	!	.lm +5
	!	The ^*Year\* field
	!	indicates the calendar year for which the W-2 Register is
	!	to be printed.
	!	.lm -5
	!
	! Index:
	!	.x Year>W-2 Register
	!	.x W-2 Register>Year
	!
	!--

	QTR% = VAL%(TRM$(UTL_REPORTX::OPTDEF(1%)))
	QTR% = 4% IF QTR% < 0% OR QTR% > 4%

	!++
	! Abstract:FLD02
	!	.ts 55
	!	^*(01) Quarter	1,2,3,4\*
	!	.b
	!	.lm +5
	!	Specifies the last quarter to include.
	!	.lm -5
	!
	! Index:
	!	.x Year>W-2 Register
	!	.x W-2 Register>Year
	!
	!--

	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

330	!
	! Open Tax withholding register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.OPN"
	USE
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		CONTINUE HelpError
	END WHEN

340	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.OPN"

	GET #PR_TAX_TABLE.CH%, KEY #0% EQ "F  ", REGARDLESS

	FICA_LIMIT = PR_TAX_TABLE::FICA_LIMIT
	FICA_LIMIT_HI = PR_TAX_TABLE::FICA_LIMIT_HI
	FICA_EMPE_PCT = PR_TAX_TABLE::FICA_EMPE_PCT / 10000.0
	FICA_EMPE_PCT_HI = PR_TAX_TABLE::FICA_EMPE_PCT_HI / 10000.0

	IF FICA_EMPE_PCT > 0.10
	THEN
		FICA_EMPE_PCT = FICA_EMPE_PCT / 10.0
		FICA_EMPE_PCT_HI = FICA_EMPE_PCT_HI / 10.0
	END IF

400	!
	! Create W2 Register file
	!
	CALL ENTR_3MESSAGE(SCOPE, &
		"Creating work file.  Reading Pay Registers.", 1%)

	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "PR_TEMP.TMP" FOR OUTPUT &
			AS FILE PR_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP PR_TEMP, &
			TEMPORARY, &
			BUFFER 32%, &
			PRIMARY KEY (PR_TEMP::TTYPE, &
				PR_TEMP::CODE, &
				PR_TEMP::EMPNUM) &
			DUPLICATES, &
			ACCESS MODIFY, ALLOW NONE
	USE
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

410	WHEN ERROR IN
		RESET #PR_EMP_MASTER.CH%
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

420	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, REGARDLESS
	USE
		CONTINUE ReportTitle IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	X% = PR_FUNC_READTAXES(PR_EMP_MASTER::EMPNUM, &
		PR_REG_TAXES.CH%, &
		PR_TAXES%, &
		PR_TAXES())

	PR_TEMP::EMPNUM = PR_EMP_MASTER::EMPNUM

	!*******************************************************************
	! Load federal record
	!
	PR_TEMP::TTYPE = "FW"
	PR_TEMP::CODE = ""

	PR_TEMP::TAXABLE_WAGE(1%) = 0.0
	PR_TEMP::TAXES(1%) = 0.0
	PR_TEMP::TAXABLE_WAGE(2%) = 0.0
	PR_TEMP::TAXES(2%) = 0.0
	PR_TEMP::TAXABLE_WAGE(3%) = 0.0
	PR_TEMP::TAXES(3%) = 0.0

	STATE$ = ""

	FOR I% = 1% TO PR_TAXES%

		SELECT PR_TAXES(I%)::TTYPE

		CASE "FW"
			PR_TEMP::TAXABLE_WAGE(1%) = &
				PR_TEMP::TAXABLE_WAGE(1%) + &
				PR_TAXES(I%)::REPORTABLE(Q%) &
				FOR Q% = 0% TO QTR% - 1%
			PR_TEMP::TAXES(1%) = PR_TEMP::TAXES(1%) + &
				PR_TAXES(I%)::TAX(Q%) &
				FOR Q% = 0% TO QTR% - 1%

		CASE "FI"
			PR_TEMP::TAXABLE_WAGE(2%) = &
				PR_TEMP::TAXABLE_WAGE(2%) + &
				PR_TAXES(I%)::REPORTABLE(Q%) &
				FOR Q% = 0% TO QTR% - 1%
			PR_TEMP::TAXABLE_WAGE(2%) = FICA_LIMIT &
				IF PR_TEMP::TAXABLE_WAGE(2%) > FICA_LIMIT
			PR_TEMP::TAXES(2%) = PR_TEMP::TAXES(2%) + &
				PR_TAXES(I%)::TAX(Q%) &
				FOR Q% = 0% TO QTR% - 1%

		CASE "FH"
			PR_TEMP::TAXABLE_WAGE(3%) = &
				PR_TEMP::TAXABLE_WAGE(3%) + &
				PR_TAXES(I%)::REPORTABLE(Q%) &
				FOR Q% = 0% TO QTR% - 1%
			PR_TEMP::TAXABLE_WAGE(3%) = FICA_LIMIT_HI &
				IF PR_TEMP::TAXABLE_WAGE(3%) > FICA_LIMIT_HI
			PR_TEMP::TAXES(3%) = PR_TEMP::TAXES(3%) + &
				PR_TAXES(I%)::TAX(Q%) &
				FOR Q% = 0% TO QTR% - 1%

		CASE "SW", "SI", "SX", "SU"
			!
			! We need to know all of the states for creating
			! the taxes for the states in the next section.
			!
			STATE$ = STATE$ + PR_TAXES(I%)::CODE + "!" &
				IF INSTR(1%, STATE$, PR_TAXES(I%)::CODE) = 0%

		END SELECT

	NEXT I%

	!
	! Handle the *obscene* older files that don't split out the
	! FICA correctly between HI and OASDI
	!
	IF (PR_TEMP::TAXABLE_WAGE(3%) = 0.0) AND &
		(PR_TEMP::TAXABLE_WAGE(2%) <> 0.0)
	THEN
		!
		! Hope they don't go over the limit (aaarrrggghhh)
		!
		PR_TEMP::TAXABLE_WAGE(3%) = PR_TEMP::TAXABLE_WAGE(2%)

		!
		! Split the medicare off of the social security
		!
		SSTAXX = PR_TEMP::TAXES(2%) + PR_TEMP::TAXES(3%)
		PR_TEMP::TAXES(2%) = &
			FUNC_ROUND(PR_TEMP::TAXABLE_WAGE(2%) * FICA_EMPE_PCT, 2%)
		PR_TEMP::TAXES(3%) = &
			SSTAXX - PR_TEMP::TAXES(2%)
	END IF

	IF (PR_TEMP::TAXES(2%) <> 0.0) AND &
		(PR_TEMP::TAXES(3%) = 0.0)
	THEN
		!
		! Split the medicare off of the social security
		!
		SSTAXX = PR_TEMP::TAXES(2%) + PR_TEMP::TAXES(3%)
		PR_TEMP::TAXES(2%) = &
			FUNC_ROUND(PR_TEMP::TAXABLE_WAGE(2%) * FICA_EMPE_PCT, 2%)
		PR_TEMP::TAXES(3%) = &
			SSTAXX - PR_TEMP::TAXES(2%)
	END IF

430	PUT #PR_TEMP.CH% &
		IF (PR_TEMP::TAXABLE_WAGE(1%) <> 0.0) OR &
			(PR_TEMP::TAXABLE_WAGE(2%) <> 0.0) OR &
			(PR_TEMP::TAXES(1%) <> 0.0) OR &
			(PR_TEMP::TAXES(2%) <> 0.0)

	!*******************************************************************
	! Load State records
	!
	PR_TEMP::TTYPE = "SW"

	FOR STATE% = 1% TO LEN(STATE$) / 3%
		PR_TEMP::CODE = MID(STATE$, STATE% * 3% - 2%, 2%)

		PR_TEMP::TAXABLE_WAGE(1%) = 0.0
		PR_TEMP::TAXES(1%) = 0.0
		PR_TEMP::TAXABLE_WAGE(2%) = 0.0
		PR_TEMP::TAXES(2%) = 0.0

		FOR I% = 1% TO PR_TAXES%

			SELECT PR_TAXES(I%)::TTYPE

			CASE "SW"
				IF PR_TAXES(I%)::CODE = PR_TEMP::CODE
				THEN
					PR_TEMP::TAXABLE_WAGE(1%) = &
						PR_TEMP::TAXABLE_WAGE(1%) + &
						PR_TAXES(I%)::REPORTABLE(Q%) &
						FOR Q% = 0% TO QTR% - 1%
					PR_TEMP::TAXES(1%) = &
						PR_TEMP::TAXES(1%) + &
						PR_TAXES(I%)::TAX(Q%) &
						FOR Q% = 0% TO QTR% - 1%
				END IF

			CASE "SX", "SI", "SU"
				IF PR_TAXES(I%)::CODE = PR_TEMP::CODE
				THEN
					PR_TEMP::TAXABLE_WAGE(2%) = &
						PR_TEMP::TAXABLE_WAGE(2%) + &
						PR_TAXES(I%)::REPORTABLE(Q%) &
						FOR Q% = 0% TO QTR% - 1%
					PR_TEMP::TAXES(2%) = &
						PR_TEMP::TAXES(2%) + &
						PR_TAXES(I%)::TAX(Q%) &
						FOR Q% = 0% TO QTR% - 1%
				END IF

			END SELECT

		NEXT I%

		PUT #PR_TEMP.CH% &
			IF (PR_TEMP::TAXABLE_WAGE(1%) <> 0.0) OR &
				(PR_TEMP::TAXABLE_WAGE(2%) <> 0.0) OR &
				(PR_TEMP::TAXES(1%) <> 0.0) OR &
				(PR_TEMP::TAXES(2%) <> 0.0)

	NEXT STATE%

	!*******************************************************************
	! Load City/County/District records
	!
	FOR I% = 1% TO PR_TAXES%

		SELECT PR_TAXES(I%)::TTYPE

		CASE "CW"
			PR_TEMP::TTYPE = "CW"
			PR_TEMP::CODE = PR_TAXES(I%)::CODE

			PR_TEMP::TAXABLE_WAGE(1%) = 0.0
			PR_TEMP::TAXABLE_WAGE(1%) = &
				PR_TEMP::TAXABLE_WAGE(1%) + &
				PR_TAXES(I%)::REPORTABLE(Q%) &
				FOR Q% = 0% TO QTR% - 1%
			PR_TEMP::TAXES(1%) = 0.0
			PR_TEMP::TAXES(1%) = PR_TEMP::TAXES(1%) + &
				PR_TAXES(I%)::TAX(Q%) &
				FOR Q% = 0% TO QTR% - 1%
			PR_TEMP::TAXABLE_WAGE(2%) = 0.0
			PR_TEMP::TAXES(2%) = 0.0

			PUT #PR_TEMP.CH% &
				IF (PR_TEMP::TAXABLE_WAGE(1%) <> 0.0) OR &
					(PR_TEMP::TAXABLE_WAGE(2%) <> 0.0) OR &
					(PR_TEMP::TAXES(1%) <> 0.0) OR &
					(PR_TEMP::TAXES(2%) <> 0.0)

		CASE "DW"
			PR_TEMP::TTYPE = "DW"
			PR_TEMP::CODE = PR_TAXES(I%)::CODE

			PR_TEMP::TAXABLE_WAGE(1%) = 0.0
			PR_TEMP::TAXABLE_WAGE(1%) = &
				PR_TEMP::TAXABLE_WAGE(1%) + &
				PR_TAXES(I%)::REPORTABLE(Q%) &
				FOR Q% = 0% TO QTR% - 1%
			PR_TEMP::TAXES(1%) = 0.0
			PR_TEMP::TAXES(1%) = &
				PR_TEMP::TAXES(1%) + PR_TAXES(I%)::TAX(Q%) &
				FOR Q% = 0% TO QTR% - 1%
			PR_TEMP::TAXABLE_WAGE(2%) = 0.0
			PR_TEMP::TAXES(2%) = 0.0

			PUT #PR_TEMP.CH% &
				IF (PR_TEMP::TAXABLE_WAGE(1%) <> 0.0) OR &
					(PR_TEMP::TAXABLE_WAGE(2%) <> 0.0) OR &
					(PR_TEMP::TAXES(1%) <> 0.0) OR &
					(PR_TEMP::TAXES(2%) <> 0.0)

		CASE "EW"
			PR_TEMP::TTYPE = "EW"
			PR_TEMP::CODE = PR_TAXES(I%)::CODE

			PR_TEMP::TAXABLE_WAGE(1%) = 0.0
			PR_TEMP::TAXABLE_WAGE(1%) = &
				PR_TEMP::TAXABLE_WAGE(1%) + &
				PR_TAXES(I%)::REPORTABLE(Q%) &
				FOR Q% = 0% TO QTR% - 1%
			PR_TEMP::TAXES(1%) = 0.0
			PR_TEMP::TAXES(1%) = &
				PR_TEMP::TAXES(1%) + &
				PR_TAXES(I%)::TAX(Q%) &
				FOR Q% = 0% TO QTR% - 1%
			PR_TEMP::TAXABLE_WAGE(2%) = 0.0
			PR_TEMP::TAXES(2%) = 0.0

			PUT #PR_TEMP.CH% &
				IF (PR_TEMP::TAXABLE_WAGE(1%) <> 0.0) OR &
					(PR_TEMP::TAXABLE_WAGE(2%) <> 0.0) OR &
					(PR_TEMP::TAXES(1%) <> 0.0) OR &
					(PR_TEMP::TAXES(2%) <> 0.0)

		END SELECT

	NEXT I%

	GOTO 420

	%PAGE

 ReportTitle:
	CALL ENTR_3MESSAGE(SCOPE, "", 1%)

	!
	! Set up titles and whatnot
	!
	TITLE$(1%) = "W2 Register"
	TITLE$(2%) = "For the year of " + YYYY$
	TITLE$(3%) = ""

	TITLE$(4%) = "."
	TITLE$(5%) = "Emp #      Name                       " + &
			"SSN             Taxable Fed    Taxes Fed Taxable F" + &
			"ICA   Taxes Fica Taxable OASDI  Taxes OASDI"

	TITLE$(6%) = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		RESET #PR_TEMP.CH%
	USE
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

	PASS_1% = 0%
	EMP_COUNT% = 0%

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_TEMP.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

17040	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, KEY #0% EQ PR_TEMP::EMPNUM, REGARDLESS
	USE
		PR_EMP_MASTER::EMPNAME = STRING$(LEN(PR_EMP_MASTER::EMPNAME), 63%)
		PR_EMP_MASTER::SSN = STRING$(LEN(PR_EMP_MASTER::SSN), 63%)

		CONTINUE 17050 IF ERR = 155%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

17050	!
	! Should there be a subtotal now
	!
	IF TEST_TTYPE$ <> PR_TEMP::TTYPE OR TEST_CODE$ <> PR_TEMP::CODE
	THEN
		IF PASS_1%
		THEN
			SELECT TEST_TTYPE$

			CASE "FW"
				GOSUB FedTotal
			CASE "SW"
				GOSUB StateTotal
			CASE "CW"
				GOSUB CityTotal
			CASE "DW"
				GOSUB CountyTotal
			CASE "EW"
				GOSUB DistrictTotal
			END SELECT
		END IF

		!
		! Check to see if end of report
		!
		GOTO ExitProgram IF UTL_REPORTX::STAT

		!
		! Change the headings now
		!
		SELECT PR_TEMP::TTYPE
		CASE "FW"
			TITLE$(5%) = "Emp #      Name                       " + &
				"SSN             Taxable Fed    Taxes Fed Taxable F" + &
				"ICA   Taxes Fica Taxable OASDI  Taxes OASDI"
			TITLE$(4%) = "Federal: " + PR_TEMP::CODE
			AREA$ = PR_TEMP::CODE

		CASE "SW"
			TITLE$(5%) = "Emp #      Name                       " + &
				"SSN           Taxable State  Taxes State          " + &
				"OST"
			TITLE$(4%) = "State: " + PR_TEMP::CODE
			AREA$ = PR_TEMP::CODE

		CASE "CW"
			TITLE$(5%) = "Emp #      Name                       " + &
				"SSN            Taxable City   Taxes City"
			TITLE$(4%) = "City: " + PR_TEMP::CODE
			AREA$ = PR_TEMP::CODE

		CASE "DW"
			TITLE$(5%) = "Emp #      Name                       " + &
				"SSN          Taxable County Taxes County"
			TITLE$(4%) = "County: " + PR_TEMP::CODE
			AREA$ = PR_TEMP::CODE

		CASE "EW"
			TITLE$(5%) = "Emp #      Name                       " + &
				"SSN        Taxable District Taxes District"
			TITLE$(4%) = "District: " + PR_TEMP::CODE
			AREA$ = PR_TEMP::CODE

		END SELECT

		IF PASS_1%
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 1000%)

			GOTO ExitProgram IF UTL_REPORTX::STAT
		END IF

	END IF

	!
	! Select type to be printed
	!
	SELECT PR_TEMP::TTYPE
	CASE "FW"
		GOSUB Fed
	CASE "SW"
		GOSUB State
	CASE "CW"
		GOSUB City
	CASE "DW"
		GOSUB County
	CASE "EW"
		GOSUB District
	END SELECT

	!
	! Check to see if end of report
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Set test variables
	!
	TEST_TTYPE$ = PR_TEMP::TTYPE
	TEST_CODE$ = PR_TEMP::CODE

	TOTAL_TAXABLE_WAGE(I%) = TOTAL_TAXABLE_WAGE(I%) + PR_TEMP::TAXABLE_WAGE(I%) &
		FOR I% = 1% TO 3%

	TOTAL_TAXES(I%) = TOTAL_TAXES(I%) + PR_TEMP::TAXES(I%) &
		FOR I% = 1% TO 3%

17350	!
	! Try for next record
	!
	GOTO 17020

 ExitTotal:
	!
	! Handle end of report
	!
	IF PASS_1%
	THEN
		SELECT TEST_TTYPE$
		CASE "FW"
			GOSUB FedTotal
		CASE "SW"
			GOSUB StateTotal
		CASE "CW"
			GOSUB CityTotal
		CASE "DW"
			GOSUB CountyTotal
		CASE "EW"
			GOSUB DistrictTotal
		END SELECT
	END IF

 ExitProgram:
17500	CALL OUTP_FINISH(UTL_REPORTX)

17510	!
	! Exit to next program or menu
	!
	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

	%PAGE


 Fed:	!******************************************************************
	! Print federal tax information
	!******************************************************************
	PASS_1% = -1%

	TEXT$ = PR_TEMP::EMPNUM + " " + &
		LEFT(PR_EMP_MASTER::EMPNAME, 25%) + "  " + &
		LEFT(PRNT_SSN(PR_EMP_MASTER::SSN, 0%) + SPACE$(13%), 13%) + "  " + &
		FORMAT$(PR_TEMP::TAXABLE_WAGE(1%), "#,###,###.## ") + &
		FORMAT$(PR_TEMP::TAXES(1%), "#,###,###.## ") + &
		FORMAT$(PR_TEMP::TAXABLE_WAGE(2%), "#,###,###.## ") + &
		FORMAT$(PR_TEMP::TAXES(2%), "#,###,###.## ") + &
		FORMAT$(PR_TEMP::TAXABLE_WAGE(3%), "#,###,###.## ") + &
		FORMAT$(PR_TEMP::TAXES(3%), "#,###,###.## ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	EMP_COUNT% = EMP_COUNT% + 1%

	RETURN

 State:	!******************************************************************
	! Print State tax information
	!******************************************************************
	PASS_1% = -1%

	TEXT$ = PR_TEMP::EMPNUM + " " + &
		LEFT(PR_EMP_MASTER::EMPNAME, 25%) + "  " + &
		LEFT(PRNT_SSN(PR_EMP_MASTER::SSN, 0%) + SPACE$(13%), 13%) + "  " + &
		FORMAT$(PR_TEMP::TAXABLE_WAGE(1%), "#,###,###.## ") + &
		FORMAT$(PR_TEMP::TAXES(1%), "#,###,###.## ") + &
		FORMAT$(PR_TEMP::TAXES(2%), "#,###,###.## ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	EMP_COUNT% = EMP_COUNT% + 1%

	RETURN

 City:	!******************************************************************
	! Print City tax information
	!******************************************************************
 County:
	!******************************************************************
	! Print County tax information
	!******************************************************************
 District:
	!******************************************************************
	! Print District tax information
	!******************************************************************
	PASS_1% = -1%

	TEXT$ = PR_TEMP::EMPNUM + " " + &
		LEFT(PR_EMP_MASTER::EMPNAME, 25%) + "  " + &
		LEFT(PR_EMP_MASTER::SSN + SPACE$(13%), 13%) + "  " + &
		FORMAT$(PR_TEMP::TAXABLE_WAGE(1%), "#,###,###.## ") + &
		FORMAT$(PR_TEMP::TAXES(1%), "#,###,###.## ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	EMP_COUNT% = EMP_COUNT% + 1%

	RETURN

	%Page

 FedTotal:
	!******************************************************************
	! Print Federal totals
	!******************************************************************
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEMP$ = "*** Federal Totals"

	TEXT$ = SPACE$(11%) + &
		LEFT(TEMP$ + SPACE$(25%), 25%) + &
		SPACE$(17%) + &
		FORMAT$(TOTAL_TAXABLE_WAGE(1%), "#,###,###.## ") + &
		FORMAT$(TOTAL_TAXES(1%), "#,###,###.## ") + &
		FORMAT$(TOTAL_TAXABLE_WAGE(2%), "#,###,###.## ") + &
		FORMAT$(TOTAL_TAXES(2%), "#,###,###.## ") + &
		FORMAT$(TOTAL_TAXABLE_WAGE(3%), "#,###,###.## ") + &
		FORMAT$(TOTAL_TAXES(3%), "#,###,###.## ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "           *** Total Employees: " + NUM1$(EMP_COUNT%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -2%)
	EMP_COUNT% = 0%

	TOTAL_TAXABLE_WAGE(I%) = 0.0 FOR I% = 1% TO 3%
	TOTAL_TAXES(I%) = 0.0 FOR I% = 1% TO 3%

	RETURN

 StateTotal:
	!******************************************************************
	! Print State totals
	!******************************************************************
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEMP$ = "*** State " + AREA$ + " Totals"

	TEXT$ = SPACE$(11%) + &
		LEFT(TEMP$ + SPACE$(25%), 25%) + &
		SPACE$(17%) + &
		FORMAT$(TOTAL_TAXABLE_WAGE(1%), "#,###,###.## ") + &
		FORMAT$(TOTAL_TAXES(1%), "#,###,###.## ") + &
		FORMAT$(TOTAL_TAXES(2%), "#,###,###.## ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "           *** Total Employees: " + NUM1$(EMP_COUNT%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -2%)
	EMP_COUNT% = 0%

	TOTAL_TAXABLE_WAGE(I%) = 0.0 FOR I% = 1% TO 3%
	TOTAL_TAXES(I%) = 0.0 FOR I% = 1% TO 3%

	RETURN

 CityTotal:
	!******************************************************************
	! Print City totals
	!******************************************************************

 CountyTotal:
	!******************************************************************
	! Print County totals
	!******************************************************************

 DistrictTotal:
	!******************************************************************
	! Print District totals
	!******************************************************************

	TEMP$ = ""

	IF TEST_TTYPE$ = "CW"
	THEN
		TEMP$ = "*** City " + AREA$ + " Totals"
	END IF

	IF TEST_TTYPE$ = "DW"
	THEN
		TEMP$ = "*** County " + AREA$ + " Totals"
	END IF

	IF TEST_TTYPE$ = "EW"
	THEN
		TEMP$ = "*** District " + AREA$ + " Totals"
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = SPACE$(11%) + &
		LEFT(TEMP$ + SPACE$(25%), 25%) + &
		SPACE$(17%) + &
		FORMAT$(TOTAL_TAXABLE_WAGE(1%), "#,###,###.## ") + &
		FORMAT$(TOTAL_TAXES(1%), "#,###,###.## ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "           *** Total Employees: " + NUM1$(EMP_COUNT%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -2%)
	EMP_COUNT% = 0%

	TOTAL_TAXABLE_WAGE(I%) = 0.0 FOR I% = 1% TO 3%
	TOTAL_TAXES(I%) = 0.0 FOR I% = 1% TO 3%

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

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
