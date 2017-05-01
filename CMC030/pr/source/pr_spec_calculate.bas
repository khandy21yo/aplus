1	%TITLE "CALC - Calculate Taxes _& Std Ded"
	%SBTTL "PR_SPEC_CALCULATE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986, 1988, 1990 BY
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
	! Abstract:HELP
	!	.p
	!	This option calculates
	!	withholding taxes, standard payroll deductions,
	!	and accrual infromation.
	!	This information is then placed in
	!	the deduction journal.
	!	.p
	!	It will recalculate any employee who does
	!	not have a check number assigned.
	!	.p
	!	Employees must have at least one Pay record
	!	to have anything calculated.
	!	You can create a pay recourd with zero amounts
	!	to fulfil this requirement if necessary.
	!
	! Index:
	!	.x Calculate>Withholding Taxes
	!	.x Withholding Taxes>Calculation
	!	.x Taxes>Withholding Calculation
	!	.x Deductions>Standard
	!	.x Deductions>Payroll
	!
	! Option:
	!	PR_SPEC_CALCULATE$CONFIRM
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_SPEC_CALCULATE/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_SPEC_CALCULATE, -
	!	FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_SPEC_CALCULATE.OBJ;*
	!
	! Author:
	!
	!	11/27/87 - Robert Peterson
	!
	! Modification history:
	!
	!	01/09/89 - Kevin Handy
	!		Modified so that the last insurance rate is used
	!		instead of the first one (we want the one with
	!		the latest effective date).
	!
	!	03/04/89 - Frank F. Starman
	!		Calculate OST and SUI
	!
	!	03/21/89 - Kevin Handy
	!		Fixed figuring taxable/untaxable status of deductions,
	!		and how they are computed.
	!
	!	03/21/89 - Kevin Handy and Frank F. Starman
	!		More fixes in the computation of taxes.  Clean
	!		up processing through taxes.
	!
	!	03/27/89 - Kevin Handy
	!		Worked on calculation of FICA taxes. Added numerous
	!		comments. Worked on problem for people who get
	!		two checks in the same pay period.
	!
	!	03/27/89 - Kevin Handy
	!		Fixed bug where was using the wrong code for
	!		city withholding (CHW instead of CWH).
	!
	!	03/31/89 - Kevin Handy
	!		Re-structured tax calculation section to place
	!		all code into a select statement to make it easier
	!		to read.  Moved check for zero periods up to
	!		where employee record is read in.
	!		Used the full TAX_TYPE_TABLE$ code instead of a
	!		single letter from the code.
	!
	!	03/31/89 - Kevin Handy
	!		Handled problem with state calculation where
	!		a table with only one line and no over amount
	!		was not working at all.
	!
	!	04/03/89 - Kevin Handy
	!		Modified the way that WC is handled in the
	!		calculations, so that it is separate from
	!		OST.  Fixed to use PR_TRN_PAY::FACTOR in the
	!		calculation instead of FACTOR.
	!
	!	04/11/89 - Kevin Handy
	!		Fix problem that adding a new pay check into
	!		the file can cause a negative amount of FICA
	!		to be calculated because it was not keeping
	!		CUR_FICA_SUBJECT current, but it was keeping
	!		CUR_FICA_TAX current, and thus caused a large
	!		negative adjustment.
	!
	!	04/17/89 - Kevin Handy
	!		Fixed SU so that status code (State) is included
	!		in the record.
	!
	!	06/07/89 - Kevin Handy
	!		Fixed problem in workman comp calculation.
	!
	!	08/09/89 - B. Craig Larsen
	!		Changed the OST calculation to base the limit
	!		per pay period on the taxable wages rather than on
	!		the tax amount per period.
	!
	!	08/23/89 - Kevin Handy
	!		Modified to display employee number on help error
	!		screen so it is easier to find out which employee
	!		caused the error.
	!
	!	09/07/89 - Frank Starman
	!		Modified to check limit maximum hours in a quarter
	!		for workcomp. Also ask for effective quarter before
	!		doing any calculation.
	!
	!	01/03/89 - Kevin Handy
	!		Fix call to HelpError so that it has the correct
	!		number of parameters.
	!
	!	01/04/89 - Kevin Handy
	!		Modified the structure of some IF statements to
	!		make their intent clearer and easier to figure
	!		out their looping structure.
	!
	!	02/08/90 - Frank Starman
	!		Add line under FICA to subtract FICA from NET_CHECK
	!
	!	03/01/90 - Kevin Handy
	!		Modified to use one structure array instead of
	!		thousands of different arrays.
	!
	!	03/09/90 - Kevin Handy
	!		Added code to fill in the new TAXABLE field in
	!		PR_TRN_DED file.
	!
	!	03/16/90 - Kevin Handy
	!		Added code to fill in the new REPORTABLE field in
	!		PR_TRN_DED file.
	!
	!	05/14/90 - Kevin Handy
	!		Modified to add records to the deduction file for
	!		taxes if a taxable or reportable amount is
	!		given, even if the total tax withheld is zero.
	!
	!	05/17/90 - Frank F. Starman
	!		Don't delete any records from PAY file.
	!
	!		WRONG| WRONG| WRONG| WRONG| WRONG| WRONG|
	!		Please don't remove it again||||||||||||
	!		WRONG| WRONG| WRONG| WRONG| WRONG| WRONG|
	!
	!		STD ERNDED Types must be removed before
	!		adding them back in again.
	!		Added back in again 10/17/90, 01/04/92
	!
	!	05/29/90 - Kevin Handy
	!		Fixed problem in FACTOR calculation to distribute
	!		non-taxable amounts (made simpler and easier to
	!		understand, as well as work correctly).
	!
	!	06/04/90 - Kevin Handy
	!		Fixed yet another bug in the FACTOR calculation.
	!
	!	10/03/90 - Kevin Handy
	!		Fixed error trapping so an error on 310 would not
	!		jump over open on 315.
	!
	!	10/04/90 - Kevin Handy
	!		Added code to handle method 6 in PR_EMP_STD_ERNDED
	!
	!	10/17/90 - Kevin Handy
	!		Re-added delete from TRN file that Frank deleted
	!		on 05/17/90.  Reorganized it so it made only one
	!		pass through the file.
	!
	!	10/25/90 - Kevin Handy
	!		Added pay type "X" (eXcess).
	!
	!	12/13/90 - Kevin Handy
	!		Modification to add ADDEXEMPT fields to carry
	!		through files.
	!
	!	12/17/90 - Kevin Handy
	!		Modification in line 1070 in an attempt to gain a
	!		little bit more speed.  Also sorted the error
	!		trapping section to make it possible to find
	!		the error handlers for a specific line.
	!
	!	12/26/90 - Kevin Handy
	!		Modification of FICA calculation to split HI
	!		and OASDI parts apart.
	!
	!	12/27/90 - Kevin Handy
	!		Modification for handling additional exemptions.
	!
	!	12/27/90 - Kevin Handy
	!		Modification to handle federal threshold loss.
	!		(Also enabled for other tax types in case they
	!		decide to become screwball also)
	!
	!	12/27/90 - Kevin Handy
	!		Modification to handle credit for federal withholdings
	!		in the SW/CW/DW/EW taxes.
	!
	!	12/27/90 - Kevin Handy
	!		Modified to handle Low Income Credit (California).
	!		Also allowed for Federal (just in case).
	!
	!	01/08/91 - Kevin Handy
	!		Disabled the PR_WC_DEFINITION file entirely.
	!
	!	01/14/91 - Craig Tanner
	!		Added YYYY$ to some filename$ in error trapping.
	!
	!	01/23/91 - Kevin Handy
	!		Modified so that an employee status of "E" means
	!		that the tax is forced to zero, not his taxable
	!		and reportable.  This causes the taxable and reportable
	!		for these people to show up on reports.
	!
	!	02/06/91 - Kevin Handy
	!		Modification to round incoming MAXQHOURS, because
	!		it was coming in as 3.456E-77 on northwest center,
	!		which caused the calculation to fail.
	!
	!	02/22/91 - Kevin Handy
	!		Modification to get fica taxable ytd from
	!		PR_REG_TAXES instead of PR_REG_ERNDED.
	!
	!	03/26/91 - Kevin Handy
	!		Spell Checked Source Code.
	!
	!	04/23/91 - Kevin Handy
	!		Added method 7 to PR_EMP_STD_ERNDED file.
	!		Removed lines that were commented out.
	!
	!	05/06/91 - Kevin Handy
	!		Added a few page markers to source.
	!		Removed error trapping for 3400, which could
	!		not create any error since there was only a
	!		comment there.
	!
	!	05/06/91 - Kevin Handy
	!		Disabled defaulting the PR_EMP_STATUS record
	!		and the PR_TAX_TABLE record, because if the
	!		record was not found, then it did nothing
	!		with the data in the record.
	!
	!	05/06/91 - Kevin Handy
	!		Changed error trapping for 18210 to go to 18280
	!		instead of 18278, because the IF statements therin
	!		caused nothing to happen if an error occured there.
	!
	!	05/06/91 - Kevin Handy
	!		Modified so that exempt employees for FW,SW,...
	!		will still create a record for reportable amount.
	!
	!	07/14/91 - Kevin Handy
	!		Changed line 10882 to 18002 to get it back where
	!		it belongs.
	!
	!	10/02/91 - Kevin Handy
	!		Scanned through for bug in automatic pay not being
	!		replaced on a recalculate.  Improved comments in
	!		that section during scan.
	!
	!	10/10/91 - Kevin Handy
	!		Removed commented out code.
	!
	!	01/04/92 - Kevin Handy
	!		Modified to remove "A" Accrual records, and
	!		standard pay records "O" from pay file.
	!		(See 05/17/90 change that should have been
	!		made)
	!
	!	01/04/92 - Kevin Handy
	!		Moved check for CK file from 18000 (STD Calc)
	!		up higher so wouldn't have to do it so often.
	!		Modified error trap to fake a check record
	!		as a possible speedup for future searches.
	!
	!	01/04/92 - Kevin Handy
	!		Added code to calculate accruals "A" records.
	!		NOTE: Assumes it is accrued at an hour rate.
	!
	!	01/06/92 - Kevin Handy
	!		Moved all master file searches to FindMaster.
	!		Simplified handling of EFF_PERIOD$ by adding
	!		EFF_PERIOD% variable.
	!
	!	01/14/92 - Kevin Handy
	!		Modified "A" accrual account number calculation.
	!
	!	02/17/92 - Kevin Handy
	!		Fixed bug in looking up accrual account to put in
	!		"A" records.  Also modified to store DRCR account
	!		instead of ACCRUAL account in time record.
	!
	!	02/19/92 - Kevin Handy
	!		Added FUNC_ROUND to calculation of gross amount
	!		in accrual calculation.
	!
	!	05/13/92 - Kevin Handy
	!		Modified to guess the effective quarter, instead
	!		of defaulting to '1'.  Maybe this will reduce
	!		user error a little bit.
	!
	!	08/11/92 - Kevin Handy
	!		Added message when PR_EMP_STATUS tries to define an
	!		employee with a bad status code
	!
	!	10/13/92 - Kevin Handy
	!		Fixed calculation of SUBJECT_PREM to use
	!		(FACTOR-1) instead (1-FACTOR).
	!
	!	10/30/92 - Kevin Handy
	!		Fixed bug in 08/11/92 where it gave error message
	!		for exempt (E) employees.
	!
	!	04/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/25/93 - Kevin Handy
	!		Modified to fill out more fields in pay record
	!		when it has to create an empty one, so there will
	!		be fewer "undefined" errors.
	!
	!	08/31/93 - Kevin Handy
	!		Changed the default "effective period" from "0"
	!		to "1" to see if CMC's accrual will calculate more
	!		often.
	!
	!	08/31/93 - Kevin Handy
	!		Reworked code to calculate effective quarter.
	!		Now uses BATCH_NO$ instead of PR_TRN_PAY_DATE$ which
	!		doesn't alwayes get set up.
	!
	!	10/04/93 - Kevin Handy
	!		Changed source code from "=>" to ">=".
	!
	!	09/07/94 - Kevin Handy
	!		Modified so that exempt "E" types do not leave
	!		the reportable zero. It did fill in the taxable.
	!
	!	04/10/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico source code standards.
	!		Removed SUBJECT% parameter from PR_READ_SUBJTAX.
	!
	!	04/15/95 - Kevin Handy
	!		Fix last parameter to entr_3choice.
	!
	!	08/30/95 - Kevin Handy
	!		Trap null pay frequency error.
	!
	!	04/16/96 - Kevin Handy
	!		Reformat source code.
	!
	!	10/25/96 - Kevin Handy
	!		Remove commented out code
	!		Clean up (Check)
	!
	!	12/12/96 - Kevin Handy
	!		Fixed bug when calculating state tax as a
	!		percentage of federal. Wasn't setting taxable
	!		or reportable amounts.
	!
	!	12/16/96 - Kevin Handy
	!		Fix state=%of federal so taxable/reportable
	!		is not the same as the federal.
	!
	!	01/14/96 - Kevin Handy
	!		Reformat some lines to 80 columns.
	!		Redo a lot of the help messages.
	!
	!	01/14/96 - Kevin Handy
	!		If a status doesn't exist for an employee
	!		by state, grab the federal information so that
	!		something gets calculated, instead of just
	!		giving up.
	!
	!	03/17/97 - Kevin Handy
	!		Handle FH code/calculation.
	!		Lose EXEMPT_LOSS code.
	!
	!	05/05/97 - Kevin Handy
	!		Lose blank line 410.
	!
	!	05/12/97 - Kevin Handy
	!		Use one more digit for fica rates.
	!
	!	05/16/97 - Kevin Handy
	!		Fix federal tax calculation for other than
	!		first employee.
	!
	!	07/24/97 - Kevin Handy
	!		Allow old FICA rates to work until I get
	!		everyone converted over.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	01/16/98 - Kevin Handy
	!		Fix several bugs in handling of TAXABLE and
	!		REPORTABLE flags. (Was not calculating any
	!		REPORTABLE if TAXABLE was no)
	!
	!	05/27/98 - Kevin Handy
	!		This is the start of a lot of re-work to make
	!		the algorithm of calculation much more evident,
	!		with the intention of handling the new 'F'
	!		deduction types corrently. Changes include
	!		improved comments, and breaking the process into
	!		clearly idientified subroutines.
	!
	!	05/27/98 - Kevin Handy
	!		Moved check for existing check number from 1070
	!		to 1025, so it wouldn't do any expensive checks
	!		if the check number exists.
	!
	!	05/27/98 - Kevin Handy
	!		Lost error trapping for line 18050, which was an
	!		empty line. Also lost line 18050.
	!
	!	05/27/98 - Kevin Handy
	!		Resequenced lines 18000-18099 to 175?? to make
	!		room for more code.
	!
	!	05/27/98 - Kevin Handy
	!		Changed a weird INSTR to a '<>' in two places.
	!
	!	05/27/98 - Kevin Handy
	!		Modified so that it will create a pay record
	!		when it is NOT deleting records, and only
	!		bother with it on the first record seen.
	!
	!	05/28/98 - Kevin Handy
	!		Made 1000,2000,3000 into subroutines instead
	!		of being linked end-to-end routines. Makes it
	!		easier for me to add new sections to the
	!		calculation.
	!
	!	05/28/98 - Kevin Handy
	!		Made 3100,3030,3200,3300 into subroutines.
	!		Lost stupid END_OF_FILE flag.
	!
	!	05/28/98 - Kevin Handy
	!		Merged 3300 into the main loop, since it
	!		was just another set of GOSUB's anyway.
	!
	!	05/28/98 - Kevin Handy
	!		Added some code to handle new 'F' type of deduction.
	!
	!	06/03/98 - Kevin Handy
	!		Zero out NET_CHECK. (Big Bug after changes)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/04/99 - Kevin Handy
	!		Improve "Missing Tax Code" error
	!
	!	03/23/99 - Kevin Handy
	!		Always create 'F' deductions, even if they are 0.
	!
	!	04/21/99 - Kevin Handy
	!		Fix unsolicited unsolicited input
	!
	!	05/13/99 - Kevin Handy
	!		Adjust NET_CHECK for extra taxes being witheld
	!
	!	09/09/99 - Kevin Handy
	!		Handle limits on deductions that are type "4".
	!
	!	01/20/2000 - Kevin Handy
	!		Modify so that a FICA limit of 0 means unlimited.
	!
	!	03/22/2000 - Kevin Handy
	!		Added EFF_DATE parameter to PR_READ_RATE
	!
	!	05/17/2000 - Kevin Handy
	!		Handle rounding of state tax amounts using the new
	!		field PR_TAX_TABLE::DROUNDING
	!
	!	07/16/2001 - Kevin Handy
	!		Make accruals try to read a specific rate for that
	!		code first.  Still somewhast goofy in that the rate
	!		must be dated before the standard rate, else that
	!		will be used as the employees regular pay rate. (marco)
	!
	!	08/08/2003 - Kevin Handy
	!		Add bloody Arizona minimum tax witholding.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)		PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STATUS.HB"
	MAP	(PR_EMP_STATUS)		PR_EMP_STATUS_CDD	PR_EMP_STATUS

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STD_ERNDED.HB"
	MAP	(PR_EMP_STD_ERNDED)	PR_EMP_STD_ERNDED_CDD PR_EMP_STD_ERNDED

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP	(PR_TRN_PAY)		PR_TRN_PAY_CDD		PR_TRN_PAY

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.HB"
	MAP	(PR_TRN_DED)		PR_TRN_DED_CDD		PR_TRN_DED

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.HB"
	MAP	(PR_TRN_CHECK)		PR_TRN_CHECK_CDD	PR_TRN_CHECK

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

	%INCLUDE "SOURCE:[PR.OPEN]PR_WC_INSURANCE.HB"
	MAP	(PR_WC_INSURANCE)	PR_WC_INSURANCE_CDD	PR_WC_INSURANCE

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL.HB"
	MAP	(PR_EMP_ACCRUAL)	PR_EMP_ACCRUAL_CDD	PR_EMP_ACCRUAL

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL_RATE.HB"
	MAP (PR_EMP_ACCRUAL_RATE) PR_EMP_ACCRUAL_RATE_CDD PR_EMP_ACCRUAL_RATE
	MAP (PR_EMP_ACCRUAL_RATE_XXX) PR_EMP_ACCRUAL_RATE_CDD &
		PR_EMP_ACCRUAL_RATE_XXX

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	!
	! External functions
	!
	EXTERNAL LONG	OUTP_XUNSOL ! (It's really an AST routine)

	!
	! Dimension
	!
	RECORD EMPLOYEE_RECORD
		STRING CODE = 2%
		REAL   TAXABLE
		REAL   REPORTABLE
		REAL   HOURS
	END RECORD

	DIM EMPLOYEE_RECORD EMPLOYEE(10%, 10%)
	DECLARE EMPLOYEE_RECORD BLANK_EMPLOYEE

	DIM EMP_NT(10%), &
		EMP_NR(10%), &
		EMP_WH_CODE%(10%), &
		EMP_OST(10%), &
		INSURANCE(10%)

	DIM REVERSE_LIST$(200%), DATE_FILE$(200%), SHOW_TAX_PKG$(200%)

	DECLARE RFA PR_TRN_PAY_RFA

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initialize
	!*******************************************************************

	BLANK_EMPLOYEE::CODE		= ""
	BLANK_EMPLOYEE::TAXABLE		= 0.0
	BLANK_EMPLOYEE::REPORTABLE	= 0.0
	BLANK_EMPLOYEE::HOURS		= 0.0

	CALL READ_INITIALIZE

	!
	! Other Variables
	!
	SUBJECT_TYPE_TABLE$ = "FIE!FHE!FWH!SWH!OST!CWH!DWH!EWH!SUI!SWC!"

	TAX_TYPE_TABLE$ = "FI!FH!FW!SW!SX!CW!DW!EW!SU!SI!"

	!
	! List of effective periods
	!
	EPTITLE$ = "Code Description"

	EP$(0%) = "7"
	EP$(1%) = "0    Not Applicable"
	EP$(2%) = "1    First Payroll in Month"
	EP$(3%) = "2    Second Payroll in Month"
	EP$(4%) = "3    Third Payroll in Month"
	EP$(5%) = "4    Fourth Payroll in Month"
	EP$(6%) = "5    Fifth Payroll in Month"
	EP$(7%) = "S    Special Payroll"

	!
	! List of periods
	!
	EQTITLE$ = "Code Description"

	EQ$(0%) = "5"
	EQ$(1%) = "1    First Quarter"
	EQ$(2%) = "2    Second Quarter"
	EQ$(3%) = "3    Third Quarter"
	EQ$(4%) = "4    Fourth Quarter"

100	!******************************************************************
	! Get Year for file name
	!******************************************************************

	!
	! Look up device
	!
	CALL READ_DEVICE("PR_TRN_PAY", PR_TRN_PAY.DEV$, STAT%)

	CALL FIND_FILE(PR_TRN_PAY.DEV$ + "PR_TRN_PAY_*.JRL", DATE_FILE$(), &
		16%, "", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram
	END SELECT

	DATE_FILE% = VAL%(DATE_FILE$(0%))

	IF DATE_FILE%
	THEN
		REVERSE_LIST$(DATE_FILE% - LOOP% + 1%) = &
			MID(DATE_FILE$(LOOP%), 16%, 2%) + "/" + &
			MID(DATE_FILE$(LOOP%), 18%, 2%) + "/" + &
			MID(DATE_FILE$(LOOP%), 12%, 4%) &
				FOR LOOP% = DATE_FILE% TO 1% STEP -1%

		TEMP$ = "Payroll Folder Dates"

		X% = ENTR_3CHOICE(SCOPE, "", "", REVERSE_LIST$(), "", 0%, &
			TEMP$, "", 0%)

		IF X% > 0%
		THEN
			BATCH_NO$ = RIGHT(REVERSE_LIST$(X%), 7%) + &
				LEFT(REVERSE_LIST$(X%), 2%) + &
				MID(REVERSE_LIST$(X%), 4%, 2%)
			GOTO 190
		END IF
	END IF

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	!
	! Ask for year
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 80%, SMG_SCREEN_DATA%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 1%, 1%)

	BATCH_NO$ = DATE_TODAY

120	SCOPE::PRG_ITEM = "FLD01FDATE"

	!++
	! Abstract:FLD01FDATE
	!	^*Payroll Folder Date\*
	!	.b
	!	.lm +5
	!	The ^*Payroll Folder Date\* field
	!	selects an existing folder or to create a new one.
	!	.lm -5
	!
	! Index:
	!	.x Folder Date>Calculate
	!	.x Calculate>Folder Date
	!	.x Date>Calculate
	!
	!--

	PR_TRN_PAY_DATE$ = ENTR_3DATE(SCOPE, SMG_SCREEN_DATA%, "", &
		"Enter Payroll Folder Date (MMDDYYYY) ", &
		BATCH_NO$, 64%, "8", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 120
	END SELECT

	PR_TRN_PAY_DATE$ = EDIT$(PR_TRN_PAY_DATE$, -1%)

	IF LEN(EDIT$(PR_TRN_PAY_DATE$, -1%)) <> 8%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter the folder date in (MMDDYYYY) format", 0%)
		GOTO 120
	END IF

	BATCH_NO$ = PR_TRN_PAY_DATE$

190	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

	YYYY$ = LEFT(BATCH_NO$, 4%)

	%Page

300	!
	! Open Employee master file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Employee standard deduction file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STD_ERNDED.OPN"
	USE
		CONTINUE 315 IF ERR = 5%
		FILENAME$ = "PR_EMP_STD_ERNDED"
		CONTINUE HelpError
	END WHEN

315	!
	! Open Employee status file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STATUS.OPN"
	USE
		FILENAME$ = "PR_EMP_STATUS"
		CONTINUE HelpError
	END WHEN

320	!
	! Open Pay folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.UPD"
	USE
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

330	!
	! Open Deduction folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.CRE"
		CLOSE PR_TRN_DED.CH%
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.UPD"
	USE
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

340	!
	! Open Check folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.CRE"
		CLOSE PR_TRN_CHECK.CH%
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.UPD"
	USE
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

350	!
	! Open ERNDED Definition file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"
	USE
		CONTINUE 360 IF ERR = 5%
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

360	!
	! Open TaxWH register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.OPN"
	USE
		CONTINUE 370 IF ERR = 5%
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		CONTINUE HelpError
	END WHEN

370	!
	! Open ERNDED register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.OPN"
	USE
		CONTINUE 380 IF ERR = 5%
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

380	!
	! Open Tax Table file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.OPN"

		GET #PR_TAX_TABLE.CH%, KEY #0% EQ "F", REGARDLESS
	USE
		FICA_EMPE_PCT = 0.0
		FICA_LIMIT = 0.0
		FICA_EMPE_PCT_HI = 0.0
		FICA_LIMIT_HI = 0.0

		FILENAME$ = "PR_TAX_TABLE_" + YYYY$
		CONTINUE HelpError
	END WHEN

	FICA_EMPE_PCT = (PR_TAX_TABLE::FICA_EMPE_PCT) / 10000.0
	FICA_LIMIT = PR_TAX_TABLE::FICA_LIMIT
	FICA_EMPE_PCT_HI = (PR_TAX_TABLE::FICA_EMPE_PCT_HI) / 10000.0
	FICA_LIMIT_HI = PR_TAX_TABLE::FICA_LIMIT_HI

	IF FICA_EMPE_PCT > 0.10
	THEN
		FICA_EMPE_PCT = FICA_EMPE_PCT / 10.0
		FICA_EMPE_PCT_HI = FICA_EMPE_PCT_HI / 10.0
	END IF

390	!
	! Open Profile
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.OPN"
	USE
		CONTINUE 400 IF ERR = 5%
		FILENAME$ = "PR_TAX_PROFILE"
		CONTINUE HelpError
	END WHEN

400	!
	! Open Tax package
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.OPN"

		RESET #PR_TAX_PKG.CH%
	USE
		FILENAME$ = "PR_TAX_PKG"
		CONTINUE HelpError
	END WHEN

	TAX_PKG_LOOP% = 0%

	TEST_TAX_PKG$ = ""

405	WHEN ERROR IN
		GET #PR_TAX_PKG.CH%, REGARDLESS
	USE
		CONTINUE 420 IF ERR = 11%
		FILENAME$ = "PR_TAX_PKG"
		CONTINUE HelpError
	END WHEN

	TAX_PKG_LOOP% = TAX_PKG_LOOP% + 1%

	IF PR_TAX_PKG::TAX_PKG <> TEST_TAX_PKG$
	THEN
		SHOW_TAX_PKG$(TAX_PKG_LOOP%) = &
			PR_TAX_PKG::TAX_PKG
	END IF

	TEST_TAX_PKG$ = PR_TAX_PKG::TAX_PKG

	SELECT PR_TAX_PKG::STTYPE

	CASE "SW"
		SHOW_TAX_PKG$(TAX_PKG_LOOP%) = &
			LEFT(SHOW_TAX_PKG$(TAX_PKG_LOOP%) + &
			SPACE$(10%), 10%) + &
			"State Withholding"

	CASE "SU"
		SHOW_TAX_PKG$(TAX_PKG_LOOP%) = &
			LEFT(SHOW_TAX_PKG$(TAX_PKG_LOOP%) + &
			SPACE$(10%), 10%) + &
			"State Unemployment"

	CASE "SX"
		SHOW_TAX_PKG$(TAX_PKG_LOOP%) = &
			LEFT(SHOW_TAX_PKG$(TAX_PKG_LOOP%) + &
			SPACE$(10%), 10%) + &
			"Other State Tax"

	CASE "CW"
		SHOW_TAX_PKG$(TAX_PKG_LOOP%) = &
			LEFT(SHOW_TAX_PKG$(TAX_PKG_LOOP%) + &
			SPACE$(10%), 10%) + &
			"City Withholding"

	CASE "DW"
		SHOW_TAX_PKG$(TAX_PKG_LOOP%) = &
			LEFT(SHOW_TAX_PKG$(TAX_PKG_LOOP%) + &
			SPACE$(10%), 10%) + &
			"County Withholding"

	CASE "EW"
		SHOW_TAX_PKG$(TAX_PKG_LOOP%) = &
			LEFT(SHOW_TAX_PKG$(TAX_PKG_LOOP%) + &
			SPACE$(10%), 10%) + &
			"School Withholding"

	CASE "SI"
		SHOW_TAX_PKG$(TAX_PKG_LOOP%) = &
			LEFT(SHOW_TAX_PKG$(TAX_PKG_LOOP%) + &
			SPACE$(10%), 10%) + &
			"Work Comp Insurance"
	END SELECT

	SHOW_TAX_PKG$(TAX_PKG_LOOP%) = &
		LEFT(SHOW_TAX_PKG$(TAX_PKG_LOOP%) + SPACE$(32%), 32%) + &
		"Code " + PR_TAX_PKG::CODE

	GOTO 405

420	!
	! Open WC Insurance
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_WC_INSURANCE.OPN"
	USE
		CONTINUE 430 IF ERR = 5%
		FILENAME$ = "PR_WC_INSURANCE"
		CONTINUE HelpError
	END WHEN

430	!
	! Open Accrual definition file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL.OPN"
	USE
		PR_EMP_ACCRUAL.CH% = 0%
		CONTINUE 500 IF ERR = 5%		! SKIPS 440
		FILENAME$ = "PR_EMP_ACCRUAL"
		CONTINUE HelpError
	END WHEN

440	!
	! Open Accrual definition file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL_RATE.OPN"
	USE
		PR_EMP_ACCRUAL.CH% = 0%
		CONTINUE 500 IF ERR = 5%
		FILENAME$ = "PR_EMP_ACCRUAL_RATE"
		CONTINUE HelpError
	END WHEN

	%PAGE

500	!*********************************************************************
	! Test to see if payroll has been closed or posted
	!	1 - Updated to Register
	!	2 - Accrued Post
	!	4 - Final Post
	!*********************************************************************

	WHEN ERROR IN
		RESET #PR_TRN_PAY.CH%

		GET #PR_TRN_PAY.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	!
	! Set up default payroll end date
	!
	TEMP_PR_END_DATE$ = PR_TRN_PAY::PR_END_DATE

	IF (PR_TRN_PAY::UPDATE_FLAG AND 1%)
	THEN
		CALL HELP_3MESSAGE(SCOPE, "PR Folder is Closed", &
			"ERR", "PR_CLOSED", &
			"ERROR_PR_CLOSED")
		GOTO ExitProgram
	END IF

	IF (PR_TRN_PAY::UPDATE_FLAG AND 4%)
	THEN
		CALL HELP_3MESSAGE(SCOPE, "PR Folder has been Posted", &
			"ERR", "PR_POSTED", &
			"ERROR_PR_POSTED")
		GOTO ExitProgram
	END IF

510	!
	! Paint the background
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		78%, &
		SMG_SCREEN_DATA%, &
		SMG$M_BORDER &
	)

	SMG_STATUS% = SMG$LABEL_BORDER &
	( &
		SMG_SCREEN_DATA%, &
		"Payroll Calculate " + TRM$(SCOPE::PRG_COMPANY), &
		SMG$K_TOP &
	)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Payroll Date", 4%, 5%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		PRNT_DATE(BATCH_NO$, 8%), &
		4%, 35%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Effective Period", 6%, 5%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Effective Quarter", 7%, 5%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Payroll end Date for Std P/D  ", 8%, 5%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Employee # ", 10%, 5%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)

	!----------------------------------------------------------------
	! Show tax package
	!----------------------------------------------------------------

	TEMP$ = "SHOW TAX PACKAGE CODES"

	X% = ENTR_3CHOICE(SCOPE, "", "", SHOW_TAX_PKG$(), "", 0%, &
		TEMP$, "", 0%)

	EFF_PERIOD$ = "1"
	EFF_QUARTER% = (VAL%(MID(BATCH_NO$, 5%, 2%)) + 2%) / 3%
	EFF_QUARTER$ = NUM1$(EFF_QUARTER%)

520	SCOPE::PRG_ITEM = "FLD02EPER"
	!++
	!
	! Abstract:FLD02EPER
	!	^*Effective Period\*
	!	.b
	!	.lm +5
	!	This field
	!	refers to the specific payroll period within a
	!	month, i.e. ^*1\* (First week), ^*2\* (Second Week),
	!	^*3\* (Third Week), ^*4\* (Fourth Week),
	!	or ^*5\* (Fifth Week). A special payroll,
	!	such as a special bonus payroll,
	!	is to be designated as ^*S\*.
	!	.b
	!	Standard payroll deductions can be
	!	effective on selected periods.
	!	The value in the Effective Period
	!	field determines which standard
	!	deductions will be activated
	!	in a specific payroll period.
	!	.lm -5
	!
	! Index:
	!	.x Period>Calculate
	!	.x Effective Period>Calculate
	!	.x Calculate>Effective Period
	!
	!--

	EFF_PERIOD$ = EDIT$(ENTR_3STRINGLIST(SCOPE, SMG_SCREEN_DATA%, "6;35", &
		"Effective Period for deductions", EFF_PERIOD$, 8%, "'", &
		"", EP$(), EPTITLE$, "005"), -1%)

	EFF_PERIOD% = INSTR(1%, "12345S", EFF_PERIOD$)

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE SMG$K_TRM_UP
		GOTO 520

	CASE SMG$K_TRM_DOWN
		GOTO 525

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 520
	END SELECT


525	SCOPE::PRG_ITEM = "FLD03EQTR"
	!+-+-+
	!++
	! Abstract:FLD03EQTR
	!	.ts 55
	!	^*Effective Quarter	1,2,3,4\*
	!	.b
	!	.lm +5
	!	This field determines which payroll quarter
	!	the calculations will be made.
	!	.lm -5
	!
	! Index:
	!	.x Effective Quarter>Calculate
	!	.x Calculate>Effective Quarter
	!
	!--

	EFF_QUARTER$ = EDIT$(ENTR_3STRINGLIST(SCOPE, SMG_SCREEN_DATA%, "7;35", &
		"Effective Quarter for limits", EFF_QUARTER$, 8%, "'", &
		"", EQ$(), EQTITLE$, "005"), -1%)

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE SMG$K_TRM_UP
		GOTO 520

	CASE SMG$K_TRM_DOWN
		GOTO 530

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 525
	END SELECT

	QUARTER% = VAL%(EFF_QUARTER$) - 1%

530	SCOPE::PRG_ITEM = "FLD04ENDDATE"
	!++
	!
	! Abstract:FLD04ENDDATE
	!	^*Payroll End Date for Standard P/D\*
	!	.b
	!	.lm +5
	!	This field refers to the ^*Payroll End Date for Standard
	!	Payments and Deductions\*, and the value
	!	entered in this field should usually be
	!	equal to the "general" payroll end date
	!	entered for a specific payroll folder.
	!	(Ordinarily there will be only one payroll
	!	end date associated with a specific
	!	payroll folder, but there could be more than one.)
	!	.b
	!	This field controls which standard payments
	!	and deductions which will be collected
	!	from the Employee Master Deductions
	!	file. If there were to be more than one
	!	check written for a specific employee
	!	in the same payroll period,
	!	it is likely that any standard payments and
	!	deductions would be applicable to only the
	!	"general" check and not the "special" check.
	!	The "general" check would be associated
	!	with the "general"payroll
	!	end date. The "special" check would
	!	be associated with some other payroll end date.
	!	During the execution of the Calculate option,
	!	standard payments
	!	and deductions are collected for records
	!	associated with the "general"
	!	payroll end date only.
	!	.lm -5
	!
	! Index:
	!	.x Payroll End Date>Calculate
	!	.x Calculate>Payroll End Date
	!
	!--

	PR_END_DATE$ = ENTR_3DATE(SCOPE, SMG_SCREEN_DATA%, "8;35", &
		"Enter Payroll End for Std E/D (MMDDYYYY) ", &
		TEMP_PR_END_DATE$, 8%, "8", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE SMG$K_TRM_UP
		GOTO 525

	CASE SMG$K_TRM_DOWN
		GOTO 530

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 530
	END SELECT

	PR_END_DATE$ = EDIT$(PR_END_DATE$, -1%)

	IF LEN(EDIT$(PR_END_DATE$, -1%)) <> 8%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter the Payroll End Date in (MMDDYYYY) format", 0%)
		GOTO 530
	END IF

	!++
	! Abstract:CONFIRM
	!	^*Confirm\*
	!	.b
	!	.lm +5
	!	This entry confirms that everything is correct
	!	for the calculation.
	!	.b
	!	The entry must be "^*Y\*es" for the calculation to occur.
	!	.lm -5
	!
	! Index:
	!	.x Confirm>Calculate
	!	.x Calculate>Confirm
	!
	!--
	SCOPE::PRG_ITEM = "CONFIRM"
	INP$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", &
		"Confirm Calculate process  - then press <Do> ", &
		"N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	IF INP$ <> "Y"
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	END IF

	!
	! Set help message
	!
	SCOPE::PRG_ITEM = "HELP"

	%PAGE

	!******************************************************************
	! Do all of the necessary steps in calculating this payroll
	!******************************************************************

	GOSUB 1000		! Remove deductions and taxes (everyone)
	GOSUB 2000		! Remove standard pay (everyone)
	GOSUB 3000		! Calculate taxes (everyone)

	GOTO ExitProgram	! Done

1000	!******************************************************************
	! Removing calculated taxes/ded
	!******************************************************************
	CALL ENTR_3MESSAGE(SCOPE, "Removing calculated Taxes/Ded", 1%)

	WHEN ERROR IN
		RESET #PR_TRN_DED.CH%
	USE
		CONTINUE 1099
	END WHEN

	!
	! Set up to trap interrupt
	!
	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		LOC(OUTP_XUNSOL) BY VALUE, &
		LOC(SCOPE::SMG_KBID) BY VALUE)

	RRR_FLAG% = 0%

1020	!*******************************************************************
	! Loop through deduction file, searching for previous calculated
	! statdard deductions, and missing PAY records.
	!*******************************************************************

	!
	! Check for an interrupt
	!
	GOSUB Interupt IF RRR_FLAG%

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_TRN_DED.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 1099 IF ERR = 11%
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, PR_TRN_DED::EMPNUM, &
		10%, 35%, , SMG$M_BOLD)

	REMOVE_FLAG% = 0%

1025	!
	! Is this a check that has already been written
	!
	IF (PR_TRN_DED::EMPNUM <> PR_TRN_CHECK::EMPNUM) OR &
		(PR_TRN_DED::PR_END_DATE <> PR_TRN_CHECK::PR_END_DATE)
	THEN
		WHEN ERROR IN
			GET #PR_TRN_CHECK.CH%, KEY #0% EQ PR_TRN_DED::EMPNUM + &
				PR_TRN_DED::PR_END_DATE, REGARDLESS
		USE
			!
			! Speed up likely future checks
			!
			PR_TRN_CHECK::EMPNUM = PR_TRN_DED::EMPNUM
			PR_TRN_CHECK::PR_END_DATE = PR_TRN_DED::PR_END_DATE
			PR_TRN_CHECK::CHECK = ""

			CONTINUE 1027 IF ERR = 155%
			FILENAME$ = "PR_TRN_CHECK"
			CONTINUE HelpError
		END WHEN
	END IF

	IF EDIT$(PR_TRN_CHECK::CHECK, -1%) <> ""
	THEN
		GOTO 1020
	END IF

1027	SELECT PR_TRN_DED::DTYPE

	CASE "D", "X"
		!
		! Is this item in the std ernded file
		!
1030		WHEN ERROR IN
			FIND #PR_EMP_STD_ERNDED.CH%, &
				KEY #0% EQ PR_TRN_DED::EMPNUM + &
				PR_TRN_DED::DTYPE + PR_TRN_DED::CODE, &
				REGARDLESS
		USE
			CONTINUE 1080 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "PR_EMP_STD_ERNDED"
			CONTINUE HelpError
		END WHEN

		REMOVE_FLAG% = -1%

	CASE "F"
		!
		! Is this item a "final deduction". These must be calculated.
		!
		REMOVE_FLAG% = -1%

	CASE "C"
		!
		! Is this item a tax record
		!
		REMOVE_FLAG% = -1% IF INSTR(1%, TAX_TYPE_TABLE$, &
			PR_TRN_DED::CODE)

	END SELECT

1080	!
	! Remove record from deduction file
	!
	IF REMOVE_FLAG%
	THEN
		WHEN ERROR IN
			DELETE #PR_TRN_DED.CH%
		USE
			FILENAME$ = "PR_TRN_DED"
			CONTINUE HelpError
		END WHEN

	END IF

1085	!
	! Is there a record in the pay file for this
	!
	IF PR_TRN_DED::EMPNUM <> PR_TRN_PAY::EMPNUM OR &
		PR_TRN_DED::PR_END_DATE <> PR_TRN_PAY::PR_END_DATE
	THEN
		WHEN ERROR IN
			GET #PR_TRN_PAY.CH%, &
				KEY #0% EQ PR_TRN_DED::EMPNUM + &
				PR_TRN_DED::PR_END_DATE, &
				REGARDLESS
		USE
			CONTINUE 1087 IF ERR = 155%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN
	END IF

	GOTO 1095

1087	TEST_EMPNUM$ = PR_TRN_DED::EMPNUM
	GOSUB FindMaster

1090	!
	! Check to see if an employee has a zero in the
	! pay frequency field.
	!
	IF PR_EMP_MASTER::PAYFREQ = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, PR_EMP_MASTER::EMPNUM + &
			" has a zero in the pay frequency", 0%)
		PR_EMP_MASTER::PAYFREQ = 1%
	END IF

	!
	! Add dummy pay record to file
	!
	PR_TRN_PAY::EMPNUM	= PR_TRN_DED::EMPNUM
	PR_TRN_PAY::PR_END_DATE	= PR_TRN_DED::PR_END_DATE
	PR_TRN_PAY::EMP_SKILL	= PR_EMP_MASTER::EMP_SKILL
	PR_TRN_PAY::EMP_GRADE	= PR_EMP_MASTER::EMP_GRADE
	PR_TRN_PAY::ACCT	= PR_EMP_MASTER::ACCT
	PR_TRN_PAY::SUBACC	= PR_EMP_MASTER::SUBACC
	PR_TRN_PAY::OPER	= PR_EMP_MASTER::OPER
	PR_TRN_PAY::LOCATION	= PR_EMP_MASTER::LOCATION
	PR_TRN_PAY::DEPT	= PR_EMP_MASTER::DEPT
	PR_TRN_PAY::WORK_CENTER	= PR_EMP_MASTER::WORK_CENTER
	PR_TRN_PAY::UNION	= PR_EMP_MASTER::UNION
	PR_TRN_PAY::PTYPE	= "P"
	PR_TRN_PAY::RTYPE	= PR_EMP_MASTER::RATE_TYPE
	PR_TRN_PAY::CODE	= PR_EMP_MASTER::RATE_CDE
	PR_TRN_PAY::HOUR_RATE	= 0.0
	PR_TRN_PAY::REG_HR	= 0.0
	PR_TRN_PAY::OVT_HR	= 0.0
	PR_TRN_PAY::PIECE	= 0.0
	PR_TRN_PAY::FACTOR	= 0%
	PR_TRN_PAY::GROSS	= 0.0
	PR_TRN_PAY::TAX_PKG	= PR_EMP_MASTER::TAX_PKG
	PR_TRN_PAY::UPDATE_FLAG	= 0%
	PR_TRN_PAY::BATCH_ENTRY	= ""
	PR_TRN_PAY::SEQNUM	= ""
	PR_TRN_PAY::BATCH	= ""

	WHEN ERROR IN
		PUT #PR_TRN_PAY.CH%
	USE
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

1095	GOTO 1020

1099	RETURN

	%PAGE

2000	!*****************************************************************
	! Add standard deductions
	!*****************************************************************

	CALL ENTR_3MESSAGE(SCOPE, "Adding Standard Pay/Ded", 1%)

	WHEN ERROR IN
		RESET #PR_TRN_PAY.CH%
	USE
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	TEST_EMPNUM$ = ""

2020	!
	! Check for an interrupt
	!
	GOSUB Interupt IF RRR_FLAG%

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_TRN_PAY.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 2090 IF ERR = 11%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	!
	! Ignore records we can't do anything about
	!
	GOSUB TestPayDeletable
	GOTO 2020 IF REMOVE_FLAG% = 0%

	!
	! Delete this record if it is an accrual record, so we can
	! recalculate the accruals.
	!
	IF PR_TRN_PAY::PTYPE = "A"
	THEN
		GOSUB DeletePay
		GOTO 2020
	END IF

	IF PR_TRN_PAY::PR_END_DATE = PR_END_DATE$
	THEN
		!
		! Check to see if this is a STDDED record.
		!
		IF PR_TRN_PAY::PTYPE = "O"
		THEN
			GOSUB TestPayStd
			IF REMOVE_FLAG%
			THEN
				GOSUB DeletePay
				GOTO 2020
			END IF
		END IF

		IF TEST_EMPNUM$ <> PR_TRN_PAY::EMPNUM
		THEN
			PR_TRN_PAY_RFA = GETRFA(PR_TRN_PAY.CH%)
			SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
				PR_TRN_PAY::EMPNUM, &
				10%, 35%, , SMG$M_BOLD)

			IF TEST_EMPNUM$ <> ""
			THEN
				GOSUB AddAccrual
				GOSUB AddSTDDed
				GET #PR_TRN_PAY.CH%, RFA PR_TRN_PAY_RFA
			END IF
		END IF

		WORK_HOUR = WORK_HOUR + PR_TRN_PAY::REG_HR + PR_TRN_PAY::OVT_HR
		WORK_OVT = WORK_OVT + PR_TRN_PAY::OVT_HR
		WORK_PREM = WORK_PREM + &
			PR_TRN_PAY::OVT_HR * PR_TRN_PAY::FACTOR / 100
		WORK_GROSS = WORK_GROSS + PR_TRN_PAY::GROSS

		TEST_EMPNUM$ = PR_TRN_PAY::EMPNUM
	END IF

	GOTO 2020

2090	IF TEST_EMPNUM$ <> ""
	THEN
		GOSUB AddAccrual
		GOSUB AddSTDDed
	END IF

	RETURN

	%PAGE

3000	!****************************************************************
	! Now calculate taxes
	!****************************************************************

	CALL ENTR_3MESSAGE(SCOPE, "Calculating Taxes", 1%)

	WHEN ERROR IN
		RESET #PR_TRN_PAY.CH%
	USE
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	!
	! Zero out everything
	!
	TEST_EMPNUM$ = SPACE$(LEN(PR_TRN_PAY::EMPNUM))
	TEST_PR_END_DATE$ = SPACE$(8%)

3005	!
	! Look for the next check to work on
	!
	WHEN ERROR IN
		GET #PR_TRN_PAY.CH%, &
			KEY #0% GT TEST_EMPNUM$ + TEST_PR_END_DATE$, &
			REGARDLESS
	USE
		CONTINUE 3009
	END WHEN

	TEST_EMPNUM$ = PR_TRN_PAY::EMPNUM
	TEST_PR_END_DATE$ = PR_TRN_PAY::PR_END_DATE

	!
	! Zero out all totals
	!
	FOR I% = 0% TO 10%
		EMP_NT(I%) = 0.0
		EMP_NR(I%) = 0.0
		EMP_WH_CODE%(I%) = 0%
		EMP_OST(I%) = 0.0

		FOR J% = 0% TO 10%
			EMPLOYEE(I%, J%) = BLANK_EMPLOYEE
		NEXT J%
	NEXT I%

	CUR_FICA_TAX, CUR_FICA_SUBJECT = 0.0
	CUR_HI_TAX, CUR_HI_SUBJECT = 0.0
	CUR_OST_SUBJECT = 0.0
	CUR_SUI_SUBJECT = 0.0
	CUR_SWC_SUBJECT = 0.0
	WORK_HOUR, WORK_GROSS, WORK_OVT, WORK_PREM = 0.0
	NET_CHECK = 0.0

	MAXQHOURS = 0.0

	!
	! Set Federal EMP_WH_CODE%() for 1
	!
	EMP_WH_CODE%(3%) = 1%

	!
	! Announce the employee being calculated
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		PR_TRN_PAY::EMPNUM, &
		10%, 35%, , SMG$M_BOLD)

	!
	! Look up the check record. We can't skip out here because
	! we have to track pay information in previous checks for
	! an employee to correctly handle the FICA limits.
	!
	GOSUB 3100

	!
	! Do the preliminary work for the tax calculation
	!
	GOSUB FindMaster	! Get employee information
	GOSUB 3020		! Look up pay records
	GOSUB 3200		! Look up deduction records

	!
	! Do the tax calculations (if a check doesn't exist)
	!
	IF CHECK_WRITTEN% = 0%
	THEN
		GOSUB TaxCal
		GOSUB AddFDeductions
	ELSE
		!
		! Handle increasing FICA Subject wages here, since
		! it isn't being handled in the tax calculation section.
		!
		CUR_FICA_SUBJECT = CUR_FICA_SUBJECT + &
			EMPLOYEE(1%, 1%)::TAXABLE - EMP_NT(1%)

		CUR_HI_SUBJECT = CUR_HI_SUBJECT + &
			EMPLOYEE(1%, 1%)::TAXABLE - EMP_NT(1%)
	END IF

	!
	! Do next check
	!
	GOTO 3005

3009	!
	! Calculation is finished
	!
	RETURN


3020	!******************************************************************
	! Scan for all pay records
	!******************************************************************

	!
	! Check for an interrupt
	!
	GOSUB Interupt IF RRR_FLAG%

3030	!******************************************************************
	! Do all the work for one pay record
	!******************************************************************
	!
	! Summarise this employees work history so that the
	! deductions can be calculated on that basis.
	!
	WORK_HOUR = WORK_HOUR + PR_TRN_PAY::REG_HR + PR_TRN_PAY::OVT_HR
	WORK_OVT = WORK_OVT + PR_TRN_PAY::OVT_HR
	WORK_PREM = WORK_PREM + &
		PR_TRN_PAY::OVT_HR * PR_TRN_PAY::FACTOR / 100
	WORK_GROSS = WORK_GROSS + PR_TRN_PAY::GROSS

	!
	! Pull up all the 'tax package' information
	!
	PKG_WH_CODE$(I%) = "" FOR I% = 3% TO 8%

	WHEN ERROR IN
		FIND #PR_TAX_PKG.CH%, &
			KEY #0% EQ PR_TRN_PAY::TAX_PKG, &
			REGARDLESS
	USE
		CONTINUE 3035 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_TAX_PKG"
		CONTINUE HelpError
	END WHEN

	GOTO 3040

3035	!
	! Tax package code is missing
	!
	CALL HELP_3MESSAGE(SCOPE, &
		"PR Missing Tax Code " + PR_TRN_PAY::TAX_PKG, &
		"ERR", "PR_SPEC_CALCULATE", &
		"PR_TAX_PKG")

	CALL ENTR_3MESSAGE(SCOPE, "Employee # " + TRM$(TEST_EMPNUM$) + &
		" is missing a Tax Code", 0%)

	GOTO ExitProgram

3040	!
	! Get Tax Package code
	!
	WHEN ERROR IN
		GET #PR_TAX_PKG.CH%, REGARDLESS
	USE
		CONTINUE 3050 IF ERR = 11%
		FILENAME$ = "PR_TAX_PKG"
		CONTINUE HelpError
	END WHEN

	IF PR_TRN_PAY::TAX_PKG = PR_TAX_PKG::TAX_PKG
	THEN
		TAX_TYPE% = (INSTR(1%, TAX_TYPE_TABLE$, &
			PR_TAX_PKG::STTYPE) + 2%) /3%

		PKG_WH_CODE$(TAX_TYPE%) = PR_TAX_PKG::CODE IF TAX_TYPE% > 3%

		GOTO 3040
	END IF

3050	!
	! Add this pay record to whatever total lines it belongs to
	!
	NET_CHECK = FUNC_ROUND(NET_CHECK + PR_TRN_PAY::GROSS, 2%)

	FOR TAX_TYPE% = 1% TO LEN(SUBJECT_TYPE_TABLE$) / 4%
		!
		! See if taxable
		!
		SUBJECT_CODE$ = MID(SUBJECT_TYPE_TABLE$, &
			TAX_TYPE% * 4% - 3%, 3%)

		CALL PR_READ_SUBJTAX(PR_ERNDED_DEF.CH%, &
			SUBJECT_CODE$, &
			"P", &
			PR_TRN_PAY::CODE, &
			TAXABLE%, &
			REPORTABLE%)

		!
		! Jump over accumulation of taxable wages if not taxable
		!
		GOTO 3090 IF TAXABLE% AND REPORTABLE%

		WH_LOOP% = 1%

		IF TAX_TYPE% > 3%
		THEN
			GOTO 3060 &
				IF PKG_WH_CODE$(TAX_TYPE%) = &
				EMPLOYEE(TAX_TYPE%, WH_LOOP%)::CODE &
				FOR WH_LOOP% = 1% TO EMP_WH_CODE%(TAX_TYPE%)

			EMP_WH_CODE%(TAX_TYPE%), WH_LOOP% = &
				EMP_WH_CODE%(TAX_TYPE%) + 1%
			EMPLOYEE(TAX_TYPE%, WH_LOOP%)::CODE = &
				PKG_WH_CODE$(TAX_TYPE%)
		END IF

3060		IF TAX_TYPE% < 4% OR PKG_WH_CODE$(TAX_TYPE%) <> ""
		THEN
			EMPLOYEE(TAX_TYPE%, WH_LOOP%)::TAXABLE = &
				FUNC_ROUND(EMPLOYEE(TAX_TYPE%, &
				WH_LOOP%)::TAXABLE + &
				PR_TRN_PAY::GROSS, 2%) &
				IF TAXABLE% = 0%
			EMPLOYEE(TAX_TYPE%, WH_LOOP%)::REPORTABLE = &
				FUNC_ROUND(EMPLOYEE(TAX_TYPE%, &
				WH_LOOP%)::REPORTABLE + &
				PR_TRN_PAY::GROSS, 2%) &
				IF REPORTABLE% = 0%
			EMPLOYEE(TAX_TYPE%, WH_LOOP%)::HOURS = &
				FUNC_ROUND(EMPLOYEE(TAX_TYPE%, &
				WH_LOOP%)::HOURS+ &
				PR_TRN_PAY::REG_HR + PR_TRN_PAY::OVT_HR, 2%) &
				IF TAXABLE% = 0%
		END IF

		EMPLOYEE(TAX_TYPE%, 0%)::TAXABLE = &
			FUNC_ROUND(EMPLOYEE(TAX_TYPE%, 0%)::TAXABLE + &
			PR_TRN_PAY::GROSS, 2%) &
			IF TAXABLE% = 0%
		EMPLOYEE(TAX_TYPE%, 0%)::REPORTABLE = &
			FUNC_ROUND(EMPLOYEE(TAX_TYPE%, 0%)::REPORTABLE + &
			PR_TRN_PAY::GROSS, 2%) &
			IF REPORTABLE% = 0%
		EMPLOYEE(TAX_TYPE%, 0%)::HOURS = &
			FUNC_ROUND(EMPLOYEE(TAX_TYPE%, 0%)::HOURS + &
			PR_TRN_PAY::REG_HR + PR_TRN_PAY::OVT_HR, 2%) &
			IF TAXABLE% = 0%

		!
		! Jump over OST calculation if code is null
		!
		GOTO 3090 &
			IF TAX_TYPE% <> 10% OR PKG_WH_CODE$(TAX_TYPE%) = "" &
			OR TAXABLE%

3070		!
		! Look up Workmans Comp Insurance records
		!
		WHEN ERROR IN
			FIND #PR_WC_INSURANCE.CH%, &
				KEY #0% EQ PR_EMP_MASTER::WC + &
				PKG_WH_CODE$(TAX_TYPE%), &
				REGARDLESS
		USE
			CONTINUE 3090 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "PR_WC_INSURANCE"
			CONTINUE HelpError
		END WHEN

		INS_LOOP% = 0%
		INSURANCE(I%) = 0.0 FOR I% = 0% TO 10%
		TEST_INS_TYPE$ = ""
		SUBJECT_PREM = 0.0

		IF PR_TRN_PAY::RTYPE = "H" OR PR_TRN_PAY::RTYPE = "S" OR &
			PR_TRN_PAY::RTYPE = "X"
		THEN
			SUBJECT_PREM = FUNC_ROUND(PR_TRN_PAY::OVT_HR * ( &
				((PR_TRN_PAY::FACTOR / 100.0) - 1.0) * &
				PR_TRN_PAY::HOUR_RATE), 2%)
			SUBJECT_PREM = 0.0 IF SUBJECT_PREM < 0.0
		END IF

 GetNextIns:
3080		WHEN ERROR IN
			GET #PR_WC_INSURANCE.CH%, REGARDLESS
		USE
			CONTINUE 3085 IF ERR = 11%
			FILENAME$ = "PR_WC_INSURANCE"
			CONTINUE HelpError
		END WHEN

		IF PR_WC_INSURANCE::CODE + PR_WC_INSURANCE::STATE = &
			PR_EMP_MASTER::WC + PKG_WH_CODE$(TAX_TYPE%)
		THEN
			IF PR_WC_INSURANCE::EFFDAT <= BATCH_NO$
			THEN
				!
				! Note: This compute section for WC insurance
				! changed so that it would take the LAST insurance
				! rate defined, instead of the FIRST one.
				!
				INS_LOOP% = INS_LOOP% + 1% &
					IF TEST_INS_TYPE$ <> &
					PR_WC_INSURANCE::INS_TYPE &
					OR INS_LOOP% = 0%

				TEST_INS_TYPE$ = PR_WC_INSURANCE::INS_TYPE + ""
				MAXQHOURS = FUNC_ROUND(PR_WC_INSURANCE::MAXQHOURS, 2%)

				SELECT PR_WC_INSURANCE::METHOD

				CASE "1"
					INSURANCE(INS_LOOP%) = &
						(PR_TRN_PAY::GROSS - &
						SUBJECT_PREM) * &
						PR_WC_INSURANCE::EMPLE_RATE

				CASE "2"
					INSURANCE(INS_LOOP%) = &
						PR_TRN_PAY::GROSS * &
						PR_WC_INSURANCE::EMPLE_RATE

				CASE "3"
					INSURANCE(INS_LOOP%) = &
						(PR_TRN_PAY::REG_HR + &
						PR_TRN_PAY::OVT_HR) * &
						PR_WC_INSURANCE::EMPLE_RATE

				CASE "4"
					INSURANCE(INS_LOOP%) = &
						PR_TRN_PAY::REG_HR * &
						PR_WC_INSURANCE::EMPLE_RATE

				CASE "5"
					INSURANCE(INS_LOOP%) = &
						PR_TRN_PAY::REG_HR * &
						PR_WC_INSURANCE::EMPLE_RATE / 8.0

				END SELECT
			END IF

			GOTO GetNextIns

		END IF

3085		TEMP = 0.0
		TEMP = TEMP + INSURANCE(LOOP%) FOR LOOP% = 1% TO INS_LOOP%

		EMP_OST(WH_LOOP%) = FUNC_ROUND(EMP_OST(WH_LOOP%) + TEMP, 2%)

3090	NEXT TAX_TYPE%

3093	WHEN ERROR IN
		GET #PR_TRN_PAY.CH%, REGARDLESS
	USE
		IF ERR = 11%
		THEN
			CONTINUE 3095
		END IF
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	IF TEST_EMPNUM$ = PR_TRN_PAY::EMPNUM AND &
		TEST_PR_END_DATE$ = PR_TRN_PAY::PR_END_DATE
	THEN
		!
		! Skip if accrual
		!
		GOTO 3093 IF PR_TRN_PAY::PTYPE = "A"

		!
		! This is another one for this check
		!
		GOTO 3030
	END IF

3095	!
	! Done with pay records
	!
	RETURN


3100	!******************************************************************
	! Scan the check file
	!******************************************************************
	!
	! Is this a check that has already been written
	!
	CHECK_WRITTEN% = 0%

	WHEN ERROR IN
		GET #PR_TRN_CHECK.CH%, &
			KEY #0% EQ TEST_EMPNUM$ + TEST_PR_END_DATE$, &
			REGARDLESS
	USE
		CONTINUE 3150 IF ERR = 155%
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

	IF EDIT$(PR_TRN_CHECK::CHECK, -1%) <> ""
	THEN
		CHECK_WRITTEN% = -1%
	END IF

	GOTO 3190

3150	!
	! Add check record is it doesn't exist
	!
	PR_TRN_CHECK::EMPNUM		= TEST_EMPNUM$
	PR_TRN_CHECK::PR_END_DATE	= TEST_PR_END_DATE$
	PR_TRN_CHECK::CHECK		= ""
	PR_TRN_CHECK::CHECK_DATE	= ""
	PR_TRN_CHECK::PAYFREQ		= PR_EMP_MASTER::PAYFREQ
	PR_TRN_CHECK::BATCH		= ""

	PR_TRN_CHECK::UPDATE_FLAG	= 0%

	WHEN ERROR IN
		PUT #PR_TRN_CHECK.CH%
	USE
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

3190	RETURN

3200	!******************************************************************
	! Scan through the deduction file
	!******************************************************************
	!
	! Get Tax/Ded detail information
	!
	WHEN ERROR IN
		FIND #PR_TRN_DED.CH%, &
			KEY #0% EQ TEST_EMPNUM$ + TEST_PR_END_DATE$, &
			REGARDLESS
	USE
		CONTINUE 3290 IF ERR = 155%
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

3220	!
	! Check for an interrupt
	!
	GOSUB Interupt IF RRR_FLAG%

	WHEN ERROR IN
		GET #PR_TRN_DED.CH%, REGARDLESS
	USE
		CONTINUE 3290 IF ERR = 11%
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

	GOTO 3290 IF PR_TRN_DED::EMPNUM + PR_TRN_DED::PR_END_DATE <> &
		TEST_EMPNUM$ + TEST_PR_END_DATE$

	!
	! See if this is a tax
	!
	TAX_TYPE% = (INSTR(1%, TAX_TYPE_TABLE$, PR_TRN_DED::CODE) + 2%) / 3%

	IF TAX_TYPE%
	THEN
		!
		! Accumulate fica tax in case there is more than one
		! check to print for an employee.
		!
		IF PR_TRN_DED::CODE = "FI" AND CHECK_WRITTEN%
		THEN
			CUR_FICA_TAX = FUNC_ROUND(CUR_FICA_TAX + &
				PR_TRN_DED::AMOUNT, 2%)
		END IF

		IF PR_TRN_DED::CODE = "FH" AND CHECK_WRITTEN%
		THEN
			CUR_HI_TAX = FUNC_ROUND(CUR_HI_TAX + &
				PR_TRN_DED::AMOUNT, 2%)
		END IF

		NET_CHECK = FUNC_ROUND(NET_CHECK - PR_TRN_DED::AMOUNT, 2%) &
			IF PR_TRN_DED::DTYPE = "D"

		GOTO 3220
	END IF

3230	!
	! Check Deductions
	!
	IF PR_TRN_DED::DTYPE <> "T" AND PR_TRN_DED::DTYPE <> "M"
	THEN

		NET_CHECK = FUNC_ROUND(NET_CHECK - PR_TRN_DED::AMOUNT, 2%)

		FOR TAX_TYPE% = 1% TO LEN(SUBJECT_TYPE_TABLE$) / 4%

			!
			! See if taxable and reportable
			!
			SUBJECT_CODE$ = MID(SUBJECT_TYPE_TABLE$, &
				TAX_TYPE% * 4% - 3%, 3%)

			CALL PR_READ_SUBJTAX(PR_ERNDED_DEF.CH%, &
				SUBJECT_CODE$, &
				PR_TRN_DED::DTYPE, &
				PR_TRN_DED::CODE, &
				TAXABLE%, &
				REPORTABLE%)

			IF (TAXABLE% = 0% AND PR_TRN_DED::DTYPE <> "D") OR &
				(TAXABLE% <> 0% AND PR_TRN_DED::DTYPE = "D")
			THEN
				!
				! Take out after taxes, not before taxes.
				!
				EMP_NT(TAX_TYPE%) = &
					FUNC_ROUND(EMP_NT(TAX_TYPE%) + &
					PR_TRN_DED::AMOUNT, 2%)
			END IF

			IF (REPORTABLE% = 0% AND PR_TRN_DED::DTYPE <> "D") OR &
				(REPORTABLE% <> 0% AND PR_TRN_DED::DTYPE = "D")
			THEN
				!
				! Take out after taxes, not before taxes.
				!
				EMP_NR(TAX_TYPE%) = &
					FUNC_ROUND(EMP_NR(TAX_TYPE%) + &
					PR_TRN_DED::AMOUNT, 2%)
			END IF

		NEXT TAX_TYPE%

	ELSE
		!
		! Add noncompensation items
		!
		TEMP = 0.0 ! JUNK
	END IF

	GOTO 3220

3290	RETURN

	%Page

 AddFDeductions:
3400	!******************************************************************
	! Now, we get to try to add in all of the F type of
	! deductions. These add on to all checks in a payroll,
	! not just the first one.
	!******************************************************************

	!
	! Get first record for employee in standard Pay deduction file
	!
	WHEN ERROR IN
		FIND #PR_EMP_STD_ERNDED.CH%, &
			KEY #0% EQ TEST_EMPNUM$, &
			REGARDLESS
	USE
		CONTINUE 3490 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_EMP_STD_ERNDED"
		CONTINUE HelpError
	END WHEN

3420	WHEN ERROR IN
		GET #PR_EMP_STD_ERNDED.CH%, REGARDLESS
	USE
		CONTINUE 3490 IF ERR = 11%
		FILENAME$ = "PR_EMP_STD_ERNDED"
		CONTINUE HelpError
	END WHEN

	GOTO 3490 &
		IF PR_EMP_STD_ERNDED::EMPNUM <> TEST_EMPNUM$

	PR_EMP_STD_ERNDED::ENDDAT = "" &
		IF PR_EMP_STD_ERNDED::ENDDAT = "00000000"

	GOTO 3420 &
		IF (PR_EMP_STD_ERNDED::RTYPE <> "F") &
		OR ((PR_END_DATE$ > PR_EMP_STD_ERNDED::ENDDAT) AND &
			(EDIT$(PR_EMP_STD_ERNDED::ENDDAT, -1%) <> ""))

3430	!
	! Make sure limits haven't been exceeded
	!
	IF (PR_EMP_STD_ERNDED::LIMIT = 0.0) OR &
		(PR_EMP_STD_ERNDED::LIMIT > PR_EMP_STD_ERNDED::CTDBAL) OR &
		(PR_EMP_STD_ERNDED::METHOD = "7")
	THEN
		!
		! Handle calculating deduction based of methods
		!
		SELECT PR_EMP_STD_ERNDED::METHOD
		CASE "1"
			!
			! Per Hour
			!
			AMOUNT = FUNC_ROUND(PR_EMP_STD_ERNDED::RATE * &
				WORK_HOUR, 2%)
			TAXABLE_AMOUNT = 0.0
			REPORTABLE_AMOUNT = 0.0

		CASE "3"
			!
			! % of gross
			!
			AMOUNT = FUNC_ROUND(PR_EMP_STD_ERNDED::RATE * &
				WORK_GROSS / 100.0, 2%)
			TAXABLE_AMOUNT = WORK_GROSS
			REPORTABLE_AMOUNT = WORK_GROSS

		CASE "4"
			!
			! % of net
			!
			TAXABLE_AMOUNT = NET_CHECK
			REPORTABLE_AMOUNT = TAXABLE_AMOUNT
			AMOUNT = FUNC_ROUND(PR_EMP_STD_ERNDED::RATE * &
				TAXABLE_AMOUNT / 100.0, 2%)

		CASE "6"
			!
			! Per hour/premium
			!
			AMOUNT = FUNC_ROUND(PR_EMP_STD_ERNDED::RATE * &
				(WORK_HOUR + WORK_PREM - WORK_OVT), 2%)
			TAXABLE_AMOUNT = 0.0
			REPORTABLE_AMOUNT = 0.0

		CASE "7"
			!
			! Per hour with a limit
			!
			AMOUNT = FUNC_ROUND(PR_EMP_STD_ERNDED::RATE * &
				WORK_HOUR, 2%)
			AMOUNT = PR_EMP_STD_ERNDED::LIMIT &
				IF AMOUNT > PR_EMP_STD_ERNDED::LIMIT
			TAXABLE_AMOUNT = 0.0
			REPORTABLE_AMOUNT = 0.0

		CASE ELSE
			!
			! Anything else
			!
			AMOUNT = FUNC_ROUND(PR_EMP_STD_ERNDED::RATE, 2%)
			TAXABLE_AMOUNT = 0.0
			REPORTABLE_AMOUNT = 0.0
		END SELECT


		IF AMOUNT + PR_EMP_STD_ERNDED::CTDBAL > &
			PR_EMP_STD_ERNDED::LIMIT &
			AND PR_EMP_STD_ERNDED::LIMIT <> 0.0
		THEN
			AMOUNT = PR_EMP_STD_ERNDED::LIMIT - &
				PR_EMP_STD_ERNDED::CTDBAL
		END IF

		!
		! Set tax code to null.
		!
		TAX_CODE$ = ""

 !		IF (AMOUNT <> 0.0) OR &
 !			(REPORTABLE_AMOUNT <> 0.0) OR &
 !			(TAXABLE_AMOUNT <> 0.0) OR &
 !			(PR_EMP_STD_ERNDED::METHOD = "4")
 !		THEN
			PR_TRN_DED::EMPNUM	= TEST_EMPNUM$
			PR_TRN_DED::PR_END_DATE	= PR_END_DATE$
			PR_TRN_DED::DTYPE	= PR_EMP_STD_ERNDED::RTYPE
			PR_TRN_DED::CODE	= PR_EMP_STD_ERNDED::CODE
			PR_TRN_DED::AMOUNT	= AMOUNT
			PR_TRN_DED::TAX_CODE	= TAX_CODE$
			PR_TRN_DED::SSTATUS	= ""

			PR_TRN_DED::EXEMPT	= 0%
			PR_TRN_DED::ADDEXEMPT	= 0%
			PR_TRN_DED::UPDATE_FLAG	= 0%
			PR_TRN_DED::BATCH	= ""

			PR_TRN_DED::REPORTABLE	= REPORTABLE_AMOUNT
			PR_TRN_DED::TAXABLE	= TAXABLE_AMOUNT

			WHEN ERROR IN
				PUT #PR_TRN_DED.CH%
			USE
				FILENAME$ = "PR_TRN_DED"
				CONTINUE HelpError
			END WHEN

 !		END IF
	END IF

	GOTO 3420

3490	RETURN

	%PAGE

	!******************************************************************
	! Exit the program
	!******************************************************************
 ExitProgram:
	!
	! Exit to next program or menu
	!
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 AddAccrual:
17000	!*******************************************************************
	! Add accrual records in for current employee defined by
	! TEST_EMPNUM$ and PR_EN_DATE$.
	!*******************************************************************

	!
	! Skip if no accrual file, or period out of range.
	!
	GOTO 17090 IF (PR_EMP_ACCRUAL.CH% = 0%) OR (EFF_PERIOD% = 0%)

	!
	! Search for any accruals defined for this employee
	!
	WHEN ERROR IN
		FIND #PR_EMP_ACCRUAL.CH%, &
			KEY #0% EQ TEST_EMPNUM$, &
			REGARDLESS
	USE
		CONTINUE 17090
	END WHEN

17010	!
	! Pull in accrual definition
	!
	WHEN ERROR IN
		GET #PR_EMP_ACCRUAL.CH%, REGARDLESS
	USE
		CONTINUE 17090
	END WHEN

	GOTO 17090 IF PR_EMP_ACCRUAL::EMPNUM <> TEST_EMPNUM$

17020	!
	! Search for good rate for this item
	!
	WHEN ERROR IN
		GET #PR_EMP_ACCRUAL_RATE.CH%, &
			KEY #0% EQ PR_EMP_ACCRUAL::EMPNUM + &
				PR_EMP_ACCRUAL::ATYPE, &
			REGARDLESS
	USE
		CONTINUE 17010
	END WHEN

	!
	! Skip if not viable
	!
	GOTO 17010 IF PR_EMP_ACCRUAL_RATE::SDATE > PR_END_DATE$

17030	!
	! Loop through to find correct rate to use
	!
	PR_EMP_ACCRUAL_RATE_XXX = PR_EMP_ACCRUAL_RATE

	WHEN ERROR IN
		GET #PR_EMP_ACCRUAL_RATE.CH%, REGARDLESS
	USE
		CONTINUE 17040
	END WHEN

	IF (PR_EMP_ACCRUAL::EMPNUM = PR_EMP_ACCRUAL_RATE::EMPNUM) AND &
		(PR_EMP_ACCRUAL::ATYPE = PR_EMP_ACCRUAL_RATE::ATYPE) AND &
		(PR_EMP_ACCRUAL_RATE::SDATE <= PR_END_DATE$)
	THEN
		!
		! This is a better rate.
		!
		GOTO 17030
	END IF

17040	!
	! We now have proper rate in PR_EMP_ACCRUAL_RATE_XXX.
	! Do a calculation.
	!
	GOTO 17010 IF MID(PR_EMP_ACCRUAL_RATE_XXX::FREQ, EFF_PERIOD%, 1%) <> "Y"

	!
	! Skip if they havent worked long enough
	!
	GOTO 17010 IF PR_EMP_ACCRUAL_RATE_XXX::MINHOUR > WORK_HOUR

	!
	! Do calculation based on rate type
	!
	SELECT PR_EMP_ACCRUAL_RATE_XXX::RATECODE

	CASE "1"
		!
		! Flat rate
		!
		AMOUNT = FUNC_ROUND(PR_EMP_ACCRUAL_RATE_XXX::HOURRATE, 2%)

	CASE "2"
		!
		! Based on regular hours
		!
		IF PR_EMP_ACCRUAL_RATE_XXX::MAXHOUR > WORK_HOUR
		THEN
			AMOUNT = PR_EMP_ACCRUAL_RATE_XXX::MAXHOUR
		ELSE
			AMOUNT = WORK_HOUR
		END IF

		AMOUNT = FUNC_ROUND(AMOUNT * &
			PR_EMP_ACCRUAL_RATE_XXX::HOURRATE, 2%)

	CASE ELSE
		!
		! Junk Method
		!
		AMOUNT = 0.0

	END SELECT

	GOTO 17010 IF AMOUNT = 0.0

17080	!
	! Add record to PAY folder
	!
	GOSUB FindMaster

	!
	! Try for a specific rate based on the pay code first
	!
	CALL PR_READ_RATE_CODE(PR_EMP_MASTER::EMPNUM, &
		PR_EMP_MASTER::OPER, &
		BATCH_NO$, &
		RATE_TYPE$, &
		PR_EMP_ACCRUAL_RATE_XXX::ATYPE, &
		HOUR_RATE, &
		PIECE_RATE, &
		FACTOR%, &
		STDEFF, &
		EVALDATE$, &
		EFF_DATE$)

	!
	! Try for the standard employee rate if a specific rate isn't found
	!
	IF HOUR_RATE = 0.0
	THEN
		CALL PR_READ_RATE_CODE(PR_EMP_MASTER::EMPNUM, &
			PR_EMP_MASTER::OPER, &
			BATCH_NO$, &
			PR_EMP_MASTER::RATE_TYPE, &
			PR_EMP_MASTER::RATE_CDE, &
			HOUR_RATE, &
			PIECE_RATE, &
			FACTOR%, &
			STDEFF, &
			EVALDATE$, &
			EFF_DATE$)
	END IF

	!
	! Try for a generic rate if a no other rate is found
	!
	IF HOUR_RATE = 0.0
	THEN
		CALL PR_READ_RATE(PR_EMP_MASTER::EMPNUM, &
			PR_EMP_MASTER::OPER, &
			BATCH_NO$, &
			RATE_TYPE$, &
			RATE_CDE$, &
			HOUR_RATE, &
			PIECE_RATE, &
			FACTOR%, &
			STDEFF, &
			EVALDATE$, &
			EFF_DATE$)
	END IF

	IF (PR_ERNDED_DEF::ETYPE <> "P") OR &
		(PR_ERNDED_DEF::CODE <> PR_EMP_ACCRUAL::ATYPE)
	THEN
		!
		! Look up ernded definition file
		!
		PR_ERNDED_DEF::ETYPE = "P"
		PR_ERNDED_DEF::CODE = PR_TRN_PAY::CODE

		PR_ERNDED_DEF::DESCR = STRING$(LEN(PR_ERNDED_DEF::DESCR), 63%)
		PR_ERNDED_DEF::DRCR_ACCT = &
			STRING$(LEN(PR_ERNDED_DEF::DRCR_ACCT), 63%)
		PR_ERNDED_DEF::ACCRUAL_ACCT = &
			STRING$(LEN(PR_ERNDED_DEF::ACCRUAL_ACCT), 63%)

		WHEN ERROR IN
			GET #PR_ERNDED_DEF.CH%, &
				KEY #0% EQ "P" + PR_EMP_ACCRUAL::ATYPE, &
				REGARDLESS
		USE
			CONTINUE 17085 IF ERR = 155%
			FILENAME$ = "PR_ERNDED_DEF"
			CONTINUE HelpError
		END WHEN

	END IF

17085	CALL GL_ASSG_ACCMASK(PR_ERNDED_DEF::DRCR_ACCT, &
		PR_EMP_MASTER::ACCT, &
		ACCT_NUM$)

	PR_TRN_PAY::EMPNUM	= TEST_EMPNUM$
	PR_TRN_PAY::PR_END_DATE	= PR_END_DATE$

	PR_TRN_PAY::EMP_SKILL	= PR_EMP_MASTER::EMP_SKILL
	PR_TRN_PAY::EMP_GRADE	= PR_EMP_MASTER::EMP_GRADE
	PR_TRN_PAY::ACCT	= ACCT_NUM$
	PR_TRN_PAY::SUBACC	= PR_EMP_MASTER::SUBACC
	PR_TRN_PAY::OPER	= PR_EMP_MASTER::OPER
	PR_TRN_PAY::LOCATION	= PR_EMP_MASTER::LOCATION
	PR_TRN_PAY::DEPT	= PR_EMP_MASTER::DEPT
	PR_TRN_PAY::WORK_CENTER	= PR_EMP_MASTER::WORK_CENTER
	PR_TRN_PAY::UNION	= PR_EMP_MASTER::UNION

	PR_TRN_PAY::PTYPE	= "A"
	PR_TRN_PAY::RTYPE	= RATE_TYPE$
	PR_TRN_PAY::CODE	= PR_EMP_ACCRUAL_RATE_XXX::ATYPE
	PR_TRN_PAY::HOUR_RATE	= HOUR_RATE
	PR_TRN_PAY::REG_HR	= AMOUNT
	PR_TRN_PAY::OVT_HR	= 0.0
	PR_TRN_PAY::PIECE	= PIECE_RATE
	PR_TRN_PAY::FACTOR	= FACTOR%
	PR_TRN_PAY::GROSS	= FUNC_ROUND(AMOUNT * HOUR_RATE, 2%)
	PR_TRN_PAY::TAX_PKG	= PR_EMP_MASTER::TAX_PKG
	PR_TRN_PAY::UPDATE_FLAG	= 0%
	PR_TRN_PAY::BATCH_ENTRY	= ""
	PR_TRN_PAY::SEQNUM	= ""
	PR_TRN_PAY::BATCH	= ""

	PUT #PR_TRN_PAY.CH%

	GOTO 17010

17090	RETURN

	%PAGE

 FindMaster:
	!*******************************************************************
	! Search employee master file for TEST_EMPNUM
	!*******************************************************************

17100	IF TEST_EMPNUM$ <> PR_EMP_MASTER::EMPNUM
	THEN
		PR_EMP_MASTER::PAYFREQ	= 1%
		PR_EMP_MASTER::TAX_PKG	= "??"
		PR_EMP_MASTER::ACCT	= "??????????????????"
		PR_EMP_MASTER::SUBACC	= "??????????"
		PR_EMP_MASTER::TRADE	= "??????"
		PR_EMP_MASTER::OPER	= "????????"
		PR_EMP_MASTER::UNION	= "??"
		PR_EMP_MASTER::LOCATION	= "????"
		PR_EMP_MASTER::DEPT	= "??????"
		PR_EMP_MASTER::WORK_CENTER= "????"
		PR_EMP_MASTER::EMP_SKILL= "??????"
		PR_EMP_MASTER::EMP_GRADE= "??"

		WHEN ERROR IN
			GET #PR_EMP_MASTER.CH%, &
				KEY #0% EQ TEST_EMPNUM$, &
				REGARDLESS
		USE
			CONTINUE 17190
		END WHEN

	END IF

17190	RETURN

	%PAGE

 AddSTDDed:
	!****************************************************************
	! Add standard pay deductions for employee defined by
	! TEST_EMPNUM$ and PR_EN_DATE$.
	!****************************************************************

17500	!
	! Skip calc if effective period is out of range (why waste time)
	!
	GOTO 17590 IF EFF_PERIOD% = 0%

	!
	! Get first record for employee in standard Pay deduction file
	!
	WHEN ERROR IN
		FIND #PR_EMP_STD_ERNDED.CH%, &
			KEY #0% EQ TEST_EMPNUM$, &
			REGARDLESS
	USE
		CONTINUE 17590 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_EMP_STD_ERNDED"
		CONTINUE HelpError
	END WHEN

17520	WHEN ERROR IN
		GET #PR_EMP_STD_ERNDED.CH%, REGARDLESS
	USE
		CONTINUE 17590 IF ERR = 11%
		FILENAME$ = "PR_EMP_STD_ERNDED"
		CONTINUE HelpError
	END WHEN

	GOTO 17590 &
		IF PR_EMP_STD_ERNDED::EMPNUM <> TEST_EMPNUM$

	GOTO 17520 &
		IF MID(PR_EMP_STD_ERNDED::FREQ, EFF_PERIOD%, 1%) <> "Y"

	PR_EMP_STD_ERNDED::ENDDAT = "" &
		IF PR_EMP_STD_ERNDED::ENDDAT = "00000000"

	GOTO 17570 &
		IF PR_EMP_STD_ERNDED::RTYPE = "P"

	GOTO 17520 &
		IF (PR_EMP_STD_ERNDED::RTYPE <> "D") &
		OR ((PR_END_DATE$ > PR_EMP_STD_ERNDED::ENDDAT) AND &
			(EDIT$(PR_EMP_STD_ERNDED::ENDDAT, -1%) <> ""))

17530	!
	! Make sure limits haven't been exceeded
	!
	IF (PR_EMP_STD_ERNDED::LIMIT = 0.0) OR &
		(PR_EMP_STD_ERNDED::LIMIT > PR_EMP_STD_ERNDED::CTDBAL) OR &
		(PR_EMP_STD_ERNDED::METHOD = "7")
	THEN
		!
		! Handle calculating deduction based of methods
		!
		SELECT PR_EMP_STD_ERNDED::METHOD
		CASE "1"
			!
			! Per hour
			!
			AMOUNT = FUNC_ROUND(PR_EMP_STD_ERNDED::RATE * &
				WORK_HOUR, 2%)
			TAXABLE_AMOUNT = 0.0
			REPORTABLE_AMOUNT = 0.0

		CASE "3"
			!
			! % of gross
			!
			AMOUNT = FUNC_ROUND(PR_EMP_STD_ERNDED::RATE * &
				WORK_GROSS / 100.0, 2%)
			TAXABLE_AMOUNT = WORK_GROSS
			REPORTABLE_AMOUNT = WORK_GROSS

		CASE "4"
			!
			! % of net
			!
			AMOUNT = 0.0
			PR_EMP_STD_ERNDED::RTYPE = "X"
			TAXABLE_AMOUNT = 0.0
			REPORTABLE_AMOUNT = 0.0

		CASE "6"
			!
			! Per hour/premium
			!
			AMOUNT = FUNC_ROUND(PR_EMP_STD_ERNDED::RATE * &
				(WORK_HOUR + WORK_PREM - WORK_OVT), 2%)
			TAXABLE_AMOUNT = 0.0
			REPORTABLE_AMOUNT = 0.0

		CASE "7"
			!
			! Per hour (with a limit)
			!
			AMOUNT = FUNC_ROUND(PR_EMP_STD_ERNDED::RATE * &
				WORK_HOUR, 2%)
			AMOUNT = PR_EMP_STD_ERNDED::LIMIT &
				IF AMOUNT > PR_EMP_STD_ERNDED::LIMIT
			TAXABLE_AMOUNT = 0.0
			REPORTABLE_AMOUNT = 0.0

		CASE ELSE
			!
			! Anything else
			!
			AMOUNT = FUNC_ROUND(PR_EMP_STD_ERNDED::RATE, 2%)
			TAXABLE_AMOUNT = 0.0
			REPORTABLE_AMOUNT = 0.0
		END SELECT


		IF AMOUNT + PR_EMP_STD_ERNDED::CTDBAL > &
			PR_EMP_STD_ERNDED::LIMIT &
			AND PR_EMP_STD_ERNDED::LIMIT <> 0.0
		THEN
			AMOUNT = PR_EMP_STD_ERNDED::LIMIT - &
				PR_EMP_STD_ERNDED::CTDBAL
		END IF

		!
		! Set tax code to null.
		!
		TAX_CODE$ = ""

		!
		! If this is extra tax being withheld then look up the
		! tax code.
		!
		IF INSTR(1%, TAX_TYPE_TABLE$, PR_EMP_STD_ERNDED::CODE)
		THEN
			GOSUB LookUpTaxCode
		END IF

		IF (AMOUNT <> 0.0) OR &
			(REPORTABLE_AMOUNT <> 0.0) OR &
			(TAXABLE_AMOUNT <> 0.0) OR &
			(PR_EMP_STD_ERNDED::METHOD = "4")
		THEN
			PR_TRN_DED::EMPNUM	= TEST_EMPNUM$
			PR_TRN_DED::PR_END_DATE	= PR_END_DATE$
			PR_TRN_DED::DTYPE	= PR_EMP_STD_ERNDED::RTYPE
			PR_TRN_DED::CODE	= PR_EMP_STD_ERNDED::CODE
			PR_TRN_DED::AMOUNT	= AMOUNT
			PR_TRN_DED::TAX_CODE	= TAX_CODE$
			PR_TRN_DED::SSTATUS	= ""

			PR_TRN_DED::EXEMPT	= 0%
			PR_TRN_DED::ADDEXEMPT	= 0%
			PR_TRN_DED::UPDATE_FLAG	= 0%
			PR_TRN_DED::BATCH	= ""

			PR_TRN_DED::REPORTABLE	= REPORTABLE_AMOUNT
			PR_TRN_DED::TAXABLE	= TAXABLE_AMOUNT

			WHEN ERROR IN
				PUT #PR_TRN_DED.CH%
			USE
				FILENAME$ = "PR_TRN_DED"
				CONTINUE HelpError
			END WHEN

		END IF
	END IF

	GOTO 17520

17570	!
	! Finish searching for standard pay in pay file
	!
	GOTO 17520 IF PR_EMP_STD_ERNDED::RTYPE <> "P" &
		OR PR_END_DATE$ > PR_EMP_STD_ERNDED::ENDDAT AND &
			EDIT$(PR_EMP_STD_ERNDED::ENDDAT, -1%) <> ""

	!
	! Look up the employee master file
	!
	GOSUB FindMaster

17580	!
	! Determine if this standard pay should be add to the pay file
	!
	IF PR_EMP_STD_ERNDED::LIMIT = 0.0 OR &
		PR_EMP_STD_ERNDED::LIMIT > PR_EMP_STD_ERNDED::CTDBAL
	THEN
		!
		! Handle the different methods
		!
		SELECT PR_EMP_STD_ERNDED::METHOD

		CASE "1"
			AMOUNT = FUNC_ROUND(PR_EMP_STD_ERNDED::RATE * &
				WORK_HOUR, 2%)
		CASE "3"
			AMOUNT = FUNC_ROUND(PR_EMP_STD_ERNDED::RATE * &
				WORK_GROSS / 100.0, 2%)
		CASE "6"
			AMOUNT = FUNC_ROUND(PR_EMP_STD_ERNDED::RATE * &
				(WORK_HOUR + WORK_PREM - WORK_OVT), 2%)

		CASE ELSE
			AMOUNT = PR_EMP_STD_ERNDED::RATE
		END SELECT

		IF AMOUNT + PR_EMP_STD_ERNDED::CTDBAL > &
			PR_EMP_STD_ERNDED::LIMIT &
			AND PR_EMP_STD_ERNDED::LIMIT <> 0.0
		THEN
			AMOUNT = PR_EMP_STD_ERNDED::LIMIT - &
				PR_EMP_STD_ERNDED::CTDBAL
		END IF

		IF AMOUNT <> 0.0
		THEN
			PR_TRN_PAY::EMPNUM	= TEST_EMPNUM$
			PR_TRN_PAY::PR_END_DATE	= PR_END_DATE$

			PR_TRN_PAY::EMP_SKILL	= PR_EMP_MASTER::EMP_SKILL
			PR_TRN_PAY::EMP_GRADE	= PR_EMP_MASTER::EMP_GRADE
			PR_TRN_PAY::ACCT	= PR_EMP_MASTER::ACCT
			PR_TRN_PAY::SUBACC	= PR_EMP_MASTER::SUBACC
			PR_TRN_PAY::OPER	= PR_EMP_MASTER::OPER
			PR_TRN_PAY::LOCATION	= PR_EMP_MASTER::LOCATION
			PR_TRN_PAY::DEPT	= PR_EMP_MASTER::DEPT
			PR_TRN_PAY::WORK_CENTER	= PR_EMP_MASTER::WORK_CENTER
			PR_TRN_PAY::UNION	= PR_EMP_MASTER::UNION

			PR_TRN_PAY::PTYPE	= "O"
			PR_TRN_PAY::RTYPE	= ""
			PR_TRN_PAY::CODE	= PR_EMP_STD_ERNDED::CODE
			PR_TRN_PAY::HOUR_RATE	= 0.0
			PR_TRN_PAY::REG_HR	= 0.0
			PR_TRN_PAY::OVT_HR	= 0.0
			PR_TRN_PAY::PIECE	= 0.0
			PR_TRN_PAY::FACTOR	= 0%
			PR_TRN_PAY::GROSS	= AMOUNT
			PR_TRN_PAY::TAX_PKG	= PR_EMP_MASTER::TAX_PKG
			PR_TRN_PAY::UPDATE_FLAG	= 0%
			PR_TRN_PAY::BATCH_ENTRY	= ""
			PR_TRN_PAY::SEQNUM	= ""
			PR_TRN_PAY::BATCH	= ""

			WHEN ERROR IN
				PUT #PR_TRN_PAY.CH%
			USE
				FILENAME$ = "PR_TRN_PAY"
				CONTINUE HelpError
			END WHEN

		END IF
	END IF

	GOTO 17520


17590	!
	! Finished with this employee's standard Pay/Ded
	!
	WORK_HOUR, WORK_GROSS, WORK_OVT, WORK_PREM = 0.0

	RETURN

	%PAGE

 TaxCal:
	!******************************************************************
	! Calculate taxes
	!******************************************************************

	!
	! Subtract not taxable from federal and fica earnings
	!
	EMPLOYEE(TAX_TYPE%, 1%)::TAXABLE = EMPLOYEE(TAX_TYPE%, 1%)::TAXABLE - &
		EMP_NT(TAX_TYPE%) &
		FOR TAX_TYPE% = 1% TO 3%
	EMPLOYEE(TAX_TYPE%, 1%)::REPORTABLE = &
		EMPLOYEE(TAX_TYPE%, 1%)::REPORTABLE - &
		EMP_NR(TAX_TYPE%) &
		FOR TAX_TYPE% = 1% TO 3%

	!
	! Allocate taxable wages for all other taxes
	!
	! This segment takes various amounts that are not allocated to
	! a particular (state), and distributes them through all of the
	! (states) proportionally.
	!
	FOR TAX_TYPE% = 4% TO LEN(SUBJECT_TYPE_TABLE$) / 4%

		FACTOR = 1.0
		FACTOR = 1.0 - (EMP_NT(TAX_TYPE%) / &
			EMPLOYEE(TAX_TYPE%, 0%)::TAXABLE) &
			IF EMPLOYEE(TAX_TYPE%, 0%)::TAXABLE <> 0.0

		TOTAL_TO_DIST = FUNC_ROUND(EMPLOYEE(TAX_TYPE%, 0%)::TAXABLE - &
			EMP_NT(TAX_TYPE%), 2%)

		FOR LOOP% = 1% TO EMP_WH_CODE%(TAX_TYPE%) - 1%
			EMPLOYEE(TAX_TYPE%, LOOP%)::TAXABLE = &
				FUNC_ROUND(EMPLOYEE(TAX_TYPE%, &
				LOOP%)::TAXABLE * FACTOR, 2%)
			TOTAL_TO_DIST = FUNC_ROUND(TOTAL_TO_DIST - &
				EMPLOYEE(TAX_TYPE%, LOOP%)::TAXABLE, 2%)
		NEXT LOOP%

		EMPLOYEE(TAX_TYPE%, EMP_WH_CODE%(TAX_TYPE%))::TAXABLE = &
			FUNC_ROUND(TOTAL_TO_DIST, 2%)

		!
		! Reportable
		!
		FACTOR = 1.0
		FACTOR = 1.0 - (EMP_NR(TAX_TYPE%) / &
			EMPLOYEE(TAX_TYPE%, 0%)::REPORTABLE) &
			IF EMPLOYEE(TAX_TYPE%, 0%)::REPORTABLE <> 0.0

		TOTAL_TO_DIST = FUNC_ROUND(EMPLOYEE(TAX_TYPE%, &
			0%)::REPORTABLE - &
			EMP_NR(TAX_TYPE%), 2%)

		FOR LOOP% = 1% TO EMP_WH_CODE%(TAX_TYPE%) - 1%
			EMPLOYEE(TAX_TYPE%, LOOP%)::REPORTABLE = &
				FUNC_ROUND(EMPLOYEE(TAX_TYPE%, &
				LOOP%)::REPORTABLE * &
				FACTOR, 2%)
			TOTAL_TO_DIST = FUNC_ROUND(TOTAL_TO_DIST - &
				EMPLOYEE(TAX_TYPE%, LOOP%)::REPORTABLE, 2%)
		NEXT LOOP%

		EMPLOYEE(TAX_TYPE%, EMP_WH_CODE%(TAX_TYPE%))::REPORTABLE = &
			FUNC_ROUND(TOTAL_TO_DIST, 2%)

	NEXT TAX_TYPE%

18100	!--------------------------------------------------------------------
	! Look up fica (oasdi) tax
	!--------------------------------------------------------------------

	FICA_SUBJECT = EMPLOYEE(1%, 1%)::TAXABLE + CUR_FICA_SUBJECT
	FICA_TAX = CUR_FICA_TAX

	WHEN ERROR IN
		GET #PR_REG_TAXES.CH%, &
			KEY #0% EQ TEST_EMPNUM$ + "FI", &
			REGARDLESS
	USE
		CONTINUE 18120 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		CONTINUE HelpError
	END WHEN

	FICA_TAX = FUNC_ROUND(FICA_TAX + PR_REG_TAXES::TAX(LOOP%), 2%) &
		FOR LOOP% = 0% TO 3%
	FICA_SUBJECT = FUNC_ROUND(FICA_SUBJECT + &
		PR_REG_TAXES::TAXABLE(LOOP%), 2%) &
		FOR LOOP% = 0% TO 3%

18120	!------------------------------------------------------------------
	! Calculate fica taxes
	!------------------------------------------------------------------

	TAXABLE_AMOUNT = EMPLOYEE(1%, 1%)::TAXABLE
	REPORTABLE_AMOUNT = EMPLOYEE(1%, 1%)::REPORTABLE

	!
	! Add to the current FICA subject information in case this
	! employee gets two checks this period.
	!
	CUR_FICA_SUBJECT = CUR_FICA_SUBJECT + EMPLOYEE(1%, 1%)::TAXABLE

	!
	! Calculate difference between how much of this employees
	! annual wages was fica taxable, and how much of it has
	! been fica taxed already.
	!
	IF (FICA_SUBJECT > FICA_LIMIT) AND (FICA_LIMIT <> 0.0)
	THEN
		FICA_OASDI = FICA_LIMIT
	ELSE
		FICA_OASDI = FICA_SUBJECT
	END IF

	!
	! Calculate the amount of fica to take out this period
	! to make his entire wages fica taxed.
	!
	! Calculation is YTD_OASDI_PAY * FICA_OASDI_RATE +
	!		YTD_HI_PAY * FICA_HI_RATE -
	!		PREVIOUS_FICA_PAID
	!
	AMOUNT = FUNC_ROUND(FICA_OASDI * FICA_EMPE_PCT - &
		FICA_TAX, 2%)

	!
	! Add record to transaction file if necessary
	!
	IF (AMOUNT <> 0.0) OR &
		(REPORTABLE_AMOUNT <> 0.0) OR &
		(TAXABLE_AMOUNT <> 0.0)
	THEN
		NET_CHECK = FUNC_ROUND(NET_CHECK - AMOUNT, 2%)

		!
		! Add current fica tax in case there are more
		! than check for a single employee
		!
		CUR_FICA_TAX = FUNC_ROUND(CUR_FICA_TAX + AMOUNT, 2%)

		PR_TRN_DED::EMPNUM	= TEST_EMPNUM$
		PR_TRN_DED::PR_END_DATE	= TEST_PR_END_DATE$
		PR_TRN_DED::DTYPE	= "C"
		PR_TRN_DED::CODE	= "FI"
		PR_TRN_DED::AMOUNT	= AMOUNT
		PR_TRN_DED::TAX_CODE	= ""
		PR_TRN_DED::SSTATUS	= ""

		PR_TRN_DED::EXEMPT	= 0%
		PR_TRN_DED::ADDEXEMPT	= 0%
		PR_TRN_DED::UPDATE_FLAG	= 0%
		PR_TRN_DED::BATCH	= ""

		PR_TRN_DED::REPORTABLE	= REPORTABLE_AMOUNT
		PR_TRN_DED::TAXABLE	= TAXABLE_AMOUNT

		WHEN ERROR IN
			PUT #PR_TRN_DED.CH%
		USE
			FILENAME$ = "PR_TRN_DED"
			CONTINUE HelpError
		END WHEN

	END IF

18150	!--------------------------------------------------------------------
	! Look up fica tax
	!--------------------------------------------------------------------

	FICA_SUBJECT = EMPLOYEE(2%, 1%)::TAXABLE + CUR_HI_SUBJECT
	FICA_TAX = CUR_HI_TAX

	WHEN ERROR IN
		GET #PR_REG_TAXES.CH%, &
			KEY #0% EQ TEST_EMPNUM$ + "FH", &
			REGARDLESS
	USE
		CONTINUE 18160 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		CONTINUE HelpError
	END WHEN

	FICA_TAX = FUNC_ROUND(FICA_TAX + PR_REG_TAXES::TAX(LOOP%), 2%) &
		FOR LOOP% = 0% TO 3%
	FICA_SUBJECT = FUNC_ROUND(FICA_SUBJECT + &
		PR_REG_TAXES::TAXABLE(LOOP%), 2%) &
		FOR LOOP% = 0% TO 3%

18160	!------------------------------------------------------------------
	! Calculate fica taxes
	!------------------------------------------------------------------

	TAXABLE_AMOUNT = EMPLOYEE(2%, 1%)::TAXABLE
	REPORTABLE_AMOUNT = EMPLOYEE(2%, 1%)::REPORTABLE

	!
	! Add to the current FICA subject information in case this
	! employee gets two checks this period.
	!
	CUR_HI_SUBJECT = CUR_HI_SUBJECT + EMPLOYEE(2%, 1%)::TAXABLE

	!
	! Calculate difference between how much of this employees
	! annual wages was fica taxable, and how much of it has
	! been fica taxed already.
	!
	IF (FICA_SUBJECT > FICA_LIMIT_HI) AND (FICA_LIMIT_HI <> 0.0)
	THEN
		FICA_HI = FICA_LIMIT_HI
	ELSE
		FICA_HI = FICA_SUBJECT
	END IF

	!
	! Calculate the amount of fica to take out this period
	! to make his entire wages fica taxed.
	!
	! Calculation is YTD_OASDI_PAY * FICA_OASDI_RATE +
	!		YTD_HI_PAY * FICA_HI_RATE -
	!		PREVIOUS_FICA_PAID
	!
	AMOUNT = FUNC_ROUND(FICA_HI * FICA_EMPE_PCT_HI - &
		FICA_TAX, 2%)

	!
	! Add record to transaction file if necessary
	!
	IF (AMOUNT <> 0.0) OR &
		(REPORTABLE_AMOUNT <> 0.0) OR &
		(TAXABLE_AMOUNT <> 0.0)
	THEN
		NET_CHECK = FUNC_ROUND(NET_CHECK - AMOUNT, 2%)

		!
		! Add current fica tax in case there are more
		! than check for a single employee
		!
		CUR_HI_TAX = FUNC_ROUND(CUR_HI_TAX + AMOUNT, 2%)

		PR_TRN_DED::EMPNUM	= TEST_EMPNUM$
		PR_TRN_DED::PR_END_DATE	= TEST_PR_END_DATE$
		PR_TRN_DED::DTYPE	= "C"
		PR_TRN_DED::CODE	= "FH"
		PR_TRN_DED::AMOUNT	= AMOUNT
		PR_TRN_DED::TAX_CODE	= ""
		PR_TRN_DED::SSTATUS	= ""

		PR_TRN_DED::EXEMPT	= 0%
		PR_TRN_DED::ADDEXEMPT	= 0%
		PR_TRN_DED::UPDATE_FLAG	= 0%
		PR_TRN_DED::BATCH	= ""

		PR_TRN_DED::REPORTABLE	= REPORTABLE_AMOUNT
		PR_TRN_DED::TAXABLE	= TAXABLE_AMOUNT

		PUT #PR_TRN_DED.CH%

	END IF

18200	!---------------------------------------------------------------
	! Calculate withholding taxes
	!---------------------------------------------------------------

	!
	! Set federal tax to zero
	!
	FED_TAXABLE, FED_REPORTABLE, FED_TAX = 0.0

	FOR TAX_TYPE% = 3% TO LEN(SUBJECT_TYPE_TABLE$) / 4%

		TAX_TYPE$ = MID(TAX_TYPE_TABLE$, TAX_TYPE% * 3% - 2%, 2%)

		FOR LOOP% = 1% TO EMP_WH_CODE%(TAX_TYPE%)

			GOTO 18290 &
				IF EMPLOYEE(TAX_TYPE%, LOOP%)::TAXABLE = 0.0 &
				AND EMPLOYEE(TAX_TYPE%, LOOP%)::REPORTABLE = 0.0

			AMOUNT = 0.0
			TAXABLE_AMOUNT = 0.0
			REPORTABLE_AMOUNT = 0.0

			!
			! Handle 'SI' special, because it is a weird
			! tax that doesn't look up in the tables like
			! most others.
			!
			GOTO 18215 IF (TAX_TYPE$ = "SI")

			WHEN ERROR IN
				GET #PR_EMP_STATUS.CH%, &
					KEY #0% EQ TEST_EMPNUM$ + TAX_TYPE$ + &
					EMPLOYEE(TAX_TYPE%, LOOP%)::CODE, &
					REGARDLESS
			USE
				CONTINUE 18205 IF ERR = 155% AND &
					(TAX_TYPE$ = "SW" OR TAX_TYPE$ = "SI")
				CONTINUE 18280 IF ERR = 155%
				FILENAME$ = "PR_EMP_STATUS"
				CONTINUE HelpError
			END WHEN

			GOTO 18210

18205			!
			! If we couldn't get the correct status for the
			! state, let's try for the federal record
			! instead.
			!
			CALL ENTR_3MESSAGE(SCOPE, &
				"Employee " + TRM$(TEST_EMPNUM$) + &
				" is missing Status for State " + &
				EMPLOYEE(TAX_TYPE%, LOOP%)::CODE, 0%)

			WHEN ERROR IN
				GET #PR_EMP_STATUS.CH%, &
					KEY #0% EQ TEST_EMPNUM$ + "FW" + "  ", &
					REGARDLESS
			USE
				CONTINUE 18280 IF ERR = 155%
				FILENAME$ = "PR_EMP_STATUS"
				CONTINUE HelpError
			END WHEN

18210			!
			! Get tax table information (Not used for SI)
			!
			WHEN ERROR IN
				GET #PR_TAX_TABLE.CH%, &
					KEY #0% EQ LEFT(TAX_TYPE$, 1%) + &
					EMPLOYEE(TAX_TYPE%, LOOP%)::CODE + &
					PR_EMP_STATUS::STSTATUS, &
					REGARDLESS
			USE
				IF ERR = 155% OR ERR = 9%
				THEN
					!
					! Handle an exempt employee so taxable/reportable
					! will show.
					!
					IF (PR_EMP_STATUS::STSTATUS = "E")
					THEN
						AMOUNT = 0.0
						TAXABLE_AMOUNT, REPORTABLE_AMOUNT = &
							EMPLOYEE(TAX_TYPE%, LOOP%)::REPORTABLE
					ELSE
						CALL ENTR_3MESSAGE(SCOPE, &
							"WARNING: Emp " + &
							TRM$(PR_EMP_MASTER::EMPNUM) + &
							" undef. status for `" + &
							TAX_TYPE$ + "' of `" + &
							PR_EMP_STATUS::STSTATUS + "'", 0%)

						AMOUNT = 0.0
						TAXABLE_AMOUNT, REPORTABLE_AMOUNT = &
							EMPLOYEE(TAX_TYPE%,LOOP%)::REPORTABLE
					END IF
					CONTINUE 18278
				END IF

				FILENAME$ = "PR_TAX_TABLE_" + YYYY$
				CONTINUE HelpError

			END WHEN

			TEST_LOOP% = 1%
			TEST_LOOP% = I% &
				IF PR_TAX_TABLE::OVER(I%) <> 0.0 &
				FOR I% = 1% TO 11%

18215			!
			! Figure out which tax calculation section to use
			!
			SELECT TAX_TYPE$

			CASE "FW"
	!*******************************************************************
	! Annualize federal payroll
	!*******************************************************************

				ANNUAL_AMOUNT = EMPLOYEE(TAX_TYPE%, &
					LOOP%)::TAXABLE * &
					PR_EMP_MASTER::PAYFREQ

				IF ANNUAL_AMOUNT < &
					PR_TAX_TABLE::LOW_INCOME AND &
					(PR_TAX_TABLE::LOW_INCOME <> 0.0)
				THEN
					AMOUNT = 0.0
					GOTO 18225
				END IF

				ANNUAL_AMOUNT = EMPLOYEE(TAX_TYPE%, &
					LOOP%)::TAXABLE * &
					PR_EMP_MASTER::PAYFREQ - &
					((PR_EMP_STATUS::EXEMPT * &
						PR_TAX_TABLE::PR_EX) + &
					(PR_EMP_STATUS::ADDEXEMPT * &
					PR_TAX_TABLE::PR_EX_ADD)) - &
					PR_TAX_TABLE::STD_WH

				GOTO 18220 &
					IF ANNUAL_AMOUNT <= &
					PR_TAX_TABLE::OVER(I%) &
					FOR I% = 1% TO TEST_LOOP%

				I% = TEST_LOOP% + 1%

18220				WHEN ERROR IN
					I% = I% - 1%
					AMOUNT = FUNC_ROUND((PR_TAX_TABLE::TAXAMT(I%) + &
						(ANNUAL_AMOUNT - &
						PR_TAX_TABLE::OVER(I%)) * &
						PR_TAX_TABLE::PLUS(I%) / 100.0) / &
						PR_EMP_MASTER::PAYFREQ, 2%)
				USE
					FILENAME$ = "PR_EMP_MASTER(" + &
						PR_EMP_MASTER::EMPNUM + ")"
					CONTINUE HelpError
				END WHEN

18225				TAXABLE_AMOUNT, FED_TAXABLE = &
					EMPLOYEE(TAX_TYPE%, LOOP%)::TAXABLE
				REPORTABLE_AMOUNT, FED_REPORTABLE = &
					EMPLOYEE(TAX_TYPE%, LOOP%)::REPORTABLE

				FED_TAX = AMOUNT


			CASE "SW", "CW", "DW", "EW"
				! Withholding
18235	!*******************************************************************
	! Handle W types
	!*******************************************************************

				IF PR_TAX_TABLE::CALC_BASIS = "F"
				THEN
					FACTOR = 1.0
					FACTOR = EMPLOYEE(TAX_TYPE%, &
						LOOP%)::TAXABLE/FED_TAXABLE &
						IF FED_TAXABLE <> 0.0 AND &
						LOOP% > 1%
					AMOUNT = FUNC_ROUNDPAYROLL(FED_TAX * &
						PR_TAX_TABLE::BASIS_PCT / 100.0 * &
						FACTOR, PR_TAX_TABLE::DROUNDING)
					TAXABLE_AMOUNT = &
						EMPLOYEE(TAX_TYPE%, LOOP%)::TAXABLE
					REPORTABLE_AMOUNT = &
						EMPLOYEE(TAX_TYPE%, LOOP%)::REPORTABLE
					GOTO 18278
				END IF

				TAXABLE_AMOUNT = &
					EMPLOYEE(TAX_TYPE%, LOOP%)::TAXABLE
				REPORTABLE_AMOUNT = &
					EMPLOYEE(TAX_TYPE%, LOOP%)::REPORTABLE
				ANNUAL_AMOUNT = &
					EMPLOYEE(TAX_TYPE%, LOOP%)::TAXABLE * &
					PR_EMP_MASTER::PAYFREQ

				OTHER = 0.0
				OTHER = ANNUAL_AMOUNT * &
					PR_TAX_TABLE::ADJ_GRS_PCT / 100.0 &
					IF PR_TAX_TABLE::ADJ_GRS_PCT <> 0.0
				OTHER = PR_TAX_TABLE::MIN_STD_ADJ &
					IF OTHER = 0.0 OR &
					OTHER <= PR_TAX_TABLE::MIN_STD_ADJ
				OTHER = PR_TAX_TABLE::MAX_STD_ADJ &
					IF OTHER >= &
					PR_TAX_TABLE::MAX_STD_ADJ AND &
					PR_TAX_TABLE::MAX_STD_ADJ <> 0.0

				IF ANNUAL_AMOUNT < &
					PR_TAX_TABLE::LOW_INCOME AND &
					(PR_TAX_TABLE::LOW_INCOME <> 0.0)
				THEN
					AMOUNT = 0.0
					GOTO 18245
				END IF

				ANNUAL_AMOUNT = FUNC_ROUND(ANNUAL_AMOUNT - &
					((PR_EMP_STATUS::EXEMPT * &
					PR_TAX_TABLE::PR_EX) + &
					(PR_EMP_STATUS::ADDEXEMPT * &
					PR_TAX_TABLE::PR_EX_ADD)) - &
					(FED_TAX * FED_CREDIT_PCT / 100.0) - &
					PR_TAX_TABLE::STD_WH - &
					OTHER, 2%)


				GOTO 18240 &
					IF ANNUAL_AMOUNT <= &
					PR_TAX_TABLE::OVER(I%) &
					FOR I% = 1% TO TEST_LOOP%
				I% = TEST_LOOP% + 1%

18240				I% = I% - 1%
				AMOUNT = &
					FUNC_ROUNDPAYROLL(( &
					PR_TAX_TABLE::TAXAMT(I%) + &
					(ANNUAL_AMOUNT - &
					PR_TAX_TABLE::OVER(I%)) * &
					PR_TAX_TABLE::PLUS(I%) / 100.0) / &
					PR_EMP_MASTER::PAYFREQ, &
					PR_TAX_TABLE::DROUNDING)

				!
				! Arizona Minimum Tax
				!
				IF (PR_TAX_TABLE::MINTAX <> 0.0)
				THEN
					TEMP = FUNC_ROUNDPAYROLL( &
						PR_TAX_TABLE::MINTAX / &
						PR_EMP_MASTER::PAYFREQ, &
						PR_TAX_TABLE::DROUNDING)
					IF (AMOUNT < TEMP)
					THEN
						AMOUNT = PR_TAX_TABLE::MINTAX
					END IF
				END IF

18245				I% = I%		! Necessary else compiler gets
						! error on GOTO.


			CASE "SU"
18250	!*******************************************************************
	! U types
	!*******************************************************************

18252				!---------------------------------------------
				! Get SUI subject wages from ernded file
				!---------------------------------------------
				SUI_SUBJECT = EMPLOYEE(8%, LOOP%)::TAXABLE + &
					CUR_SUI_SUBJECT
				TAXABLE_AMOUNT = EMPLOYEE(8%, LOOP%)::TAXABLE
				REPORTABLE_AMOUNT = &
					EMPLOYEE(8%, LOOP%)::REPORTABLE

				WHEN ERROR IN
					FIND #PR_REG_ERNDED.CH%, &
						KEY #0% EQ TEST_EMPNUM$, &
						REGARDLESS
				USE
					CONTINUE 18256 IF ERR = 155% OR ERR = 9%
					FILENAME$ = "PR_REG_ERNDED_" + YYYY$
					CONTINUE HelpError
				END WHEN

18254				WHEN ERROR IN
					GET #PR_REG_ERNDED.CH%, REGARDLESS
				USE
					CONTINUE 18256 IF ERR = 11%
					FILENAME$ = "PR_REG_ERNDED_" + YYYY$
					CONTINUE HelpError
				END WHEN

				IF PR_REG_ERNDED::EMPNUM = TEST_EMPNUM$
				THEN

					CALL PR_READ_SUBJTAX(PR_ERNDED_DEF.CH%, &
						"SUI", &
						PR_REG_ERNDED::ETYPE, &
						PR_REG_ERNDED::CODE, &
						TAXABLE%, &
						REPORTABLE%)

					!
					! Calculate ytd
					!
					SUI_TOTAL = 0.0

					SUI_TOTAL = FUNC_ROUND(SUI_TOTAL + &
						PR_REG_ERNDED::QTR_DOLL(LP%), 2%) &
						FOR LP% = 0% TO 3%

					!
					! Add to subject if taxable
					!
					IF PR_REG_ERNDED::ETYPE = "P" AND &
						TAXABLE% = 0%
					THEN
						SUI_SUBJECT = &
							FUNC_ROUND(SUI_SUBJECT + &
							SUI_TOTAL, 2%)
					END IF

					!
					! Subtract from subject if not taxable
					!
					IF PR_REG_ERNDED::ETYPE = "D" AND &
						TAXABLE%
					THEN
						SUI_SUBJECT = &
							FUNC_ROUND(SUI_SUBJECT - &
							SUI_TOTAL, 2%)
					END IF

					GOTO 18254
				END IF

18256				!----------------------------------------------
				! Calculate SUI taxes
				!----------------------------------------------
				AMOUNT = 0.0

				SUI_PCT = (PR_TAX_TABLE::SUI_PCT) / 100.0
				SUI_LIMIT = PR_TAX_TABLE::SUI_MAX

				SUI = EMPLOYEE(8%,LOOP%)::TAXABLE
				SUI = SUI_LIMIT - &
					(SUI_SUBJECT - &
					EMPLOYEE(8%, LOOP%)::TAXABLE) &
					IF (SUI_SUBJECT > SUI_LIMIT) AND &
					(SUI_LIMIT <> 0.0)
				SUI = 0.0 IF SUI < 0.0

				AMOUNT = FUNC_ROUND(SUI * SUI_PCT, 2%)

				CUR_SUI_SUBJECT = FUNC_ROUND(CUR_SUI_SUBJECT + &
					EMPLOYEE(8%, LOOP%)::TAXABLE, 2%)



			CASE "SX"
18260	!*******************************************************************
	! X types
	!*******************************************************************

18262				!----------------------------------------------
				! Get OST subject wages from ernded file
				!----------------------------------------------

				OST_SUBJECT = EMPLOYEE(4%, LOOP%)::TAXABLE + &
					CUR_OST_SUBJECT
				TAXABLE_AMOUNT = EMPLOYEE(4%, LOOP%)::TAXABLE
				REPORTABLE_AMOUNT = &
					EMPLOYEE(4%, LOOP%)::REPORTABLE

				WHEN ERROR IN
					FIND #PR_REG_ERNDED.CH%, &
						KEY #0% EQ TEST_EMPNUM$, &
						REGARDLESS
				USE
					CONTINUE 18266 IF ERR = 155% OR ERR = 9%
					FILENAME$ = "PR_REG_ERNDED_" + YYYY$
					CONTINUE HelpError
				END WHEN

18264				WHEN ERROR IN
					GET #PR_REG_ERNDED.CH%, REGARDLESS
				USE
					CONTINUE 18266 IF ERR = 11%
					FILENAME$ = "PR_REG_ERNDED_" + YYYY$
					CONTINUE HelpError
				END WHEN

				IF PR_REG_ERNDED::EMPNUM = TEST_EMPNUM$
				THEN
					CALL PR_READ_SUBJTAX(PR_ERNDED_DEF.CH%, &
						"OST", &
						PR_REG_ERNDED::ETYPE, &
						PR_REG_ERNDED::CODE, &
						TAXABLE%, &
						REPORTABLE%)

					!
					! Calculate ytd
					!
					OST_TOTAL = 0.0

					OST_TOTAL = FUNC_ROUND(OST_TOTAL + &
						PR_REG_ERNDED::QTR_DOLL(LP%), 2%) &
						FOR LP% = 0% TO 3%

					!
					! Add to subject if taxable
					!
					IF PR_REG_ERNDED::ETYPE = "P" AND &
						TAXABLE% = 0%
					THEN
						OST_SUBJECT = &
							FUNC_ROUND(OST_SUBJECT + &
							OST_TOTAL, 2%)
					END IF

					!
					! Subtract from subject if not taxable
					!
					IF PR_REG_ERNDED::ETYPE = "D" AND &
						TAXABLE%
					THEN
						OST_SUBJECT = &
							FUNC_ROUND(OST_SUBJECT - &
							OST_TOTAL, 2%)
					END IF

					GOTO 18264
				END IF

18266				!----------------------------------------------
				! Calculate OST taxes
				!
				! Calculated as a percentage of the current
				! gross, with an annual limit, as well as a
				! maximum limit for any one pay period.
				!----------------------------------------------
				AMOUNT = 0.0

				OST_PCT = (PR_TAX_TABLE::OT_ANL_PCT) / 100.0
				OST_LIMIT = PR_TAX_TABLE::OT_ANL_MAX
				PER_LIMIT = &
					FUNC_ROUND(PR_TAX_TABLE::OT_DED_MAX / &
					PR_EMP_MASTER::PAYFREQ, 2%)

				OST = EMPLOYEE(4%, LOOP%)::TAXABLE
				OST = OST_LIMIT - (OST_SUBJECT - OST) &
					IF (OST_SUBJECT > OST_LIMIT) AND &
						(OST_LIMIT <> 0.0)
				!
				! Check Taxable amount against the Period limit
				!
				OST = PER_LIMIT IF OST > PER_LIMIT AND &
					PER_LIMIT <> 0.0

				OST = 0.0 IF OST < 0.0

				AMOUNT = FUNC_ROUND(OST * OST_PCT, 2%)

				!
				! Check Tax amount against the Period limit
				!
				CUR_OST_SUBJECT = FUNC_ROUND(CUR_OST_SUBJECT + &
					EMPLOYEE(4%, LOOP%)::TAXABLE, 2%)


			CASE "SI"
18272	!*********************************************************************
	! Get SWC subject wages from ernded file
	!*********************************************************************
				SWC_SUBJECT = EMPLOYEE(9%, LOOP%)::HOURS + &
					CUR_SWC_SUBJECT

				WHEN ERROR IN
					FIND #PR_REG_ERNDED.CH%, &
						KEY #0% EQ TEST_EMPNUM$, &
						REGARDLESS
				USE
					CONTINUE 18276 IF ERR = 155% OR ERR = 9%
					FILENAME$ = "PR_REG_ERNDED_" + YYYY$
					CONTINUE HelpError
				END WHEN

18274				WHEN ERROR IN
					GET #PR_REG_ERNDED.CH%, REGARDLESS
				USE
					CONTINUE 18276 IF ERR = 11%
					FILENAME$ = "PR_REG_ERNDED_" + YYYY$
					CONTINUE HelpError
				END WHEN

				IF PR_REG_ERNDED::EMPNUM = TEST_EMPNUM$
				THEN
					CALL PR_READ_SUBJTAX(PR_ERNDED_DEF.CH%, &
						"SWC", &
						PR_REG_ERNDED::ETYPE, &
						PR_REG_ERNDED::CODE, &
						TAXABLE%, &
						REPORTABLE%)

					!
					! Take hours in one quarter
					!
					SWC_TOTAL = &
						PR_REG_ERNDED::REG_HRS(QUARTER%) + &
						PR_REG_ERNDED::PRE_HRS(QUARTER%)

					!
					! Add to subject if taxable
					!
					SWC_SUBJECT = &
						FUNC_ROUND(SWC_SUBJECT + &
						SWC_TOTAL, 2%) &
						IF PR_REG_ERNDED::ETYPE = "P" &
						AND TAXABLE% = 0%

					!
					! Subtract from subject if not taxable
					!
					SWC_SUBJECT = FUNC_ROUND(SWC_SUBJECT - &
						SWC_TOTAL, 2%) &
						IF PR_REG_ERNDED::ETYPE = "D" &
						AND TAXABLE%

					GOTO 18274
				END IF

18276				!----------------------------------------------
				! Calculate SWC taxes
				!
				! Check qtd hours against maximum
				!----------------------------------------------

				AMOUNT = 0.0
				IF SWC_SUBJECT >= MAXQHOURS AND &
					MAXQHOURS <> 0.0
				THEN
					!
					! How much over ?
					!
					IF SWC_SUBJECT - MAXQHOURS < &
						EMPLOYEE(9%, LOOP%)::HOURS
					THEN
						!
						! Prorate tax
						!
						AMOUNT = (SWC_SUBJECT - &
							MAXQHOURS) * &
							EMP_OST(LOOP%) / &
							EMPLOYEE(9%, LOOP%)::HOURS &
							IF EMPLOYEE(9%, &
							LOOP%)::HOURS <> 0.0
					END IF
				ELSE
					!
					! All dollars are subject this time
					!
					AMOUNT = EMP_OST(LOOP%)
					PR_EMP_STATUS::STSTATUS = ""
					PR_EMP_STATUS::EXEMPT = 0%
					PR_EMP_STATUS::ADDEXEMPT = 0%
				END IF


				!
				! Carry on for the next rec for the same empl
				!
				CUR_SWC_SUBJECT = FUNC_ROUND(CUR_SWC_SUBJECT + &
					EMPLOYEE(9%, LOOP%)::HOURS, 2%)


			CASE ELSE
				!
				! Unknown tax type.
				! Should never get here.
				!

			END SELECT

18278	!*******************************************************************
	! Write out record
	!*******************************************************************

			IF PR_EMP_STATUS::STSTATUS = "E"
			THEN
				AMOUNT = 0.0
			END IF

			IF (AMOUNT <> 0.0) OR &
				(REPORTABLE_AMOUNT <> 0.0) OR &
				(TAXABLE_AMOUNT <> 0.0)
			THEN
				NET_CHECK = FUNC_ROUND(NET_CHECK - AMOUNT, 2%)

				PR_TRN_DED::EMPNUM	= TEST_EMPNUM$
				PR_TRN_DED::PR_END_DATE	= TEST_PR_END_DATE$
				PR_TRN_DED::DTYPE	= "C"
				PR_TRN_DED::CODE	= TAX_TYPE$
				PR_TRN_DED::AMOUNT	= AMOUNT
				PR_TRN_DED::TAX_CODE	= &
					EMPLOYEE(TAX_TYPE%, LOOP%)::CODE
				PR_TRN_DED::SSTATUS	= &
					PR_EMP_STATUS::STSTATUS
				PR_TRN_DED::EXEMPT	= &
					PR_EMP_STATUS::EXEMPT
				PR_TRN_DED::ADDEXEMPT	= &
					PR_EMP_STATUS::ADDEXEMPT
				PR_TRN_DED::UPDATE_FLAG	= 0%
				PR_TRN_DED::BATCH	= ""

				PR_TRN_DED::REPORTABLE	= REPORTABLE_AMOUNT
				PR_TRN_DED::TAXABLE	= TAXABLE_AMOUNT

				WHEN ERROR IN
					PUT #PR_TRN_DED.CH%
				USE
					FILENAME$ = "PR_TRN_DED"
					CONTINUE HelpError
				END WHEN
			END IF


18280		NEXT LOOP%


18290	NEXT TAX_TYPE%


	!
	! Zero totals
	!
	FOR I% = 1% TO 10%
		EMP_NT(I%) = 0.0
		EMP_NR(I%) = 0.0
		EMP_WH_CODE%(I%) = 0%
		EMP_OST(I%) = 0.0

		FOR J% = 0% TO 10%
			EMPLOYEE(I%, J%) = BLANK_EMPLOYEE
		NEXT J%
	NEXT I%

	!
	! Set Federal EMP_WH_CODE%() for 1
	!
	EMP_WH_CODE%(3%) = 1%

18300	!---------------------------------------------------------------------
	! Add deductions that are a percent of net pay
	!---------------------------------------------------------------------

	WHEN ERROR IN
		GET #PR_TRN_DED.CH%, &
			KEY #0% EQ TEST_EMPNUM$ + TEST_PR_END_DATE$ + "X"
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 18400 IF ERR = 155%
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

18310	AMOUNT = 0.0

	WHEN ERROR IN
		GET #PR_EMP_STD_ERNDED.CH%, KEY #0% EQ TEST_EMPNUM$ + "D" + &
			PR_TRN_DED::CODE, &
			REGARDLESS
	USE
		CONTINUE 18320 IF ERR = 155%
		FILENAME$ = "PR_EMP_STD_ERNDED"
		CONTINUE HelpError
	END WHEN

	AMOUNT = FUNC_ROUND(NET_CHECK * PR_EMP_STD_ERNDED::RATE / 100.0, 2%)

	IF AMOUNT + PR_EMP_STD_ERNDED::CTDBAL > &
		PR_EMP_STD_ERNDED::LIMIT &
		AND PR_EMP_STD_ERNDED::LIMIT <> 0.0
	THEN
		AMOUNT = PR_EMP_STD_ERNDED::LIMIT - &
			PR_EMP_STD_ERNDED::CTDBAL
	END IF

18320	NET_CHECK = FUNC_ROUND(NET_CHECK - AMOUNT, 2%)

	WHEN ERROR IN
		DELETE #PR_TRN_DED.CH%
	USE
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

	PR_TRN_DED::DTYPE	= "D"
	PR_TRN_DED::AMOUNT	= AMOUNT
	PR_TRN_DED::BATCH	= ""

	PR_TRN_DED::REPORTABLE	= NET_CHECK
	PR_TRN_DED::TAXABLE	= NET_CHECK

	WHEN ERROR IN
		PUT #PR_TRN_DED.CH%
	USE
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

	GOTO 18300

18400	!---------------------------------------------------------------------
	! Is the net check amount greater than zero
	!---------------------------------------------------------------------
	GOTO 18500 IF NET_CHECK >= 0.0

	WHEN ERROR IN
		FIND #PR_TRN_DED.CH%, KEY #0% EQ TEST_EMPNUM$
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 18500 IF ERR = 155%
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

	!
	! Disable for interrupt trap
	!
	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	TEMP_ITEM$ = SCOPE::PRG_ITEM

	SCOPE::PRG_ITEM = "NEG_CHECK"

18410	WHEN ERROR IN
		GET #PR_TRN_DED.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 18490 IF ERR = 11%
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

	GOTO 18490 IF PR_TRN_DED::EMPNUM <> TEST_EMPNUM$ &
		OR NET_CHECK >= 0.0

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Net check is negative " + &
		FORMAT$(NET_CHECK, "###,###.##"), &
		12%, 5%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Ded Code " + PR_TRN_DED::CODE + &
		" Amount " + &
		FORMAT$(PR_TRN_DED::AMOUNT, "###,###.##"), &
		14%, 5%)
 NewAmount:
	AMOUNT = ENTR_3NUMBER(SCOPE, SMG_SCREEN_DATA%, &
		"", "New Amount ", PR_TRN_DED::AMOUNT * 1.0, &
		8%, "###,###.##", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO NewAmount
	END SELECT

18420	IF AMOUNT <> PR_TRN_DED::AMOUNT
	THEN
		NET_CHECK = FUNC_ROUND(NET_CHECK + PR_TRN_DED::AMOUNT, 2%)

		PR_TRN_DED::AMOUNT = AMOUNT

		WHEN ERROR IN
			UPDATE	#PR_TRN_DED.CH%
		USE
			FILENAME$ = "PR_TRN_DED"
			CONTINUE HelpError
		END WHEN
	END IF

	GOTO 18410

18490	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, SPACE$(78%), 12%, 5%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, SPACE$(78%), 14%, 5%)

	CALL ENTR_3MESSAGE(SCOPE, "Calculating Taxes", 1%)

	!
	! Enable interrupt trap
	!
	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		LOC(OUTP_XUNSOL) BY VALUE, &
		LOC(SCOPE::SMG_KBID) BY VALUE)

	SCOPE::PRG_ITEM = TEMP_ITEM$

18500	RETURN

	%Page


 LookUpTaxCode:
18600	!********************************************************************
	! This section looks up the tax code is the standard deduction is
	! extra tax withheld.
	!********************************************************************
	TAX_CODE$ = ""

	GOSUB FindMaster

	PR_TAX_PKG::CODE = ""

18605	WHEN ERROR IN
		FIND #PR_TAX_PKG.CH%, &
			KEY #0% EQ PR_EMP_MASTER::TAX_PKG, &
			REGARDLESS
	USE
		CONTINUE 18690 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_TAX_PKG"
		CONTINUE HelpError
	END WHEN

18610	WHEN ERROR IN
		GET #PR_TAX_PKG.CH%, REGARDLESS
	USE
		CONTINUE 18690 IF ERR = 11%
		FILENAME$ = "PR_TAX_PKG"
		CONTINUE HelpError
	END WHEN

	IF PR_TAX_PKG::TAX_PKG = PR_EMP_MASTER::TAX_PKG
	THEN
		IF PR_TAX_PKG::STTYPE = PR_EMP_STD_ERNDED::CODE
		THEN
			TAX_CODE$ = PR_TAX_PKG::CODE
		ELSE
			GOTO 18610
		END IF
	END IF

18690	RETURN

	%PAGE

 TestPayDeletable:
18700	!*******************************************************************
	! Check to see if the pay record currently loaded is deletable.
	! This is done by checking the _CHECK_ file to see if it has a
	! check number assigned to it.
	!*******************************************************************

	!
	! Assume we can delete it
	!
	REMOVE_FLAG% = 1%

	!
	! Is this a check that has already been written.
	!
	! Only do I/O operation if we don't already have record (to speed
	! this dog up).
	!
	IF (PR_TRN_PAY::EMPNUM <> PR_TRN_CHECK::EMPNUM) OR &
		(PR_TRN_PAY::PR_END_DATE <> PR_TRN_CHECK::PR_END_DATE)
	THEN
		WHEN ERROR IN
			GET #PR_TRN_CHECK.CH%, &
				KEY #0% EQ PR_TRN_PAY::EMPNUM + &
				PR_TRN_PAY::PR_END_DATE, &
				REGARDLESS
		USE
			!
			! Deletable? Must be if no check record found.
			!
			!
			! Speed up likely possible future checks
			!
			PR_TRN_CHECK::EMPNUM = PR_TRN_PAY::EMPNUM
			PR_TRN_CHECK::PR_END_DATE = PR_TRN_PAY::PR_END_DATE
			PR_TRN_CHECK::CHECK = ""

			CONTINUE 18790

		END WHEN
	END IF

	IF PR_TRN_CHECK::CHECK <> ""
	THEN
		REMOVE_FLAG% = 0%
	END IF

18790	RETURN

	%PAGE

 DeletePay:
18800	!*******************************************************************
	! Remove current pay record.
	! Placed here to make error trapping easier.
	!*******************************************************************

	WHEN ERROR IN
		DELETE #PR_TRN_PAY.CH%
	USE
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

18820	RETURN

	%PAGE

 TestPayStd:
18850	!*******************************************************************
	! Check to see if current record is a Standard Earnings record.
	!*******************************************************************

	!
	! Assume it is not
	!
	REMOVE_FLAG% = 0%

	!
	! Search STD Pay file
	!
	WHEN ERROR IN
		GET #PR_EMP_STD_ERNDED.CH%, &
			KEY #0% EQ PR_TRN_PAY::EMPNUM + "P" + PR_TRN_PAY::CODE, &
			REGARDLESS
	USE
		CONTINUE 18890
	END WHEN

	REMOVE_FLAG% = 1%

18890	RETURN

	%PAGE

 Interupt:
	!********************************************************************
	! Handle any special junk in RRR_FLAG%
	!********************************************************************
	SELECT RRR_FLAG%

	!
	! Repaint screen
	!
	CASE SMG$K_TRM_F11, SMG$K_TRM_CTRLW
		SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	!
	! Help
	!
	CASE SMG$K_TRM_HELP
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)
		CALL HELP_34MESSAGE(SCOPE, "", &
			SCOPE::PRG_IDENT, SCOPE::PRG_PROGRAM, "", &
			SCOPE::PRG_ITEM)
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	!
	! Interrupt
	!
	CASE SMG$K_TRM_F6, SMG$K_TRM_F20
		SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

		CALL MENU_3INTERRUPT(SCOPE)

		SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
			LOC(OUTP_XUNSOL) BY VALUE, &
			LOC(SCOPE::SMG_KBID) BY VALUE)

	END SELECT

	RRR_FLAG% = 0%

	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
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
