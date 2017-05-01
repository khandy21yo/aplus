1	%TITLE "Print Payroll W2 Form"
	%SBTTL "PR_FORM_W2"
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
	! ID:PRW2
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print W-2 Forms\* option in the W-2 Processing menu allows for the
	!	printing of the W-2 Forms.
	!	.b
	!	The W-2's may be printed on double-wide or single-wide forms
	!	and can be printed in employee number, social security number or
	!	alphabetical order.
	!	.b
	!	If the forms jam in the middle of printing, you can restart them at that
	!	point by specifying the start page (SP) as the first page to be reprinted
	!	when running the forms a second time.
	!	.b
	!	Note: If your printer has a letter quality mode, you may want to use
	!	it because it usually goes through the carbon copies better
	!	than draft mode, although it is much slower.
	!	.b
	!	Before running the W2's, please check the following:
	!	.list 0,"*"
	!	.le
	!	The Federal and State ID numbers are correct.
	!	.le
	!	Your company name and address is correct.
	!	.le
	!	All data for the year has been entered (Year is closed).
	!	.le
	!	The amounts printed on the W2 register are correct.
	!	.le
	!	The 941's are correct.
	!	.le
	!	All employee's Social Security Numbers are defined.
	!	.le
	!	Any special deductions (ie. 401(k)'s) are defined in the PAYDED
	!	definition file.
	!	.le
	!	You have received the correct W2 format for this year (run a few blanks).
	!	.els
	!	.lm -5
	!
	! Index:
	!	.x Annual>Reports>W-2 Forms
	!	.x W-2 Forms>Print
	!	.x Print>W-2 Forms
	!
	! Option:
	!	PR_FORM_W2$CONFIRM
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_FORM_W2
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_FORM_W2, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_FORM_W2.OBJ;*
	!
	! Author:
	!
	!	12/05/87 - Robert Peterson
	!
	! Modification history:
	!
	!	01/19/89 - Kevin Handy
	!		Hard-coded the fica limit into the program.
	!		THIS NEEDS TO BE FIXED TO PULL UP THE CORRECT
	!		FICA AMOUNT.
	!
	!	02/09/89 - Kevin Handy
	!		Modified for changes in ENTR_ENTER
	!
	!	04/03/89 - Kevin Handy
	!		Added code for SI tax package.
	!
	!	01/17/90 - Kevin Handy
	!		Fixed program so that it will look up the FICA
	!		limit in the tax tables (First 'F' record it can
	!		find).  Assumes Married and Single people will
	!		have the same FICA limit.
	!
	!	01/17/90 - Kevin Handy
	!		Modified error trapping making it possible to
	!		figure out errors.
	!
	!	01/19/90 - Kevin Handy
	!		Modified sortby list, so that SSN is the default,
	!		and allow Location sort.
	!
	!	01/24/90 - Kevin Handy
	!		Added start page capability.
	!
	!	01/29/90 - Kevin Handy
	!		Completely rewrote section that calculates wages.
	!
	!	01/30/90 - Kevin Handy
	!		Modified to handle P types in BOX_16A.
	!
	!	02/01/90 - Kevin Handy
	!		Added end page capability.
	!
	!	12/31/90 - Kevin Handy
	!		Modified to handle 2 high instead of 3 high
	!		W2 forms.
	!
	!	01/11/91 - Craig Tanner
	!		Added YYYY$ to some of filenames in error trapping.
	!
	!	01/26/91 - Kevin Handy
	!		Added code to handle 401k information.
	!
	!	06/19/91 - Craig Tanner
	!		Added section to update UTL_REPORT from master.
	!
	!	07/13/91 - Kevin Handy
	!		Removed PR_REG_ERNDED.CH and PR_ERNDED_DEF.CH
	!		from call to PR_FUNC_READTAXES.
	!
	!	07/13/91 - Kevin Handy
	!		Removed PR_FUNC_SUBJECT com definition.
	!
	!	07/13/91 - Kevin Handy
	!		Removed PR_ERNDED_DEF file which is no longer
	!		used in this program.
	!
	!	12/18/91 - Kevin Handy
	!		Modified to ignore "A" types in PR_REG_ERNDED.
	!
	!	12/26/91 - Kevin Handy
	!		Modified to handle split between HI and OASDI.
	!
	!	06/24/92 - Kevin Handy
	!		Modified to use OUTP_INITFORM function.
	!
	!	12/30/92 - Kevin Handy
	!		Quick modification to print out a state id
	!		and number on summary and total w2 form.
	!
	!	01/19/93 - Kevin Handy
	!		Modified to use array of records to handle
	!		employee, subtotal, total instead of BOX_TEXT()...
	!		because moving things around all the BOX_ variables
	!		was becomming impossible to keep track of.
	!		Added second state line.
	!
	!	12/22/93 - Kevin Handy
	!		Fixed bug where K401c() was not being zeroes out.
	!
	!	01/24/94 - Kevin Handy
	!		Fixed to use FICA_EMPE_PCT instead of FICA_EMPR_PCT.
	!
	!	02/01/94 - Kevin Handy
	!		Added code to handle reading W2 location out of
	!		the PR_ERNDED_DEF file instead of having to input
	!		all that garbage every time W2's are run.
	!
	!	02/02/94 - Kevin Handy
	!		Modifications to make this version print both
	!		federal and state forms so that I don't have
	!		to maintain two incompatible form printers.
	!
	!	01/09/94 - Kevin Handy
	!		Added code for locality tax.
	!
	!	01/19/95 - Kevin Handy
	!		Added section FRM-CODES so that I could make
	!		the new version of this program work with those
	!		people who don't have a current version of
	!		the payroll system.
	!
	!	01/23/95 - Kevin Handy
	!		Added code to handle OTHER section. Copied from 401k
	!		stuff.
	!
	!	01/23/95 - Kevin Handy
	!		Fixed handling of SX stuff, when it can't find the
	!		existing state.
	!
	!	01/25/95 - Kevin Handy
	!		Modified to summarise all other's (D?) with the same
	!		code together. (all 401k's, 402b's, ...)
	!
	!	01/26/95 - Kevin Handy
	!		Modified to make local wage equal to Montanna wage.
	!
	!	01/27/95 - Kevin Handy
	!		Added "CD" (Dependent Care) to form.
	!
	!	01/31/95 - Kevin Handy
	!		Lost input of BOX_16A input, since it can be
	!		handled as "FR" code, and blank pay codes
	!		won't confuse it.
	!
	!	01/31/95 - Kevin Handy
	!		Lost input of DEF_CONP$, because there is no
	!		code that uses it.
	!
	!	02/07/95 - Kevin Handy
	!		Added code to handle goofy case where they input
	!		a blank pay code.
	!
	!	02/07/95 - Kevin Handy
	!		Disabled the old 401K section, since it is
	!		better to do in under FRM-CODES now.
	!
	!	03/07/95 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!		Fix last parameter to entr_3choices.
	!
	!	05/18/95 - Kevin Handy
	!		Reformat source.
	!
	!	05/18/95 - Kevin Handy
	!		Move Year question to report settings screen.
	!		Required moving around several opens.
	!
	!	05/18/95 - Kevin Handy
	!		Added quarter option to report settings screen.
	!		Forces quarter to 4 every time it restarts.
	!		City on total tells it is only for certain quarter.
	!
	!	05/18/95 - Kevin Handy
	!		Merged in the stuff from PR_FORM_W2ASK so that
	!		I don't need to maintain a seperate program.
	!
	!	01/03/96 - Kevin Handy
	!		Removed lots and lots of commented out code.
	!
	!	01/22/96 - Kevin Handy
	!		Modified to add a SIGN multiplier for codes
	!		handled through W2LOCATION.
	!
	!	01/29/96 - Kevin Handy
	!		Changed STRING$(...,ASCII(" ")) to "" in
	!		several places.
	!
	!	09/09/96 - Kevin Handy
	!		Clena up (Check)
	!
	!	03/12/97 - Kevin Handy
	!		Handle FH taxes.
	!
	!	05/10/97 - Kevin Handy
	!		Lose PRNT.CH% variable.
	!
	!	05/12/97 - Kevin Handy
	!		Don't need to look up FICA_RATE any more.
	!		Use integer for #key
	!
	!	09/09/97 - Kevin Handy
	!		Lose unecessary external defintions
	!
	!	01/15/98 - Kevin Handy
	!		Put in something for those who still don't have
	!		the FICA split in the tax file.
	!
	!	06/02/98 - Kevin Handy
	!		Use '0' instead of 'KEY_NUM%' for the key
	!		number in the lookup to PR_TAX_PROFILE
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/26/98 - Kevin Handy
	!		Don't erase SMG_SCREEN_DATA%, which is
	!		never created.
	!
	!	01/12/99 - Kevin Handy
	!		Modified to handle case where no subtotals
	!		are wanted (new 98 change in W2 rules)
	!
	!	01/28/99 - Kevin Handy
	!		Fix problem where employees have both taxable
	!		FICA_OASDI and FICA_HI but only show OASDI.
	!
	!	02/04/99 - Kevin Handy
	!		Aaarrrggghhh. More fica problems.
	!
	!	01/21/2000 - Kevin Handy
	!		SX problems
	!
	!	01/28/2000 - Kevin Handy
	!		Local taxes problems
	!
	!	11/03/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!
	!	01/14/2002 - Kevin Handy
	!		Added 'OTHER1' and 'OTHER1C' as synonyms for
	!		'OTHER' AND 'OTHERC'
	!
	!	01/18/2001 - Kevin Handy
	!		Added "DUMMY_" values so I don't have to fix all
	!		of the forms already installed.
	!
	!	01/17/2004 - Kevin Handy
	!		Added "SDI" as an "OTHER" entry, instead of hardcoding
	!		it into the form.
	!
	!	01/19/2006 - Kevin Handy
	!		Created SICK_PAY variable. Nothing sets it yet,
	!		but alignment now displays an 'X'.
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PR_STRUCTURE.INC"

	!
	! Define maps
	!
	%INCLUDE "SOURCE:[UTL.OPEN]FORM_GROUP.HB"
	DIM FORM_GROUP_CDD FORM_GROUP(10%)	! Max of 10 groups

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.HB"
	MAP (UTL_REPORT)	UTL_REPORT_CDD		UTL_REPORT
	DECLARE			UTL_REPORT_CDD		UTL_REPORT_SYS

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.HB"
	MAP (UTL_PROFILE)	UTL_PROFILE_CDD		UTL_PROFILE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.HB"
	MAP (PR_REG_TAXES)	PR_REG_TAXES_CDD	PR_REG_TAXES

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.HB"
	MAP (PR_REG_ERNDED)	PR_REG_ERNDED_CDD	PR_REG_ERNDED

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.HB"
	MAP (PR_TAX_PROFILE_F)	PR_TAX_PROFILE_F_CDD	PR_TAX_PROFILE_F

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_S.HB"
	MAP (PR_TAX_PROFILE_F)	PR_TAX_PROFILE_S_CDD	PR_TAX_PROFILE_S

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_C.HB"
	MAP (PR_TAX_PROFILE_F)	PR_TAX_PROFILE_C_CDD	PR_TAX_PROFILE_C

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_E.HB"
	MAP (PR_TAX_PROFILE_F)	PR_TAX_PROFILE_E_CDD	PR_TAX_PROFILE_E

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_D.HB"
	MAP (PR_TAX_PROFILE_F)	PR_TAX_PROFILE_D_CDD	PR_TAX_PROFILE_D

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.HB"
	MAP (PR_TAX_TABLE) PR_TAX_TABLE_CDD PR_TAX_TABLE

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP (PR_ERNDED_DEF) PR_ERNDED_DEF_CDD PR_ERNDED_DEF

	MAP (PR_TEMP) &
		PR_TEMP.TTYPE$ = 2%, &
		PR_TEMP.CODE$ = 2%, &
		PR_TEMP.EMPNUM$ = 10%, &
		PR_TEMP.TAXABLE_WAGE(2%), &
		PR_TEMP.TAXES(2%)

	DIM PR_TAXES_STRUCT PR_TAXES(50%)

	RECORD W2_FORM_CDD
		! Company
		STRING	COMP_NAME = 40%
		STRING	COMP_ADD1 = 40%
		STRING	COMP_ADD2 = 40%
		STRING	COMP_CITY = 20%
		STRING	COMP_STATE = 2%
		STRING	COMP_ZIP = 10%
		! Employee
		STRING	EMP_NUM = 10%
		STRING	EMP_NAME = 40%
		STRING	EMP_ADD1 = 40%
		STRING	EMP_ADD2 = 40%
		STRING	EMP_CITY = 20%
		STRING	EMP_STATE = 2%
		STRING	EMP_ZIP = 10%
		STRING	EMP_SSN = 11%
		! Flags
		STRING	STAT_EMP = 1%
		STRING	DECEASED = 1%
		STRING	PEN_PLAN = 1%
		STRING	SICK_PAY = 1%
		STRING	LEGAL_REP = 1%
		STRING	EMP_942 = 1%
		STRING	SUBTOTAL = 1%
		STRING	DEF_COMP = 1%
		STRING	VOID = 1%
		! Other
		REAL	ALL_TIPS
		REAL	AD_EIC
		REAL	FRINGE_BENEFIT
		REAL	QUAL
		REAL	NONQUAL
		REAL	SOCTIP
		! Federal Tax
		REAL	FEDTAX_WH
		REAL	FED_WAGE
		REAL	SSTAX_WH
		REAL	SSTAX_WH_HI
		REAL	SS_WAGE
		REAL	SS_WAGE_HI
		REAL	SS_TIPS
		STRING	FED_REPNO = 20%
		! State Tax
		REAL	STATETAX_WH(7%)
		REAL	STATE_WAGE(7%)
		REAL	STATE_SDI(7%)
		STRING	STATE_ID(7%) = 2%
		STRING	STATE_REPNO(7%) = 20%
		! 401K
		REAL	K401A(5%)
		STRING	K401C(5%) = 1%
		! Local Taxes
		LONG LOCALCOUNT
		STRING LOCALNAME(7%) = 4%
		REAL LOCALWAGE(7%)
		REAL LOCALTAX(7%)
		! OTHER
		REAL	OTHER(5%)
		STRING	OTHERC(5%) = 3%
		REAL DEP_CARE
	END RECORD

	!
	! (0) = BLANK
	! (1) = EMPLOYEE
	! (2) = SUBTOTAL
	! (3) = GRAND TOTAL
	!
	MAP (EMP_W2_FORM) &
		W2_FORM_CDD W2_FORM(3%)

	MAP (W2_OTHER) &
		STATE%, &
		PAGE_COUNT%, &
		SUMMARY%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION OUTP_FORMINIT
	EXTERNAL LONG	FUNCTION OUTP_FORMPRINT
	EXTERNAL LONG	FUNCTION PR_FUNC_READTAXES
	EXTERNAL LONG   FUNCTION OUTP_INITFORM


	!
	! Declarations
	!
	DECLARE INTEGER CONSTANT LIB_INDEX = 200	! Size of the array
	DECLARE INTEGER CONSTANT FORM_HIGH = 2	! Forms per sheet
	DECLARE INTEGER CONSTANT FORM_LENGTH = 33	! Individual form length
	DECLARE INTEGER CONSTANT FORM_TOTAL = 42	! How often for subtotal
 !	DECLARE INTEGER CONSTANT FORM_TOTAL = 0		* How often for subtotal

	!
	! Dimension statements
	!
	DIM LIB_INDEX$(LIB_INDEX), RFA LIB_RFA(LIB_INDEX)

	%PAGE

	!
	! Constants
	!

	! Company
	W2_FORM(0%)::COMP_NAME	= ""
	W2_FORM(0%)::COMP_ADD1	= ""
	W2_FORM(0%)::COMP_ADD2	= ""
	W2_FORM(0%)::COMP_CITY	= ""
	W2_FORM(0%)::COMP_STATE	= ""
	W2_FORM(0%)::COMP_ZIP	= ""
	! Employee
	W2_FORM(0%)::EMP_NUM	= ""
	W2_FORM(0%)::EMP_NAME	= ""
	W2_FORM(0%)::EMP_ADD1	= ""
	W2_FORM(0%)::EMP_ADD2	= ""
	W2_FORM(0%)::EMP_CITY	= ""
	W2_FORM(0%)::EMP_STATE	= ""
	W2_FORM(0%)::EMP_ZIP	= ""
	W2_FORM(0%)::EMP_SSN	= ""
	! Flags
	W2_FORM(0%)::STAT_EMP	= ""
	W2_FORM(0%)::DECEASED	= ""
	W2_FORM(0%)::PEN_PLAN	= ""
	W2_FORM(0%)::SICK_PAY	= ""
	W2_FORM(0%)::LEGAL_REP	= ""
	W2_FORM(0%)::EMP_942	= ""
	W2_FORM(0%)::SUBTOTAL	= ""
	W2_FORM(0%)::DEF_COMP	= ""
	W2_FORM(0%)::VOID	= ""
	! Federal Tax
	W2_FORM(0%)::FED_REPNO	= ""
	! State Tax
	FOR I% = 0% TO 5%
		W2_FORM(0%)::STATE_ID(I%) = ""
		W2_FORM(0%)::STATE_REPNO(I%) = ""
	NEXT I%
	! 401K
	FOR I% = 0% TO 5%
		W2_FORM(0%)::K401C(I%)	= ""
	NEXT I%
	! Local Tax
	W2_FORM(0%)::LOCALCOUNT = 0%
	FOR I% = 0% TO 7%
		W2_FORM(0%)::LOCALNAME(I%) = ""
		W2_FORM(0%)::LOCALWAGE(I%) = 0.0
		W2_FORM(0%)::LOCALTAX(I%) = 0.0
	NEXT I%
	! OTHER
	FOR I% = 0% TO 5%
		W2_FORM(0%)::OTHERC(I%)	= ""
	NEXT I%
	DEP_CARE = 0.0

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	REPORT$ = "PRW2  "

	!
	! Look up device
	!
	CALL READ_DEVICE("PR_FORM", PR_FORM.DEV$, STAT%)

200	!*******************************************************************
	! Ask for W2 form name
	!*******************************************************************
	CALL LIBR_INDEX(PR_FORM.DEV$ + "PR_FORM", "W2*", LIB_INDEX$(), &
		LIB_RFA())

210	X% = ENTR_3CHOICE(SCOPE, "", "", LIB_INDEX$(), "", &
		8%, "W2 Form", "", 0%)

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	END SELECT

	IF X% > 0%
	THEN
		W2_FORM$ = LIB_INDEX$(X%)
	ELSE
		GOTO 210
	END IF

	!*******************************************************************
	! Initilize W2 form
	!*******************************************************************

	!
	! Get form from the PR form library
	!
	SMG_STATUS% = OUTP_FORMINIT(PR_FORM.DEV$ + "PR_FORM", W2_FORM$, &
		FORM_TEXT$, FORM_GROUP%, FORM_GROUP())

	!
	! Was there an error?
	!
	IF SMG_STATUS% <> 0%
	THEN
		CALL HELP_34MESSAGE(SCOPE, "W2 form is missing", &
			"E", SCOPE::PRG_PROGRAM, W2_FORM$, NUM1$(SMG_STATUS%))
		GOTO ExitProgram
	END IF

	!
	! Search for the desired parts of the form
	!
	FRM_BODY% = 0%

	FOR I% = 1% TO FORM_GROUP%

		SELECT FORM_GROUP(I%)::FGROUP

		CASE "FRM-BODY"
			FRM_BODY% = I%

		CASE "FRM-CODES"
			FRM_CODES$ = EDIT$(SEG$(FORM_TEXT$, &
				FORM_GROUP(I%)::POINTER, &
				FORM_GROUP(I% + 1%)::POINTER - 1%), 4% + 2%)

		END SELECT

	NEXT I%

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
	! Open Tax Profile file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.OPN"

		GET #PR_TAX_PROFILE.CH%, KEY #0% EQ "F  ", REGARDLESS
	USE
		FILENAME$ = "PR_TAX_PROFILE"
		CONTINUE HelpError
	END WHEN

	W2_FORM(0%)::FED_REPNO = PR_TAX_PROFILE_F::REPNO


350	!
	! Open earnings/deduction definition file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"
	USE
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

420	!
	! Open Company Profile
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.OPN"
		GET #UTL_PROFILE.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "UTL_PROFILE"
		CONTINUE HelpError
	END WHEN

425	!
	! Open LOCATION file, Set up company name as the default
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

	W2_FORM(0%)::COMP_NAME = &
		STRING$(LEN(UTL_LOCATION::LOCNAME), A"?"B)
	W2_FORM(0%)::COMP_ADD1 = ""
	W2_FORM(0%)::COMP_ADD2 = ""
	W2_FORM(0%)::COMP_CITY = &
		STRING$(LEN(UTL_LOCATION::CITY), A"?"B)
	W2_FORM(0%)::COMP_STATE = &
		STRING$(LEN(UTL_LOCATION::STATE), A"?"B)
	W2_FORM(0%)::COMP_ZIP = &
		STRING$(LEN(UTL_LOCATION::ZIP), A"?"B)

	WHEN ERROR IN
		GET #UTL_LOCATION.CH%, &
			KEY #0% EQ UTL_PROFILE::MAINLOCATION, &
			REGARDLESS
	USE
		CONTINUE 430 IF ERR = 155%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

	W2_FORM(0%)::COMP_NAME	= UTL_LOCATION::LOCNAME
	W2_FORM(0%)::COMP_ADD1	= UTL_LOCATION::ADDRESS1
	W2_FORM(0%)::COMP_ADD2	= UTL_LOCATION::ADDRESS2
	W2_FORM(0%)::COMP_CITY	= UTL_LOCATION::CITY
	W2_FORM(0%)::COMP_STATE	= UTL_LOCATION::STATE
	W2_FORM(0%)::COMP_ZIP	= UTL_LOCATION::ZIP

430	!
	! Open REPORT file
	!
	GOTO ExitProgram &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "014,09N") <> CMC$_NORMAL

	!
	! Set user variables
	!
	YYYY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	!++
	!
	! Abstract:FLD01
	!	^*(01) Year\*
	!	.b
	!	.lm +5
	!	Specifies which payroll year is to be printed.
	!	.lm -5
	!
	! Index:
	!	.x W-2 Forms>Year
	!	.x Year>W-2 Forms
	!
	!--

	QTR% = VAL%(TRM$(UTL_REPORTX::OPTDEF(1%)))

	IF QTR% <> 4%
	THEN
		QTRFLAG$ = "For the " + NUM1$(QTR%) + " quarter"
	ELSE
		QTRFLAG$ = ""
	END IF

	!++
	!
	! Abstract:FLD02
	!	^*(02) Quarter\*
	!	.b
	!	.lm +5
	!	Specifies which quarter is to be used as the last quarter
	!	for the forms.
	!	^*Note:\* This field should normally be *4,
	!	and should only be changed
	!	to a different value for debugging problems.
	!	.lm -5
	!
	! Index:
	!	.x W-2 Forms>Quarter
	!	.x Quarter>W-2 Forms
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)
	!++
	!
	! Abstract:FLD05
	!	.ts 55
	!	^*(05) Sort by	NU,SN,SO,LO\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field enters the code indicating the
	!	order in which the report will be printed.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.TE
	!	^*NU\*	Number
	!	.te
	!	^*SN\*	Social Security Number
	!	.te
	!	^*SO\*	Alphabetical
	!	.te
	!	^*LO\*	Location
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort by>W-2 Forms
	!	.x W-2 Forms>Sort by
	!
	!--

	SELECT_STATE$ = LEFT(UTL_REPORTX::OPTDEF(5%), 2%)
	!++
	!
	! Abstract:FLD06
	!	.ts 55
	!	^*(06) For State	2 Characters\*
	!	.b
	!	.lm +5
	!	The ^*For State\* field enters the state for which the
	!	form is being completed. Leaving this blank empty will cause all states
	!	to be printed.
	!	.b
	!	If you are running the Federal W2 forms, leave this
	!	field blank.
	!	.b
	!	Valid entries are the two character postal codes.
	!	.lm -5
	!
	! Index:
	!	.x W-2 Forms>For State
	!	.x For State>W-2 Forms
	!	.x State>W-2 Forms
	!
	!--

	ASK_EMP$ = LEFT(UTL_REPORTX::OPTDEF(9%), 1%)
	!++
	!
	! Abstract:FLD10
	!	.ts 55
	!	^*(10) Ask for Employee	Yes/No\*
	!	.b
	!	.lm +5
	!	This field is used to chose to selectively print individual W2
	!	forms by entering each employee number, or print
	!	all employees in the specified range.
	!	.b
	!	A "No" in this field will cause all selected W2 forms
	!	to print.
	!	A "Yes" in this field will cause the program to ask for
	!	each employee number to be printed.
	!	.lm -5
	!
	! Index:
	!	.x W-2 Forms>Ask For Employee
	!	.x Ask For Employee>W-2 Forms
	!
	!--

	SELECT SORTBY$

	CASE "NU"
		K_NUM% = 0%

	CASE "SO"
		K_NUM% = 2%

	CASE "LO"
		K_NUM% = 4%

	CASE ELSE	! (SN) is the default
		K_NUM% = 3%

	END SELECT

470	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.OPN"
		GET #PR_TAX_TABLE.CH%, KEY #0% EQ "F  ", REGARDLESS
	USE
		FILENAME$ = "PR_TAX_TABLE_" + YYYY$
		CONTINUE HelpError
	END WHEN

 !	FICA_LIMIT = PR_TAX_TABLE::FICA_LIMIT
 !	FICA_LIMIT_HI = PR_TAX_TABLE::FICA_LIMIT_HI
 !	FICA_RATE = PR_TAX_TABLE::FICA_EMPE_PCT / 100000.0

	FICA_EMPR_PCT = (PR_TAX_TABLE::FICA_EMPR_PCT) / 10000.0
	FICA_EMPE_PCT = (PR_TAX_TABLE::FICA_EMPE_PCT) / 10000.0
	FICA_LIMIT = PR_TAX_TABLE::FICA_LIMIT

	FICA_EMPR_PCT_HI = (PR_TAX_TABLE::FICA_EMPR_PCT_HI) / 10000.0
	FICA_EMPE_PCT_HI = (PR_TAX_TABLE::FICA_EMPE_PCT_HI) / 10000.0
	FICA_LIMIT_HI = PR_TAX_TABLE::FICA_LIMIT_HI

	!
	! Give the additional digit when possible
	!
	IF FICA_EMPR_PCT > 0.100
	THEN
		FICA_EMPR_PCT = FICA_EMPR_PCT / 10.0
		FICA_EMPE_PCT = FICA_EMPE_PCT / 10.0
		FICA_EMPR_PCT_HI = FICA_EMPR_PCT_HI / 10.0
		FICA_EMPE_PCT_HI = FICA_EMPE_PCT_HI / 10.0
	END IF


480	!
	! Open Earnings and Deduction register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.OPN"
	USE
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

490	!
	! Open Tax withholding register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.OPN"
	USE
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		CONTINUE HelpError
	END WHEN

	!
	! Restore original values for the help message
	!
	SCOPE::PRG_IDENT = TEMP_IDENT$
	SCOPE::PRG_PROGRAM = TEMP_PROGRAM$

	!
	! GOTO aligment routine
	!
	GOSUB Alignment

	%PAGE

2000	!*******************************************************************
	! Read through Payroll master file
	!*******************************************************************

	WHEN ERROR IN
		RESET #PR_EMP_MASTER.CH%, KEY #K_NUM%
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	TEST_42% = 0%
	FORM_COUNT = 0%
	UTL_REPORTX::LINENO = 0%

	W2_FORM(2%) = W2_FORM(0%)
	W2_FORM(3%) = W2_FORM(0%)

2010	!
	! Skip out if last page has been printed
	!
	GOTO ExitProgram &
		IF (PAGE_COUNT% > UTL_REPORTX::ENDP) AND (UTL_REPORTX::ENDP <> 0%)

	!
	! Grab next employee
	!
	IF ASK_EMP$ = "Y"
	THEN
		PR_EMP_MASTER::EMPNUM = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"", "Employee to print", &
			PR_EMP_MASTER::EMPNUM, 0%, "'E", "")

		GOTO ExitTotal IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F10 OR &
			SCOPE::SCOPE_EXIT = 26%

		!
		! Grab next employee
		!
		WHEN ERROR IN
			GET #PR_EMP_MASTER.CH%, &
				KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
				REGARDLESS
		USE
			CONTINUE ExitTotal IF ERR = 11%
			CONTINUE 2010 IF ERR = 155%
			FILENAME$ = "PR_EMP_MASTER"
			CONTINUE HelpError
		END WHEN
	ELSE
		WHEN ERROR IN
			GET #PR_EMP_MASTER.CH%, REGARDLESS
		USE
			CONTINUE ExitTotal IF ERR = 11%
			CONTINUE 2010 IF ERR = 155%
			FILENAME$ = "PR_EMP_MASTER"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Initialize W2 form values for employee
	!
	W2_FORM(1%) = W2_FORM(0%)
	STATE% = 0%
	EMP_K401% = 0%
	OTHER% = 0%

	W2_FORM(1%)::EMP_NUM	= PR_EMP_MASTER::EMPNUM
	W2_FORM(1%)::EMP_NAME	= PR_EMP_MASTER::EMPNAME
	W2_FORM(1%)::EMP_ADD1	= PR_EMP_MASTER::ADD1
	W2_FORM(1%)::EMP_ADD2	= PR_EMP_MASTER::ADD2
	W2_FORM(1%)::EMP_CITY	= PR_EMP_MASTER::CITY
	W2_FORM(1%)::EMP_STATE	= PR_EMP_MASTER::STATE
	W2_FORM(1%)::EMP_ZIP	= PR_EMP_MASTER::ZIP
	W2_FORM(1%)::EMP_SSN	= PRNT_SSN(PR_EMP_MASTER::SSN, 0%)

	X% = PR_FUNC_READTAXES(PR_EMP_MASTER::EMPNUM, &
		PR_REG_TAXES.CH%, &
		PR_TAXES%, &
		PR_TAXES())

	FOR LOOP% = 1% TO PR_TAXES%

		XTAX = 0.0
		XREPORTABLE = 0.0

		XTAX = XTAX + PR_TAXES(LOOP%)::TAX(I%) &
			FOR I% = 0% TO QTR% - 1%
		XREPORTABLE = XREPORTABLE + PR_TAXES(LOOP%)::REPORTABLE(I%) &
			FOR I% = 0% TO QTR% - 1%

		SELECT PR_TAXES(LOOP%)::TTYPE

		!***********************************************************
		! Look up federal wages and taxes
		!***********************************************************
		CASE "FW"

			W2_FORM(1%)::FEDTAX_WH = XTAX
			W2_FORM(1%)::FED_WAGE = XREPORTABLE

		!***********************************************************
		! Look up FICA wages and taxes
		!***********************************************************
 !		CASE "FI"
 !
 !			W2_FORM(1%)::SS_WAGE = XREPORTABLE
 !			W2_FORM(1%)::SS_WAGE = FICA_LIMIT &
 !				IF W2_FORM(1%)::SS_WAGE > FICA_LIMIT
 !
 !			W2_FORM(1%)::SS_WAGE_HI = XREPORTABLE
 !			W2_FORM(1%)::SS_WAGE_HI = FICA_LIMIT_HI &
 !				IF W2_FORM(1%)::SS_WAGE_HI > FICA_LIMIT_HI

			!
			! Split the medicare off of the social security
			!
 !			W2_FORM(1%)::SSTAX_WH = &
 !				FUNC_ROUND(W2_FORM(1%)::SS_WAGE * FICA_EMPE_PCT, 2%)
 !			W2_FORM(1%)::SSTAX_WH_HI = &
 !				XTAX - W2_FORM(1%)::SSTAX_WH


		!***********************************************************
		! Look up FICA (OASDI) wages and taxes
		!***********************************************************
		CASE "FI"

			W2_FORM(1%)::SS_WAGE = XREPORTABLE
			W2_FORM(1%)::SS_WAGE = FICA_LIMIT &
				IF W2_FORM(1%)::SS_WAGE > FICA_LIMIT
			W2_FORM(1%)::SSTAX_WH = W2_FORM(1%)::SSTAX_WH + XTAX


		!***********************************************************
		! Look up FICA (HI) wages and taxes
		!***********************************************************
		CASE "FH"

			W2_FORM(1%)::SS_WAGE_HI = XREPORTABLE
			W2_FORM(1%)::SS_WAGE_HI = FICA_LIMIT_HI &
				IF W2_FORM(1%)::SS_WAGE_HI > FICA_LIMIT_HI

			W2_FORM(1%)::SSTAX_WH_HI = &
				W2_FORM(1%)::SSTAX_WH_HI + XTAX


2130		!***********************************************************
		! Look up state wages and taxes
		!***********************************************************
		CASE "SW"
			STATE% = STATE% + 1%

			W2_FORM(1%)::STATETAX_WH(STATE%) = XTAX
			W2_FORM(1%)::STATE_ID(STATE%) = PR_TAXES(LOOP%)::CODE
			W2_FORM(1%)::STATE_ID(STATE%) = &
				PR_EMP_MASTER::STATE &
				IF PR_TAXES(LOOP%)::CODE = ""
			W2_FORM(1%)::STATE_WAGE(STATE%) = XREPORTABLE
			W2_FORM(1%)::STATE_SDI(STATE%) = 0.0

			WHEN ERROR IN
				GET #PR_TAX_PROFILE.CH%, &
					KEY #0% EQ "S" + PR_TAXES(LOOP%)::CODE, &
					REGARDLESS
			USE
				CONTINUE 2140 IF ERR = 155%
				FILENAME$ = "PR_TAX_PROFILE"
				CONTINUE HelpError
			END WHEN

			W2_FORM(1%)::STATE_REPNO(STATE%) = PR_TAX_PROFILE_S::REPNO


		!***********************************************************
		! Look up state wages and taxes
		!***********************************************************
		CASE "CW"

			W2_FORM(1%)::LOCALCOUNT = W2_FORM(1%)::LOCALCOUNT + 1%
			W2_FORM(1%)::LOCALNAME(W2_FORM(1%)::LOCALCOUNT) = &
				PR_TAXES(LOOP%)::CODE
			W2_FORM(1%)::LOCALTAX(W2_FORM(1%)::LOCALCOUNT) = XTAX
			W2_FORM(1%)::LOCALWAGE(W2_FORM(1%)::LOCALCOUNT) = XREPORTABLE


		!***********************************************************
		! Look up State Disability Insurance (SX)
		!***********************************************************
		CASE "SX"

			SELECT_STATE% = 0%
			SELECT_STATE% = TEMP% &
				IF W2_FORM(1%)::STATE_ID(TEMP%) = &
				PR_TAXES(LOOP%)::CODE &
				FOR TEMP% = 1% TO STATE%

			IF SELECT_STATE% = 0%
			THEN
				STATE%, SELECT_STATE% = STATE% + 1%
			END IF

			W2_FORM(1%)::STATE_ID(SELECT_STATE%) = &
				PR_TAXES(LOOP%)::CODE
			W2_FORM(1%)::STATE_SDI(SELECT_STATE%) = &
				FUNC_ROUND(W2_FORM(1%)::STATE_SDI(SELECT_STATE%) + &
				XTAX, 2%)

			OTHER% = OTHER% + 1%
			W2_FORM(1%)::OTHER(OTHER%) = &
				FUNC_ROUND(XTAX, 2%)
			W2_FORM(1%)::OTHERC(OTHER%) = "SDI"

		!***********************************************************
		! Look up State Disability Insurance (SI)
		!***********************************************************
		CASE "SI"

			SELECT_STATE% = 1%
			SELECT_STATE% = TEMP% &
				IF W2_FORM(1%)::STATE_ID(TEMP%) = &
				PR_TAXES(LOOP%)::CODE &
				FOR TEMP% = 1% TO STATE%

			W2_FORM(1%)::STATE_ID(SELECT_STATE%) = &
				PR_TAXES(LOOP%)::CODE
			W2_FORM(1%)::STATE_SDI(SELECT_STATE%) = &
				FUNC_ROUND(W2_FORM(1%)::STATE_SDI(SELECT_STATE%) + &
				XTAX, 2%)

		END SELECT


2140	NEXT LOOP%

	IF (W2_FORM(1%)::SS_WAGE_HI = 0.0) AND &
		(W2_FORM(1%)::SS_WAGE <> 0.0)
	THEN
		!
		! Hope they don't go over the limit (aaarrrggghhh)
		!
		W2_FORM(1%)::SS_WAGE_HI = W2_FORM(1%)::SS_WAGE

		!
		! Split the medicare off of the social security
		!
		SSTAXX = W2_FORM(1%)::SSTAX_WH + W2_FORM(1%)::SSTAX_WH_HI
		W2_FORM(1%)::SSTAX_WH = &
			FUNC_ROUND(W2_FORM(1%)::SS_WAGE * FICA_EMPE_PCT, 2%)
		W2_FORM(1%)::SSTAX_WH_HI = &
			SSTAXX - W2_FORM(1%)::SSTAX_WH
	END IF

	IF (W2_FORM(1%)::SSTAX_WH <> 0.0) AND &
		(W2_FORM(1%)::SSTAX_WH_HI = 0.0)
	THEN
		!
		! Split the medicare off of the social security
		!
		SSTAXX = W2_FORM(1%)::SSTAX_WH + W2_FORM(1%)::SSTAX_WH_HI
		W2_FORM(1%)::SSTAX_WH = &
			FUNC_ROUND(W2_FORM(1%)::SS_WAGE * FICA_EMPE_PCT, 2%)
		W2_FORM(1%)::SSTAX_WH_HI = &
			SSTAXX - W2_FORM(1%)::SSTAX_WH
	END IF

2200	!*******************************************************************
	! Scan through PR_REG_ERNDED file for anything to put in box
	! 16A (if selected)
	!*******************************************************************

	WHEN ERROR IN
		FIND #PR_REG_ERNDED.CH%, &
			KEY #0% GE PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 2300
	END WHEN

2210	WHEN ERROR IN
		GET #PR_REG_ERNDED.CH%, REGARDLESS
	USE
		CONTINUE 2300
	END WHEN

	GOTO 2300 IF PR_REG_ERNDED::EMPNUM <> PR_EMP_MASTER::EMPNUM

	GOTO 2210 IF PR_REG_ERNDED::ETYPE = "A"

2220	!
	! See if the location is defined in the ernded file
	!
	WHEN ERROR IN
		GET #PR_ERNDED_DEF.CH%, &
			KEY #0% EQ PR_REG_ERNDED::ETYPE + PR_REG_ERNDED::CODE, &
			REGARDLESS
	USE
		CONTINUE 2230
	END WHEN

	W2LOCATION$ = PR_ERNDED_DEF::W2LOCATION

	GOTO 2225 IF EDIT$(W2LOCATION$, 4% + 128%) <> ""

2223	I% = INSTR(1%, FRM_CODES$, &
		PR_REG_ERNDED::ETYPE + "-" + PR_REG_ERNDED::CODE)
	I% = 0% IF PR_REG_ERNDED::ETYPE = ""

	IF I%
	THEN
		I1% = INSTR(I%, FRM_CODES$, ":")
		I2% = INSTR(I1%, FRM_CODES$ + ",", ",")

		W2LOCATION$ = SEG$(FRM_CODES$, I1% + 1%, I2% - 1%)
	ELSE
		GOTO 2230
	END IF

2225	IF EDIT$(W2LOCATION$, 4%) <> ""
	THEN
		!
		! Calculate total amount
		!
		! The sign is calculated so that deductions will show as
		! positive numbers (since most of these are deductions)
		! and pay comes through as negative.
		!
		IF (PR_REG_ERNDED::ETYPE = "P")
		THEN
			SIGN = -1.0
		ELSE
			SIGN = 1.0
		END IF

		AMOUNT = 0.0
		AMOUNT = AMOUNT + &
			SIGN * PR_REG_ERNDED::QTR_DOLL(I%) &
			FOR I% = 0% TO QTR% - 1%

		SELECT W2LOCATION$

		CASE "AE"		! Advanced earned income credit

			W2_FORM(1%)::AD_EIC = &
				FUNC_ROUND(W2_FORM(1%)::AD_EIC + &
				AMOUNT, 2%)

		CASE "AT"		! Allocated tips

			W2_FORM(1%)::ALL_TIPS = &
				FUNC_ROUND(W2_FORM(1%)::ALL_TIPS + &
				AMOUNT, 2%)

		CASE "CD"

			W2_FORM(1%)::DEP_CARE = &
				FUNC_ROUND(W2_FORM(1%)::DEP_CARE + &
				AMOUNT, 2%)

		CASE "DA" TO "DZ"	! Random information (401k's)

			FOR I% = 1% TO EMP_K401%
				IF W2_FORM(1%)::K401C(I%) = &
					MID(W2LOCATION$, 2%, 1%)
				THEN
					W2_FORM(1%)::K401A(I%) = &
						W2_FORM(1%)::K401A(I%) + &
						FUNC_ROUND(AMOUNT, 2%)
					GOTO 2210
				END IF
			NEXT I%

			EMP_K401% = EMP_K401% + 1%
			W2_FORM(1%)::K401A(EMP_K401%) = &
				FUNC_ROUND(AMOUNT, 2%)
			W2_FORM(1%)::K401C(EMP_K401%) = &
				MID(W2LOCATION$, 2%, 1%)

		CASE "FI"		! FICA Disibility Payment
			W2_FORM(1%)::SSTAX_WH = &
				FUNC_ROUND(W2_FORM(1%)::SSTAX_WH + AMOUNT, 2%)

		CASE "FR"		! Fringe Benefits

			W2_FORM(1%)::FRINGE_BENEFIT = &
				FUNC_ROUND(W2_FORM(1%)::FRINGE_BENEFIT + &
				AMOUNT, 2%)

		CASE "LO" TO "LOZZ"

			W2_FORM(1%)::LOCALCOUNT = W2_FORM(1%)::LOCALCOUNT + 1%
			W2_FORM(1%)::LOCALNAME(W2_FORM(1%)::LOCALCOUNT) = &
				RIGHT(W2LOCATION$, 3%)
			W2_FORM(1%)::LOCALTAX(W2_FORM(1%)::LOCALCOUNT) = AMOUNT

		CASE "NQ"		! Nonqualified plans

			W2_FORM(1%)::NONQUAL = &
				FUNC_ROUND(W2_FORM(1%)::NONQUAL + &
				AMOUNT, 2%)

		CASE "OT" TO "OTZZZZ"		! Other

			OTHER% = OTHER% + 1%
			W2_FORM(1%)::OTHER(OTHER%) = &
				FUNC_ROUND(AMOUNT, 2%)
			W2_FORM(1%)::OTHERC(OTHER%) = &
				MID(W2LOCATION$, 3%, 2%)

		CASE "QU"		! Qualified plans

			W2_FORM(1%)::QUAL = &
				FUNC_ROUND(W2_FORM(1%)::QUAL + &
				AMOUNT, 2%)

		CASE "ST"		! Social Security Tips

			W2_FORM(1%)::SOCTIP = &
				FUNC_ROUND(W2_FORM(1%)::SOCTIP + &
				AMOUNT, 2%)

		END SELECT

		GOTO 2210
	END IF

2230	GOTO 2210

2300	!*****************************************************************
	! Print form
	!*****************************************************************

	!
	! Hack to make local wage work
	!
 !	FOR I% = 1% TO 2%
 !		IF (W2_FORM(1%)::LOCALTAX(I%) <> 0.0)
 !		THEN
 !			W2_FORM(1%)::LOCALWAGE(I%) = W2_FORM(1%)::FED_WAGE
 !		END IF
 !	NEXT I%

	!
	! Is this employee in the correct state?
	!
	IF SELECT_STATE$ <> ""
	THEN
		SELECT_STATE% = 0%
		SELECT_STATE% = I% &
			IF W2_FORM(1%)::STATE_ID(I%) = SELECT_STATE$ &
			FOR I% = 1% TO STATE%

		!
		! Swap the top state with this one if necessary
		!
		IF SELECT_STATE% > 1%
		THEN
			TEMP = W2_FORM(1%)::STATETAX_WH(1%)
			W2_FORM(1%)::STATETAX_WH(1%) = &
				W2_FORM(1%)::STATETAX_WH(SELECT_STATE%)
			W2_FORM(1%)::STATETAX_WH(SELECT_STATE%) = TEMP

			TEMP$ = W2_FORM(1%)::STATE_ID(1%)
			W2_FORM(1%)::STATE_ID(1%) = &
				W2_FORM(1%)::STATE_ID(SELECT_STATE%)
			W2_FORM(1%)::STATE_ID(SELECT_STATE%) = TEMP$

			TEMP = W2_FORM(1%)::STATE_WAGE(1%)
			W2_FORM(1%)::STATE_WAGE(1%) = &
				W2_FORM(1%)::STATE_WAGE(SELECT_STATE%)
			W2_FORM(1%)::STATE_WAGE(SELECT_STATE%) = TEMP
		END IF
	ELSE
		SELECT_STATE% = -1%
	END IF

	!
	! Any reason to print this form?
	!
	SUM = W2_FORM(1%)::FED_WAGE + &
		W2_FORM(1%)::STATE_WAGE(1%) + &
		W2_FORM(1%)::SS_WAGE + &
		W2_FORM(1%)::FEDTAX_WH + &
		W2_FORM(1%)::STATETAX_WH(1%)

	IF (SUM <> 0.0) AND (SELECT_STATE% <> 0%)
	THEN
		!
		! Print Employee W2 form
		!
		EMP_COUNT% = EMP_COUNT% + 1%
		SUMMARY% = 1%
		GOSUB SetValue

		GOTO ExitProgram IF UTL_REPORTX::STAT

		!
		! Total W2 forms
		!
		SUMMARY% = 2%		! Subtotal
		GOSUB SummarizeEmp

		SUMMARY% = 3%		! Total
		GOSUB SummarizeEmp

		!
		! Check to see if 42nd form
		!
		TEST_42% = TEST_42% + 1%
		IF (TEST_42% >= FORM_TOTAL - 1%) AND (FORM_TOTAL <> 0%)
		THEN
			GOSUB Subtotal
		END IF
	END IF

2990	GOTO 2010

	%Page

 SummarizeEmp:
	!*******************************************************************
	! Subtotal/Total employee information
	!*******************************************************************

	! Other

	W2_FORM(SUMMARY%)::ALL_TIPS = &
		FUNC_ROUND(W2_FORM(SUMMARY%)::ALL_TIPS + &
		W2_FORM(1%)::ALL_TIPS, 2%)
	W2_FORM(SUMMARY%)::AD_EIC = &
		FUNC_ROUND(W2_FORM(SUMMARY%)::AD_EIC + &
		W2_FORM(1%)::AD_EIC, 2%)
	W2_FORM(SUMMARY%)::DEP_CARE = &
		FUNC_ROUND(W2_FORM(SUMMARY%)::DEP_CARE + &
		W2_FORM(1%)::DEP_CARE, 2%)
	W2_FORM(SUMMARY%)::FRINGE_BENEFIT = &
		FUNC_ROUND(W2_FORM(SUMMARY%)::FRINGE_BENEFIT + &
		W2_FORM(1%)::FRINGE_BENEFIT, 2%)

		! Federal Tax
	W2_FORM(SUMMARY%)::FEDTAX_WH = &
		FUNC_ROUND(W2_FORM(SUMMARY%)::FEDTAX_WH + &
		W2_FORM(1%)::FEDTAX_WH, 2%)
	W2_FORM(SUMMARY%)::FED_WAGE = &
		FUNC_ROUND(W2_FORM(SUMMARY%)::FED_WAGE + &
		W2_FORM(1%)::FED_WAGE, 2%)
	W2_FORM(SUMMARY%)::SSTAX_WH = &
		FUNC_ROUND(W2_FORM(SUMMARY%)::SSTAX_WH + &
		W2_FORM(1%)::SSTAX_WH, 2%)
	W2_FORM(SUMMARY%)::SSTAX_WH_HI = &
		FUNC_ROUND(W2_FORM(SUMMARY%)::SSTAX_WH_HI + &
		W2_FORM(1%)::SSTAX_WH_HI, 2%)
	W2_FORM(SUMMARY%)::SS_WAGE = &
		FUNC_ROUND(W2_FORM(SUMMARY%)::SS_WAGE + &
		W2_FORM(1%)::SS_WAGE, 2%)
	W2_FORM(SUMMARY%)::SS_WAGE_HI = &
		FUNC_ROUND(W2_FORM(SUMMARY%)::SS_WAGE_HI + &
		W2_FORM(1%)::SS_WAGE_HI, 2%)
	W2_FORM(SUMMARY%)::SS_TIPS = &
		FUNC_ROUND(W2_FORM(SUMMARY%)::SS_TIPS + &
		W2_FORM(1%)::SS_TIPS, 2%)

	! State Tax
	FOR I% = 1% TO 5%
		J% = 0%
		J% = K% IF (W2_FORM(SUMMARY%)::STATE_ID(K%) = &
			W2_FORM(1%)::STATE_ID(I%)) OR &
			((W2_FORM(SUMMARY%)::STATE_ID(K%) = "") AND (J% = 0%)) &
			FOR K% = 0% TO 5%
		J% = 5% IF J% = 0%

		W2_FORM(SUMMARY%)::STATETAX_WH(J%) = &
			FUNC_ROUND(W2_FORM(SUMMARY%)::STATETAX_WH(J%) + &
			W2_FORM(1%)::STATETAX_WH(I%), 2%)
		W2_FORM(SUMMARY%)::STATE_WAGE(J%) = &
			FUNC_ROUND(W2_FORM(SUMMARY%)::STATE_WAGE(J%) + &
			W2_FORM(1%)::STATE_WAGE(I%), 2%)
		W2_FORM(SUMMARY%)::STATE_SDI(J%) = &
			FUNC_ROUND(W2_FORM(SUMMARY%)::STATE_SDI(J%) + &
			W2_FORM(1%)::STATE_SDI(I%), 2%)
		W2_FORM(SUMMARY%)::STATE_ID(J%) = &
			W2_FORM(1%)::STATE_ID(I%)
		W2_FORM(SUMMARY%)::STATE_REPNO(J%) = &
			W2_FORM(1%)::STATE_REPNO(I%)
	NEXT I%

	! Local Tax
	FOR I% = 1% TO W2_FORM(1%)::LOCALCOUNT

		FOR J% = 1% TO W2_FORM(SUMMARY%)::LOCALCOUNT

			IF W2_FORM(1%)::LOCALNAME(I%) = &
				W2_FORM(SUMMARY%)::LOCALNAME(J%)
			THEN
				W2_FORM(SUMMARY%)::LOCALWAGE(J%) = &
					W2_FORM(SUMMARY%)::LOCALWAGE(J%) + &
					W2_FORM(1%)::LOCALWAGE(I%)

				W2_FORM(SUMMARY%)::LOCALTAX(J%) = &
					W2_FORM(SUMMARY%)::LOCALTAX(J%) + &
					W2_FORM(1%)::LOCALTAX(I%)

				GOTO EndLocal
			END IF
		NEXT J%

		J%, W2_FORM(SUMMARY%)::LOCALCOUNT = &
			W2_FORM(SUMMARY%)::LOCALCOUNT + 1%

		W2_FORM(SUMMARY%)::LOCALNAME(J%) = &
			W2_FORM(1%)::LOCALNAME(I%)

		W2_FORM(SUMMARY%)::LOCALWAGE(J%) = &
			W2_FORM(1%)::LOCALWAGE(I%)

		W2_FORM(SUMMARY%)::LOCALTAX(J%) = &
			W2_FORM(1%)::LOCALTAX(I%)

 EndLocal:
	NEXT I%

	!
	! 401K Info
	!
	FOR I% = 1% TO 5%
		J% = 0%
		J% = K% IF (W2_FORM(SUMMARY%)::K401C(K%) = &
			W2_FORM(1%)::K401C(I%)) OR &
			((W2_FORM(SUMMARY%)::K401C(K%) = "") AND (J% = 0%)) &
			FOR K% = 1% TO 5%
		J% = 5% IF J% = 0%

		W2_FORM(SUMMARY%)::K401A(J%) = &
			FUNC_ROUND(W2_FORM(SUMMARY%)::K401A(J%) + &
			W2_FORM(1%)::K401A(I%), 2%)
		W2_FORM(SUMMARY%)::K401C(J%) = &
			W2_FORM(1%)::K401C(I%)
	NEXT I%

	!
	! OTHER
	!
	FOR I% = 1% TO 5%
		J% = 0%
		J% = K% IF (W2_FORM(SUMMARY%)::OTHERC(K%) = &
			W2_FORM(1%)::OTHERC(I%)) OR &
			((W2_FORM(SUMMARY%)::OTHERC(K%) = "") AND (J% = 0%)) &
			FOR K% = 1% TO 5%
		J% = 5% IF J% = 0%

		W2_FORM(SUMMARY%)::OTHER(J%) = &
			FUNC_ROUND(W2_FORM(SUMMARY%)::OTHER(J%) + &
			W2_FORM(1%)::OTHER(I%), 2%)
		W2_FORM(SUMMARY%)::OTHERC(J%) = &
			W2_FORM(1%)::OTHERC(I%)
	NEXT I%

	RETURN

 Subtotal:
	!*******************************************************************
	! Print subtotal form
	!*******************************************************************

	SUMMARY% = 2%

	!
	! Set employee name to total
	!
	W2_FORM(2%)::EMP_NUM	= ""
	W2_FORM(2%)::EMP_NAME	= "*********************"
	W2_FORM(2%)::EMP_ADD1	= "*****  Subtotal  ****"
	W2_FORM(2%)::EMP_ADD2	= "*********************"
	W2_FORM(2%)::EMP_CITY	= QTRFLAG$
	W2_FORM(2%)::EMP_STATE	= ""
	W2_FORM(2%)::EMP_ZIP	= ""
	W2_FORM(2%)::EMP_SSN	= ""
	W2_FORM(2%)::SUBTOTAL	= "X"

	GOSUB SetValue

	GOTO ExitProgram IF UTL_REPORTX::STAT

	W2_FORM(2%) = W2_FORM(0%)

	TEST_42% = 0%

	RETURN

	%PAGE

 ExitTotal:
	!*********************************************************************
	! Print Grand total
	!*********************************************************************
	IF (TEST_42% > 0%) AND (FORM_TOTAL <> 0%)
	THEN
		!
		! Print to bottom of form
		!
		FORM% = FORM_COUNT - (INT(FORM_COUNT/FORM_HIGH) * FORM_HIGH)
		FORM% = FORM_HIGH IF FORM% = 0%

		FORM_COUNT = FORM_COUNT + FORM% - 1%

		!
		! Page counter
		!
		PAGE_COUNT% = INT(FORM_COUNT/FORM_HIGH) + 1%

		FORM% = (FORM% - 1%) * FORM_LENGTH

		CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
			FOR LOOP% = 1% TO FORM%

		GOSUB Subtotal
	END IF

	!******************************************************************
	! Print grand total
	!******************************************************************

	SUMMARY% = 3%

	!
	! Set employee name to total
	!
	W2_FORM(3%)::EMP_NUM	= ""
	W2_FORM(3%)::EMP_NAME	= "*********************"
	W2_FORM(3%)::EMP_ADD1	= "Total Employees " + NUM1$(EMP_COUNT%)
	W2_FORM(3%)::EMP_ADD2	= "*********************"
	W2_FORM(3%)::EMP_CITY	= QTRFLAG$
	W2_FORM(3%)::EMP_STATE	= ""
	W2_FORM(3%)::EMP_ZIP	= ""
	W2_FORM(3%)::EMP_SSN	= ""

	GOSUB SetValue

	CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
		FOR LOOP% = 1% TO FORM_LENGTH * (FORM_HIGH - 1%)

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

	SCOPE::PRG_ITEM = "CONFIRM"
	!++
	! Abstract:CONFIRM
	!
	!
	! Index:
	!
	!
	!--
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

	FOR I% = 1% TO FORM_HIGH
		!
		! Print the body of the form
		!
		TEST_COUNT% = OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_BODY%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			1%)

	NEXT I%

	!
	! Do they need another?
	!
	GOTO Alignment

 AlignmentReturn:
	RETURN

	%Page

 SetValue:
	!********************************************************************
	! Set values and print form
	!********************************************************************

	!
	! Page counter
	!
	PAGE_COUNT% = INT(FORM_COUNT/FORM_HIGH) + 1%

	!
	! Print the body of the Form
	!
	IF (PAGE_COUNT% >= UTL_REPORTX::STARTP) AND &
		((PAGE_COUNT% <= UTL_REPORTX::ENDP) OR (UTL_REPORTX::ENDP = 0%))
	THEN
		TEST_COUNT% = OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_BODY%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			0%)
	END IF

	!
	! Increment form counter
	!
	FORM_COUNT = FORM_COUNT + 1%

	RETURN

	%Page

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

	END

20000	SUB FORM_LOADVAR(VARNAME$, REALVALUE, TEXTVALUE$)

	!*******************************************************************
	! This function is used to return the value of a field back to the
	! form printer.
	!*******************************************************************

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Define maps
	!
	RECORD W2_FORM_CDD
		! Company
		STRING	COMP_NAME = 40%
		STRING	COMP_ADD1 = 40%
		STRING	COMP_ADD2 = 40%
		STRING	COMP_CITY = 20%
		STRING	COMP_STATE = 2%
		STRING	COMP_ZIP = 10%
		! Employee
		STRING	EMP_NUM = 10%
		STRING	EMP_NAME = 40%
		STRING	EMP_ADD1 = 40%
		STRING	EMP_ADD2 = 40%
		STRING	EMP_CITY = 20%
		STRING	EMP_STATE = 2%
		STRING	EMP_ZIP = 10%
		STRING	EMP_SSN = 11%
		! Flags
		STRING	STAT_EMP = 1%
		STRING	DECEASED = 1%
		STRING	PEN_PLAN = 1%
		STRING	SICK_PAY = 1%
		STRING	LEGAL_REP = 1%
		STRING	EMP_942 = 1%
		STRING	SUBTOTAL = 1%
		STRING	DEF_COMP = 1%
		STRING	VOID = 1%
		! Other
		REAL	ALL_TIPS
		REAL	AD_EIC
		REAL	FRINGE_BENEFIT
		REAL	QUAL
		REAL	NONQUAL
		REAL	SOCTIP
		! Federal Tax
		REAL	FEDTAX_WH
		REAL	FED_WAGE
		REAL	SSTAX_WH
		REAL	SSTAX_WH_HI
		REAL	SS_WAGE
		REAL	SS_WAGE_HI
		REAL	SS_TIPS
		STRING	FED_REPNO = 20%
		! State Tax
		REAL	STATETAX_WH(7%)
		REAL	STATE_WAGE(7%)
		REAL	STATE_SDI(7%)
		STRING	STATE_ID(7%) = 2%
		STRING	STATE_REPNO(7%) = 20%
		! 401K
		REAL	K401A(5%)
		STRING	K401C(5%) = 1%
		! Local Taxes
		LONG LOCALCOUNT
		STRING LOCALNAME(7%) = 4%
		REAL LOCALWAGE(7%)
		REAL LOCALTAX(7%)
		! OTHER
		REAL	OTHER(5%)
		STRING	OTHERC(5%) = 3%
		REAL	DEP_CARE
	END RECORD

	MAP (W2_OTHER) &
		STATE%, &
		PAGE_COUNT%, &
		SUMMARY%

	!
	! (0) = BLANK
	! (1) = EMPLOYEE
	! (2) = SUBTOTAL
	! (3) = GRAND TOTAL
	!
	MAP (EMP_W2_FORM) &
		W2_FORM_CDD W2_FORM(3%)

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

	!*************************************************************
	! Company
	!*************************************************************
	CASE "COMP_NAME"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::COMP_NAME

	CASE "COMP_ADD1"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::COMP_ADD1

	CASE "COMP_ADD2"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::COMP_ADD2

	CASE "COMP_CITY"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::COMP_CITY

	CASE "COMP_STATE"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::COMP_STATE

	CASE "COMP_ZIP"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::COMP_ZIP

	!************************************************************
	! Employee
	!************************************************************
	CASE "EMP_NUM"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::EMP_NUM

	CASE "EMP_NAME"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::EMP_NAME

	CASE "EMP_ADD1"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::EMP_ADD1

	CASE "EMP_ADD2"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::EMP_ADD2

	CASE "EMP_CITY"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::EMP_CITY

	CASE "EMP_STATE"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::EMP_STATE

	CASE "EMP_ZIP"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::EMP_ZIP

	CASE "EMP_SSN"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::EMP_SSN

	!************************************************************
	! Flags
	!************************************************************
	CASE "STAT_EMP"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::STAT_EMP

	CASE "DECEASED"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::DECEASED

	CASE "PEN_PLAN"
		! Gets an X if have a pension plan (401k, ...)
		TEXTVALUE$ = " "
		TEXTVALUE$ = "X" &
			IF INSTR(1%, " DEFGH", &
			W2_FORM(SUMMARY%)::K401C(I%)) > 1% &
			FOR I% = 1% TO 5%

	CASE "SICK_PAY"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::SICK_PAY

	CASE "LEGAL_REP"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::LEGAL_REP

	CASE "EMP_942"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::EMP_942

	CASE "SUBTOTAL"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::SUBTOTAL

	CASE "DEF_COMP"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::DEF_COMP

	CASE "VOID"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::VOID

	!************************************************************
	! Other
	!************************************************************
	CASE "ALL_TIPS"
		REALVALUE = W2_FORM(SUMMARY%)::ALL_TIPS

	CASE "AD_EIC"
		REALVALUE = W2_FORM(SUMMARY%)::AD_EIC

	CASE "DEP_CARE", "DUMMY_DEPNT"
		REALVALUE = W2_FORM(SUMMARY%)::DEP_CARE

	CASE "FRINGE_BENEFIT"
		REALVALUE = W2_FORM(SUMMARY%)::FRINGE_BENEFIT

	CASE "NONQUAL", "DUMMY_PLAN"
		REALVALUE = W2_FORM(SUMMARY%)::NONQUAL

	CASE "QUAL"
		REALVALUE = W2_FORM(SUMMARY%)::QUAL

	CASE "SOCTIP"
		REALVALUE = W2_FORM(SUMMARY%)::SOCTIP

	CASE "PAGE_COUNT"
		REALVALUE = PAGE_COUNT%

	!************************************************************
	! Federal tax
	!************************************************************
	CASE "FEDTAX_WH"
		REALVALUE = W2_FORM(SUMMARY%)::FEDTAX_WH

	CASE "FED_WAGE"
		REALVALUE = W2_FORM(SUMMARY%)::FED_WAGE

	CASE "SSTAX_WH"
		REALVALUE = W2_FORM(SUMMARY%)::SSTAX_WH

	CASE "SSTAX_WH_HI"
		REALVALUE = W2_FORM(SUMMARY%)::SSTAX_WH_HI

	CASE "SS_WAGE"
		REALVALUE = W2_FORM(SUMMARY%)::SS_WAGE

	CASE "SS_WAGE_HI"
		REALVALUE = W2_FORM(SUMMARY%)::SS_WAGE_HI

	CASE "SS_TIPS"
		REALVALUE = W2_FORM(SUMMARY%)::SS_TIPS

	CASE "FED_REPNO"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::FED_REPNO

	!************************************************************
	! State tax
	!************************************************************
	CASE "STATETAX_WH"
		REALVALUE = W2_FORM(SUMMARY%)::STATETAX_WH(1%)
	CASE "STATETAX2_WH"
		REALVALUE = W2_FORM(SUMMARY%)::STATETAX_WH(2%)
	CASE "STATETAX3_WH"
		REALVALUE = W2_FORM(SUMMARY%)::STATETAX_WH(3%)
	CASE "STATETAX4_WH"
		REALVALUE = W2_FORM(SUMMARY%)::STATETAX_WH(4%)
	CASE "STATETAX5_WH"
		REALVALUE = W2_FORM(SUMMARY%)::STATETAX_WH(5%)

	CASE "STATE_WAGE"
		REALVALUE = W2_FORM(SUMMARY%)::STATE_WAGE(1%)
	CASE "STATE2_WAGE"
		REALVALUE = W2_FORM(SUMMARY%)::STATE_WAGE(2%)
	CASE "STATE3_WAGE"
		REALVALUE = W2_FORM(SUMMARY%)::STATE_WAGE(3%)
	CASE "STATE4_WAGE"
		REALVALUE = W2_FORM(SUMMARY%)::STATE_WAGE(4%)
	CASE "STATE5_WAGE"
		REALVALUE = W2_FORM(SUMMARY%)::STATE_WAGE(5%)

	CASE "STATE_ID"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::STATE_ID(1%)
	CASE "STATE2_ID"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::STATE_ID(2%)
	CASE "STATE3_ID"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::STATE_ID(3%)
	CASE "STATE4_ID"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::STATE_ID(4%)
	CASE "STATE5_ID"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::STATE_ID(5%)

	CASE "STATE_SDI"
		REALVALUE = W2_FORM(SUMMARY%)::STATE_SDI(1%)
	CASE "STATE2_SDI"
		REALVALUE = W2_FORM(SUMMARY%)::STATE_SDI(2%)
	CASE "STATE3_SDI"
		REALVALUE = W2_FORM(SUMMARY%)::STATE_SDI(3%)
	CASE "STATE4_SDI"
		REALVALUE = W2_FORM(SUMMARY%)::STATE_SDI(4%)
	CASE "STATE5_SDI"
		REALVALUE = W2_FORM(SUMMARY%)::STATE_SDI(5%)

	CASE "STATE_REPNO"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::STATE_REPNO(1%)
	CASE "STATE2_REPNO"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::STATE_REPNO(2%)
	CASE "STATE3_REPNO"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::STATE_REPNO(3%)
	CASE "STATE4_REPNO"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::STATE_REPNO(4%)
	CASE "STATE5_REPNO"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::STATE_REPNO(5%)

	CASE "LOCAL_NAME"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::LOCALNAME(1%)
	CASE "LOCAL2_NAME"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::LOCALNAME(2%)
	CASE "LOCAL3_NAME"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::LOCALNAME(3%)

	CASE "LOCAL_WAGE"
		REALVALUE = W2_FORM(SUMMARY%)::LOCALWAGE(1%)
 !
 ! Use montanna wage
 !
 !		REALVALUE = W2_FORM(SUMMARY%)::STATE_WAGE(I%) &
 !			IF W2_FORM(SUMMARY%)::STATE_ID(I%) = "MT" AND &
 !			W2_FORM(SUMMARY%)::LOCALTAX(1%) <> 0.0 &
 !			FOR I% = 1% TO 5%

	CASE "LOCAL2_WAGE"
		REALVALUE = W2_FORM(SUMMARY%)::LOCALWAGE(2%)
	CASE "LOCAL3_WAGE"
		REALVALUE = W2_FORM(SUMMARY%)::LOCALWAGE(3%)

	CASE "LOCAL_TAX"
		REALVALUE = W2_FORM(SUMMARY%)::LOCALTAX(1%)
	CASE "LOCAL2_TAX"
		REALVALUE = W2_FORM(SUMMARY%)::LOCALTAX(2%)
	CASE "LOCAL3_TAX"
		REALVALUE = W2_FORM(SUMMARY%)::LOCALTAX(3%)

	!***********************************************************
	! 401K
	!***********************************************************
	CASE "K401C1"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::K401C(1%)
	CASE "K401A1"
		REALVALUE = W2_FORM(SUMMARY%)::K401A(1%)
	CASE "K401C2"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::K401C(2%)
	CASE "K401A2"
		REALVALUE = W2_FORM(SUMMARY%)::K401A(2%)
	CASE "K401C3"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::K401C(3%)
	CASE "K401A3"
		REALVALUE = W2_FORM(SUMMARY%)::K401A(3%)
	CASE "K401C4"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::K401C(4%)
	CASE "K401A4"
		REALVALUE = W2_FORM(SUMMARY%)::K401A(4%)
	CASE "K401C5"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::K401C(5%)
	CASE "K401A5"
		REALVALUE = W2_FORM(SUMMARY%)::K401A(5%)

	!***********************************************************
	! OTHER
	!***********************************************************
	CASE "OTHERC", "OTHER1C"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::OTHERC(1%)
	CASE "OTHER", "OTHER1"
		REALVALUE = W2_FORM(SUMMARY%)::OTHER(1%)
	CASE "OTHER2C"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::OTHERC(2%)
	CASE "OTHER2"
		REALVALUE = W2_FORM(SUMMARY%)::OTHER(2%)
	CASE "OTHER3C"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::OTHERC(3%)
	CASE "OTHER3"
		REALVALUE = W2_FORM(SUMMARY%)::OTHER(3%)
	CASE "OTHER4C"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::OTHERC(4%)
	CASE "OTHER4"
		REALVALUE = W2_FORM(SUMMARY%)::OTHER(4%)
	CASE "OTHER5C"
		TEXTVALUE$ = W2_FORM(SUMMARY%)::OTHERC(5%)
	CASE "OTHER5"
		REALVALUE = W2_FORM(SUMMARY%)::OTHER(5%)

	END SELECT

	END SUB
	!+-+-+
	!++
	! Abstract:YEAR
	!	.ts 55
	!	^*Year	YYYY\*
	!	.b
	!	.lm +5
	!	The ^*Year\* field allows for entry of the year for which this report is
	!	to print.
	!	.lm -5
	!
	! Index:
	!	.x Year>Print W-2 Forms
	!	.x Print W-2 Forms>Year
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD03
	!	^*(03) Pension Plan Code\*
	!	.b
	!	.lm +5
	!	The ^*Pension Plan Code\* enters the code indicating
	!	the appropriate pension code.
	!	.lm -5
	!
	! Index:
	!	.x W-2 Forms>Pension Plan Code
	!	.x W-2 Forms>Code>Pension Plan
	!	.x Pension Plan Code>W-2 Forms
	!	.x Code>Pension Plan>W-2 Forms
	!
	!--
