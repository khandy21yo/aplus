1	%TITLE "PR Deduction Journal Maintenance"
	%SBTTL "PR_MAIN_TRN_DED"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_TRN_DED(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	!	The ^*Deduction Journal\*is where non-standard deductions
	!	are entered. A "non-standard deduction" refers to the ^&frequency\& of a
	!	deduction rather than the ^&type\& of deduction. Additional withholding
	!	taxes may also be entered.
	!	.p
	!	Manually calculated taxes withheld in the event of a manual payoff
	!	can also be entered in the Deduction Journal.
	!
	! Index:
	!	.x Functions>dedUction
	!	.x Payroll>Deductions
	!	.x Deductions>Payroll
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_TRN_DED/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PR_MAIN_TRN_DED
	!	$ DELETE PR_MAIN_TRN_DED.OBJ;*
	!
	! Author:
	!
	!	11/27/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	06/01/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	04/03/89 - Kevin Handy
	!		Added code for SI tax type.
	!
	!	08/25/89 - Kevin Handy
	!		Modified to allow reading of an updated file.
	!
	!	10/27/89 - Kevin Handy
	!		Modified to allow editing of state field when
	!		working with SU types.
	!
	!	03/09/90 - Kevin Handy
	!		Added maintainence for new TAXABLE and REPORTABLE
	!		fields in the journal.
	!
	!	12/13/90 - Kevin Handy
	!		Added PR_TRN_DED::ADDEXEMPT field.
	!
	!	05/13/91 - Kevin Handy
	!		Modified to set UPDATE_FLAG to zero on entry.
	!
	!	04/23/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/14/93 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/31/95 - Kevin Handy
	!		Fix some positioning problems with amount and
	!		exemptions overlapping other fields.
	!
	!	09/10/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/29/97 - Kevin Handy
	!		Use integer for #key
	!
	!	05/26/98 - Kevin Handy
	!		Add 'A'ccrual and 'F'inal to valid codes
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/21/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.HB"
	MAP (PR_TRN_DED)	PR_TRN_DED_CDD	PR_TRN_DED
	MAP (PR_TRN_DED_OLD)	PR_TRN_DED_CDD	PR_TRN_DED_OLD, PR_TRN_DED2

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP (PR_ERNDED_DEF)	PR_ERNDED_DEF_CDD	PR_ERNDED_DEF

	MAP (PR_DETAIL) &
		BATCH_NO$ = 8, &
		CLOSE_FLAG%

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PR_TRN_DED) &
		PR_TRN_DED.CH%, &
		PR_TRN_DED.READONLY%

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
	END RECORD

	MAP (TT_PR_TRN_DED) RARRAY_RECORD RARRAY(300%)

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (TT_TRN_DED) &
		DED_PTTITLE$ = 30%, &
		DED_PT$(6%) = 30%, &
		DED_STTITLE$ = 20%, &
		DED_ST$(11%) = 20% &

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Line items"
		SMG_WINDOW::NHELP = "PR_MAIN_TRN_DED"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 14%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 6%
		SMG_WINDOW::NITEMS= 9%
		IF CLOSE_FLAG%
		THEN
			SMG_WINDOW::FLAGS = 2%
		ELSE
			SMG_WINDOW::FLAGS = 0%
		END IF

		SMG_WINDOW::TOPLIN = 3%
		SMG_WINDOW::BOTLIN = 14%
		SMG_WINDOW::LINREC = 1%

		!
		! Deduction types
		!
		DED_PTTITLE$ = "Type Description"

		DED_PT$(0%) = "6"
		DED_PT$(1%) = "C    Calculated"
		DED_PT$(2%) = "D    Deduction"
		DED_PT$(3%) = "T    NonCompensation"
		DED_PT$(4%) = "M    Memo"
		DED_PT$(5%) = "A    Accrual"
		DED_PT$(6%) = "F    Final (Deduction)"

		!
		! Methods
		!
		DED_STTITLE$ = "Type Description"

		DED_ST$(0%) = "10"
		DED_ST$(1%) = "FI   Fica (OASDI)"
		DED_ST$(2%) = "FH   Fica (HI)"
		DED_ST$(3%) = "FW   Federal"
		DED_ST$(4%) = "SW   State"
		DED_ST$(5%) = "SX   OST"
		DED_ST$(6%) = "CW   City"
		DED_ST$(7%) = "DW   County"
		DED_ST$(8%) = "EW   School"
		DED_ST$(9%) = "SI   WC Liability Ins."
		DED_ST$(10%) = "SU   State Unemployment"

		IF INSTR(1%, " QV", MVALUE) <= 1%
		THEN
			!
			! Load in defaults
			!
			CALL READ_DEFAULTS(SMG_WINDOW)
		END IF

700		!
		! Declare channels
		!
		IF PR_TRN_DED.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_TRN_DED.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		IF (SMG_WINDOW::FLAGS AND 2%) = 0%
		THEN
			WHEN ERROR IN
				%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.CRE"
			USE
				CONTINUE 760 IF ERR = 10%
				PR_MAIN_TRN_DED = ERR
				CONTINUE 770
			END WHEN

			PR_TRN_DED.READONLY% = 0%
			GOTO 790
		END IF

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.OPN"
		USE
			PR_MAIN_TRN_DED = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_TRN_DED.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PR_TRN_DED.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PR_TRN_DED.CH%
		WHEN ERROR IN
			RESET #PR_TRN_DED.CH%
			GET #PR_TRN_DED.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display the background
	!
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  (01)       (02) (03)        (04) " + &
			"  (05)   (06)   (07)       (08)        (09)", &
			1%, 1%,, &
			SMG$M_REVERSE)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  End Date   Type Code      Amount " + &
			"Tax Code Stat Exempt    Taxable  Reportable", &
			2%, 1%,, SMG$M_REVERSE)

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Extra display stuff
	!
	CASE OPT_DISPLAY

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		!
		! Paint lines on screen
		!
		FOR I% = 1% TO 8%
			A% = VAL%(MID("013,018,023,035,044,049,057,068", &
				I% * 4% - 3%, 3%))

			SMG_STATUS% = SMG$DRAW_LINE(SMG_WINDOW::WNUMBER, &
				1%, A%, SMG_WINDOW::BOTLIN, A%)
		NEXT I%

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Enter/Display/Default
	!
20200	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		XLINE$ = NUM1$(SMG_WINDOW::CURLIN)

		TFLAG% = MFLAG
		TVALUE$ = MVALUE

		SELECT PR_TRN_DED::CODE
		CASE "FW"

			SELECT MLOOP
			CASE 5%
				MFLAG = MFLAG OR 33%
				MFLAG = MFLAG OR NOT 64%
				MVALUE = ""
			END SELECT

		CASE "SW", "SX", "CW", "DW", "EW", "SI", "SU"

		CASE ELSE

			SELECT MLOOP
			CASE 5%, 6%, 7%
				MFLAG = MFLAG OR 33%
				MFLAG = MFLAG OR NOT 64%
				MVALUE = ""
			END SELECT

		END SELECT

		SELECT MLOOP

		CASE 1%
	!++
	!
	! Abstract:FLD001
	!	^*(01) End Date\*
	!	.p
	!	The ^*End Date\* field
	!	indicates the Payroll End Date for any deductions.
	!
	! Index:
	!	.x End Date>Detail>Deduction
	!	.x Detail>Deduction>End Date
	!
	!--

			PR_TRN_DED::PR_END_DATE = BATCH_NO$ &
				IF PR_TRN_DED::PR_END_DATE = ""
			PR_TRN_DED::PR_END_DATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";3", TEMP$, &
				PR_TRN_DED::PR_END_DATE, MFLAG, "8", MVALUE)

		CASE 2%
	!++
	!
	! Abstract:FLD002
	!	^*(02) Type\*
	!	.p
	!	The ^*Type\* column indicates the
	!	type of deduction represented by a record. Valid types are:
	!	.B
	!	.lm +5
	!	.LS 0,"o"
	!	.LE
	!	C = Calculated withholding taxes.
	!	.LE
	!	D = Deduction. Any deduction other than a *C type.
	!	.LE
	!	T = NonCompensation
	!	.le
	!	M = Memo
	!	.lm -5
	!	.els
	!
	! Index:
	!	.x Type>Detail>Deduction
	!	.x Detail>Deduction>Type
	!
	!--
			PR_TRN_DED::DTYPE = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";15", TEMP$, &
				PR_TRN_DED::DTYPE, MFLAG, "!", MVALUE, &
				DED_PT$(), DED_PTTITLE$, "005"), -1%)

		CASE 3%
	!++
	!
	! Abstract:FLD003
	!	^*(03) Code\*
	!	.p
	!	The ^*Code\* column indicates the
	!	code which designates the specific kind of payroll deduction which
	!	has been made.
	!	.p
	!	Valid codes for *D type deductions are user defined and can be
	!	viewed by pressing ^*<List Choices>\*.
	!	.p
	!	Valid codes for *C type deductions can also be viewed by pressing
	!	^*<List Choices>\* and are:
	!	.lm +5
	!	.ls 0,"o"
	!	.le
	!	FI = FICA
	!	.le
	!	FW = Federal Withholding Tax
	!	.le
	!	SW = State Withholding Tax
	!	.le
	!	SX = Other State Taxes (i.e. State Disability)
	!	.le
	!	CW = City Withholding Tax
	!	.le
	!	DW = County Withholding Tax
	!	.le
	!	EW = School District Withholding Tax
	!	.els
	!
	! Index:
	!	.x Code>Detail>Deduction
	!	.x Detail>Deduction>Code
	!
	!--
			IF PR_TRN_DED::DTYPE = "C"
			THEN
				PR_TRN_DED::CODE = EDIT$(ENTR_3STRINGLIST(SCOPE, &
					SMG_WINDOW::WNUMBER, XLINE$ + ";20", TEMP$, &
					PR_TRN_DED::CODE, MFLAG, "'E", MVALUE, &
					DED_ST$(), DED_STTITLE$, "005"), -1%)
			ELSE
				PR_TRN_DED::CODE = ENTR_3STRING(SCOPE, &
					SMG_WINDOW::WNUMBER, XLINE$ + ";20", TEMP$, &
					PR_TRN_DED::CODE, MFLAG, "'E", MVALUE)

				IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
				THEN
					IF (MAIN_WINDOW(PR_MAIN_ERNDED_DEF.ID, "V0 ") = 1%)
					THEN
						PR_TRN_DED::CODE = &
							PR_ERNDED_DEF::CODE
					END IF
					GOTO E0Loop
				END IF
			END IF

		CASE 4%
	!++
	!
	! Abstract:FLD004
	!	^*(04) Amount\*
	!	.p
	!	The ^*Amount\* field indicates the
	!	amount deducted for a specific withholding tax or other deduction.
	!
	! Index:
	!	.x Amount>Detail>Deduction
	!	.x Detail>Deduction>Amount
	!
	!--
			PR_TRN_DED::AMOUNT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";25", TEMP$, &
				PR_TRN_DED::AMOUNT * 1.0, MFLAG, &
				"#######.##", MVALUE)

		CASE 5%
	!++
	!
	! Abstract:FLD005
	!	^*(05) Tax Code\*
	!	.p
	!	The ^*Tax Code\* field
	!	indicates the code for a specific tax jurisdiction when the related
	!	deduction is State Withholding, Other State, City Withholding, County
	!	Withholding or School District Withholding Taxes.  (An ^*FI\* or ^*FW\* code
	!	does not require a Tax Code.)
	!	.p
	!	The field is not applicable when the deduction relates to Federal
	!	Withholding or FICA Taxes or deductions other than taxes.
	!
	! Index:
	!	.x Tax Code>Detail>Deduction
	!	.x Detail>Deduction>Tax Code
	!
	!--
			PR_TRN_DED::TAX_CODE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";39", TEMP$, &
				PR_TRN_DED::TAX_CODE, MFLAG, "'E", MVALUE)

		CASE 6%
	!++
	!
	! Abstract:FLD006
	!	^*(06) Status\*
	!	.p
	!	The ^*Status\* field indicates
	!	the marital status which was in effect for a specific withholding tax.
	!	(An ^*FI\* code does not require a status code.)
	!	.p
	!	The field is not applicable for a non-tax type deduction.
	!
	! Index:
	!	.x Status>Detail>Deduction
	!	.x Detail>Deduction>Status
	!
	!--
			PR_TRN_DED::SSTATUS = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";46", TEMP$, &
				PR_TRN_DED::SSTATUS, MFLAG, "'", MVALUE)

		CASE 7%
	!++
	!
	! Abstract:FLD007
	!	^*(07) Exemptions\*
	!	.p
	!	The ^*Exemptions\* field
	!	indicates the number of exemptions which are applicable for a specific
	!	tax withholding code.
	!	There are two entries here, one for regular exemptions, and the second
	!	for additional exemptions. See the Tax Table for more information about
	!	the difference between the exemptions.
	!	.b
	!	Many deductions, such as ^*FI\* do not require a value in the
	!	exemption fields. State (*S*W, *S*X) and Federal (*F*W) require them.
	!
	! Index:
	!	.x Exemptions>Detail>Deduction
	!	.x Detail>Deduction>Exemptions
	!
	!--
			PR_TRN_DED::EXEMPT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";50", TEMP$, &
				PR_TRN_DED::EXEMPT * 1.0, MFLAG, "###", MVALUE)

			PR_TRN_DED::ADDEXEMPT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";54", TEMP$, &
				PR_TRN_DED::ADDEXEMPT * 1.0, MFLAG, "###", MVALUE)

		CASE 8%
	!++
	!
	! Abstract:FLD008
	!	^*(08) Taxable\*
	!	.p
	!	The ^*Taxable\* field records the taxable
	!	basis for the calculation of a specific deduction.
	!	This field is used mostly with calculated taxes, and only rarely
	!	with users deductions.
	!
	! Index:
	!	.x Taxable>Detail>Deduction
	!	.x Detail>Deduction>Taxable
	!
	!--
			PR_TRN_DED::TAXABLE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";58", TEMP$, &
				PR_TRN_DED::TAXABLE, MFLAG, &
				"#######.##", MVALUE)

		CASE 9%
	!++
	!
	! Abstract:FLD009
	!	^*(09) Reportable\*
	!	.p
	!	The ^*Reportable\* field contains the reportable basis used in the
	!	calculation of a deduction.
	!	This field is used mostly by calculated taxes and only rarely
	!	for deductions.
	!
	! Index:
	!	.x Detail>Deduction>Reportable
	!	.x Reportable>Detail>Deduction
	!
	!--
			PR_TRN_DED::REPORTABLE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";69", TEMP$, &
				PR_TRN_DED::REPORTABLE, MFLAG, &
				"#######.##", MVALUE)

		END SELECT

		MFLAG = TFLAG%
		MVALUE = TVALUE$
		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		PR_MAIN_TRN_DED = 0%

		SELECT MLOOP

		CASE 3%
			IF PR_TRN_DED::DTYPE <> "C"
			THEN
				PR_MAIN_TRN_DED = FUNC_TESTENTRY(SMG_WINDOW, &
					PR_TRN_DED::DTYPE + PR_TRN_DED::CODE, &
					PR_ERNDED_DEF::DESCR, &
					"PR", MLOOP, "PRG", &
					"Code", PR_MAIN_ERNDED_DEF.ID)
			END IF

			SELECT PR_TRN_DED::CODE
			CASE "SW", "SX", "CW", "DW", "EW", "SI"

			CASE ELSE
				PR_TRN_DED::TAX_CODE = ENTR_3STRING(SCOPE, &
					SMG_WINDOW::WNUMBER, XLINE$ + ";38", &
					TEMP$, PR_TRN_DED::TAX_CODE, 33%, &
					"'E", "")
			END SELECT

			SELECT PR_TRN_DED::CODE
			CASE "FW", "SW", "SX", "CW", "DW", "EW", "SI"

			CASE ELSE
				PR_TRN_DED::SSTATUS = ENTR_3STRING(SCOPE, &
					SMG_WINDOW::WNUMBER, XLINE$ + ";46", &
					TEMP$, PR_TRN_DED::SSTATUS, 33%, &
					"'", "")

				PR_TRN_DED::EXEMPT = ENTR_3NUMBER(SCOPE, &
					SMG_WINDOW::WNUMBER, XLINE$ + ";53", &
					TEMP$, PR_TRN_DED::EXEMPT * 1.0, 33%, &
					"###", "")

				PR_TRN_DED::ADDEXEMPT = ENTR_3NUMBER(SCOPE, &
					SMG_WINDOW::WNUMBER, XLINE$ + ";59", &
					TEMP$, PR_TRN_DED::ADDEXEMPT * 1.0, 33%, &
					"###", "")

			END SELECT

		END SELECT

	!
	! Test option
	!
	CASE OPT_TESTOPT
		FUNC_FLAG% = 0%

		IF EDIT$(MVALUE, -1%) = "ERASE" OR &
			EDIT$(MVALUE, -1%) = "CHANGE" &
			OR EDIT$(MVALUE, -1%) = "INITIALIZE" &
			OR EDIT$(MVALUE, -1%) = "BLANK"
		THEN
			!***************************************************
			! Test to see if payroll has been closed or posted
			!	1 - Updated to Register
			!	2 - Accrued Post
			!	4 - Final Post
			!***************************************************

			IF (PR_TRN_DED::UPDATE_FLAG AND 1%)
			THEN
				CALL HELP_3MESSAGE(SCOPE, "PR Folder is Closed", "ERR", "PR_CLOSED", &
						"ERROR_PR_CLOSED")
				FUNC_FLAG% = 1%
			END IF

			IF (PR_TRN_DED::UPDATE_FLAG AND 2%) AND FUNC_FLAG% = 0%
			THEN
				CALL HELP_3MESSAGE(SCOPE, "PR Folder is Accrued", "ERR", "PR_ACCRUED", &
						"ERROR_PR_ACCRUED")
				FUNC_FLAG% = 1%
			END IF

			IF (PR_TRN_DED::UPDATE_FLAG AND 4%) AND FUNC_FLAG% = 0%
			THEN
				CALL HELP_3MESSAGE(SCOPE, "PR Folder has been Posted", "ERR", "PR_POSTED", &
					"ERROR_PR_POSTED")
				FUNC_FLAG% = 1%
			END IF
		END IF

	!
	! Set PR_TRN_DED_OLD value
	!
20500	CASE OPT_SETOLD
		PR_TRN_DED_OLD = PR_TRN_DED

	!
	! Restore PR_TRN_DED_OLD value
	!
	CASE OPT_RESETOLD
		PR_TRN_DED = PR_TRN_DED_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PR_TRN_DED::UPDATE_FLAG = 0%
		PR_TRN_DED2 = PR_TRN_DED

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PR_TRN_DED = PR_TRN_DED2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		PR_TRN_DED::EMPNUM = PR_EMP_MASTER::EMPNUM


	!
	! Find
	!
	CASE OPT_FIND
		FIND #PR_TRN_DED.CH%, &
			KEY #0% GE PR_TRN_DED::EMPNUM + "", &
			REGARDLESS

	!
	! Handle array of records
	!
27000	CASE OPT_ARRAY

		!
		! Select sub-option of array
		!
		SELECT MLOOP

		!
		! Load array with line items
		!
		CASE 1%

			!
			! Empty array
			!
			SMG_WINDOW::TOTREC = 0%

27110			!
			! Search for first record
			!
			WHEN ERROR IN
				FIND #SMG_WINDOW::CHAN, &
					KEY #0% GE PR_EMP_MASTER::EMPNUM + "", &
					REGARDLESS
			USE
				CONTINUE 28000
			END WHEN

27120			!
			! Get a record
			!
			WHEN ERROR IN
				GET #SMG_WINDOW::CHAN
			USE
				CONTINUE 28000 IF ERR = 11%
				EXIT HANDLER
			END WHEN

			IF (PR_TRN_DED::EMPNUM = PR_EMP_MASTER::EMPNUM)
			THEN
				!
				! Add information to array
				!
				SMG_WINDOW::TOTREC = SMG_WINDOW::TOTREC + 1%
				RARRAY(SMG_WINDOW::TOTREC)::LINRFA = &
					GETRFA(SMG_WINDOW::CHAN)
				GOTO 27120
			END IF

		!
		! Remove one element of the array
		!
		CASE 2%
			!
			! Remove item pointed to by Mflag
			!
			RARRAY(I%) = RARRAY(I% + 1%) &
				FOR I% = MFLAG TO SMG_WINDOW::TOTREC - 1%

		!
		! Set array item to current record
		!
		CASE 3%
			RARRAY(MFLAG)::LINRFA = GETRFA(SMG_WINDOW::CHAN)

		!
		! Load in current record, locked
		!
		CASE 4%
27200			GET #SMG_WINDOW::CHAN, RFA RARRAY(MFLAG)::LINRFA

		!
		! Load in current record, unlocked
		!
		CASE 5%
			GET #SMG_WINDOW::CHAN, RFA RARRAY(MFLAG)::LINRFA, &
				REGARDLESS

		!
		! Change the current record's key to match header.  The
		! new key is probibly passes through MVALUE, unless some
		! other means is devised.
		!
		CASE 6%
			PR_TRN_DED::EMPNUM = MID(MVALUE, 2%, 10%)

		END SELECT
	END SELECT

28000	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
