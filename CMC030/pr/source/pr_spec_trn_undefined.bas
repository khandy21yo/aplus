1	%TITLE "RESYNC - Resync Taxable & Reportable"
	%SBTTL "PR_SPEC_TRN_UNDEFINED"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1994 BY
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
	! ID:PR073
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Resync Folder\* option is used to re-calculate
	!	the ^~taxable\~ and ^~Reportable\~ fields in the deduction
	!	part of the payroll folder.
	!	.lm -5
	!
	! Index:
	!	.x Resync>Deduction
	!	.x Deduction>Resync
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_SPEC_TRN_UNDEFINED/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_SPEC_TRN_UNDEFINED, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_SPEC_TRN_UNDEFINED.OBJ;*
	!
	! Author:
	!
	!	12/16/94 - Kevin Handy
	!
	! Modification history:
	!
	!	01/18/95 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/11/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 standards.
	!		Remove unsolicited_input stuff.
	!
	!	08/26/96 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	03/13/97 - Kevin Handy
	!		Handle FH Codes.
	!
	!	05/12/97 - Kevin Handy
	!		Dont open PR_TAX_TABLE, since no information
	!		from there gets used.
	!
	!	08/27/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/13/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.HB"
	MAP	(PR_TRN_CHECK)		PR_TRN_CHECK_CDD	PR_TRN_CHECK
	MAP	(PR_HIS_CHECK)		PR_TRN_CHECK_CDD	PR_HIS_CHECK

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

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STATUS.HB"
	MAP	(PR_EMP_STATUS)		PR_EMP_STATUS_CDD	PR_EMP_STATUS

	!
	! External functions
	!
	EXTERNAL STRING  FUNCTION DATE_STOREDATE


	!
	! Structures local to program
	!
	RECORD TOTAL_RECORD
		STRING CODE = 2%
		GFLOAT TAXABLE
		GFLOAT REPORTABLE
		GFLOAT TAX
		STRING STAT = 1%
		LONG EXEMPT
		LONG ADDEXEMPT
		LONG EMPCOUNT
	END RECORD

	DIM TOTAL_RECORD EMPLOYEE(10%, 10%)
	DIM TOTAL_RECORD TOTAL(10%, 30%)

	!
	! Dimension
	!
	DIM	EMP_NT(10%), &
		EMP_NTR(10%), &
		EMPLOYEE_CODES%(10%), &
		TOTAL_WH_CODE%(10%)

	DIM DATA_FILE$(600%)

	%PAGE

	ON ERROR GOTO 19000

	!
	! Other Variables
	!
 !	SUBJECT_TYPE_TABLE$ = "FIE*FWH*SWH*OST*SUI*CWH*DWH*EWH*SWC"
 !	TAX_TYPE_TABLE$ = "FI*FW*SW*SX*SU*CW*DW*EW*SI*"

 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	FROM_BATCH_NO$ = DATE_STOREDATE(FROM_BATCH_NO$)

	!++
	! Abstract:FLD01
	!	^*(01) Start Payroll Date\*
	!	.p
	!	The ^*Start Payroll Date\* field is to contain the date of
	!	the payroll folder with which the report will begin printing.
	!	A blank field will cause the report to start
	!	with the first payroll folder date in the file.
	!
	! Index:
	!	.x Start Payroll Date>Employee Check Audit Report
	!	.x Employee Check Audit Report>Start Payroll Date
	!
	! Datatype:DATE
	! Size:8
	! Required:Y
	!--

	TO_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	TO_BATCH_NO$ = DATE_STOREDATE(TO_BATCH_NO$) ! Reformat to (YYYYMMDD)

	!++
	! Abstract:FLD02
	!	^*(02) End Payroll Date\*
	!	.p
	!	The ^*End Payroll Date\* field is to contain the date of
	!	the payroll folder with which the report is to end printing.
	!	A blank field will cause the report to end
	!	with the last payroll folder date in the file.
	!
	! Index:
	!	.x End Payroll Date>Employee Check Audit Report
	!	.x Employee Check Audit Report>End Payroll Date
	!
	! Datatype:DATE
	! Size:8
	! Required:Y
	!--

	!
	! Look up all folders
	!
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
 !	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.OPN"
 !
 !	GET #PR_TAX_TABLE.CH%, KEY #0 EQ "F", REGARDLESS
 !
 !	FICA_EMPR_PCT = (PR_TAX_TABLE::FICA_EMPR_PCT) / 100000.
 !	FICA_EMPE_PCT = (PR_TAX_TABLE::FICA_EMPE_PCT) / 100000.
 !	FICA_LIMIT = PR_TAX_TABLE::FICA_LIMIT
 !
 !	CLOSE PR_TAX_TABLE.CH%

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

380	!
	! Open Tax package
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.OPN"
	USE
		FILENAME$ = "PR_TAX_PKG"
		CONTINUE HelpError
	END WHEN

390	!
	! Open employee status file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STATUS.OPN"
	USE
		FILENAME$ = "PR_EMP_STATUS"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
 !	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	!
	! Set up titles and whatnot
	!
	TITLE$(1%) = "Employee Payroll Taxes Report - Resync"
	TITLE$(2%) = "For the Payroll Folder Dated: " + &
		MID(BATCH_NO$, 5%, 2%) + "/" + &
		MID(BATCH_NO$, 7%, 2%) + "/" + &
		LEFT(BATCH_NO$, 4%)
	TITLE$(3%) = "By Employee Number"

	TITLE$(4%) = ""

	TITLE$(5%) = "                                      Pay/        FICA" + &
		"     Federal        State          OST         SUI         " + &
		"City       County"

	TITLE$(6%) = "Emp #      Name                    NonComp Taxable/Tax" + &
		" Taxable/Tax  Taxable/Tax  Taxable/Tax  Taxable/Tax  Taxable/Tax" + &
		"  Taxable/Tax"

	TITLE$(7%) = ""

	%PAGE

16000	FOR PR_LOOP% = 1% TO DATA_FILE%

		BATCH_NO$ = DATA_FILE$(PR_LOOP%)
		CALL ENTR_3MESSAGE(SCOPE, "Processing: " + BATCH_NO$, 1%)

		YYYY$ = LEFT(BATCH_NO$, 4%)

		!
		! Open Pay folder
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
		USE
			CONTINUE 16010 IF ERR = 5%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		USE_HISTORY% = 0%

		GOTO 16020

16010		!
		! Open pay history folder if journal not there
		!
		%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.OPN"

		PR_TRN_PAY.CH% = PR_HIS_PAY.CH%
		USE_HISTORY% = -1%

16020		!
		! Open Deduction folder
		!
		IF USE_HISTORY% = 0%
		THEN
			WHEN ERROR IN
				%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.OPN"
			USE
				CONTINUE 16090 IF ERR = 5%
				FILENAME$ = "PR_TRN_DED"
				CONTINUE HelpError
			END WHEN
		ELSE
			WHEN ERROR IN
				%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_DED.OPN"
			USE
				CONTINUE 16090 IF ERR = 5%
				FILENAME$ = "PR_TRN_DED"
				CONTINUE HelpError
			END WHEN

			PR_TRN_DED.CH% = PR_HIS_DED.CH%
		END IF

16030		!
		! Open check folder
		!
		IF USE_HISTORY% = 0%
		THEN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.OPN"
		ELSE
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_CHECK.OPN"

			PR_TRN_CHECK.CH% = PR_HIS_CHECK.CH%
		END IF


16040		TEXT$ = "Starting folder " + BATCH_NO$ + " " + &
			DATE$(0%) + " " + TIME$(0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		GOSUB 17000

		CLOSE PR_TRN_PAY.CH%
		CLOSE PR_TRN_DED.CH%

16090	NEXT PR_LOOP%

	TEXT$ = "Finished " + DATE$(0%) + " " + TIME$(0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	RESET #PR_TRN_PAY.CH%, KEY #0%

	LAST_FOUND$ = ""

17020	WHEN ERROR IN
		GET #PR_TRN_PAY.CH%, REGARDLESS
	USE
		CONTINUE 17100
	END WHEN

	!
	! If history then set history into journal map
	!
	IF USE_HISTORY%
	THEN
		PR_TRN_PAY = PR_HIS_PAY
	END IF

17030	!
	! Only want to try them once
	!
	GOTO 17020 IF LAST_FOUND$ = PR_TRN_PAY::EMPNUM
	LAST_FOUND$ = PR_TRN_PAY::EMPNUM

	WHEN ERROR IN
		FIND #PR_EMP_MASTER.CH%, KEY #0% EQ PR_TRN_PAY::EMPNUM, REGARDLESS
	USE
		CONTINUE 17040
	END WHEN

	GOTO 17020

17040	TEXT$ = PR_TRN_PAY::EMPNUM + " (Pay)"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO 17020

17100	!**********************************************************************
	! Now do deduction file
	!**********************************************************************

	RESET #PR_TRN_DED.CH%, KEY #0%

	LAST_FOUND$ = ""

17120	WHEN ERROR IN
		GET #PR_TRN_DED.CH%, REGARDLESS
	USE
		CONTINUE 17200
	END WHEN

	!
	! If history then set history into journal map
	!
	IF USE_HISTORY%
	THEN
		PR_TRN_DED = PR_HIS_DED
	END IF

17130	!
	! Only want to try them once
	!
	GOTO 17120 IF LAST_FOUND$ = PR_TRN_DED::EMPNUM
	LAST_FOUND$ = PR_TRN_DED::EMPNUM

	WHEN ERROR IN
		FIND #PR_EMP_MASTER.CH%, KEY #0% EQ PR_TRN_DED::EMPNUM, REGARDLESS
	USE
		CONTINUE 17140
	END WHEN

	GOTO 17150

17140	TEXT$ = PR_TRN_DED::EMPNUM + " (Deduction)"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

17150	WHEN ERROR IN
		FIND #PR_TRN_PAY.CH%, KEY #0% EQ PR_TRN_DED::EMPNUM, REGARDLESS
	USE
		CONTINUE 17160
	END WHEN

	GOTO 17120

17160	TEXT$ = PR_TRN_DED::EMPNUM + " (Deduction Not in Pay)"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO 17120

17200	!**********************************************************************
	! Now do Check file
	!**********************************************************************

	RESET #PR_TRN_CHECK.CH%, KEY #0%

	LAST_FOUND$ = ""

17220	WHEN ERROR IN
		GET #PR_TRN_CHECK.CH%, REGARDLESS
	USE
		CONTINUE 17300
	END WHEN

	!
	! If history then set history into journal map
	!
	IF USE_HISTORY%
	THEN
		PR_TRN_CHECK = PR_HIS_CHECK
	END IF

17230	!
	! Only want to try them once
	!
	GOTO 17220 IF LAST_FOUND$ = PR_TRN_CHECK::EMPNUM
	LAST_FOUND$ = PR_TRN_CHECK::EMPNUM

	WHEN ERROR IN
		FIND #PR_EMP_MASTER.CH%, KEY #0% EQ PR_TRN_CHECK::EMPNUM, REGARDLESS
	USE
		CONTINUE 17240
	END WHEN

	GOTO 17250

17240	TEXT$ = PR_TRN_CHECK::EMPNUM + " (Check)"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

17250	WHEN ERROR IN
		FIND #PR_TRN_PAY.CH%, KEY #0% EQ PR_TRN_CHECK::EMPNUM, REGARDLESS
	USE
		CONTINUE 17260
	END WHEN

	GOTO 17220

17260	TEXT$ = PR_TRN_CHECK::EMPNUM + " (Check Not in Pay)"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO 17220

17300	RETURN


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
