1	%TITLE "Create Payroll Master File from W2 Tape"
	%SBTTL "PR_SPEC_TAPETOMASTER"
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
	! Abstract:HELP
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_SPEC_TAPETOMASTER
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_SPEC_TAPETOMASTER, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_SPEC_TAPETOMASTER.OBJ;*
	!
	! Author:
	!
	!	03/22/90 - Kevin Handy
	!
	! Modification history:
	!
	!	06/04/91 - Kevin Handy
	!		Removed junk in error trapping.
	!
	!	07/13/91 - Kevin Handy
	!		Removed PR_FUNC_SUBJECT com definition.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/25/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/16/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/09/99 - Kevin Handy
	!		Lose NoFile (Dead Code)
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER) PR_EMP_MASTER_CDD PR_EMP_MASTER

	!
	! Define maps
	!
	MAP (PR_CODE) CODE$ = 276%

	MAP (PR_CODE) &
		CODE_A_RECID$ = 1%, &
		CODE_A_PAYYR$ = 4%, &
		CODE_A_EIN$ = 9%, &
		CODE_A_SPACE1$ = 8%, &
		CODE_A_FORADD$ = 1%, &
		CODE_A_NAME$ = 50%, &
		CODE_A_ADD$ = 40%, &
		CODE_A_CITY$ = 25%, &
		CODE_A_ST$ = 2%, &
		CODE_A_SPACE2$ = 13%, &
		CODE_A_ZIP$ = 5%, &
		CODE_A_ZIPEXT$ = 5%, &
		CODE_A_SPACE3$ = 113%


	MAP (PR_CODE) &
		CODE_B_RECID$ = 1%, &
		CODE_B_PAYYR$ = 4%, &
		CODE_B_EIN$ = 9%, &
		CODE_B_COMTYP$ = 8%, &
		CODE_B_LABEL$ = 2%, &
		CODE_B_SPACE1$ = 1%, &
		CODE_B_DENSITY$ = 2%, &
		CODE_B_RECCOD$ = 3%, &
		CODE_B_SPACE2$ = 115%, &
		CODE_B_FORADD$ = 1%, &
		CODE_B_NAME$ = 44%, &
		CODE_B_ADD$ = 35%, &
		CODE_B_CITY$ = 20%, &
		CODE_B_ST$ = 2%, &
		CODE_B_SPACE3$ = 5%, &
		CODE_B_ZIP$ = 5%, &
		CODE_B_ZIPEXT$ = 5%, &
		CODE_B_SPACE4$ = 14%

	MAP (PR_CODE) &
		CODE_E_RECID$ = 1%, &
		CODE_E_PAYYR$ = 4%, &
		CODE_E_EIN$ = 9%, &
		CODE_E_SIN$ = 9%, &
		CODE_E_NAME$ = 50%, &
		CODE_E_ADD$ = 40%, &
		CODE_E_CITY$ = 25%, &
		CODE_E_STATE$ = 10%, &
		CODE_E_ZIPEXT$ = 5%, &
		CODE_E_ZIP$ = 5%, &
		CODE_E_NAMCODE$ = 1%, &
		CODE_E_TYPEMP$ = 1%, &
		CODE_E_BLKFAC$ = 2%, &
		CODE_E_PRU$ = 4%, &
		CODE_E_SPACE1$ = 88%, &
		CODE_E_SLL$ = 1%, &
		CODE_E_FORADD$ = 1%, &
		CODE_E_SPACE2$ = 20%

	MAP (PR_CODE) &
		CODE_W_RECID$ = 1%, &
		CODE_W_SSN$ = 9%, &
		CODE_W_NAME$ = 27%, &
		CODE_W_ADD$ = 40%, &
		CODE_W_CITY$ = 25%, &
		CODE_W_ST$ = 10%, &
		CODE_W_ZIPEXT$ = 5%, &
		CODE_W_ZIP$ = 5%, &
		CODE_W_SEC$ = 1%, &
		CODE_W_FICA_WAGE$ = 7%, &
		CODE_W_SPACE1$ = 1%, &
		CODE_W_FICA_TIP$ = 7%, &
		CODE_W_SPACE2$ = 1%, &
		CODE_W_ANNWAGE$ = 9%, &
		CODE_W_SPACE3$ = 1%, &
		CODE_W_FICA_WTHLD$ = 6%, &
		CODE_W_FED_WTHLD$ = 9%, &
		CODE_W_SPACE4$ = 1%, &
		CODE_W_ALLTIP$ = 7%, &
		CODE_W_SPACE5$ = 1%, &
		CODE_W_FR_BEN$ = 9%, &
		CODE_W_SPACE6$ = 51%, &
		CODE_W_CN$ = 7%, &
		CODE_W_GTL$ = 7%, &
		CODE_W_UNCOLL_FICA$ = 7%, &
		CODE_W_EIC$ = 7%, &
		CODE_W_SPACE7$ = 1%, &
		CODE_W_PENPLAN_FLAG$ = 1%, &
		CODE_W_SPACE8$ = 1%, &
		CODE_W_DEFCOMP_FLAG$ = 1%, &
		CODE_W_SPACE9$ = 1%, &
		CODE_W_DEFCOMP$ = 9%, &
		CODE_W_SPACE10$ = 1%

	MAP (PR_CODE) &
		CODE_S_RECID$ = 1%, &
		CODE_S_SSN$ = 9%, &
		CODE_S_NAME$ = 27%, &
		CODE_S_ADD$ = 40%, &
		CODE_S_CITY$ = 25%, &
		CODE_S_ST$ = 10%, &
		CODE_S_ZIPEXT$ = 5%, &
		CODE_S_ZIP$ = 5%, &
		CODE_S_SPACE1$ = 1%, &
		CODE_S_STCODE1$ = 2%, &
		CODE_S_OPTIONAL$ = 2%, &
		CODE_S_REPPER$ = 4%, &
		CODE_S_TOTWAG$ = 9%, &
		CODE_S_SUIWAG$ = 9%, &
		CODE_S_WEEKWORK$ = 2%, &
		CODE_S_HIREDATE$ = 4%, &
		CODE_S_FIREDATE$ = 4%, &
		CODE_S_ENTITYCODE1$ = 5%, &
		CODE_S_SEAN$ = 12%, &
		CODE_S_SPACE2$ = 6%, &
		CODE_S_STCODE2$ = 2%, &
		CODE_S_STWAGE$ = 9%, &
		CODE_S_STTAX$ = 8%, &
		CODE_S_OTHDATA$ = 10%, &
		CODE_S_TAXTYP$ = 1%, &
		CODE_S_ENTITYCODE2$ = 5%, &
		CODE_S_LOCWAGE$ = 9%, &
		CODE_S_LOCTAX$ = 7%, &
		CODE_S_STCONNUM$ = 7%, &
		CODE_S_SPACE3$ = 36%

	MAP (PR_CODE) &
		CODE_I_RECID$ = 1%, &
		CODE_I_FICA_WAGE$ = 10%, &
		CODE_I_SPACE1$ = 1%, &
		CODE_I_FICA_TIP$ = 10%, &
		CODE_I_SPACE$ = 1%, &
		CODE_I_ANNWAGE$ = 10%, &
		CODE_I_SPACE2$ = 1%, &
		CODE_I_FICA_WTHLD$ = 10%, &
		CODE_I_SPACE3$ = 1%, &
		CODE_I_FED_WTHLD$ = 10%, &
		CODE_I_CONTNUM$ = 7%, &
		CODE_I_LIFINS$ = 10%, &
		CODE_I_UNCFICA$ = 10%, &
		CODE_I_EIC$ = 11%, &
		CODE_I_ALLTIP$ = 10%, &
		CODE_I_FR_BEN$ = 10%, &
		CODE_I_SPACE4$ = 1%, &
		CODE_I_DEFCOMP$ = 10%, &
		CODE_I_SPACE5$ = 152%

	MAP (PR_CODE) &
		CODE_T_RECID$ = 1%, &
		CODE_T_NUM_OF_EMP$ = 7%, &
		CODE_T_FICA_WAGE$ = 13%, &
		CODE_T_SPACE1$ = 1%, &
		CODE_T_FICA_TIP$ = 12%, &
		CODE_T_ANNWAGE$ = 13%, &
		CODE_T_SPACE2$ = 1%, &
		CODE_T_FICA_WTHLD$ = 12%, &
		CODE_T_SPACE3$ = 1%, &
		CODE_T_FED_WTHLD$ = 12%, &
		CODE_T_LIFINS$ = 12%, &
		CODE_T_UNCFICA$ = 12%, &
		CODE_T_EIC$ = 12%, &
		CODE_T_ALLTIP$ = 12%, &
		CODE_T_FR_BEN$ = 12%, &
		CODE_T_SPACE4$ = 1%, &
		CODE_T_DEFCOMP$ = 12%, &
		CODE_T_SPACE5$ = 130%

	MAP (PR_CODE) &
		CODE_F_RECID$ = 1%, &
		CODE_F_NUM_OF_EMP$ = 7%, &
		CODE_F_SPACE1$ = 1%, &
		CODE_F_TOTALSSW$ = 16%, &
		CODE_F_SPACE2$ = 1%, &
		CODE_F_TOTALSST$ = 16%, &
		CODE_F_SPACE3$ = 1%, &
		CODE_F_TOTALATW$ = 16%, &
		CODE_F_SPACE4$ = 1%, &
		CODE_F_TOTALSSTW$ = 16%, &
		CODE_F_SPACE5$ = 1%, &
		CODE_F_TOTALFITW$ = 16%, &
		CODE_F_SPACE6$ = 1%, &
		CODE_F_TOTALEIC$ = 16%, &
		CODE_F_SPACE7$ = 166%

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

300	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.CRE"

	!
	! Set up screen
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 80%, SMG_SCREEN_DATA%)
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 1%, 1%)

	!
	! Look up device
	!
	CALL  READ_DEVICE("TAPE_DEVICE", TAPE_DEVICE$, STAT%)

 AskTape:
	TEMP$ = "Tape device <"+TAPE_DEVICE$+"> "

	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, TEMP$, 1%, 1%)

	JUNK$ = SPACE$(20%)
	SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, 1%, LEN(TEMP$) + 2%, &
		JUNK$, -1%, 16%+4096%)

	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, 87%, 73%, 65%, &
		69%, 70%, 87%, SMG$K_TRM_DO

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE,SCOPE::SCOPE_EXIT)
		GOTO AskTape

	END SELECT

	IF JUNK$ <> ""
	THEN
		TAPE_DEVICE$ = JUNK$
	END IF

	IF TAPE_DEVICE$ = ""
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Please Enter Tape Device", 1%)
		GOTO AskTape
	END IF

	!***************************************************************
	! Open mag tape drive
	!***************************************************************

	CALL ASSG_CHANNEL(PRNT.CH%,STAT%)

	OPEN TAPE_DEVICE$ FOR INPUT AS FILE PRNT.CH%, &
		ACCESS READ, &
		RECORDSIZE 25% * 276%

	V% = MAGTAPE(3%, 0%, PRNT.CH%)

	SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_SCREEN_DATA%)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)

2000	!*******************************************************************
	! Read through Payroll master file
	!*******************************************************************

	GET #PRNT.CH%

	MOVE FROM #PRNT.CH%, BUFF_TEXT$ = RECOUNT

	X% = RECOUNT%
	IF (X% / 276%) * 276% = X%
	THEN
		X% = 276%
	ELSE
		X% = 275%
	END IF

	FOR TEXT_LOOP% = 1% TO LEN(BUFF_TEXT$) STEP X%

		TEXT$ = MID(BUFF_TEXT$, TEXT_LOOP%, X%)
		GOSUB 2100

	NEXT TEXT_LOOP%

	GOTO 2000

2100	SETCODE$ = LEFT(TEXT$, 1%)

	SCOPE::SCOPE_EXIT = 0%

	GOSUB SetCode

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram
	END SELECT

	RETURN

	%Page

 ExitTotal:
	!*********************************************************************
	! totals
	!*********************************************************************

	CLOSE #PRNT.CH%

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 SetCode:
	!********************************************************************
	! Set codes records
	!*******************************************************************
	CODE$ = TEXT$

	RETURN UNLESS SETCODE$ = "W"

	SELECT SETCODE$
	CASE "A"
		!****************************************************
		! Code A Transmitter record
		!****************************************************

		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Record Identifier     " + CODE_A_RECID$, &
			2%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Payment Year          " + CODE_A_PAYYR$, &
			3%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Employer ID #         " + CODE_A_EIN$, &
			4%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Foreign Address Ind.  " + CODE_A_FORADD$, &
			5%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Transmitter Name      " + CODE_A_NAME$, &
			6%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Address               " + CODE_A_ADD$, &
			7%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"City                  " + CODE_A_CITY$, &
			8%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"State                 " + CODE_A_ST$, &
			9%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Foreign Postal Code   " + CODE_A_FPC$, &
			10%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Zip                   " + CODE_A_ZIP$, &
			11%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Zip code extension    " + CODE_A_ZIPEXT$, &
			12%, 5%)

	CASE "B"
		!****************************************************
		! Code B Basic Authorization Record
		!****************************************************

		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Record Identifier     " + CODE_B_RECID$, &
			2%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Payment Year          " + CODE_B_PAYYR$, &
			3%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Employer ID #         " + CODE_B_EIN$, &
			4%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Computer              " + CODE_B_COMTYP$, &
			5%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Internal Labeling     " + CODE_B_LABEL$, &
			6%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Density               " + CODE_B_DENSITY$, &
			7%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Recording Code        " + CODE_B_RECCOD$, &
			8%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Foreign Address Ind.  " + CODE_B_FORADD$, &
			9%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"File Return Name      " + CODE_B_NAME$, &
			10%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"File Return Address   " + CODE_B_ADD$, &
			11%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"File Return City      " + CODE_B_CITY$, &
			12%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"File Return State     " + CODE_B_ST$, &
			13%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Zip                   " + CODE_B_ZIP$, &
			14%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Zip Code Extension    " + CODE_B_ZIPEXT$, &
			14%, 5%)

	CASE "E"
		!****************************************************
		! Code E Employer/Establishment
		!****************************************************

		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Record Identifier     " + CODE_E_RECID$, &
			2%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Payment Year          " + CODE_E_PAYYR$, &
			3%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Employer ID #         " + CODE_E_EIN$, &
			4%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"State Local 69 Number " + CODE_E_SIN$, &
			5%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Name                  " + CODE_E_NAME$, &
			6%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Address               " + CODE_E_ADD$, &
			7%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"City                  " + CODE_E_CITY$, &
			8%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"State                 " + CODE_E_STATE$, &
			9%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Zip                   " + CODE_E_ZIP$, &
			10%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Zip code extension    " + CODE_E_ZIPEXT$, &
			11%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Name Code             " + CODE_E_NAMCODE$, &
			12%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Type of Employment    " + CODE_E_TYPEMP$, &
			13%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Blocking Factor       " + CODE_E_BLKFAC$, &
			14%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Establishment #       " + CODE_E_PRU$, &
			15%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Limitation of Lia     " + CODE_E_SLL$, &
			16%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Foreign Address Ind.  " + CODE_E_FORADD$, &
			17%, 5%)

	CASE "W"
		!****************************************************
		! Code W Employee Wage Record
		!****************************************************
		CALL ENTR_3MESSAGE(SCOPE, CODE_W_CN$, 1%)

		PR_EMP_MASTER::EMPNUM = CODE_W_CN$
		PR_EMP_MASTER::EMPNAME = CODE_W_NAME$
		PR_EMP_MASTER::ADD1 = CODE_W_ADD$
		PR_EMP_MASTER::ADD2 = ""
		PR_EMP_MASTER::CITY = CODE_W_CITY$
		PR_EMP_MASTER::STATE = CODE_W_ST$
		IF CODE_W_ZIPEXT$ = ""
		THEN
			PR_EMP_MASTER::ZIP = CODE_W_ZIP$
		ELSE
			PR_EMP_MASTER::ZIP = CODE_W_ZIP$ + "-" + CODE_W_ZIPEXT$
		END IF
		PR_EMP_MASTER::COUNTRY = ""
		PR_EMP_MASTER::PHONE = ""
		PR_EMP_MASTER::SSN = LEFT(CODE_W_SSN$, 3%) + "-" + &
			MID(CODE_W_SSN$, 4%, 2%) + "-" + &
			RIGHT(CODE_W_SSN$, 6%)
		PR_EMP_MASTER::SORT = CODE_W_NAME$
		PR_EMP_MASTER::SUBACC = ""
		PR_EMP_MASTER::ACCT = ""
		PR_EMP_MASTER::TRADE = ""
		PR_EMP_MASTER::OPER = ""
		PR_EMP_MASTER::UNION = ""
		PR_EMP_MASTER::LOCATION = ""
		PR_EMP_MASTER::DEPT = ""
		PR_EMP_MASTER::WORK_CENTER = ""
		PR_EMP_MASTER::EMP_SKILL = ""
		PR_EMP_MASTER::EMP_GRADE = ""
		PR_EMP_MASTER::DISABLED = ""
		PR_EMP_MASTER::PAYFREQ = 0.0
		PR_EMP_MASTER::RATE_TYPE = ""
		PR_EMP_MASTER::RATE_CDE = ""
		PR_EMP_MASTER::SUI_SW = ""
		PR_EMP_MASTER::TAX_PKG = ""
		PR_EMP_MASTER::WC = ""
		PR_EMP_MASTER::W2_1099 = ""
		PR_EMP_MASTER::BIRTH = ""
		PR_EMP_MASTER::HIREDAY = ""
		PR_EMP_MASTER::TERMDAY = ""
		PR_EMP_MASTER::REHIRE_FLAG = ""
		PR_EMP_MASTER::SEX = ""
		PR_EMP_MASTER::RACE = ""
		PR_EMP_MASTER::USCIT = "Y"
		PR_EMP_MASTER::WRKPERMIT = ""
		PR_EMP_MASTER::HOMCNTRY = ""
		PR_EMP_MASTER::ACTIVE_FLAG = ""

		PUT #PR_EMP_MASTER.CH%

	CASE "S"
		!****************************************************
		! Code S Supplemental State Record
		!****************************************************

		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Record Identifier     " + CODE_S_RECID$, &
			1%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Social Security #     " + CODE_S_SSN$, &
			2%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Employee Name         " + CODE_S_NAME$, &
			3%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Employee Address      " + CODE_S_ADD$, &
			4%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Employee City         " + CODE_S_CITY$, &
			5%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Employee State        " + CODE_S_ST$, &
			6%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Zip Code Extension    " + CODE_S_ZIPEXT$, &
			7%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Zip                   " + CODE_S_ZIP$, &
			8%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"State Code            " + CODE_S_STCODE1$, &
			9%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Optional Code         " + CODE_S_OPTIONAL$, &
			10%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Reporting Period      " + CODE_S_REPPER$, &
			11%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"SUI Total Wages       " + CODE_S_TOTWAG$, &
			12%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"SUI Taxable Wages     " + CODE_S_SUIWAG$, &
			13%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Number of Weeks Worked" + CODE_S_WEEKWORK$, &
			14%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Hire Date             " + CODE_S_HIREDATE$, &
			15%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Fire Date             " + CODE_S_FIREDATE$, &
			16%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Taxing Entity Code    " + CODE_S_ENTITYCODE1$, &
			17%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"State Emp. Account No " + CODE_S_SEAN$, &
			18%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"State Code            " + CODE_S_STCODE2$, &
			9%, 40%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"State Taxable Wages   " + CODE_S_STWAGE$, &
			10%, 40%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"State Tax Withheld    " + CODE_S_STTAX$, &
			11%, 40%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Other State Data      " + CODE_S_OTHDATA$, &
			12%, 40%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Tax Type Code         " + CODE_S_TAXTYP$, &
			13%, 40%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Taxing Entity Code    " + CODE_S_ENTITYCODE2$, &
			14%, 40%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Local Taxable Wages   " + CODE_S_LOCWAGE$, &
			15%, 40%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Local Tax Withheld    " + CODE_S_LOCTAX$, &
			16%, 40%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"State Control #       " + CODE_S_STCONNUM$, &
			17%, 40%)

	CASE "I"
		!****************************************************
		! Code I Intermediate Totals Record
		!****************************************************

		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Record Identifier     " + CODE_I_RECID$, &
			1%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"FICA Wages            " + CODE_I_FICA_WAGE$, &
			2%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"FICA Tips             " + CODE_I_FICA_TIP$, &
			3%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Annual Wages          " + CODE_I_ANNWAGE$, &
			4%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"FICA Withheld         " + CODE_I_FICA_WTHLD$, &
			5%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Federal Withheld      " + CODE_I_FED_WTHLD$, &
			6%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Control Number        " + CODE_I_CONTNUM$, &
			7%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Group Term Life Ins   " + CODE_I_LIFINS$, &
			8%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Uncollected FICA      " + CODE_I_UNCFICA$, &
			9%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Earned Income Credit  " + CODE_I_EIC$, &
			10%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Allocated Tips        " + CODE_I_ALLTIP$, &
			11%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Fringe Benefits       " + CODE_I_FR_BEN$, &
			12%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Deferred Comp Amount  " + CODE_I_DEFCOMP$, &
			13%, 5%)

	CASE "T"
		!****************************************************
		! Code T Total Record
		!****************************************************

		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Record Identifier     " + CODE_T_RECID$, &
			1%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Number of Employee    " + CODE_T_NUM_OF_EMP$, &
			2%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"FICA Wages            " + CODE_T_FICA_WAGE$, &
			3%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"FICA Tips             " + CODE_T_FICA_TIP$, &
			4%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Annual Wages          " + CODE_T_ANNWAGE$, &
			5%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"FICA Withheld         " + CODE_T_FICA_WTHLD$, &
			6%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Federal Withheld      " + CODE_T_FED_WTHLD$, &
			7%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Group Term Life Ins   " + CODE_T_LIFINS$, &
			8%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Uncollected FICA      " + CODE_T_UNCFICA$, &
			9%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Earned Income Credit  " + CODE_T_EIC$, &
			10%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Allocated Tips        " + CODE_T_ALLTIP$, &
			11%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Fringe Benefits       " + CODE_T_FR_BEN$, &
			12%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Deferred Comp Amount  " + CODE_T_DEFCOMP$, &
			13%, 5%)

	CASE "F"
		!****************************************************
		! Code F Final Record
		!****************************************************

		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Record Identifier     " + CODE_F_RECID$, &
			1%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Number of Employees   " + CODE_F_NUM_OF_EMP$, &
			2%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Soc. Sec. Wages       " + CODE_F_TOTALSSW$, &
			3%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Soc. Sec. Tips        " + CODE_F_TOTALSST$, &
			4%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Wages, Tips, Other    " + CODE_F_TOTALATW$, &
			5%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Soc. Sec. Withheld    " + CODE_F_TOTALSSTW$, &
			6%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Fed Tax Withheld      " + CODE_F_TOTALFITW$, &
			7%, 5%)
		ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Earned Income Credit  " + CODE_F_TOTALEIC$, &
			8%, 5%)
	END SELECT

	V% = SMG$END_PASTEBOARD_UPDATE(SCOPE::SMG_PBID)

	RETURN

	%Page

19000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	FILENAME$ = ""
	SELECT ERL

	!
	! End of file
	!
	CASE 2000%
		RESUME ExitTotal IF ERR = 11%
		FILENAME$ = TAPE_DEVICE$

	END SELECT

	!
	! Untrapped error
	!
	RESUME 19990

19990	!
	! This moved from inside error to outside so that errors occuring
	! at lower levels could be trapped.  Basic will not allow any
	! error to occur inside of an error no matter if it is in a
	! different module.
	!
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

	!
	! Handle the cases where a file couldn't be opened
	!
 ! NoFile:
 !	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
 !		"E", ERN$, FILENAME$, NUM1$(ERR))
 !
 !	GOTO ExitProgram

32767	END
