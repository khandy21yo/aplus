1	%TITLE "Payroll W2 Tape Process"
	%SBTTL "PR_SPEC_TAXFLOPPY"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2001 BY
	! Software Solutions, Inc.
	! Idaho Falls, Idaho  83402
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
	! Abstract:HELP
	!	.p
	!	The ^*Generate W-2 Floppy\* option
	!	in the W-2 Processing menu provides
	!	the means to generate the W-2 information on Floppy Disk when
	!	required to report that information to the Social Security
	!	Administration on magnetic media.
	!	.P
	!	This program will create a file which will then need to be
	!	copied to a MS-DOS floppy disk using ^*Kermit\* or some
	!	other file transfer routine.
	!	.p
	!	The requirements for the floppy disk are:
	!	1.2MB 5-1/4", 360K 5-1/4",
	!	1.44MB 3-1/2", or 720K 3-1/2".
	!	It must be given the label "^*W2REPORT\*" (use "^*LABEL A:W2REPORT\*").
	!	The data must be stored on the floppy in a file named "^*W2REPORT\*"
	!
	! Index:
	!	.X W-2 Floppy>Generate
	!	.x Generate>W-2 Floppy
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_SPEC_TAXFLOPPY
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_SPEC_TAXFLOPPY, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_SPEC_TAXFLOPPY.OBJ;*
	!
	! Author:
	!
	!	02/24/93 - Kevin Handy
	!		Taken from PR_SPEC_TAXTAPE_STATE and modified
	!		to output in floppy disk format, since our tape
	!		drive took a dive.
	!
	! Modification history:
	!
	!	04/06/93 - Kevin Handy
	!		Fixed bug caused by mixing CODE_W in with the
	!		CODE_1W, _2W, ...
	!
	!	04/06/93 - Kevin Handy
	!		Expand State ID Number (SIN$) field to 12 characters
	!		from the 9 that the tapes use.  (Don't ask me why
	!		the difference)
	!
	!	02/01/94 - Kevin Handy
	!		Modifications for defered compensation.
	!
	!	02/02/94 - Kevin Handy
	!		Added code to handle reading W2 location out of
	!		the PR_ERNDED_DEF file instead of having to input
	!		all that garbage every time W2's are run.
	!
	!	01/31/95 - Kevin Handy
	!		Added Deferec Compensation items.
	!
	!	02/15/95 - Kevin Handy
	!		Added handling of "w2.def" file so I could
	!		process NorthWest payroll data.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!		Fix last parameter of entr_3choice
	!
	!	06/26/95 - Kevin Handy
	!		Fixed bug where the defered compensation was totaled
	!		up twice.
	!
	!	09/12/96 - Kevin Handy
	!		Reformat source code.
	!
	!	03/13/97 - Kevin Handy
	!		Modified to handle FH codes
	!
	!	05/12/97 - Kevin Handy
	!		Lose FICA_RATE variable
	!
	!	05/16/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	02/28/99 - Kevin Handy
	!		Force SORTBY and STATE to be upper case
	!
	!	03/05/99 - Kevin Handy
	!		Make field 'OPTIONAL' to 'XOPTIONAL'
	!
	!	04/10/2000 - Kevin Handy
	!		Modified to handle 4 digit years in 2S record
	!
	!	10/12/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	11/03/2000 - Kevin Handy
	!		Use A"x"B
	!
	!	02/21/2000 - Kevin Handy
	!		Diminsion YYYY_FILE$() array so I can have more
	!		than 10 years listed.
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

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_STATE.HB"
	MAP (UTL_STATE) UTL_STATE_CDD UTL_STATE

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP (PR_ERNDED_DEF) PR_ERNDED_DEF_CDD PR_ERNDED_DEF

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAXTAPE.INC"

	!
	DIM PR_TAXES_STRUCT PR_TAXES(50%)
	DIM YYYY_FILE$(50%)

	!
	! External functions
	!
	EXTERNAL STRING	FUNCTION FUNC_REFORMAT
	EXTERNAL LONG   FUNCTION PR_FUNC_READTAXES

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!
	! List of Sort By
	!
	SORTBYTITLE$ = "Code  Description"

	SORTBY$(0%) = "5"
	SORTBY$(1%) = "NU    Employee Number"
	SORTBY$(2%) = "NA    Employee Name"
	SORTBY$(3%) = "SN    Social Security"
	SORTBY$(4%) = "LO    Location"
	SORTBY$(5%) = "SO    Alpha Sort Key"

	CALL ASSG_CHANNEL(PRNT.CH%, STAT%)

	!
	! Look up device
	!
	CALL READ_DEVICE("PR_REG_TAXES", PR_REG_TAXES.DEV$, STAT%)
	TAPE_DEVICE$ = "W2REPORT."

100	!******************************************************************
	! Get Year for file name
	!******************************************************************

	CALL FIND_FILE(PR_REG_TAXES.DEV$ + "PR_REG_TAXES_*.LED", &
		YYYY_FILE$(), &
		16%, "", "")

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram
	END SELECT

	YYYY_FILE% = VAL%(YYYY_FILE$(0%))

	IF YYYY_FILE%
	THEN
		YYYY_FILE$(LOOP%) = &
			MID(YYYY_FILE$(LOOP%), 14%, 4%) &
				FOR LOOP% = 1% TO YYYY_FILE%

		TEMP$ = "Payroll Year"

		X% = ENTR_3CHOICE(SCOPE, "", "", YYYY_FILE$(), "", &
			0%, TEMP$, "", 0%)

		IF X% > 0%
		THEN
			YYYY$ = EDIT$(YYYY_FILE$(X%), -1%)
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
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		80%, &
		SMG_SCREEN_DATA% &
	)

	!
	! Paint a prompt
	!
	SMG_STATUS% = SMG$PUT_CHARS &
	( &
		SMG_SCREEN_DATA%, &
		"!!!! THIS IS THE OLD VERSION !!!!", &
		4%, &
		24% &
	)
	SMG_STATUS% = SMG$PUT_CHARS &
	( &
		SMG_SCREEN_DATA%, &
		"Enter Year for W2", &
		6%, &
		28% &
	)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		1%, &
		1% &
	)

120	SCOPE::PRG_ITEM = "FLDYEAR"
	!++
	! Abstract:FLDYEAR
	!	^*Year\*
	!	.p
	!	The ^*Year\* field is to be entered with the year for which this report is
	!	to print on the tape.
	!	.p
	!	The format for entry is YYYY.
	!
	! Index:
	!	.x W-2 Tape>Year
	!	.x Year>W-2 Tape
	!
	!--
	YYYY$ = LEFT(DATE_TODAY, 4%)

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 8%, 38%, YYYY$, 0%, 0%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
		! Good key

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 120
	END SELECT

	YYYY$ = EDIT$(YYYY$, -1%)

	IF LEN(YYYY$) <> 4%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter the year in YYYY format", 0%)
		GOTO 120
	END IF

190	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

	!
	! Set up screen
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 80%, SMG_SCREEN_DATA%)
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 1%, 1%)

 AskTape:
	TEMP$ = "Output File <" + TAPE_DEVICE$ + "> "
	TOTAPE% = -1%

	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, TEMP$, 1%, 1%)

	JUNK$ = SPACE$(20%)
	SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, 1%, &
		LEN(TEMP$) + 2%, JUNK$, -1%, 16% + 4096%)

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
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO AskTape

	END SELECT

	IF JUNK$ <> ""
	THEN
		TAPE_DEVICE$ = JUNK$
	END IF

	IF TAPE_DEVICE$ = ""
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Please Enter Output File", 1%)
		GOTO AskTape
	END IF

	!***************************************************************
	! Open mag tape drive
	!***************************************************************

	!
	! Disk file (I Hope)
	!
	TOTAPE% = 0%
	open TAPE_DEVICE$ for output as file #prnt.ch%, &
		recordsize 132%, &
		allow read

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
		CONTINUE NoFile
	END WHEN

320	!
	! Open Earnings and Deduction register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.OPN"
	USE
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE NoFile
	END WHEN

330	!
	! Open Tax withholding register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.OPN"
	USE
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		CONTINUE NoFile
	END WHEN

340	!
	! Open Tax Profile file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.OPN"
		GET #PR_TAX_PROFILE.CH%, KEY #K_NUM% EQ "F  "
	USE
		FILENAME$ = "PR_TAX_PROFILE"
		CONTINUE NoFile
	END WHEN

	EIN$ = XLATE(PR_TAX_PROFILE_F::REPNO, STRING$(48%, 0%) + &
		"0123456789")

350	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.OPN"
		GET #PR_TAX_TABLE.CH%, KEY #0% EQ "F  "
	USE
		FILENAME$ = "PR_TAX_TABLE_" + YYYY$
		CONTINUE HelpError
	END WHEN

	FICA_LIMIT = PR_TAX_TABLE::FICA_LIMIT
	HI_LIMIT = PR_TAX_TABLE::FICA_LIMIT_HI
 !	FICA_RATE = PR_TAX_TABLE::FICA_EMPE_PCT / 100000.0

	FICA_EMPE_PCT = (PR_TAX_TABLE::FICA_EMPE_PCT) / 10000.0
	FICA_EMPE_PCT_HI = (PR_TAX_TABLE::FICA_EMPE_PCT_HI) / 10000.0

	IF FICA_EMPE_PCT > 0.10
	THEN
		FICA_EMPE_PCT = FICA_EMPE_PCT / 10.0
		FICA_EMPE_PCT_HI = FICA_EMPE_PCT_HI / 10.0
	END IF

360	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_STATE.OPN"
	USE
		FILENAME$ = "UTL_STATE"
		CONTINUE HelpError
	END WHEN

370	!
	! Open earnings/deduction definition file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"
	USE
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

380	!
	! Try to read in text file containing codes
	!
	FRM_CODES$ = ""

	CALL ASSG_CHANNEL(W2DEF.CH%, STATUS%)
	WHEN ERROR IN
		OPEN "W2.DEF" FOR INPUT AS FILE W2DEF.CH%
	USE
		CONTINUE 420
	END WHEN

385	WHEN ERROR IN
		LINPUT #W2DEF.CH%, X$
	USE
		CONTINUE 420
	END WHEN

	FRM_CODES$ = FRM_CODES$ + X$

	GOTO 385

420	!
	! Open Company Profile
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.OPN"

		!
		! Now get the Company data
		!
		GET #UTL_PROFILE.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "UTL_PROFILE"
		CONTINUE NoFile
	END WHEN

425	!
	! Open LOCATION file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
		GET #UTL_LOCATION.CH%, &
			KEY #0% EQ UTL_PROFILE::MAINLOCATION, &
			REGARDLESS
	USE
		FILENAME$ = "UTL_LOCATION"
		CONTINUE NoFile
	END WHEN

	!
	! Paint instructions on screen
	!
	ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"(01) Fringe Benefits Codes", 4%, 5%)
	ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"(02) Deferred Compensation Codes", 5%, 5%)
	ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"(05) Sort By", 8%, 5%)
	ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"(06) State", 9%, 5%)

	ENTER_CNT% = 6%
	M_LOOP% = 1%

	WHILE M_LOOP% <= ENTER_CNT%

 Eloop:		SCOPE::SCOPE_EXIT = 0%

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(M_LOOP%, "<0>##")

		SELECT M_LOOP%

		CASE 1%
	!++
	! Abstract:FLD001
	!
	!
	! Index:
	!--
			FR_BEN$ = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
				"4;37", "Fringe", &
				SPACE$(20%), 0%, "'E", "")

		CASE 2%
	!++
	! Abstract:FLD002
	!
	!
	! Index:
	!--
			DEF_COMP$ = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
				"5;37", "Deferred", &
				SPACE$(20%), 0%, "'E", "")

		CASE 5%
	!++
	! Abstract:FLD005
	!
	!
	! Index:
	!--
			SORTBY$ = ENTR_3STRINGLIST(SCOPE,  SMG_SCREEN_DATA%, &
				"8;37", "Sort By", SPACE$(2%), 16%, "'E", &
				"", SORTBY$(), SORTBYTITLE$, "005")

		CASE 6%
	!++
	! Abstract:FLD006
	!
	!
	! Index:
	!--
			SELECT_STATE$ = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
				"9;37", "State", &
				SPACE$(2%), 16%, "'E", "")

		END SELECT

		!
		! Test scope exit
		!
		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO ExitProgram

		!
		! Up Arrow
		!
		CASE SMG$K_TRM_UP
			IF M_LOOP% > 1%
			THEN
				M_LOOP% = M_LOOP% - 1%
			END IF

			GOTO Eloop

		!
		! Down Arrow
		!
		CASE SMG$K_TRM_DOWN
			IF M_LOOP% < ENTER_CNT%
			THEN
				M_LOOP% = M_LOOP% + 1%
			END IF

			GOTO Eloop

		!
		! Good keys
		!
		CASE 0%, 10%, 12%, 13%, 87%, 73%, 65%, &
			69%, 70%, 87%, SMG$K_TRM_DO

		!
		! Bad Keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO Eloop

		END SELECT

		M_LOOP% = M_LOOP% + 1%

	NEXT

	!
	! Erase displays
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_SCREEN_DATA%)

	!
	! Select sort sequence
	!
	SELECT SORTBY$
	CASE "NU"
		K_NUM% = 0%

	CASE "NA"
		K_NUM% = 1%

	CASE "SN"
		K_NUM% = 3%

	CASE "LO"
		K_NUM% = 4%

	CASE ELSE
		K_NUM% = 2%

	END SELECT

	%Page

2005	!*******************************************************************
	! Initialize the tape
	!*******************************************************************

	CALL ENTR_3MESSAGE(SCOPE, "Writing Tape.", 1% + 16%)

	TEST_42% = 0%

	TW_SSW = 0.0
	TW_SST = 0.0
	TW_ATW = 0.0
	TW_SSTW = 0.0
	TW_FITW = 0.0
	TW_AEIC = 0.0

	BUFF_FLOPPY$ = ""
	BUFF_CODE% = 0%

	SETCODE$ = "A" ! A.....
	GOSUB SetCode  ! Write to tape

	SETCODE$ = "B" ! B.....
	GOSUB SetCode  ! Write to tape

	SETCODE$ = "E" ! E.....
	GOSUB SetCode  ! Write to tape

	!*******************************************************************
	! Read through Payroll master file
	!*******************************************************************

	WHEN ERROR IN
		RESET #PR_EMP_MASTER.CH%, KEY #K_NUM%
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

2010	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE NoFile
	END WHEN

	W_FICA_WAGE = 0.0
	W_FICA_TIP = 0.0
	W_HI_WAGE = 0.0
	W_HI_WTHLD = 0.0
	W_ANNWAGE = 0.0
	W_FICA_WTHLD = 0.0
	W_FED_WTHLD = 0.0
	W_ALLTIP = 0.0
	W_DEFCOMP = 0.0
	W_FR_BEN = 0.0
	W_GTL = 0.0
	W_UNCOLL_FICA = 0.0
	W_EIC = 0.0
	W_NQP457 = 0.0
	W_NQPN457 = 0.0
	W_UNCFICA = 0.0
 !	W_DEFCOMP = 0.0
	TOTSTATE% = 0%
	ERnDED_FLAG% = 0%

2100	!*******************************************************************
	! Look up wages and taxes
	!*******************************************************************

	X% = PR_FUNC_READTAXES(PR_EMP_MASTER::EMPNUM, &
		PR_REG_TAXES.CH%, &
		PR_TAXES%, &
		PR_TAXES())

	FOR TAX_LOOP% = 1% TO PR_TAXES%
		ERNDED_FLAG% = -1%

		SELECT PR_TAXES(TAX_LOOP%)::TTYPE

		CASE "FW"

			W_FED_WTHLD = FUNC_ROUND(W_FED_WTHLD + &
				PR_TAXES(TAX_LOOP%)::TAX(4%), 2%)

			W_ANNWAGE = FUNC_ROUND(W_ANNWAGE + &
				PR_TAXES(TAX_LOOP%)::REPORTABLE(4%), 2%)

		CASE "FI"

			W_FICA_WAGE = FUNC_ROUND(W_FICA_WAGE + &
				PR_TAXES(TAX_LOOP%)::REPORTABLE(4%), 2%)

			!
			! The following is a kludge to force the fica wage
			! to come out correctly on the tape.
			! THIS SHOULD BE FIXED PROPERLY WHEN THE TIME IS AVAILABLE.
			!
			W_FICA_WAGE = FICA_LIMIT IF W_FICA_WAGE > FICA_LIMIT

			!
			! So we can hanle creating old tapes again
			!
			W_FICA_WTHLD = FUNC_ROUND(W_FICA_WTHLD + &
				PR_TAXES(TAX_LOOP%)::TAX(4%) + W_HI_WTHLD, 2%)

		CASE "FH"

			W_HI_WAGE = FUNC_ROUND(W_HI_WAGE + &
				PR_TAXES(TAX_LOOP%)::REPORTABLE(4%), 2%)

			!
			! The following is a kludge to force the fica wage
			! to come out correctly on the tape.
			! THIS SHOULD BE FIXED PROPERLY WHEN THE TIME
			! IS AVAILABLE.
			!
			W_HI_WAGE = HI_LIMIT IF W_HI_WAGE > HI_LIMIT

			!
			! So we can hanle creating old tapes again
			!
			W_HI_WTHLD = FUNC_ROUND(W_FICA_WTHLD + &
				PR_TAXES(TAX_LOOP%)::TAX(4%) + W_HI_WTHLD, 2%)

		CASE "SW"
			GOSUB AddState

			S_STTAX(THIS_STATE%) = &
				FUNC_ROUND(S_STTAX(THIS_STATE%) + &
				PR_TAXES(TAX_LOOP%)::TAX(4%), 2%)

			S_STWAGE(THIS_STATE%) = &
				FUNC_ROUND(S_STWAGE(THIS_STATE%) + &
				PR_TAXES(TAX_LOOP%)::REPORTABLE(4%), 2%)

			S_TOTWAG(THIS_STATE%) = &
				FUNC_ROUND(S_TOTWAG(THIS_STATE%) + &
				PR_TAXES(TAX_LOOP%)::REPORTABLE(4%), 2%)

			S_WEEKWORK(THIS_STATE%) = S_WEEKWORK(THIS_STATE%) + &
				PR_TAXES(TAX_LOOP%)::WKWRK(4%)

		CASE "SU"
			GOSUB AddState

			S_SUIWAG(THIS_STATE%) = &
				FUNC_ROUND(S_SUIWAG(THIS_STATE%) + &
				PR_TAXES(TAX_LOOP%)::REPORTABLE(4%), 2%)


		CASE "SX", "SI"
			GOSUB AddState

			S_OTHDATA(THIS_STATE%) = &
				FUNC_ROUND(S_OTHDATA(THIS_STATE%) + &
				PR_TAXES(TAX_LOOP%)::TAX(4%), 2%)

		END SELECT

		IF (PR_TAXES(LOOP%)::CODE <> "  ") AND &
			(COMP_STRING(EDIT$(PR_TAXES(LOOP%)::CODE, 132%), &
			FR_BEN$) <> 0%)
		THEN
			W_FR_BEN = FUNC_ROUND(W_FR_BEN + &
				PR_TAXES(TAX_LOOP%)::TAX(4%), 2%)
		END IF


2190	NEXT TAX_LOOP%

	!
	! If we have an old version that doesn't calculate the HI
	! separately, then fake up a calculation to fix the problem
	!
	IF W_HI_WAGE = 0.0
	THEN
		W_HI_WAGE = W_FICA_WTHLD
		W_HI_WAGE = HI_LIMIT IF W_HI_WAGE > HI_LIMIT

		TEMP = FUNC_ROUND(W_FICA_WAGE * FICA_EMPE_PCT, 2%)
		W_HI_WTHLD = W_FICA_WTHLD - TEMP
		W_FICA_WTHLD = TEMP

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
		! Calculate total amount (Assumption is that we will use it
		! if there is an ernded code)
		!
		AMOUNT = 0.0
		AMOUNT = AMOUNT + &
			PR_REG_ERNDED::QTR_DOLL(I%) &
			FOR I% = 0% TO 3%

		SELECT W2LOCATION$

		CASE "AE"		! Advanced earned income credit

			W_EIC = FUNC_ROUND(W_EIC + AMOUNT, 2%)

		CASE "AT"		! Allocated tips

			W_ALLTIP = FUNC_ROUND(W_ALLTIP + AMOUNT, 2%)

		CASE "CD"		! Defered Compensation

			W_DEFCOMP = FUNC_ROUND(W_DEFCOMP + AMOUNT, 2%)

		CASE "DA" TO "DZ"	! Random information (401k's)

			W_DEFCOMP = FUNC_ROUND(W_DEFCOMP + AMOUNT, 2%)

		CASE "FR"		! Fringe Benefits

			W_FR_BEN = FUNC_ROUND(W_FR_BEN + AMOUNT, 2%)

		CASE "NQ"		! Nonqualified plans

			W_NQP457 = FUNC_ROUND(W_NQP457 + AMOUNT, 2%)

		CASE "QU"		! Qualified plans

			W_NQPN457 = FUNC_ROUND(W_NQPN457 + AMOUNT, 2%)

		CASE "ST"		! Social Security Tips

			W_UNCFICA = FUNC_ROUND(W_UNCFICA + AMOUNT, 2%)

		END SELECT

		GOTO 2210
	END IF

	!
	! And here is the old way of doing things, to be used until
	! everyone switches over to the new version of ERNDED_DEF
	!
2230	GOTO 2210 IF PR_REG_ERNDED::ETYPE = "A"

	IF (COMP_STRING(PR_REG_ERNDED::CODE, FR_BEN$) <> 0%)
	THEN
		W_FR_BEN = W_FR_BEN + &
			PR_REG_ERNDED::QTR_DOLL(I%) &
			FOR I% = 0% TO 3%
	END IF

	IF (COMP_STRING(PR_REG_ERNDED::CODE, DEF_COMP$) <> 0%)
	THEN
		W_DEFCOMP = W_DEFCOMP + &
			PR_REG_ERNDED::QTR_DOLL(I%) &
			FOR I% = 0% TO 3%
	END IF

	GOTO 2210

2300	!*****************************************************************
	! Output to tape (Only if state information exists)
	!*****************************************************************

	IF ((TOTSTATE% <> 0%) AND (SELECT_STATE$ <> "")) OR &
		((SELECT_STATE$ = "") AND (ERNDED_FLAG% <> 0%))
	THEN
		!
		! Intermediate totals
		!
		I_FICA_WAGE = I_FICA_WAGE + W_FICA_WAGE
		I_HI_WAGE = I_HI_WAGE + W_HI_WAGE
		I_FICA_TIP = I_FICA_TIP + W_FICA_TIP
		I_ANNWAGE = I_ANNWAGE + W_ANNWAGE
		I_FICA_WTHLD = I_FICA_WTHLD + W_FICA_WTHLD
		I_HI_WTHLD = I_HI_WTHLD + W_HI_WTHLD
		I_FED_WTHLD = I_FED_WTHLD + W_FED_WTHLD
		I_LIFINS = I_LIFINS + W_LIFINS
		I_UNCFICA = I_UNCFICA + W_UNCFICA
		I_EIC = I_EIC + W_EIC
		I_NQP457 = I_NQP457 + W_NQP457
		I_NQPN457 = I_NQPN457 + W_NQPN457
		I_ALLTIP = I_ALLTIP + W_ALLTIP
		I_DEFCOMP = I_DEFCOMP + W_DEFCOMP
		I_FR_BEN = I_FR_BEN + W_FR_BEN

		!
		! Total Record
		!
		T_NUM_OF_EMP = T_NUM_OF_EMP + 1%
		T_FICA_WAGE = T_FICA_WAGE + W_FICA_WAGE
		T_HI_WAGE = T_HI_WAGE + W_HI_WAGE
		T_FICA_TIP = T_FICA_TIP + W_FICA_TIP
		T_ANNWAGE = T_ANNWAGE + W_ANNWAGE
		T_FICA_WTHLD = T_FICA_WTHLD + W_FICA_WTHLD
		T_HI_WTHLD = T_HI_WTHLD + W_HI_WTHLD
		T_FED_WTHLD = T_FED_WTHLD + W_FED_WTHLD
		T_LIFINS = T_LIFINS + W_LIFINS
		T_UNCFICA = T_UNCFICA + W_UNCFICA
		T_EIC = T_EIC + W_EIC
		T_NQP457 = T_NQP457 + W_NQP457
		T_NQPN457 = T_NQPN457 + W_NQPN457
		T_ALLTIP = T_ALLTIP + W_ALLTIP
		T_DEFCOMP = T_DEFCOMP + W_DEFCOMP
		T_FR_BEN = T_FR_BEN + W_FR_BEN
 !		T_DEFCOMP = T_DEFCOMP + W_DEFCOMP

		!
		! Final record
		!
		F_NUM_OF_EMP = F_NUM_OF_EMP + 1%

		SETCODE$ = "W"
		GOSUB SetCode

		IF (SELECT_STATE$ <> "")
		THEN
			SETCODE$ = "S"
			GOSUB SetCode
		END IF

		!
		! Check to see if 42nd form
		!
		TEST_42% = TEST_42% + 1%
		IF TEST_42%>=41%
		THEN
			SETCODE$ = "I"
			GOSUB SetCode

			I_FICA_WAGE = 0.0
			I_HI_WAGE = 0.0
			I_FICA_TIP = 0.0
			I_ANNWAGE = 0.0
			I_FICA_WTHLD = 0.0
			I_HI_WTHLD = 0.0
			I_FED_WTHLD = 0.0
			I_LIFINS = 0.0
			I_UNCFICA = 0.0
			I_EIC = 0.0
			I_NQP457 = 0.0
			I_NQPN457 = 0.0
			I_ALLTIP = 0.0
			I_DEFCOMP = 0.0
			I_FR_BEN = 0.0
 !			I_DEFCOMP = 0.0

			TEST_42% = 0%
		END IF
	END IF

2990	GOTO 2010

	%Page

 ExitTotal:
	!*********************************************************************
	! totals
	!*********************************************************************
	IF TEST_42% > 0%
	THEN
		SETCODE$ = "I"
		GOSUB SetCode

		TEST_42% = 0%
	END IF

	!******************************************************************
	! grand total
	!******************************************************************
	SETCODE$ = "T"
	GOSUB SetCode

	SETCODE$ = "F"
	GOSUB SetCode

	GOSUB ForceFinish

	CLOSE #PRNT.CH%		! Write EOF

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
	FLOPPY$ = ""

	SELECT SETCODE$
	CASE "A"
		!****************************************************
		! Code A Transmitter record
		!****************************************************
		CODE_1A::RECID	= "1A"
		CODE_1A::PAYYR	= YYYY$
		CODE_1A::EIN	= EIN$
		CODE_1A::FORADD	= ""
		CODE_1A::ENAME	= EDIT$(UTL_PROFILE::REP_NAME, 32%)
		TEMP$ = EDIT$(UTL_LOCATION::ADDRESS1, 132%)
		TEMP$ = TEMP$ + " " IF TEMP$ <> ""
		CODE_1A::ADD	= EDIT$(TEMP$ + UTL_LOCATION::ADDRESS2, 32%)

		GOSUB Putrecord

		FLOPPY$ = ""

		CODE_2A::RECID	= "2A"
		CODE_2A::CITY	= EDIT$(UTL_LOCATION::CITY, 32%)
		CODE_2A::ST	= UTL_LOCATION::STATE
		CODE_2A::ZIP	= UTL_LOCATION::ZIP
		CODE_2A::ZIPEXT	= FNZIPEXT$(UTL_LOCATION::ZIP)

		GOSUB Putrecord

	CASE "B"
		!****************************************************
		! Code B Basic Authorization Record
		!****************************************************
		CODE_1B::RECID	= "1B"
		CODE_1B::PAYYR	= YYYY$
		CODE_1B::EIN	= EIN$
		CODE_1B::COMTYP	= "DEC"

		GOSUB Putrecord

		FLOPPY$ = ""

		CODE_2B::RECID	= "2B"
		CODE_2B::FORADD	= ""
		CODE_2B::ENAME	= EDIT$(UTL_PROFILE::REP_NAME, 32%)
		TEMP$ = EDIT$(UTL_LOCATION::ADDRESS1, 132%)
		TEMP$ = TEMP$ + " " IF TEMP$ <> ""
		CODE_2B::ADD	= EDIT$(TEMP$ + UTL_LOCATION::ADDRESS2, 32%)
		CODE_2B::CITY	= EDIT$(UTL_LOCATION::CITY, 32%)
		CODE_2B::ST	= UTL_LOCATION::STATE
		CODE_2B::ZIP	= UTL_LOCATION::ZIP
		CODE_2B::ZIPEXT	= FNZIPEXT$(UTL_LOCATION::ZIP)

		GOSUB Putrecord

	CASE "E"
		!****************************************************
		! Code E Employer/Establishment
		!****************************************************
		CODE_1E::RECID	= "1E"
		CODE_1E::PAYYR	= YYYY$
		CODE_1E::EIN	= EIN$
		CODE_1E::ESIN	= ""
		CODE_1E::ENAME	= EDIT$(UTL_PROFILE::REP_NAME, 32%)
		TEMP$ = EDIT$(UTL_LOCATION::ADDRESS1, 132%)
		TEMP$ = TEMP$ + " " IF TEMP$ <> ""
		CODE_1E::ADD	= EDIT$(TEMP$ + UTL_LOCATION::ADDRESS2, 32%)
		CODE_1E::FORADD	= ""

		GOSUB Putrecord

		FLOPPY$ = ""

		CODE_2E::RECID	= "2E"
		CODE_2E::CITY	= EDIT$(UTL_LOCATION::CITY, 32%)
		CODE_2E::STATE	= UTL_LOCATION::STATE
		CODE_2E::ZIPEXT	= FNZIPEXT$(UTL_LOCATION::ZIP)
		CODE_2E::ZIP	= UTL_LOCATION::ZIP
		CODE_2E::NAMCODE= "F"
		CODE_2E::TYPEMP	= "R"
		CODE_2E::PRU	= ""

		GOSUB Putrecord

	CASE "W"
		!****************************************************
		! Code W Employee Wage Record
		!****************************************************
		IF PR_EMP_MASTER::UNION = ""
		THEN
			PENPLAN_FLAG$ = " "
		ELSE
			PENPLAN_FLAG$ = "P"
		END IF

		CODE_1W::RECID	= "1W"
		CODE_1W::SSN	= XLATE(PR_EMP_MASTER::SSN, &
			STRING$(48%, 0%) + "0123456789")
		CODE_1W::SSN	= "I" IF CODE_1W::SSN = ""
		CODE_1W::ENAME	= EDIT$(PR_EMP_MASTER::EMPNAME, 32%)
		TEMP$ = EDIT$(PR_EMP_MASTER::ADD1, 132%)
		TEMP$ = TEMP$ + " " IF TEMP$ <> ""
		CODE_1W::ADD	= EDIT$(TEMP$ + PR_EMP_MASTER::ADD2, 32%)
		TEMP$ = EDIT$(PR_EMP_MASTER::CITY, 32%+128%)
		TEMP$ = LEFT(TEMP$, LEN(TEMP$) - 1%) &
			IF RIGHT(TEMP$, LEN(TEMP$)) = ","
		CODE_1W::CITY	= TEMP$
		CODE_1W::ST	= PR_EMP_MASTER::STATE
		CODE_1W::ZIPEXT	= FNZIPEXT$(PR_EMP_MASTER::ZIP)
		CODE_1W::ZIP	= PR_EMP_MASTER::ZIP
		CODE_1W::SEC	= ""

		GOSUB Putrecord

		FLOPPY$ = ""
		CODE_2W::RECID	= "2W"
		CODE_2W::FICA_WAGE= FUNC_REFORMAT(W_FICA_WAGE, &
			LEN(CODE_2W::FICA_WAGE), 2%)
		CODE_2W::FICA_TIP= FUNC_REFORMAT(W_FICA_TIP, &
			LEN(CODE_2W::FICA_TIP), 2%)
		CODE_2W::ANNWAGE	= FUNC_REFORMAT(W_ANNWAGE, &
			LEN(CODE_2W::ANNWAGE), 2%)
		CODE_2W::FICA_WTHLD= FUNC_REFORMAT(W_FICA_WTHLD, &
			LEN(CODE_2W::FICA_WTHLD), 2%)
		CODE_2W::FED_WTHLD = FUNC_REFORMAT(W_FED_WTHLD, &
			LEN(CODE_2W::FED_WTHLD), 2%)
		CODE_2W::NQP457 = FUNC_REFORMAT(W_NQP457, &
			LEN(CODE_2W::NQP457), 2%)
		CODE_2W::NQPN457 = FUNC_REFORMAT(W_NQPN457, &
			LEN(CODE_2W::NQPN457), 2%)
		CODE_2W::CN	= PR_EMP_MASTER::EMPNUM
		CODE_2W::GTL	= FUNC_REFORMAT(W_GTL, &
			LEN(CODE_2W::GTL), 2%)
		CODE_2W::UNCOLL_FICA = FUNC_REFORMAT(W_UNCOLL_FICA, &
			LEN(CODE_2W::UNCOLL_FICA), 2%)
		CODE_2W::EIC	= FUNC_REFORMAT(W_EIC, &
			LEN(CODE_2W::EIC), 2%)
		CODE_2W::ALLTIP	= FUNC_REFORMAT(W_ALLTIP, &
			LEN(CODE_2W::ALLTIP), 2%)
		CODE_2W::DEFCOMP = FUNC_REFORMAT(W_DEFCOMP, &
			LEN(CODE_2W::DEFCOMP), 2%)
		CODE_2W::FR_BEN	= FUNC_REFORMAT(W_FR_BEN, &
			LEN(CODE_2W::FR_BEN), 2%)
		CODE_2W::PENPLAN_FLAG = PENPLAN_FLAG$
		IF (W_DEFCOMP > 0)
		THEN
			CODE_2W::DEFCOMP_FLAG = "D"
		ELSE
			CODE_2W::DEFCOMP_FLAG = ""
		END IF
		CODE_2W::DCB = FUNC_REFORMAT(0.0, &
			LEN(CODE_2W::DCB), 2%)

		GOSUB Putrecord

		FLOPPY$ = ""

		CODE_3W::RECID	= "3W"
		CODE_3W::MWAT	= FUNC_REFORMAT(W_HI_WAGE, &
			LEN(CODE_3W::MWAT), 2%)
		CODE_3W::MTW	= FUNC_REFORMAT(W_HI_WTHLD, &
			LEN(CODE_3W::MTW), 2%)

		GOSUB Putrecord

		TW_SSW = TW_SSW + W_FICA_WAGE
		TW_SST = TW_SST + W_FICA_TIP
		!TW_HIW = TW_HIW + W_HI_WAGE
		TW_HITW = TW_HITW + W_HI_WTHLD
		TW_ATW = TW_ATW + W_ANNWAGE
		TW_SSTW = TW_SSTW + W_FICA_WTHLD
		TW_FITW = TW_FITW + W_FED_WTHLD
		TW_AEIC = TW_AEIC + W_EIC

	CASE "S"
		!****************************************************
		! Code S Supplemental State Record
		!****************************************************
		!
		! Create one record per state
		!
		FOR Z% = 1% TO TOTSTATE%
			STATE$ = THIS_STATE$(Z%)
			GOSUB GetState

			FLOPPY$ = ""

			CODE_1S::RECID	= "1S"
			CODE_1S::SSN	= XLATE(PR_EMP_MASTER::SSN, &
				STRING$(48%, 0%) + "0123456789")
			CODE_1S::SSN	= "I" IF CODE_1S::SSN = ""
			CODE_1S::ENAME	= EDIT$(PR_EMP_MASTER::EMPNAME, 32%)
			TEMP$ = EDIT$(PR_EMP_MASTER::ADD1, 132%)
			TEMP$ = TEMP$ + " " IF TEMP$ <> ""
			CODE_1S::ADD = EDIT$(TEMP$ + PR_EMP_MASTER::ADD2, 32%)
			TEMP$ = EDIT$(PR_EMP_MASTER::CITY, 32%+128%)
			TEMP$ = LEFT(TEMP$, LEN(TEMP$) - 1%) &
				IF RIGHT(TEMP$, LEN(TEMP$)) = ","
			CODE_1S::CITY	= TEMP$
			CODE_1S::ST	= PR_EMP_MASTER::STATE
			CODE_1S::ZIPEXT	= FNZIPEXT$(PR_EMP_MASTER::ZIP)
			CODE_1S::ZIP	= PR_EMP_MASTER::ZIP
			CODE_1S::STCODE1 = ST_FIPS$ ! Fips Postal Numeric Code
			CODE_1S::XOPTIONAL = ""

			GOSUB Putrecord

			FLOPPY$ = ""

			CODE_2S::RECID	= "2S"
			CODE_2S::HIREDATE= MID(PR_EMP_MASTER::HIREDAY, 5%, 2%) + &
				MID(PR_EMP_MASTER::HIREDAY, 1%, 4%)
			CODE_2S::HIREDATE= "" IF CODE_2S::HIREDATE = "000000"
			CODE_2S::FIREDATE= MID(PR_EMP_MASTER::TERMDAY, 5%, 2%) + &
				MID(PR_EMP_MASTER::TERMDAY, 4%, 4%)
			CODE_2S::FIREDATE= "" IF CODE_2S::FIREDATE = "0000"
			CODE_2S::TAXTYP	= "F"
			CODE_2S::ENTITYCODE1 = ""
			CODE_2S::ENTITYCODE2 = ""
			CODE_2S::LOCWAGE = FUNC_REFORMAT(S_LOCWAGE, &
				LEN(CODE_2S::LOCWAGE), 2%)
			CODE_2S::LOCTAX	= FUNC_REFORMAT(S_LOCTAX, &
				LEN(CODE_2S::LOCTAX), 2%)
			CODE_2S::STCONNUM= ""
			CODE_2S::REPPER	= "12" + MID(YYYY$, 1%, 4%)

			CODE_2S::SEAN	= SIN$
			CODE_2S::STCODE2 = ST_FIPS$ ! FIPS postal numeric code

			CODE_2S::TOTWAG	= FUNC_REFORMAT(S_TOTWAG(Z%), &
				LEN(CODE_2S::TOTWAG), 2%)
			CODE_2S::SUIWAG	= FUNC_REFORMAT(S_SUIWAG(Z%), &
				LEN(CODE_2S::SUIWAG), 2%)
			CODE_2S::WEEKWORK= FUNC_REFORMAT(S_WEEKWORK(Z%), &
				LEN(CODE_2S::WEEKWORK), 0%)
			CODE_2S::STWAGE	= FUNC_REFORMAT(S_STWAGE(Z%), &
				LEN(CODE_2S::STWAGE), 2%)
			CODE_2S::STTAX	= FUNC_REFORMAT(S_STTAX(Z%), &
				LEN(CODE_2S::STTAX), 2%)
			CODE_2S::OTHDATA = FUNC_REFORMAT(S_OTHDATA(Z%), &
				LEN(CODE_2S::OTHDATA), 2%)

			GOSUB Putrecord
		NEXT Z%

	CASE "I"
		!****************************************************
		! Code I Intermediate Totals Record
		!****************************************************
		CODE_1I::RECID	= "1I"
		CODE_1I::FICA_WAGE= FUNC_REFORMAT(I_FICA_WAGE, &
			LEN(CODE_1I::FICA_WAGE), 2%)
		CODE_1I::FICA_TIP= FUNC_REFORMAT(I_FICA_TIP, &
			LEN(CODE_1I::FICA_TIP), 2%)
		CODE_1I::ANNWAGE = FUNC_REFORMAT(I_ANNWAGE, &
			LEN(CODE_1I::ANNWAGE), 2%)
		CODE_1I::FICA_WTHLD= FUNC_REFORMAT(I_FICA_WTHLD, &
			LEN(CODE_1I::FICA_WTHLD), 2%)
		CODE_1I::FED_WTHLD= FUNC_REFORMAT(I_FED_WTHLD, &
			LEN(CODE_1I::FED_WTHLD), 2%)
		CODE_1I::CONTNUM = ""
		CODE_1I::LIFINS	= FUNC_REFORMAT(I_LIFINS, &
			LEN(CODE_1I::LIFINS), 2%)
		CODE_1I::UNCFICA = FUNC_REFORMAT(I_UNCFICA, &
			LEN(CODE_1I::UNCFICA), 2%)
		CODE_1I::EIC	= FUNC_REFORMAT(I_EIC, &
			LEN(CODE_1I::EIC), 2%)
		CODE_1I::ALLTIP	= FUNC_REFORMAT(I_ALLTIP, &
			LEN(CODE_1I::ALLTIP), 2%)
		CODE_1I::DEFCOMP = FUNC_REFORMAT(I_DEFCOMP, &
			LEN(CODE_1I::DEFCOMP), 2%)
		CODE_1I::FR_BEN	= FUNC_REFORMAT(I_FR_BEN, &
			LEN(CODE_1I::FR_BEN), 2%)

		GOSUB Putrecord

		FLOPPY$ = ""

		CODE_2I::RECID	= "2I"
		CODE_2I::MWAT	= FUNC_REFORMAT(I_HI_WAGE, &
			LEN(CODE_2I::MWAT), 2%)
		CODE_2I::MTW	= FUNC_REFORMAT(I_HI_WTHLD, &
			LEN(CODE_2I::MTW), 2%)
		CODE_2I::DCB	= FUNC_REFORMAT(0.0, &
			LEN(CODE_2I::DCB), 2%)
		CODE_2I::NQP457	= FUNC_REFORMAT(I_NQP457, &
			LEN(CODE_2I::NQP457), 2%)
		CODE_2I::NQPN457= FUNC_REFORMAT(I_NQPN457, &
			LEN(CODE_2I::NQPN457), 2%)

		GOSUB Putrecord

	CASE "T"
		!****************************************************
		! Code T Total Record
		!****************************************************
		CODE_1T::RECID	= "1T"
		CODE_1T::NUM_OF_EMP = FUNC_REFORMAT(T_NUM_OF_EMP, &
			LEN(CODE_1T::NUM_OF_EMP), 0%)
		CODE_1T::FICA_WAGE = FUNC_REFORMAT(T_FICA_WAGE, &
			LEN(CODE_1T::FICA_WAGE), 2%)
		CODE_1T::FICA_TIP = FUNC_REFORMAT(T_FICA_TIP, &
			LEN(CODE_1T::FICA_TIP), 2%)
		CODE_1T::ANNWAGE = FUNC_REFORMAT(T_ANNWAGE, &
			LEN(CODE_1T::ANNWAGE), 2%)
		CODE_1T::FICA_WTHLD= FUNC_REFORMAT(T_FICA_WTHLD, &
			LEN(CODE_1T::FICA_WTHLD), 2%)
		CODE_1T::FED_WTHLD= FUNC_REFORMAT(T_FED_WTHLD, &
			LEN(CODE_1T::FED_WTHLD), 2%)
		CODE_1T::LIFINS	= FUNC_REFORMAT(T_LIFINS, &
			LEN(CODE_1T::LIFINS), 2%)
		CODE_1T::UNCFICA = FUNC_REFORMAT(T_UNCFICA, &
			LEN(CODE_1T::UNCFICA), 2%)
		CODE_1T::EIC	= FUNC_REFORMAT(T_EIC, &
			LEN(CODE_1T::EIC), 2%)
		CODE_1T::ALLTIP	= FUNC_REFORMAT(T_ALLTIP, &
			LEN(CODE_1T::ALLTIP), 2%)

		GOSUB Putrecord

		FLOPPY$ = ""

		CODE_2T::RECID	= "2T"
		CODE_2T::FR_BEN	= FUNC_REFORMAT(T_FR_BEN, &
			LEN(CODE_2T::FR_BEN), 2%)
		CODE_2T::DEFCOMP = FUNC_REFORMAT(T_DEFCOMP, &
			LEN(CODE_2T::DEFCOMP), 2%)
		CODE_2T::MWAT	= FUNC_REFORMAT(T_HI_WAGE, &
			LEN(CODE_2T::MWAT), 2%)
		CODE_2T::MTW	= FUNC_REFORMAT(T_HI_WTHLD, &
			LEN(CODE_2T::MTW), 2%)
		CODE_2T::DCB	= FUNC_REFORMAT(0.0, &
			LEN(CODE_2T::DCB), 2%)
		CODE_2T::NQP457	= FUNC_REFORMAT(0.0, &
			LEN(CODE_2T::NQP457), 2%)
		CODE_2T::NQPN457 = FUNC_REFORMAT(0.0, &
			LEN(CODE_2T::NQPN457), 2%)

		GOSUB Putrecord

	CASE "F"
		!****************************************************
		! Code F Final Record
		!****************************************************
		CODE_1F::RECID	= "1F"
		CODE_1F::NUM_OF_EMP= FUNC_REFORMAT(F_NUM_OF_EMP, &
			LEN(CODE_1F::NUM_OF_EMP), 0%)
		CODE_1F::TOTALSSW= FUNC_REFORMAT(TW_SSW, &
			LEN(CODE_1F::TOTALSSW), 2%)
		CODE_1F::TOTALSST= FUNC_REFORMAT(TW_SST, &
			LEN(CODE_1F::TOTALSST), 2%)
		CODE_1F::TOTALATW= FUNC_REFORMAT(TW_ATW, &
			LEN(CODE_1F::TOTALATW), 2%)
		CODE_1F::TOTALSSTW= FUNC_REFORMAT(TW_SSTW+TW_HITW, &
			LEN(CODE_1F::TOTALSSTW), 2%)
		CODE_1F::TOTALFITW= FUNC_REFORMAT(TW_FITW, &
			LEN(CODE_1F::TOTALFITW), 2%)
		CODE_1F::TOTALAEIC= FUNC_REFORMAT(TW_AEIC, &
			LEN(CODE_1F::TOTALAEIC), 2%)

		GOSUB Putrecord

	END SELECT

	RETURN

	%Page

	!*******************************************************************
	! Output one record to tape device
	!*******************************************************************

 Putrecord:
	!
	! Disk
	!
	PRINT #PRNT.CH%, FLOPPY$

	RETURN

	%PAGE

	!*******************************************************************
	! Force last buffer (Not a full one) to tape
	!*******************************************************************

 ForceFinish:

	RETURN

	%PAGE

	!*******************************************************************
	! Function to find a place for a state
	!*******************************************************************
 AddState:
	!
	! Handle if only doing one state
	!
	IF (SELECT_STATE$ = "") OR (SELECT_STATE$ = PR_TAXES(TAX_LOOP%)::CODE)
	THEN
		!
		! Search to see if already created
		!
		FOR I% = 1% TO TOTSTATE%
			IF THIS_STATE$(I%) = PR_TAXES(TAX_LOOP%)::CODE
			THEN
				THIS_STATE% = I%
				RETURN
			END IF
		NEXT I%

		!
		! Create new state
		!
		THIS_STATE%, TOTSTATE% = TOTSTATE% + 1%
		THIS_STATE$(THIS_STATE%) = PR_TAXES(TAX_LOOP%)::CODE
		S_STTAX(THIS_STATE%) = 0.0
		S_STWAGE(THIS_STATE%) = 0.0
		S_TOTWAG(THIS_STATE%) = 0.0
		S_WEEKWORK(THIS_STATE%) = 0.0
		S_SUIWAG(THIS_STATE%) = 0.0
		S_OTHDATA(THIS_STATE%) = 0.0
	END IF

	RETURN

	%PAGE

 GetState:
18000	!*******************************************************************
	! Set up the state ID numbers
	!*******************************************************************

	SIN$, ST_FIPS$ = ""
	IF (PR_TAX_PROFILE_S::AUTH <> "S") OR (PR_TAX_PROFILE_S::CODE <> STATE$)
	THEN
		WHEN ERROR IN
			GET #PR_TAX_PROFILE.CH%, &
				KEY #0% EQ "S" + STATE$, &
				REGARDLESS
		USE
			CONTINUE 18010 IF ERR = 155%
			FILENAME$ = "PR_TAX_PROFILE"
			CONTINUE NoFile
		END WHEN
	END IF
	SIN$ = TRM$(PR_TAX_PROFILE_S::REPNO)

	IF SELECT_STATE$ = "ID"
	THEN
		I% = INSTR(1%, SIN$, "-")
		I% = LEN(SIN$) + 1% IF I% = 0%

		SIN$ = STRING$(13% - I%, A"0"B) + LEFT(SIN$, I% - 1%)
	END IF

18010	!
	! Get the FIPS code for this state.
	!
	IF (UTL_STATE::COUNTRY <> "US") OR (UTL_STATE::STATE <> STATE$)
	THEN
		WHEN ERROR IN
			GET #UTL_STATE.CH%, KEY #0% EQ "US" + STATE$, REGARDLESS
		USE
			CONTINUE 18090 IF ERR = 155%
			FILENAME$ = "UTL_STATE"
			CONTINUE NoFile
		END WHEN
	END IF

	ST_FIPS$ = UTL_STATE::FIPS

18090	RETURN

	%PAGE

	!*******************************************************************
	! Function to strip off extended zip code part
	!*******************************************************************

	DEF FNZIPEXT$(FULLZIP$)

		EXTZIP$ = RIGHT(FULLZIP$, 6%)
		EXTZIP$ = RIGHT(EXTZIP$, 2%) IF LEFT(EXTZIP$, 1%) = "-"

		FNZIPEXT$ = EXTZIP$

	END DEF

	%PAGE

 HelpError:
	!
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
 NoFile:
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

32767	END
