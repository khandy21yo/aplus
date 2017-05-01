1	%TITLE "Payroll W2 Tape Process"
	%SBTTL "PR_SPEC_TAXFLOPPY_2005"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2006 BY
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
	!	$ BAS PR_SOURCE:PR_SPEC_TAXFLOPPY_2005
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_SPEC_TAXFLOPPY_2005, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_SPEC_TAXFLOPPY_2005.OBJ;*
	!
	! Author:
	!
	!	01/30/2006 - Kevin Handy
	!		Taken from PR_SPEC_TAXFLOPPY_2004 and modified
	!		for new format
	!
	! Modification history:
	!
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

	!*******************************************************************
	! Define records for the various codes used by tax tape
	!*******************************************************************
	!
	!
	! Transmitter record
	!
	RECORD CODE_RA_STRUCT
		STRING RECID		= 2%
		STRING EIN		= 9%
		STRING PIN		= 8%
		STRING FILL		= 9%
		STRING RESUB		= 1%
		STRING RESUBTLCN	= 6%
		STRING SOFTCODE		= 2%
		STRING ENAME		= 57%
		STRING LADD		= 22%
		STRING DADD		= 22%
		STRING CITY		= 22%
		STRING ST		= 2%
		STRING ZIP		= 5%
		STRING ZIPEXT		= 4%
		STRING FILL		= 5%
		STRING PROVINCE		= 23%
		STRING FZIP		= 15%
		STRING COUNTRY		= 2%
		STRING SUBMITTER	= 57%
		STRING SLADDRESS	= 22%
		STRING SDADDRESS	= 22%
		STRING SCITY		= 22%
		STRING SSTATE		= 2%
		STRING SZIP		= 5%
		STRING SZIPEXT		= 4%
		STRING FILL		= 5%
		STRING SPROVINCE	= 23%
		STRING SFZIP		= 15%
		STRING SCOUNTRY		= 2%
		STRING SCONTACT		= 27%
		STRING SPHONE		= 15%
		STRING SPHONEEXT	= 5%
		STRING FILL		= 3%
		STRING SEMAIL		= 40%
		STRING FILL		= 3%
		STRING SFAX		= 10%
		STRING PROBLEM		= 1%
		STRING PREPARER		= 1%
		STRING FILL		= 12%
	END RECORD

	!
	! Employer Records
	!
	RECORD CODE_RE_STRUCT
		STRING RECID		= 2%
		STRING PAYYR		= 4%
		STRING AGENT		= 1%
		STRING EIN		= 9%
		STRING ESIN		= 9%
		STRING TERMBUS		= 1%
		STRING ESTNO		= 4%
		STRING OTHEREIN		= 9%
		STRING ENAME		= 57%
		STRING LOCADD		= 22%
		STRING ADD		= 22%
		STRING CITY		= 22%
		STRING STATE		= 2%
		STRING ZIP		= 5%
		STRING ZIPEXT		= 4%
		STRING FILL		= 5%
		STRING FORSTATE		= 23%
		STRING FORZIP		= 15%
		STRING CTYCODE		= 2%
		STRING EMPCODE		= 1%
		STRING TAXJOUR		= 1%
		STRING THIRDPARTY	= 1%
		STRING FILL		= 291%
	END RECORD

	!
	! Employee Wage Records
	!
	RECORD CODE_RW_STRUCT
		STRING RECID		= 2%
		STRING SSN		= 9%
		STRING ENAME		= 15%
		STRING ENAMEMID		= 15%
		STRING ENAMELAST	= 20%
		STRING SUFFIX		= 4%
		STRING LOCADD		= 22%
		STRING ADD		= 22%
		STRING CITY		= 22%
		STRING ST		= 2%
		STRING ZIP		= 5%
		STRING ZIPEXT		= 4%
		STRING FILL		= 5%
		STRING FORSTATE		= 23%
		STRING FORZIP		= 15%
		STRING CTYCODE		= 2%
		STRING FED_WAGE		= 11%
		STRING FED_WTHLD	= 11%
		STRING FICA_WAGE	= 11%
		STRING FICA_WTHLD	= 11%
		STRING MED_WAGE		= 11%
		STRING MED_WTHLD	= 11%
		STRING FICA_TIP		= 11%
		STRING EIC		= 11%
		STRING DCB		= 11%
		STRING DEFCOMP401	= 11%
		STRING DEFCOMP403	= 11%
		STRING DEFCOMP408	= 11%
		STRING DEFCOMP457	= 11%
		STRING DEFCOMP501	= 11%
		STRING MEBQSCP		= 11%
		STRING NQP457		= 11%
		STRING ECHSA		= 11%
		STRING NQPN457		= 11%
		STRING NCP		= 11%
		STRING FILL		= 11%
		STRING ECPGTLI		= 11%
		STRING IENSO		= 11%
		STRING DUS409		= 11%
		STRING FILL		= 45%
		STRING SEC		= 1%
		STRING FILL		= 1%
		STRING PENPLAN_FLAG	= 1%
		STRING TPSPI		= 1%
		STRING FILL		= 23%
	END RECORD

	!
	! Employee wage record
	!
	RECORD CODE_RO_STRUCT
		STRING RECID		= 2%
		STRING FILL		= 8%
		STRING ALLTIP		= 11%
		STRING UNCTIPTAX	= 11%
		STRING MSA		= 11%
		STRING SRA		= 11%
		STRING QAE		= 11%
		STRING USSRT		= 11%
		STRING UMT		= 11%
		STRING IUS409A		= 11%
		STRING FILL		= 165%
		STRING CS		= 1%
		STRING SPOSSN		= 9%
		STRING WSPR		= 11%
		STRING CSTPR		= 11%
		STRING ASPR		= 11%
		STRING TSPR		= 11%
		STRING TWTASPR		= 11%
		STRING PRTW		= 11%
		STRING RFAC		= 11%
		STRING FILL		= 11%
		STRING TWTOCSXX		= 11%
		STRING VIGXX		= 11%
		STRING FILL		= 128%
	END RECORD

	!
	! Supplimantal records
	!
	RECORD CODE_RS_STRUCT
		STRING RECID		= 2%
		STRING STCODE1		= 2%
		STRING TAXINGENT	= 5%
		STRING SSN		= 9%
		STRING ENAME		= 15%
		STRING ENAMEMIDDLE	= 15%
		STRING ENAMELAST	= 20%
		STRING SUFFIX		= 4%
		STRING LOCADD		= 22%
		STRING ADD		= 22%
		STRING CITY		= 22%
		STRING ST		= 2%
		STRING ZIP		= 5%
		STRING ZIPEXT		= 4%
		STRING FILL		= 5%
		STRING FORSTATE		= 23%
		STRING FORZIP		= 15%
		STRING CTYCODE		= 2%
		STRING XOPTIONAL	= 2%
		STRING REPPER		= 6%
		STRING TOTWAG		= 11%
		STRING SUIWAG		= 11%
		STRING WEEKWORK		= 2%
		STRING HIREDATE		= 8%
		STRING FIREDATE		= 8%
		STRING FILL		= 5%
		STRING SEAN		= 20%
		STRING FILL		= 6%
		STRING STCODE2		= 2%
		STRING STWAGE		= 11%
		STRING STTAX		= 11%
		STRING OTHDATA		= 10%
		STRING TAXTYP		= 1%
		STRING LOCWAGE		= 11%
		STRING LOCTAX		= 11%
		STRING STCONNUM		= 7%
		STRING SUPDATA1		= 75%
		STRING SUPDATA2		= 75%
		STRING FILL		= 25%
	END RECORD

	!
	! Total Records
	!
	RECORD CODE_RT_STRUCT
		STRING RECID		= 2%
		STRING NUM_OF_EMP	= 7%
		STRING ANNWAGE		= 15%
		STRING FED_WTHLD	= 15%
		STRING FICA_WAGE	= 15%
		STRING FICA_WTHLD	= 15%
		STRING MED_WAGE		= 15%
		STRING MED_WTHLD	= 15%
		STRING FICA_TIP		= 15%
		STRING EIC		= 15%
		STRING DCB		= 15%
		STRING DEFCOMP401	= 15%
		STRING DEFCOMP403	= 15%
		STRING DEFCOMP408	= 15%
		STRING DEFCOMP457	= 15%
		STRING DEFCOMP501	= 15%
		STRING MEBQSC		= 15%
		STRING NQP457		= 15%
		STRING ECHSA		= 15%
		STRING NQPN457		= 15%
		STRING NCP		= 15%
		STRING FILL		= 15%
		STRING LIFINS		= 15%
		STRING ITWTPP		= 15%
		STRING IENSO		= 15%
		STRING DUS409		= 15%
		STRING FILL		= 143%
	END RECORD

	RECORD CODE_RF_STRUCT
		STRING RECID		= 2%
		STRING FILL		= 5%
		STRING NUM_OF_EMP	= 9%
		STRING FILL		= 496%
	END RECORD

	MAP	(PR_FLOPPY)	FLOPPY$			= 512%
	MAP	(PR_FLOPPY)	CODE_RA_STRUCT		CODE_RA
	MAP	(PR_FLOPPY)	CODE_RE_STRUCT		CODE_RE
	MAP	(PR_FLOPPY)	CODE_RW_STRUCT		CODE_RW
	MAP	(PR_FLOPPY)	CODE_RS_STRUCT		CODE_RS
	MAP	(PR_FLOPPY)	CODE_RT_STRUCT		CODE_RT
	MAP	(PR_FLOPPY)	CODE_RF_STRUCT		CODE_RF


	!
	DIM PR_TAXES_STRUCT PR_TAXES(50%)
	DIM YYYY_FILE$(50%)

	!
	! External functions
	!
	EXTERNAL STRING	FUNCTION FUNC_REFORMAT
	EXTERNAL LONG   FUNCTION PR_FUNC_READTAXES
	EXTERNAL SUB SPLITNAME(STRING, INTEGER, STRING, STRING, STRING, STRING)

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

50	!******************************************************************
	! Load in PIN definitions
	!******************************************************************

	WHEN ERROR IN
		PIN% = 0%
		OPEN "W2PIN.INFO" FOR INPUT AS FILE PRNT.CH%

		WHILE 1%

			LINPUT #PRNT.CH%, INLINE$
			PIN% = PIN% + 1%
			PIN$(PIN%) = INLINE$
		NEXT
	USE
		CLOSE PRNT.CH%
	END WHEN

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
		!
		! Only keep the year
		!
		YYYY_FILE$(LOOP%) = &
			MID(YYYY_FILE$(LOOP%), 14%, 4%) &
			FOR LOOP% = 1% TO YYYY_FILE%

		!
		! Reverse the list
		!
		FOR LOOP% = 1% TO YYYY_FILE% / 2%
			TEMP$ = YYYY_FILE$(LOOP%)
			YYYY_FILE$(LOOP%) = YYYY_FILE$(YYYY_FILE% - LOOP% + 1%)
			YYYY_FILE$(YYYY_FILE% - LOOP% + 1%) = TEMP$
		NEXT LOOP%

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
	OPEN TAPE_DEVICE$ FOR OUTPUT AS FILE #PRNT.CH%, &
		RECORDSIZE 512%, &
		ALLOW READ

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
	ST% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"(07) Name Format", 10%, 5%)

	ENTER_CNT% = 7%
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
				"8;37", "Sort By", "SN", 16%, "'E", &
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

		CASE 7%
	!++
	! Abstract:FLD007
	!
	!
	! Index:
	!--
			NAMEFORM% = ENTR_3NUMBER(SCOPE, SMG_SCREEN_DATA%, &
				"10;37", "Name Format", &
				1.0, 16%, "##", "")
			NAMEFORM% = 1% &
				IF NAMEFORM% < 1% OR NAMEFORM% > 3%

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
	W_ECHSA = 0.0
	W_NQPN457 = 0.0
	W_NCP = 0.0
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

		CASE "EC"		! Employer Contributions to a
					! Health Savings Account

			W_ECHSA = FUNC_ROUND(W_ECHSA + AMOUNT, 2%)

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
		T_ECHSA = T_ECHSA + W_ECHSA
		T_NQPN457 = T_NQPN457 + W_NQPN457
		T_NCP = T_NCP + W_NCP
		T_ALLTIP = T_ALLTIP + W_ALLTIP
		T_DEFCOMP = T_DEFCOMP + W_DEFCOMP
		T_FR_BEN = T_FR_BEN + W_FR_BEN

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

	END IF

2990	GOTO 2010

	%PAGE

 ExitTotal:

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
		CODE_RA::RECID	= "RA"
		CODE_RA::EIN	= EIN$
		CODE_RA::PIN	= ""
		CODE_RA::RESUB	= "0"
		CODE_RA::RESUBTLCN = ""
		CODE_RA::SOFTCODE = "98"
		CODE_RA::ENAME	= EDIT$(UTL_PROFILE::REP_NAME, 32%)
		CODE_RA::LADD	= UTL_LOCATION::ADDRESS1
		CODE_RA::DADD	= UTL_LOCATION::ADDRESS2
		CODE_RA::CITY	= UTL_LOCATION::CITY
		CODE_RA::ST	= UTL_LOCATION::STATE
		CODE_RA::ZIP	= UTL_LOCATION::ZIP
		CODE_RA::ZIPEXT	= FNZIPEXT$(UTL_LOCATION::ZIP)
		CODE_RA::PROVINCE	= ""
		CODE_RA::FZIP	= ""
		CODE_RA::COUNTRY = ""
		CODE_RA::SUBMITTER	= EDIT$(UTL_PROFILE::REP_NAME, 32%)
		CODE_RA::SLADDRESS	= UTL_LOCATION::ADDRESS1
		CODE_RA::SDADDRESS	= UTL_LOCATION::ADDRESS2
		CODE_RA::SCITY		= UTL_LOCATION::CITY
		CODE_RA::SSTATE		= UTL_LOCATION::STATE
		CODE_RA::SZIP		= UTL_LOCATION::ZIP
		CODE_RA::SZIPEXT	= FNZIPEXT$(UTL_LOCATION::ZIP)
		CODE_RA::SPROVINCE	= ""
		CODE_RA::SFZIP		= ""
		CODE_RA::SCOUNTRY	= ""
		CODE_RA::SCONTACT	= ""
		CODE_RA::SPHONE		= ""
		CODE_RA::SPHONEEXT	= ""
		CODE_RA::SEMAIL		= ""
		CODE_RA::SFAX		= ""
		CODE_RA::PROBLEM	= "1"
		CODE_RA::PREPARER	= "L"

		FOR I% = 1% TO PIN%

			I1% = INSTR(1%, PIN$(I%), "	")
			IF I1%
			THEN
				XLEFT$ = LEFT(PIN$(I%), I1% - 1%)
				XRIGHT$ = RIGHT(PIN$(I%), I1% + 1%)

				SELECT XLEFT$
				CASE "PIN"
					CODE_RA::PIN = XRIGHT$
				CASE "RESUB"
					CODE_RA::RESUB = XRIGHT$
				CASE "TLCN"
					CODE_RA::RESUBTLCN = XRIGHT$
				CASE "NAME"
					CODE_RA::SCONTACT = XRIGHT$
				CASE "PHONE"
					CODE_RA::SPHONE = XRIGHT$
				CASE "EXT"
					CODE_RA::SPHONEEXT = XRIGHT$
				CASE "EMAIL"
					CODE_RA::SEMAIL = XRIGHT$
				CASE "FAX"
					CODE_RA::SFAX = XRIGHT$
				CASE "NOTIFY"
					CODE_RA::PROBLEM = XRIGHT$
				CASE "CODE"
					CODE_RA::PREPARER = XRIGHT$

				END SELECT
			END IF

		NEXT I%

		GOSUB Putrecord

	CASE "E"
		!****************************************************
		! Code E Employer/Establishment
		!****************************************************
		CODE_RE::RECID	= "RE"
		CODE_RE::PAYYR	= YYYY$
		CODE_RE::AGENT	= ""	! ****
		CODE_RE::EIN	= EIN$
		CODE_RE::ESIN	= ""
		CODE_RE::TERMBUS = "0"
		CODE_RE::ESTNO	= ""
		CODE_RE::OTHEREIN = ""
		CODE_RE::ENAME	= EDIT$(UTL_PROFILE::REP_NAME, 32%)
		CODE_RE::LOCADD = EDIT$(UTL_LOCATION::ADDRESS1, 32%)
		CODE_RE::ADD	= EDIT$(UTL_LOCATION::ADDRESS2, 32%)
		CODE_RE::CITY	= EDIT$(UTL_LOCATION::CITY, 32%)
		CODE_RE::STATE	= UTL_LOCATION::STATE
		CODE_RE::ZIP	= UTL_LOCATION::ZIP
		CODE_RE::ZIPEXT	= FNZIPEXT$(UTL_LOCATION::ZIP)
		CODE_RE::FORSTATE	= ""
		CODE_RE::FORZIP		= ""
		CODE_RE::CTYCODE	= ""
		CODE_RE::EMPCODE	= "R"
		CODE_RE::TAXJOUR	= ""
		CODE_RE::THIRDPARTY	= "0"

		GOSUB Putrecord

	CASE "W"
		!****************************************************
		! Code W Employee Wage Record
		!****************************************************
		IF PR_EMP_MASTER::UNION = ""
		THEN
			PENPLAN_FLAG$ = "0"
		ELSE
			PENPLAN_FLAG$ = "1"
		END IF

		CODE_RW::RECID	= "RW"
		CODE_RW::SSN	= XLATE(PR_EMP_MASTER::SSN, &
			STRING$(48%, 0%) + "0123456789")
		CODE_RW::SSN	= "000000000" IF CODE_RW::SSN = ""

		CALL SPLITNAME(PR_EMP_MASTER::EMPNAME, NAMEFORM%, &
			CODE_RW::ENAME, CODE_RW::ENAMEMID, &
			CODE_RW::ENAMELAST, CODE_RW::SUFFIX)
 !		CODE_RW::ENAME	= EDIT$(PR_EMP_MASTER::EMPNAME, 32%)
 !		CODE_RW::ENAMEMID	= ""
 !		CODE_RW::ENAMELAST	= ""
 !		CODE_RW::SUFFIX		= ""
		CODE_RW::LOCADD		= EDIT$(PR_EMP_MASTER::ADD1, 32%)
		CODE_RW::ADD	= EDIT$(PR_EMP_MASTER::ADD2, 32%)
		TEMP$ = EDIT$(PR_EMP_MASTER::CITY, 32% + 128%)
		TEMP$ = LEFT(TEMP$, LEN(TEMP$) - 1%) &
			IF RIGHT(TEMP$, LEN(TEMP$)) = ","
		CODE_RW::CITY	= TEMP$
		CODE_RW::ST	= PR_EMP_MASTER::STATE
		CODE_RW::ZIP	= PR_EMP_MASTER::ZIP
		CODE_RW::ZIPEXT	= FNZIPEXT$(PR_EMP_MASTER::ZIP)
		CODE_RW::FORSTATE		= ""
		CODE_RW::FORZIP		= ""
		CODE_RW::CTYCODE		= ""

		CODE_RW::FED_WAGE	= FUNC_REFORMAT(W_ANNWAGE, &
			LEN(CODE_RW::FED_WAGE), 2%)
		CODE_RW::FED_WTHLD = FUNC_REFORMAT(W_FED_WTHLD, &
			LEN(CODE_RW::FED_WTHLD), 2%)
		CODE_RW::FICA_WTHLD= FUNC_REFORMAT(W_FICA_WTHLD, &
			LEN(CODE_RW::FICA_WTHLD), 2%)
		CODE_RW::FICA_WAGE = FUNC_REFORMAT(W_FICA_WAGE, &
			LEN(CODE_RW::FICA_WAGE), 2%)
		CODE_RW::MED_WAGE	= FUNC_REFORMAT(W_HI_WAGE, &
			LEN(CODE_RW::MED_WAGE), 2%)
		CODE_RW::MED_WTHLD	= FUNC_REFORMAT(W_HI_WTHLD, &
			LEN(CODE_RW::MED_WTHLD), 2%)

		CODE_RW::FICA_TIP = FUNC_REFORMAT(W_FICA_TIP, &
			LEN(CODE_RW::FICA_TIP), 2%)
		CODE_RW::EIC	= FUNC_REFORMAT(W_EIC, &
			LEN(CODE_RW::EIC), 2%)
		CODE_RW::DCB	= FUNC_REFORMAT(0.0, &
			LEN(CODE_RW::DCB), 2%)
		CODE_RW::DEFCOMP401 = FUNC_REFORMAT(W_DEFCOMP, &
			LEN(CODE_RW::DEFCOMP401), 2%)
		CODE_RW::DEFCOMP403	= FUNC_REFORMAT(0.0, &
			LEN(CODE_RW::DEFCOMP403), 2%)
		CODE_RW::DEFCOMP408	= FUNC_REFORMAT(0.0, &
			LEN(CODE_RW::DEFCOMP408), 2%)
		CODE_RW::DEFCOMP457	= FUNC_REFORMAT(0.0, &
			LEN(CODE_RW::DEFCOMP457), 2%)
		CODE_RW::DEFCOMP501	= FUNC_REFORMAT(0.0, &
			LEN(CODE_RW::DEFCOMP501), 2%)
		CODE_RW::MEBQSCP	= FUNC_REFORMAT(0.0, &
			LEN(CODE_RW::MEBQSCP), 2%)
		CODE_RW::NQP457 = FUNC_REFORMAT(W_NQP457, &
			LEN(CODE_RW::NQP457), 2%)
		CODE_RW::ECHSA = FUNC_REFORMAT(W_ECHSA, &
			LEN(CODE_RW::ECHSA), 2%)
		CODE_RW::NQPN457 = FUNC_REFORMAT(W_NQPN457, &
			LEN(CODE_RW::NQPN457), 2%)
		CODE_RW::NCP = FUNC_REFORMAT(W_NCP, &
			LEN(CODE_RW::NCP), 2%)
		CODE_RW::ECPGTLI	= FUNC_REFORMAT(0.0, &
			LEN(CODE_RW::ECPGTLI), 2%)
		CODE_RW::IENSO		= FUNC_REFORMAT(0.0, &
			LEN(CODE_RW::IENSO), 2%)
		CODE_RW::DUS409		= FUNC_REFORMAT(0.0, &
			LEN(CODE_RW::DUS409), 2%)
		CODE_RW::SEC		= "0"
		CODE_RW::PENPLAN_FLAG = PENPLAN_FLAG$
		CODE_RW::TPSPI		= "0"

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

			CODE_RS::RECID = "RS"
			CODE_RS::STCODE1	= ST_FIPS$ ! Fips Postal Numeric Code
			CODE_RS::TAXINGENT	= ""
			CODE_RS::SSN	= XLATE(PR_EMP_MASTER::SSN, &
				STRING$(48%, 0%) + "0123456789")
			CODE_RS::SSN	= "000000000" IF CODE_RS::SSN = ""

			CALL SPLITNAME(PR_EMP_MASTER::EMPNAME, NAMEFORM%, &
				CODE_RS::ENAME, CODE_RS::ENAMEMIDDLE, &
				CODE_RS::ENAMELAST, CODE_RS::SUFFIX)

 !			CODE_RS::ENAME	= EDIT$(PR_EMP_MASTER::EMPNAME, 32%)
 !			CODE_RS::ENAMEMIDDLE	= ""
 !			CODE_RS::ENAMELAST	= ""
 !			CODE_RS::SUFFIX		= ""
			CODE_RS::LOCADD		= EDIT$(PR_EMP_MASTER::ADD1, 32%)
			CODE_RS::ADD		= EDIT$(PR_EMP_MASTER::ADD2, 32%)
			TEMP$ = EDIT$(PR_EMP_MASTER::CITY, 32% + 128%)
			TEMP$ = LEFT(TEMP$, LEN(TEMP$) - 1%) &
				IF RIGHT(TEMP$, LEN(TEMP$)) = ","
			CODE_RS::CITY	= TEMP$
			CODE_RS::ST	= PR_EMP_MASTER::STATE
			CODE_RS::ZIP	= PR_EMP_MASTER::ZIP
			CODE_RS::ZIPEXT	= FNZIPEXT$(PR_EMP_MASTER::ZIP)
			CODE_RS::FORSTATE	= ""
			CODE_RS::FORZIP		= ""
			CODE_RS::CTYCODE	= ""
			CODE_RS::XOPTIONAL	= ""
			CODE_RS::REPPER	= "12" + MID(YYYY$, 1%, 4%)
			CODE_RS::TOTWAG	= FUNC_REFORMAT(S_TOTWAG(Z%), &
				LEN(CODE_RS::TOTWAG), 2%)
			CODE_RS::SUIWAG		= FUNC_REFORMAT(S_SUIWAG(Z%), &
				LEN(CODE_RS::SUIWAG), 2%)
			CODE_RS::WEEKWORK= FUNC_REFORMAT(S_WEEKWORK(Z%), &
				LEN(CODE_RS::WEEKWORK), 0%)
			CODE_RS::HIREDATE	= MID(PR_EMP_MASTER::HIREDAY, 5%, 2%) + &
				MID(PR_EMP_MASTER::HIREDAY, 1%, 4%)
			CODE_RS::HIREDATE = "" IF CODE_RS::HIREDATE = "000000"
			CODE_RS::FIREDATE = MID(PR_EMP_MASTER::TERMDAY, 5%, 2%) + &
				MID(PR_EMP_MASTER::TERMDAY, 4%, 4%)
			CODE_RS::FIREDATE = "" IF CODE_RS::FIREDATE = "0000"
			CODE_RS::SEAN	= SIN$
			CODE_RS::STCODE2 = ST_FIPS$ ! FIPS postal numeric code
			CODE_RS::STWAGE	= FUNC_REFORMAT(S_STWAGE(Z%), &
				LEN(CODE_RS::STWAGE), 2%)
			CODE_RS::STTAX	= FUNC_REFORMAT(S_STTAX(Z%), &
				LEN(CODE_RS::STTAX), 2%)
			CODE_RS::OTHDATA = FUNC_REFORMAT(S_OTHDATA(Z%), &
				LEN(CODE_RS::OTHDATA), 2%)
			CODE_RS::TAXTYP	= "F"
			CODE_RS::LOCWAGE = FUNC_REFORMAT(S_LOCWAGE, &
				LEN(CODE_RS::LOCWAGE), 2%)
			CODE_RS::LOCTAX		= FUNC_REFORMAT(S_LOCTAX, &
				LEN(CODE_RS::LOCTAX), 2%)
			CODE_RS::STCONNUM		= ""
			CODE_RS::SUPDATA1		= ""
			CODE_RS::SUPDATA2		= ""


			GOSUB Putrecord

		NEXT Z%

	CASE "T"
		!****************************************************
		! Code T Total Record
		!****************************************************
		CODE_RT::RECID		= "RT"

		CODE_RT::NUM_OF_EMP	= FUNC_REFORMAT(T_NUM_OF_EMP, &
			LEN(CODE_RT::NUM_OF_EMP), 0%)
		CODE_RT::ANNWAGE	= FUNC_REFORMAT(T_ANNWAGE, &
			LEN(CODE_RT::ANNWAGE), 2%)
		CODE_RT::FED_WTHLD	= FUNC_REFORMAT(T_FED_WTHLD, &
			LEN(CODE_RT::FED_WTHLD), 2%)
		CODE_RT::FICA_WAGE	= FUNC_REFORMAT(T_FICA_WAGE, &
			LEN(CODE_RT::FICA_WAGE), 2%)
		CODE_RT::FICA_WTHLD	= FUNC_REFORMAT(T_FICA_WTHLD, &
			LEN(CODE_RT::FICA_WTHLD), 2%)
		CODE_RT::MED_WAGE	= FUNC_REFORMAT(T_HI_WAGE, &
			LEN(CODE_RT::MED_WAGE), 2%)
		CODE_RT::MED_WTHLD	= FUNC_REFORMAT(T_HI_WTHLD, &
			LEN(CODE_RT::MED_WTHLD), 2%)
		CODE_RT::FICA_TIP	= FUNC_REFORMAT(T_FICA_TIP, &
			LEN(CODE_RT::FICA_TIP), 2%)
		CODE_RT::EIC		= FUNC_REFORMAT(T_EIC, &
			LEN(CODE_RT::EIC), 2%)
		CODE_RT::DCB		= FUNC_REFORMAT(0.0, &
			LEN(CODE_RT::DCB), 2%)
		CODE_RT::DEFCOMP401	= FUNC_REFORMAT(T_DEFCOMP, &
			LEN(CODE_RT::DEFCOMP401), 2%)
		CODE_RT::DEFCOMP403	= FUNC_REFORMAT(0.0, &
			LEN(CODE_RT::DEFCOMP403), 2%)
		CODE_RT::DEFCOMP408	= FUNC_REFORMAT(0.0, &
			LEN(CODE_RT::DEFCOMP408), 2%)
		CODE_RT::DEFCOMP457	= FUNC_REFORMAT(0.0, &
			LEN(CODE_RT::DEFCOMP457), 2%)
		CODE_RT::DEFCOMP501	= FUNC_REFORMAT(0.0, &
			LEN(CODE_RT::DEFCOMP501), 2%)
		CODE_RT::MEBQSC		= FUNC_REFORMAT(0.0, &
			LEN(CODE_RT::MEBQSC), 2%)
		CODE_RT::NQP457		= FUNC_REFORMAT(T_NQP457, &
			LEN(CODE_RT::NQP457), 2%)
		CODE_RT::ECHSA = FUNC_REFORMAT(W_ECHSA, &
			LEN(CODE_RT::ECHSA), 2%)
		CODE_RT::NQPN457	= FUNC_REFORMAT(0.0, &
			LEN(CODE_RT::NQPN457), 2%)
		CODE_RT::NCP		= FUNC_REFORMAT(0.0, &
			LEN(CODE_RT::NCP), 2%)
		CODE_RT::LIFINS		= FUNC_REFORMAT(T_LIFINS, &
			LEN(CODE_RT::LIFINS), 2%)
		CODE_RT::ITWTPP		= FUNC_REFORMAT(0.0, &
			LEN(CODE_RT::ITWTPP), 2%)
		CODE_RT::IENSO		= FUNC_REFORMAT(0.0, &
			LEN(CODE_RT::IENSO), 2%)
		CODE_RT::DUS409		= FUNC_REFORMAT(0.0, &
			LEN(CODE_RT::DUS409), 2%)

		GOSUB Putrecord

	CASE "F"
		!****************************************************
		! Code F Final Record
		!****************************************************
		CODE_RF::RECID	= "RF"
		CODE_RF::NUM_OF_EMP = FUNC_REFORMAT(F_NUM_OF_EMP, &
			LEN(CODE_RF::NUM_OF_EMP), 0%)

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
		EXTZIP$ = "" IF LEN(EXTZIP$) <> 4%

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

	END

20000	SUB SPLITNAME(SOURCE$, FORM%, FIRST$, MIDDLE$, LAST$, SUFFIX$)

	!*******************************************************************
	! Split a name into firs\t/middle/last
	!*******************************************************************

	!
	! INPUTS:
	!
	!	SOURCE = Original name string
	!
	!	FORM% = Format used for names
	!		1% = first middle last
	!		2% = last first middle
	!		3% = last, first, middle
	!
	! OUTPUTS:
	!
	!	FIRST$ = First name
	!
	!	MIDDLE$ = Middle name
	!
	!	LAST$ = Last name
	!

	!
	! Zip out everything to start
	!
	FIRST$ = ""
	MIDDLE$ = ""
	LAST$ = ""
	SUFFIX$ = ""

	!*****************************************************
	! Split the name into sections
	!*****************************************************

	SECTIONS% = 0%
	WORKSOURCE$ = EDIT$(SOURCE$, 4% + 8% + 16% + 32% + 128%)

	!
	! Drop off all periods
	!
	SRCH% = INSTR(1%, WORKSOURCE$, ".")
	WHILE(SRCH%)
		WORKSOURCE$ = LEFT(WORKSOURCE$, SRCH% - 1%) + &
			RIGHT(WORKSOURCE$, SRCH% + 1%)
		SRCH% = INSTR(1%, WORKSOURCE$, ".")
	NEXT

	!
	! Drop off all QUOTES
	!
	SRCH% = INSTR(1%, WORKSOURCE$, '"')
	WHILE(SRCH%)
		WORKSOURCE$ = LEFT(WORKSOURCE$, SRCH% - 1%) + &
			RIGHT(WORKSOURCE$, SRCH% + 1%)
		SRCH% = INSTR(1%, WORKSOURCE$, '"')
	NEXT

	!
	! If FORM=3, try to pop last name off
	!
	IF FORM% = 3%
	THEN
		SRCH% = INSTR(1%, WORKSOURCE$, ",")
		IF SRCH%
		THEN
			SECTIONS% = SECTIONS% + 1%
			SECTIONS$(SECTIONS%) = &
				LEFT(WORKSOURCE$, SRCH% - 1%)
			WORKSOURCE$ = &
				EDIT$(RIGHT(WORKSOURCE$, SRCH% + 1%), &
				8%)
		END IF
	END IF

	!
	! Split off the rest of the name sections
	!
	SRCH% = INSTR(1%, WORKSOURCE$, " ")
	WHILE SRCH%
		SECTIONS% = SECTIONS% + 1%
		SECTIONS$(SECTIONS%) = &
			LEFT(WORKSOURCE$, SRCH% - 1%)
		WORKSOURCE$ = &
			EDIT$(RIGHT(WORKSOURCE$, SRCH% + 1%), 8%)
		SRCH% = INSTR(1%, WORKSOURCE$, " ")
	NEXT

	!
	! Do something with any last segments
	!
	IF WORKSOURCE$ <> ""
	THEN
		SECTIONS% = SECTIONS% + 1%
		SECTIONS$(SECTIONS%) = WORKSOURCE$
	END IF

	!
	! Now try to split things up
	!
	SELECT FORM%
	CASE 1%		! First name first
		IF SECTIONS% = 3%
		THEN
			FIRST$ = SECTIONS$(1%)
			MIDDLE$ = SECTIONS$(2%)
			LAST$ = SECTIONS$(3%)
		ELSE
			LAST$ = SECTIONS$(SECTIONS%)
			FIRST$ = SECTIONS$(1%)
			FIRST$ = FIRST$ + SECTIONS$(I%) &
				FOR I% = 2% TO SECTIONS% - 1%
		END IF

	CASE 2%, 3%	! Last name first

		IF SECTIONS% = 3%
		THEN
			FIRST$ = SECTIONS$(2%)
			MIDDLE$ = SECTIONS$(3%)
			LAST$ = SECTIONS$(1%)
		ELSE
			LAST$ = SECTIONS$(1%)
			FIRST$ = SECTIONS$(2%)
			FIRST$ = FIRST$ + SECTIONS$(I%) &
				FOR I% = 3% TO SECTIONS%
		END IF

	END SELECT

	END SUB
