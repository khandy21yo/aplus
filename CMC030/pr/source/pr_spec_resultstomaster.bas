1	%TITLE "Create Payroll Master File from W2 Tape"
	%SBTTL "PR_SPEC_RESULTSTOMASTER"
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
	!	.p
	!	The ^*Verify W-2 Tape\* option in the W-2 Processing menu provides
	!	the means to verify the accuracy of the W-2 data written on the
	!	magnetic tape.
	!	.P
	!	Before running this option, you need to put a blank magtape in the
	!	drive and mount it (at the *$ prompt) using the following
	!	command:
	!	.b
	!	.lm +10
	!	^*MOUNT MUA0:/FOREIGN/BLOCKSIZE=6900\*
	!	.LM -10
	!	.B
	!	and then start up the program.  Replace ^*MUA0:\* whith the proper
	!	drive name if it is different on your system.
	!	.p
	!	After running this option, dismount the tape (again at the *$) using the
	!	following command:
	!	.b
	!	.lm +10
	!	^*DISMOUNT MUA0:\*
	!	.lm -10
	!	.b
	!	and take the tape out of the drive.
	!
	! Index:
	!	.x W-2 Forms>Tape>Verification
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_SPEC_RESULTSTOMASTER
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_SPEC_RESULTSTOMASTER, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_SPEC_RESULTSTOMASTER.OBJ;*
	!
	! Author:
	!
	!	03/22/90 - Kevin Handy
	!
	! Modification history:
	!
	!	07/13/91 - Kevin Handy
	!		Removed PR_FUNC_SUBJECT com definition.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/16/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/09/99 - Kevin Handy
	!		Lose NoFile (Dead code)
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_RATE.HB"
	MAP (PR_EMP_RATE) PR_EMP_RATE_CDD PR_EMP_RATE

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STD_ERNDED.HB"
	MAP (PR_EMP_STD_ERNDED) PR_EMP_STD_ERNDED_CDD PR_EMP_STD_ERNDED

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STATUS.HB"
	MAP (PR_EMP_STATUS) PR_EMP_STATUS_CDD PR_EMP_STATUS

	DIM Y$(100%)

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

300	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.CRE"

310	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STD_ERNDED.CRE"

320	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_RATE.CRE"

330	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STATUS.CRE"

	!
	! Set up screen
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 80%, SMG_SCREEN_DATA%)
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 1%, 1%)

	TAPE_DEVICE$ = "091.DAT"

 AskTape:
	TEMP$ = "File name <"+TAPE_DEVICE$+"> "

	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, TEMP$, 1%, 1%)

	JUNK$ = SPACE$(20%)
	LSET JUNK$ = TAPE_DEVICE$

	SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, 1%, &
		LEN(TEMP$) + 2%, JUNK$, -1%, 16%+4096%)

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
		ACCESS READ

	SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_SCREEN_DATA%)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)

	START_DATE% = DATE_DAYCODE("19671231")

2000	!*******************************************************************
	! Read through Payroll master file
	!*******************************************************************

	FCOUNT% = 0%

2010	LINPUT #PRNT.CH%, D$

	IF D$ <> "<END>"
	THEN
		FCOUNT% = FCOUNT% + 1%
		Y$(FCOUNT%) = D$
		GOTO 2010
	END IF

	GOSUB 2100

	GOTO 2000

2100	SCOPE::SCOPE_EXIT = 0%

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
4000	!********************************************************************
	! Set codes records
	!*******************************************************************

	!****************************************************
	! Employee Name Record
	!****************************************************
	CALL ENTR_3MESSAGE(SCOPE, Y$(1%), 1%)

4100	SSN$ = LEFT(Y$(13%), 3%) + MID(Y$(13%),5%,2%) + RIGHT(Y$(13%),8%)
	PR_EMP_MASTER::EMPNUM = SSN$
	PR_EMP_MASTER::EMPNAME = Y$(6%) + " " + Y$(5%)
	PR_EMP_MASTER::ADD1 = Y$(7%)
	PR_EMP_MASTER::ADD2 = ""
	PR_EMP_MASTER::CITY = Y$(8%)
	PR_EMP_MASTER::STATE = Y$(9%)
	PR_EMP_MASTER::ZIP = Y$(10%)
	PR_EMP_MASTER::COUNTRY = "US"
	PR_EMP_MASTER::PHONE = LEFT(Y$(11%), 3%) + &
		MID(Y$(11%),5%,3%) + &
		RIGHT(Y$(11%), 9%)
	PR_EMP_MASTER::SSN = Y$(13%)
	PR_EMP_MASTER::SORT = Y$(1%)		! EDIT$(Y$(5%)+Y$(6%), 2%)
	PR_EMP_MASTER::SUBACC = ""
	PR_EMP_MASTER::ACCT = ""
	PR_EMP_MASTER::TRADE = ""
	PR_EMP_MASTER::OPER = ""
	PR_EMP_MASTER::UNION = Y$(43%)
	PR_EMP_MASTER::LOCATION = Y$(2%)
	PR_EMP_MASTER::DEPT = Y$(4%)
	PR_EMP_MASTER::WORK_CENTER = ""
	PR_EMP_MASTER::EMP_SKILL = ""
	PR_EMP_MASTER::EMP_GRADE = ""
	PR_EMP_MASTER::DISABLED = ""
	PR_EMP_MASTER::PAYFREQ = 26%
	PR_EMP_MASTER::RATE_TYPE = "H"
	PR_EMP_MASTER::RATE_CDE = "RT"
	PR_EMP_MASTER::SUI_SW = Y$(17%)
	PR_EMP_MASTER::TAX_PKG = Y$(17%)
	PR_EMP_MASTER::WC = ""
	PR_EMP_MASTER::W2_1099 = ""
	IF Y$(12%) = ""
	THEN
		PR_EMP_MASTER::BIRTH = ""
	ELSE
		PR_EMP_MASTER::BIRTH = DATE_INVDCODE(START_DATE% + &
			VAL%(Y$(12%)))
	END IF
	IF Y$(19%) = ""
	THEN
		PR_EMP_MASTER::HIREDAY =  ""
	ELSE
		PR_EMP_MASTER::HIREDAY = DATE_INVDCODE(START_DATE% + &
			VAL%(Y$(19%)))
	END IF
	IF Y$(20%) = ""
	THEN
		PR_EMP_MASTER::TERMDAY = ""
	ELSE
		PR_EMP_MASTER::TERMDAY = DATE_INVDCODE(START_DATE% + &
			VAL%(Y$(20%)))
	END IF
	PR_EMP_MASTER::REHIRE_FLAG = ""
	PR_EMP_MASTER::SEX = Y$(42%)
	PR_EMP_MASTER::RACE = ""
	PR_EMP_MASTER::USCIT = "Y"
	PR_EMP_MASTER::WRKPERMIT = ""
	PR_EMP_MASTER::HOMCNTRY = ""
	PR_EMP_MASTER::ACTIVE_FLAG = ""

4110	PUT #PR_EMP_MASTER.CH%

	!****************************************************
	! Employee Rate Record
	!****************************************************

4200	SELECT Y$(24%)
	CASE "H"
		PR_EMP_RATE::EMPNUM	= PR_EMP_MASTER::EMPNUM
		PR_EMP_RATE::OPER	= ""
		PR_EMP_RATE::EFFDAT	= ""
		PR_EMP_RATE::RATE_TYPE	= "H"
		PR_EMP_RATE::RATE_CDE	= "RT"
		PR_EMP_RATE::HOUR_RATE	= VAL(Y$(26%))/10000
		PR_EMP_RATE::PIECE_RATE	= 0.0
		PR_EMP_RATE::FACTOR	= 150%
		PR_EMP_RATE::STDEFF	= 0.0
		PR_EMP_RATE::EVAL_DATE	= ""

		PUT #PR_EMP_RATE.CH%
	CASE "S"
		PR_EMP_RATE::EMPNUM	= PR_EMP_MASTER::EMPNUM
		PR_EMP_RATE::OPER	= ""
		PR_EMP_RATE::EFFDAT	= ""
		PR_EMP_RATE::RATE_TYPE	= "S"
		PR_EMP_RATE::RATE_CDE	= "RT"
		PR_EMP_RATE::HOUR_RATE	= VAL(Y$(25%))/100
		PR_EMP_RATE::PIECE_RATE	= 0.0
		PR_EMP_RATE::FACTOR	= 150%
		PR_EMP_RATE::STDEFF	= 0.0
		PR_EMP_RATE::EVAL_DATE	= ""

		PUT #PR_EMP_RATE.CH%
	END SELECT

	V% = SMG$END_PASTEBOARD_UPDATE(SCOPE::SMG_PBID)

	!****************************************************
	! Employee Deduction Record(s)
	!****************************************************

4300	FOR I% = 31% TO 39%

		GOTO 4320 IF Y$(I%) = ""

		I1% = INSTR(1%, Y$(I%), CHR$(253%))
		I2% = INSTR(I1% + 1%, Y$(I%), CHR$(253%))
		I3% = INSTR(I2% + 1%, Y$(I%), CHR$(253%))

		PR_EMP_STD_ERNDED::EMPNUM	= PR_EMP_MASTER::EMPNUM
		PR_EMP_STD_ERNDED::RTYPE	= "D"
		PR_EMP_STD_ERNDED::CODE		= LEFT(Y$(I%), I1% - 1%)
		PR_EMP_STD_ERNDED::RATE		= &
			VAL(SEG$(Y$(I%), I1%+1%, I2%-1%))/100.0
		PR_EMP_STD_ERNDED::LIMIT	= 0.0
		PR_EMP_STD_ERNDED::CTDBAL	= 0.0
		PR_EMP_STD_ERNDED::ACCRUED	= 0.0
		PR_EMP_STD_ERNDED::ENDDAT	= ""
		SELECT MID(Y$(I%),I3%+1%,1%)
		CASE "1"
			PR_EMP_STD_ERNDED::FREQ		= "YNNNNN"
		CASE "2"
			PR_EMP_STD_ERNDED::FREQ		= "NYNNNN"
		CASE "3"
			PR_EMP_STD_ERNDED::FREQ		= "NNYNNN"
		CASE "4"
			PR_EMP_STD_ERNDED::FREQ		= "NNNYNN"
		CASE "5"
			PR_EMP_STD_ERNDED::FREQ		= "NNNNYN"
		CASE "0"
			PR_EMP_STD_ERNDED::FREQ		= "YYYYYN"
		CASE ELSE
			PR_EMP_STD_ERNDED::FREQ		= "NNNNNY"
		END SELECT

		select PR_EMP_STD_ERNDED::CODE
		CASE "RE","GN"
			PR_EMP_STD_ERNDED::METHOD	= "3"
		CASE ELSE
			PR_EMP_STD_ERNDED::METHOD	= "5"
		END SELECT
		PR_EMP_STD_ERNDED::USERDEF	= ""

4310		PUT #PR_EMP_STD_ERNDED.CH%

4320	NEXT I%

	!****************************************************
	! Employee Status Record(s)
	!****************************************************

4400	I% = INSTR(1%, Y$(15%), CHR$(253%))
	I1% = VAL%(LEFT(Y$(15%),I% - 1%))

	PR_EMP_STATUS::EMPNUM	= PR_EMP_MASTER::EMPNUM
	PR_EMP_STATUS::STTYPE	= "FW"
	PR_EMP_STATUS::CODE	= ""
	PR_EMP_STATUS::STSTATUS	= Y$(14%)
	PR_EMP_STATUS::EXEMPT	= I1%

	PUT #PR_EMP_STATUS.CH%

	PR_EMP_STATUS::EMPNUM	= PR_EMP_MASTER::EMPNUM
	PR_EMP_STATUS::STTYPE	= "SW"
	PR_EMP_STATUS::CODE	= Y$(17%)
	PR_EMP_STATUS::STSTATUS	= Y$(14%)
	PR_EMP_STATUS::EXEMPT	= I1%

	PUT #PR_EMP_STATUS.CH%

4900	!
	! Pause to read screen
	!
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
	CASE 2010%
		RESUME ExitTotal IF ERR = 11%
		FILENAME$ = TAPE_DEVICE$

	CASE 4110%
		IF ERR=134%
		THEN
			A$ = RIGHT(PR_EMP_MASTER::EMPNUM, 10%)
			IF A$ >= "A" AND A$ <="Y"
			THEN
				A$ = CHR$(ASCII(A$)+1%)
			ELSE
				A$ = "A"
			END IF
			PR_EMP_MASTER::EMPNUM = &
				LEFT(PR_EMP_MASTER::EMPNUM, 9%) + A$
			RESUME 4110
		END IF
		FILENAME$ = "PR_EMP_MASTER"

	CASE 4200%
		RESUME 4300

	CASE 4310%
		IF PR_EMP_STD_ERNDED::CODE <> "??"
		THEN
			PR_EMP_STD_ERNDED::CODE	= "??"
			RESUME 4310
		END IF
		FILENAME$ = "PR_EMP_STD_ERNDED"

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
