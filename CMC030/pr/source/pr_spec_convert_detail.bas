1	%TITLE "Payroll Conversion"
	%SBTTL "PR_SPEC_CONVERT_DETAIL"
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
	! Computer Management Center, Inc.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	! Abstract:HELP
	!	.p
	!	This program converts ascii file to detail files
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_SPEC_CONVERT_DETAIL/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_SPEC_CONVERT_DETAIL, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_SPEC_CONVERT_DETAIL.OBJ;*
	!
	! Author:
	!
	!	11/27/87 - Robert Peterson
	!
	! Modification history:
	!
	!	06/27/89 - Kevin Handy
	!		Modified to use READ_INITIALIZE.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code.
	!
	!	06/18/98 - Kevin Handy
	!		Splity the fica approximately correctly
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.HB"
	MAP	(PR_HIS_PAY)	PR_HIS_PAY_CDD	PR_HIS_PAY

	%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_DED.HB"
	MAP	(PR_HIS_DED)	PR_HIS_DED_CDD	PR_HIS_DED

	%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_CHECK.HB"
	MAP	(PR_HIS_CHECK)	PR_HIS_CHECK_CDD PR_HIS_CHECK

	EXTERNAL STRING FUNCTION FIND_STRING(STRING, STRING)

	%PAGE

10	ON ERROR GOTO 19000

	CALL ASSG_CHANNEL(PRSYS.CH%, STAT%)

	!
	! Open the keyboard
	!
	CALL READ_INITIALIZE

100	!
	! Open UTL_DEVICE file and read information
	!
	CALL READ_DEVICE("PRSYS.ASC", PRSYS_ASC.DEV$, STAT%)

200	! RESUME LINE

250	WHEN ERROR IN
		OPEN PRSYS_ASC.DEV$ + "PRSYS1.ASC" FOR INPUT AS FILE PRSYS.CH%, &
		ACCESS READ
	USE
		CALL ENTR_3MESSAGE(SCOPE, "PRSYS1.ASC file is missing" + &
			"ERROR " + NUM1$(ERR), 0%)
		CONTINUE ExitProgram
	END WHEN

	LINECOUNT% = 0%

550	WHEN ERROR IN
		INPUT LINE #PRSYS.CH%, INP$
	USE
		CONTINUE 560 IF STRG$ <> ""
		CONTINUE 600
	END WHEN

	INP$ = EDIT$(INP$, 4% + 128%)
	LINECOUNT% = LINECOUNT% + 1%

	IF INSTR(1%, INP$, "<ENDFILE>") = 0% AND &
		INSTR(1%, INP$, "<STARTFILE>") = 0%
	THEN
		IF INSTR(1%, INP$, "<>") = 0%
		THEN
			STRG$ = STRG$ + "<" + EDIT$(INP$, 4%) + ">"
			GOTO 550
		ELSE
			STRG$ = STRG$ + INP$
		END IF
	END IF

	STRG$ = STRG$ + INP$

560	TEMP$ = "<STARTFILE>"

	GOTO 580 IF INSTR(1%, STRG$, TEMP$) = 0%

	TEST% = INSTR(1% + LEN(TEMP$), STRG$, "19")

	IF TEST%
	THEN
		BATCH_NO$ = MID(STRG$, TEST%, 8%)
	ELSE
		PRINT STRG$
		STOP
	END IF

	ON_LOOP% = 0%
	ON_LOOP% =  1% IF INSTR(1% + LEN(TEMP$), STRG$, "PR_HIS_PAY")
	ON_LOOP% =  1% IF INSTR(1% + LEN(TEMP$), STRG$, "PR_TRN_PAY")
	ON_LOOP% =  2% IF INSTR(1% + LEN(TEMP$), STRG$, "PR_HIS_DED")
	ON_LOOP% =  2% IF INSTR(1% + LEN(TEMP$), STRG$, "PR_TRN_DED")
	ON_LOOP% =  3% IF INSTR(1% + LEN(TEMP$), STRG$, "PR_HIS_CHECK")
	ON_LOOP% =  3% IF INSTR(1% + LEN(TEMP$), STRG$, "PR_TRN_CHECK")

	IF ON_LOOP% = 0%
	THEN
		PRINT STRG$
		STOP
	END IF

	ON ON_LOOP% GOSUB 1000, 2000, 3000

	STRG$ = ""
	GOTO 550

580	TEMP$ = "<ENDFILE>"

	IF INSTR(1%, STRG$, TEMP$)
	THEN

		ON ON_LOOP% GOSUB &
			1200,	2200,	3200

		STRG$ = ""
		GOTO 550
	END IF

	ON ON_LOOP% GOSUB &
		1100,	2100,	3100

	STRG$ = ""

	GOTO 550

600	GOTO ExitProgram

1000	!
	! PR_HIS_PAY file does not exist, so create it
	!
	!======================================================================
	CALL ENTR_3MESSAGE(SCOPE, &
		"Creating new PR_HIS_PAY file for " + BATCH_NO$, 1%)

 !	KILL PR_HIS_PAY.DEV$ + "PR_HIS_PAY_" + BATCH_NO$ + ".JRL"

	SMG_STATUS% = LIB$DELETE_FILE(PR_HIS_PAY.DEV$ + &
		"PR_HIS_PAY_" + BATCH_NO$ + ".JRL;*")

1050	%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.CRE"
	CLOSE PR_HIS_PAY.CH%
	%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.UPD"

1090	! Convert the PR_HIS_PAY
	RETURN

1100	!
	! Create new record
	!
	PR_HIS_PAY::EMPNUM	= FIND_STRING(STRG$, "EMPNUM")
	PR_HIS_PAY::PR_END_DATE	= FIND_STRING(STRG$, "PR_END_DATE")
	PR_HIS_PAY::EMP_SKILL	= FIND_STRING(STRG$, "EMP_CLASS")
	PR_HIS_PAY::EMP_GRADE	= FIND_STRING(STRG$, "EMP_TYPE")
	PR_HIS_PAY::ACCT	= EDIT$(FIND_STRING(STRG$, "ACCT"), -1%)
	PR_HIS_PAY::SUBACC	= FIND_STRING(STRG$, "SUBACC")
	PR_HIS_PAY::OPER	= FIND_STRING(STRG$, "OPER")
	PR_HIS_PAY::LOCATION	= FIND_STRING(STRG$, "LOCATION")
	PR_HIS_PAY::DEPT	= FIND_STRING(STRG$, "DEPT")
	PR_HIS_PAY::WORK_CENTER	= FIND_STRING(STRG$, "WORK_CENTER")
	PR_HIS_PAY::UNION	= FIND_STRING(STRG$, "UNION")
	PR_HIS_PAY::PTYPE	= FIND_STRING(STRG$, "PTYPE")
	PR_HIS_PAY::RTYPE	= FIND_STRING(STRG$, "RTYPE")
	PR_HIS_PAY::CODE	= FIND_STRING(STRG$, "CODE")
	PR_HIS_PAY::HOUR_RATE	= VAL(FIND_STRING(STRG$, "HOUR_RATE"))
	PR_HIS_PAY::REG_HR	= VAL(FIND_STRING(STRG$, "REG_HR"))
	PR_HIS_PAY::OVT_HR	= VAL(FIND_STRING(STRG$, "OVT_HR"))
	PR_HIS_PAY::PIECE	= 0.0
	PR_HIS_PAY::FACTOR	= VAL(FIND_STRING(STRG$, "FACTOR"))
	PR_HIS_PAY::GROSS	= FUNC_ROUND(VAL(FIND_STRING(STRG$, "GROSS")), 2%)
	PR_HIS_PAY::TAX_PKG	= FIND_STRING(STRG$, "TAX_PKG")
	PR_HIS_PAY::BATCH_ENTRY	= FIND_STRING(STRG$, "BATCH_ENTRY")
	PR_HIS_PAY::UPDATE_FLAG	= VAL(FIND_STRING(STRG$, "UPDATE_FLAG"))
	PR_HIS_PAY::SEQNUM	= FIND_STRING(STRG$, "SEQNUM")

	!
	! Fixup so things don't magically go screwy
	!
	IF (PR_HIS_PAY::PTYPE = "P") AND &
		(PR_HIS_PAY::REG_HR = 0.0) AND &
		(PR_HIS_PAY::OVT_HR = 0.0)
	THEN
		PR_HIS_PAY::PTYPE = "O"
	END IF

	PUT #PR_HIS_PAY.CH%

	RETURN

1200	CLOSE #PR_HIS_PAY.CH%
	RETURN

2000	!
	! PR_HIS_DED file does not exist, so create it
	!
	!======================================================================
	CALL ENTR_3MESSAGE(SCOPE, &
		"Creating new PR_HIS_DED file for " + BATCH_NO$, 1%)

 !	KILL PR_HIS_DED.DEV$ + "PR_HIS_DED_" + BATCH_NO$ + ".JRL"

	SMG_STATUS% = LIB$DELETE_FILE(PR_HIS_DED.DEV$ + &
		"PR_HIS_DED_" + BATCH_NO$ + ".JRL;*")

2050	%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_DED.CRE"
	CLOSE PR_HIS_DED.CH%
	%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_DED.UPD"

2090	! Convert the PR_HIS_DED
	RETURN

2100	!
	! Create new record
	!
	PR_HIS_DED::EMPNUM	= FIND_STRING(STRG$, "EMPNUM")
	PR_HIS_DED::PR_END_DATE	= FIND_STRING(STRG$, "PR_END_DATE")
	PR_HIS_DED::DTYPE	= FIND_STRING(STRG$, "DTYPE")
	PR_HIS_DED::CODE	= FIND_STRING(STRG$, "CODE")
	PR_HIS_DED::TAX_CODE	= FIND_STRING(STRG$, "TAX_CODE")
	PR_HIS_DED::SSTATUS	= FIND_STRING(STRG$, "STATUS")
	PR_HIS_DED::EXEMPT	= VAL(FIND_STRING(STRG$, "EXEMPT"))

	PR_HIS_DED::UPDATE_FLAG	= VAL(FIND_STRING(STRG$, "UPDATE_FLAG"))

	!
	! Split up the fica into two parts so they don't see a huge
	! adjustment in their first check
	!
	IF PR_HIS_DED::CODE = "FI"
	THEN
		TFICA = VAL(FIND_STRING(STRG$, "AMOUNT"))
		PR_HIS_DED::AMOUNT = FUNC_ROUND(TFICA * (6.2 / 7.65), 2%)
		PUT #PR_HIS_DED.CH%

		PR_HIS_DED::CODE = "FH"
		PR_HIS_DED::AMOUNT = TFICA - PR_HIS_DED::AMOUNT
		PUT #PR_HIS_DED.CH%
	ELSE
		PR_HIS_DED::AMOUNT	= VAL(FIND_STRING(STRG$, "AMOUNT"))
		PUT #PR_HIS_DED.CH%
	END IF

	RETURN

2200	CLOSE #PR_HIS_DED.CH%
	RETURN

3000	!
	! PR_HIS_CHECK file does not exist, so create it
	!
	!======================================================================
	CALL ENTR_3MESSAGE(SCOPE, &
		"Creating new PR_HIS_CHECK file for " + BATCH_NO$, 1%)

 !	KILL PR_HIS_CHECK.DEV$ + "PR_HIS_CHECK_" + BATCH_NO$ + ".JRL"

	SMG_STATUS% = LIB$DELETE_FILE(PR_HIS_CHECK.DEV$ + &
		"PR_HIS_CHECK_" + BATCH_NO$ + ".JRL;*")

3050	%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_CHECK.CRE"
	CLOSE PR_HIS_CHECK.CH%
	%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_CHECK.UPD"

3090	! Convert the PR_HIS_CHECK
	RETURN

3100	!
	! Create new record
	!
	PR_HIS_CHECK::EMPNUM		= FIND_STRING(STRG$, "EMPNUM")
	PR_HIS_CHECK::PR_END_DATE	= FIND_STRING(STRG$, "PR_END_DATE")
	PR_HIS_CHECK::CHECK		= FIND_STRING(STRG$, "CHECK")
	PR_HIS_CHECK::CHECK_DATE	= FIND_STRING(STRG$, "CHECK_DATE")
	PR_HIS_CHECK::PAYFREQ		= VAL(FIND_STRING(STRG$, "PAYFREQ"))

	PR_HIS_CHECK::UPDATE_FLAG	= VAL(FIND_STRING(STRG$, "UPDATE_FLAG"))

	PUT	#PR_HIS_CHECK.CH%

	RETURN

3200	CLOSE #PR_HIS_CHECK.CH%
	RETURN

 ExitProgram:
15000	!*******************************************************************
	! Exit program
	!*******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR) + &
		", LINE " + NUM1$(LINECOUNT%), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19000	!*******************************************************************
	! Error trapping
	!*******************************************************************

	!===================================================================
	! Error NUMBER(ERR) cases
	!===================================================================
	SELECT ERR

	CASE 134%, 153%	! Dup key detected, record already exists
		IF ERL > 1000%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"DUPLICATE KEY/RECORD EXISTS ERL" + &
				NUM1$(ERL) + " ERR" + NUM1$(ERR), 1%)
			RESUME 1090
		END IF
	END SELECT

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
