1	%TITLE "READ FASFAX RAIS STANDARD FILE"
	%SBTTL "PR_CONV_CONVERT"
	%IDENT "V3.6a Calico"

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
	!	This program reads the FASFAX RAIS standard data file
	!	and
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_CONV_CONVERT/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_CONV_CONVERT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_CONV_CONVERT.OBJ;*
	!
	! Author:
	!
	!	06/28/89 - Lance Williams
	!
	! Modification history:
	!
	!	04/13/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/21/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/29/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.HB"
	MAP (PR_TRN_DED)	PR_TRN_DED_CDD	PR_TRN_DED

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_RATE.HB"
	MAP (PR_EMP_RATE)	PR_EMP_RATE_CDD	PR_EMP_RATE

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP (PR_TRN_PAY)		PR_TRN_PAY_CDD	PR_TRN_PAY

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.CRE"
	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.CRE"
	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_RATE.CRE"
	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.CRE"

	DECLARE STRING FUNCTION CHECK( STRING, WORD, WORD, BYTE)

10	ON ERROR GOTO 19000

	INPUT "Batch Number"; BATCH_NO$

	!
	! Declare channels
	!
	READFILE.CH% = 10%
	CALL ASSG_CHANNEL(INSYS.CH%, STAT%)

	!*******************************************************************
	! Initilize Convert
	!*******************************************************************

	CALL READ_INITIALIZE

240	CALL READ_DEVICE("INSYS_ASC", INSYS_ASC.DEV$, STAT%)
	!
	! Ask for the file date to read
	!
	INPUT "Date"; DATES$
	INPUT "Location"; LOCATION$

	!*******************************************************************
	! Initilize Convert
	!*******************************************************************

	STRG$ = ""
	READFILE$ = DATES$ + LOCATION$ + ".SDF"

250	OPEN  READFILE$ FOR INPUT AS FILE READFILE.CH%
	INPUT LINE #READFILE.CH%, STRG1$

300	INPUT LINE #READFILE.CH%, STRG$

	CUR_REC$ = MID(STRG$, 1%, 2%) + MID(STRG$, 8%, 1%)
	GOTO 560 IF CUR_REC$ = "701"
	GOTO 300 IF CUR_REC$ <> "700"

310	GOSUB 320

	STRG$ = ""
	GOTO 300

320	EMPLOYEE_NUM$ = CHECK(STRG$, 09%, 08%, 09%)
	EMPLOYEE_NAME$ = MID(STRG$, 17%, 12%) + " "
	SOCIAL_SECUR_N$ = CHECK(STRG$, 29%, 09%, 09%)
	LABOR_GROUP$ = CHECK(STRG$, 40%, 02%, 02%)

	GET #PR_EMP_MASTER.CH%, KEY #0% EQ EMPLOYEE_NUM$, REGARDLESS

	RETURN

330	!
	! PR_EMP_MASTER RECORD
	!
	PR_EMP_MASTER::EMPNUM	= EMPLOYEE_NUM$
	PR_EMP_MASTER::EMPNAME	= EMPLOYEE_NAME$
	PR_EMP_MASTER::SSN	= SOCIAL_SECUR_N$
	PR_EMP_MASTER::EMPNAME	= EMPLOYEE_NAME$
	PR_EMP_MASTER::EMPNAME	= EMPLOYEE_NAME$
	PR_EMP_MASTER::SUBACC	= "1000-00"
	PR_EMP_MASTER::ACCT	= "2000-00"
	PR_EMP_MASTER::LOCATION	= "0101"
	PR_EMP_MASTER::DEPT	= LABOR_GROUP$

	PUT	#PR_EMP_MASTER.CH%

340	RETURN


 Get_next_line:
540	INPUT LINE #READFILE.CH%, STRG1$

550	INPUT LINE #READFILE.CH%, STRG$

	CUR_REC$ = MID(STRG$, 1%, 2%) + MID(STRG$, 8%, 1%)

	GOTO 550 IF CUR_REC$ <> "701"

560	GOSUB 2000

	STRG$ = ""
	GOTO 550

2000	EMPLOYEE_NUM$ = CHECK(STRG$, 09%, 08%, 09%)
	DAY_REG_HRS$ = CHECK(STRG$, 29%, 05%, 06%)
	DAY_OVER_HRS$ = CHECK(STRG$, 34%, 05%, 06%)
	DAY_TOT_MEA_CO$ = CHECK(STRG$, 49%, 02%, 02%)
	!DAY_TOT_MEA_CE$ = CHECK(STRG$, 51%, 08%, 08%)

	GET #PR_EMP_MASTER.CH%, KEY #0% EQ EMPLOYEE_NUM$, REGARDLESS
	GET #PR_EMP_RATE.CH%, KEY #0% EQ EMPLOYEE_NUM$, REGARDLESS
	GET #PR_TRN_DED.CH%, KEY #0% EQ EMPLOYEE_NUM$, REGARDLESS

	!
	! PR_TRN_DED RECORD
	!
	PR_TRN_PAY::EMPNUM	= EMPLOYEE_NUM$
	PR_TRN_PAY::PR_END_DATE	= PR_TRN_DED::PR_END_DATE
	PR_TRN_PAY::EMP_SKILL	= PR_EMP_MASTER::EMP_SKILL
	PR_TRN_PAY::EMP_GRADE	= PR_EMP_MASTER::EMP_GRADE
	PR_TRN_PAY::ACCT	= PR_EMP_MASTER::ACCT
	PR_TRN_PAY::SUBACC	= PR_EMP_MASTER::SUBACC
	PR_TRN_PAY::OPER	= PR_EMP_MASTER::OPER
	PR_TRN_PAY::LOCATION	= LOCATION$
	PR_TRN_PAY::DEPT	= PR_EMP_MASTER::DEPT
	PR_TRN_PAY::WORK_CENTER	= PR_EMP_MASTER::WORK_CENTER
	PR_TRN_PAY::UNION	= PR_EMP_MASTER::UNION
	PR_TRN_PAY::PTYPE	= "0.00"
	PR_TRN_PAY::RTYPE	= PR_EMP_MASTER::RATE_TYPE
	PR_TRN_PAY::CODE	= "0.00"
	PR_TRN_PAY::PIECE_RATE	= PR_EMP_RATE::PIECE_RATE
	PR_TRN_PAY::HOUR_RATE	= PR_EMP_RATE::HOUR_RATE
	PR_TRN_PAY::REG_HR	= VAL(DAY_REG_HRS$)
	PR_TRN_PAY::OVT_HR	= VAL(DAY_OVER_HRS$)
	PR_TRN_PAY::PIECE	= VAL(DAY_TOT_MEA_CO$)
	PR_TRN_PAY::FACTOR	= PR_EMP_RATE::FACTOR
	PR_TRN_PAY::GROSS	= PR_EMP_RATE::HOUR_RATE * VAL(DAY_REG_HRS$) + &
		VAL(DAY_OVER_HRS$) * PR_EMP_RATE::HOUR_RATE * PR_EMP_RATE::FACTOR
	PR_TRN_PAY::TAX_PKG	= PR_EMP_MASTER::TAX_PKG
	PR_TRN_PAY::BATCH_ENTRY	= "0.00"
	PR_TRN_PAY::UPDATE_FLAG	= PR_TRN_DED::UPDATE_FLAG
	PR_TRN_PAY::SEQNUM	= "0.00"
	PR_TRN_PAY::BATCH	= PR_TRN_DED::BATCH
	PR_TRN_PAY::WORKDATE	= DATES$

	PUT	#PR_TRN_PAY.CH%

2100	RETURN

2200	CLOSE #PR_EMP_MASTER.CH%
	CLOSE #PR_TRN_DED.CH%
	CLOSE #PR_EMP_RATE.CH%
	CLOSE #PR_TRN_PAY.CH%

 ExitProgram:
15000	!*******************************************************************
	! Exit program
	!*******************************************************************

	CLOSE READFILE.CH%

	GOTO 32767

	%PAGE

 HelpError:
	!*******************************************************************
	! Help Message for an Error
	!*******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

19000	!*******************************************************************
	! Error trapping
	!*******************************************************************
	SELECT ERR
	CASE 154%	! Locked Block
		SLEEP 1%
		RESUME
	END SELECT

	FILENAME$ = ""
	SELECT ERL

	CASE 320%	! End of file
		RESUME 330	IF ERR = 155%
		FILENAME$ = "PR_EMP_MASTER"

	CASE 550%	! End of file
		RESUME 15000	IF ERR = 11%
		FILENAME$ = "READFILE"

	CASE 2000%	! End of file
		SELECT ERR
			CASE 155%	! File not found.
			PRINT "PRODUCT NUMBER NOT FOUND! "
			RESUME 2100
		END SELECT

	END SELECT

	RESUME HelpError

	!
	! FUNCTION TO CHECK IF THERE ARE xx'S
	!
30000	DEF CHECK (STRING T_LINE,WORD START,WORD LENGTH, BYTE IMAGE_TYPE)

		DIVIDE_V = 1
		EXT_SPACE% = 0%
		SELECT IMAGE_TYPE
		CASE 1%
			P_IMAGE_TYPE$ = "# "
		CASE 2%
			P_IMAGE_TYPE$ = "## "
		CASE 3%
			P_IMAGE_TYPE$ = "### "
		CASE 4%
			P_IMAGE_TYPE$ = "##.## "
			EXT_SPACE% = 1%
			DIVIDE_V=100
		CASE 5%
			P_IMAGE_TYPE$ = "##### "
		CASE 6%
			P_IMAGE_TYPE$ = "###.## "
			DIVIDE_V=100
		CASE 7%
			P_IMAGE_TYPE$ = "##,###,### "
			EXT_SPACE% = 2%
		CASE 8%
			P_IMAGE_TYPE$ = "###,###.## "
			EXT_SPACE% = 2%
			DIVIDE_V=100
		CASE 9%
			P_IMAGE_TYPE$ = "######### "
		END SELECT
		TEMP$ = SPACE$(EXT_SPACE%) + MID(T_LINE, START, LENGTH) + " "
		TEMP$ = FORMAT$(VAL(TEMP$) / DIVIDE_V, P_IMAGE_TYPE$) &
			IF INSTR(1%, TEMP$, "x") = 0%
		CHECK = TEMP$

	END DEF

32767	END
