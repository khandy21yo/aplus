1	%TITLE "Convert from RSTS/E to VMS"
	%SBTTL "JC_CONV_CONVERT"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1992 BY
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
	!	.b
	!	.lm +5
	!	This program is used in the conversion from RSTS/E
	!	to VMS.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS JC_SOURCE:JC_CONV_CONVERT/LINE
	!	$ LINK/EXECUTABLE=JC_EXE: JC_CONV_CONVERT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE JC_CONV_CONVERT.OBJ;*
	!
	! Author:
	!
	!	09/28/92 - Dan Perkins
	!
	! Modification history:
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/01/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/28/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/18/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

10	ON ERROR GOTO 19000

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.HB"
	MAP (SB_BALANCE)	SB_BALANCE_CDD		SB_BALANCE

	!*******************************************************************
	! Initilize File to Convert
	!*******************************************************************

	!
	! Declare channels
	!
	CALL ASSG_CHANNEL(JCRMS.CH%, STAT%)

	CALL READ_INITIALIZE

	CALL READ_DEVICE("JCRMS_ASC", JCRMS_ASC.DEV$, STAT%)

	!
	! Open input file
	!
300	WHEN ERROR IN
		OPEN JCRMS_ASC.DEV$ + "JCRMS.ASC" FOR INPUT AS FILE JCRMS.CH%
	USE
		CALL ENTR_3MESSAGE(SCOPE, "File to convert is missing", 0%)
		CONTINUE ExitProgram
	END WHEN

	!
	! Process the input file
	!
	FILEFLAG% = 0%

1000	WHEN ERROR IN
		INPUT LINE #JCRMS.CH%, INP$
	USE
		CONTINUE ExitProgram
	END WHEN

	GOTO 1000 IF INP$ = ""

	INP$ = EDIT$(INP$, 4%)
	I2%  = INSTR(1%, INP$, ">")
	FLD$ = SEG$(INP$, 2%, I2% - 1%)
	DTA$ = RIGHT(INP$, I2% + 1%)

	SELECT FLD$

	CASE "STARTFILE SB_BALANCE"
		YYYYPP$ = LEFT(DTA$, 6%)
		OPEN_FLAG$ = RIGHT(DTA$, 7%)

		CONF$ = EDIT$(ENTR_3YESNO(SCOPE, DISPLAY_ID%, &
			"9;22", "Confirm converting SB_BALANCE file", &
			"N", 16%, "'", "N"), -1%)

		IF CONF$ = "Y"
		THEN
			GOSUB 3000
			GOSUB InitSBBalanceRec
			WORKFILE% = 1%
			FILEFLAG% = -1%
		END IF

	CASE "ENDFILE SB_BALANCE"
		CLOSE #SB_BALANCE.CH%
		CALL ASSG_FREECHANNEL(SB_BALANCE.CH%)
		FILEFLAG% = 0%

	END SELECT

	ON WORKFILE% GOSUB 10000 IF FILEFLAG%

	GOTO 1000

3000	!
	! Create IC_TRANSACTION file
	!
	CALL ENTR_3MESSAGE(SCOPE, "Creating new SB_BALANCE file", 1%)

 !	KILL SB_BALANCE.DEV$ + "SB_BALANCE.HIS" &
 !		IF OPEN_FLAG$ = ""

	SMG_STATUS% = LIB$DELETE_FILE(SB_BALANCE.DEV$ + "SB_BALANCE.HIS;*") &
		IF OPEN_FLAG$ = ""

3010	!======================================================================
	! SB_BALANCE file (create, open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(SB_BALANCE.CH%, STAT%)
	CALL READ_DEVICE("SB_BALANCE", SB_BALANCE.DEV$, STAT%)
	CALL READ_PROTECTION("SB_BALANCE", SB_BALANCE.PRO$, STAT%)
	CALL READ_CURPROTECTION(OLD_PROT$, STAT%)
	CALL WRIT_CURPROTECTION(SB_BALANCE.PRO$, STAT%)

	SB_BALANCE.NAME$ = SB_BALANCE.DEV$ + "SB_BALANCE.HIS"

	OPEN SB_BALANCE.NAME$ AS FILE SB_BALANCE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP SB_BALANCE, &
		BUFFER 32%, &
		PRIMARY KEY &
		( &
			SB_BALANCE::SYSTEM, &
			SB_BALANCE::SUBACCOUNT, &
			SB_BALANCE::OPERATION, &
			SB_BALANCE::ACCOUNT, &
			SB_BALANCE::PERIOD &
		), &
		ALTERNATE KEY &
		( &
			SB_BALANCE::PERIOD, &
			SB_BALANCE::SYSTEM, &
			SB_BALANCE::SUBACCOUNT &
		)	DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW NONE

	CALL WRIT_CURPROTECTION(OLD_PROT$, STAT%)

	RETURN

10000	SELECT FLD$

	CASE "ENDRECORD"
		WHEN ERROR IN
			PUT #SB_BALANCE.CH% IF ERRFLAG% = 0%
		USE
			FILENAME$ = "SB_BALANCE"
			CONTINUE HelpError
		END WHEN

		GOSUB InitSBBalanceRec

	CASE "SYSTEM"
		SB_BALANCE::SYSTEM	= DTA$

	CASE "SUBACCOUNT"
		SB_BALANCE::SUBACCOUNT	= DTA$

	CASE "OPERATION"
		SB_BALANCE::OPERATION	= DTA$

	CASE "ACCOUNT"
		SB_BALANCE::ACCOUNT	= DTA$

	CASE "PERIOD"
		SB_BALANCE::PERIOD	= DTA$

	CASE "AMOUNT"
		WHEN ERROR IN
			SB_BALANCE::AMOUNT	= VAL(DTA$)
		USE
			SB_BALANCE::AMOUNT	= 0.0
		END WHEN

	CASE "UNITS"
		WHEN ERROR IN
			SB_BALANCE::UNITS	= VAL(DTA$)
		USE
			SB_BALANCE::UNITS	= 0.0
		END WHEN

	CASE "HOURS"
		WHEN ERROR IN
			SB_BALANCE::HOURS	= VAL(DTA$)
		USE
			SB_BALANCE::HOURS	= 0.0
		END WHEN

	CASE "BEG_AMOUNT"
		WHEN ERROR IN
			SB_BALANCE::BEG_AMOUNT	= VAL(DTA$)
		USE
			SB_BALANCE::BEG_AMOUNT	= 0.0
		END WHEN

	CASE "BEG_UNITS"
		WHEN ERROR IN
			SB_BALANCE::BEG_UNITS	= VAL(DTA$)
		USE
			SB_BALANCE::BEG_UNITS	= 0.0
		END WHEN

	CASE "BEG_HOURS"
		WHEN ERROR IN
			SB_BALANCE::BEG_HOURS	= VAL(DTA$)
		USE
			SB_BALANCE::BEG_HOURS	= 0.0
		END WHEN

	END SELECT

	RETURN

 InitSBBalanceRec:
	SB_BALANCE::SYSTEM	= "JC"
	SB_BALANCE::SUBACCOUNT	= ""
	SB_BALANCE::OPERATION	= ""
	SB_BALANCE::ACCOUNT	= ""
	SB_BALANCE::PERIOD	= "199209"
	SB_BALANCE::AMOUNT	= 0.0
	SB_BALANCE::UNITS	= 0.0
	SB_BALANCE::HOURS	= 0.0
	SB_BALANCE::BEG_AMOUNT	= 0.0
	SB_BALANCE::BEG_UNITS	= 0.0
	SB_BALANCE::BEG_HOURS	= 0.0

	ERRFLAG% = 0%

	RETURN

 ExitProgram:
	!*******************************************************************
	! Exit program
	!*******************************************************************

	CLOSE #ICRMS3.CH%
	CALL ASSG_FREECHANNEL(ICRMS3.CH%)

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

	FILENAME$ = ""
	RESUME HelpError

32767	END
