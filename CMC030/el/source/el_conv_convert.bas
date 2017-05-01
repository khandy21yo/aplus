1	%TITLE "Convert from RSTS/E to VMS"
	%SBTTL "EL_CONV_CONVERT"
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
	!	$ BAS EL_SOURCE:EL_CONV_CONVERT/LINE
	!	$ LINK/EXECUTABLE=EL_EXE: EL_CONV_CONVERT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE EL_CONV_CONVERT.OBJ;*
	!
	! Author:
	!
	!	08/20/92 - Dan Perkins
	!
	! Modification history:
	!
	!	09/22/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/22/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

10	ON ERROR GOTO 19000

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE


	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[EL.OPEN]EL_EQUIPMENT.HB"
	MAP (SB_SUBACCOUNT)	EL_EQUIPMENT_CDD	EL_EQUIPMENT

	%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.HB"
	MAP (SB_BALANCE)	SB_BALANCE_CDD		SB_BALANCE

	!
	! External functions
	!
	EXTERNAL STRING FUNCTION ENTR_3YESNO

	!*******************************************************************
	! Initilize File to Convert
	!*******************************************************************

	!
	! Declare channels
	!
	CALL ASSG_CHANNEL(ELRMS.CH%, STAT%)

	CALL READ_INITIALIZE

	CALL READ_DEVICE("ELRMS_ASC", ELRMS_ASC.DEV$, STAT%)

	!
	! Open input file
	!
300	WHEN ERROR IN
		OPEN ELRMS_ASC.DEV$ + "ELRMS.ASC" FOR INPUT AS FILE ELRMS.CH%
	USE
		CALL ENTR_3MESSAGE(SCOPE, "File to convert is missing", 0%)
		CONTINUE ExitProgram
	END WHEN

	!
	! Process the input file
	!
	FILEFLAG% = 0%

1000	WHEN ERROR IN
		INPUT LINE #ELRMS.CH%, INP$
	USE
		CONTINUE ExitProgram
	END WHEN

	GOTO 1000 IF INP$ = ""

	INP$ = EDIT$(INP$, 4%)
	I2%  = INSTR(1%, INP$, ">")
	FLD$ = SEG$(INP$, 2%, I2% - 1%)
	DTA$ = RIGHT(INP$, I2% + 1%)

	SELECT FLD$

	CASE "STARTFILE SB_SUBACCOUNT"
		CONF$ = EDIT$(ENTR_3YESNO(SCOPE, DISPLAY_ID%, &
			"9;22", "Confirm converting SB_SUBACCOUNT file", &
			"N", 16%, "'", "N"), -1%)

		IF CONF$ = "Y"
		THEN
			GOSUB 3000
			GOSUB InitELEquipRec
			WORKFILE% = 1%
			FILEFLAG% = -1%
		END IF

	CASE "ENDFILE SB_SUBACCOUNT"
		CLOSE #SB_SUBACCOUNT.CH%
		CALL ASSG_FREECHANNEL(SB_SUBACCOUNT.CH%)
		FILEFLAG% = 0%

	CASE "STARTFILE SB_BALANCE"
		YYYYPP$ = LEFT(DTA$, 6%)
		OPEN_FLAG$ = RIGHT(DTA$, 7%)

		CONF$ = EDIT$(ENTR_3YESNO(SCOPE, DISPLAY_ID%, &
			"9;22", "Confirm converting SB_BALANCE file", &
			"N", 16%, "'", "N"), -1%)

		IF CONF$ = "Y"
		THEN
			GOSUB 3100
			GOSUB InitSBBalanceRec
			WORKFILE% = 2%
			FILEFLAG% = -1%
		END IF

	CASE "ENDFILE SB_BALANCE"
		CLOSE #SB_BALANCE.CH%
		CALL ASSG_FREECHANNEL(SB_BALANCE.CH%)

		CLOSE #SB_SUBACCOUNT.CH%
		CALL ASSG_FREECHANNEL(SB_SUBACCOUNT.CH%)

		FILEFLAG% = 0%

	END SELECT

	ON WORKFILE% GOSUB 10000, 11000 IF FILEFLAG%

	GOTO 1000

3000	!
	! Create EL_EQUIPMENT file
	!
	CALL ENTR_3MESSAGE(SCOPE, "Creating EL_EQUIPMENT file", 1%)

	!KILL SB_SUBACCOUNT.DEV$ + "SB_SUBACCOUNT.MAS"

3010	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.CRE"

	RETURN

3100	!
	! Create SB_BALANCE file
	!
	CALL ENTR_3MESSAGE(SCOPE, "Creating SB_BALANCE file", 1%)

	!KILL SB_BALANCE.DEV$ + "SB_BALANCE.HIS"

3110	%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.CRE"

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.CRE"

	RETURN

10000	SELECT FLD$

	CASE "ENDRECORD"
		WHEN ERROR IN
			PUT #SB_SUBACCOUNT.CH% IF ERRFLAG% = 0%
		USE
			IF ERR = 134%
			THEN
				ERRFLAG% = -1%
				CONTINUE 1000
			END IF

			FILENAME$ = "EL_EQUIPMENT"
			CONTINUE HelpError
		END WHEN

		GOSUB InitELEquipRec

	CASE "SUBJECT"
		EL_EQUIPMENT::SUBJECT	= DTA$

	CASE "EQNUM"
		EL_EQUIPMENT::EQNUM	= DTA$

	CASE "DESCR"
		EL_EQUIPMENT::DESCR	= DTA$

	CASE "TTYPE"
		EL_EQUIPMENT::TTYPE	= DTA$

	CASE "CLASS"
		EL_EQUIPMENT::CLASS	= DTA$

	CASE "BDATE"
		EL_EQUIPMENT::BDATE	= DTA$

	CASE "SSTATUS"
		EL_EQUIPMENT::SSTATUS	= DTA$

	CASE "EDATE"
		EL_EQUIPMENT::EDATE	= DTA$

	CASE "LOCATION"
		EL_EQUIPMENT::LOCATION	= DTA$

	CASE "OPERATOR"
		EL_EQUIPMENT::OPERATOR	= DTA$

	CASE "REFNO"
		EL_EQUIPMENT::REFNO	= DTA$

	END SELECT

	RETURN

11000	SELECT FLD$

	CASE "ENDRECORD"
		WHEN ERROR IN
			PUT #SB_BALANCE.CH% IF ERRFLAG% = 0%
		USE
			IF ERR = 52% OR ERR = 134%
			THEN
				ERRFLAG% = -1%
				CONTINUE 1000
			END IF

			FILENAME$ = "SB_BALANCE"
			CONTINUE HelpError
		END WHEN

		GOSUB TestSubaccount

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
		SB_BALANCE::AMOUNT	= VAL(DTA$)

	CASE "UNITS"
		SB_BALANCE::UNITS	= VAL(DTA$)

	CASE "HOURS"
		SB_BALANCE::HOURS	= VAL(DTA$)

	CASE "BEG_AMOUNT"
		SB_BALANCE::BEG_AMOUNT	= VAL(DTA$)

	CASE "BEG_UNITS"
		SB_BALANCE::BEG_UNITS	= VAL(DTA$)

	CASE "BEG_HOURS"
		SB_BALANCE::BEG_HOURS	= VAL(DTA$)

	END SELECT

	RETURN

 InitELEquipRec:
	EL_EQUIPMENT::SUBJECT	= "E"
	EL_EQUIPMENT::EQNUM	= ""
	EL_EQUIPMENT::DESCR	= ""
	EL_EQUIPMENT::TTYPE	= ""
	EL_EQUIPMENT::CLASS	= ""
	EL_EQUIPMENT::BDATE	= ""
	EL_EQUIPMENT::SSTATUS	= "A"
	EL_EQUIPMENT::EDATE	= ""
	EL_EQUIPMENT::LOCATION	= ""
	EL_EQUIPMENT::OPERATOR	= ""
	EL_EQUIPMENT::REFNO	= ""

	ERRFLAG% = 0%

	RETURN

 InitSBBalanceRec:
	SB_BALANCE::SYSTEM	= "EL"
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

 TestSubaccount:
12000	WHEN ERROR IN
		GET #SB_SUBACCOUNT.CH%, KEY #0% EQ "E" + SB_BALANCE::SUBACCOUNT
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 12050 IF ERR = 155%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

	GOTO EndSub

12050	GOSUB InitELEquipRec

	EL_EQUIPMENT::EQNUM = SB_BALANCE::SUBACCOUNT

12100	PUT #SB_SUBACCOUNT.CH%

 EndSub:
	RETURN

 ExitProgram:
	!*******************************************************************
	! Exit program
	!*******************************************************************

	CLOSE #WPRMS.CH%
	CALL ASSG_FREECHANNEL(WPRMS.CH%)

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

	RESUME HelpError

32767	END
