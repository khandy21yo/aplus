1	%TITLE "Convert from RSTS/E to VMS"
	%SBTTL "AD_CONV_CONVERT"
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
	!	$ BAS AD_SOURCE:AD_CONV_CONVERT/LINE
	!	$ LINK/EXECUTABLE=AD_EXE: AD_CONV_CONVERT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_CONV_CONVERT.OBJ;*
	!
	! Author:
	!
	!	08/20/92 - Dan Perkins
	!
	! Modification history:
	!
	!	09/21/92 - Kevin Handy
	!		Clean up (check).
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/25/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/28/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/13/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
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

	%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.HB"
	MAP (AD_35ASSET)	AD_35ASSET_CDD		AD_35ASSET

	%INCLUDE "SOURCE:[AD.OPEN]AD_BALANCE.HB"
	MAP (AD_BALANCE)	AD_BALANCE_CDD		AD_BALANCE

	%INCLUDE "SOURCE:[AD.OPEN]AD_DEPRECIATION.HB"
	MAP (AD_DEPRECIATION)	AD_DEPRECIATION_CDD	AD_DEPRECIATION

	!*******************************************************************
	! Initilize File to Convert
	!*******************************************************************

	!
	! Declare channels
	!
	CALL ASSG_CHANNEL(ADRMS.CH%, STAT%)

	CALL READ_INITIALIZE

	CALL READ_DEVICE("ADRMS_ASC", ADRMS_ASC.DEV$, STAT%)

	!
	! Open input file
	!
300	WHEN ERROR IN
		OPEN ADRMS_ASC.DEV$ + "ADRMS.ASC" FOR INPUT AS FILE ADRMS.CH%
	USE
		CALL ENTR_3MESSAGE(SCOPE, "File to convert is missing", 0%)
		CONTINUE ExitProgram
	END WHEN

	!
	! Process the input file
	!
	FILEFLAG% = 0%

1000	WHEN ERROR IN
		INPUT LINE #ADRMS.CH%, INP$
	USE
		CONTINUE ExitProgram
	END WHEN

	GOTO 1000 IF INP$ = ""

	INP$ = EDIT$(INP$, 4%)
	I2%  = INSTR(1%, INP$, ">")
	FLD$ = SEG$(INP$, 2%, I2% - 1%)
	DTA$ = RIGHT(INP$, I2% + 1%)

	SELECT FLD$

	CASE "STARTFILE AD_35ASSET"
		CONF$ = EDIT$(ENTR_3YESNO(SCOPE, DISPLAY_ID%, &
			"9;22", "Confirm converting AD_35ASSET file", &
			"N", 16%, "'", "N"), -1%)

		IF CONF$ = "Y"
		THEN
			GOSUB 3000
			GOSUB InitAD35assetRec
			WORKFILE% = 1%
			FILEFLAG% = -1%
		END IF

	CASE "ENDFILE AD_35ASSET"
		CLOSE #AD_35ASSET.CH%
		CALL ASSG_FREECHANNEL(AD_35ASSET.CH%)
		FILEFLAG% = 0%

	CASE "STARTFILE AD_BALANCE"
		CONF$ = EDIT$(ENTR_3YESNO(SCOPE, DISPLAY_ID%, &
			"9;21", "Confirm converting AD_BALANCE file", &
			"N", 16%, "'", "N"), -1%)

		IF CONF$ = "Y"
		THEN
			GOSUB 3100
			GOSUB InitADBalanceRec
			WORKFILE% = 2%
			FILEFLAG% = -1%
		END IF

	CASE "ENDFILE AD_BALANCE"
		CLOSE #AD_BALANCE.CH%
		CALL ASSG_FREECHANNEL(AD_BALANCE.CH%)
		FILEFLAG% = 0%

	CASE "STARTFILE AD_DEPRECIATION"
		CONF$ = EDIT$(ENTR_3YESNO(SCOPE, DISPLAY_ID%, &
			"9;21", "Confirm converting AD_DEPRECIATION file", &
			"N", 16%, "'", "N"), -1%)

		IF CONF$ = "Y"
		THEN
			GOSUB 3200
			GOSUB InitADDepreciationRec
			WORKFILE% = 3%
			FILEFLAG% = -1%
		END IF

	CASE "ENDFILE AD_DEPRECIATION"
		CLOSE #AD_DEPRECIATION.CH%
		CALL ASSG_FREECHANNEL(AD_DEPRECIATION.CH%)
		FILEFLAG% = 0%

	END SELECT

	ON WORKFILE% GOSUB 10000, 11000, 12000 IF FILEFLAG%

	GOTO 1000

3000	!
	! Create AD_35ASSET file
	!
	CALL ENTR_3MESSAGE(SCOPE, "Creating new AD_35ASSET file", 1%)

	SMG_STATUS% = LIB$DELETE_FILE(AD_35ASSET.DEV$ + "AD_35ASSET.MAS;*")

 !	WHEN ERROR IN
 !		KILL AD_35ASSET.DEV$ + "AD_35ASSET.MAS"
 !	USE
 !		CONTINUE 3010
 !	END WHEN

3010	%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.CRE"

	RETURN

3100	!
	! Create AD_BALANCE file
	!
	CALL ENTR_3MESSAGE(SCOPE, "Creating new AD_BALANCE file", 1%)
 !	WHEN ERROR IN
 !		KILL AD_BALANCE.DEV$ + "AD_BALANCE.MAS"
 !	USE
 !		CONTINUE 3110
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(AD_BALANCE.DEV$ + "AD_BALANCE.MAS;*")

3110	%INCLUDE "SOURCE:[AD.OPEN]AD_BALANCE.CRE"

	RETURN

3200	!
	! Create AD_DEPRECIATION file
	!
	CALL ENTR_3MESSAGE(SCOPE, "Creating new AD_DEPRECIATION file", 1%)
 !	WHEN ERROR IN
 !		KILL AD_DEPRECIATION.DEV$ + "AD_DEPRECIATION.MAS"
 !	USE
 !		CONTINUE 3110
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(AD_DEPRECIATION.DEV$ + &
		"AD_DEPRECIATION.MAS;*")

3210	%INCLUDE "SOURCE:[AD.OPEN]AD_DEPRECIATION.CRE"

	RETURN

10000	SELECT FLD$

	CASE "ENDRECORD"
		WHEN ERROR IN
			PUT #AD_35ASSET.CH% IF ERRFLAG% = 0%
		USE
			CONTINUE 1000 IF ERR = 134%

			IF ERR = 52%
			THEN
				ERRFLAG% = -1%
				CONTINUE 1000
			END IF

			FILENAME$ = "AD_35ASSET"
			CONTINUE HelpError
		END WHEN

		GOSUB InitAD35assetRec

	CASE "ASSET_NUM"
		AD_35ASSET::ASSET_NUM	= DTA$

	CASE "DESCRIPTION"
		AD_35ASSET::DESCRIPTION	= DTA$

	CASE "ASSET_TYPE"
		AD_35ASSET::ASSET_TYPE	= DTA$

	CASE "LOCATION"
		AD_35ASSET::LOCATION	= DTA$

	CASE "DEPT_NUM"
		AD_35ASSET::DEPT_NUM	= DTA$

	CASE "SERIAL_NUM"
		AD_35ASSET::SERIAL_NUM	= DTA$

	CASE "SERVDATE"
		AD_35ASSET::SERVDATE	= DTA$

	CASE "COST"
		AD_35ASSET::COST	= VAL(DTA$)

	CASE "SALVAGE"
		AD_35ASSET::SALVAGE	= VAL(DTA$)

	CASE "BONUS"
		AD_35ASSET::BONUS	= VAL(DTA$)

	CASE "ITC"
		AD_35ASSET::ITC		= VAL(DTA$)

	CASE "ITCREDUCE"
		AD_35ASSET::ITCREDUCE	= VAL(DTA$)

	CASE "UNITS"
		AD_35ASSET::UNITS	= VAL(DTA$)

	CASE "RET_DATE"
		AD_35ASSET::RET_DATE	= DTA$

	CASE "PROCEEDS"
		AD_35ASSET::PROCEEDS	= VAL(DTA$)

	CASE "NOTES"
		AD_35ASSET::NOTES	= DTA$

	END SELECT

	RETURN

11000	SELECT FLD$

	CASE "ENDRECORD"
		WHEN ERROR IN
			PUT #AD_BALANCE.CH% IF ERRFLAG% = 0%
		USE
			CONTINUE 1000 IF ERR = 134%

			IF ERR = 52%
			THEN
				ERRFLAG% = -1%
				CONTINUE 1000
			END IF

			FILENAME$ = "AD_BALANCE"
			CONTINUE HelpError
		END WHEN

		GOSUB InitADBalanceRec

	CASE "ASSET_NUM"
		AD_BALANCE::ASSET_NUM	= DTA$

	CASE "DEP_OBJECT"
		AD_BALANCE::DEP_OBJECT	= DTA$

	CASE "DEP_STATUS"
		AD_BALANCE::DEP_STATUS	= DTA$

	CASE "AMOUNT_CTD"
		AD_BALANCE::AMOUNT_CTD	= VAL(DTA$)

	CASE "UNIT_CTD"
		AD_BALANCE::UNIT_CTD	= VAL(DTA$)

	CASE "LASTPER"
		AD_BALANCE::LASTPER	= DTA$

	END SELECT

	RETURN

12000	SELECT FLD$

	CASE "ENDRECORD"
		WHEN ERROR IN
			PUT #AD_DEPRECIATION.CH%
		USE
			CONTINUE 1000 IF ERR = 134%
			FILENAME$ = "AD_DEPRECIATION"
			CONTINUE HelpError
		END WHEN

		GOSUB InitADDepreciationRec

	CASE "ASSET_NUM"
		AD_DEPRECIATION::ASSET_NUM	= DTA$

	CASE "DEP_OBJECT"
		AD_DEPRECIATION::DEP_OBJECT	= DTA$

	CASE "DEPCLASS"
		AD_DEPRECIATION::DEPCLASS	= DTA$

	END SELECT

	RETURN

 InitAD35assetRec:
	AD_35ASSET::ASSET_NUM	= ""
	AD_35ASSET::DESCRIPTION	= ""
	AD_35ASSET::ASSET_TYPE	= ""
	AD_35ASSET::LOCATION	= ""
	AD_35ASSET::DEPT_NUM	= ""
	AD_35ASSET::SERIAL_NUM	= ""
	AD_35ASSET::SERVDATE	= ""
	AD_35ASSET::COST	= 0.
	AD_35ASSET::SALVAGE	= 0.
	AD_35ASSET::BONUS	= 0.
	AD_35ASSET::ITC		= 0.
	AD_35ASSET::ITCREDUCE	= 0.
	AD_35ASSET::UNITS	= 0.
	AD_35ASSET::RET_DATE	= DTA$
	AD_35ASSET::PROCEEDS	= 0.
	AD_35ASSET::NOTES	= DTA$

	ERRFLAG% = 0%

	RETURN

 InitADBalanceRec:
	AD_BALANCE::ASSET_NUM	= ""
	AD_BALANCE::DEP_OBJECT	= ""
	AD_BALANCE::DEP_STATUS	= ""
	AD_BALANCE::AMOUNT_CTD	= 0.
	AD_BALANCE::UNIT_CTD	= 0.
	AD_BALANCE::LASTPER	= ""

	ERRFLAG% = 0%

	RETURN

 InitADDepreciationRec:
	AD_DEPRECIATION::ASSET_NUM	= ""
	AD_DEPRECIATION::DEP_OBJECT	= ""
	AD_DEPRECIATION::DEPCLASS	= ""

	RETURN

 ExitProgram:
	!*******************************************************************
	! Exit program
	!*******************************************************************

	CLOSE #ADRMS.CH%
	CALL ASSG_FREECHANNEL(ADRMS.CH%)

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
