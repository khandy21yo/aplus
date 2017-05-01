1	%TITLE "Convert from RSTS/E to VMS"
	%SBTTL "AD_CONV_1_ASSET"
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
	!	.b
	!	.lm +5
	!	This program is used in the conversion from RSTS/E
	!	Ver. 1 to VMS.
	!
	! Index:
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_CONV_1_ASSET/LINE
	!	$ LINK/EXECUTABLE=AD_EXE: AD_CONV_1_ASSET, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_CONV_1_ASSET.OBJ;*
	!
	! Author:
	!
	!	06/09/89 - J. Shad Rydalch
	!
	! Modification history:
	!
	!	07/15/91 - Kevin Handy
	!		Chenged line 230 to 243 to put it in proper
	!		sequence.
	!
	!	03/18/92 - Dan Perkins
	!		Commented out unused variables.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/26/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/15/99 - Kevin Handy
	!		Use WHEN ERROR
	!
	!	06/09/99 - Kevin Handy
	!		Lose HelpErr (Deas Code)
	!--

	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

10	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.HB"
	MAP (AD_35ASSET)	AD_35ASSET_CDD	AD_35ASSET

	%INCLUDE "SOURCE:[AD.OPEN]AD_ASSTYPE.HB"
	MAP (AD_ASSTYPE)	AD_ASSTYPE_CDD	AD_ASSTYPE

	!
	! Declare channels
	!
	CALL ASSG_CHANNEL(ASSYS.CH%, STAT%)

	!*******************************************************************
	! Initilize Convert
	!*******************************************************************

	CALL READ_INITIALIZE

240	CALL READ_DEVICE("ASSYS_ASC", ASSYS_ASC.DEV$, STAT%)

243	!
	! Convert the ASSET of accounts
	!
	STRG$ = ""

245	%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.CRE"
	%INCLUDE "SOURCE:[AD.OPEN]AD_ASSTYPE.CRE"

250	WHEN ERROR IN
		OPEN ASSYS_ASC.DEV$ + "ASSYS.ASC" FOR INPUT AS FILE ASSYS.CH%, &
			RECORDSIZE 512%
	USE
		CALL ENTR_3MESSAGE(SCOPE, "ASSYS.ASC file is missing", 0%)
		CONTINUE ExitProgram
	END WHEN

550	WHEN ERROR IN
		INPUT LINE #ASSYS.CH%, INP$
	USE
		CONTINUE 560 IF STRG$ <> "" AND ERR = 11%
		CONTINUE ExitProgram
	END WHEN

	IF INSTR(1%, INP$, CHR$(13%)) = 0%
	THEN
		STRG$ = STRG$ + INP$
		GOTO 550
	END IF

	IF INSTR(1%, INP$, "<STARTFILE>")
	THEN
		STRG$ = ""
		GOTO 550
	END IF

	IF INSTR(1%, INP$, "<ENDFILE>")
	THEN
		GOSUB 2200
		STRG$ = ""
		GOTO ExitProgram
	END IF

	STRG$ = STRG$ + INP$

560	GOSUB 2000
	COUNT% = COUNT% + 1%
	PRINT COUNT%; CHR$(13%)

	STRG$ = ""
	GOTO 550

2000	!COUNTER% = COUNTER% + 1%

	ASSET_NUM$ = MID(STRG$, 2%, 6%)
	DESCR1$ = MID(STRG$, 10%, 30%)
	DESCR2$ = MID(STRG$, 42%, 30%)
	DESCR2$ = MID(STRG$, 74%, 06%)
	DESCRIPTION$ = EDIT$((DESCR1$ + DESCR2$), 16%)
	PUR_DAT$ = MID(STRG$, 82%, 8%)
	COST = VAL(MID(STRG$, 125%, 12%))
	SALVAGE = VAL(MID(STRG$, 139%, 12%))
	ITC = VAL(MID(STRG$, 153%, 12%))

	!
	! AD_35ASSET RECORD
	!
	AD_35ASSET::ASSET_NUM	= ASSET_NUM$
	AD_35ASSET::DESCRIPTION	= DESCRIPTION$
	AD_35ASSET::ASSET_TYPE	= LEFT(ASSET_NUM$, 1%)
	AD_35ASSET::LOCATION	= "0001"
	AD_35ASSET::DEPT_NUM	= "      "
	AD_35ASSET::SERIAL_NUM	= SPACE$(20%)
	AD_35ASSET::SERVDATE	= PUR_DAT$
	AD_35ASSET::COST	= COST
	AD_35ASSET::SALVAGE	= SALVAGE
	AD_35ASSET::BONUS	= 0
	AD_35ASSET::ITC		= ITC
	AD_35ASSET::ITCREDUCE	= ITC / 2
	AD_35ASSET::UNITS	= 0

	IF RIGHT(ASSET_NUM$, 2%) = "00000"
	THEN
		AD_ASSTYPE::ASSET_TYPE = AD_35ASSET::ASSET_TYPE
		AD_ASSTYPE::DESCRIPTION = AD_35ASSET::DESCRIPTION
		PUT #AD_ASSTYPE.CH%
	ELSE
		PUT #AD_35ASSET.CH%

		RETURN
	END IF

2200	CLOSE #AD_35ASSET.CH%

	CALL ASSG_FREECHANNEL(AD_35ASSET.CH%)

	RETURN


 ExitProgram:
15000	!*******************************************************************
	! Exit program
	!*******************************************************************

	GOTO 32767

	%PAGE

 ! HelpError:
	!*******************************************************************
	! Help Message for an Error
	!*******************************************************************
 !	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
 !		"E", ERN$, FILENAME$, NUM1$(ERR))
 !
 !	GOTO ExitProgram

32767	END
