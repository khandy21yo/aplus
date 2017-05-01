1	%TITLE "Convert from RSTS/E to VMS"
	%SBTTL "PO_CONV_CONVERT"
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
	!	$ BAS PO_SOURCE:PO_CONV_CONVERT/LINE
	!	$ LINK/EXECUTABLE=PO_EXE: PO_CONV_CONVERT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PO_CONV_CONVERT.OBJ;*
	!
	! Author:
	!
	!	08/20/92 - Dan Perkins
	!
	! Modification history:
	!
	!	09/21/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/21/98 - Kevin Handy
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

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	MAP (PO_REG_LINE)	PO_REG_LINE_CDD		PO_REG_LINE

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.HB"
	MAP (PO_REG_SUB_LINE)	PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE

	!*******************************************************************
	! Initilize File to Convert
	!*******************************************************************

	!
	! Declare channels
	!
	CALL ASSG_CHANNEL(PORMS.CH%, STAT%)

	CALL READ_INITIALIZE

	CALL READ_DEVICE("PORMS_ASC", PORMS_ASC.DEV$, STAT%)

	!
	! Open input file
	!
300	WHEN ERROR IN
		OPEN PORMS_ASC.DEV$ + "PORMS.ASC" FOR INPUT AS FILE PORMS.CH%
	USE
		CALL ENTR_3MESSAGE(SCOPE, "File to convert is missing", 0%)
		CONTINUE ExitProgram
	END WHEN

	!
	! Process the input file
	!
	FILEFLAG% = 0%

1000	WHEN ERROR IN
		INPUT LINE #PORMS.CH%, INP$
	USE
		CONTINUE ExitProgram
	END WHEN

	GOTO 1000 IF INP$ = ""

	INP$ = EDIT$(INP$, 4%)
	I2%  = INSTR(1%, INP$, ">")
	FLD$ = SEG$(INP$, 2%, I2% - 1%)
	DTA$ = RIGHT(INP$, I2% + 1%)

	SELECT FLD$

	CASE "STARTFILE PO_REG_LINE"
		CONF$ = EDIT$(ENTR_3YESNO(SCOPE, DISPLAY_ID%, &
			"9;22", "Confirm converting PO_REG_LINE file", &
			"N", 16%, "'", "N"), -1%)

		IF CONF$ = "Y"
		THEN
			GOSUB 3000
			GOSUB InitPORegLineRec
			WORKFILE% = 1%
			FILEFLAG% = -1%
		END IF

	CASE "ENDFILE PO_REG_LINE"
		CLOSE #PO_REG_LINE.CH%
		CALL ASSG_FREECHANNEL(PO_REG_LINE.CH%)
		FILEFLAG% = 0%

	CASE "STARTFILE PO_REG_SUB_LINE"
		CONF$ = EDIT$(ENTR_3YESNO(SCOPE, DISPLAY_ID%, &
			"9;21", "Confirm converting PO_REG_SUB_LINE file", &
			"N", 16%, "'", "N"), -1%)

		IF CONF$ = "Y"
		THEN
			GOSUB 3100
			GOSUB InitPORegSubLineRec
			WORKFILE% = 2%
			FILEFLAG% = -1%
		END IF

	CASE "ENDFILE PO_REG_SUB_LINE"
		CLOSE #PO_REG_SUB_LINE.CH%
		CALL ASSG_FREECHANNEL(PO_REG_SUB_LINE.CH%)
		FILEFLAG% = 0%

	END SELECT

	ON WORKFILE% GOSUB 10000, 11000 IF FILEFLAG%

	GOTO 1000

3000	!
	! Create PO_REG_LINE file
	!
	CALL ENTR_3MESSAGE(SCOPE, "Creating new PO_REG_LINE file", 1%)
 !	KILL PO_REG_LINE.DEV$ + "PO_REG_LINE.LED"

	SMG_STATUS% = LIB$DELETE_FILE(PO_REG_LINE.DEV$ + "PO_REG_LINE.LED;*")

3010	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.CRE"

	RETURN

3100	!
	! Create PO_REG_SUB_LINE file
	!
	CALL ENTR_3MESSAGE(SCOPE, "Creating new PO_REG_SUB_LINE file", 1%)
 !	KILL PO_REG_SUB_LINE.DEV$ + "PO_REG_SUB_LINE.LED"

	SMG_STATUS% = LIB$DELETE_FILE(PO_REG_SUB_LINE.DEV$ + &
		"PO_REG_SUB_LINE.LED;*")

3110	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.CRE"

	RETURN

10000	SELECT FLD$

	CASE "ENDRECORD"
		WHEN ERROR IN
			PUT #PO_REG_LINE.CH%
		USE
			CONTINUE 1000 IF ERR = 134%
			FILENAME$ = "PO_REG_LINE"
			CONTINUE HelpError
		END WHEN

		GOSUB InitPORegLineRec

	CASE "PO"
		RSET PO_REG_LINE::PO		= TRM$(DTA$)

	CASE "PO_LINE"
		PO_REG_LINE::PO_LINE		= DTA$

	CASE "VENDOR"
		PO_REG_LINE::VENDOR		= DTA$

	CASE "FROMLOCATION"
		PO_REG_LINE::FROMLOCATION	= DTA$

	CASE "PRODUCT"
		PO_REG_LINE::PRODUCT		= DTA$

	CASE "UOM"
		PO_REG_LINE::UOM		= DTA$

	CASE "DESCRIPTION"
		PO_REG_LINE::DESCRIPTION	= DTA$

	CASE "PO_TYPE"
		PO_REG_LINE::PO_TYPE		= DTA$

	CASE "OPEN_CLOSE"
		PO_REG_LINE::OPEN_CLOSE		= DTA$

	CASE "ORDDATE"
		PO_REG_LINE::ORDDATE		= DTA$

	CASE "BATCH"
		PO_REG_LINE::BATCH		= DTA$

	CASE "PERIOD"
		PO_REG_LINE::PERIOD		= DTA$

	END SELECT

	RETURN

11000	SELECT FLD$

	CASE "ENDRECORD"
		WHEN ERROR IN
			PUT #PO_REG_SUB_LINE.CH% IF ERRFLAG% = 0%
		USE
			CONTINUE 1000 IF ERR = 134%
			FILENAME$ = "PO_REG_SUB_LINE"
			CONTINUE HelpError
		END WHEN

		GOSUB InitPORegSubLineRec

	CASE "PO"
		RSET PO_REG_SUB_LINE::PO	= TRM$(DTA$)

	CASE "PO_LINE"
		PO_REG_SUB_LINE::PO_LINE	= DTA$

	CASE "PO_ACTION"
		PO_REG_SUB_LINE::PO_ACTION	= DTA$

	CASE "ACTION_DATE"
		PO_REG_SUB_LINE::ACTION_DATE	= DTA$

	CASE "QTY"
		WHEN ERROR IN
			PO_REG_SUB_LINE::QTY		= VAL(DTA$)
		USE
			PO_REG_SUB_LINE::QTY		= 0.0
		END WHEN

	CASE "PRICE"
		WHEN ERROR IN
			PO_REG_SUB_LINE::PRICE		= VAL(DTA$)
		USE
			PO_REG_SUB_LINE::PRICE		= 0.0
		END WHEN

	CASE "SUBACCT"
		PO_REG_SUB_LINE::SUBACCT	= DTA$

	CASE "ACCOUNT"
		PO_REG_SUB_LINE::ACCOUNT	= EDIT$(DTA$, -1%)

	CASE "BATCH"
		PO_REG_SUB_LINE::BATCH		= DTA$

	CASE "POSTDATE"
		PO_REG_SUB_LINE::POSTDATE	= DTA$

	CASE "POSTTIME"
		PO_REG_SUB_LINE::POSTTIME	= DTA$

	END SELECT

	RETURN

 InitPORegLineRec:
	PO_REG_LINE::PO			= ""
	PO_REG_LINE::PO_LINE		= ""
	PO_REG_LINE::VENDOR		= ""
	PO_REG_LINE::FROMLOCATION	= ""
	PO_REG_LINE::PRODUCT		= ""
	PO_REG_LINE::UOM		= ""
	PO_REG_LINE::DESCRIPTION	= ""
	PO_REG_LINE::PO_TYPE		= ""
	PO_REG_LINE::OPEN_CLOSE		= ""
	PO_REG_LINE::ORDDATE		= ""
	PO_REG_LINE::BATCH		= "CONV"
	PO_REG_LINE::PERIOD		= ""

	RETURN

 InitPORegSubLineRec:
	PO_REG_SUB_LINE::PO		= ""
	PO_REG_SUB_LINE::PO_LINE	= ""
	PO_REG_SUB_LINE::PO_ACTION	= ""
	PO_REG_SUB_LINE::ACTION_DATE	= ""
	PO_REG_SUB_LINE::QTY		= 0.0
	PO_REG_SUB_LINE::PRICE		= 0.0
	PO_REG_SUB_LINE::SUBACCT	= ""
	PO_REG_SUB_LINE::ACCOUNT	= ""
	PO_REG_SUB_LINE::BATCH		= "CONV"
	PO_REG_SUB_LINE::POSTDATE	= "19920801"
	PO_REG_SUB_LINE::POSTTIME	= "000000"

	ERRFLAG% = 0%

	RETURN

 ExitProgram:
	!*******************************************************************
	! Exit program
	!*******************************************************************

	CLOSE #PORMS.CH%
	CALL ASSG_FREECHANNEL(PORMS.CH%)

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
