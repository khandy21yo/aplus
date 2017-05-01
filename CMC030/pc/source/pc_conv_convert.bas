1	%TITLE "Convert from RSTS/E to VMS"
	%SBTTL "PC_CONV_CONVERT"
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
	!	$ BAS PC_SOURCE:PC_CONV_CONVERT/LINE
	!	$ LINK/EXECUTABLE=PC_EXE: PC_CONV_CONVERT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PC_CONV_CONVERT.OBJ;*
	!
	! Author:
	!
	!	08/13/92 - Dan Perkins
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

	%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.HB"
	MAP (PC_PRICE)		PC_PRICE_CDD		PC_PRICE

	%INCLUDE "SOURCE:[PC.OPEN]PC_COST.HB"
	MAP (PC_COST)		PC_COST_CDD		PC_COST

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION PD_EXAM_PRODUCT

	%PAGE

	!*******************************************************************
	! Initilize File to Convert
	!*******************************************************************

	!
	! Declare channels
	!
	CALL ASSG_CHANNEL(ICRMS1.CH%, STAT%)

	CALL READ_INITIALIZE

	CALL READ_DEVICE("ICRMS1_ASC", ICRMS1_ASC.DEV$, STAT%)

	!
	! Open input file
	!
300	WHEN ERROR IN
		OPEN ICRMS1_ASC.DEV$ + "ICRMS1.ASC" FOR INPUT AS FILE ICRMS1.CH%
	USE
		CALL ENTR_3MESSAGE(SCOPE, "File to convert is missing", 0%)
		CONTINUE ExitProgram
	END WHEN

	!
	! Process the input file
	!
	FILEFLAG% = 0%

1000	WHEN ERROR IN
		INPUT LINE #ICRMS1.CH%, INP$
	USE
		CONTINUE ExitProgram
	END WHEN

	GOTO 1000 IF INP$ = ""

	INP$ = EDIT$(INP$, 4%)
	I2%  = INSTR(1%, INP$, ">")
	FLD$ = SEG$(INP$, 2%, I2% - 1%)
	DTA$ = RIGHT(INP$, I2% + 1%)

	SELECT FLD$

	CASE "STARTFILE PC_PRICE"
		CONF$ = EDIT$(ENTR_3YESNO(SCOPE, DISPLAY_ID%, &
			"9;22", "Confirm converting PC_PRICE file", &
			"N", 16%, "'", "N"), -1%)

		IF CONF$ = "Y"
		THEN
			GOSUB 3000
			GOSUB InitPCPriceRec
			WORKFILE% = 1%
			FILEFLAG% = -1%
		END IF

	CASE "ENDFILE PC_PRICE"
		CLOSE #PC_PRICE.CH%
		CALL ASSG_FREECHANNEL(PC_PRICE.CH%)
		FILEFLAG% = 0%

	CASE "STARTFILE PC_COST"
		GOSUB 3100
		GOSUB InitPCCostRec
		WORKFILE% = 2%
		FILEFLAG% = -1%

	CASE "ENDFILE PC_COST"
		CLOSE #PC_COST.CH%
		CALL ASSG_FREECHANNEL(PC_COST.CH%)
		FILEFLAG% = 0%

	END SELECT

	ON WORKFILE% GOSUB 10000, 11000 IF FILEFLAG%

	GOTO 1000

3000	!
	! Create PC_PRICE file
	!
	CALL ENTR_3MESSAGE(SCOPE, "Creating new PC_PRICE file", 1%)
 !	KILL IC_35BALANCE.DEV$ + "PC_PRICE.MAS"

	SMG_STATUS% = LIB$DELETE_FILE(IC_35BALANCE.DEV$ + "PC_PRICE.MAS;*")

3010	%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.CRE"

	RETURN

3100	!
	! Create PC_COST file
	!
	CALL ENTR_3MESSAGE(SCOPE, "Creating new PC_COST file", 1%)
 !	KILL IC_35HISTORY.DEV$ + "PC_COST.MAS"

	SMG_STATUS% = LIB$DELETE_FILE(IC_35HISTORY.DEV$ + "PC_COST.MAS;*")

3110	%INCLUDE "SOURCE:[PC.OPEN]PC_COST.CRE"

	RETURN

10000	SELECT FLD$

	CASE "ENDRECORD"
		IF PD_EXAM_PRODUCT(PC_PRICE::PRODUCT_NUM, &
			PD_PRODUCT_EXAM) = CMC$_NORMAL
		THEN
			WHEN ERROR IN
				PUT #PC_PRICE.CH% IF ERRFLAG% = 0%
			USE
				FILENAME$ = "PC_PRICE"
				CONTINUE HelpError
			END WHEN
		END IF

		GOSUB InitPCPriceRec

	CASE "PRODUCT_NUM"
		PC_PRICE::PRODUCT_NUM	= DTA$

	CASE "LOCATION"
		PC_PRICE::LOCATION	= DTA$

	CASE "PCTYPE"
		PC_PRICE::PCTYPE	= DTA$

	CASE "XDATE"
		PC_PRICE::XDATE		= DTA$

	CASE "XTIME"
		PC_PRICE::XTIME		= DTA$

	CASE "PRICECOST"
		WHEN ERROR IN
			PC_PRICE::PRICECOST	= VAL(DTA$)
		USE
			PC_PRICE::PRICECOST	= 0.0
		END WHEN

	END SELECT

	RETURN

11000	SELECT FLD$

	CASE "ENDRECORD"
		IF PD_EXAM_PRODUCT(PC_COST::PRODUCT, &
			PD_PRODUCT_EXAM) = CMC$_NORMAL
		THEN
			WHEN ERROR IN
				PUT #PC_COST.CH% IF ERRFLAG% = 0%
			USE
				FILENAME$ = "PC_COST"
				CONTINUE HelpError
			END WHEN

		END IF

		GOSUB InitPCCostRec

	CASE "PRODUCT"
		PC_COST::PRODUCT	= DTA$

	CASE "LOCATION"
		PC_COST::LOCATION	= DTA$

	CASE "EFFDATE"
		PC_COST::EFFDATE	= DTA$

	CASE "COST"
		WHEN ERROR IN
			PC_COST::COST		= VAL(DTA$)
		USE
			PC_COST::COST		= 0.0
		END WHEN


	END SELECT

	RETURN

 InitPCPriceRec:
	PC_PRICE::PRODUCT_NUM	= ""
	PC_PRICE::LOCATION	= ""
	PC_PRICE::PCTYPE	= ""
	PC_PRICE::XDATE		= "19920801"
	PC_PRICE::XTIME		= ""
	PC_PRICE::PRICECOST	= 0.

	ERRFLAG% = 0%

	RETURN

 InitPCCostRec:
	PC_COST::PRODUCT	= ""
	PC_COST::LOCATION	= ""
	PC_COST::EFFDATE	= "19920801"
	PC_COST::COST		= 0.

	ERRFLAG% = 0%

	RETURN

 ExitProgram:
	!*******************************************************************
	! Exit program
	!*******************************************************************

	CLOSE #ICRMS1.CH%
	CALL ASSG_FREECHANNEL(ICRMS1.CH%)

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
