1	%TITLE "Convert from RSTS/E to VMS"
	%SBTTL "BM_CONV_CONVERT"
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
	!	$ BAS BM_SOURCE:BM_CONV_CONVERT/LINE
	!	$ LINK/EXECUTABLE=BM_EXE: BM_CONV_CONVERT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE BM_CONV_CONVERT.OBJ;*
	!
	! Author:
	!
	!	08/14/92 - Dan Perkins
	!
	! Modification history:
	!
	!	09/21/92 - Kevin Handy
	!		Clean Up (Check)
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/13/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/14/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!		Use WHEN ERROR IN
	!
	!	02/08/2001 - Kevin Handy
	!		Use BM_???.DEV instead of IC_???.DEV
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.HB"
	MAP (BM_PRODOPER)	BM_PRODOPER_CDD		BM_PRODOPER

	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.HB"
	MAP (BM_RELATION)	BM_RELATION_CDD		BM_RELATION

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION PD_EXAM_PRODUCT

	%PAGE

10	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize File to Convert
	!*******************************************************************

	!
	! Declare channels
	!
	CALL ASSG_CHANNEL(BMRMS.CH%, STAT%)

	CALL READ_INITIALIZE

	CALL READ_DEVICE("BMRMS_ASC", BMRMS_ASC.DEV$, STAT%)

	!
	! Open input file
	!
300	WHEN ERROR IN
		OPEN BMRMS_ASC.DEV$ + "BMRMS.ASC" FOR INPUT AS FILE BMRMS.CH%
	USE
		CALL ENTR_3MESSAGE(SCOPE, "File to convert is missing", 0%)
		CONTINUE ExitProgram
	END WHEN

	!
	! Process the input file
	!
	FILEFLAG% = 0%

1000	WHEN ERROR IN
		INPUT LINE #BMRMS.CH%, INP$
	USE
		CONTINUE ExitProgram
	END WHEN

	GOTO 1000 IF INP$ = ""

	INP$ = EDIT$(INP$, 4%)
	I2%  = INSTR(1%, INP$, ">")
	FLD$ = SEG$(INP$, 2%, I2% - 1%)
	DTA$ = RIGHT(INP$, I2% + 1%)

	SELECT FLD$

	CASE "STARTFILE BM_PRODOPER"
		CONF$ = EDIT$(ENTR_3YESNO(SCOPE, DISPLAY_ID%, &
			"9;22", "Confirm converting BM_PRODOPER file", &
			"N", 16%, "'", "N"), -1%)

		IF CONF$ = "Y"
		THEN
			GOSUB 3000
			GOSUB InitBMProdoperRec
			WORKFILE% = 1%
			FILEFLAG% = -1%
		END IF

	CASE "ENDFILE BM_PRODOPER"
		CLOSE #BM_PRODOPER.CH%
		CALL ASSG_FREECHANNEL(BM_PRODOPER.CH%)
		FILEFLAG% = 0%

	CASE "STARTFILE BM_RELATION"
		CONF$ = EDIT$(ENTR_3YESNO(SCOPE, DISPLAY_ID%, &
			"9;22", "Confirm converting BM_RELATION file", &
			"N", 16%, "'", "N"), -1%)

		IF CONF$ = "Y"
		THEN
			GOSUB 3100
			GOSUB InitBMRelationRec
			WORKFILE% = 2%
			FILEFLAG% = -1%
		END IF

	CASE "ENDFILE BM_RELATION"
		CLOSE #BM_RELATION.CH%
		CALL ASSG_FREECHANNEL(BM_RELATION.CH%)
		FILEFLAG% = 0%

	END SELECT

	ON WORKFILE% GOSUB 10000, 11000 IF FILEFLAG%

	GOTO 1000

3000	!
	! Create BM_PRODOPER file
	!
	CALL ENTR_3MESSAGE(SCOPE, "Creating new BM_PRODOPER file", 1%)
 !	KILL BM_PRODOPER.DEV$ + "BM_PRODOPER.MAS"

	SMG_STATUS% = LIB$DELETE_FILE(BM_PRODOPER.DEV$ + "BM_PRODOPER.MAS;*")

3010	%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.CRE"

	RETURN

3100	!
	! Create BM_RELATION file
	!
	CALL ENTR_3MESSAGE(SCOPE, "Creating new BM_RELATION file", 1%)
 !	KILL BM_RELATION.DEV$ + "BM_RELATION.MAS"

	SMG_STATUS% = LIB$DELETE_FILE(BM_RELATION.DEV$ + "BM_RELATION.MAS;*")

3110	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.CRE"

	RETURN

10000	SELECT FLD$

	CASE "ENDRECORD"
		IF PD_EXAM_PRODUCT(BM_PRODOPER::PRODUCT, &
			PD_PRODUCT_EXAM) = CMC$_NORMAL
		THEN
			WHEN ERROR IN
				PUT #BM_PRODOPER.CH%
			USE
				IF ERR = 134%
				THEN
					CONTINUE 1000
				END IF

				IF ERR = 52%
				THEN
					BM_PRODOPER::HOURS	= 0.0
					CONTINUE 1000
				END IF

				FILENAME$ = "BM_PRODOPER"
				CONTINUE HelpError
			END WHEN
		END IF

		GOSUB InitBMProdoperRec

	CASE "PRODUCT"
		BM_PRODOPER::PRODUCT	= DTA$

	CASE "ITEMNUM"
		BM_PRODOPER::ITEMNUM	= DTA$

	CASE "OPERATION"
		BM_PRODOPER::OPERATION	= DTA$

	CASE "HOURS"
		BM_PRODOPER::HOURS	= VAL(DTA$)

	CASE "EFFDATE"
		BM_PRODOPER::EFFDATE	= DTA$

	CASE "STAT"
		BM_PRODOPER::STAT	= DTA$

	END SELECT

	RETURN

11000	SELECT FLD$

	CASE "ENDRECORD"
		IF PD_EXAM_PRODUCT(BM_RELATION::PRODUCT, &
			PD_PRODUCT_EXAM) = CMC$_NORMAL
		THEN
			WHEN ERROR IN
				PUT #BM_RELATION.CH%
			USE
				IF ERR = 134%
				THEN
					BM_RELATION::ITEMNUM = &
						FORMAT$(VAL(BM_RELATION::ITEMNUM) + 1%, "<0>###")
					RETRY
				END IF

				IF ERR = 52%
				THEN
					CONTINUE 1000
				END IF

				FILENAME$ = "BM_RELATION"
				CONTINUE HelpError
			END WHEN

		END IF

		GOSUB InitBMRelationRec

	CASE "PRODUCT"
		BM_RELATION::PRODUCT	= DTA$

	CASE "ITEMNUM"
		BM_RELATION::ITEMNUM	= DTA$

	CASE "COMPONENT"
		BM_RELATION::COMPONENT	= DTA$

	CASE "QUANTITY"
		BM_RELATION::QUANTITY	= VAL(DTA$)

	CASE "OPERATION"
		BM_RELATION::OPERATION	= DTA$

	CASE "SCRAP"
		BM_RELATION::SCRAP	= VAL(DTA$)

	END SELECT

	RETURN

 InitBMProdoperRec:
	BM_PRODOPER::PRODUCT	= ""
	BM_PRODOPER::ITEMNUM	= ""
	BM_PRODOPER::OPERATION	= ""
	BM_PRODOPER::HOURS	= 0.
	BM_PRODOPER::EFFDATE	= "19920801"
	BM_PRODOPER::STAT	= "A"

	RETURN

 InitBMRelationRec:
	BM_RELATION::PRODUCT	= ""
	BM_RELATION::ITEMNUM	= ""
	BM_RELATION::COMPONENT	= ""
	BM_RELATION::QUANTITY	= 0.
	BM_RELATION::OPERATION	= ""
	BM_RELATION::SCRAP	= 0%

	RETURN

 ExitProgram:
	!*******************************************************************
	! Exit program
	!*******************************************************************

	CLOSE #BMRMS.CH%
	CALL ASSG_FREECHANNEL(BMRMS.CH%)

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
