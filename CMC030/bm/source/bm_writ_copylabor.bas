1	%TITLE "Copy Labor Product to Product"
	%SBTTL "BM_WRIT_COPYLABOR"
	%IDENT "V3.6a Calico"

	FUNCTION LONG BM_WRIT_COPYLABOR(STRING PRODUCT)
	!
	!	COPYRIGHT (C) 1986 BY
	!	Computer Management Center, Idaho Falls, Idaho.
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
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This function copies the BOM of one product to another.
	!	.lm -5
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS BM_SOURCE:BM_WRIT_COPYLABOR/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP BM_WRIT_COPYLABOR
	!	$ DELETE BM_WRIT_COPYLABOR.OBJ;*
	!
	! Author:
	!
	!	08/25/92 - Frank F. Starman
	!
	! Modification history:
	!
	!	09/21/92 - Kevin Handy
	!		Clean up (check)
	!
	!	03/02/94 - Kevin Handy
	!		Format to 80 columns.
	!
	!	03/03/94 - Kevin Handy
	!		Modification for new field THISHOURS in
	!		BM_PRODOPER file. (Threw in some comments for free).
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/15/95 - Kevin Handy
	!		Clean up unecessary externals.
	!
	!	08/08/96 - Kevin Handy
	!		Lose some commented out code
	!
	!	05/22/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/08/99 - Kevin Handy
	!		Lose HelpError (Dead Code)
	!
	!	10/20/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"

	!
	! Map file
	!
	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.HB"
	MAP	(BM_RELATION)	BM_RELATION_CDD	BM_RELATION

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.HB"
	MAP	(BM_PRODOPER)	BM_PRODOPER_CDD	BM_PRODOPER

	COM (CH_BM_RELATION)	BM_RELATION.CH%
	COM (CH_BM_PRODOPER)	BM_PRODOPER.CH%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION BM_READ_PRODOPER

	DECLARE LONG SMG_COPY

	DECLARE	BM_PRODOPER_CDD	BM_PRODOPER_ARR(50%)
	DECLARE	BM_PRODOPER_CDD	BM_PRODOPER_READ

	%PAGE

	ON ERROR GOTO 19000


300	IF BM_RELATION.CH% <= 0%
	THEN
		!
		! Open relation file
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.CRE"
		USE
			FILENAME$ = "BM_RELATION"
			EXIT HANDLER
		END WHEN
	END IF

310	IF BM_PRODOPER.CH% <= 0%
	THEN
		!
		! Open relation file
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.CRE"
		USE
			FILENAME$ = "BM_PRODOPER"
			EXIT HANDLER
		END WHEN
	END IF

	SCOPE::PRG_ITEM = "CONFIRM"
	INP$ = ENTR_3YESNO(SCOPE, SMG_COPY, &
		"", "Confirm labor copy  - then press <Do> ", "N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	IF INP$ <> "Y"
	THEN
		GOTO ExitFunction
	END IF

	CALL ENTR_3MESSAGE(SCOPE, "Copying", 1% + 16%)

	ITEM% = 0%

1000	!
	! Remove existing LABOR (Keep hours at this level)
	!
	WHEN ERROR IN
		GET #BM_PRODOPER.CH%, KEY #0% EQ PRODUCT
	USE
		CONTINUE 2000 IF ERR = 155%
		FILENAME$ = "BM_PRODOPER"
		EXIT HANDLER
	END WHEN

	IF BM_PRODOPER::THISHOURS <> 0.0
	THEN
		ITEM% = ITEM% + 1%
		BM_PRODOPER_ARR(ITEM%) = BM_PRODOPER
		BM_PRODOPER_ARR(ITEM%)::HOURS = 0.0
	END IF

	DELETE #BM_PRODOPER.CH%

	GOTO 1000

	!*******************************************************************
	! Scan next level down for all operations
	!*******************************************************************

2000	WHEN ERROR IN
		FIND #BM_RELATION.CH%, KEY #0% EQ PRODUCT, REGARDLESS
	USE
		CONTINUE 2100 IF ERR = 11% OR ERR = 155%
		FILENAME$ = "BM_RELATION"
		EXIT HANDLER
	END WHEN

 GetNextRec:
	!
	! Get next record
	!
	WHEN ERROR IN
		GET #BM_RELATION.CH%, REGARDLESS
	USE
		CONTINUE 2100 IF ERR = 11% OR ERR = 155%
		FILENAME$ = "BM_RELATION"
		EXIT HANDLER
	END WHEN

	GOTO 2100 IF PRODUCT <> BM_RELATION::PRODUCT
	OPERATION$ = "        "

 ReadOperation:
	IF BM_READ_PRODOPER(BM_RELATION::COMPONENT, &
		OPERATION$, "GT", DATE_TODAY, BM_PRODOPER_READ) = CMC$_NORMAL
	THEN
		OPERATION$ = BM_PRODOPER_READ::OPERATION

		!
		! See if we already have an entry for this operation
		!
		FOR I% = 1% TO ITEM%
			GOTO SumOperation &
				IF BM_PRODOPER_ARR(I%)::OPERATION = &
				BM_PRODOPER_READ::OPERATION
		NEXT I%

		!
		! No current entry for operation, so add one in
		!
		ITEM%, I% = ITEM% + 1%
		BM_PRODOPER_ARR(I%)::OPERATION = BM_PRODOPER_READ::OPERATION
		BM_PRODOPER_ARR(I%)::THISHOURS = 0.0

		!
		! Add to hours needed for this operation
		!
 SumOperation:
		BM_PRODOPER_ARR(I%)::HOURS = BM_PRODOPER_ARR(I%)::HOURS + &
			FUNC_ROUND(BM_PRODOPER_READ::HOURS * &
			BM_RELATION::QUANTITY, 6%)

		GOTO ReadOperation
	END IF

	! Try for next record
	!
	GOTO GetNextRec

2100	!*******************************************************************
	! Add records back into file
	!*******************************************************************

	FOR I% = 1% TO ITEM%

		BM_PRODOPER = BM_PRODOPER_ARR(I%)
		BM_PRODOPER::PRODUCT	= PRODUCT
		BM_PRODOPER::EFFDATE	= DATE_TODAY
		BM_PRODOPER::STAT	= "A"
		BM_PRODOPER::ITEMNUM	= FORMAT$(I%, "<0>###")

		!
		! Don't forget labor at this level.
		!
		BM_PRODOPER::HOURS = BM_PRODOPER::HOURS + BM_PRODOPER::THISHOURS

		WHEN ERROR IN
			PUT #BM_PRODOPER.CH%
		USE
			FILENAME$ = "BM_PRODOPER"
			EXIT HANDLER
		END WHEN

	NEXT I%

 ExitFunction:

	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_COPY)

	EXIT FUNCTION

	%PAGE

 ! HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
 !	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
 !		"E", ERN$, FILENAME$, NUM1$(ERR))
 !	BM_WRIT_COPYLABOR = CMC$_UNTERROR
 !	GOTO ExitFunction

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	ON ERROR GO BACK

32767	END FUNCTION
