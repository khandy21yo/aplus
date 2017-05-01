1	%TITLE "Copy Labor Product to Product"
	%SBTTL "BM_SPEC_INITTHISHOURS"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 1995 BY
	!	Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This routine creates the ThisHours fields based on
	!	the existing hours.
	!	.lm -5
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS BM_SOURCE:BM_SPEC_INITTHISHOURS
	!	$ LINK/EXE=BM_EXE: BM_SPEC_INITTHISHOURS,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE BM_SPEC_INITTHISHOURS.OBJ;*
	!
	! Author:
	!
	!	06/12/95 - Kevin Handy
	!
	! Modification history:
	!
	!	06/16/95 - Kevin Handy
	!		Modified to handle operations better with
	!		multiple effective dates, instead of picking
	!		just the first one.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/09/99 - Kevin Handy
	!		Lose HelpError (Dead Code)
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"
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
	DECLARE	BM_PRODOPER_CDD	BM_PRODOPER_ARR(50%)

	%PAGE

	ON ERROR GOTO 19000


300	!
	! Open relation file
	!
	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.OPN"

310	!
	! Open relation file
	!
	%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.MOD"

320	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"


900	INPUT "Confirm labor update (NO/Yes"; INP$

	IF LEFT(EDIT$(INP$, -1%), 1%) <> "Y"
	THEN
		GOTO ExitFunction
	END IF

990	GET #PD_PRODUCT.CH%, REGARDLESS

	ITEM% = 0%

	PRINT PD_PRODUCT::PRODUCT_NUM

1000	!
	! Get existing labor (Keep hours at this level)
	!
 !	GET #BM_PRODOPER.CH%, KEY#0% EQ PD_PRODUCT::PRODUCT_NUM
 !
 !	ITEM% = ITEM% + 1%
 !	BM_PRODOPER_ARR(ITEM%) = BM_PRODOPER
 !
 !	GOTO 1000

	!*******************************************************************
	! Scan next level down for all operations
	!*******************************************************************

2000	FIND #BM_RELATION.CH%, KEY#0% EQ PD_PRODUCT::PRODUCT_NUM, REGARDLESS

 GetNextRec:
	!
	! Get next record
	!
	GET #BM_RELATION.CH%, REGARDLESS

	GOTO 2100 IF PD_PRODUCT::PRODUCT_NUM <> BM_RELATION::PRODUCT

	OPERATION$ = "~~~~~~~~~~~~~~~~"
	OPERATIONDATE$ = "~~~~~~~~~~~~~~~~"
	OPERATION = 0.0
	OPERATION% = 0%

2010	FIND #BM_PRODOPER.CH%, KEY #0% GE BM_RELATION::COMPONENT

 ReadOperation:
	GET #BM_PRODOPER.CH%

	IF BM_PRODOPER::PRODUCT == BM_RELATION::COMPONENT
	THEN
		IF OPERATION$ = BM_PRODOPER::OPERATION
		THEN
			IF OPERATIONDATE$ < BM_PRODOPER::EFFDATE
			THEN
				BM_PRODOPER_ARR(OPERATION%)::HOURS = &
					BM_PRODOPER_ARR(OPERATION%)::HOURS + &
					FUNC_ROUND(BM_PRODOPER::HOURS * &
					BM_RELATION::QUANTITY - OPERATION, 6%)
			END IF
		ELSE

			!
			! See if we already have an entry for this operation
			!
			FOR I%=1% TO ITEM%
				GOTO SumOperation &
					IF BM_PRODOPER_ARR(I%)::OPERATION = &
					BM_PRODOPER::OPERATION
			NEXT I%

			!
			! No current entry for operation, so add one in
			!
			ITEM%, I% = ITEM% + 1%
			BM_PRODOPER_ARR(I%) = BM_PRODOPER
			BM_PRODOPER_ARR(I%)::HOURS = 0.0
			BM_PRODOPER_ARR(I%)::THISHOURS = 0.0

			!
			! Add to hours needed for this operation
			!
 SumOperation:
			BM_PRODOPER_ARR(I%)::HOURS = BM_PRODOPER_ARR(I%)::HOURS + &
				FUNC_ROUND(BM_PRODOPER::HOURS * &
				BM_RELATION::QUANTITY, 6%)

			OPERATION$ = BM_PRODOPER::OPERATION
			OPERATION% = I%
			OPERATION = BM_PRODOPER::HOURS * &
				BM_RELATION::QUANTITY
			OPERATIONDATE$ = BM_PRODOPER::EFFDATE
		END IF

		GOTO ReadOperation
	END IF

	! Try for next record
	!
	GOTO GetNextRec

2100	!*******************************************************************
	! Add records back into file
	!*******************************************************************

	FOR I%=1% TO ITEM%

		PRINT " >>"; BM_PRODOPER_ARR(I%)::OPERATION; I%

2110		GET #BM_PRODOPER.CH%, KEY #1% EQ &
			PD_PRODUCT::PRODUCT_NUM + &
			BM_PRODOPER_ARR(I%)::OPERATION


2120		!
		! Don't forget labor at this level.
		!
		BM_PRODOPER::THISHOURS = BM_PRODOPER::HOURS - &
			BM_PRODOPER_ARR(I%)::HOURS

		UPDATE #BM_PRODOPER.CH%

		PRINT "   "; BM_PRODOPER::OPERATION

2190	NEXT I%

	GOTO 990

 ExitFunction:

 !	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_COPY)

	GOTO 32767

	%PAGE

 ! HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
 !	PRINT "ERROR: "; ERR; " "; ERT$(ERR); " at line "; ERL
 !	GOTO ExitFunction

	%Page

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	SELECT ERL

	CASE 300%
		FILENAME$ = "BM_RELATION"

	CASE 310%
		FILENAME$ = "BM_PRODOPER"

	CASE 1000 ! Cant find record
		RESUME 2000 IF ERR = 155%
		FILENAME$ = "BM_PRODOPER"

	CASE 2000 ! End of the file
		RESUME 2100 IF ERR = 11% OR ERR = 155%
		FILENAME$ = "BM_RELATION"

	CASE 2010%
		RESUME GetNextRec IF ERR = 11% OR ERR = 155%

	CASE 2100 ! End of the file
		FILENAME$ = "BM_PRODOPER"

	CASE 2110%
		RESUME 2190

	END SELECT

	PRINT "ITEM% = "; ITEM%

	ON ERROR GOTO 0

32767	END
