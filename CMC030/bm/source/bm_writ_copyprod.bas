1	%TITLE "Copy BOM Product to Product"
	%SBTTL "BM_WRIT_COPYPROD"
	%IDENT "V3.6a Calico"

	FUNCTION LONG BM_WRIT_COPYPROD (STRING PRODUCT)
	!
	! COPYRIGHT (C) 1992 BY
	! Computer Management Center, Idaho Falls, Idaho.
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
	!	$ BAS BM_SOURCE:BM_WRIT_COPYPROD/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP BM_WRIT_COPYPROD
	!	$ DELETE BM_WRIT_COPYPROD.OBJ;*
	!
	! Author:
	!
	!	07/16/92 - Frank F. Starman
	!
	! Modification history:
	!
	!	01/08/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/24/93 - Frank F. Starman
	!		Check if from_ITEM is different from to_ITEM.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	06/21/95 - Kevin Handy
	!		Reformat source code to 80 columns.
	!
	!	06/21/95 - Kevin Handy
	!		Modified to copy laber along with the material.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/08/99 - Kevin Handy
	!		Lose HelpError (DeadCode)
	!
	!	10/20/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
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
	MAP (BM_PRODOPER)	BM_PRODOPER_CDD		BM_PRODOPER

	COM (CH_BM_RELATION) BM_RELATION.CH%
	COM (CH_PD_PRODUCT_READ) PD_PRODUCT.CH%
	COM (BM_PRODOPER_CH) BM_PRODOPER.CH%

	DECLARE LONG SMG_COPY

	%PAGE

	ON ERROR GOTO 19000

	!
	! Paint the background, and confirm close
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		6%, &
		60%, &
		SMG_COPY, &
		SMG$M_BORDER &
	)

	!
	! Label the display
	!
	SMG_STATUS% = SMG$LABEL_BORDER(SMG_COPY, &
		"Copy Product to Product")

	SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY, "From Product", 1%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY, "To   Product", 2%, 2%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_COPY, &
		SCOPE::SMG_PBID, &
		11%, &
		10% &
	)


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

310	IF PD_PRODUCT.CH% <= 0%
	THEN
		!
		! Open product file
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
		USE
			FILENAME$ = "PD_PRODUCT"
			EXIT HANDLER
		END WHEN
	END IF

320	IF BM_PRODOPER.CH% <= 0%
	THEN
		!
		! Open product file
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.CRE"
		USE
			FILENAME$ = "BM_PRODOPER"
			EXIT HANDLER
		END WHEN
	END IF

	FROM_ITEM$ = PRODUCT
	TO_ITEM$   = SPACE$(30%)

 CopyItem:
	CALL ENTR_3MESSAGE(SCOPE, "", 1%)

	!++
	! Abstract:FLD001
	!	^*(01) From Product _#\*
	!	.b
	!	.lm +5
	!	The ^*From Product _#\* field
	!	copies the records contained in the Product Components
	!	file, for this product _#, to a selected product _# which will
	!	be entered in field (02).
	!	.lm -5
	!
	! Index:
	!	.x From Product Number>Copy Product to Product
	!	.x Copy Product to Product>From Product Number
	!	.x Number>From Product
	!
	!--

 FromItem:
	FROM_ITEM$ = ENTR_3STRING(SCOPE, SMG_COPY, "1;15", "Product #", &
		FROM_ITEM$, FLAG%, "'E", DEFLT$)

	SELECT SCOPE::SCOPE_EXIT

	!
	! Exit key
	!
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitFunction

	CASE SMG$K_TRM_F14
		IF (MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX  ") = 1%)
		THEN
			FROM_ITEM$ = PD_PRODUCT::PRODUCT_NUM
		END IF
		GOTO FromItem

	END SELECT

	IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "Q0" + FROM_ITEM$) <> 1%
	THEN
		PD_PRODUCT::DESCRIPTION	= &
			STRING$(LEN(PD_PRODUCT::DESCRIPTION), A"?"B)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY, PD_PRODUCT::DESCRIPTION, &
			1%, 30%, , SMG$M_BOLD)

		GOTO FromItem
	END IF

	SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY, PD_PRODUCT::DESCRIPTION, &
		1%, 30%, , SMG$M_BOLD)


	IF FROM_ITEM$ <> PRODUCT AND &
		TO_ITEM$ = SPACE$(LEN(PD_PRODUCT::PRODUCT_NUM))
	THEN
		TO_ITEM$ = PRODUCT
	END IF

	!++
	! Abstract:FLD002
	!	^*(02) To Product _#\*
	!	.b
	!	.lm +5
	!	The ^*To Product _#\* field determines
	!	the product _# into which the selected product entered in
	!	field (01) will be copied.
	!	.lm -5
	!
	! Index:
	!	.x To Product Number>Copy Product to Product
	!	.x Copy Product to Product>To Product Number
	!
	!--

 ToItem:
	TO_ITEM$ = ENTR_3STRING(SCOPE, SMG_COPY, "2;15", "Product #", &
		TO_ITEM$, FLAG%, "'E", DEFLT$)

	SELECT SCOPE::SCOPE_EXIT

	!
	! Exit key
	!
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitFunction

	CASE SMG$K_TRM_F14
		IF (MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX  ") = 1%)
		THEN
			TO_ITEM$ = PD_PRODUCT::PRODUCT_NUM
		END IF
		GOTO ToItem

	END SELECT

	TO_WILD% = INSTR(1%, TO_ITEM$, "%") + INSTR(1%, TO_ITEM$, "*") + &
		INSTR(1%, TO_ITEM$, ",") + INSTR(1%, TO_ITEM$, "?") + &
		INSTR(1%, TO_ITEM$, "/")

	IF TO_WILD%
	THEN
		SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY, "Wildcard", &
			2%, 47%, , SMG$M_BOLD)

	ELSE
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PD_PRODUCT::PRODUCT_NUM))
		IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "Q0" + TO_ITEM$) <> 1%
		THEN
			PD_PRODUCT::DESCRIPTION = &
				STRING$(LEN(PD_PRODUCT::DESCRIPTION), A"?"B)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY, &
				PD_PRODUCT::DESCRIPTION, &
				2%, 30%, , SMG$M_BOLD)

			GOTO ToItem
		END IF

		SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY, &
			PD_PRODUCT::DESCRIPTION, &
			2%, 30%, , SMG$M_BOLD)

	END IF

	GOTO ToItem IF FROM_ITEM$ = TO_ITEM$

	SCOPE::PRG_ITEM = "CONFIRM"
	INP$ = ENTR_3YESNO(SCOPE, SMG_COPY, &
		"", "Confirm copy  - then press <Do> ", "N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	IF INP$ <> "Y"
	THEN
		GOTO CopyItem
	END IF

	CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)
	ITEM% = 0%
	ITEM2% = 0%

	GOTO 1000 IF TO_WILD% = 0%
	TO_WILD$ = TRM$(TO_ITEM$)

900	WHEN ERROR IN
		RESET #PD_PRODUCT.CH%
	USE
		CONTINUE CopyItem IF ERR = 11% OR ERR = 131%
		FILENAME$ = "PD_PRODUCT"
		EXIT HANDLER
	END WHEN

 GetProduct:
	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, REGARDLESS
	USE
		CONTINUE CopyItem IF ERR = 11% OR ERR = 131%
		FILENAME$ = "PD_PRODUCT"
		EXIT HANDLER
	END WHEN

	GOTO GetProduct &
		IF COMP_STRING(TRM$(PD_PRODUCT::PRODUCT_NUM), TO_WILD$) = 0%

	TO_ITEM$ = PD_PRODUCT::PRODUCT_NUM

1000	!
	! Remove existing BOM
	!
	WHEN ERROR IN
		GET #BM_RELATION.CH%, KEY #0% EQ TO_ITEM$
	USE
		CONTINUE 2000 IF ERR = 155%
		FILENAME$ = "BM_RELATION"
		EXIT HANDLER
	END WHEN

	DELETE #BM_RELATION.CH%
	GOTO 1000

	!
	! Add
	!
2000	WHEN ERROR IN
		FIND #BM_RELATION.CH%, KEY #0% EQ FROM_ITEM$, REGARDLESS
	USE
		CONTINUE 2100 IF ERR = 11% OR ERR = 155%
		FILENAME$ = "BM_RELATION"
		EXIT HANDLER
	END WHEN

	SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY, "Copy              ", 4%, 5%)

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

	GOTO 2100 IF FROM_ITEM$ <> BM_RELATION::PRODUCT

	ITEM% = ITEM% + 1%
	BM_RELATION::PRODUCT = TO_ITEM$
	PUT #BM_RELATION.CH%

	SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY, BM_RELATION::ITEMNUM + " " + &
		BM_RELATION::COMPONENT, 4%, 15%,, SMG$M_REVERSE)

	!
	! Try for next record
	!
	GOTO GetNextRec

2100	SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY, SPACE$(40%), 4%, 05%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY, "Copied " + &
		FORMAT$(ITEM%, "#### Material Items"), 5%, 5%)

3000	!
	! Remove existing BOM
	!
	WHEN ERROR IN
		GET #BM_PRODOPER.CH%, KEY #0% EQ TO_ITEM$
	USE
		CONTINUE 4000 IF ERR = 155%
		FILENAME$ = "BM_RELATION"
		EXIT HANDLER
	END WHEN

	DELETE #BM_PRODOPER.CH%
	GOTO 3000

	!
	! Add
	!
4000	WHEN ERROR IN
		FIND #BM_PRODOPER.CH%, KEY #0% EQ FROM_ITEM$, REGARDLESS
	USE
		CONTINUE 4100 IF ERR = 11% OR ERR = 155%
		FILENAME$ = "BM_PRODOPER"
		EXIT HANDLER
	END WHEN

 GetNextRec2:
	!
	! Get next record
	!
	WHEN ERROR IN
		GET #BM_PRODOPER.CH%, REGARDLESS
	USE
		CONTINUE 4100 IF ERR = 11% OR ERR = 155%
		FILENAME$ = "BM_PRODOPER"
		EXIT HANDLER
	END WHEN

	GOTO 4100 IF FROM_ITEM$ <> BM_PRODOPER::PRODUCT

	ITEM2% = ITEM2% + 1%
	BM_PRODOPER::PRODUCT = TO_ITEM$
	PUT #BM_PRODOPER.CH%

	SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY, BM_PRODOPER::ITEMNUM + " " + &
		BM_PRODOPER::OPERATION, 4%, 15%,, SMG$M_REVERSE)

	!
	! Try for next record
	!
	GOTO GetNextRec2

4100	SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY, SPACE$(40%), 4%, 05%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY, "Copied " + &
		FORMAT$(ITEM2%, "#### Labor Items"), 6%, 5%)

	GOTO GetProduct IF TO_WILD%
	GOTO CopyItem

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
 !	BM_WRIT_COPYPROD = CMC$_UNTERROR
 !	GOTO ExitFunction

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	ON ERROR GO BACK

32767	END FUNCTION
