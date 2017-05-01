1	%TITLE "Substitute Product in BOM"
	%SBTTL "BM_WRIT_SUBS"
	%IDENT "V3.6a Calico"

	FUNCTION LONG BM_WRIT_SUBS(STRING PRODUCT)

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
	!	.p
	!	This function substitutes BOM of one product to another
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS BM_SOURCE:BM_WRIT_SUBS/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP BM_WRIT_SUBS
	!	$ DELETE BM_WRIT_SUBS.OBJ;*
	!
	! Author:
	!
	!	12/19/92 - Frank F. Starman
	!
	! Modification history:
	!
	!	03/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
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

	COM (CH_BM_RELATION)	BM_RELATION.CH%

	DECLARE LONG SMG_COPY

	%PAGE

	ON ERROR GOTO 19000

	!
	! Paint the background, and confirm close
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		5%, &
		60%, &
		SMG_COPY, &
		SMG$M_BORDER &
	)

	!
	! Label the display
	!
	SMG_STATUS% = SMG$LABEL_BORDER(SMG_COPY, &
		"Product Substitution")

	SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY, "Old Product", 1%, 02%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY, "New Product", 2%, 02%)

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

	FROM_ITEM$ = PRODUCT
	TO_ITEM$   = SPACE$(LEN(PD_PRODUCT::PRODUCT_NUM))

 CopyItem:
	CALL ENTR_3MESSAGE(SCOPE, "", 1%)

	!++
	! Abstract:FLD001
	!	^*(01) Old Product _#\*
	!	.p
	!	The ^*Old Product _#\* field
	!	substitutes this product as component with a new
	!	product entered in field (02).
	!
	! Index:
	!	.x Old Product Number>Substitute Product
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


	IF FROM_ITEM$ <> PRODUCT AND TO_ITEM$ = &
		SPACE$(LEN(PD_PRODUCT::PRODUCT_NUM))
	THEN
		TO_ITEM$ = PRODUCT
	END IF

	!++
	! Abstract:FLD002
	!	^*(02) New Product _#\*
	!	.p
	!	The ^*New Product _#\* field determines
	!	the product _# which will replace product entered in
	!	field (01).
	!
	! Index:
	!	.x New Product Number>Substitute Product
	!
	!--

 ToItem:
	TO_ITEM$ = ENTR_3STRING(SCOPE, SMG_COPY, "2;15", "New Product #", &
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

	IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "Q0" + TO_ITEM$) <> 1%
	THEN
		PD_PRODUCT::DESCRIPTION	= &
			STRING$(LEN(PD_PRODUCT::DESCRIPTION), A"?"B)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY, PD_PRODUCT::DESCRIPTION, &
			2%, 30%, , SMG$M_BOLD)

		GOTO ToItem
	END IF

	SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY, PD_PRODUCT::DESCRIPTION, &
		2%,30%, , SMG$M_BOLD)

	SCOPE::PRG_ITEM = "CONFIRM"
	INP$ = ENTR_3YESNO(SCOPE, SMG_COPY, &
		"", "Confirm substituion  - then press <Do> ", "N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	IF INP$ <> "Y"
	THEN
		GOTO CopyItem
	END IF

	CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)
	ITEM% = 0%

	!
	! Add
	!
 GetNextRec:
2000	WHEN ERROR IN
		GET #BM_RELATION.CH%, KEY #1% EQ FROM_ITEM$
	USE
		CONTINUE 2100 IF ERR = 11% OR ERR = 155%
		FILENAME$ = "BM_RELATION"
		EXIT HANDLER
	END WHEN

	!SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY, "Copy              ", &
	!	'4'L, '05'L)

	GOTO 2100 IF FROM_ITEM$ <> BM_RELATION::COMPONENT

	DELETE #BM_RELATION.CH%

	BM_RELATION::COMPONENT	= TO_ITEM$
	PUT #BM_RELATION.CH%

	ITEM% = ITEM% + 1%

	!SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY, BM_RELATION::ITEMNUM+' '+ &
	!	BM_RELATION::COMPONENT, '4'L, '15'L,, SMG$M_REVERSE)

	!
	! Try for next record
	!
	GOTO GetNextRec

2100	!SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY, SPACE$(40%),'4'L, '05'L)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY, "Substitued " + &
		FORMAT$(ITEM%, "#### Items"), 5%, 05%)

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
 !	BM_WRIT_SUBS = CMC$_UNTERROR
 !	GOTO ExitFunction

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	ON ERROR GO BACK

32767	END FUNCTION
