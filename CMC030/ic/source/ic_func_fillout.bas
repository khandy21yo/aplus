1	%TITLE "Set Inventory Adjustment Journal"
	%SBTTL "IC_FUNC_FILLOUT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG IC_FUNC_FILLOUT(STRING LOCATION, STRING CATEGORY)

	!
	! COPYRIGHT (C) 1997 BY
	!
	! Software Solutions, Inc.
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
	!	This function converts the product cycle count entry journal to
	!	the inventory adjustment journal (posting form of journal).
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_FUNC_FILLOUT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP IC_FUNC_FILLOUT
	!	$ DELETE IC_FUNC_FILLOUT.OBJ;*
	!
	! Author:
	!
	!	12/24/97 - Kevin Handy
	!
	! Modification history:
	!
	!	01/09/98 - Kevin Handy
	!		Fix so there aren't two line 1050's
	!
	!	04/22/1998 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/24/98 - Kevin Handy
	!		Added category field to calling parameters.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/20/98 - Kevin Handy
	!		Add new CONTROL field, change in keys.
	!
	!	10/23/98 - Kevin Handy
	!		Use .MOD for jourcount so don't have to create
	!		lines first.
	!		Also test product number for already having it
	!		in current record before trying to find it again.
	!
	!	10/23/98 - Kevin Handy
	!		Include inactive/obsolete products.
	!
	!	10/30/98 - Kevin Handy
	!		Open IC_35BALANCE as "MOD" instead of ".OPN" so
	!		that the adjustment journal will not get so
	!		confused.
	!
	!	10/30/98 - Kevin Handy
	!		Open "IC_JOURCOUNT" with "CRE" instead of "MOD"
	!		because it may have not yet been created.
	!
	!	09/08/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[IC.OPEN]IC_CYCLEJOUR.HB"
	MAP (IC_CYCLEJOUR)	IC_CYCLEJOUR_CDD	IC_CYCLEJOUR
	MAP (IC_CYCLEJOUR_INIT)	IC_CYCLEJOUR_CDD	IC_CYCLEJOUR_INIT

	%INCLUDE "SOURCE:[IC.OPEN]IC_JOURCOUNT.HB"
	MAP (IC_JOURCOUNT)	IC_JOURCOUNT_CDD	IC_JOURCOUNT

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP (IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSACCT.HB"
	DECLARE			UTL_TRANSACCT_CDD	UTL_TRANSACCT_READ

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.HB"
	MAP (IC_35BALANCE) IC_35BALANCE_CDD IC_35BALANCE

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_IC_CYCLEJOUR) &
		IC_CYCLEJOUR.CH%, &
		IC_CYCLEJOUR.READONLY%

	COM (CH_IC_JOURCOUNT) &
		IC_JOURCOUNT.CH%, &
		IC_JOURCOUNT.READONLY%

	COM (CH_IC_35BALANCE) &
		IC_35BALANCE.CH%, &
		IC_35BALANCE.READONLY%

	COM (BATCH_IC_CYCLEJOUR) &
		BATCH_NO$ = 2%


	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION PD_EXAM_PRODUCT

	%PAGE

	IC_FUNC_FILLOUT = 0%
	IC_CYCLEJOUR_INIT = IC_CYCLEJOUR

300	!
	! Open main file (existing) for modification
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_JOURCOUNT.CRE"
	USE
		FILENAME$ = "IC_JOURCOUNT"
		CONTINUE HelpError
	END WHEN

310	!
	! Open balance file
	!
	IF (IC_35BALANCE.CH% <= 0%)
	THEN
		%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.MOD"
	END IF

	!
	! Paint the background
	!
500	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		6%, &
		40%, &
		SMG_EXPAND%, &
		SMG$M_BORDER &
	)

	SMG_STATUS% = SMG$LABEL_BORDER &
	( &
		SMG_EXPAND%, &
		"Adjustment Batch No. " + BATCH_NO$ + &
		"    Location: " + LOCATION, &
		SMG$K_TOP &
	)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_EXPAND%, "Product # ", 2%, 5%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_EXPAND%, &
		SCOPE::SMG_PBID, &
		12%, &
		5% &
	)

	%PAGE

1000	!******************************************************************
	! Adjustment
	!******************************************************************
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	ADD_COUNT% = 0%

	WHEN ERROR IN
		FIND #IC_35BALANCE.CH%, KEY #1% EQ LOCATION
	USE
		CONTINUE 2000
	END WHEN

1020	!
	! Main loop starts here
	!
	WHEN ERROR IN
		GET #IC_35BALANCE.CH%
	USE
		CONTINUE 2000 IF ERR = 11%
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

	GOTO 2000 IF IC_35BALANCE::LOCATION <> LOCATION

1030	!
	! Do we already have one of these?
	!
	GOTO 1020 IF IC_JOURCOUNT::PRODUCT = IC_35BALANCE::PRODUCT

	WHEN ERROR IN
		GET #IC_JOURCOUNT.CH%, &
			KEY #0% EQ LOCATION + IC_35BALANCE::PRODUCT, &
			REGARDLESS
	USE
		CONTINUE 1050 IF ERR = 155%
		FILENAME$ = "IC_JOURCOUNT"
		CONTINUE HelpError
	END WHEN

	GOTO 1020

1050	!
	! Grab the product description, and see that the product
	! isn't expired or non-existant
	!
	GOTO 1020 &
		IF PD_EXAM_PRODUCT(IC_35BALANCE::PRODUCT, PD_PRODUCT_EXAM) <> &
		CMC$_NORMAL

 !	GOTO 1020 IF PD_PRODUCT_EXAM::SSTATUS <> "A"

	GOTO 1020 IF COMP_STRING(PD_PRODUCT_EXAM::CATEGORY, CATEGORY) = 0%

1060	!
	! We ain't got none, so we gots'ta add one
	!
	IF EDIT$(IC_35BALANCE::PRODUCT, -1%) <> ""
	THEN
		IC_JOURCOUNT::LOCATION = LOCATION
		IC_JOURCOUNT::PRODUCT = IC_35BALANCE::PRODUCT
		IC_JOURCOUNT::QUANTITY = 0.0
		IC_JOURCOUNT::CONTROL = ""

		ADD_COUNT% = ADD_COUNT% + 1%

		SMG_STATUS% = SMG$PUT_CHARS(SMG_EXPAND%, &
			IC_JOURCOUNT::PRODUCT, &
			2%, 16%)

		PUT #IC_JOURCOUNT.CH%
	END IF

	GOTO 1020

2000	CALL ENTR_3MESSAGE(SCOPE, &
		"Fill Out Completed, Added " + NUM1$(ADD_COUNT%), 1%)

	IC_FUNC_FILLOUT = 0%

	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_EXPAND%)

 ExitFunction:

	EXIT FUNCTION

	%Page

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	IC_FUNC_FILLOUT = 1%
	GOTO ExitFunction

32767	END FUNCTION
