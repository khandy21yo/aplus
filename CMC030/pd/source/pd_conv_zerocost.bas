1	%TITLE "Put a 0.00 cost on items without any cost"
	%SBTTL "PD_CONV_ZEROCOST"
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
	!
	! Compile:
	!
	!	$ BAS PD_SOURCE:PD_CONV_ZEROCOST/LINE
	!	$ LINK/EXECUTABLE=PD_EXE: PD_CONV_ZEROCOST,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PD_CONV_ZEROCOST.OBJ;*
	!
	! Author:
	!
	!	07/24/92 - Kevin Handy
	!
	! Modification history:
	!
	!	08/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/14/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/10/99 - Kevin Handy
	!		Lose FNUOM$ (Dead Code)
	!		Lose InitializeFiles (Dead Code)
	!
	!	12/22/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

10	ON ERROR GOTO 19000

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD	PD_PRODUCT

	%INCLUDE "SOURCE:[PC.OPEN]PC_COST.HB"
	MAP (PC_COST)	PC_COST_CDD	PC_COST

 !	DIM OLDUNIT$(40%), NEWUNIT$(40%)

	EXTERNAL STRING FUNCTION DATE_TODAY

	%PAGE

	!*******************************************************************
	! Initilize Convert
	!*******************************************************************

	CALL READ_INITIALIZE

 !	READ UNITCOUNT%
 !	READ OLDUNIT$(I%), NEWUNIT$(I%) FOR I% = 1% TO UNITCOUNT%
 !
 !	DATA	20, &
 !		"BAG",		"BG", &
 !		"BARRELL",	"BR", &
 !		"BOX",		"BX", &
 !		"CADDIE",	"EA", &
 !		"CAN",		"CN", &
 !		" CAN",		"CN", &
 !		"CASE",		"CA", &
 !		"CASH",		"EA", &
 !		"EACH",		"EA", &
 !		"FOLL",		"RL", &
 !		"FOOT",		"FT", &
 !		"GALLON",	"GA", &
 !		"JAR",		"JR", &
 !		"OUNCE",	"OZ", &
 !		"PACKAGE",	"PK", &
 !		"PIECE",	"PC", &
 !		"POUND",	"LB", &
 !		"ROLL",		"RL", &
 !		"STICK",	"EA", &
 !		"UNIT",		"UN", &
 !		""
 !
 !	DEF FNUOM$(X$)
 !		Y$ = LEFT(X$, 2%)
 !		Y$ = NEWUNIT$(IZ%) IF X$ = OLDUNIT$(IZ%) FOR IZ% = 1% TO UNITCOUNT%
 !		FNUOM$ = Y$
 !	FNEND

	!
	! Create the data display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(20%, 80%, DISPLAY_ID%,,,)
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	SMG_STATUS% = SMG$ERASE_DISPLAY(DISPLAY_ID%)
	!
	! Paste the data display
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(DISPLAY_ID%, PASTE_ID%, &
		1%, 1%)
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%
	SMG_STATUS% = SMG$SET_CURSOR_MODE(PASTE_ID%, SMG$M_CURSOR_OFF)

	!
	! PRINT BANNER
	!
	SMG_STATUS% = SMG$PUT_CHARS_WIDE(DISPLAY_ID%, "Convert King B files", &
		2%, 15%, SMG$M_BOLD)
	SMG_STATUS% = SMG$DRAW_LINE(DISPLAY_ID%, 4%, 1%, 4%, 80%, &
		SMG$M_BOLD)

	!
	! Ask user if they realy would like to convert all files
	!
 ConfirmConv:

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "Convert files :", 9%, 20%)
	CONF$ = EDIT$(ENTR_3YESNO(SCOPE, DISPLAY_ID%, &
		"9;36", "Confirm Converting", "N", 16%, "'", "N"), -1%)

	!
	! Check for special keys typed
	!
	SELECT SCOPE::SCOPE_EXIT
	!
	! Control/C, Uparrow,
	!
	CASE 3%, SMG$K_TRM_UP, SMG$K_TRM_DOWN
		GOTO ConfirmConv	! (Ignored)

	!
	! Exit key typed
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	!
	! Good Keys
	!
	CASE 0%, 10%, 12%, SMG$K_TRM_DO, SMG$K_TRM_CR, &
		SMG$K_TRM_SELECT

	!
	! Bad Keys
	!
	CASE ELSE
		GOTO ConfirmConv

	END SELECT

	SMG_STATUS% = SMG$DELETE_CHARS(DISPLAY_ID%, 30%, 9%, 20%)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	GOTO ExitProgram IF CONF$ <> "Y"

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "File   :", 18%, 4%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "Record :", 19%, 4%)

	! Tell user the file we are converting is Product
	!
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "PRODUCT ", 18%, 15%)

	!
	! Open the needed files
	!
1405	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.opn"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

1407	%INCLUDE "SOURCE:[PC.OPEN]PC_COST.MOD"

1410	RESET #PD_PRODUCT.CH%

 GetNextCaseRec:
1420	!
	! Start the actual conversion
	!
	WHEN ERROR IN
		GET #PD_PRODUCT.CH%
	USE
		CONTINUE 15000 IF ERR = 11%
		FILENAME$ = "PRODUCT_INPUT"
		CONTINUE HelpError
	END WHEN

1430	!
	! Build conversion record - basic field
	!

1440	!
	! Create cost record
	!
	WHEN ERROR IN
		GET #PC_COST.CH%, KEY #0% EQ PD_PRODUCT::PRODUCT_NUM
	USE
		CONTINUE 1445
	END WHEN

	GOTO 1450

1445	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, &
		PD_PRODUCT::PRODUCT_NUM, 18%, 24%)

	PC_COST::PRODUCT	= PD_PRODUCT::PRODUCT_NUM
	PC_COST::LOCATION	= "100"
	PC_COST::EFFDATE	= DATE_TODAY
	PC_COST::COST		= 0.0

	PUT #PC_COST.CH%

1450	!
	! Delete mistake
	!
 !	get #pc_cost.ch%, key #0% eq &
 !		PC_COST::PRODUCT + "0100" + PC_COST::EFFDATE
 !	delete #pc_cost.ch%

1490	GOTO 1420

 ! InitializeFiles:
 !
 !	TYPEFLAG$ = ""
 !	PRODUCT$ = ""
 !	LOCATION$ = ""
 !	ONHAND = 0.0
 !	MINLEVEL = 0.0
 !	MAXLEVEL = 0.0
 !
 !	PC_COST::PRODUCT	= ""
 !	PC_COST::LOCATION	= "100"
 !	PC_COST::EFFDATE	= "19920101"
 !	PC_COST::COST		= 0.0
 !
 !	RETURN


 ExitProgram:
15000	!*******************************************************************
	! Exit program
	!*******************************************************************

	IF CONF$ = "Y"
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Conversion Process Complete", 0%)
	ELSE
		CALL ENTR_3MESSAGE(SCOPE, "Aborting Conversion Process", 0%)
	END IF

	CALL ENTR_3MESSAGE(SCOPE, "", 1%)
	SMG_STATUS% = SMG$SET_CURSOR_MODE(PASTE_ID%, SMG$M_CURSOR_ON)
	SMG_STATUS% = SMG$DELETE_PASTEBOARD(PASTE_ID%)
	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(DISPLAY_ID%)

	GOTO 32767

	%PAGE

 HelpError:
	!*******************************************************************
	! Help Message for an Error
	!*******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	CONF$ = "N"
	GOTO ExitProgram

19000	!*******************************************************************
	! Error trapping
	!*******************************************************************

	FILENAME$ = ""
	RESUME HelpError

32767	END
