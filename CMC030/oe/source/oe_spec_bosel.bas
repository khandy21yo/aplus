1	%TITLE "Load Selected Backorders"
	%SBTTL "OE_SPEC_BOSEL"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
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
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Load Selected Backorders\* option selects
	!	backordered items for which packing lists or shipping documents will be
	!	printed. Selections may be made by selecting specific customers, specific
	!	products or a combination of the two. Unwanted orders or line items within
	!	an order may be deleted prior to printing the packing lists by utilizing
	!	the maintenance routine.
	!	.b
	!	Selecting backorders with this option is a more time effective method of
	!	making the selections. However, if the user wished to select specific orders,
	!	the Shipping Journal option could be utilized. In either event, a shipping
	!	journal is created with this process.
	!	.b
	!	When the shipping data is recorded, the user may wish to edit the same batch
	!	number that was created with the Load Selected Backorders or Shipping Journal
	!	options, otherwise it would be necessary to delete the batch.
	!	.lm -5
	!
	! Index:
	!	.x Load Backorders
	!	.x Backorders>Load
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_SPEC_BOSEL/LINE
	!	$ LINK/EXECUTABLE=OE_EXE:*.EXE OE_SPEC_BOSEL, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_SPEC_BOSEL.OBJ;*
	!
	! Author:
	!
	!	07/16/91 - Val James Allen
	!
	! Modification history:
	!
	!	07/01/92 - Kevin Handy
	!		Clean up (check)
	!
	!	07/02/92 - Frank F. Starman
	!		Rewrite program
	!
	!	08/12/92 - Frank F. Starman
	!		Added option select by order.
	!
	!	08/20/92 - Kevin Handy
	!		Clean up (check)
	!
	!	09/21/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	10/12/92 - Dan Perkins
	!		Added MISCACCT field to OE_INVJOUR record.
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	02/02/93 - Frank F. Starman
	!		Load to OE_ORDERJOUR and OE_ORDERLINE
	!
	!	02/26/93 - Kevin Handy
	!		Clead up (Check)
	!
	!	04/08/93 - Kevin Handy
	!		Clead up (Check)
	!
	!	04/21/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/21/93 - Frank F. Starman
	!		Assign notes to the third line of notes
	!		Assign time as TIME now.
	!
	!	06/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	07/06/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	07/24/93 - Frank F. Starman
	!		Added PC_READ_COST function.
	!
	!	05/06/94 - Kevin Handy
	!		Reformat to 80 columns.
	!
	!	05/17/94 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/17/94 - Kevin Handy
	!		Added code for ::MISCH2.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!		Fix last parameter to ENTR_3CHOICE.
	!
	!	12/08/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		Change layout for OE_ORDERLINE::NOTES, and add
	!		OE_ORDERLINE::SUBACCT.
	!
	!	01/27/96 - Kevin Handy
	!		Change STRING$(...,ASCII(" ")) to SPACE$(...) in
	!		several places.
	!
	!	09/06/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	09/08/97 - Kevin Handy
	!		Change 'Qty' change option so that it doesn't
	!		get stuck on the qty.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/16/98 - Kevin Handy
	!		Blank out deposit number field
	!
	!	10/02/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Add a lot of error handling code
	!
	!	04/19/2006 - Kevin Handy
	!		Base cost on date, instead of using current date.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:OE_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PS_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MO_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_SCROLL.HB"
	DECLARE SMG_SCROLL_CDD SMG_SCROLL

	!
	! Define maps
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.HB"
	MAP (OE_ORDERJOUR)	OE_ORDERJOUR_CDD	OE_ORDERJOUR

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	MAP (OE_REGHEADER)	OE_REGHEADER_CDD	OE_REGHEADER

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	MAP (OE_REGLINE)	OE_REGLINE_CDD		OE_REGLINE
	DECLARE			OE_REGLINE_CDD		OE_REGLINE_READ

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERLINE.HB"
	MAP (OE_ORDERLINE)	OE_ORDERLINE_CDD	OE_ORDERLINE

	%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINE.HB"
	MAP (MO_REGLINE)	MO_REGLINE_CDD		MO_REGLINE
	DECLARE			MO_REGLINE_CDD		MO_REGLINE_READ

	%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINE.HB"
	MAP (MO_ORDERLINE)	MO_ORDERLINE_CDD	MO_ORDERLINE

	%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINEOPT.HB"
	MAP (MO_REGLINEOPT)	MO_REGLINEOPT_CDD	MO_REGLINEOPT
	DECLARE			MO_REGLINEOPT_CDD	MO_REGLINEOPT_READ

	%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINEOPT.HB"
	MAP (MO_ORDERLINEOPT)	MO_ORDERLINEOPT_CDD	MO_ORDERLINEOPT

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.HB"
	MAP (UTL_PROFILE)	UTL_PROFILE_CDD		UTL_PROFILE

	%INCLUDE "SOURCE:[PS.OPEN]PS_CASHREG.HB"
	MAP (PS_CASHREG)	PS_CASHREG_CDD		PS_CASHREG

	DIM SELECT_LIST$(1000%)

	COM (ORDERLIN) &
		ORDERNUM$, &
		LINENUM$

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION DSPL_SCROLL

	EXTERNAL LONG FUNCTION AR_MAIN_35CUSTOM
	EXTERNAL LONG FUNCTION PD_MAIN_PRODUCT
	EXTERNAL LONG FUNCTION MAINT_GROUP
	EXTERNAL LONG FUNCTION IC_WRIT_35BALANCE
	EXTERNAL LONG FUNCTION IC_READ_35BALANCE
	EXTERNAL LONG FUNCTION IC_DSPL_35BALANCE
	EXTERNAL LONG FUNCTION OE_READ_REGLINE
	EXTERNAL LONG FUNCTION MO_READ_REGLINE
	EXTERNAL LONG FUNCTION MO_READ_REGLINEOPT
	EXTERNAL REAL FUNCTION PC_READ_COST

	COM (CH_OE_REGLINE)	OE_REGLINE.CH%
	COM (CH_OE_REGHEADER)	OE_REGHEADER.CH%
	COM (CH_MO_REGLINE)	MO_REGLINE.CH%
	COM (CH_MO_REGLINEOPT)	MO_REGLINEOPT.CH%

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

	!
	! Look up device
	!
	CALL  READ_DEVICE("OE_ORDERJOUR", OE_ORDERJOUR.DEV$, STAT%)

100	!
	! Query user for year of file
	!
	CALL FIND_FILE(OE_ORDERJOUR.DEV$ + "OE_ORDERJOUR_*_%%.JRL", &
		OE_ORDERJOUR_FILE$(), &
		16%, "", "")

	OE_ORDERJOUR_FILE% = VAL%(OE_ORDERJOUR_FILE$(0%))

	IF OE_ORDERJOUR_FILE%
	THEN
		OE_ORDERJOUR_FILE$(LOOP%) = &
			RIGHT(OE_ORDERJOUR_FILE$(LOOP%), 14%) &
				FOR LOOP% = 1% TO OE_ORDERJOUR_FILE%

		TEMP$ = "Fill Orders Batch"

		X% = ENTR_3CHOICE(SCOPE, "", "", OE_ORDERJOUR_FILE$(), "", &
			0%, TEMP$, "", 0%)

		IF X% > 0%
		THEN
			BATCH_NO$ = EDIT$(OE_ORDERJOUR_FILE$(X%), -1%)
			GOTO 200

		END IF
	END IF

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	!
	! Ask for year
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		20%, &
		80%, &
		SMG_SCREEN_DATA% &
	)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		1%, &
		1% &
	)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Register Number:", 10%, 30%)

	!
	! Assign default register number
	!
	REG_NO$ = "????"

110	!
	! Set up the help message
	!
	SCOPE::PRG_ITEM = "FLD01REGNUM"

	!++
	! Abstract:FLD01REGNUM
	!--

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 10%, 48%, REG_NO$, -1%, 16%)

	CASE SMG$K_TRM_F14
		IF MAIN_WINDOW(PS_MAIN_CASHREG.ID, "VX") = 1%
		THEN
			REG_NO$ = PS_CASHREG::CASHREG
			END IF
		GOTO 310

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		OPTION$ = "EXIT"
		GOTO ExitProgram

	END SELECT

	REG_NO$ = EDIT$(REG_NO$, -1%)

	GOTO 110 IF INSTR(1%, REG_NO$, "?")

	V% = MAIN_WINDOW(PS_MAIN_CASHREG.ID, "M0" + REG_NO$)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Batch number:", &
		11%, 30%)

120	!
	! Set up the help message
	!
	SCOPE::PRG_ITEM = "FLD02BATCH"

	!++
	! Abstract:FLD02BATCH
	!--

	!
	! Assign default batch number
	!
	BATCH_NUM$ = "01"

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 11%, 48%, &
		BATCH_NUM$, -1%, 16%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		OPTION$ = "EXIT"
		GOTO ExitProgram

	CASE SMG$K_TRM_UP
		GOTO 110

	END SELECT

	BATCH_NUM$ = EDIT$(BATCH_NUM$, -1%)

	IF LEN(TRM$(BATCH_NUM$)) <> 2%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter the batch number in XX format", 0%)
		GOTO 320
	END IF

	BATCH_NO$ = REG_NO$ + "_" + BATCH_NUM$

	V% = MAIN_WINDOW(PS_MAIN_CASHINOUT.ID, "M")

200	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

	!***************************************************************
	! Open all of the files
	!***************************************************************

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	!
	! Open register header file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.CRE"
	USE
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

	!
	! Open register line file
	!
320	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.CRE"
	USE
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

	!
	! Open product file
	!
330	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	!Open the Invoice header file
	!
340	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.CRE"
	USE
		FILENAME$ = "OE_ORDERJOUR"
		CONTINUE HelpError
	END WHEN

	!
	!Open the invoice line file
	!
350	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERLINE.CRE"
	USE
		FILENAME$ = "OE_ORDERLINE"
		CONTINUE HelpError
	END WHEN

360	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.OPN"
		GET #UTL_PROFILE.CH%, RECORD 1%, REGARDLESS
		CLOSE #UTL_PROFILE.CH%
		CALL ASSG_FREECHANNEL(UTL_PROFILE.CH%)
	USE
		FILENAME$ = "UTL_PROFILE"
		CONTINUE HelpError
	END WHEN

	!
	! Open MO Regline file
	!
370	WHEN ERROR IN
		%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINE.CRE"
	USE
		FILENAME$ = "MO_REGLINE"
		CONTINUE HelpError
	END WHEN


	!
	! Open MO ReglineOpt file
	!
375	WHEN ERROR IN
		%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINEOPT.CRE"
	USE
		FILENAME$ = "MO_REGLINEOPT"
		CONTINUE HelpError
	END WHEN

	!
	! Open the MO Orderline file
	!
380	WHEN ERROR IN
		%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINE.CRE"
	USE
		FILENAME$ = "MO_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	!
	!Open the MO OrderlineOpt file
	!
385	WHEN ERROR IN
		%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINEOPT.CRE"
	USE
		FILENAME$ = "MO_ORDERLINEOPT"
		CONTINUE HelpError
	END WHEN

	!*******************************************************************
	! Select which orders/lines are to be printed
	!*******************************************************************

	!
	! Create the data display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%,			! 20 Rows &
		78%,			! 80 Columns &
		SMG_SCREEN_DATA%,	! Identifier &
		SMG$M_BORDER		! Put a border around it &
	)

	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	SMG_STATUS% = SMG$LABEL_BORDER(SMG_SCREEN_DATA%, &
		"Fill Orders Batch No " + BATCH_NO$)

	!
	! Paste the data display
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%,	! Data pasteboard &
		SCOPE::SMG_PBID,		! Pasetboard &
		2%,			! Row to start in &
		2%,			! Column to start in &
					! Don't need top-disp &
	)

	!
	! Define scrolling region
	!
	SMG_SCROLL::WINDOW	= SMG_SCREEN_DATA%
	SMG_SCROLL::TOP_ARRAY	= 1%
	SMG_SCROLL::BOT_ARRAY	= 1%
	SMG_SCROLL::SCROLL_TOP	= 2%
	SMG_SCROLL::SCROLL_BOT	= 17%
	SMG_SCROLL::BEG_ELEMENT	= 1%
	SMG_SCROLL::END_ELEMENT	= 1%
	SMG_SCROLL::TOP_LINE	= 1%
	SMG_SCROLL::CUR_LINE	= 1%
	SMG_SCROLL::CUR_W_ROW	= 1%
	SMG_SCROLL::CUR_W_COL	= 1%
	SMG_SCROLL::FIND_LINE	= 1%
	SMG_SCROLL::SMG_FLAG	= 0%
	SMG_SCROLL::PROMPT	= "->"
	SMG_SCROLL::VIDEO_SET	= SMG$M_REVERSE
	SMG_SCROLL::VIDEO_COMP	= 0%
	SMG_SCROLL::CHARSET	= 0%
	SMG_SCROLL::DRAW_COLS	= ""

	TOTAL_SELECTED% = 0%
	SMG_SCROLL::CUR_LINE = 1%

	!
	! Initial paint on screen
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_SCREEN_DATA%)

	V% = DSPL_SCROLL(SMG_SCROLL, SELECT_LIST$(), 0%, "PAINT")

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"  Product#       ReqDate     Qty  DocNumber  " + &
		"Line Customer#  CustomerName          ", &
		1%, 1%,, SMG$M_REVERSE)

410	SCOPE::PRG_ITEM = ""
	OPTLIST$ = "Prod_s Cust_s Doc_s Qty_change Sort Remove Load " + &
		"clEar Help eXit"
	GOSUB StatusLine

	OPT$ = ENTR_3OPTION(SCOPE, "COMMAND", OPTLIST$, OPT%, 0%)

	SELECT SCOPE::SCOPE_EXIT

	CASE SMG$K_TRM_CTRLC
		GOTO 410

	!
	! Next Screen, Downarrow, etc.
	!
	CASE SMG$K_TRM_DOWN, SMG$K_TRM_NEXT_SCREEN, SMG$K_TRM_F19, &
		SMG$K_TRM_UP, SMG$K_TRM_PREV_SCREEN, SMG$K_TRM_F18 &

		V% = DSPL_SCROLL(SMG_SCROLL, SELECT_LIST$(), &
			SCOPE::SCOPE_EXIT, "")
		GOTO 410

	!
	! Exit
	!
	CASE SMG$K_TRM_F8, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	!
	! view
	!
	CASE SMG$K_TRM_F14

		IF MID(SELECT_LIST$(SMG_SCROLL::CUR_LINE), &
			85%, 1%) = "M"
		THEN
			ORDERNUM$ = CONV_STRING(MID(SELECT_LIST$( &
				SMG_SCROLL::CUR_LINE), 33%, 10%), &
				CMC$_RIGHT)
			LINENUM$  = MID(SELECT_LIST$( &
				SMG_SCROLL::CUR_LINE), 44%, 4%)
			V% = MAIN_WINDOW(MO_MAIN_REGLINEOPT.ID, &
				"V0" + ORDER$ + LINE$)
		ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		END IF
		GOTO 410

	!
	! Good keys
	!
	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	!
	! Bad Keys
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 410

	END SELECT

	SELECT OPT$

	CASE "X"
		GOTO ExitProgram

	!
	! Remove one entry from the list
	!
	CASE "R"

	!++
	! Abstract:REMOVE
	!	^*Remove\*
	!	.b
	!	.lm +5
	!	The ^*Remove\* function
	!	removes one line entry
	!	from the select window. After the remove
	!	function has been accessed, place the arrow beside the line to
	!	be removed and press ^*_<Do_>\*.
	!	.lm -5
	!
	! Index:
	!	.x Remove>Function
	!
	!--
		SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, &
			SPACE$(80%), 1%, 1%)

420		TEMP$ = "Position the arrow on the line to remove " + &
			"- Then press <DO> "

		SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, TEMP$, 1%, 1%)

		JUNK$ = ""

		SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, 1%, &
			LEN(TEMP$), JUNK$, -1%, 4096%)

		CASE SMG$K_TRM_CTRLC
			GOTO 410

		CASE SMG$K_TRM_DOWN, SMG$K_TRM_NEXT_SCREEN, &
			SMG$K_TRM_F19, &
			SMG$K_TRM_UP, SMG$K_TRM_PREV_SCREEN, &
			SMG$K_TRM_F18 &

			V% = DSPL_SCROLL(SMG_SCROLL, &
				SELECT_LIST$(), SCOPE::SCOPE_EXIT, "")
			GOSUB StatusLine
			GOTO 420

		!
		! Exit
		!
		CASE SMG$K_TRM_F8, SMG$K_TRM_F10, &
			SMG$K_TRM_CTRLZ

			GOTO 410

		!
		! Good keys
		!
		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		!
		! Bad Keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO 420

		END SELECT

		!
		! Remove from the list
		!
		SELECT_LIST$(LOOP%) = SELECT_LIST$(LOOP% + 1%) &
			FOR LOOP% = SMG_SCROLL::CUR_LINE TO &
			SMG_SCROLL::BOT_ARRAY - 1%

		SELECT_LIST$(TOTAL_SELECTED%) = " "

		!
		! Repaint screen
		!
		V% = DSPL_SCROLL(SMG_SCROLL, SELECT_LIST$(), 0%, "PAINT")

		!
		! subtract from counter
		!
		TOTAL_SELECTED% = TOTAL_SELECTED% - 1%

		SMG_SCROLL::BOT_ARRAY,	SMG_SCROLL::END_ELEMENT = &
			TOTAL_SELECTED%

	CASE "Q"

	!++
	! Abstract:QTYCHANGE
	!	^*Quantity Change\*
	!	.b
	!	.lm +5
	!	The ^*Remove\* function
	!	removes one line entry
	!	from the select window. After the remove
	!	function has been accessed, place the arrow beside the line to
	!	be removed and press ^*_<Do_>\*.
	!	.lm -5
	!
	! Index:
	!	.x Remove>Function
	!
	!--
		QTY = VAL(MID(SELECT_LIST$(SMG_SCROLL::CUR_LINE), 25%, 6%))
 QtyEnter:
		QTY = ENTR_3NUMBER(SCOPE, SMG_SCREEN_DATA%, "", &
			"New Quantity ", QTY, 0%, "######", "")

		SELECT SCOPE::SCOPE_EXIT

		!
		! Exit
		!
		CASE SMG$K_TRM_CTRLC, SMG$K_TRM_F8, &
			SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

			GOTO 410

		CASE SMG$K_TRM_DOWN, SMG$K_TRM_NEXT_SCREEN, &
			SMG$K_TRM_F19, &
			SMG$K_TRM_UP, SMG$K_TRM_PREV_SCREEN, &
			SMG$K_TRM_F18 &

		!
		! Good keys
		!
		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO
			IF SMG_SCROLL::END_ELEMENT <> &
				SMG_SCROLL::CUR_LINE
			THEN
				SCOPE::SCOPE_EXIT = SMG$K_TRM_DOWN
			ELSE
				SCOPE::SCOPE_EXIT = SMG$K_TRM_F19
			END IF

		!
		! Bad Keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)

			GOTO QtyEnter

		END SELECT

		!
		! Change on the list
		!
		SELECT_LIST$(SMG_SCROLL::CUR_LINE) = &
			LEFT(SELECT_LIST$(SMG_SCROLL::CUR_LINE), 24%) + &
			FORMAT$(QTY, "######") + &
			RIGHT(SELECT_LIST$(SMG_SCROLL::CUR_LINE), 31%)

		!
		! Repaint screen
		!
		V% = DSPL_SCROLL(SMG_SCROLL, SELECT_LIST$(), &
			SCOPE::SCOPE_EXIT, "")

		QTY = VAL(MID(SELECT_LIST$(SMG_SCROLL::CUR_LINE), 25%, 6%))
		GOSUB StatusLine
 !		GOTO QtyEnter

	CASE "P"

	!++
	! Abstract:PRODUCT_SELECT
	!	^*Product__select\*
	!	.b
	!	.lm +5
	!	The ^*Product__select\* function
	!	selects all orders which contain
	!	backorders of the entered product and schedule them for
	!	the load. Any order/line that is not to be released should
	!	be removed prior to the load function.
	!	..lm -5
	!
	! Index:
	!	.x Product_Select>Function
	!
	!--
		PRODUCT$ = SPACE$(LEN(PD_PRODUCT::PRODUCT_NUM))

440		PRODUCT$ = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"", "Product to select ", &
			PRODUCT$, 0%, "'E", "")

		SELECT SCOPE::SCOPE_EXIT

		!
		! Exit
		!
		CASE SMG$K_TRM_CTRLC, SMG$K_TRM_F8, &
			SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

			GOTO 410

		CASE SMG$K_TRM_F17

			V% = IC_DSPL_35BALANCE(PRODUCT$, &
				UTL_PROFILE::DEFLOCATION, &
				AVAILABLE, "14;10", 0%)

			GOTO 440

		!
		! List choices
		!
		CASE SMG$K_TRM_F14
			IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX") = 1%
			THEN
				PRODUCT$ = PD_PRODUCT::PRODUCT_NUM
			END IF
			GOTO 440

		!
		! Good keys
		!
		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		!
		! Bad Keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO 440

		END SELECT

		GOTO 410 IF EDIT$(PRODUCT$, -1%) = ""

		!
		! Start look up in Order file
		!
450		WHEN ERROR IN
			FIND #OE_REGLINE.CH%, KEY #1% EQ PRODUCT$, REGARDLESS
		USE
			CONTINUE 410 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "OE_REGLINE"
			CONTINUE HelpError
		END WHEN

		TOTORD = 0.0
		TOTSHP = 0.0
		TOTCAN = 0.0
		ORDER$ = ""
		LINE$ = ""

		!
		! Get next record
		!
455		WHEN ERROR IN
			GET #OE_REGLINE.CH%, REGARDLESS
		USE
			CONTINUE 471 IF ERR = 11%
			FILENAME$ = "OE_REGLINE"
			CONTINUE HelpError
		END WHEN

		GOTO EndProduct IF OE_REGLINE::PRODUCT <> PRODUCT$

		!
		! Check to see if same order and line
		!
		IF OE_REGLINE::ORDNUM + OE_REGLINE::LIN <> ORDER$ + LINE$
		THEN
			LINETYPE$ = "O"

			GOSUB PlugProduct

			ORDER$ = OE_REGLINE::ORDNUM
			LINE$  = OE_REGLINE::LIN
			TOTORD = 0.0
			TOTSHP = 0.0
			TOTCAN = 0.0
		END IF

		SELECT OE_REGLINE::TRANTYPE

		CASE "01"
			TOTORD = TOTORD + OE_REGLINE::QTY
			REQ_DATE$ = OE_REGLINE::TDATE

		CASE "02"
			TOTSHP = TOTSHP + OE_REGLINE::QTY

		CASE "03"
			TOTCAN = TOTCAN + OE_REGLINE::QTY
		END SELECT

		GOTO 455

 PlugProduct:
		GOTO RetProduct IF ORDER$ = "" OR LINE$ = "" OR &
			FUNC_ROUND(TOTORD - TOTSHP - TOTCAN, 2%) <= 0.0

		FOR LOOP% = 1% TO TOTAL_SELECTED%

			IF MID(SELECT_LIST$(LOOP%), 33%, 15%) = &
				CONV_STRING(ORDER$, CMC$_LEFT) + &
				" " + LINE$ &
				AND MID(SELECT_LIST$(LOOP%), 85%, 1%) = &
				LINETYPE$
			THEN
				GOTO RetProduct
			END IF

		NEXT LOOP%

		TOTAL_SELECTED% = TOTAL_SELECTED% + 1%

		SMG_SCROLL::FIND_LINE, SMG_SCROLL::BOT_ARRAY, &
			SMG_SCROLL::END_ELEMENT, &
			SMG_SCROLL::CUR_LINE = TOTAL_SELECTED%

459		GET #OE_REGHEADER.CH%, KEY #0% EQ ORDER$, REGARDLESS

		!
		! Get billing client number
		!
460		WHEN ERROR IN
			GET #AR_35CUSTOM.CH%, &
				KEY #0% EQ OE_REGHEADER::CUSNUM, REGARDLESS
		USE
			AR_35CUSTOM::CUSNAM = STRING$(LEN(AR_35CUSTOM::CUSNAM), 63%)

			CONTINUE 470 IF ERR = 155%
			FILENAME$ = "AR_35CUSTOM"
			CONTINUE HelpError
		END WHEN

		!
		! Set selected item into an array
		!
470		SELECT_LIST$(TOTAL_SELECTED%) = &
			PRODUCT$ + " "  + &
			PRNT_DATE(REQ_DATE$, 6%) + " " + &
			FORMAT$((TOTORD - TOTSHP - TOTCAN), "######") + "  " + &
			CONV_STRING(ORDER$, CMC$_LEFT) + " "  + &
			LINE$ + " "  + &
			OE_REGHEADER::CUSNUM + " "  + &
			LEFT$(AR_35CUSTOM::CUSNAM, 20%) + " "  + &
			OE_REGHEADER::LOCATION + &
			LINETYPE$

		IF TOTAL_SELECTED% > SMG_SCROLL::SCROLL_BOT
		THEN
			V% = DSPL_SCROLL(SMG_SCROLL, SELECT_LIST$(), &
				SMG$K_TRM_DOWN, "")
		ELSE
			V% = DSPL_SCROLL(SMG_SCROLL, SELECT_LIST$(), &
				0%, "PAINT")
		END IF

		SORT_FLAG% = 0%

 RetProduct:
		TOTORD = 0.0
		TOTSHP = 0.0
		TOTCAN = 0.0

		RETURN
471
 EndProduct:
		GOSUB PlugProduct
		GOTO 410

	CASE "C"
	!++
	! Abstract:CUSTOMER_SELECT
	!	^*Customer__select\*
	!	.b
	!	.lm +5
	!	The ^*Customer__select\* function
	!	selects a customer number to be
	!	loaded if there are any backorders for this customer.
	!	All backordered lines will be selected.
	!	Any lines that are not to be loaded should be removed.
	!	.lm -5
	!
	! Index:
	!	.x Customer select>Function
	!
	!--

		CLIENT_NUM$ = SPACE$(LEN(AR_35CUSTOM::CUSNUM))

480		CLIENT_NUM$ = ENTR_3STRING(SCOPE, &
			SMG_SCREEN_DATA%, "", &
			"Customer# to select ", &
			CLIENT_NUM$, 0%, "'E", "")

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_CTRLC
			GOTO 410

		!
		! Exit
		!
		CASE SMG$K_TRM_F8, SMG$K_TRM_F10, &
			SMG$K_TRM_CTRLZ

			GOTO 410

		CASE SMG$K_TRM_F14
			IF MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, "VX") = 1%
			THEN
				CLIENT_NUM$ = AR_35CUSTOM::CUSNUM
				GOTO 480
			END IF

		!
		! Good keys
		!
		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		!
		! Bad Keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO 480

		END SELECT

		GOTO 410 IF EDIT$(CLIENT_NUM$, -1%) = ""

482		WHEN ERROR IN
			FIND #OE_REGHEADER.CH%, &
				KEY #3% GE CLIENT_NUM$, REGARDLESS
		USE
			CONTINUE 410 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "OE_REGHEADER"
			CONTINUE HelpError
		END WHEN

483		WHEN ERROR IN
			GET #OE_REGHEADER.CH%, REGARDLESS
		USE
			CONTINUE 410 IF ERR = 11%
			FILENAME$ = "OE_REGHEADER"
			CONTINUE HelpError
		END WHEN

		GOTO 410 IF OE_REGHEADER::CUSNUM <> CLIENT_NUM$
		ORDER$ = OE_REGHEADER::ORDNUM

		!
		! Start look up in Order file
		!
484		WHEN ERROR IN
			FIND #OE_REGLINE.CH%, &
				KEY #0% GE OE_REGHEADER::ORDNUM, REGARDLESS
		USE
			CONTINUE 486 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "OE_REGLINE"
			CONTINUE HelpError
		END WHEN

		TOTORD = 0.0
		TOTSHP = 0.0
		TOTCAN = 0.0
		LINE$ = ""

		!
		! Get next record
		!
485		WHEN ERROR IN
			GET #OE_REGLINE.CH%, REGARDLESS
		USE
			CONTINUE 486 IF ERR = 11%
			FILENAME$ = "OE_REGLINE"
			CONTINUE HelpError
		END WHEN

		IF OE_REGLINE::ORDNUM <> OE_REGHEADER::ORDNUM
		THEN
			LINETYPE$ = "O"

			GOSUB PlugProduct1
			GOTO 486
		END IF

		!
		! Check to see if same order and line
		!
		IF OE_REGLINE::LIN <> LINE$
		THEN
			LINETYPE$ = "O"

			GOSUB PlugProduct1

			LINE$    = OE_REGLINE::LIN
			PRODUCT$ = OE_REGLINE::PRODUCT
			TOTORD   = 0.0
			TOTSHP   = 0.0
			TOTCAN   = 0.0
		END IF

		SELECT OE_REGLINE::TRANTYPE

		CASE "01"
			TOTORD = TOTORD + OE_REGLINE::QTY
			REQ_DATE$ = OE_REGLINE::TDATE

		CASE "02"
			TOTSHP = TOTSHP + OE_REGLINE::QTY

		CASE "03"
			TOTCAN = TOTCAN + OE_REGLINE::QTY
		END SELECT

		GOTO 485

		!
		! Start look up in Order file
		!
486		GOSUB PlugProduct1
		WHEN ERROR IN
			FIND #MO_REGLINE.CH%, &
				KEY #0% GE OE_REGHEADER::ORDNUM, REGARDLESS
		USE
			CONTINUE 492 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "OE_REGLINE"
			CONTINUE HelpError
		END WHEN

		TOTORD = 0.0
		TOTSHP = 0.0
		TOTCAN = 0.0
		LINE$  = ""

		!
		! Get next record
		!
487		WHEN ERROR IN
			GET #MO_REGLINE.CH%, REGARDLESS
		USE
			CONTINUE 492 IF ERR = 11%
			FILENAME$ = "OE_REGLINE"
			CONTINUE HelpError
		END WHEN

		IF MO_REGLINE::ORDNUM <> OE_REGHEADER::ORDNUM
		THEN
			LINETYPE$ = "M"

			GOSUB PlugProduct1
			GOTO EndProduct1
		END IF

		!
		! Check to see if same order and line
		!
		IF MO_REGLINE::LIN <> LINE$
		THEN
			LINETYPE$ = "M"

			GOSUB PlugProduct1

			LINE$    = MO_REGLINE::LIN
			PRODUCT$ = MO_REGLINE::MAKE + MO_REGLINE::MODELCODE
			TOTORD   = 0.0
			TOTSHP   = 0.0
			TOTCAN   = 0.0
		END IF

		SELECT MO_REGLINE::TRANTYPE

		CASE "01"
			TOTORD = TOTORD + MO_REGLINE::QTY
			REQ_DATE$ = MO_REGLINE::TDATE

		CASE "02"
			TOTSHP = TOTSHP + MO_REGLINE::QTY

		CASE "03"
			TOTCAN = TOTCAN + MO_REGLINE::QTY
		END SELECT

		GOTO 487

 PlugProduct1:
		RETURN IF LINE$ = ""
		RETURN IF FUNC_ROUND(TOTORD - TOTSHP - TOTCAN, 2%) <= 0.0

		FOR LOOP% = 1% TO TOTAL_SELECTED%

			IF MID(SELECT_LIST$(LOOP%), 33%, 15%) = &
				CONV_STRING(ORDER$, CMC$_LEFT) + " " + LINE$ &
				AND MID(SELECT_LIST$(LOOP%), 85%, 1%) = &
				LINETYPE$
			THEN
				RETURN
			END IF

		NEXT LOOP%

		TOTAL_SELECTED% = TOTAL_SELECTED% + 1%

		SMG_SCROLL::FIND_LINE, SMG_SCROLL::BOT_ARRAY, &
			SMG_SCROLL::END_ELEMENT, &
			SMG_SCROLL::CUR_LINE = TOTAL_SELECTED%

		!
		! Get billing client number
		!
490		WHEN ERROR IN
			GET #AR_35CUSTOM.CH%, &
				KEY #0% EQ OE_REGHEADER::CUSNUM, REGARDLESS
		USE
			AR_35CUSTOM::CUSNAM = STRING$(LEN(AR_35CUSTOM::CUSNAM), 63%)

			CONTINUE 491 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "AR_35CUSTOM"
			CONTINUE HelpError
		END WHEN

		!
		! Set selected item into an array
		!
491		SELECT_LIST$(TOTAL_SELECTED%) = &
			PRODUCT$ + " "  + &
			PRNT_DATE(REQ_DATE$, 6%) + " "  + &
			FORMAT$((TOTORD - TOTSHP - TOTCAN), "######") + "  " + &
			CONV_STRING(ORDER$, CMC$_LEFT) + " "  + &
			LINE$ + " "  + &
			OE_REGHEADER::CUSNUM + " "  + &
			LEFT$(AR_35CUSTOM::CUSNAM, 20%) + " "  + &
			OE_REGHEADER::LOCATION + &
			LINETYPE$

		IF TOTAL_SELECTED% > SMG_SCROLL::SCROLL_BOT
		THEN
			V% = DSPL_SCROLL(SMG_SCROLL, SELECT_LIST$(), &
				SMG$K_TRM_DOWN, "")
		ELSE
			V% = DSPL_SCROLL(SMG_SCROLL, SELECT_LIST$(), &
				0%, "PAINT")
		END IF

		SORT_FLAG% = 0%

		RETURN

492
 EndProduct1:
		GOSUB PlugProduct1
		GOTO 483

	CASE "D"
	!++
	! Abstract:DOCUMENT_SELECT
	!	^*Document__select\*
	!	.b
	!	.lm +5
	!	The ^*Document__select\* function
	!	selects a document number to be
	!	loaded if there are any open orders for this document.
	!	All open lines will be selected.
	!	Any lines that are not to be loaded should be removed.
	!	.lm -5
	!
	! Index:
	!	.x Document select>Function
	!
	!--

		DOC_NUM$ = SPACE$(LEN(OE_REGHEADER::ORDNUM))

580		DOC_NUM$ = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, "", &
			"Document# to select ", &
			DOC_NUM$, 2%, "~R 'E", "")

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_CTRLC
			GOTO 410

		!
		! Exit
		!
		CASE SMG$K_TRM_F8, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

			GOTO 410

		CASE SMG$K_TRM_F14
			IF MAIN_WINDOW(OE_MAIN_REGHEADER.ID, "VX") = 1%
			THEN
				DOC_NUM$ = OE_REGHEADER::ORDNUM
				GOTO 580
			END IF

		!
		! Good keys
		!
		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		!
		! Bad Keys
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO 580

		END SELECT

		GOTO 410 IF EDIT$(DOC_NUM$, -1%) = ""

582		WHEN ERROR IN
			FIND #OE_REGHEADER.CH%, KEY #0% EQ DOC_NUM$, REGARDLESS
		USE
			CONTINUE 410 IF ERR = 11% OR ERR = 155% OR ERR = 9%
			FILENAME$ = "OE_REGHEADER"
			CONTINUE HelpError
		END WHEN

583		WHEN ERROR IN
			GET #OE_REGHEADER.CH%, REGARDLESS
		USE
			FILENAME$ = "OE_REGHEADER"
			CONTINUE HelpError
		END WHEN

		ORDER$ = OE_REGHEADER::ORDNUM
		CLIENT_NUM$ = OE_REGHEADER::CUSNUM

		!
		! Start look up in Order file
		!
584		WHEN ERROR IN
			FIND #OE_REGLINE.CH%, &
				KEY #0% GE OE_REGHEADER::ORDNUM, REGARDLESS
		USE
			CONTINUE 586 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "OE_REGLINE"
			CONTINUE HelpError
		END WHEN

		TOTORD = 0.0
		TOTSHP = 0.0
		TOTCAN = 0.0
		LINE$  = ""

		!
		! Get next record
		!
585		WHEN ERROR IN
			GET #OE_REGLINE.CH%, REGARDLESS
		USE
			CONTINUE 586 IF ERR = 11%
			FILENAME$ = "OE_REGLINE"
			CONTINUE HelpError
		END WHEN

		IF OE_REGLINE::ORDNUM <> OE_REGHEADER::ORDNUM
		THEN
			LINETYPE$ = "O"

			GOSUB PlugProduct2
			GOTO 586
		END IF

		!
		! Check to see if same order and line
		!
		IF OE_REGLINE::LIN <> LINE$
		THEN
			LINETYPE$ = "O"

			GOSUB PlugProduct2

			LINE$    = OE_REGLINE::LIN
			PRODUCT$ = OE_REGLINE::PRODUCT
			TOTORD   = 0.0
			TOTSHP   = 0.0
			TOTCAN   = 0.0
		END IF

		SELECT OE_REGLINE::TRANTYPE

		CASE "01"
			TOTORD = TOTORD + OE_REGLINE::QTY
			REQ_DATE$ = OE_REGLINE::TDATE

		CASE "02"
			TOTSHP = TOTSHP + OE_REGLINE::QTY

		CASE "03"
			TOTCAN = TOTCAN + OE_REGLINE::QTY
		END SELECT

		GOTO 585

		!
		! Start look up in Order file
		!
586		GOSUB PlugProduct2

		WHEN ERROR IN
			FIND #MO_REGLINE.CH%, &
				KEY #0% GE OE_REGHEADER::ORDNUM, REGARDLESS
		USE
			CONTINUE 592 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "MO_REGLINE"
			CONTINUE HelpError
		END WHEN

		TOTORD = 0.0
		TOTSHP = 0.0
		TOTCAN = 0.0
		LINE$  = ""

		!
		! Get next record
		!
587		WHEN ERROR IN
			GET #MO_REGLINE.CH%, REGARDLESS
		USE
			CONTINUE 592 IF ERR = 11%
			FILENAME$ = "MO_REGLINE"
			CONTINUE HelpError
		END WHEN

		IF MO_REGLINE::ORDNUM <> OE_REGHEADER::ORDNUM
		THEN
			LINETYPE$ = "M"

			GOSUB PlugProduct2
			GOTO 592
		END IF

		!
		! Check to see if same order and line
		!
		IF MO_REGLINE::LIN <> LINE$
		THEN
			LINETYPE$ = "M"

			GOSUB PlugProduct2

			LINE$    = MO_REGLINE::LIN
			PRODUCT$ = MO_REGLINE::MAKE + MO_REGLINE::MODELCODE
			TOTORD   = 0.0
			TOTSHP   = 0.0
			TOTCAN   = 0.0
		END IF

		SELECT MO_REGLINE::TRANTYPE

		CASE "01"
			TOTORD = TOTORD + MO_REGLINE::QTY
			REQ_DATE$ = MO_REGLINE::TDATE

		CASE "02"
			TOTSHP = TOTSHP + MO_REGLINE::QTY

		CASE "03"
			TOTCAN = TOTCAN + MO_REGLINE::QTY
		END SELECT

		GOTO 587

 PlugProduct2:
		RETURN IF LINE$ = ""
		RETURN IF FUNC_ROUND(TOTORD - TOTSHP - TOTCAN, 2%) <= 0.0

		FOR LOOP% = 1% TO TOTAL_SELECTED%

			IF MID(SELECT_LIST$(LOOP%), 33%, 15%) = &
				CONV_STRING(ORDER$, CMC$_LEFT) + &
				" " + LINE$ &
				AND MID(SELECT_LIST$(LOOP%), 85%, 1%) = &
				LINETYPE$
			THEN
				RETURN
			END IF

		NEXT LOOP%

		TOTAL_SELECTED% = TOTAL_SELECTED% + 1%

		SMG_SCROLL::FIND_LINE, SMG_SCROLL::BOT_ARRAY, &
			SMG_SCROLL::END_ELEMENT, &
			SMG_SCROLL::CUR_LINE = TOTAL_SELECTED%

		!
		! Get billing client number
		!
590		AR_35CUSTOM::CUSNAM = &
			STRING$(LEN(AR_35CUSTOM::CUSNAM), 63%)

		WHEN ERROR IN
			GET #AR_35CUSTOM.CH%, &
				KEY #0% EQ OE_REGHEADER::CUSNUM, REGARDLESS
		USE
			CONTINUE 591 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "AR_35CUSTOM"
			CONTINUE HelpError
		END WHEN

		!
		! Set selected item into an array
		!
591		SELECT_LIST$(TOTAL_SELECTED%) = &
			PRODUCT$ + " "  + &
			PRNT_DATE(REQ_DATE$, 6%) + " "  + &
			FORMAT$((TOTORD - TOTSHP - TOTCAN), "######") + "  " + &
			CONV_STRING(ORDER$, CMC$_LEFT) + " "  + &
			LINE$ + " "  + &
			OE_REGHEADER::CUSNUM + " "  + &
			LEFT$(AR_35CUSTOM::CUSNAM, 20%) + " "  + &
			OE_REGHEADER::LOCATION + &
			LINETYPE$

		IF TOTAL_SELECTED% > SMG_SCROLL::SCROLL_BOT
		THEN
			V% = DSPL_SCROLL(SMG_SCROLL, SELECT_LIST$(), &
				SMG$K_TRM_DOWN, "")
		ELSE
			V% = DSPL_SCROLL(SMG_SCROLL, SELECT_LIST$(), &
				0%, "PAINT")
		END IF

		SORT_FLAG% = 0%

		RETURN

592		GOSUB PlugProduct2
		GOTO 410

	CASE "T"
	!++
	! Abstract:PARTIAL
	!	^*Partial\*
	!	.b
	!	.lm +5
	!	The ^*Partial\* shipment function
	!	selects the documents to be
	!	loaded if there are any previously shipped item open orders for this document.
	!	All open lines will be selected.
	!	Any lines that are not to be loaded should be removed.
	!	.lm -5
	!
	! Index:
	!	.x Partial select>Function
	!
	!--

682		WHEN ERROR IN
			RESET #OE_REGHEADER.CH%
		USE
			CONTINUE 410 IF ERR = 11% OR ERR = 155% OR ERR = 9%
			FILENAME$ = "OE_REGHEADER"
			CONTINUE HelpError
		END WHEN

683		WHEN ERROR IN
			GET #OE_REGHEADER.CH%, REGARDLESS
		USE
			FILENAME$ = "OE_REGHEADER"
			CONTINUE HelpError
		END WHEN

		GOTO 683 IF OE_REGHEADER::ASTATUS = "C"

		ORDER$      = OE_REGHEADER::ORDNUM
		CLIENT_NUM$ = OE_REGHEADER::CUSNUM
		PICK_FLAG%  = 0%
		SHIP_FLAG%  = 0%

		!
		! Start look up in Order file
		!
684		WHEN ERROR IN
			FIND #OE_REGLINE.CH%, &
				KEY #0% GE OE_REGHEADER::ORDNUM, REGARDLESS
		USE
			CONTINUE 683 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "OE_REGLINE"
			CONTINUE HelpError
		END WHEN

		TOTORD = 0.0
		TOTSHP = 0.0
		TOTCAN = 0.0
		LINE$  = ""

		!
		! Get next record
		!
685		WHEN ERROR IN
			GET #OE_REGLINE.CH%, REGARDLESS
		USE
			CONTINUE 692 IF ERR = 11%
			FILENAME$ = "OE_REGLINE"
			CONTINUE HelpError
		END WHEN

		IF OE_REGLINE::ORDNUM <> OE_REGHEADER::ORDNUM
		THEN
			LINETYPE$ = "O"

			GOSUB PlugProduct3
			GOTO 692
		END IF

		!
		! Check to see if same order and line
		!
		IF OE_REGLINE::LIN <> LINE$
		THEN
			LINETYPE$ = "O"

			GOSUB PlugProduct3
			GOTO 684 IF PICK_FLAG% = 1%

			LINE$    = OE_REGLINE::LIN
			PRODUCT$ = OE_REGLINE::PRODUCT
			TOTORD   = 0.0
			TOTSHP   = 0.0
			TOTCAN   = 0.0
		END IF

		SELECT OE_REGLINE::TRANTYPE

		CASE "01"
			TOTORD = TOTORD + OE_REGLINE::QTY
			REQ_DATE$ = OE_REGLINE::TDATE

		CASE "02"
			TOTSHP = TOTSHP + OE_REGLINE::QTY

		CASE "03"
			TOTCAN = TOTCAN + OE_REGLINE::QTY
		END SELECT

		GOTO 685

 PlugProduct3:
		RETURN IF LINE$ = ""

		SHIP_FLAG% = SHIP_FLAG% + 1%  IF TOTSHP <> 0.0

		IF FUNC_ROUND(TOTORD - TOTSHP - TOTCAN, 2%) <= 0.0
		THEN
			RETURN
		END IF

		RETURN IF SHIP_FLAG% <= 1%

		PICK_FLAG% = PICK_FLAG% + 1%

		RETURN IF PICK_FLAG% <= 1%

		FOR LOOP% = 1% TO TOTAL_SELECTED%

			IF MID(SELECT_LIST$(LOOP%), 33%, 15%) = &
				CONV_STRING(ORDER$,CMC$_LEFT) + &
				" " + LINE$ &
				AND MID(SELECT_LIST$(LOOP%), 85%, 1%) = &
				LINETYPE$
			THEN
				RETURN
			END IF

		NEXT LOOP%

		TOTAL_SELECTED% = TOTAL_SELECTED% + 1%

		SMG_SCROLL::FIND_LINE, SMG_SCROLL::BOT_ARRAY, &
			SMG_SCROLL::END_ELEMENT, &
			SMG_SCROLL::CUR_LINE = TOTAL_SELECTED%

		!
		! Get billing client number
		!
690		WHEN ERROR IN
			GET #AR_35CUSTOM.CH%, &
				KEY #0% EQ OE_REGHEADER::CUSNUM, REGARDLESS
		USE
			AR_35CUSTOM::CUSNAM = STRING$(LEN(AR_35CUSTOM::CUSNAM), 63%)

			CONTINUE 691 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "AR_35CUSTOM"
			CONTINUE HelpError
		END WHEN

		!
		! Set selected item into an array
		!
691		SELECT_LIST$(TOTAL_SELECTED%) = &
			PRODUCT$ + " "  + &
			PRNT_DATE(REQ_DATE$, 6%) + " "  + &
			FORMAT$((TOTORD - TOTSHP - TOTCAN), "######") + "  " + &
			CONV_STRING(ORDER$, CMC$_LEFT) + " "  + &
			LINE$ + " "  + &
			OE_REGHEADER::CUSNUM + " "  + &
			LEFT$(AR_35CUSTOM::CUSNAM, 20%) + " "  + &
			OE_REGHEADER::LOCATION + &
			LINETYPE$

		IF TOTAL_SELECTED% > SMG_SCROLL::SCROLL_BOT
		THEN
			V% = DSPL_SCROLL(SMG_SCROLL, SELECT_LIST$(), &
				SMG$K_TRM_DOWN, "")
		ELSE
			V% = DSPL_SCROLL(SMG_SCROLL, SELECT_LIST$(), &
				0%, "PAINT")
		END IF

		SORT_FLAG% = 0%

		RETURN

692		GOSUB PlugProduct3
		GOTO 684 IF PICK_FLAG% = 1%
		GOTO 410

	CASE "E"
	!++
	! Abstract:CLEAR
	!	^*Clear\*
	!	.b
	!	.lm +5
	!	The ^*Clear\* function
	!	clears all matters that
	!	have been selected to be billed. After this command has been
	!	completed the billing select screen will display an empty screen.
	!	.lm -5
	!
	! Index:
	!	.x Clear>Function
	!
	!--
		JUNK$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", &
			"Confirm Clear then press <Do> ", &
			"N", 0%, "'E", "")

		IF EDIT$(JUNK$, -1%) = "Y"
		THEN
			SELECT_LIST$(LOOP%) = "" &
				FOR LOOP% = 1% TO TOTAL_SELECTED%

			IF TOTAL_SELECTED% > 0%
			THEN
				V% = DSPL_SCROLL(SMG_SCROLL, &
					SELECT_LIST$(), &
					SCOPE::SCOPE_EXIT, "PAINT")
			END IF

			SMG_SCROLL::BOT_ARRAY, &
				SMG_SCROLL::END_ELEMENT = 0%
			SMG_SCROLL::CUR_LINE = 1%

			TOTAL_SELECTED% = 0%
		END IF

	CASE "H"
		CALL HELP_3MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, &
			SCOPE::PRG_PROGRAM, "HELP")

	CASE "L"
	!++
	! Abstract:LOAD
	!	^*Load\*
	!	.b
	!	.lm +5
	!	The ^*Load\* function
	!	creates shipping journal records
	!	based on the backorder items selected.
	!	.lm -5
	!
	! Index:
	!
	!--
		IF TOTAL_SELECTED%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"Loading Records in the Journal", &
				16% + 1%)
			GOSUB 700
			GOTO ExitProgram
		ELSE
			CALL ENTR_3MESSAGE(SCOPE, &
				"No Records Selected to print", 0%)
		END IF

	CASE "S"
	!++
	! Abstract:SORT
	!	^*Sort\*
	!	.b
	!	.lm +5
	!	The ^*Sort\* function sorts selected
	!	lines by product and requested date.
	!	.lm -5
	!
	! Index:
	!	.x Sort>Function
	!
	!--
		CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)

		!
		! Sort array
		!
		FOR LOOP.A% = 1% TO TOTAL_SELECTED%

			FOR LOOP.B% = LOOP.A% + 1% TO TOTAL_SELECTED%

				IF SELECT_LIST$(LOOP.B%) < SELECT_LIST$(LOOP.A%)
				THEN
					TEMP$ = SELECT_LIST$(LOOP.A%)
					SELECT_LIST$(LOOP.A%) = &
						SELECT_LIST$(LOOP.B%)
					SELECT_LIST$(LOOP.B%) = TEMP$
				END IF

			NEXT LOOP.B%

		NEXT LOOP.A%

		V% = DSPL_SCROLL(SMG_SCROLL, SELECT_LIST$(), &
			SCOPE::SCOPE_EXIT, "PAINT")
		CALL ENTR_3MESSAGE(SCOPE, "", 1%)
		SORT_FLAG% = -1%

	END SELECT

	GOTO 410

700	!**********************************************************************
	!* Here's where we do our thing and load the suckers up
	!**********************************************************************

	FOR LOOP% = 1% TO TOTAL_SELECTED%

		ORDER$     = CONV_STRING(MID(SELECT_LIST$(LOOP%), 33%, 10%), &
			CMC$_RIGHT)
		LINE$      = MID(SELECT_LIST$(LOOP%), 44%, 4%)
		SHPQTY = FUNC_ROUND(VAL(MID(SELECT_LIST$(LOOP%), 25%, 6%)), 2%)
		LINETYPE$  = MID(SELECT_LIST$(LOOP%), 85%, 1%)

710		SELECT LINETYPE$

		CASE "O"
			WHEN ERROR IN
				GET #OE_ORDERLINE.CH%, &
					KEY #0% EQ ORDER$ + LINE$, REGARDLESS
			USE
				CONTINUE 720 IF ERR = 155%
				FILENAME$ = "OE_ORDERLINE"
				CONTINUE HelpError
			END WHEN

		CASE "M"
			WHEN ERROR IN
				GET #MO_ORDERLINE.CH%, &
					KEY #0% EQ ORDER$ + LINE$, REGARDLESS
			USE
				CONTINUE 720 IF ERR = 155%
				FILENAME$ = "OE_ORDERLINE"
				CONTINUE HelpError
			END WHEN

		END SELECT

		GOTO LoopDeLoop

720		WHEN ERROR IN
			GET #OE_ORDERJOUR.CH%, KEY #0% EQ ORDER$, REGARDLESS
		USE
			CONTINUE 730 IF ERR = 155%
			FILENAME$ = "OE_ORDERJOUR"
			CONTINUE HelpError
		END WHEN

		GOTO BuildLines

730		WHEN ERROR IN
			GET #OE_REGHEADER.CH%, KEY #0% EQ ORDER$, REGARDLESS
		USE
			OE_REGHEADER::SHIPVIA = ""
			OE_REGHEADER::TERMS   = ""

			CONTINUE 740 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "OE_REGHEADER"
			CONTINUE HelpError
		END WHEN

740		OE_ORDERJOUR::ORDNUM    = ORDER$
		OE_ORDERJOUR::ORDDATE   = OE_REGHEADER::ORDDATE
		OE_ORDERJOUR::ORDTYPE   = OE_REGHEADER::ORDTYPE
		OE_ORDERJOUR::ORDCAT    = OE_REGHEADER::ORDCAT
		OE_ORDERJOUR::CUSNUM    = OE_REGHEADER::CUSNUM
		OE_ORDERJOUR::SHIPDATE  = DATE_TODAY
		OE_ORDERJOUR::SHIPNAM   = OE_REGHEADER::SHIPNAM
		OE_ORDERJOUR::SHIPLIN   = OE_REGHEADER::SHIPLIN
		OE_ORDERJOUR::ADD1      = OE_REGHEADER::ADD1
		OE_ORDERJOUR::ADD2      = OE_REGHEADER::ADD2
		OE_ORDERJOUR::ADD3      = OE_REGHEADER::ADD3
		OE_ORDERJOUR::CITY      = OE_REGHEADER::CITY
		OE_ORDERJOUR::STATE     = OE_REGHEADER::STATE
		OE_ORDERJOUR::ZIP       = OE_REGHEADER::ZIP
		OE_ORDERJOUR::COUNTRY   = OE_REGHEADER::COUNTRY
		OE_ORDERJOUR::CUSTPO    = OE_REGHEADER::CUSTPO
		OE_ORDERJOUR::LOCATION  = OE_REGHEADER::LOCATION
		OE_ORDERJOUR::COMMAMT   = OE_REGHEADER::COMMAMT
		OE_ORDERJOUR::COMMPERC  = 0.0
		OE_ORDERJOUR::SALESMAN  = OE_REGHEADER::SALESMAN
		OE_ORDERJOUR::SALCOMM   = OE_REGHEADER::SALCOMM
		OE_ORDERJOUR::AMTPAID   = OE_REGHEADER::AMTPAID
		OE_ORDERJOUR::CHECK     = ""
		OE_ORDERJOUR::DEPOSIT   = ""
		OE_ORDERJOUR::TRANDATE  = DATE_TODAY
		OE_ORDERJOUR::TRANTIME  = TIME_NOW
		OE_ORDERJOUR::TAXCODE   = OE_REGHEADER::TAXCODE
		OE_ORDERJOUR::TAXFLAG   = OE_REGHEADER::TAXFLAG
		OE_ORDERJOUR::TERMS     = OE_REGHEADER::TERMS
		OE_ORDERJOUR::HANDLING  = 0.0
		OE_ORDERJOUR::MISC      = 0.0
		OE_ORDERJOUR::DISC      = 0.0
		OE_ORDERJOUR::MISCACCT  = ""
		OE_ORDERJOUR::CREASON   = ""
		OE_ORDERJOUR::FREIGHT   = 0.0
		OE_ORDERJOUR::SALESTAX  = 0.0
		OE_ORDERJOUR::OPERATOR  = ""
		OE_ORDERJOUR::NOTES(0%) = OE_REGHEADER::NOTES(0%)
		OE_ORDERJOUR::NOTES(1%) = OE_REGHEADER::NOTES(1%)
		OE_ORDERJOUR::NOTES(2%) = OE_REGHEADER::NOTES(2%)
		OE_ORDERJOUR::INVNUM    = ""
		OE_ORDERJOUR::SHIPVIA   = OE_REGHEADER::SHIPVIA
		OE_ORDERJOUR::PAYMNT    = 1%
		OE_ORDERJOUR::REG_FLAG  = "Y"

		PUT #OE_ORDERJOUR.CH%

 BuildLines:
		SELECT LINETYPE$

		CASE "O"
800			WHEN ERROR IN
				FIND #OE_REGLINE.CH%, &
					KEY #0% EQ ORDER$ + LINE$, REGARDLESS
				GET #OE_REGLINE.CH%, REGARDLESS
			USE
				CONTINUE 990 IF ERR = 155% OR ERR = 11%
				FILENAME$ = "OE_REGLINE"
				CONTINUE HelpError
			END WHEN

			GOTO LoopDeLoop IF OE_REGLINE::ORDNUM + &
				OE_REGLINE::LIN <> &
				ORDER$ + LINE$

			V% = OE_READ_REGLINE(ORDER$, LINE$, "EQ", &
				OE_REGLINE_READ, QTY())

			!
			! Calculate remaining on-order for new or current line
			!
			ALLOCATE_QTY = SHPQTY
			ALLOCATE_QTY = QTY(0%) IF QTY(0%) < SHPQTY
			BACK_QTY     = FUNC_ROUND(ALLOCATE_QTY - SHPQTY, 3%)
			BACK_QTY     = 0.0 IF BACK_QTY < 0.0

			OE_ORDERLINE::ORDNUM	= ORDER$
			OE_ORDERLINE::LIN	= LINE$
			OE_ORDERLINE::PRODUCT	= OE_REGLINE_READ::PRODUCT
			OE_ORDERLINE::ORDQTY	= ALLOCATE_QTY
			OE_ORDERLINE::SHPQTY	= SHPQTY
			OE_ORDERLINE::BCKQTY	= BACK_QTY
			OE_ORDERLINE::PRICE	= OE_REGLINE_READ::PRICE
			OE_ORDERLINE::DISCOUNT	= OE_REGLINE_READ::DISCOUNT
			OE_ORDERLINE::REQDATE	= OE_REGLINE_READ::TDATE
			OE_ORDERLINE::COST	= PC_READ_COST( &
				OE_REGLINE_READ::PRODUCT, &
				OE_ORDERJOUR::LOCATION, &
				DATE_TODAY, "")
 !				OE_ORDERLINE::REQDATE, "")
			OE_ORDERLINE::PROMO	= OE_REGLINE_READ::PROMO
			OE_ORDERLINE::MISCH	= OE_REGLINE_READ::MISCH
			OE_ORDERLINE::MISCH2	= OE_REGLINE_READ::MISCH2
			OE_ORDERLINE::NOTES1 = OE_REGLINE_READ::NOTES1
			OE_ORDERLINE::NOTES2 = OE_REGLINE_READ::NOTES2
			OE_ORDERLINE::SUBACCT	= OE_REGLINE_READ::SUBACCT

			PUT #OE_ORDERLINE.CH%

			V% = IC_WRIT_35BALANCE (OE_ORDERLINE::PRODUCT, &
				OE_ORDERJOUR::LOCATION, "SO", &
				ALLOCATE_QTY)

			V% = IC_WRIT_35BALANCE (OE_ORDERLINE::PRODUCT, &
				OE_ORDERJOUR::LOCATION, "SA", &
				-OE_ORDERLINE::SHPQTY)

		CASE "M"
910			WHEN ERROR IN
				FIND #MO_REGLINE.CH%, &
					KEY #0% EQ ORDER$ + LINE$, REGARDLESS
				GET #MO_REGLINE.CH%, REGARDLESS
			USE
				CONTINUE 990 IF ERR = 155% OR ERR = 11%
				FILENAME$ = "MO_REGLINE"
				CONTINUE HelpError
			END WHEN

			GOTO LoopDeLoop &
				IF MO_REGLINE::ORDNUM + MO_REGLINE::LIN <> &
				ORDER$ + LINE$

			V% = MO_READ_REGLINE(ORDER$, LINE$, "EQ", &
				MO_REGLINE_READ, QTY())

			!
			! Calculate remaining on-order for new or current line
			!
			ALLOCATE_QTY = SHPQTY
			ALLOCATE_QTY = QTY(0%) IF QTY(0%) < SHPQTY
			BACK_QTY     = FUNC_ROUND(ALLOCATE_QTY - SHPQTY, 3%)
			BACK_QTY     = 0.0 IF BACK_QTY < 0.0

			MO_ORDERLINE::ORDNUM	= ORDER$
			MO_ORDERLINE::LIN	= LINE$
			MO_ORDERLINE::PRODUCT	= MO_REGLINE_READ::PRODUCT
			MO_ORDERLINE::ORDQTY	= ALLOCATE_QTY
			MO_ORDERLINE::SHPQTY	= SHPQTY
			MO_ORDERLINE::BCKQTY	= BACK_QTY
			MO_ORDERLINE::PRICE	= MO_REGLINE_READ::PRICE
			MO_ORDERLINE::DISCOUNT	= MO_REGLINE_READ::DISCOUNT
			MO_ORDERLINE::REQDATE	= MO_REGLINE_READ::TDATE
			MO_ORDERLINE::COST	= PC_READ_COST( &
				MO_REGLINE_READ::IDNUM, &
				OE_ORDERJOUR::LOCATION, &
				DATE_TODAY, "")
 !				MO_ORDERLINE::REQDATE, "")
			MO_ORDERLINE::NOTES(0%) = MO_REGLINE_READ::NOTES(0%)
			MO_ORDERLINE::NOTES(1%) = MO_REGLINE_READ::NOTES(1%)
			MO_ORDERLINE::MAKE	= MO_REGLINE_READ::MAKE
			MO_ORDERLINE::YEAR	= MO_REGLINE_READ::YEAR
			MO_ORDERLINE::MTYPE	= MO_REGLINE_READ::MTYPE
			MO_ORDERLINE::MSIZE	= MO_REGLINE_READ::MSIZE
			MO_ORDERLINE::MODELCODE	= MO_REGLINE_READ::MODELCODE
			MO_ORDERLINE::IDNUM	= MO_REGLINE_READ::IDNUM

			PUT #MO_ORDERLINE.CH%

			V% = IC_WRIT_35BALANCE (MO_ORDERLINE::PRODUCT, &
				OE_ORDERJOUR::LOCATION, "SO", &
				ALLOCATE_QTY)

			V% = IC_WRIT_35BALANCE (MO_ORDERLINE::PRODUCT, &
				OE_ORDERJOUR::LOCATION, "SA", &
				-MO_ORDERLINE::SHPQTY)

			LIN$ = "    "

 MORegLineOpt:
			IF MO_READ_REGLINEOPT(OE_ORDERJOUR::ORDNUM, &
				MO_ORDERLINE::LIN, LIN$, "GT", &
				MO_REGLINEOPT_READ, QTY()) = CMC$_NORMAL
			THEN
				!
				! Calculate remaining on-order for new or current line
				!
				MO_ORDERLINEOPT::ORDNUM	= MO_ORDERLINE::ORDNUM
				MO_ORDERLINEOPT::LIN	= MO_ORDERLINE::LIN
				MO_ORDERLINEOPT::MAKE	= MO_ORDERLINE::MAKE
				MO_ORDERLINEOPT::MODELCODE= &
					MO_ORDERLINE::MODELCODE
				MO_ORDERLINEOPT::LINOPT	= &
					MO_REGLINEOPT_READ::OPTLIN
				MO_ORDERLINEOPT::ORDQTY	= QTY(0%)
				MO_ORDERLINEOPT::SHPQTY	= QTY(0%)
				MO_ORDERLINEOPT::BCKQTY	= 0.0
				MO_ORDERLINEOPT::OPTN	= &
					MO_REGLINEOPT_READ::OPTN
				MO_ORDERLINEOPT::PRODUCT= &
					MO_REGLINEOPT_READ::PRODUCT
				MO_ORDERLINEOPT::PRICE    = &
					MO_REGLINEOPT_READ::PRICE
				MO_ORDERLINEOPT::COST     = &
					MO_REGLINEOPT_READ::COST
				MO_ORDERLINEOPT::OPTGROUP = &
					MO_REGLINEOPT_READ::OPTGROUP
				MO_ORDERLINEOPT::OPTDESCR = &
					MO_REGLINEOPT_READ::OPTDESCR

				LIN$ = MO_REGLINEOPT_READ::OPTLIN

				PUT #MO_ORDERLINEOPT.CH%

				V% = IC_WRIT_35BALANCE( &
					MO_ORDERLINEOPT::PRODUCT, &
					OE_ORDERJOUR::LOCATION, "SO", &
					ALLOCATE_QTY)

				V% = IC_WRIT_35BALANCE( &
					MO_ORDERLINEOPT::PRODUCT, &
					OE_ORDERJOUR::LOCATION, "SA", &
					-MO_ORDERLINEOPT::SHPQTY)

				GOTO MORegLineOpt
			END IF

		END SELECT
 LoopDeLoop:
990	NEXT LOOP%

	RETURN

 ExitProgram:
4010	!******************************************************************
	! Exit the program
	!******************************************************************

	!
	! Exit to next program or menu
	!
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 StatusLine:
	IF TOTAL_SELECTED%
	THEN
		PRODUCT$  = LEFT(SELECT_LIST$(SMG_SCROLL::CUR_LINE), 14%)
		LOCATION$ = MID(SELECT_LIST$(SMG_SCROLL::CUR_LINE), 81%, 4%)

		V% = IC_READ_35BALANCE(PRODUCT$, LOCATION$, XBALANCE(,))

		ONHAND = XBALANCE(1%, 1%) + XBALANCE(1%, 2%) + XBALANCE(1%, 3%)
	ELSE
		PRODUCT$  = SPACE$(LEN(PD_PRODUCT::PRODUCT_NUM))
		LOCATION$ = SPACE$(LEN(OE_REGHEADER::LOCATION))
		ONHAND    = 0.0
	END IF

	LINE_QTY = 0.0

	FOR LOOP% = 1% TO TOTAL_SELECTED%

		IF LEFT(SELECT_LIST$(LOOP%), 14%) = PRODUCT$ AND &
			MID(SELECT_LIST$(LOOP%), 81%, 4%) = LOCATION$
		THEN
			LINE_QTY = LINE_QTY + &
				VAL(MID(SELECT_LIST$(LOOP%), 25%, 6%))
		ELSE
			GOTO PrintLine IF SORT_FLAG% = -1% AND &
				LEFT(SELECT_LIST$(LOOP%), 14%) > PRODUCT$
		END IF

	NEXT LOOP%

 PrintLine:
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"  " + PRODUCT$ + "          " + FORMAT$(LINE_QTY, "######") + &
		"     QtyOnhand " + FORMAT$(ONHAND, "######") + &
		"       Location  " + LOCATION$ + "            ", &
		18%, 1%,, SMG$M_REVERSE)

	RETURN

19000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	!
	! Untrapped error
	!
	RESUME 19990

 HelpError:
19990	!
	! This moved from inside error to outside so that errors occuring
	! at lower levels could be trapped.  Basic will not allow any
	! error to occur inside of an error no matter if it is in a
	! different module.
	!
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

	END

21000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	COM (ORDERLIN) &
		ORDERNUM$, &
		LINENUM$

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:OE_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MO_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PS_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	EXTERNAL LONG FUNCTION OE_MAIN_REGHEADER
	EXTERNAL LONG FUNCTION MO_MAIN_OPTION
	EXTERNAL LONG FUNCTION MO_MAIN_REGLINEOPT
	EXTERNAL LONG FUNCTION AR_MAIN_35CUSTOM
	EXTERNAL LONG FUNCTION PD_MAIN_PRODUCT
	EXTERNAL LONG FUNCTION PS_MAIN_CASHREG
	EXTERNAL LONG FUNCTION PS_MAIN_CASHINOUT
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE AR_MAIN_35CUSTOM.ID

		MAINT_GROUP = AR_MAIN_35CUSTOM(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_PRODUCT.ID

		MAINT_GROUP = PD_MAIN_PRODUCT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE OE_MAIN_REGHEADER.ID

		MAINT_GROUP = OE_MAIN_REGHEADER(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PS_MAIN_CASHREG.ID

		MAINT_GROUP = PS_MAIN_CASHREG(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PS_MAIN_CASHINOUT.ID

		MAINT_GROUP = PS_MAIN_CASHINOUT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_LOCATION.ID

		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE MO_MAIN_OPTION.ID

		MAINT_GROUP = MO_MAIN_OPTION(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE MO_MAIN_REGLINEOPT.ID

		SELECT MOPTION

		CASE OPT_SUBWIND

			SELECT MLOOP

			CASE 6%
				MVALUE = ORDERNUM$ + LINNUM$

			CASE ELSE
				MVALUE = ORDERNUM$ + LINNUM$

			END SELECT

		END SELECT

		MAINT_GROUP = MO_MAIN_REGLINEOPT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
