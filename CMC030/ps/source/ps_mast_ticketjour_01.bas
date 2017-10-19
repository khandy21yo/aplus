1	%TITLE "Ticket Entry Journal"
	%SBTTL "PS_MAST_TICKETJOUR_01"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1996 BY
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
	! Software Solutions, Inc. assumes no responsibility for the
	! use or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Ticket Entry Journal\* option
	!	maintains orders being placed.  Each journal file is assigned a
	!	user batch number consisting of two (2) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Ticket Entry Journal
	!
	! Option:
	!
	!	PS_MAIN_TICKETJOUR$HELP
	!	PS_MAIN_TICKETLINE$HELP
	!
	! Compile:
	!
	!	$ BAS PS_SOURCE:PS_MAST_TICKETJOUR_01/LINE/NOOPT
	!	$ LINK/EXE=PS_EXE: PS_MAST_TICKETJOUR_01, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PS_MAST_TICKETJOUR_01.OBJ;*
	!
	! Author:
	!
	!	08/15/96 - Kevin Handy
	!		Copied from PS_MAST_TICKETJOUR and modified
	!		so that it doesn't have the serial number.
	!
	! Modification history:
	!
	!	10/25/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/26/96 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$ routines
	!
	!	06/30/99 - Kevin Handy
	!		Compile ?NOOPT to lose probles on Alpha
	!
	!	11/19/2000 - Kevin Handy
	!		Use WHWN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PS_WINDOW.INC"

	%INCLUDE "SOURCE:[PS.OPEN]PS_CASHREG.HB"
	MAP (PS_CASHREG)	PS_CASHREG_CDD		PS_CASHREG

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.HB"
	MAP (PC_PRICE)		PC_PRICE_CDD		PC_PRICE

	%INCLUDE "SOURCE:[PC.OPEN]PC_COST.HB"
	MAP (PC_COST)		PC_COST_CDD		PC_COST

	!
	! This common area must be mapped in both the main program and
	! in MAINT_GROUP.
	!
	COM (BATCH_NO) &
		BATCH_NO$ = 7%

	COM (CH_PD_PRODUCT) &
		PD_PRODUCT.CH%, &
		PD_PRODUCT.READONLY%

	COM (CH_PC_PRICE) &
		PC_PRICE.CH%, &
		PC_PRICE.READONLY%

	COM (CH_PC_COST) &
		PC_COST.CH%, &
		PC_COST.READONLY%

	%PAGE

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

200	!
	! Open up a bunch a files in read/write mode so they will not
	! be opened up in a read-only mode by some random function
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.MOD"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpFile
	END WHEN

	PD_PRODUCT.READONLY% = 0%

210	WHEN ERROR IN
		%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.MOD"
	USE
		FILENAME$ = "PC_PRICE"
		CONTINUE HelpFile
	END WHEN

	PC_PRICE.READONLY% = 0%

220	WHEN ERROR IN
		%INCLUDE "SOURCE:[PC.OPEN]PC_COST.MOD"
	USE
		FILENAME$ = "PC_COST"
		CONTINUE HelpFile
	END WHEN

	PC_COST.READONLY% = 0%

	!
	! Look up device
	!
	CALL READ_DEVICE("OE_ORDERJOUR", OE_ORDERJOUR.DEV$, STAT%)

300	!
	! Query user for year of file
	!
	CALL FIND_FILE(OE_ORDERJOUR.DEV$ + "OE_ORDERJOUR_*_%%.JRL", &
		OE_ORDERJOUR_FILE$(), &
		16%, "", "")

	OE_ORDERJOUR_FILE% = VAL%(OE_ORDERJOUR_FILE$(0%))

	IF OE_ORDERJOUR_FILE%
	THEN
		FOR LOOP% = 1% TO OE_ORDERJOUR_FILE%
			OE_ORDERJOUR_FILE$(LOOP%) = &
				RIGHT(OE_ORDERJOUR_FILE$(LOOP%), 14%)
		NEXT LOOP%

		TEMP$ = "Order-Invoice Journal Batch"

		X% = ENTR_3CHOICE(SCOPE, "", "", OE_ORDERJOUR_FILE$(), "", &
			0%, TEMP$, "", 0%)

		IF X% > 0%
		THEN
			BATCH_NO$ = EDIT$(OE_ORDERJOUR_FILE$(X%), -1%)
			GOTO 400
		END IF
	END IF

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	!
	! Ask for batch number
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

310	!
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

	GOTO 310 IF INSTR(1%, REG_NO$, "?")

	V% = MAIN_WINDOW(PS_MAIN_CASHREG.ID, "M0" + REG_NO$)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Batch number:", 11%, 30%)

320	!
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

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, &
		11%, 48%, BATCH_NUM$, -1%, 16%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		OPTION$ = "EXIT"
		GOTO ExitProgram

	CASE SMG$K_TRM_UP
		GOTO 310

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

400	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

1000	!******************************************************************
	! Handle the main file
	!******************************************************************

	!
	! Maintain file
	!
	V% = MAIN_WINDOW(PS_MAIN_TICKETJOUR.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

 HelpFile:
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	RESUME ExitProgram


19990	END


20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:PS_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:OE_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:SA_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:SB_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:IC_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.HB"
	MAP (OE_ORDERJOUR)	OE_ORDERJOUR_CDD	OE_ORDERJOUR
	MAP (OE_ORDERJOUR_OLD)	OE_ORDERJOUR_CDD	OE_ORDERJOUR_OLD
	MAP (OE_ORDERJOUR_ONE)	OE_ORDERJOUR_CDD	OE_ORDERJOUR_ONE

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERLINE.HB"
	MAP (OE_ORDERLINE)	OE_ORDERLINE_CDD	OE_ORDERLINE

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT
	MAP (PD_PRODUCT_ONE)	PD_PRODUCT_CDD		PD_PRODUCT_ONE

	COM (BATCH_NO) &
		BATCH_NO$ = 7%

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION PS_MAIN_TICKETJOUR
	EXTERNAL LONG FUNCTION PS_MAIN_TICKETLINE_01
	EXTERNAL LONG FUNCTION PS_MAIN_CASHREG
	EXTERNAL LONG FUNCTION PS_MAIN_CASHINOUT
	EXTERNAL LONG FUNCTION PS_OUTP_TICKET
	EXTERNAL LONG FUNCTION OE_MAIN_ORDERTYPE
	EXTERNAL LONG FUNCTION OE_MAIN_SALESTAX
	EXTERNAL LONG FUNCTION OE_MAIN_SHIPTO
	EXTERNAL LONG FUNCTION OE_MAIN_CATEGORY
	EXTERNAL LONG FUNCTION GL_MAIN_CHART
	EXTERNAL LONG FUNCTION IC_WRIT_PRODUCT
	EXTERNAL LONG FUNCTION PD_MAIN_PRODUCT
	EXTERNAL LONG FUNCTION AR_MAIN_35CUSTOM
	EXTERNAL LONG FUNCTION AR_MAIN_CUSTYPE
	EXTERNAL LONG FUNCTION UT_MAIN_CARRIER
	EXTERNAL LONG FUNCTION UT_MAIN_TERMS
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION
	EXTERNAL LONG FUNCTION UTL_MAIN_COUNTRY
	EXTERNAL LONG FUNCTION SA_MAIN_SALESMAN
	EXTERNAL LONG FUNCTION SB_MAIN_SUBACCOUNT
	EXTERNAL LONG FUNCTION OE_MAIN_CREASON
	EXTERNAL LONG FUNCTION OE_MAIN_REASONACCT
	EXTERNAL LONG FUNCTION OE_MAIN_REGHEADER
	EXTERNAL LONG FUNCTION OE_MAIN_REGLINE
	EXTERNAL LONG FUNCTION PD_MAIN_PRODTYPE
	EXTERNAL LONG FUNCTION PD_MAIN_CATEGORY
	EXTERNAL LONG FUNCTION PC_MAIN_COST
	EXTERNAL LONG FUNCTION PC_MAIN_PRICE
	EXTERNAL LONG FUNCTION PC_MAIN_PRCTYPE
	EXTERNAL LONG FUNCTION UTL_MAIN_MEASURE

	%PAGE

	!
	! Loop up the inventory query
	!
	IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
	THEN
		IF MVALUE = "MAGIC"
		THEN
			V% = LIB$SET_SYMBOL("CMC$SYSTEM", "PS")
			V% = LIB$SET_SYMBOL("CMC$LOCATION", &
				OE_ORDERJOUR::LOCATION)
			V% = LIB$SET_SYMBOL("CMC$SPAWN", "LO")
			CALL SUBR_3SPAWN(SCOPE, &
				"RUN CMC$ROOT:[IC]IC_QURY_QUERY")
			V% = LIB$SET_SYMBOL("CMC$SYSTEM", "")
			V% = LIB$SET_SYMBOL("CMC$SPAWN", "")
			V% = LIB$SET_SYMBOL("CMC$LOCATION", "")
			EXIT FUNCTION
		END IF

		SCOPE::SCOPE_EXIT = 0%
	END IF

 Cont:
	SELECT SMG_WINDOW::IDENT

	CASE PS_MAIN_TICKETJOUR.ID

		MAINT_GROUP = PS_MAIN_TICKETJOUR(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

		SELECT MOPTION

		CASE OPT_ENTRY
			OE_ORDERJOUR_ONE = OE_ORDERJOUR

		CASE OPT_OPTLIST
			MVALUE = MVALUE + " Line caSh forM " + &
				"Ord_print pacK_print inv_prinT"

		CASE OPT_MOREMENU
			OE_ORDERJOUR_ONE = OE_ORDERJOUR

			SELECT EDIT$(MVALUE, -1%)
			!
			! Line
			!
			CASE "LINE"
	!++
	! Abstract:LINE
	!--
				MAINT_GROUP = MAIN_WINDOW(PS_MAIN_TICKETLINE.ID, "")
			!
			! cash in and out
			!
			CASE "CASH"
	!++
	! Abstract:CASH
	!--
				MAINT_GROUP = MAIN_WINDOW(PS_MAIN_CASHINOUT.ID, "M")

			CASE "FORM"
	!++
	! Abstract:FORM
	!--
				SOPTION$(1%) = "Order   Form"
				SOPTION$(2%) = "Packing Form"
				SOPTION$(3%) = "Invoice Form"

 SelectSOption:
				X% = ENTR_3CHOICE(SCOPE, "", "", SOPTION$(), "", &
					0%, "Select Option", "", 0%)

				SELECT SCOPE::SCOPE_EXIT
				CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
					GOTO ExitFunction
				END SELECT

				SELECT X%

				CASE 1%

					V% = PS_OUTP_TICKET(OE_ORDERJOUR::ORDNUM, &
						BATCH_NO$, 4%)

				CASE 2%

	!++
	! Abstract:FORM
	!--
					V% = PS_OUTP_TICKET(OE_ORDERJOUR::ORDNUM, &
						BATCH_NO$, 2%)

				CASE 3%

					V% = PS_OUTP_TICKET(OE_ORDERJOUR::ORDNUM, &
						BATCH_NO$, 0%)

				CASE ELSE
					GOTO SelectSOption
				END SELECT

			CASE "INV_PRINT"
	!++
	! Abstract:INV_PRINT
	!--
				V% = PS_OUTP_TICKET(OE_ORDERJOUR::ORDNUM, &
					BATCH_NO$, 1%)

			CASE "PACK_PRINT"
	!++
	! Abstract:PACK_PRINT
	!--
				V% = PS_OUTP_TICKET(OE_ORDERJOUR::ORDNUM, &
					BATCH_NO$, 3%)

			CASE "ORD_PRINT"
	!++
	! Abstract:ORD_PRINT
	!--
				V% = PS_OUTP_TICKET(OE_ORDERJOUR::ORDNUM, &
					BATCH_NO$, 5%)

			END SELECT

		CASE OPT_AFTEROPT

			SELECT MVALUE

			!
			! Need to remove under old key, and insert under
			! (possibly) new key
			!
			CASE "Change", "Blank", "Initialize"
				IF OE_ORDERJOUR::ORDNUM <> &
						OE_ORDERJOUR_OLD::ORDNUM
				THEN
					OE_ORDERJOUR_ONE = OE_ORDERJOUR_OLD

					MAINT_GROUP = MAIN_WINDOW( &
						PS_MAIN_TICKETLINE.ID, "C")
				END IF

				!
				! Check for location change and if so then reset
				! inventory quantities from old location to new
				! location.
				!
				IF OE_ORDERJOUR::LOCATION <> &
					OE_ORDERJOUR_OLD::LOCATION
				THEN
					OE_ORDERJOUR_ONE = OE_ORDERJOUR_OLD

					MAINT_GROUP = MAIN_WINDOW( &
						PS_MAIN_TICKETLINE.ID, "R")
				END IF

			!
			! Erase records in subwindow
			!
			CASE "Erase"
				OE_ORDERJOUR_ONE = OE_ORDERJOUR
				MAINT_GROUP = &
					MAIN_WINDOW(PS_MAIN_TICKETLINE.ID, "E")

			END SELECT

		CASE OPT_TESTENTRY

			SELECT MLOOP

			CASE 9%
				IF MVALUE = "ADD"
				THEN
					OE_ORDERJOUR_ONE = OE_ORDERJOUR
					V% = MAIN_WINDOW(PS_MAIN_TICKETLINE.ID, "A")
					OE_ORDERJOUR = OE_ORDERJOUR_ONE
				END IF

			END SELECT

		END SELECT

	CASE PS_MAIN_TICKETLINE.ID

		SELECT MOPTION

		CASE OPT_RESETDEFAULT
			MVALUE = OE_ORDERJOUR_ONE::ORDNUM

		CASE OPT_SUBWIND

			SELECT MLOOP

			CASE 6%
				MVALUE = OE_ORDERJOUR::ORDNUM

			CASE ELSE
				MVALUE = OE_ORDERJOUR_ONE::ORDNUM

			END SELECT

		END SELECT

		MAINT_GROUP = PS_MAIN_TICKETLINE_01(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE OE_MAIN_SHIPTO.ID

		SELECT MOPTION

		CASE OPT_RESETDEFAULT
			MVALUE = OE_ORDERJOUR_ONE::CUSNUM

		CASE OPT_SUBWIND

			SELECT MLOOP

			CASE 6%
				MVALUE = OE_ORDERJOUR::CUSNUM

			CASE ELSE
				MVALUE = OE_ORDERJOUR_ONE::CUSNUM

			END SELECT

		END SELECT

		MAINT_GROUP = OE_MAIN_SHIPTO(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE OE_MAIN_ORDERTYPE.ID

		MAINT_GROUP = OE_MAIN_ORDERTYPE(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE OE_MAIN_SALESTAX.ID

		MAINT_GROUP = OE_MAIN_SALESTAX(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE OE_MAIN_CATEGORY.ID

		MAINT_GROUP = OE_MAIN_CATEGORY(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_35CUSTOM.ID

		MAINT_GROUP = AR_MAIN_35CUSTOM(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE AR_MAIN_CUSTYPE.ID

		MAINT_GROUP = AR_MAIN_CUSTYPE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PC_MAIN_COST.ID

		SELECT MOPTION

		CASE OPT_RESETDEFAULT
			MVALUE = PD_PRODUCT_ONE::PRODUCT_NUM

		CASE OPT_SUBWIND

			SELECT MLOOP

			CASE 6%
				MVALUE = PD_PRODUCT::PRODUCT_NUM

			CASE ELSE
				MVALUE = PD_PRODUCT_ONE::PRODUCT_NUM

			END SELECT

		END SELECT

		MAINT_GROUP = PC_MAIN_COST(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PC_MAIN_PRICE.ID

		SELECT MOPTION

		CASE OPT_RESETDEFAULT
			MVALUE = PD_PRODUCT_ONE::PRODUCT_NUM

		CASE OPT_SUBWIND

			SELECT MLOOP

			CASE 6%
				MVALUE = PD_PRODUCT::PRODUCT_NUM

			CASE ELSE
				MVALUE = PD_PRODUCT_ONE::PRODUCT_NUM

			END SELECT

		END SELECT

		MAINT_GROUP = PC_MAIN_PRICE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PC_MAIN_PRCTYPE.ID

		MAINT_GROUP = PC_MAIN_PRCTYPE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_PRODUCT.ID

		MAINT_GROUP = PD_MAIN_PRODUCT(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE IC_WRIT_PRODUCT.ID

		SELECT MOPTION

		CASE OPT_RESETDEFAULT
			MVALUE = OE_ORDERLINE::PRODUCT

		CASE OPT_OPTLIST
			MVALUE = "Add Default Page eXit"

		END SELECT

		MAINT_GROUP = IC_WRIT_PRODUCT(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE UT_MAIN_CARRIER.ID

		MAINT_GROUP = UT_MAIN_CARRIER(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE UT_MAIN_TERMS.ID

		MAINT_GROUP = UT_MAIN_TERMS(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_LOCATION.ID

		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_COUNTRY.ID

		MAINT_GROUP = UTL_MAIN_COUNTRY(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE SA_MAIN_SALESMAN.ID

		MAINT_GROUP = SA_MAIN_SALESMAN(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE SB_MAIN_SUBACCOUNT.ID

		MAINT_GROUP = SB_MAIN_SUBACCOUNT(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE PS_MAIN_CASHREG.ID

		MAINT_GROUP = PS_MAIN_CASHREG(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE PS_MAIN_CASHINOUT.ID

		MAINT_GROUP = PS_MAIN_CASHINOUT(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE OE_MAIN_CREASON.ID

		MAINT_GROUP = OE_MAIN_CREASON(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_PRODTYPE.ID

		MAINT_GROUP = PD_MAIN_PRODTYPE(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_CATEGORY.ID

		MAINT_GROUP = PD_MAIN_CATEGORY(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_MEASURE.ID

		MAINT_GROUP = UTL_MAIN_MEASURE(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE OE_MAIN_REGHEADER.ID

		MAINT_GROUP = OE_MAIN_REGHEADER(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE OE_MAIN_REASONACCT.ID

		MAINT_GROUP = OE_MAIN_REASONACCT(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE OE_MAIN_REGLINE.ID

		MAINT_GROUP = OE_MAIN_REGLINE(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE GL_MAIN_CHART.ID

		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	END SELECT

	!
	! Loop up the inventory query
	!
	IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
	THEN
		V% = LIB$SET_SYMBOL("CMC$LOCATION", OE_ORDERJOUR::LOCATION)
		V% = LIB$SET_SYMBOL("CMC$SPAWN", "LO")
		CALL SUBR_3SPAWN(SCOPE, "RUN CMC$ROOT:[IC]IC_QURY_QUERY")
		V% = LIB$SET_SYMBOL("CMC$SPAWN", "")
		V% = LIB$SET_SYMBOL("CMC$LOCATION", "")
		GOTO Cont
	END IF

 ExitFunction:
	END FUNCTION


30000	!*******************************************************************
	! HACK to make it possible to store _OUTP_ functions in
	! object library
	!*******************************************************************

	SUB FORM_LOADVAR(VARNAME$, REALVALUE, TEXTVALUE$)

		CALL PS_OUTP_TICKET_LOADVAR(VARNAME$, REALVALUE, TEXTVALUE$)

	END SUB