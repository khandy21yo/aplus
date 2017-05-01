1	%TITLE "Order Entry Journal"
	%SBTTL "OE_MAST_ORDERJOUR"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1990 BY
	!
	! Computer Management Center
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
	!	The ^*Order Entry Journal\* option
	!	maintains orders being placed. Each journal file is assigned a
	!	user batch number consisting of two (2) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Order Entry Journal
	!	.x Enter Orders
	!
	! Option:
	!
	!	PS_MAIN_TICKETJOUR$HELP
	!	PS_MAIN_TICKETLINE$HELP
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_MAST_ORDERJOUR/LINE
	!	$ LINK/EXE=OE_EXE: OE_MAST_ORDERJOUR, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_MAST_ORDERJOUR.OBJ;*
	!
	! Author:
	!
	!
	! Modification history:
	!
	!	06/09/92 - Dan Perkins
	!		Added CASE OPT_TESTENTRY to OE_MAIN_ORDERJOUR.
	!		This is done here in the MAST program so that
	!		OE_MAIN_ORDERJOUR can also be used by the MO system.
	!
	!	06/11/92 - Dan Perkins
	!		Added code to SHIPTO.ID so that records could be
	!		added to SHIPTO while in ORDER JOURNAL.
	!
	!	06/12/92 - Dan Perkins
	!		Added ability to print Order Forms directly from
	!		within the Order Journal Program.
	!
	!	09/04/92 - Dan Perkins
	!		Modifications to make _OUTP_ functions work in
	!		an object library.
	!
	!	11/17/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	12/14/92 - Dan Perkins
	!		Added AR_MAIN_CUSTYPE to MAINT_GROUP so that F-14
	!		key will work when adding a customer with the F-17 key.
	!
	!	01/29/93 - Dan Perkins
	!		Added options to call OUTP_INVOICE function to print
	!		pick lists.
	!
	!	02/02/93 - Frank F. Starman
	!		Replace PS_MAIN_TICKETJOUR with PS_MAIN_TICKETJOUR
	!
	!	04/14/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!		Fix last parameter on entr_3choice.
	!
	!	10/20/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/30/2000 - Kevin Handy
	!		Lose useless error trap
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PS_WINDOW.INC"

	!
	! This common area must be mapped in both the main program and
	! in MAINT_GROUP.
	!
	COM (CH_OE_ORDERJOUR) &
		OE_ORDERJOUR.CH%

	COM (BATCH_NO) &
		BATCH_NO$ = 2%

	%PAGE

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

	!
	! Look up device
	!
	CALL  READ_DEVICE("OE_ORDERJOUR", OE_ORDERJOUR.DEV$, STAT%)

300	!
	! Query user for year of file
	!
	CALL FIND_FILE(OE_ORDERJOUR.DEV$ + "OE_ORDERJOUR_%%.JRL", OE_ORDERJOUR_FILE$(), &
		16%, "", "")

	OE_ORDERJOUR_FILE% = VAL%(OE_ORDERJOUR_FILE$(0%))

	IF OE_ORDERJOUR_FILE%
	THEN
		OE_ORDERJOUR_FILE$(LOOP%) = &
			MID(OE_ORDERJOUR_FILE$(LOOP%), 14%, 2%) &
				FOR LOOP% = 1% TO OE_ORDERJOUR_FILE%

		TEMP$ = "Order Journal Batch"

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

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Batch number:", 11%, 30%)

320	!
	! Set up the help message
	!
	SCOPE::PRG_ITEM = "FLD01BATCH"

	!
	! Assign default batch number
	!
	BATCH_NO$ = "01"

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 11%, 43%, BATCH_NO$, -1%, 16%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	BATCH_NO$ = EDIT$(BATCH_NO$, -1%)

	IF LEN(TRM$(BATCH_NO$)) <> 2%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Please enter the batch number in XX format", 0%)
		GOTO 320
	END IF

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

19990	END


20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	COM (BATCH_NO) &
		BATCH_NO$ = 2%

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:PS_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:OE_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:SA_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:SB_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.HB"
	MAP (OE_ORDERJOUR)	OE_ORDERJOUR_CDD OE_ORDERJOUR
	MAP (OE_ORDERJOUR_OLD)	OE_ORDERJOUR_CDD OE_ORDERJOUR_OLD
	MAP (OE_ORDERJOUR_ONE)	OE_ORDERJOUR_CDD OE_ORDERJOUR_ONE

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION PS_MAIN_TICKETJOUR
	EXTERNAL LONG FUNCTION PS_MAIN_TICKETLINE
	EXTERNAL LONG FUNCTION PS_OUTP_TICKET
	EXTERNAL LONG FUNCTION OE_MAIN_ORDERTYPE
	EXTERNAL LONG FUNCTION OE_MAIN_SALESTAX
	EXTERNAL LONG FUNCTION OE_MAIN_SHIPTO
	EXTERNAL LONG FUNCTION OE_MAIN_CATEGORY
	EXTERNAL LONG FUNCTION PD_MAIN_PRODUCT
	EXTERNAL LONG FUNCTION AR_MAIN_35CUSTOM
	EXTERNAL LONG FUNCTION AR_MAIN_CUSTYPE
	EXTERNAL LONG FUNCTION UT_MAIN_CARRIER
	EXTERNAL LONG FUNCTION UT_MAIN_TERMS
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION
	EXTERNAL LONG FUNCTION UTL_MAIN_COUNTRY
	EXTERNAL LONG FUNCTION MAIN_WINDOW
	EXTERNAL LONG FUNCTION SA_MAIN_SALESMAN
	EXTERNAL LONG FUNCTION SB_MAIN_SUBACCOUNT
	EXTERNAL LONG FUNCTION OE_MAIN_REGHEADER
	EXTERNAL LONG FUNCTION OE_MAIN_REGLINE

	%PAGE

	SELECT SMG_WINDOW::IDENT

	CASE PS_MAIN_TICKETJOUR.ID

		MAINT_GROUP = PS_MAIN_TICKETJOUR(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

		SELECT MOPTION

		CASE OPT_ENTRY
			OE_ORDERJOUR_ONE = OE_ORDERJOUR

		CASE OPT_OPTLIST
			MVALUE = MVALUE + " Line forM form_direcT " + &
				"pKform pkfOrm_direct"

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

			CASE "FORM"
	!++
	! Abstract:FORM
	!--
				V% = PS_OUTP_TICKET(OE_ORDERJOUR::ORDNUM, BATCH_NO$, 0%)

			CASE "FORM_DIRECT"
	!++
	! Abstract:FORM_DIRECT
	!--
				V% = PS_OUTP_TICKET(OE_ORDERJOUR::ORDNUM, BATCH_NO$, 1%)

			CASE "PKFORM"
	!++
	! Abstract:PKFORM
	!--
				V% = PS_OUTP_TICKET(OE_ORDERJOUR::ORDNUM, BATCH_NO$, 2%)

			CASE "PKFORM_DIRECT"
	!++
	! Abstract:PKFORM_DIRECT
	!--
				V% = PS_OUTP_TICKET(OE_ORDERJOUR::ORDNUM, BATCH_NO$, 3%)

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
				IF OE_ORDERJOUR::LOCATION <> OE_ORDERJOUR_OLD::LOCATION
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
				MAINT_GROUP = MAIN_WINDOW(PS_MAIN_TICKETLINE.ID, "E")

			END SELECT

		CASE OPT_TESTENTRY

			SELECT MLOOP

			CASE 9%
				IF MVALUE = "ADD"
				THEN
					OE_ORDERJOUR_ONE  = OE_ORDERJOUR
					V% = MAIN_WINDOW(PS_MAIN_TICKETLINE.ID, "A")
					OE_ORDERJOUR      = OE_ORDERJOUR_ONE
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

		MAINT_GROUP = PS_MAIN_TICKETLINE(SMG_WINDOW, MOPTION, &
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

		MAINT_GROUP = AR_MAIN_CUSTYPE(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_PRODUCT.ID

		MAINT_GROUP = PD_MAIN_PRODUCT(SMG_WINDOW, MOPTION, &
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

	CASE OE_MAIN_REGHEADER.ID

		MAINT_GROUP = OE_MAIN_REGHEADER(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE OE_MAIN_REGLINE.ID

		MAINT_GROUP = OE_MAIN_REGLINE(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	END SELECT

	END FUNCTION

30000	!*******************************************************************
	! HACK to make it possible to store _OUTP_ functions in
	! object library
	!*******************************************************************

	SUB FORM_LOADVAR(VARNAME$, REALVALUE, TEXTVALUE$)

		CALL PS_OUTP_TICKET_LOADVAR(VARNAME$, REALVALUE, TEXTVALUE$)

	END SUB
