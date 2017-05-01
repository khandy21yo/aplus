1	%TITLE "Kill Ticket Journal"
	%SBTTL "PS_SPEC_KILLTICKET"
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
	!	The ^*Kill Ticket Journal\* program deletes specified
	!	journals associated with the PS Ticket Journal Maintenance routines.
	!	.-5
	!
	! Index:
	!	.x Kill Ticket Journal Maintenance
	!
	! Option:
	!
	! Author:
	!
	!	01/30/92 - Dan Perkins
	!
	! Compile:
	!
	!	$ BAS PS_SOURCE:PS_SPEC_KILLTICKET/LINE
	!	$ LINK/EXEC=PS_EXE:*.EXE PS_SPEC_KILLTICKET,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PS_SPEC_KILLTICKET.OBJ;*
	!
	! Modification history:
	!
	!	02/05/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!		Fix last parameter to entr_3choice
	!
	!	11/14/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	10/28/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/26/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/19/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!--

	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map of the various files
	!
	%INCLUDE "SOURCE:[PS.OPEN]PS_CASHINOUT.HB"
	MAP (PS_CASHINOUT)	PS_CASHINOUT_CDD	PS_CASHINOUT

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.HB"
	MAP (OE_ORDERJOUR)	OE_ORDERJOUR_CDD	OE_ORDERJOUR

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERLINE.HB"
	MAP (OE_ORDERLINE)	OE_ORDERLINE_CDD	OE_ORDERLINE

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!
	! Look up device
	!
	CALL  READ_DEVICE("OE_ORDERJOUR", OE_ORDERJOUR.DEV$, STAT%)

	!
	! Get info required for main file
	!
300	CALL FIND_FILE(OE_ORDERJOUR.DEV$ + "OE_ORDERJOUR_*_%%.JRL", &
		OE_ORDERJOUR_FILE$(), &
		16%, "", "")

	OE_ORDERJOUR_FILE% = VAL%(OE_ORDERJOUR_FILE$(0%))

	IF OE_ORDERJOUR_FILE%
	THEN
		FOR LOOP% = 1% TO OE_ORDERJOUR_FILE%
			OE_ORDERJOUR_FILE$(LOOP%) = &
				RIGHT(OE_ORDERJOUR_FILE$(LOOP%), 14%)
		NEXT LOOP%

		TEMP$ = "Ticket Journal Batch"

		X% = ENTR_3CHOICE(SCOPE, "", "", OE_ORDERJOUR_FILE$(), "", &
			0%, TEMP$, "", 0%)

		IF X% > 0%
		THEN
			BATCH_NO$ = EDIT$(OE_ORDERJOUR_FILE$(X%), -1%)
			GOTO 700
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

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	REG_NO$ = EDIT$(REG_NO$, -1%)

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

700	!
	! Prepare to delete files
	!
	INP$ = ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, &
		"", "Delete Journal " + BATCH_NO$, "N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)
	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

	GOTO ExitProgram IF INP$ = "N"

	CALL READ_DEVICE("OE_ORDERLINE", OE_ORDERLINE.DEV$, STAT%)
	CALL READ_DEVICE("PS_CASHINOUT", PS_CASHINOUT.DEV$, STAT%)

 !	KILL OE_ORDERJOUR.DEV$ + "OE_ORDERJOUR_" + BATCH_NO$ + ".JRL"
	SMG_STATUS% = LIB$DELETE_FILE(OE_ORDERJOUR.DEV$ + &
		"OE_ORDERJOUR_" + BATCH_NO$ + ".JRL;*")

 !	KILL OE_ORDERLINE.DEV$ + "OE_ORDERLINE_" + BATCH_NO$ + ".JRL"
	SMG_STATUS% = LIB$DELETE_FILE(OE_ORDERLINE.DEV$ + &
		"OE_ORDERLINE_" + BATCH_NO$ + ".JRL;*")

 !	KILL PS_CASHINOUT.DEV$ + "PS_CASHINOUT_" + BATCH_NO$ + ".JRL"
	SMG_STATUS% = LIB$DELETE_FILE(PS_CASHINOUT.DEV$ + &
		"PS_CASHINOUT_" + BATCH_NO$ + ".JRL;*")

 ExitProgram:
	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

32767	END
