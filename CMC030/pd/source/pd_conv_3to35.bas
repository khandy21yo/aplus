1	%TITLE "Convert from Version 3 to 3.5"
	%SBTTL "PD_CONV_3TO35"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	!	.p
	!	This program is used in the conversion from version 3.0
	!	Product Description files to version 3.5.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PD_SOURCE:PD_CONV_3TO35/LINE
	!	$ LINK/EXECUTABLE=PD_EXE: PD_CONV_3TO35, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PD_CONV_3TO35.OBJ;*
	!
	! Author:
	!
	!	06/19/90 - Lance Williams
	!
	! Modification history:
	!
	!	04/14/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/06/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
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

	%INCLUDE "SOURCE:[PD.OPEN]PD_ACCOUNT.HB"
	MAP (PD_ACCOUNT)		PD_ACCOUNT_CDD		PD_ACCOUNT

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODACCT.HB"
	MAP (PD_PRODACCT)	PD_PRODACCT_CDD		PD_PRODACCT

	!*******************************************************************
	! Initilize Convert
	!*******************************************************************

	CALL READ_INITIALIZE

	!
	! Create the data display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY( 20%, 80%, DISPLAY_ID%,,, )
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	SMG_STATUS% = SMG$ERASE_DISPLAY(DISPLAY_ID%)
	!
	! Paste the data display
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY( DISPLAY_ID%, PASTE_ID%, 1%, 1% )
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%
	SMG_STATUS% = SMG$SET_CURSOR_MODE(PASTE_ID%, SMG$M_CURSOR_OFF)

	!
	! PRINT BANNER
	!
	SMG_STATUS% = SMG$PUT_CHARS_WIDE(DISPLAY_ID%, "Convert PD files", &
		2%, 15%,SMG$M_BOLD)
	SMG_STATUS% = SMG$DRAW_LINE(DISPLAY_ID%, 4%, 1%, 4%, 80%, &
		SMG$M_BOLD)

	SCOPE::PRG_ITEM = "CONFIRM"
 ConfirmConv:	! Ask user if they realy would like to convert all PD files

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
		CALL ENTR_3BADKEY(SCOPE,SCOPE::SCOPE_EXIT)
		GOTO ConfirmConv

	END SELECT

	SMG_STATUS% = SMG$DELETE_CHARS(DISPLAY_ID%, 30%, 9%, 20%)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	GOTO ExitProgram IF CONF$ <> "Y"

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "File   :", 18%, 4%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "Record :", 19%, 4%)

1000	!
	! Tell user the file we are converting is PD_ACCOUNT
	!
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "PD_ACCOUNT", 18%, 15%)

	!
	! Open the needed files
	!
	%INCLUDE "SOURCE:[PD.OPEN]PD_ACCOUNT.OPN"
	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODACCT.CRE"

	RESET #PD_ACCOUNT.CH%

1200	! StPDt the actual conversion
	WHILE 1
		GET #PD_ACCOUNT.CH%
		! Show user current record
		SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, PD_ACCOUNT::INVACCT, &
			19%, 15%)

		PD_PRODACCT::INVACC	= PD_ACCOUNT::INVACCT
		PD_PRODACCT::LOCATION	= PD_ACCOUNT::LOCATION
		PD_PRODACCT::PRODTYPE	= PD_ACCOUNT::PRODTYPE

		GOSUB GetExtra

		PUT #PD_PRODACCT.CH%
	NEXT


1300	! Get Extra ACCOUNT Records
 GetExtra:	! Set with spaces incase they are not found

                PD_PRODACCT::COSACCT	= SPACE$(LEN(PD_PRODACCT::COSACCT))
                PD_PRODACCT::DISCACCT   = SPACE$(LEN(PD_PRODACCT::DISCACCT))

1350	RETURN

1500	! Close the files & free the channel numbers we don't need any more.
	CLOSE #PD_ACCOUNT.CH%
	CLOSE #PD_PRODACCT.CH%
	CALL ASSG_FREECHANNEL(PD_ACCOUNT.CH%)
	CALL ASSG_FREECHANNEL(PD_PRODACCT.CH%)

	! StPDt a list of completed converions
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "Converted Files", 6%, 10%)
	! Put file PD_ACCOUNT in a list of completed converions
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "PD_ACCOUNT", 8%, 14%)

2000	! NEXT PROGRAM TO CONVERT IS ??

 ExitProgram:
15000	!*******************************************************************
	! Exit program
	!*******************************************************************

	IF CONF$ = "Y"
	THEN
		CALL ENTR_3MESSAGE(SCOPE,"Conversion Process Complete", 0%)
	ELSE
		CALL ENTR_3MESSAGE(SCOPE,"Aborting Conversion Process", 1%)
	END IF
	CALL ENTR_3MESSAGE(SCOPE, "",1%)
	SMG_STATUS% = SMG$SET_CURSOR_MODE(PASTE_ID%, SMG$M_CURSOR_ON)
	SMG_STATUS% = SMG$DELETE_PASTEBOPDD(PASTE_ID%)
	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(DISPLAY_ID%)

	GOTO 32767

	%PAGE

 HelpError:
	!*******************************************************************
	! Help Message for an Error
	!*******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

19000	!*******************************************************************
	! Error trapping
	!*******************************************************************

	FILENAME$ = ""
	SELECT ERL
	CASE 1200%		! End of ACCOUNT file
		RESUME 1500 IF ERR = 11%
		FILENAME$ = "PD_ACCOUNT"

	CASE 1300%		! Extra ACCOUNT records not found
		RESUME 1350 IF ERR = 155%

	END SELECT

	RESUME HelpError

32767	END
