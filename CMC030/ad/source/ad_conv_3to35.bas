1	%TITLE "Convert from Version 3 to 3.5"
	%SBTTL "AD_CONV_3TO35"
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
	!	.b
	!	.lm +5
	!	This program is used in the conversion from version 3.0
	!	asset depreciation files to version 3.5.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_CONV_3TO35/LINE
	!	$ LINK/EXECUTABLE=AD_EXE: AD_CONV_3TO35, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_CONV_3TO35.OBJ;*
	!
	! Author:
	!
	!	02/22/91 - Craig Tanner
	!
	! Modification history:
	!
	!	04/12/95 - Kevin Handy
	!		Converted scope.exit% to scope::scope_exit.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/24/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/12/97 - Kevin Handy
	!		Clean up source code.
	!		Use integers in #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/13/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!		Use WHEN ERROR IN
	!		Add more error handlers
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

10	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[AD.OPEN]AD_ASSET.HB"
	MAP (AD_ASSET)		AD_ASSET_CDD	AD_ASSET

	%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.HB"
	MAP (AD_35ASSET)		AD_35ASSET_CDD	AD_35ASSET
	MAP (AD_35ASSET_OLD)	AD_35ASSET_CDD	AD_35ASSET_OLD, AD_35ASSET2

	%INCLUDE "SOURCE:[AD.OPEN]AD_RETIRED.HB"
	MAP (AD_RETIRED)	AD_RETIRED_CDD	AD_RETIRED

	!*******************************************************************
	! Initilize Convert
	!*******************************************************************

	CALL READ_INITIALIZE

	!
	! Create the data display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(20%, 80%, DISPLAY_ID%,,,)
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	SMG_STATUS% = SMG$ERASE_DISPLAY(DISPLAY_ID%)
	!
	! Paste the data display
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(DISPLAY_ID%, PASTE_ID%, 1%, 1%)
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%
	SMG_STATUS% = SMG$SET_CURSOR_MODE(PASTE_ID%, SMG$M_CURSOR_OFF)

	!
	! PRINT BANNER
	!
	SMG_STATUS% = SMG$PUT_CHARS_WIDE(DISPLAY_ID%, "Convert AD files", &
		2%, 15%,SMG$M_BOLD)
	SMG_STATUS% = SMG$DRAW_LINE(DISPLAY_ID%, 4%, 1%, 4%, 80%, &
		SMG$M_BOLD)

	SCOPE::PRG_ITEM = "CONFIRM"
 ConfirmConv:	! Ask user if they realy would like to convert all AD files

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
	! Tell user the file we are converting is AD_ASSET
	!
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "AD_ASSET", 18%, 15%)

	!
	! Open the needed files
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_ASSET.OPN"
	USE
		FILENAME$ = "AD_ASSET.OPN"
		CONTINUE HelpError
	END WHEN

1010	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_RETIRED.OPN"
	USE
		CONTINUE 1150 IF ERR = 5%
		FILENAME$ = "AD_RETIRED"
		CONTINUE HelpError
	END WHEN

1150 !	KILL AD_ASSET.DEV$ + "AD_35ASSET.MAS" FOR I%=1% TO 10%

	SMG_STATUS% = LIB$DELETE_FILE(AD_ASSET.DEV$ + "AD_35ASSET.MAS;*")

1160	%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.CRE"

	RESET #AD_ASSET.CH%

1200	! Start the actual conversion
	WHILE 1%
		WHEN ERROR IN
			GET #AD_ASSET.CH%
		USE
			CONTINUE 1500 IF ERR = 11%
			FILENAME$ = "AD_ASSET"
			CONTINUE HelpError
		END WHEN

		! Show user current record
		SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, AD_ASSET::ASSET_NUM, &
			19%, 15%)

		AD_35ASSET::ASSET_NUM	= AD_ASSET::ASSET_NUM
		AD_35ASSET::DESCRIPTION = AD_ASSET::DESCRIPTION
		AD_35ASSET::ASSET_TYPE	= AD_ASSET::ASSET_TYPE
		AD_35ASSET::LOCATION	= AD_ASSET::LOCATION
		AD_35ASSET::DEPT_NUM	= AD_ASSET::DEPT_NUM
		AD_35ASSET::SERIAL_NUM	= AD_ASSET::SERIAL_NUM
		AD_35ASSET::SERVDATE	= AD_ASSET::SERVDATE
		AD_35ASSET::COST	= AD_ASSET::COST
		AD_35ASSET::SALVAGE	= AD_ASSET::SALVAGE
		AD_35ASSET::BONUS	= AD_ASSET::BONUS
		AD_35ASSET::ITC		= AD_ASSET::ITC
		AD_35ASSET::ITCREDUCE	= AD_ASSET::ITCREDUCE
		AD_35ASSET::UNITS	= AD_ASSET::UNITS

		GOSUB GetExtra

		WHEN ERROR IN
			PUT #AD_35ASSET.CH%
		USE
			IF ERR = 134	! Duplicate Keys found (already converted)
			THEN
				CONF$  = "N"
				CONTINUE Exitprogram
			END IF

			FILENAME$ = "AD_ASSET"
			CONTINUE HelpError
		END WHEN
	NEXT

1300	! Get Extra RETIRED fields
 GetExtra:	! Set with spaces incase they are not found

		AD_35ASSET::RET_DATE	= SPACE$(8%)
		AD_35ASSET::PROCEEDS	= 0.0
		AD_35ASSET::NOTES	= SPACE$(40%)

		WHEN ERROR IN
			GET #AD_RETIRED.CH%, KEY #0% EQ AD_35ASSET::ASSET_NUM
		USE
			CONTINUE 1350 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "AD_RETIRED"
			CONTINUE HelpError
		END WHEN

		AD_35ASSET::RET_DATE	= AD_RETIRED::RET_DATE
		AD_35ASSET::PROCEEDS	= AD_RETIRED::PROCEEDS
		AD_35ASSET::NOTES	= AD_RETIRED::NOTES

1350	RETURN

1500	!
	! Close the files & free the channel numbers we don't need any more.
	!
	CLOSE #AD_ASSET.CH%
	CLOSE #AD_35ASSET.CH%
	CALL ASSG_FREECHANNEL(AD_ASSET.CH%)
	CALL ASSG_FREECHANNEL(AD_RETIRED.CH%)
	CALL ASSG_FREECHANNEL(AD_35ASSET.CH%)

	!
	! Start a list of completed converions
	!
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "Converted Files", 6%, 10%)
	!
	! Put file AD_ASSET in a list of completed converions
	!
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "AD_ASSET", 8%, 14%)

2000	!
	! NEXT PROGRAM TO CONVERT IS ??
	!

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

	GOTO ExitProgram

32767	END
