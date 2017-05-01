1	%TITLE "Convert Register files"
	%SBTTL "OE_CONV_REGISTER"
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
	!	$ BAS OE_SOURCE:OE_CONV_REGISTER/LINE
	!	$ LINK/EXECUTABLE=OE_EXE: OE_CONV_REGISTER, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_CONV_REGISTER.OBJ;*
	!
	! Author:
	!
	!	04/17/92 - Dan Perkins
	!
	! Modification history:
	!
	!	04/27/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/02/93 - Kevin Handy
	!		Clean up (check)
	!
	!	04/14/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/06/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
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


	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	MAP (OE_REGHEADER)	OE_REGHEADER_CDD	OE_REGHEADER

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	MAP (OE_REGLINE)	OE_REGLINE_CDD		OE_REGLINE

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
	SMG_STATUS% = SMG$PUT_CHARS_WIDE(DISPLAY_ID%, "Convert Register Files", &
		2%, 15%, SMG$M_BOLD)

	SMG_STATUS% = SMG$DRAW_LINE(DISPLAY_ID%, 4%, 1%, 4%, 80%, &
		SMG$M_BOLD)

 ConfirmConv:	! Ask user if they realy would like to convert all files

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

	!
	! Open the needed files
	!
1400	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.OPN"
	USE
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

1405	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.OPN"
	USE
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

1410	CALL ASSG_CHANNEL(OE_REGHEADER_TMP.CH%, STAT%)
		OPEN UTL_WORK.DEV$ + "OE_REGHEADER.TMP" FOR OUTPUT AS FILE #OE_REGHEADER_TMP.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP OE_REGHEADER, &
		PRIMARY KEY &
			OE_REGHEADER::ORDNUM, &
		ALTERNATE KEY &
		( &
			OE_REGHEADER::ORDTYPE, &
			OE_REGHEADER::ORDNUM &
		)	CHANGES, &
		ALTERNATE KEY &
		( &
			OE_REGHEADER::ORDCAT, &
			OE_REGHEADER::ORDNUM &
		)	CHANGES, &
		ALTERNATE KEY &
		( &
			OE_REGHEADER::CUSNUM, &
			OE_REGHEADER::ORDNUM &
		)	CHANGES, &
		ALTERNATE KEY &
		( &
			OE_REGHEADER::BATCH, &
			OE_REGHEADER::ORDNUM &
		)	CHANGES, &
			BUFFER 32%, &
		ACCESS MODIFY, ALLOW NONE

1420	CALL ASSG_CHANNEL(OE_REGLINE_TMP.CH%, STAT%)

	OPEN UTL_WORK.DEV$ + "OE_REGLINE.TMP" FOR OUTPUT AS FILE #OE_REGLINE_TMP.CH%, &
	ORGANIZATION INDEXED FIXED, &
		MAP OE_REGLINE, &
		PRIMARY KEY &
		( &
			OE_REGLINE::ORDNUM, &
			OE_REGLINE::LIN, &
			OE_REGLINE::TRANTYPE &
		)	DUPLICATES, &
		ALTERNATE KEY &
		( &
			OE_REGLINE::PRODUCT, &
			OE_REGLINE::ORDNUM, &
			OE_REGLINE::LIN, &
			OE_REGLINE::TRANTYPE &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
		( &
			OE_REGLINE::BATCH, &
			OE_REGLINE::ORDNUM &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
		( &
			OE_REGLINE::REFNUM, &
			OE_REGLINE::TRANTYPE, &
			OE_REGLINE::ORDNUM &
		)	DUPLICATES CHANGES, &
			BUFFER 32%, &
		ACCESS MODIFY, ALLOW NONE

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, &
			"Converting RegHeader Records", &
			9%, 20%)

	!
	! Convert the REGHEADER FILE
	!
	RESET #OE_REGHEADER.CH%

 GetNextRegHeader:
	!
	! Start the actual conversion
	!
1450	WHEN ERROR IN
		GET #OE_REGHEADER.CH%, REGARDLESS
	USE
		CONTINUE 1500 IF ERR = 11%
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, &
		OE_REGHEADER::ORDNUM, &
		11%, 20%)

	OE_REGHEADER::ORDNUM = RIGHT(OE_REGHEADER::ORDNUM, 2%) &
		WHILE LEFT(OE_REGHEADER::ORDNUM, 1%) = "0"

	RSET OE_REGHEADER::ORDNUM = EDIT$(OE_REGHEADER::ORDNUM, 128%)

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, &
		OE_REGHEADER::ORDNUM, &
		11%, 35%)

1460	WHEN ERROR IN
		PUT #OE_REGHEADER_TMP.CH%
	USE
		FILENAME$ = "OE_REGHEADER_TMP"
		CONTINUE HelpError
	END WHEN

	GOTO GetNextRegHeader

	!
	! Convert the REGLINE FILE
	!
1500	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, &
		"Converting RegLine Records", &
		15%, 20%)

	RESET #OE_REGLINE.CH%

 GetNextRegLine:
	!
	! Start the actual conversion
	!
1550	WHEN ERROR IN
		GET #OE_REGLINE.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, &
		OE_REGLINE::ORDNUM, &
		17%, 20%)

	OE_REGLINE::ORDNUM = RIGHT(OE_REGLINE::ORDNUM, 2%) &
		WHILE LEFT(OE_REGLINE::ORDNUM, 1%) = "0"

	RSET OE_REGLINE::ORDNUM = EDIT$(OE_REGLINE::ORDNUM, 128%)

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, &
		OE_REGLINE::ORDNUM, &
		17%, 35%)

1560	WHEN ERROR IN
		PUT #OE_REGLINE_TMP.CH%
	USE
		FILENAME$ = "OE_REGLINE_TMP"
		CONTINUE HelpError
	END WHEN

	GOTO GetNextRegLine

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
