1	%TITLE "Convert Microdata files"
	%SBTTL "AD_CONV_MICRODATA"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
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
	!	$ BAS AD_SOURCE:AD_CONV_MICRODATA/LINE
	!	$ LINK/EXECUTABLE=AD_EXE: AD_CONV_MICRODATA, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_CONV_MICRODATA.OBJ;*
	!
	! Author:
	!
	!	11/11/91 - Dan Perkins
	!
	! Modification history:
	!
	!	04/17/92 - Frank F. Starman
	!		Added case <2>
	!
	!	10/04/93 - Kevin Handy
	!		Changed =< to <=.
	!
	!	04/12/95 - Kevin Handy
	!		Changed scope.exit% to scope::scope_exit
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/27/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/12/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
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

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	START_DATE% = DATE_DAYCODE("19671231")

10	ON ERROR GOTO 19000

	!
	! Include CDD
	!
	%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.HB"
	MAP (AD_35ASSET)	AD_35ASSET_CDD		AD_35ASSET

	%INCLUDE "SOURCE:[AD.OPEN]AD_DEPRECIATION.HB"
	MAP (AD_DEPRECIATION)	AD_DEPRECIATION_CDD	AD_DEPRECIATION

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
	SMG_STATUS% = SMG$PUT_CHARS_WIDE(DISPLAY_ID%, &
		"Convert Microdata AD files", &
		2%, 15%,SMG$M_BOLD)

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
1405	%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.CRE"

1406	%INCLUDE "SOURCE:[AD.OPEN]AD_DEPRECIATION.CRE"

1410	CALL ASSG_CHANNEL(CASE_TEXT.CH%, STAT%)
	CASE_TEXT.DEV$ = ""
	CASE_TEXT.NAME$ = CASE_TEXT.DEV$ + "AD.ASC"

	OPEN CASE_TEXT.NAME$ FOR INPUT AS FILE CASE_TEXT.CH%, &
		ORGANIZATION SEQUENTIAL, &
		RECORDSIZE 512%

	GOSUB InitRec

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "ASSET", 9%, 14%)

 GetNextCaseRec:
	!
1420	! Start the actual conversion
	!
	LINPUT #CASE_TEXT.CH%, LINE$

	LINE$ = TRM$(LINE$)
	!
	! Build conversion record - basic field
	!
	X% = POS(LINE$, ">", 1%)

	GOTO 1420 IF X% <= 1%

	MASK$ = LEFT$(LINE$, X%)

	LINE$ = RIGHT$(LINE$, X% + 1%)

	SELECT MASK$

	CASE "<0>"
		AD_35ASSET::ASSET_NUM = RIGHT$(LINE$, 8%)
		AD_DEPRECIATION::ASSET_NUM = RIGHT$(LINE$, 8%)

	CASE "<1>"
		AD_35ASSET::DESCRIPTION = LINE$

	CASE "<2>"
		SELECT EDIT$(LINE$,-1%)
		CASE "IF","SALMON","OFFICE","BLKFT","PLANT"
			AD_35ASSET::LOCATION = "100"
		CASE "SALES","S.C.","CO","GA","","STORE","MKTG"
			AD_35ASSET::LOCATION = "100"
		CASE "P.OFFI","P.OFFIC","UTAH","ROGER","HAROLD"
			AD_35ASSET::LOCATION = "100"
		CASE "ADMIN","B$V","IFPLNT","TRIM","RETIRED"
			AD_35ASSET::LOCATION = "100"
		CASE "SCREEN","IFPL","PALNT","WAREHS","SHIP"
			AD_35ASSET::LOCATION = "100"
		CASE "BKFT","WI","SLC,UT","IFPLAN"
			AD_35ASSET::LOCATION = "100"
		CASE ELSE
			AD_35ASSET::LOCATION = "000"
		END SELECT
	CASE "<4>"
		L% = LEN(LINE$)
		AD_35ASSET::ASSET_TYPE = RIGHT$(LINE$, L%-2%)
		AD_35ASSET::DEPT_NUM = "00" + LEFT$(LINE$, 1%)

	CASE "<8>"
		!
		! this can be a multiline record
		! the delimiter between products is CHR$(253%)
		!
		S% = INSTR(1%, LINE$, '253'c)
		LINE$ = LEFT(LINE$, S% - 1%)
		AD_35ASSET::SERIAL_NUM = LINE$

	CASE "<6>"
		AD_35ASSET::SERVDATE = DATE_INVDCODE(START_DATE% + VAL%(LINE$))

	CASE "<7>"
		AD_35ASSET::COST = VAL(LINE$) / 100.

	CASE "<11>"
		!
		! this can be a multiline record
		! the delimiter between products is CHR$(253%)
		!
		S% = INSTR(1%, LINE$, '253'C)
		DEPCLASS1$ = LEFT(LINE$,2%)
		DEPCLASS2$ = MID(LINE$,S%+1%,2%)

	CASE "<12>"
		S% = INSTR(1%, LINE$, '253'C)
		DEPCLASS1$ = DEPCLASS1$+LEFT(LINE$,2%)
		DEPCLASS2$ = DEPCLASS2$+MID(LINE$,S%+1%,2%)

	CASE "<END>"
		IF AD_35ASSET::LOCATION<>"100"
		THEN
			GOSUB InitRec
			GOTO 1420
		END IF

1431		SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, &
			AD_35ASSET::ASSET_NUM + " " + &
			AD_35ASSET::DESCRIPTION, 9%, 20%)

		PUT #AD_35ASSET.CH%

1433		AD_DEPRECIATION::DEP_OBJECT = "B"
		AD_DEPRECIATION::DEPCLASS = DEPCLASS1$
		PUT #AD_DEPRECIATION.CH% IF RIGHT$(DEPCLASS1$,3%)<>"00"

1434		AD_DEPRECIATION::DEP_OBJECT = "T"
		AD_DEPRECIATION::DEPCLASS = DEPCLASS2$
		PUT #AD_DEPRECIATION.CH% IF RIGHT$(DEPCLASS2$,3%)<>"00"

1435		GOSUB InitRec
	END SELECT

	GOTO 1420

 InitRec:
	AD_35ASSET::ASSET_NUM	= ""
	AD_35ASSET::DESCRIPTION	= ""
	AD_35ASSET::ASSET_TYPE	= ""
	AD_35ASSET::LOCATION	= ""
	AD_35ASSET::DEPT_NUM    = ""
	AD_35ASSET::SERIAL_NUM	= ""
	AD_35ASSET::SERVDATE	= ""
	AD_35ASSET::COST	= 0.0
	AD_35ASSET::SALVAGE	= 0.0
	AD_35ASSET::BONUS	= 0.0
	AD_35ASSET::ITC		= 0.0
	AD_35ASSET::ITCREDUCE	= 0.0
	AD_35ASSET::UNITS	= 0.0
	AD_35ASSET::RET_DATE	= ""
	AD_35ASSET::PROCEEDS	= 0.0
	AD_35ASSET::NOTES	= ""

	RETURN

 ExitProgram:
15000	!*******************************************************************
	! Exit program
	!*******************************************************************

	IF CONF$ = "Y"
	THEN
		CALL ENTR_3MESSAGE(SCOPE,"Conversion Process Complete", 0%)
	ELSE
		CALL ENTR_3MESSAGE(SCOPE,"Aborting Conversion Process", 0%)
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

	SELECT ERL

	CASE 1405%
		FILENAME$ = "AD_35ASSET"

	CASE 1431%
		RESUME 1435

	CASE 1420%
		IF ERR = 157%
		THEN
			SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, LEFT(LINE$,5%), 11%, 54%)
			RESUME
		END IF
		RESUME 15000 IF ERR = 11%
		FILENAME$ = "AD.ASC"

	END SELECT

	RESUME HelpError

32767
