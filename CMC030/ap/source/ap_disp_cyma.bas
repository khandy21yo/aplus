1	%TITLE "Display Cyma AP files "
	%SBTTL "AP_DISP_CYMA"
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
	!	$ BAS AP_SOURCE:AP_DISP_CYMA/LINE
	!	$ LINK/EXECUTABLE=AP_EXE: AP_DISP_CYMA, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_DISP_CYMA.OBJ;*
	!
	! Author:
	!
	!	08/02/91 - Jeff Beard
	!
	! Modification history:
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/27/96 - Kevin Handy
	!		Reformat source code.
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

	DECLARE STRING FIELDNAME(50)
	DECLARE BYTE   FIELDPAR(50,2)

10	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize Convert
	!*******************************************************************

	CALL READ_INITIALIZE

	!
	! Create first data display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY( 16%, 80%, DISPLAY_ID%,,, )
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	!
	! PRINT BANNER
	!
	SMG_STATUS% = SMG$PUT_CHARS_WIDE(DISPLAY_ID%, "Display Cyma AP files", &
		2%, 15%,SMG$M_BOLD)
	SMG_STATUS% = SMG$DRAW_LINE(DISPLAY_ID%, 4%, 1%, 4%, 80%, &
		SMG$M_BOLD)

 Password:

	!
	! Paste the data display
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(DISPLAY_ID%, &
		SCOPE::SMG_PBID, 1%, 1%)
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	FLNM$= SPACE$(50%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "File Name:", 5%, 20%)
	FLNM$ = ENTR_3STRING(SCOPE, DISPLAY_ID%, &
		"5;31", "FileName ", FLNM$, 0%, "'E", "")

	!
	! Check for special keys typed
	!
	SELECT SCOPE::SCOPE_EXIT

	!
	! Control/C, Uparrow,
	!
	CASE 3%, SMG$K_TRM_UP, SMG$K_TRM_DOWN
		GOTO Password	! (Ignored)

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
		GOTO Password

	END SELECT

	SMG_STATUS% = SMG$DELETE_CHARS(DISPLAY_ID%, 30%, 9%, 20%)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	SELECT EDIT$(SEG$(FLNM$,POS(FLNM$,"]",1%)+1,LEN(FLNM$)), -1%)

	CASE "APTAUDIT.MDI", "APTAUDIT.PSI"
		NO.FIELDS% = 10%
		FIELDNAME(1%) = "Entry Number   "
			FIELDPAR(1,1) = 1%
			FIELDPAR(1,2) = 5%
		FIELDNAME(2%) = "Status 0/A 1/D "
			FIELDPAR(2,1) = 6%
			FIELDPAR(2,2) = 1%
		FIELDNAME(3%) = "Date of Trans  "
			FIELDPAR(3,1) = 7%
			FIELDPAR(3,2) = 6%
		FIELDNAME(4%) = "AP Acc Num     "
			FIELDPAR(4,1) = 13%
			FIELDPAR(4,2) = 4%
		FIELDNAME(5%) = "Doc or Source  "
			FIELDPAR(5,1) = 17%
			FIELDPAR(5,2) = 8%
		FIELDNAME(6%) = "Descr of Trans "
			FIELDPAR(6,1) = 25%
			FIELDPAR(6,2) = 20%
		FIELDNAME(7%) = "GL Account     "
			FIELDPAR(7,1) = 45%
			FIELDPAR(7,2) = 8%
		FIELDNAME(8%) = "Amount of Tran "
			FIELDPAR(8,1) = 53%
			FIELDPAR(8,2) = 12%
		FIELDNAME(9%) = "Status of Tran "
			FIELDPAR(9,1) = 65%
			FIELDPAR(9,2) = 1%
		FIELDNAME(10%) = "2 PAID, 1 NOT  "
			FIELDPAR(10,1) = 0%
			FIELDPAR(10,2) = 0%


	CASE "APCHECKS.MDI", "APCHECKS.PSI"
		NO.FIELDS% = 5%
		FIELDNAME(1%) = "Check Number   "
			FIELDPAR(1,1) = 1%
			FIELDPAR(1,2) = 8%
		FIELDNAME(2%) = "Vendor number  "
			FIELDPAR(2,1) = 9%
			FIELDPAR(2,2) = 4%
		FIELDNAME(3%) = "Date of Check  "
			FIELDPAR(3,1) = 13%
			FIELDPAR(3,2) = 6%
		FIELDNAME(4%) = "AP of Check    "
			FIELDPAR(4,1) = 19%
			FIELDPAR(4,2) = 12%
		FIELDNAME(5%) = "    or VOID    "
			FIELDPAR(5,1) = 0%
			FIELDPAR(5,2) = 0%

	CASE "APTRANS.MDI", "APTRANS.PSI", "APTRANSR.MDI", "APTRANSR.PSI"
		NO.FIELDS% = 9%
		FIELDNAME(1%) = "Status 0/A 1/D "
			FIELDPAR(1,1) = 1%
			FIELDPAR(1,2) = 1%
		FIELDNAME(2%) = "Date of Trans  "
			FIELDPAR(2,1) = 2%
			FIELDPAR(2,2) = 6%
		FIELDNAME(3%) = "AP Acc number  "
			FIELDPAR(3,1) = 8%
			FIELDPAR(3,2) = 4%
		FIELDNAME(4%) = "Doc or Source  "
			FIELDPAR(4,1) = 12%
			FIELDPAR(4,2) = 8%
		FIELDNAME(5%) = "Discr of Trans "
			FIELDPAR(5,1) = 20%
			FIELDPAR(5,2) = 20%
		FIELDNAME(6%) = "GL Account     "
			FIELDPAR(6,1) = 40%
			FIELDPAR(6,2) = 8%
		FIELDNAME(7%) = "Amount of Tran "
			FIELDPAR(7,1) = 48%
			FIELDPAR(7,2) = 12%
		FIELDNAME(8%) = "Status of Tran "
			FIELDPAR(8,1) = 60%
			FIELDPAR(8,2) = 1%
		FIELDNAME(9%) = "2 PAID, 1 NOT  "
			FIELDPAR(9,1) = 0%
			FIELDPAR(9,2) = 0%

	CASE ELSE
		CALL ENTR_3MESSAGE(SCOPE, &
			"That is not a valid File Name", 0%)
		GOTO Password

	END SELECT


2010	CALL ASSG_CHANNEL(FLNM.CH%,STAT%)

	OPEN FLNM$ FOR INPUT AS FILE FLNM.CH%, &
		ORGANIZATION SEQUENTIAL

	LINPUT #FLNM.CH%, LINE$

 ConfirmNext:
	CONF$ = EDIT$(ENTR_3YESNO(SCOPE, DISPLAY_ID%, &
		"", "Display Each Record", "Y", 16%, "'", "Y"), -1%)

	!
	! Check for special keys typed
	!
	SELECT SCOPE::SCOPE_EXIT
	!
	! Control/C, Uparrow,
	!
	CASE 3%, SMG$K_TRM_UP, SMG$K_TRM_DOWN
		GOTO ConfirmNext	! (Ignored)

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
		GOTO ConfirmNext

	END SELECT

	IF CONF$ <> "Y"
	THEN
		GOTO 2160
	END IF


2150	LINPUT #FLNM.CH%, LINE$


	FOR IND% = 1% TO NO.FIELDS%
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, FIELDNAME(IND%) + &
			MID$(LINE$,FIELDPAR(IND%,1%),FIELDPAR(IND%,2%)), &
			IND%+6%, 3%)
	NEXT IND%

	GOTO ConfirmNext

2160	SMG_STATUS% = SMG$ERASE_DISPLAY(DISPLAY_ID%)
	CLOSE #FLNM.CH%

	CALL ASSG_FREECHANNEL(FLNM.CH%)

	GOTO Password

 ExitProgram:
15000	!*******************************************************************
	! Exit program
	!*******************************************************************

	IF CONF$ = "Y"
	THEN
		CALL ENTR_3MESSAGE(SCOPE,"Display Process Complete", 0%)
	ELSE
		CALL ENTR_3MESSAGE(SCOPE,"Aborting Display Process", 0%)
	END IF

	GOTO 19999

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

	SELECT ERL
	CASE 2010%
		IF ERR = 5%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
			"That is not a valid File Name", 0%)
			RESUME PASSWORD
		END IF
		FILENAME$ = "FLNM"

	CASE 2150%
		RESUME 2160 IF ERR = 11%
		FILENAME$ = "FLNM"

	END SELECT

	RESUME HelpError

19999	END
