1	%TITLE "Convert Cyma AP files to CMC"
	%SBTTL "AP_CONV_CYMA"
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
	!	$ BAS AP_SOURCE:AP_CONV_CYMA/LINE
	!	$ LINK/EXECUTABLE=AP_EXE: AP_CONV_CYMA, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_CONV_CYMA.OBJ;*
	!
	! Author:
	!	08/01/91 - Jeff Beard
	!
	! Modification history:
	!
	!	08/14/91 - Kevin Handy
	!		Removed A+.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/27/96 - Kevin Handy
	!		Reformat source.
	!
	!	05/12/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/09/99 - Kevin Handy
	!		Add second parameter to UNPASTE_VIRTUAL_DISPLAY
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

	!
	! Extrenal Functions
	!
	EXTERNAL STRING FUNCTION CONV_ACCT

10	ON ERROR GOTO 19000

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR)		AP_VENDOR_CDD  AP_VENDOR

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
	! Create the second data display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY( 16%, 80%, DISPLAY_FILE%,,, )
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	!
	! PRINT BANNER
	!
	SMG_STATUS% = SMG$PUT_CHARS_WIDE(DISPLAY_ID%, "Convert Cyma AP files", &
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

	EXT$="???"
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "Company Password (XXX) :", &
		8%, 20%)
	EXT$ = ENTR_3STRING(SCOPE, DISPLAY_ID%, &
		"8;43", "Password", EXT$, 0%, "'E", "")

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

	! Ask user if they realy would like to convert all files

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

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "File   :", 14%, 4%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "Record :", 15%, 4%)

1200	!
	! EMPLOYEE file does not exist, so create it
	!
	IF PASS%=0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Creating new AP_VENDOR file", 1%)
		KILL "AP_VENDOR.MAS"
		PASS% = -1%
	ELSE
		CALL ENTR_3MESSAGE(SCOPE, &
			"Adding records into AP_VENDOR file", 1%)
	END IF

2000	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.CRE"

2010	CALL ASSG_CHANNEL(APISAM.CH%,STAT%)

	OPEN "APISAM."+EXT$ FOR INPUT AS FILE APISAM.CH%, &
		ORGANIZATION SEQUENTIAL

	LINPUT #APISAM.CH%, LINE$

2020	LINPUT #APISAM.CH%, LINE$

	GOTO 2020 IF LEFT(LINE$,1%)="1" OR VAL(MID$(LINE$,6%,4%))=0%


	AP_VENDOR::VENNUM	= MID$(LINE$,2%,4%)
	AP_VENDOR::VENNAM	= ""
	AP_VENDOR::ADD1		= ""
	AP_VENDOR::ADD2		= ""
	AP_VENDOR::CITY		= ""
	AP_VENDOR::STATE	= ""
	AP_VENDOR::ZIP		= FORMAT$(VAL(MID$(LINE$,6%,4%)),"<0>###")
	AP_VENDOR::COUNTRY	= ""
	AP_VENDOR::PHONE	= ""
	AP_VENDOR::POADD1	= ""
	AP_VENDOR::POADD2	= ""
	AP_VENDOR::POCITY	= ""
	AP_VENDOR::POSTATE	= ""
	AP_VENDOR::POZIP	= ""
	AP_VENDOR::POCOUNTRY	= ""
	AP_VENDOR::POPHONE	= ""
	AP_VENDOR::PURGE	= "N"
	AP_VENDOR::FEDID	= ""
	AP_VENDOR::FLG1099	= ""
	AP_VENDOR::DUEDAYS	= 0%
	AP_VENDOR::DUEDATE	= "00"
	AP_VENDOR::DISDAYS	= 0%
	AP_VENDOR::DISDATE	= ""
	AP_VENDOR::DISCPER	= 0%
	AP_VENDOR::ALPSRT	= ""

2030	PUT #AP_VENDOR.CH%

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, &
		AP_VENDOR::VENNUM+LINE$, 19%, 13%)

	GOTO 2020

2120	CLOSE #APISAM.CH%

	CALL ASSG_FREECHANNEL(APISAM.CH%)
	CALL ASSG_CHANNEL(APMASTER.CH%,STAT%)

2130	OPEN "APMASTER."+EXT$ FOR INPUT AS FILE APMASTER.CH%, &
		ORGANIZATION SEQUENTIAL

	LINPUT #APMASTER.CH%, LINE$
	COUNTER% = 1%

	!
	! Paste the second data display
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(DISPLAY_FILE%, &
		SCOPE::SMG_PBID, 5%, 1%)
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "APMASTER  ", 18%, 13%)

 ConfirmMaster:
	CONF$ = EDIT$(ENTR_3YESNO(SCOPE, DISPLAY_FILE%, &
		"", "Display Each Record", "N", 16%, "'", "N"), -1%)

	!
	! Check for special keys typed
	!
	SELECT SCOPE::SCOPE_EXIT
	!
	! Control/C, Uparrow,
	!
	CASE 3%, SMG$K_TRM_UP, SMG$K_TRM_DOWN
		GOTO ConfirmMaster	! (Ignored)

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
		GOTO ConfirmMaster

	END SELECT

	IF CONF$ <> "Y"
	THEN
		SMG_STATUS% = SMG$ERASE_DISPLAY(DISPLAY_FILE%)
		SMG_STATUS% = SMG$UNPASTE_VIRTUAL_DISPLAY(DISPLAY_FILE%, &
			SCOPE::SMG_PBID)
		GOTO 2150
	END IF

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, &
		"# of Accounts   "+MID$(LINE$,1%,5%), 1%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, &
		"# of Del Acc    "+MID$(LINE$,6%,5%), 2%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, &
		"# of transacts  "+MID$(LINE$,11%,5%), 3%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, &
		"Company Title   "+MID$(LINE$,16%,30%), 4%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, &
		"Address1        "+MID$(LINE$,46%,30%), 5%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, &
		"City            "+MID$(LINE$,76%,18%), 6%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, &
		"State           "+MID$(LINE$,94%,2%), 7%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, &
		"Zip Code        "+MID$(LINE$,96%,9%), 8%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, &
		"Audit file Y/N  "+MID$(LINE$,105%,1%), 10%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, &
		"Post GL    Y/Y  "+MID$(LINE$,106%,1%), 9%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, &
		"Cash Accrual    "+MID$(LINE$,107%,1%), 10%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, &
		"# Rec Entries   "+MID$(LINE$,108%,5%), 11%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, &
		"Bal of Rec Ent  "+MID$(LINE$,113,11%), 12%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, &
		"Blank           "+MID$(LINE$,124%,11%), 13%, 3%)


2150	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "APMASTER", 18%, 13%)
	LINPUT #APMASTER.CH%, LINE$
	COUNTER% = COUNTER% + 1%

	IF CONF$ <> "Y"
	THEN
		IF NOT BEFORE_%
		THEN
			SMG_STATUS% = SMG$ERASE_DISPLAY(DISPLAY_FILE%)
			BEFORE_% = 1%
		END IF
		GOTO 2155
	END IF

 ConfirmNext:
	CONF$ = EDIT$(ENTR_3YESNO(SCOPE, DISPLAY_FILE%, &
		"", "Display Each Record", "N", 16%, "'", "N"), -1%)

	IF NOT BEFORE_%
	THEN
		SMG_STATUS% = SMG$ERASE_DISPLAY(DISPLAY_FILE%)
	END IF

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
		GOTO 2155
	END IF

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, &
		"Account Title     "+MID$(LINE$,1%,30%), 1%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, &
		"Address1          "+MID$(LINE$,31%,30%), 2%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, &
		"City              "+MID$(LINE$,61%,18%), 3%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, &
		"State             "+MID$(LINE$,79%,2%), 4%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, &
		"Zip Code          "+MID$(LINE$,81%,9%), 5%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, &
		"Alternate Address "+MID$(LINE$,90%,10%), 6%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, &
		"Phone             "+MID$(LINE$,100%,9%), 7%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, &
		"Terms Code        "+MID$(LINE$,109%,6%), 8%, 3%)


2155	GET #AP_VENDOR.CH%, KEY#4% EQ FORMAT$(COUNTER%, "<0>###")

	AP_VENDOR::VENNAM	= MID$(LINE$,1%,30%)  ! ACCOUNT TITLE
	AP_VENDOR::ADD1		= MID$(LINE$,31%,30%) ! STREET ADDRESS
	AP_VENDOR::ADD2		= ""
	AP_VENDOR::CITY		= MID$(LINE$,61%,18%) ! CITY
	AP_VENDOR::STATE	= MID$(LINE$,79%,2%)  ! STATE
	AP_VENDOR::ZIP		= MID$(LINE$,81%,9%)+" "
	AP_VENDOR::COUNTRY	= "US"
	AP_VENDOR::PHONE	= MID$(LINE$,120%,10%)! PHONE (NO DASHES)
	AP_VENDOR::POADD1	= MID$(LINE$,31%,30%) ! STREET ADDRESS
	AP_VENDOR::POADD2	= ""
	AP_VENDOR::POCITY	= MID$(LINE$,61%,18%) ! CITY
	AP_VENDOR::POSTATE	= MID$(LINE$,79%,2%)  ! STATE
	AP_VENDOR::POZIP	= MID$(LINE$,81%,9%)+" "
	AP_VENDOR::POCOUNTRY	= "US"
	AP_VENDOR::POPHONE	= MID$(LINE$,120%,10%)! PHONE (NO DASHES)
	AP_VENDOR::ALPSRT	= MID$(LINE$,1%,30%)

	!
	! WHAT ABOUT TERMS CODE
	!
	UPDATE #AP_VENDOR.CH%

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, AP_VENDOR::VENNUM, 19%, 13%)

	GOTO 2150

2160	SMG_STATUS% = SMG$ERASE_DISPLAY(DISPLAY_FILE%)
	SMG_STATUS% = SMG$UNPASTE_VIRTUAL_DISPLAY(DISPLAY_FILE%, &
		SCOPE::SMG_PBID)
	CLOSE #APMASTER.CH%
	CLOSE #AP_VENDOR.CH%

	CALL ASSG_FREECHANNEL(APMASTER.CH%)
	CALL ASSG_FREECHANNEL(AP_VENDOR.CH%)

	GOTO Password

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

	CALL SUBR_3EXITPROGRAM(SCOPE, "RUN CMC$ROOT:[AP]AP_MAST_VENDOR", "")

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

	CASE 1200%
		RESUME 2000 IF ERR = 5%
		FILENAME$ = "AP_VENDOR"

	CASE 2000%
		FILENAME$ = "AP_VENDOR"

	CASE 2010%
		FILENAME$ = "APISAM"

	CASE 2020%
		RESUME 2120 IF ERR = 11%
		FILENAME$ = "APISAM"

	CASE 2120%
		FILENAME$ = "APMASTER"

	CASE 2130%
		FILENAME$ = "APMASTER"

	CASE 2150%
		RESUME 2160 IF ERR = 11%
		FILENAME$ = "APMASTER"

	CASE 2155%
		RESUME 2150 IF ERR = 155%
		FILENAME$ = "AP_VENDOR"

	END SELECT

	RESUME HelpError

19999	END

20000	FUNCTION STRING CONV_ACCT (STRING ASCII_ACCT)
	CONV_ACCT = LEFT$(ASCII_ACCT,4%) + "." + RIGHT$(ASCII_ACCT,5%)
29999	END FUNCTION
