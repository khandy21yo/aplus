1	%TITLE "Copy Account Information to Create GL Accounts"
	%SBTTL "GL_SPEC_COPYACCT"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1990 BY
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
	!	The ^*Copy Accounts\* option
	!	copies the information from a group of GL Accounts into
	!	another such group, changing only the Account Number.
	!	.lm -5
	!
	! Index:
	!	.x GL Accounts>Copy
	!	.x Copy>General Ledger Accounts
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_SPEC_COPYACCT/LINE
	!	$ LINK/EXE=GL_EXE GL_SPEC_COPYACCT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_SPEC_COPYACCT.OBJ;*
	!
	! Author:
	!
	!	06/14/90 - Aaron Redd
	!
	! Modification history:
	!
	!	03/12/92 - Kevin Handy
	!		Unrolled error trapping (check)
	!
	!	03/24/95 - Kevin Handy
	!		(V3.6)
	!		Changed setting of the current period to zero
	!		instead of the month that the program was
	!		run in.
	!
	!	04/10/95 - Kevin Handy
	!		Update to v3.6 coding standards.
	!
	!	04/12/95 - Kevin Handy
	!		Changed scope.exit% to scope_exit%
	!
	!	09/04/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	09/27/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!*******************************************************************
	! Bring in all of the included files, MAPs, and external functions.
	!*******************************************************************

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! MAP for the Chart of Accounts Record
	!
	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD	GL_CHART

	!
	! Declare all variables and constants
	!
	DECLARE	STRING	ACCT.WC, ACCT.MASK		! User inputs
	DECLARE	STRING	ACCT.TEMP, CHAR			! Miscellaneous strings
	DECLARE	LONG	ACCTS				! Account counter

	%PAGE

	!*******************************************************************
	! Take care of preliminary stuff
	!*******************************************************************

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Initialize maintainance
	!
	CALL READ_INITIALIZE

	SCOPE::PRG_ITEM = "QURY_USER"

	!
	! Set initial variable values
	!
	ACCT.WC = "                  "
	ACCT.MASK = "                  "
	ACCTS = 0%

	!
	! Read device for GL Chart of Accounts file
	!
	CALL READ_DEVICE("GL_CHART", GL_CHART.DEV$, STAT%)

	%PAGE

	!*******************************************************************
	! Create the background of the Query screen
	!*******************************************************************

	!
	! Create a virtual display (upon which to play out this drama)
	!
300	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 80%, &
		SMG_SCREEN_DATA%)

	!
	! Put the Query Screen background on the display
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Copy GL Accounts", 6%, 20%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Account WildCard:", 10%, 20%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Account Mask:    ", 12%, 20%)

	!
	! Paste the virtual display onto the screen where we can see it
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 1%, 1%)

	%PAGE

	!*******************************************************************
	! Ask the user for the Account WildCard and Mask
	!*******************************************************************

	!
	! Query user for WildCard
	!
320	SCOPE::PRG_ITEM = "FLD01WC"

	!++
	! Abstract:FLD01WC
	!	.x General Ledger Accounts>Account WildCard
	!	^*Account WildCard\*
	!	.b
	!	.lm +5
	!	The ^*Account WildCard\* field specifies
	!	which GL Accounts are to be copied, by entering a wildcard.
	!	Every account existing in the Chart of Accounts file will be
	!	checked against the wildcard, and all matching accounts will
	!	be copied according to the Account Mask.
	!	.b
	!	Eighteen spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!
	!--

	!
	! Enter the WildCard in the first field
	!
	ACCT.WC = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, "10;40", &
			"Account WildCard", ACCT.WC, 32%, "'E", ACCT.WC)

	!
	! Did the user press any special keys?
	!
	SELECT SCOPE::SCOPE_EXIT

	!
	! User wants to exit (^C, <EXIT>, ^Z, or <CANCEL>)
	!
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	!
	! User only entered data (keypunch, <LF>, <FF>, <CR>, or <DO>)
	!
	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	!
	! Leave this feild (<UP> or <DOWN>)
	!
	CASE SMG$K_TRM_UP, SMG$K_TRM_DOWN
		GOTO 340

	!
	! Something else got pressed
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 320

	END SELECT

	%PAGE

	!
	! Query user for Account Mask
	!
340	SCOPE::PRG_ITEM = "FLD02MK"

	!++
	! Abstract:FLD02MK
	!	.x General Ledger Accounts>Account Mask
	!	^*Account Mask\*
	!	.b
	!	.lm +5
	!	The ^*Account Mask\* field specifies
	!	the form of the account number(s) of the copied accounts.
	!	.b
	!	.lm +5
	!	If "%%%%%%-100" was entered as the
	!	Account Wildcard and "%%%%%%-200" as the Account Mask, then
	!	the information contained in Account 111111-100 would be copied over
	!	into Account 111111-200, while Account 555555-500 would be
	!	ignored.
	!	.lm -5
	!	.b
	!	Eighteen spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!
	!--

	ACCT.MASK = ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, "12;40", &
		"Account Mask", ACCT.MASK, 32%, "'E", ACCT.MASK)

	!
	! Did the user press any special keys?
	!
	SELECT SCOPE::SCOPE_EXIT

	!
	! User wants to exit (^C, <EXIT>, ^Z, or <CANCEL>)
	!
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		GOTO ExitProgram

	!
	! User only entered data (keypunch, <LF>, <FF>, <CR>, or <DO>)
	!
	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	!
	! Leave this feild (<UP> or <DOWN>)
	!
	CASE SMG$K_TRM_UP, SMG$K_TRM_DOWN
		GOTO 320

	!
	! Something else got pressed
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 340

	END SELECT

	!
	! Go back and try again if either initial value is still there
	!
	GOTO 320 IF	(ACCT.WC = "                  ") OR &
			(ACCT.MASK = "                  ")

	%PAGE

	!*******************************************************************
	! Open the GL Chart of Accounts file and the Temporary file
	!*******************************************************************

	!
	! Change help flag
	!
	SCOPE::PRG_ITEM = "OPEN_FILES"

	!
	! Find a channel number for the temporary file
	!
	CALL ASSG_CHANNEL(GL_CHART.TEMP%, STAT%)

	!
	! Open the GL Chart of Accounts File to modify it
	!
1000	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.MOD"
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	!
	! Tell the user that we've opened the file successfully
	!
	CALL HELP_34MESSAGE(SCOPE, "Opened GL Chart File ...", "I", "", "", "")

	!
	! Now, open a temporary GL Sequential file
	!
1050	WHEN ERROR IN
		OPEN GL_CHART.DEV$ + "GL_TEMP.SEQ" AS FILE GL_CHART.TEMP%, &
			ORGANIZATION SEQUENTIAL FIXED, &
			MAP GL_CHART, &
			BUFFER 32%, &
			TEMPORARY, &
			ACCESS MODIFY, ALLOW MODIFY
	USE
		FILENAME$ = "GL_TEMP"
		CONTINUE HelpError
	END WHEN

	!
	! Tell the user what we've done this time
	!
	CALL HELP_34MESSAGE(SCOPE, "Opened Temporary File ...", "I", "", "", "")

	%PAGE

	!*******************************************************************
	! Take all GL Accounts matching the WildCard, and PUT them
	!	into the temporary sequential file.
	!*******************************************************************

	!
	! Change help flag
	!
	SCOPE::PRG_ITEM = "FIND_ACCTS"

	!
	! Start at the beginning of the GL Chart file
	!
1100	WHEN ERROR IN
		RESET #GL_CHART.CH%
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	!
	! Put a place on the screen to tell how many Accounts we will copy
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Found Accounts ...    0", 14%, 20%)

 GetNextRec:
	!
	! Loop really starts here
	!
1150	WHEN ERROR IN
		GET #GL_CHART.CH%, REGARDLESS
	USE
		CONTINUE 1200 IF (ERR = 11%)
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	!
	! Check Account number versus the WildCard
	!
	GOTO GetNextRec IF (COMP_STRING(GL_CHART::ACCT, ACCT.WC) = 0%)

	!
	! Since the Account matched, PUT the record in the temporary file
	!
1160	WHEN ERROR IN
		PUT #GL_CHART.TEMP%
	USE
		FILENAME$ = "GL_TEMP"
		CONTINUE HelpError
	END WHEN

	!
	! Add one to the Account counter
	!
	ACCTS = ACCTS + 1%

	!
	! Tell the user how many Accounts we will be copying (as of now)
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		FORMAT$(ACCTS, "####"), 14%, 39%)

	!
	! Go back the GET another record
	!
	GOTO GetNextRec

	%PAGE

	!*******************************************************************
	! Now, take each record from the temporary file, fix it up, and
	!	copy it into the GL Chart file.
	!*******************************************************************

	!
	! First, let's check and see if ANY records are to be copied
	!
1200	GOTO FinishUp IF (ACCTS = 0%)

	!
	! Change help flag
	!
	SCOPE::PRG_ITEM = "COPY_ACCTS"

	!
	! Go back to the beginning of the temporary file
	!
1210	WHEN ERROR IN
		RESET #GL_CHART.TEMP%
	USE
		FILENAME$ = "GL_TEMP"
		CONTINUE HelpError
	END WHEN

	!
	! Count down the number of accounts left to copy
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Accounts to copy ... " + FORMAT$(ACCTS, "####"), 14%, 20%)

 CopyNextAcct:
	!
	! Loop starts here
	!
1250	WHEN ERROR IN
		GET #GL_CHART.TEMP%, REGARDLESS
	USE
		CONTINUE EndTempFile IF (ERR = 11%)
		FILENAME$ = "GL_TEMP"
		CONTINUE HelpError
	END WHEN

	!
	! Blank the History portions of the record
	!
	GL_CHART::DOLLAR(I%) = 0.0 FOR I% = 0% to 20%
	GL_CHART::UNIT(I%) = 0.0 FOR I% = 0% to 20%
	GL_CHART::HOUR(I%) = 0.0 FOR I% = 0% to 20%
	GL_CHART::RUNDOL = 0.0
	GL_CHART::RUNUNIT = 0.0
	GL_CHART::RUNHOUR = 0.0
	GL_CHART::CURDOL = 0.0
	GL_CHART::CURUNIT = 0.0
	GL_CHART::CURHOUR = 0.0
	GL_CHART::BATCH = ""

	!
	! Set today's date into the Current Period field
	!
	GL_CHART::CPERIOD = 0%

	!
	! Change the Account number (a long, involved process)
	!
	ACCT.TEMP = ""

	!
	! Loop with index I, up to the length of the Account Mask
	!
	FOR I% = 1% TO LEN(ACCT.MASK)

		!
		! Pick out the Ith character in ACCT.MASK
		!
		CHAR = MID(ACCT.MASK, I%, 1%)

		!
		! If the character is a wildcard, then replace it with
		!	the Ith character of the GL Account Number
		!
		IF (CHAR = "?") OR (CHAR = "%")
		THEN
			CHAR = MID(GL_CHART::ACCT, I%, 1%)
		END IF

		!
		! Add the character onto the end of the temporary Account Number
		!
		ACCT.TEMP = ACCT.TEMP + CHAR

	NEXT I%

	!
	! Stick anything the mask missed onto the end of the temporary Account
	!
	ACCT.TEMP = ACCT.TEMP + RIGHT(GL_CHART::ACCT, LEN(ACCT.MASK))

	!
	! Replace the Account number with the temporary
	!
	GL_CHART::ACCT = ACCT.TEMP

	!
	! Now, PUT the record into the Chart file
	!
1260	WHEN ERROR IN
		PUT #GL_CHART.CH%
	USE
		CONTINUE 1270 IF (ERR = 134%)
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	!
	! Subtract one from the Account counter
	!
1270	ACCTS = ACCTS - 1%

	!
	! Tell the user how many Accounts we have left to copy (as of now)
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		FORMAT$(ACCTS, "####"), 14%, 41%)

	!
	! If we're not done (counter = zero), go back to GET another record
	!
	GOTO CopyNextAcct IF (ACCTS > 0%)

	%PAGE

 FinishUp:
	!******************************************************************
	! Clean everything up and get ready to exit
	!******************************************************************

1300	!
	! Close files
	!
	CLOSE #GL_CHART.TEMP%
	CLOSE #GL_CHART.CH%

	%PAGE

	!******************************************************************
	! Send a success message to the user, and exit the program
	!******************************************************************

	!
	! Tell the user that we're done
	!
	CALL HELP_34MESSAGE(SCOPE, "Finished Copying -- Ready to Exit", &
		"S", "", "", "")

 ExitProgram:
	!
	! Erase Display
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_SCREEN_DATA%)

	!
	! Exit to the next program or menu
	!
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 EndTempFile:
	!******************************************************************
	! WARNING:
	!	This error message is caused by reaching the end of the
	!	temporary file before the counter has reached zero.
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), "W", &
		ERN$, "Temporary file ended unexpectedly", NUM1$(ERR))

	GOTO 1300

	%PAGE

 HelpError:
	!******************************************************************
	! UNTRAPPED ERRORS:
	!	This section was moved from inside the error trapping to
	!	outside so that errors occurring at lower levels could be
	!	trapped.  BASIC will not allow the possibility of an error
	!	to occur inside of an error trap, even in a different module.
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

19000	!*******************************************************************
	! Trap any and all Run-Time Errors which could occur
	!*******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
