1	%TITLE "Menu Command Level"
	%SBTTL "UTL_MENU"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1988 BY
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
	! Abstract:HELP
	!	.p
	!	The ^*Menu Command Level (MCL)\* is required to access CMC system or
	!	macro programs.
	!	.p
	!	The system programs are accessible from the first menu level by selecting
	!	a menu prompt. Selection on the menu prompt can be done by pointing arrow to
	!	the prompt and pressing the ^*<return key>\* or by typing the prompt and
	!	pressing the ^*<return key>\* (entering only bolded prompt letters makes
	!	the selection unique). String of the menu prompts separated by space is
	!	allowed. To assign a menu path use MACRO command.
	!	.p
	!	There are several ^*Menu Macro Commands\* which provide from any level
	!	of the Menu access to CMC utility programs and information common to
	!	other CMC modules. All macro commands must start with ^*/\* (slash).
	!	The wide variety of features the MCL are utilized to
	!	manage and control CMC software which extends from routines, to installing
	!	software updates, to maintaining document destination tables. The following is
	!	a list of all CMC macro commands and qualifiers. For more detail
	!	see CMC Business Software documentation or type /MACRO/LIST at MCL level.
	!	.note
	!	The user can define his macro commands using the CMC MACRO command.
	!	.end note
	!	.b
	!	.list 0,"*"
	!	.le
	!	^*AMORTIZATION\*
	!	.le
	!	^*BATCH\*,/MONITOR
	!	.le
	!	^*BYE\*
	!	.le
	!	^*CALENDAR\*
	!	.le
	!	^*COMPRESS\*
	!	.le
	!	^*COUNTRY\*,/LIST
	!	.le
	!	^*DEVICE\*,/LIST
	!	.le
	!	^*END\*
	!	.le
	!	^*EXIT\*
	!	.le
	!	^*HELP\*
	!	.le
	!	^*LOGOUT\*,/FULL
	!	.le
	!	^*MACRO\*,/DELETE,/LIST
	!	.le
	!	^*PROFILE\*,/PERIOD,/STRUCTURE
	!	.le
	!	^*QUIT\*
	!	.le
	!	^*REPORT\*,/DESTINATION,/LIST,/SETTINGS
	!	.le
	!	^*STRING\*,/LIST,/PRINT
	!	.le
	!	^*SYSTEM\*,/INSTALL,/LIST
	!	.le
	!	^*TIMEOUT\*
	!	.els
	!
	! Index:
	!	.x Menu Command Level
	!	.x MCL
	!	.x Menu Prompt
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_MENU/LINE
	!	$ LINK/EXECUTABLE=UTL_EXE:MENU UTL_MENU, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UTL_MENU.OBJ;*
	!	$ PURGE/LO CMC:MENU.EXE
	!	$ INSTAL
	!	REPLACE CMC:MENU.EXE/OPEN/HEAD/SHARE
	!	EXIT
	!
	! Author:
	!
	!	01/01/86 - Kevin Handy
	!
	! Modification history:
	!
	!	07/09/86 - Kevin Handy
	!		Converted to BP+2.3
	!
	!	10/28/86 - Kevin Handy
	!		Converted to use RMS-Indexed file
	!
	!	01/31/88 - Robert Peterson
	!		Add report settings screen into menu
	!
	!	02/02/88 - Robert Peterson
	!		Change ENTNOL to return HELP and INTERRUPT keys.
	!		Only look up help if HELP is pressed.  Similarly,
	!		only interrupt if INTERRUPT is pressed.
	!
	!	04/08/88 - Robert Peterson
	!		Print copyright and license notes when menu is
	!		started up.
	!
	!	10/16/88 - Kevin Handy
	!		Fixed problem with [c|m|c] in the corner of the
	!		screen not always showing up properly.
	!
	!	02/10/89 - Kevin Handy
	!		Modified for changes in ENTR_NOLSTRING
	!
	!	06/27/89 - Kevin Handy
	!		Modified to use READ_INITIALIZE.
	!
	!	08/11/89 - B. Craig Larsen
	!		Modified to read company name from banner file; if
	!		not there, then from profile file.  Also, modified to
	!		timeout rather than sleep after the banner is printed.
	!		Finally, modified to allow user to go out to SYSTEM
	!		and back into another chosen path all in one command
	!		line (i.e. if several levels down in the menu the user
	!		could type "SYSTEM PR MASTER EMP" to take him out and
	!		back into EMP).
	!
	!	10/25/89 - Kevin Handy
	!		Modified to use the company name out of the
	!		profile if it is there, instead of the company
	!		name in the banner (due to multiple requests).
	!
	!	10/26/89 - Frank F. Starman
	!		Remove OVERVIEW,RUN from list of choices.
	!		Add MON,HELP in List of choices. Go to help message if
	!		there in no program on the lowest level.
	!
	!	01/26/90 - Frank F. Starman
	!		Make error trapping more readable. Change variable
	!		FILENAME$ to IMAGENAME$ where it was needed.
	!
	!	01/24/90 - Frank F. Starman
	!		Add MENUINSTAL option.
	!
	!	01/24/90 - Kevin Handy
	!		Fixed bug returning from help screen caused
	!		<...> key not defined errors.
	!
	!	01/25/90 - Frank F. Starman
	!		Add BATCH, TIMEOUT and COMPRESS option.
	!
	!	04/18/90 - Frank F. Starman
	!		Add MCL and Macro commands.
	!
	!	06/21/90 - Kevin Handy
	!		Slight changes to int compile statements.
	!
	!	08/04/90 - Frank F. Starman
	!		Lower dimension for some arrays.
	!
	!	08/10/90 - Frank F. Starman
	!		Setup that all processes which require report
	!		settins screen will grab always record from
	!		CMC: directory.
	!
	!	08/30/90 - Kevin Handy
	!		Fixed problem (again) on main input that most
	!		every special key typed wipes out whatever has
	!		been typed.  The clock even wipes it out.
	!
	!	09/28/90 - Kevin Handy
	!		Modified to work with text menu files, instead
	!		of the RMS data files.
	!
	!	10/01/90 - Kevin Handy
	!		Modified to handle include menu files.
	!
	!	10/04/90 - Kevin Handy
	!		Fixed bugs: Unable to handle an empty menu.
	!		Interrupt key on initial level dies.
	!
	!	10/05/90 - Kevin Handy
	!		Fixed bug where macro item would send arrow
	!		to bottom of screen.
	!
	!	10/10/90 - Frank F. Starman
	!		Fixed bug where some macro with arguments
	!		terminated with error message "undefined command".
	!
	!	10/11/90 - Kevin Handy
	!		Disabled blanking of symbol CMC$COMMANDS after
	!		reading it in, so the user will return back after
	!		an abort.
	!
	!	10/12/90 - Kevin Handy
	!		Modified to lose command path when exiting from menu.
	!
	!	05/20/91 - Kevin Handy
	!		Modified to define SYS$LOGIN and DEFAULT when running
	!		a report in the background.  This will allow us to
	!		run in background when a "SET DEFAULT" has been done.
	!
	!	05/28/91 - Frank F. Starman
	!		Add macro RELEASE.
	!
	!	07/14/91 - Kevin Handy
	!		Removed error trapping for 510, which doesn't exist.
	!
	!	08/02/91 - Kevin Handy
	!		Some modifications to try to speed up this pig.
	!
	!	08/13/91 - Kevin Handy
	!		Modified out all of the A+'s, since the copyright
	!		office said it is too close to "A plus Tax".
	!
	!	09/03/91 - Kevin Handy
	!		Fixed bug that the report settings lookup was not
	!		padding with spaces, so "APAGE" would pull up
	!		"APAGED" if "APAGE" did not exist, then would get
	!		a "Key not changeable" error.
	!
	!	09/04/91 - Kevin handy
	!		Added code that will allow menu's to be named
	!		"username.MNU" so that we can have different users
	!		log into the same directory, but get different
	!		menus.
	!
	!	09/17/91 - Kevin Handy
	!		Modified to set return location more often than
	!		just when we chain to another program.  This will
	!		make it nicer for those of us who control/C out
	!		and then want to come back in where we were.
	!
	!	02/01/92 - Frank F. Starman
	!		Add macro SET.
	!
	!	03/13/92 - Kevin Handy
	!		Removed variables "CURLEVEL%()" and "SMG$M_CURSOR_OFF"
	!
	!	09/15/92 - Kevin Handy
	!		increase maximum number of items allowed in a menu
	!		list from 25 to 35.
	!
	!	03/16/92 - Kevin Handy
	!		Modifications to handle bucket locked errors a little
	!		bit better.  Before only allowed one person in at a
	!		time, now allows several, but only first gets to save.
	!
	!	03/23/93 - Kevin Handy
	!		Fixed bug in 03/16/93 change, where it wouldn't copy
	!		record over if it didn't already exist.
	!
	!	06/16/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/25/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	07/06/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/09/93 - Kevin Handy
	!		Disabled BEGIN_PASTEBOARD_UPDATE around menu display.
	!		Now it should scroll better.
	!
	!	10/04/93 - Kevin Handy
	!		Changed "=<" to "<=".
	!
	!	01/31/95 - Kevin Handy
	!		Modified seperator character so that a option
	!		that pulls up another menu displays ">" instead
	!		of "-".
	!
	!	03/13/95 - Kevin Handy
	!		Modified to pass "" to OUTP_SETTINGS instead of
	!		trying to send all possible options.
	!
	!	03/14/95 - Kevin Handy
	!		Modified to use the BACKGROUND field in the
	!		REPORTX structure instead of asking after
	!		the user says GO.
	!
	!	03/14/95 - Kevin Handy
	!		(V3.6)
	!		Modified to handle "After Time".
	!
	!	03/29/95 - Kevin Handy
	!		Changed version number displayed at startup
	!		from 3.5 to 3.6
	!
	!	04/10/95 - Kevin Handy
	!		Updated source code to V3.6 Calico standards.
	!
	!	04/12/95 - Kevin Handy
	!		Changed scope.exit% to scope::scope_exit
	!
	!	04/20/95 - Kevin Handy
	!		Remove bad external definitions.
	!		Fix 1st parameter to entr_4entry.
	!
	!	04/21/95 - Kevin Handy
	!		Put back in question about running background.
	!
	!	05/18/95 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/01/95 - Kevin Handy
	!		Fixed format od record with variants.
	!
	!	02/22/96 - Kevin Handy
	!		Reformat source code. Clean out commented out
	!		code.
	!
	!	05/04/96 - Kevin Handy
	!		Reformat source code.
	!
	!	06/26/96 - Kevin Handy
	!		Close banner file when done with it (and free
	!		up a channel)
	!
	!	07/01/96 - Kevin Handy
	!		Changed banner from CMC to Software Solutions,
	!		so people will know who to call.
	!
	!	05/09/97 - Kevin Handy
	!		Use OUTP_3WRITESTRUCTURE instead of
	!		OUTP_WRITESTRUCTURE
	!
	!	08/28/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/13/99 - Kevin Handy
	!		Usde BASIC$STARLET for LIB$
	!
	!	08/10/99 - Kevin Handy
	!		Clean up (Check)
	!
	!	09/17/99 - Kevin Handy
	!		Modified to make use of WHEN ERROR IN for many
	!		(but not all) errors
	!
	!	09/19/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	07/09/2001 - Kevin Handy
	!		Add code to handle "Gold/F10".
	!		Currently exits menu.
	!
	!	06/20/2002 - Kevin Handy
	!		Lose second include of LIB$ROUTINES
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"
	%INCLUDE "FUNC_INCLUDE:PRINT35.INC"

	MAP (SCOPE) SCOPE_STRUCT SCOPE
	MAP (PRINTX) PRINTX_CDD PRINTX

	%INCLUDE "FUNC_INCLUDE:MENU.INC"
	DIM MENU_CDD MENU(600%)			! Menu held in memory

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.HB"
	MAP (UTL_PROFILE) UTL_PROFILE_CDD UTL_PROFILE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_MACROS.HB"
	MAP (UTL_MACROS) UTL_MACROS_CDD UTL_MACROS

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.HB"
	MAP (UTL_REPORT) UTL_REPORT_CDD UTL_REPORT

	DECLARE UTL_REPORT_CDD UTL_REPORT_SYS

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_SCROLL.HB"
	DECLARE SMG_SCROLL_CDD SMG_SCROLL

	COM (CH_UTL_REPORT) &
		UTL_REPORT.CH%

	!
	! Encoding map
	!
	RECORD ENCODER_RECORD
	VARIANT
	CASE
		WORD E.START
		WORD E.LENTH
		BYTE E.ATTR
		WORD E.TLEN
	CASE
		STRING ENCODER = 7%
	END VARIANT
	END RECORD

	DECLARE ENCODER_RECORD ENCODER

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION DSPL_SCROLL
	EXTERNAL LONG	FUNCTION UTL_FUNC_MTRBATCH
	EXTERNAL LONG	FUNCTION FUNC_BANNERCODE
	EXTERNAL LONG	FUNCTION UT_SUBR_MENULOAD

	!
	! Dimension statements
	!
	RECORD MENU_LEVEL_RECORD
		STRING SYSTEM = 8	! System Name
		INTEGER LEVEL		! Pointer into big array
		INTEGER CURLINE		! Arrow Location
	END RECORD

	DIM MENU_LEVEL_RECORD MENU_LEVEL(7%)	! 5 Menu levels

	!
	! Declare variables
	!
	DECLARE INTEGER CONSTANT MAX_MENU = 35

	DIM SCREEN_RFA%(MAX_MENU)

	!
	! Dimension statements
	!
	DIM MENU_ARRAY$(MAX_MENU)

	!
	! MCL record structure
	!
	RECORD MCLCOMMAND
		STRING COMMAND = 20%
		STRING QUALIF(11%) = 20%
		STRING CMD(11%) = 40%
		STRING PRG(11%) = 40%
		STRING HLP(11%) = 20%
	END RECORD

	DIM MCLCOMMAND MCL(150%)

	%PAGE

	ON ERROR GOTO 19000

	XLOOP% = 0%

100	!
	! Initilizations
	!
	S$ = ""
	SCOPE::PRG_ITEM = "HELP"
	MACRO_SIGN$ = "/"
	USERMENU$ = READ_USERNAME + ".MNU"
	MENU$ = "MENU.MNU"
	VERSION$ = "3.6"
	REL_VERSION$ = "036"

	RANDOMIZE

	CALL ASSG_CHANNEL(MENU.CH%, STAT%)
	CALL ASSG_CHANNEL(PRNT.CH%, STAT%)
	CALL ASSG_CHANNEL(DET.CH%, STAT%)

	!
	! Get job number
	!
	JOBNUM$ = EDIT$(READ_SYSJOB, -1%)

	BANNER$ = "SOFT"

	!
	! If detached then exit program
	!
	GOTO ExitProgram IF READ_SYSDETACH

	!******************************************************************
	! Keyboard open routine
	!******************************************************************

	CALL READ_INITIALIZE

	!
	! Create the data display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(20%, 80%, SMG_SCREEN_DATA%,,,)
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	!
	! Paste the data display
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 1%, 1%)
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	!
	! Define scrolling region
	!
	SMG_SCROLL::WINDOW	= SMG_SCREEN_DATA%
	SMG_SCROLL::SCROLL_TOP	= 6%
	SMG_SCROLL::SCROLL_BOT	= 19%
	SMG_SCROLL::TOP_ARRAY	= 1%
	SMG_SCROLL::BOT_ARRAY	= 0%
	SMG_SCROLL::BEG_ELEMENT	= 1%
	SMG_SCROLL::END_ELEMENT	= 1%
	SMG_SCROLL::TOP_LINE	= 1%
	SMG_SCROLL::CUR_LINE	= 1%
	SMG_SCROLL::FIND_LINE	= 1%
	SMG_SCROLL::SMG_FLAG	= 1%
	SMG_SCROLL::PROMPT	= "->"
	SMG_SCROLL::DRAW_COLS	= ""

 !*****************************************************************************

	!
	! Get the parameters passed into MENU from outside
	!
	USER_COMMANDS$ = ""
	CALL LIB$GET_SYMBOL("CMC$COMMAND" BY DESC, USER_COMMANDS$ BY DESC,,)

	IF USER_COMMANDS$ <> ""
	THEN
		SYS_STATUS% = LIB$SET_SYMBOL("CMC$COMMAND", "")
	ELSE
		CALL LIB$GET_SYMBOL("CMC$MENUPATH" BY DESC, &
			USER_COMMANDS$ BY DESC,,)
	END IF

	%PAGE

105	!
	! Open REPORT file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.CRE"
	USE
		FILENAME$ = "UTL_REPORT"
		CONTINUE HelpError
	END WHEN

110	!
	! Open profile file for name of company
	!
	SCOPE::PRG_COMPANY = ""
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.OPN"

		GET #UTL_PROFILE.CH%, RECORD 1%, REGARDLESS
		SCOPE::PRG_COMPANY = UTL_PROFILE::MENU_NAME
		CLOSE #UTL_PROFILE.CH%
	USE
		CONTINUE 115 IF ERR = 5% OR ERR = 155%
		FILENAME$ = "UTL_PROFILE"
		CONTINUE HelpError
	END WHEN

115	!
	! Make sure we can get into an empty menu
	!
	MENU(0%)::EFLAG = -1%

	CURLEVEL% = 0%
	MENU_LEVEL(0%)::SYSTEM = "System"
	MENU_LEVEL(0%)::CURLINE = 1%
	MENU_LEVEL(0%)::LEVEL = 0%

	!
	! Open current menu file (Try username.MNU)
	!
	CALL READ_DEVICE("MENU", MENU.DEV$, STAT%)
	WHEN ERROR IN
		OPEN MENU.DEV$ + USERMENU$ FOR INPUT AS FILE MENU.CH%, &
			ORGANIZATION SEQUENTIAL, &
			ACCESS READ, &
			ALLOW READ
	USE
		CONTINUE 120 IF ERR = 5%
		FILENAME$ = USERMENU$
		CONTINUE HelpError
	END WHEN

	MENU_LEVEL(0%)::LEVEL = UT_SUBR_MENULOAD(SCOPE, &
		MENU.CH%, MENUPOINTER%, "", 0%, MENU())

	CLOSE MENU.CH%

	GOTO 130

120	!
	! Try a default menu
	!
	WHEN ERROR IN
		OPEN MENU.DEV$ + MENU$ FOR INPUT AS FILE MENU.CH%, &
			ORGANIZATION SEQUENTIAL, &
			ACCESS READ, &
			ALLOW READ
	USE
		CONTINUE 130 IF ERR = 5%
		FILENAME$ = "MENU"
		CONTINUE HelpError
	END WHEN

	MENU_LEVEL(0%)::LEVEL = UT_SUBR_MENULOAD(SCOPE, &
		MENU.CH%, MENUPOINTER%, "", 0%, MENU())

	CLOSE MENU.CH%

	%PAGE

130	!
	! Print copyright and license for cmc software
	!
	BANNER$(1%) = "Software Solutions"
	BANNER$(2%) = "Version " + VERSION$
	BANNER$(3%) = "(c) Copyright 1996"
	BANNER$(4%) = "All Rights Reserved"
	BANNER$(5%) = "Software Solutions, Inc."
	BANNER$(6%) = "Idaho Falls, Idaho  USA"
	BANNER$(7%) = TRM$(SCOPE::PRG_COMPANY)
	BANNER$(8%) = ""
	BANNER$(9%) = ""

	!
	! Attempt to open banner file
	!
	CALL ASSG_CHANNEL(BANNER.CH%, STAT%)
	WHEN ERROR IN
		OPEN "CMC:BANNER.CMC" FOR INPUT AS FILE BANNER.CH%, &
			ACCESS READ
	USE
		CONTINUE 133 IF ERR = 5%
		FILENAME$ = "BANNER"
		CONTINUE HelpError
	END WHEN

132	!
	! Read lines from banner
	!
	WHEN ERROR IN
		LINPUT #BANNER.CH%, INLINE$
	USE
		CONTINUE 133 IF ERR = 11%
		FILENAME$ = "BANNER"
		CONTINUE HelpError
	END WHEN

	!
	! If this is the banner to paint at the top of the
	! menu then get it and don't add one to the counter
	!
	SELECT LEFT(INLINE$, 3%)
	CASE "Ban"
		BANNER$ = TRM$(RIGHT(INLINE$, 26%))
	CASE "Exp"
		EDATE$ = TRM$(RIGHT(INLINE$, 26%))
		BANNER$(9%) = "Expiration Date " + PRNT_DATE(EDATE$, 8%) &
			IF EDATE$ <> "00000000"
	CASE "Com"
		BANNER$(7%) = TRM$(RIGHT(INLINE$, 26%))
		SCOPE::PRG_COMPANY = TRM$(RIGHT(INLINE$, 26%)) &
			IF SCOPE::PRG_COMPANY = ""
	CASE "Lic"
		BANNER$(8%) = "License # " + TRM$(RIGHT(INLINE$, 26%))
		LNUMBER$ = TRM$(RIGHT(INLINE$, 26%))
	CASE "Con"
		SNUMBER$ = TRM$(RIGHT(INLINE$, 26%))
	CASE "Ver"
		REL_VERSION$ = TRM$(RIGHT(INLINE$, 26%))
		VERSION$ = XLATE(REL_VERSION$, STRING$(49%, 0%) + "123456789")
		VERSION$ = LEFT(VERSION$, LEN(VERSION$) - 1%) + "." + &
			RIGHT(VERSION$, LEN(VERSION$))
		BANNER$(2%) = "Version " + VERSION$
	END SELECT

	GOTO 132

133	!
	! Don't need this open any longer
	!
	CLOSE BANNER.CH%
	CALL ASSG_FREECHANNEL(BANNER.CH%)

	!
	! Check user license to see if drop dead date has been reached
	!
	SNUMBER% = FUNC_BANNERCODE(EDATE$ + LNUMBER$)

	IF NUM1$(SNUMBER%) <> SNUMBER$ OR &
		(EDATE$ <> "00000000" AND EDATE$ <= DATE_TODAY)
	THEN
		!
		! Today is a good day to drop dead
		!
		CALL HELP_34MESSAGE(SCOPE, "license has expired", &
			"W", "UTL_MENU", "", "LICEXP")

	!++
	! Warning:LICEXP
	!
	!	^*License Expired\*
	!	.p
	!	^*Explanation:\*
	!	.p
	!	License for CMC software has expired.
	!	.p
	!	^*User Action:\*
	!	.p
	!	Please contact Computer Management Center.
	!
	! Index:
	!	.x Licence
	!--

		GOTO ExitProgram
	END IF

	!
	! Skip if not first time through
	!
	GOTO 200 IF USER_COMMANDS$ <> ""

	!
	! Paint the banner on the screen
	!
	TEMP% = LEN(BANNER$(1%)) / 2%
	TEMP% = LEN(BANNER$(2%)) / 2% IF LEN(BANNER$(2%)) / 2% > TEMP%
	TEMP% = TEMP% + 2%

	SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_SCREEN_DATA%)

	SMG_STATUS% = SMG$DRAW_RECTANGLE(SMG_SCREEN_DATA%, 5%, 40% - TEMP%, &
		11%, 40% + TEMP%)

	SMG_STATUS% = SMG$DRAW_LINE(SMG_SCREEN_DATA%, 9%, 40% - TEMP% + 1%, &
		9%, 40% + TEMP% - 1%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, BANNER$(1%), &
		7%, 40% - LEN(BANNER$(1%)) / 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, BANNER$(2%), &
		10%, 40% - LEN(BANNER$(2%)) / 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, BANNER$(3%), &
		12%, 40% - LEN(BANNER$(3%)) / 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, BANNER$(4%), &
		13%, 40% - LEN(BANNER$(4%)) / 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, BANNER$(5%), &
		14%, 40% - LEN(BANNER$(5%)) / 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, BANNER$(6%), &
		15%, 40% - LEN(BANNER$(6%)) / 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, BANNER$(7%), &
		17%, 40% - LEN(BANNER$(7%)) / 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, BANNER$(8%), &
		18%, 40% - LEN(BANNER$(8%)) / 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, BANNER$(9%), &
		19, 40% - LEN(BANNER$(9%)) / 2%, 0%, SMG$M_BOLD)


	SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_SCREEN_DATA%)

	SLEEP 2%

	SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_SCREEN_DATA%, 5%, 1%, 20%, 1%)

200	RESET
	DATA	"AMORTIZATION", "CMC:UT_SPEC_AMORTIZATION", &
				"UT_SPEC_AMORTIZATION", "COMMAND", &
			"", &
		"BATCH", "CMC:UTL_MAST_BATCH_CONTROL", &
				"UTL_MAST_BATCH_CONTROL", "COMMAND", &
			"MONITOR", "BATCH/MONITOR", &
				"UTL_FUNC_MTRBATCH", "COMMAND", &
			"", &
		"BYE", "LOGOUT", "UTL_MENU", "LOGOUT", &
			"", &
		"CALENDAR", "CALENDAR", "DATE_3SELECT", "COMMAND", &
			"", &
		"COMPRESS", "CMC:UTL_SPEC_COMPRESS", &
				"UTL_SPEC_COMPRESS", "COMMAND", &
			"", &
		"COUNTRY", "CMC:UTL_MAST_COUNTRY", &
				"UTL_MAST_COUNTRY", "COMMAND", &
			"LIST", "CMC:UTL_REPORT/CUT008", &
				"UTL_RPRT_COUNTRY", "COMMAND", &
			"", &
		"DEVICE", "CMC:UTL_MAST_DEVICE", &
				"UTL_MAST_DEVICE", "COMMAND", &
			"LIST", "CMC:UTL_REPORT/CUT011", &
				"UTL_RPRT_DEVICE", "COMMAND", &
			"", &
		"END", "EXIT", "UTL_MENU", "EXIT", &
			"", &
		"EXIT", "EXIT", "UTL_MENU", "EXIT", &
			"", &
		"HELP", "HELP", "UTL_MENU", "COMMAND", &
			"", &
		"LOGOUT", "LOGOUT", "UTL_MENU", "LOGOUT", &
			"FULL", "LOGOUT/FULL", "UTL_MENU", "LOGOUT_FULL", &
			"", &
		"MACRO", "MACRO", "UTL_MENU", "MACRO", &
			"DELETE", "MACRO/DELETE", "UTL_MENU", "MACRO_DELETE", &
			"LIST", "CMC:UTL_REPORT/CUT021", &
				"UTL_RPRT_MACROS", "COMMAND", &
			"", &
		"PROFILE", "CMC:UTL_MAST_PROFILE", &
				"UTL_MAST_PROFILE", "COMMAND", &
			"PERIOD", "CMC:UTL_REPORT/CUT006", &
				"UTL_RPRT_PERIOD", "COMMAND", &
			"STRUCTURE", "CMC:UTL_REPORT/CUT007", &
				"UTL_RPRT_PROFILE", "COMMAND", &
			"", &
		"QUIT", "QUIT", "UTL_MENU", "QUIT", &
			"", &
		"RELEASE", "RELEASE", "UTL_MENU", "RELEASE", &
			"", &
		"REPORT", "CMC:UTL_BATCH_REPORT", &
				"UTL_BATCH_REPORT", "COMMAND", &
			"DESTINATION", "CMC:UTL_MAST_DOC_DEST", &
				"UTL_MAST_DOC_DEST", "COMMAND", &
			"LIST", "CMC:UTL_REPORT/CUT002", &
				"UTL_RPRT_REPORT", "COMMAND", &
			"SETTINGS", "CMC:UTL_PRINT", &
				"UTL_PRINT", "COMMAND", &
			"", &
		"SET", "CMC:UTL_SET", &
				"UTL_SET", "COMMAND", &
		"STRING", "CMC:UTL_MAST_STRING_PRINT", &
				"UTL_MAST_STRING_PRINT", "COMMAND", &
			"LIST", "CMC:UTL_REPORT/CUT018", &
				"UTL_RPRT_STRING_PRINT", "COMMAND", &
			"PRINT", "CMC:UTL_SPEC_STRING_PRINT", &
				"UTL_SPEC_STRING_PRINT", "COMMAND", &
			"", &
		"SYSTEM", "SYSTEM", "UTL_MENU", "SYSTEM", &
			"INSTALL", "CMC:UT_SPEC_MENUMAINT", &
				"UT_SPEC_MENUMAINT", "COMMAND", &
			"LIST", "CMC:UTL_REPORT/CUT001", &
				"UTL_RPRT_PRINTMENU", "COMMAND", &
			"", &
		"TIMEOUT", "CMC:UTL_MAST_TIMEOUT", &
				"UTL_MAST_TIMEOUT", "COMMAND", &
			"", &
		""

	MACRO% = 0%
	READ COMMAND$

	WHILE COMMAND$ <> ""
		MACRO% = MACRO% + 1%
		MCL(MACRO%)::COMMAND = COMMAND$

		READ CMD$,PRG$,HLP$
		MCL(MACRO%)::CMD(0%) = CMD$
		MCL(MACRO%)::PRG(0%) = PRG$
		MCL(MACRO%)::HLP(0%) = HLP$

		J% = 1%
		READ COMMAND$
		WHILE COMMAND$ <> ""
			READ CMD$,PRG$,HLP$

			MCL(MACRO%)::QUALIF(J%) = COMMAND$
			MCL(MACRO%)::CMD(J%) = CMD$
			MCL(MACRO%)::PRG(J%) = PRG$
			MCL(MACRO%)::HLP(J%) = HLP$
			J% = J% + 1%
			READ COMMAND$
		NEXT
		MCL(MACRO%)::QUALIF(J%) = ""
		READ COMMAND$
	NEXT

	CMCMACRO% = MACRO%

	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_MACROS.CRE"
	USE
		FILENAME$ = "UTL_MACROS"
		CONTINUE HelpError
	END WHEN

	!
	! Add user defined macro
	!
	GOSUB ReadMacros

	%PAGE

 MainMenu:
300	!***************************************************************
	! DISPLAY OPTIONS
	!***************************************************************

	GOTO 430 IF USER_COMMANDS$ <> ""

310	!
	! Display path
	!
	IF CURLEVEL% = 0%
	THEN
		CPATH$ = "System Menu"
	ELSE
		CPATH$ = ""
		CPATH$ = CPATH$ + TRM$(MENU_LEVEL(I%)::SYSTEM) + " " &
			FOR I% = 1% TO CURLEVEL%
		CPATH$ = CPATH$ + "MENU"
	END IF

	!
	! PRINT BANNER
	!
	SMG_STATUS% = SMG$BEGIN_PASTEBOARD_UPDATE(SCOPE::SMG_PBID)

	SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_SCREEN_DATA%, 5%, 1%, 20%, 1%)

	!
	! Handle being an interrupt menu
	!
	IF READ_OWNERPID(0%)
	THEN
		!
		! Calculate depth of spawn (Max 50)
		!
		SPAWN_COUNT% = 0%
		SPAWN_LEVEL% = READ_OWNERPID(0%)
		WHILE (SPAWN_LEVEL% <> 0%) AND (SPAWN_COUNT% < 50%)
			SPAWN_COUNT% = SPAWN_COUNT% + 1%
			SPAWN_LEVEL% = READ_OWNERPID(SPAWN_LEVEL%)
		NEXT

		!
		! Display spawn title
		!
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Interrupt Menu (Level " + NUM1$(SPAWN_COUNT%) + ")", &
			1%, 20%, 0%, SMG$M_BOLD OR SMG$M_BLINK)
	ELSE
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			"Menu System", 1%, 20%)
	END IF

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		TRM$(SCOPE::PRG_COMPANY), 2%, 20%)
	SMG_STATUS% = SMG$DRAW_LINE(SMG_SCREEN_DATA%, 4%, 1%, 4%, 80%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, CPATH$, 5%, 1%)

	SMG_STATUS% = SMG$DRAW_RECTANGLE(SMG_SCREEN_DATA%, 1%, 1%, &
		3%, LEN(BANNER$) * 2% + 1%)

	FOR I% = 1% TO LEN(BANNER$)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			MID(BANNER$, I%, 1%), 2%, (I% * 2%))
		SMG_STATUS% = SMG$DRAW_LINE(SMG_SCREEN_DATA%, &
			1%, I% * 2% + 1%, &
			3%, I% * 2% + 1%)
	NEXT I%


	!
	! Initilization for arrow on screen
	!
	LINES% = 1%
	SYSTEMP% = 0%
	CURITM% = MENU_LEVEL(CURLEVEL%)::CURLINE

	!*******************************************************************
	! Load up menu to display
	!
	TEMP_POINTER% = MENU_LEVEL(CURLEVEL%)::LEVEL

320	!
	! Figure out how many characters it takes to make command unique
	!
	TEMP% = MENU(TEMP_POINTER%)::OPTLEN
	OPTION$ = MENU(TEMP_POINTER%)::OPT
	SCREEN_RFA%(LINES%) = TEMP_POINTER%

	!
	! Print the menu option
	!
	ENCODER::E.START = 1%
	ENCODER::E.LENTH = TEMP%
	ENCODER::E.ATTR  = SMG$M_BOLD
	ENCODER::E.TLEN  = 7%

	IF (MENU(TEMP_POINTER%)::OPTV = OPTV_MENU) OR &
		(MENU(TEMP_POINTER%)::OPTV = OPTV_INCLUDE)
	THEN
		SEP_CHAR$ = " > "
	ELSE
		SEP_CHAR$ = " - "
	END IF

	MENU_ARRAY$(LINES%) = LEFT(OPTION$ + "      ", 6%) + SEP_CHAR$ + &
		MENU(TEMP_POINTER%)::DESCR + ENCODER::ENCODER

	LINES% = LINES% + 1%

380	IF MENU(TEMP_POINTER%)::EFLAG = 0%
	THEN
		TEMP_POINTER% = TEMP_POINTER% + 1%
		GOTO 320
	END IF

390	!*******************************************************************
	! Display the menu
	!
	SMG_SCROLL::BOT_ARRAY, SMG_SCROLL::END_ELEMENT = LINES% - 1%
	SMG_SCROLL::FIND_LINE = 1%

	V% = DSPL_SCROLL(SMG_SCROLL, MENU_ARRAY$(), 0%, "PAINT")

	SMG_SCROLL::FIND_LINE = CURITM%
	V% = DSPL_SCROLL(SMG_SCROLL, MENU_ARRAY$(), 0%, "FIND")

	CURITM% = SMG_SCROLL::CUR_LINE

	!
	! Remove report settings screen
	!
	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(UTL_REPORTX::WINDOW) &
		IF ERASE_REPORT_SCREEN%

	ERASE_REPORT_SCREEN% = 0%

	!
	! Store time out in temp variable
	!
	TEMP_TIMEOUT% = SCOPE::SCOPE_TIMEOUT

	SMG_STATUS% = SMG$END_PASTEBOARD_UPDATE(SCOPE::SMG_PBID)

	%PAGE

 AskForCommand:
400	!***************************************************************
	! WAIT FOR USER TO ENTER OPTIONS
	!***************************************************************

	SMG_STATUS% = LIB$DATE_TIME(TEMP$)

	SECOND% = VAL%(MID(TEMP$, 19%, 2%))

	SCOPE::SCOPE_TIMEOUT = 61% - SECOND%

	SMG_STATUS% = SMG$BEGIN_PASTEBOARD_UPDATE(SCOPE::SMG_PBID)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, DATE$(0%), 1%, 72%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, TIME$(0%), 2%, 73%)

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, &
		"Menu Command Level> ", 1%, 1%) &
		UNLESS SYSTEMP%

	SMG_STATUS% = SMG$END_PASTEBOARD_UPDATE(SCOPE::SMG_PBID)

	SYSTEMP% = -1%

 InputString:
	!
	! Input string
	!
	S2$ = SPACE$(50%)
	CXPOS% = -1%

 InputString2:
	SCOPE::MACROFLAG = 2%
	RTEMP% = ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, &
		1%, 21%, S2$, CXPOS%, 8% + 16% + 1024% + 4096%)

	USER_COMMANDS$ = EDIT$(S2$, 8% + 16% + 128%)

	!
	! Restore time out in scope variable
	!
	SCOPE::SCOPE_TIMEOUT = TEMP_TIMEOUT%

	!
	! Check for special keys typed
	!
	SELECT RTEMP%

	!
	! Time out (repaint the time clock
	!
	CASE SMG$K_TRM_TIMEOUT
		SMG_STATUS% = SMG$BEGIN_PASTEBOARD_UPDATE(SCOPE::SMG_PBID)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			DATE$(0%), 1%, 72%)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
			TIME$(0%), 2%, 73%)

		SMG_STATUS% = SMG$END_PASTEBOARD_UPDATE(SCOPE::SMG_PBID)

		!
		! Calculate new wait time
		!
		SMG_STATUS% = LIB$DATE_TIME(TEMP$)
		SECOND% = VAL%(MID(TEMP$, 19%, 2%))
		SCOPE::SCOPE_TIMEOUT = 61% - SECOND%

		GOTO InputString2

	!
	! Control/C
	!
	CASE 3%
		GOTO InputString	! (Ignored)

	!
	! Next Screen, Downarrow
	!
	CASE SMG$K_TRM_NEXT_SCREEN, SMG$K_TRM_DOWN, SMG$K_TRM_UP, &
		SMG$K_TRM_PREV_SCREEN, SMG$K_TRM_F18,  SMG$K_TRM_F19

		V% = DSPL_SCROLL(SMG_SCROLL, MENU_ARRAY$(), &
			SCOPE::SCOPE_EXIT, "")

		CURITM% = SMG_SCROLL::CUR_LINE
		DO_ITEM% = SCREEN_RFA%(CURITM%)
		GOSUB SetReturn
		GOTO 390 IF INVRFA%
		GOTO InputString

	!
	! Exit key typed
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

		S$ = MACRO_SIGN$ + "EXIT"
		USER_COMMANDS$ = ""
		GOTO 500

	!
	! Gold/Exit key typed
	!
	CASE -SMG$K_TRM_F10, -SMG$K_TRM_CTRLZ

		S$ = MACRO_SIGN$ + "QUIT"
		USER_COMMANDS$ = ""
		GOTO 500

	!
	! Do key
	!
	CASE SMG$K_TRM_DO, SMG$K_TRM_CR, SMG$K_TRM_SELECT

		!
		! Do the option that the pointer points to
		! Unless the user has typed something to do.
		!
		IF USER_COMMANDS$ = ""
		THEN
			DO_ITEM% = SCREEN_RFA%(CURITM%)
			GOTO 600
		END IF

	!
	! Interupt
	!
	CASE SMG$K_TRM_F6, SMG$K_TRM_F20
		IF CURLEVEL% = 0%
		THEN
			DO_ITEM% = MENU_LEVEL(CURLEVEL%)::CURLINE + &
				MENU_LEVEL(CURLEVEL%)::LEVEL - 1%
		ELSE
			DO_ITEM% = MENU_LEVEL(CURLEVEL% - 1%)::CURLINE + &
				MENU_LEVEL(CURLEVEL% - 1%)::LEVEL - 1%
		END IF
		GOSUB SetHelpItem
		CALL MENU_3INTERRUPT(SCOPE)
		GOTO InputString

	!
	! Good keys
	!
	CASE 0%, 10%, 12%

	!
	! Bad Keys
	!
	CASE ELSE
		DO_ITEM% = SCREEN_RFA%(CURITM%)
		GOSUB SetHelpItem

		RTEMP% = ENTR_4SPECIALKEYS(SCOPE, &
			SCOPE::SMG_MESSAGE BY VALUE, &
			0% BY VALUE, RTEMP% BY VALUE)

		CALL ENTR_3BADKEY(SCOPE, RTEMP%) IF RTEMP%

		GOTO InputString2 IF RTEMP% = 0%

		GOTO InputString

	END SELECT

430	GOTO MainMenu IF USER_COMMANDS$ = ""

	I% = INSTR(1%, USER_COMMANDS$ + " ", " ")
	S$ = LEFT(USER_COMMANDS$, I% - 1%)
	USER_COMMANDS$ = RIGHT(USER_COMMANDS$, I% + 1%)

	%PAGE

500	!***************************************************************
	! Check MCL Command
	!***************************************************************

	!
	! Search for options under a system
	!
	DO_ITEM% = MENU_LEVEL(CURLEVEL%)::LEVEL

	IF LEFT(S$, 1%) = ">"
	THEN
		S$ = RIGHT(S$, 2%)
		DO_ARROW% = -1%
	ELSE
		DO_ARROW% = 0%
	END IF

	GOTO MainMenu IF EDIT$(S$, -1%) = ""

520	!
	! Run program if option matches
	!
	IF LEFT(MENU(DO_ITEM%)::OPT, LEN(S$)) = S$
	THEN
		GOTO 600
	END IF

	IF MENU(DO_ITEM%)::EFLAG = 0%
	THEN
		DO_ITEM% = DO_ITEM% + 1%
		GOTO 520
	END IF

580	!
	! Didn't match
	!
	MIDX%, MATCH%, LEV% = 0%

	!
	! Check if input can be MCL command
	!
	IF LEFT(S$, 1%) <> MACRO_SIGN$
	THEN
		CALL HELP_34MESSAGE(SCOPE, "unrecognized command verb " + &
			S$, "W", "UTL_MENU", "", "IVVERB")
		USER_COMMANDS$ = ""
		GOTO MainMenu
	END IF

	S$ = RIGHT(S$, 2%)

	SUBCOMMAND$(0%) = EDIT$(S$, 8% + 128%) + "/"
	SLASH% = INSTR(1%, SUBCOMMAND$(0%), "/")

	WHILE SLASH% AND LEV%<4%
		LEV% = LEV% + 1%
		SUBCOMMAND$(LEV%) = LEFT(SUBCOMMAND$(0%), SLASH% - 1%)
		SUBCOMMAND$(0%)   = RIGHT(SUBCOMMAND$(0%), SLASH% + 1%)
		SLASH% = INSTR(1%, SUBCOMMAND$(0%), "/")
	NEXT

	IF LEV%>2%
	THEN
		CALL HELP_34MESSAGE(SCOPE, "too many qualifiers", &
			"W", "UTL_MENU", "", "TOOMQUAL")
	!++
	! Warning:TOOMQUAL
	!	^*Too Many Qualifiers\*
	!	.b
	!	.lm +5
	!	^*Explanation:\*
	!	.b
	!	More than one qualifier has been specified.
	!	.b
	!	^*User Action:\*
	!	.b
	!	Specify no more than one qualifier and
	!	re-enter the command.
	!	.lm -5
	!
	! Index:
	!
	!--
			USER_COMMANDS$ = ""
			GOTO MainMenu
	END IF

	I%,J% = 1%
	WHILE (MCL(J%)::COMMAND <> "")
		IF LEFT(MCL(J%)::COMMAND, LEN(SUBCOMMAND$(I%))) = &
			SUBCOMMAND$(I%)
		THEN
			MATCH% = MATCH% + 1%
			MIDX% = J%
		END IF
		J% = J% + 1%
	NEXT

	SELECT MATCH%
	CASE 0%
		!
		! Not found
		!
		CALL HELP_34MESSAGE(SCOPE, "unrecognized command verb " + &
			SUBCOMMAND$(I%), "W", "UTL_MENU", "", "IVVERB")
	!++
	! Warning:IVVERB
	!	^*Unrecognized Command Verb\*
	!	.b
	!	.lm +5
	!	^*Explanation:\*
	!	.b
	!	The first word of the command is not a valid MCL command
	!	or a symbol name equated with the command or menu prompt.
	!	.b
	!	^*User Action:\*
	!	.b
	!	Check the spelling of the command name or symbol and
	!	re-enter the command.
	!	.b
	!	^*Note:\*  The MCL command or user defined macro must start with a
	!	"/" (slash) character.  For example /QUIT, /LOGOUT etc.
	!	.lm -5
	!
	! Index:
	!
	!--
		USER_COMMANDS$ = ""
		GOTO MainMenu

	CASE 1%
		IF LEV% = 1%
		THEN
			CMD$ = TRM$(MCL(MIDX%)::CMD(0%))
			GOTO CMD IF CMD$ <> ""

			CALL HELP_34MESSAGE(SCOPE, &
				"qualifier name is missing for " + &
				MCL(MIDX%)::COMMAND, &
				"W", "UTL_MENU", "", "NOKEYW")
	!++
	! Warning:NOKEYW
	!	^*Qualifier Name is Missing\*
	!	.b
	!	.lm +5
	!	^*Explanation:\*
	!	.b
	!	A slash character (/) is present in a command but is not
	!	followed by a qualifier keyword name.
	!	.b
	!	^*User Action:\*
	!	.b
	!	Re-enter the command specifying the qualifier or removing
	!	the slash.
	!	.lm -5
	!
	! Index:
	!
	!--

			USER_COMMANDS$ = ""
			GOTO MainMenu
		END IF

	CASE ELSE
		!
		! ambiguous command
		!
		CALL HELP_34MESSAGE(SCOPE, "ambiguous command verb " + &
			SUBCOMMAND$(I%), "W", "UTL_MENU", "", "ABVERB")
	!++
	! Warning:ABVERB
	!	^*Ambiguous Command Verb\*
	!	.b
	!	.lm +5
	!	^*Explanation:\*
	!	.b
	!	Too few characters were used to truncate a command name to
	!	make the command name unique.
	!	.b
	!	^*User Action:\*
	!	.b
	!	Re-enter the command. Specify at least four characters
	!	of the command name.
	!	.lm -5
	!
	! Index:
	!
	!--
		USER_COMMANDS$ = ""
		GOTO MainMenu

	END SELECT

	!
	! Check Qualifier
	!
	MATCH% = 0%

	I% = I% + 1%
	J% = 1%
	WHILE (MCL(MIDX%)::QUALIF(J%) <> "")
		IF LEFT(MCL(MIDX%)::QUALIF(J%), LEN(SUBCOMMAND$(I%))) = &
			SUBCOMMAND$(I%)
		THEN
			MATCH% = MATCH% + 1%
			CMD$ = MCL(MIDX%)::CMD(J%)
		END IF
		J% = J% + 1%
	NEXT


	SELECT MATCH%

	CASE 0%
		CALL HELP_34MESSAGE(SCOPE, &
			"unrecognized qualifier " + SUBCOMMAND$(I%), &
			"W", "UTL_MENU", "", "IVQUAL")
	!++
	! Warning:IVQUAL
	!	^*Unrecognized Qualifier \*
	!	.b
	!	.lm +5
	!	^*Explanation:\*
	!	.b
	!	The qualifier is spelled incorrectly or is improperly
	!	placed in the command line.
	!	.b
	!	^*User Action:\*
	!	.b
	!	Check validity, spelling, and placement and correct
	!	the command line.
	!	.lm -5
	!
	! Index:
	!
	!--

		USER_COMMANDS$ = ""
		GOTO MainMenu

	CASE >1%
		CALL HELP_34MESSAGE(SCOPE, &
			"ambiguous qualifier  or keyword " + SUBCOMMAND$(I%), &
			"W", "UTL_MENU", "", "ABKEYW")

	!++
	! Warning:ABKEYW
	!	^*Ambiguous Qualifier or Keyword\*
	!	.b
	!	.lm +5
	!	^*Explanation:\*
	!	.b
	!	Too few characters were used to truncate a qualifier or
	!	keyword name to make the qualifier or keyword name unique.
	!	.b
	!	^*User Action:\*
	!	.b
	!	Re-enter the command. Specify at least four characters of the
	!	qualifier or keyword name.
	!	.lm -5
	!
	! Index:
	!
	!--

		USER_COMMANDS$ = ""
		GOTO MainMenu
	END SELECT

 CMD:
	SELECT CMD$

	CASE "BATCH/MONITOR"
		SMG_STATUS% = SMG$UNPASTE_VIRTUAL_DISPLAY( &
			SMG_SCREEN_DATA%, SCOPE::SMG_PBID)

		TEMP% = UTL_FUNC_MTRBATCH

		SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY( &
			SMG_SCREEN_DATA%, SCOPE::SMG_PBID, 1%, 1%)

		SYSTEMP% = 0%
		USER_COMMANDS$ = ""
		GOTO MainMenu

	CASE "CALENDAR"

		DATUM$ = DATE_STOREDATE(USER_COMMANDS$)
		DATUM$ = DATE_TODAY IF TRM$(DATUM$) = ""

		IF DATUM$ <> XLATE(DATUM$, STRING$(48%, 0%) + "0123456789") &
			OR DATUM$ <> DATE_INVDCODE(DATE_DAYCODE(DATUM$))
		THEN
			CALL HELP_34MESSAGE(SCOPE, "invalid date", &
				"E", "UTL_MENU", USER_COMMANDS$, "INVDATE")

		ELSE
			TEMP$ = DATE_3SELECT(SCOPE, DATUM$)
		END IF

		SYSTEMP% = 0%
		USER_COMMANDS$ = ""
		GOTO MainMenu

	CASE "EXIT"

	!++
	! Abstract:EXIT
	!	^*EXIT\*
	!	.p
	!	^*Exit\* terminates the current process command level and returns
	!	control to the next higher level or DCL.
	!	.p
	!	^*Format: EXIT\*
	!	.p
	!	^*Example:\*
	!	.literal
	!	Menu Command Level: /EXIT
	!	.end literal
	!
	! Index:
	!	.x EXIT
	!
	!--

		CURLEVEL% = CURLEVEL% - 1%
		GOTO ExitProgram IF CURLEVEL% = -1%
		GOTO 430

	CASE "HELP"

	!++
	! Abstract:COMMAND
	!	^*HELP\*
	!	.p
	!	Display information concerning use of the MCL commands,
	!	including formats, parameters, and qualifiers.
	!	.p
	!	^*Format: HELP [keyword]\*
	!	.p
	!	^*Example:\*
	!	.literal
	!	Menu Command Level: /HELP MACRO
	!	.end literal
	!
	! Index:
	!	.x HELP
	!--
		SMG_STATUS% = SMG$SAVE_PHYSICAL_SCREEN(SCOPE::SMG_PBID, &
			SMG_SAVE%)

		IF USER_COMMANDS$ = ""
		THEN
			CALL HELP_34MESSAGE(SCOPE, "", "H", "UTL_MENU", &
				"", "HELP")
			GOTO HelpRet
		END IF

		SLASH% = INSTR(1%, USER_COMMANDS$, "/")
		IF SLASH% = 0%
		THEN
			F_MACRO$ = USER_COMMANDS$
			S_MACRO$ = "*"
		ELSE
			F_MACRO$ = LEFT(USER_COMMANDS$, SLASH% - 1%)
			S_MACRO$ = RIGHT(USER_COMMANDS$, SLASH% + 1%)
		END IF

		DISPLAY% = 0%
		TEXT$ = "MCL command"

		FOR I% = 1% TO MACRO%
			IF COMP_STRING(MCL(I%)::COMMAND,F_MACRO$)
			THEN
				GOTO SMacro IF MCL(I%)::CMD(0%) = "" OR &
					SLASH%
				TEXT$ = "user defined macro" IF I% > CMCMACRO%
				CALL HELP_34MESSAGE(SCOPE, TEXT$, "H", &
					MCL(I%)::PRG(0%), "", MCL(I%)::HLP(0%))
				IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F10) OR &
					(SCOPE::SCOPE_EXIT = SMG$K_TRM_CTRLZ)
				THEN
					GOTO HelpRet
				END IF
				DISPLAY% = -1%


 SMacro:			J% = 1%
				WHILE MCL(I%)::QUALIF(J%) <> ""
					IF COMP_STRING(MCL(I%)::QUALIF(J%), &
						S_MACRO$)
					THEN
						TEXT$ = "user defined macro" &
							IF I% > CMCMACRO%
						CALL HELP_34MESSAGE(SCOPE, &
							TEXT$, "H", &
							MCL(I%)::PRG(J%), &
							"",MCL(I%)::HLP(J%))
						IF (SCOPE::SCOPE_EXIT = &
							SMG$K_TRM_F10) OR &
							(SCOPE::SCOPE_EXIT = &
							SMG$K_TRM_CTRLZ)
						THEN
							GOTO HelpRet
						END IF
						DISPLAY% = -1%

					END IF
					J% = J% + 1%
				NEXT
			END IF
		NEXT I%

		IF DISPLAY% = 0%
		THEN
			CALL HELP_34MESSAGE(SCOPE, USER_COMMANDS$, "H", &
				USER_COMMANDS$, "", "UNDEFMACRO")
			USER_COMMANDS$ = ""
		END IF

 HelpRet:
		SMG_STATUS% = SMG$RESTORE_PHYSICAL_SCREEN(SCOPE::SMG_PBID, &
			SMG_SAVE%)
		SYSTEMP% = 0%
		USER_COMMANDS$ = ""
		GOTO MainMenu

	CASE "LOGOUT", "LOGOUT/FULL"

	!++
	! Abstract:LOGOUT
	!	^*LOGOUT\*
	!	.p
	!	^*Logout\* terminates an interactive terminal session.
	!	.p
	!	^*Format: LOGOUT\*
	!	.p
	!	^*Example:\*
	!	.literal
	!	Menu Command Level> /LOGOUT
	!
	!	USER logged out at 20-APR-1990 10:45:12.23
	!	.end literal
	!
	! Index:
	!	.x Logout
	!
	! Option:
	!	UTL_MENU$LOGOUT_FULL
	! Index:
	!	.x LOGOUT
	!--

	!++
	! Abstract:LOGOUT_FULL
	!	^*LOGOUT/FULL\*
	!	.p
	!	^*Logout/Full\* terminates an interactive terminal session with the long
	!	form of the the logout message. The command interpreter
	!	displays a summary of accounting information for the
	!	terminal session.
	!	.p
	!	^*Format: LOGOUT/FULL\*
	!	.p
	!	^*Examples:\*
	!	.literal
	!	Menu Command Level> /LOGOUT/FULL
	!
	!	USER logged out at 20-APR-1990 10:45:12.23
	!	Accounting information:
	!	Buffered I/O count: 15860         Peak working set size: 1024
	!	Direct I/O count: 1648            Peak virtual size 5268
	!	Page faults: 10958                Mounted volumes: 0
	!	Charged CPU Time: 0 00:03:49.26   Elapsed time: 0 01:48:02.63
	!	.end literal
	!
	! Index:
	!	.x LOGOUT/FULL
	!
	!--

		I% = RND * 20.0 + 1.0
		SELECT I%
		CASE 0% TO 15%
			JUNK$ = "have a good "

		CASE 15% TO 19%
			JUNK$ = "enjoy your "

		CASE ELSE
			JUNK$ = "have a great "
		END SELECT

		SELECT TIME(0%)
		CASE 0. TO 43199.
			JUNK1$ = "morning "

		CASE 43200. TO 62999.
			JUNK1$ = "afternoon "

		CASE 63000. TO 89199.
			JUNK1$ = "evening "

		CASE ELSE
			JUNK1$ = "night "
		END SELECT

		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, &
			SMG$M_CURSOR_ON)
		SMG_STATUS% = SMG$DELETE_PASTEBOARD(SCOPE::SMG_PBID)

		PRINT "Thank you, and " + JUNK$ + JUNK1$ + ". . ."
		PRINT LF + LF + LF

		SMG_STATUS% = LIB$DO_COMMAND(CMD$)

	CASE "MACRO"

	!++
	! Abstract:MACRO
	!	^*MACRO\*
	!	.p
	!	^*Macro\* invokes the CMC Macro assembler to assign the menu
	!	path under a user assigned macro name.
	!	.p
	!	^*Format: MACRO macro-name\*
	!	.p
	!	^*Parameter: macro-name\*
	!	.p
	!	One alphanumeric string may have a maximum length of 20 characters.
	!	It can contain one slash character, as long as it not the first character.
	!	.p
	!	^*Examples:\*
	!	.literal
	!	Menu Command Level> /MACRO TRIAL
	!	Menu Command Level> /MACRO EMPL/LIST
	!	.end literal
	!
	! Index:
	!	.x Macro
	!
	! Option:
	!	UTL_MENU$MACRO_DELETE
	!	UTL_RPRT_MACROS$COMMAND
	! Index:
	!	.x MACRO
	!--
		IF USER_COMMANDS$ = ""
		THEN
			CALL HELP_34MESSAGE(SCOPE, &
				"missing argument in MACRO command", &
				"E", "UTL_MENU", "", "MISARG")

	!++
	! Error:MISARG
	!	^*Missing Argument\*
	!	.p
	!	^*Explanation:\*
	!	.p
	!	A required argument is missing for the MCL Command.
	!	For example, "/MACRO/DELETE#XXX" to remove macro "XXX".
	!	.p
	!	^*User Action:\*
	!	.p
	!	Add the missing argument.
	!	For information on MCL commands refer to to the ^~CMC
	!	Software User's Guide and Reference Manual\~.
	!
	! Index:
	!
	!--

			USER_COMMANDS$ = ""
			GOTO MainMenu
		END IF

		SPC% = INSTR(1%, USER_COMMANDS$, " ")
		SLASH% = INSTR(1%, USER_COMMANDS$, "/")
		IF SLASH% = 1% OR SPC% OR LEN(USER_COMMANDS$) > 20%
		THEN
			CALL HELP_34MESSAGE(SCOPE, &
				"invalid macro name " + USER_COMMANDS$, &
				"E", "UTL_MENU", "", "IVMNAME")
	!++
	! Error:IVMNAME
	!	^* Invalid Macro Name\*
	!	.p
	!	^*Explanation:\*
	!	.p
	!	Invalid user defined macro name.
	!	.p
	!	^*User Action:\*
	!	.p
	!	Reenter command with the valid macro name.
	!	For information on macro names refer to to the ^~CMC
	!	Software User's Guide and Reference Manual\~.
	!
	! Index:
	!
	!--
			USER_COMMANDS$ = ""
			GOTO MainMenu
		END IF

		SLASH% = INSTR(SLASH% + 1%, USER_COMMANDS$, "/")

		IF SLASH%
		THEN
			CALL HELP_34MESSAGE(SCOPE, "too many qualifiers", &
				"W", "UTL_MENU", "", "TOOMQUAL")
			USER_COMMANDS$ = ""
			GOTO MainMenu
		END IF

		!
		! Check against existing commands
		!
		FOR I% = 1% TO MACRO%
			IF LEFT(TRM$(MCL(I%)::COMMAND),LEN(USER_COMMANDS$)) = &
				USER_COMMANDS$ &
				OR LEFT(USER_COMMANDS$, &
				LEN(TRM$(MCL(I%)::COMMAND))) = &
				TRM$(MCL(I%)::COMMAND)
			THEN
				IF TRM$(MCL(I%)::COMMAND) <> USER_COMMANDS$
				THEN
					GOTO AbMacro
				ELSE
					GOTO AbMacro IF I% <= CMCMACRO%
				END IF
			END IF
			J% = 1%
			WHILE TRM$(MCL(I%)::QUALIF(J%)) <> ""
				IF LEFT(TRM$(MCL(I%)::COMMAND) + &
					"/" + TRM$(MCL(I%)::QUALIF(J%)), &
					LEN(USER_COMMANDS$)) = USER_COMMANDS$ &
					OR LEFT(USER_COMMANDS$, &
					LEN(TRM$(MCL(I%)::COMMAND) + &
					"/" + TRM$(MCL(I%)::QUALIF(J%)))) = &
					TRM$(MCL(I%)::COMMAND) + "/" + &
					TRM$(MCL(I%)::QUALIF(J%))
				THEN
					GOTO AbMacro
				END IF
				J% = J% + 1%
			NEXT
		NEXT I%

		CPATH$ = ""
		CPATH$ = CPATH$ + TRM$(MENU_LEVEL(I%)::SYSTEM) + " " &
			FOR I% = 1% TO CURLEVEL%

		UTL_MACROS::COMMAND	= USER_COMMANDS$
		UTL_MACROS::DESCRIPTION	= MENU(SCREEN_RFA%(CURITM%))::DESCR
		UTL_MACROS::CMD		= &
			EDIT$(CPATH$ + " " + &
			MENU(SCREEN_RFA%(CURITM%))::OPT, 8%)

590		WHEN ERROR IN
			PUT #UTL_MACROS.CH%
		USE
			CONTINUE AbMacro IF ERR = 134%
			FILENAME$ = "UTL_MACROS"
			CONTINUE HelpError
		END WHEN
		GOSUB ReadMacros

		CALL HELP_34MESSAGE(SCOPE, &
			"macro " + USER_COMMANDS$ + " has been defined", &
			"S", "UTL_MENU", "", "MACRODEF")

	!++
	! Success:MACRODEF
	!	^*Macro Defined\*
	!	.b
	!	.lm +5
	!	New user defined macro has been successfuly defined.
	!	.b
	!	By typing MACRO/LIST at the MCL level all user defined macros
	!	will be displayed.
	!	.lm -5
	!
	! Index:
	!
	!--

		USER_COMMANDS$ = ""
		GOTO MainMenu

	CASE "MACRO/DELETE"

	!++
	! Abstract:MACRO_DELETE
	!	^*MACRO/DELETE\*
	!	.p
	!	^*Macro/Delete\* invokes the CMC Macro assembler to deassign the user
	!	defined macro name.
	!	.p
	!	^*Format: MACRO/DELETE macro-name\*
	!	.p
	!	^*Parameter: macro-name\*
	!	.p
	!	Full user defined macro name (no abbriviations).
	!	.p
	!	^*Examples:\*
	!	.literal
	!	Menu Command Level> /MACRO/DELETE TRIAL
	!	Menu Command Level> /MACRO/DELETE EMPL/LIST
	!	.end literal
	!
	! Index:
	!	.x MACRO/DELETE
	!
	!--
		IF USER_COMMANDS$ = ""
		THEN
			CALL HELP_34MESSAGE(SCOPE, &
				"missing argument in MACRO/DELETE command", &
				"E", "UTL_MENU", "", "MISARG")
			USER_COMMANDS$ = ""
			GOTO MainMenu
		ELSE

592		WHEN ERROR IN
			GET #UTL_MACROS.CH%, &
				KEY #0% EQ USER_COMMANDS$ + &
				SPACE$(20% - LEN(USER_COMMANDS$))
			DELETE #UTL_MACROS.CH%
		USE
			CONTINUE CommNoExist IF ERR = 155% OR ERR = 11%
			FILENAME$ = "UTL_MACROS"
			CONTINUE HelpError
		END WHEN

		GOSUB ReadMacros

		CALL HELP_34MESSAGE(SCOPE, "macro " + USER_COMMANDS$ + &
			" has been deassigned", &
			"S", "UTL_MENU", "", "MACRODEASS")

	!++
	! Success:MACRODEASS
	!	^*Macro Deassigned\*
	!	.b
	!	.lm +5
	!	User defined macro has been de-assigned.
	!	.b
	!	By typing MACRO/LIST at the MCL level all user defined macros
	!	may be displayed.
	!	.lm -5
	!
	! Index:
	!
	!--

		USER_COMMANDS$ = ""
		GOTO MainMenu

	CASE "QUIT"

	!++
	! Abstract:QUIT
	!	^*QUIT\*
	!	.p
	!	^*Quit\* terminates the current process and returns control
	!	to the DCL. Type ^*MENU\* to return back from DCL
	!	to the CMC Menu Command Level.
	!	.p
	!	^*Format: QUIT\*
	!	.p
	!	^*Example:\*
	!	.literal
	!	Menu Command Level> /QUIT
	!	.end literal
	!
	! Index:
	!	.x QUIT
	!
	!--
		GOTO ExitProgram

	CASE "RELEASE"

	!++
	! Abstract:RELEASE
	!	^*RELEASE\*
	!	.p
	!	Display information about release notes for a CMC System.
	!	.p
	!	^*Format: RELEASE [keyword]\*
	!	.p
	!	^*Example:\*
	!	.literal
	!	Menu Command Level: /RELEASE GL
	!	.end literal
	!
	! Index:
	!	.x RELEASE
	!--
		SMG_STATUS% = SMG$SAVE_PHYSICAL_SCREEN(SCOPE::SMG_PBID, &
			SMG_SAVE%)

		IF USER_COMMANDS$ = ""
		THEN
			CALL HELP_34MESSAGE(SCOPE, "", "H", "", "", &
				"APLUS" + REL_VERSION$)
		ELSE
			CALL HELP_34MESSAGE(SCOPE, "", "H", &
				USER_COMMANDS$, "", &
				"CMC" + USER_COMMANDS$ + REL_VERSION$)
		END IF

		SMG_STATUS% = SMG$RESTORE_PHYSICAL_SCREEN(SCOPE::SMG_PBID, &
			SMG_SAVE%)
		SYSTEMP% = 0%
		USER_COMMANDS$ = ""
		GOTO MainMenu

	CASE "SYSTEM"

	!++
	! Abstract:SYSTEM
	!	^*SYSTEM\*
	!	.p
	!	^*System\* terminates the current process command level and returns
	!	control to the top menu level (system level).
	!	.p
	!	^*Format: SYSTEM\*
	!	.p
	!	^*Examples:\*
	!	.literal
	!	Menu Command Level> /SYSTEM
	!	.end literal
	!
	! Index:
	!	.x SYSTEM
	!
	! Option:
	!	UT_SPEC_MENUMAINT$COMMAND
	!	UTL_RPRT_PRINTMENU$COMMAND
	! Index:
	!	.x SYSTEM
	!--
		CURLEVEL% = 0%
		GOTO 430

	CASE ELSE
		!
		! test for macro
		!
		!CALL HELP_34MESSAGE(SCOPE, &
		!	"missing parameter in "+S2$, &
		!	"F",PROGRAM, "", "MISSPAR")

	!++
	! FatalError:MISSPAR
	!
	!	^*Missing Parameter\*
	!	.p
	!	^*Explanation:\*
	!	.p
	!	The MCL command doesn't have a parameter.
	!	.p
	!	^*User Action:\*
	!	.p
	!	Add the missing parameter. Refer to the ^~ CMC
	!	Software User's Guide and Reference Manual \~ .
	!--

	END SELECT

	IF MIDX% > CMCMACRO%
	THEN
		USER_COMMANDS$ = MACRO_SIGN$ + "SYSTEM " + EDIT$(CMD$, 16%)
		GOTO 430
	END IF

	DO_ITEM% = SCREEN_RFA%(CURITM%)
	IMAGENAME$ = CMD$
	GOTO ChainTo

600	!***************************************************************
	! CHAIN TO PROGRAM
	!***************************************************************

	GOSUB SetReturn

	IF DO_ARROW%
	THEN
		MENU_LEVEL(CURLEVEL%)::CURLINE, CURITM% = &
			DO_ITEM% - MENU_LEVEL(CURLEVEL%)::LEVEL + 1%
		DO_ARROW% = 0%
		GOTO MainMenu
	END IF

	!
	! Check expiration date
	!
	IF INSTR(1%, MENU(DO_ITEM%)::SVALUE, "!")
	THEN
		LICENSE$ = EDIT$(RIGHT(MENU(DO_ITEM%)::SVALUE, &
			INSTR(1%, MENU(DO_ITEM%)::SVALUE, "!") + 1%), -1%)

		SNUMBER% = FUNC_BANNERCODE(RIGHT(LICENSE$, 9%))
		EDATE$ = MID(LICENSE$, 9%, 8%)

		IF NUM1$(SNUMBER%) <> LEFT(LICENSE$, 8%) OR &
			(EDATE$<>"00000000" AND EDATE$ < DATE_TODAY)
		THEN
			!
			! today is a good day to drop dead
			!
			CALL HELP_34MESSAGE(SCOPE, &
				"license has expired for " + &
				MENU(DO_ITEM%)::OPT, "W", "UTL_MENU", &
				"", "LICEXP")
			GOTO AskForCommand
		END IF
	END IF

	MENU_LEVEL(CURLEVEL%)::CURLINE = &
		DO_ITEM% - MENU_LEVEL(CURLEVEL%)::LEVEL + 1%
	IMAGENAME$ = MENU(DO_ITEM%)::SVALUE

	IF IMAGENAME$ = "" OR &
		MENU_LEVEL(CURLEVEL%)::LEVEL = MENU(DO_ITEM%)::OVALUE &
		AND MENU(DO_ITEM%)::OPTV = OPTV_MENU
	THEN
		CALL HELP_34MESSAGE(SCOPE, &
			"unexpected menu level termination ", &
			"W", "UTL_MENU", "", "ENDLEVEL")
		GOTO AskForCommand
	END IF

	SELECT MENU(DO_ITEM%)::OPTV

	!
	! Switch menu level
	!
	CASE OPTV_MENU

		CURLEVEL% = CURLEVEL% + 1%
		MENU_LEVEL(CURLEVEL%)::SYSTEM	= MENU(DO_ITEM%)::OPT
		MENU_LEVEL(CURLEVEL%)::LEVEL	= MENU(DO_ITEM%)::OVALUE
		MENU_LEVEL(CURLEVEL%)::CURLINE	= 1%
		GOTO 430

	!
	! Include file
	!
	CASE OPTV_INCLUDE

		GOSUB LoadInclude
		GOTO 600

	END SELECT

 ChainTo:
	CORE_COMMON$ = ""
	I% = INSTR(1%, IMAGENAME$, "/C")
	IF I%
	THEN
		I1% = INSTR(I% + 1%, IMAGENAME$, "/")
		I1% = LEN(IMAGENAME$) + 1% IF I1% = 0%
		CORE_COMMON$ = MID(IMAGENAME$, I% + 2%, I1% - (I% + 2%))
		IMAGENAME$ = LEFT(IMAGENAME$, I% - 1%) + RIGHT(IMAGENAME$, I1%)
	END IF

620	GOTO 800 &
		IF CORE_COMMON$ = "" OR &
		INSTR(1%, IMAGENAME$, "UTL_REPORT") = 0%

650	!
	! Open the print work file
	!
	ERASE_REPORT_SCREEN% = -1%

	CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)

660	XLOOP% = XLOOP% + 1%
	TEMPFILE$ = "PRNT" + JOBNUM$ + "_" + NUM1$(XLOOP%) + ".TMP"
	WHEN ERROR IN
		OPEN TEMPFILE$ FOR INPUT AS FILE PRNT.CH%
		CLOSE PRNT.CH%
	USE
		CONTINUE 670 IF ERR = 5%
		CONTINUE 660
	END WHEN

	GOTO 660

670	CLOSE PRNT.CH%
	WHEN ERROR IN
		OPEN TEMPFILE$ FOR OUTPUT AS FILE PRNT.CH%
	USE
		CONTINUE 660 IF ERR = 154%
		FILENAME$ = TEMPFILE$
		CONTINUE HelpError
	END WHEN

	SYS_STATUS% = LIB$SET_SYMBOL("CMC$REPORT", TEMPFILE$,)

	IF (SYS_STATUS% AND 1%) = 0%
	THEN
		CALL HELP_34MESSAGE(SCOPE, "unable to set symbol.", &
			"F", "UTL_MENU", "", NUM1$(SYS_STATUS%))
		GOTO ExitProgram
	END IF

	%PAGE

700	!******************************************************************
	! If program is called with a report number given, then go into
	! the report settings menu.
	!******************************************************************

	!
	! Get report settings from report file
	!
	CORE_COMMON$ = EDIT$(CORE_COMMON$, -1%)
	CORE_COMMON$ = LEFT(CORE_COMMON$ + "      ", 6%)

	LOCK_FLAG% = -1%
	USER_REPORT% = 0%

	WHEN ERROR IN
		GET #UTL_REPORT.CH%, KEY #0% EQ CORE_COMMON$
	USE
		CONTINUE 710 IF ERR = 155%
		CONTINUE 705 IF ERR = 154%

		FILENAME$ = "UTL_REPORT"
		CONTINUE HelpError
	END WHEN
	UTL_REPORT_SYS = UTL_REPORT

	USER_REPORT% = -1%
	LOCK_FLAG% = -1%

	GOTO 710

705	GET #UTL_REPORT.CH%, KEY #0% EQ CORE_COMMON$, REGARDLESS
	UTL_REPORT_SYS = UTL_REPORT

	USER_REPORT% = -1%
	LOCK_FLAG% = 0%

710	IF UTL_SYSREP.CH% = 0%
	THEN
		!
		! Open system file
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_SYSREP.OPN"
		USE
			FILENAME$ = "UTL_SYSREP"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Get report from system report file
	!
	WHEN ERROR IN
		GET #UTL_SYSREP.CH%, KEY #0% EQ CORE_COMMON$, REGARDLESS
	USE
		CONTINUE 715 IF ERR = 155%
		FILENAME$ = "UTL_SYSREP"
		CONTINUE HelpError
	END WHEN

712	!
	! Put or update record into report file
	!
	IF USER_REPORT%
	THEN
		FOR I% = 0% TO 9%
			UTL_REPORT::OPTDEF(I%)	= UTL_REPORT_SYS::OPTDEF(I%)
			UTL_REPORT::ITEMGROUP(I%)=UTL_REPORT_SYS::ITEMGROUP(I%)
			UTL_REPORT::ITEM(I%)	= UTL_REPORT_SYS::ITEM(I%)
		NEXT I%
		UTL_REPORT::DEFOUT	= UTL_REPORT_SYS::DEFOUT
		UTL_REPORT::PRINTTYPE	= UTL_REPORT_SYS::PRINTTYPE
		UTL_REPORT::SPOOLFORM	= UTL_REPORT_SYS::SPOOLFORM
		UTL_REPORT::LASTRUNDATE	= UTL_REPORT_SYS::LASTRUNDATE
		UTL_REPORT::LASTRUNTIME	= UTL_REPORT_SYS::LASTRUNTIME
		UTL_REPORT::BASERUNDATE	= UTL_REPORT_SYS::BASERUNDATE

		WHEN ERROR IN
			UPDATE #UTL_REPORT.CH% IF LOCK_FLAG%
		USE
			FILENAME$ = "UTL_REPORT"
			CONTINUE HelpError
		END WHEN
	ELSE
		WHEN ERROR IN
			PUT #UTL_REPORT.CH% IF LOCK_FLAG%
		USE
			FILENAME$ = "UTL_REPORT"
			CONTINUE HelpError
		END WHEN
	END IF

	IF LOCK_FLAG%
	THEN
		WHEN ERROR IN
			GET #UTL_REPORT.CH%, KEY #0% EQ CORE_COMMON$
		USE
			FILENAME$ = "UTL_REPORT"
			CONTINUE HelpError
		END WHEN
	END IF

	GOTO 720

715	!
	! Unable to find report in system file
	!
	CALL HELP_34MESSAGE(SCOPE, "record missing", "E", &
		"UTL_MENU", "", "RECMISS")

	!++
	! Error:RECMISS
	!	^*Report Record Missing\*
	!	.p
	!	^*Explanation\*
	!	.p
	!	Report settings record is missing in the Report Master
	!	file which resides on the CMC: directory.
	!	.p
	!	^*User Action\*
	!	.p
	!	On the MCL level enter /REPORT/SETTINGS, add missing
	!	record and put record in the Report Master file using
	!	Put_master option.
	!--

	SYSTEMP% = 0%
	CALL ENTR_3MESSAGE(SCOPE, "", 1%)
	USER_COMMANDS$ = ""
	GOTO MainMenu

720	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	UTL_REPORTX::WINDOW = 0%

	PRGNAM$ = TRM$(UTL_REPORT::PRODEV) + &
		TRM$(UTL_REPORT::PRONAM)

	!
	! Initilize defaults from report file
	!
	CALL OUTP_INITSTRUCTURE(UTL_REPORT, UTL_REPORTX)

	SCOPE::MACROFLAG = 1%
	!
	! Ask user to change settings
	!
	CALL OUTP_SETTINGS(UTL_REPORT, UTL_REPORTX, UTL_REPORT.CH%, "", "")

	UNLOCK #UTL_REPORT.CH%

	!
	! Un-normal abort, exit, etc.
	!
	IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F10) OR &
		(SCOPE::SCOPE_EXIT = SMG$K_TRM_CTRLC) OR &
		(SCOPE::SCOPE_EXIT = SMG$K_TRM_CTRLZ) OR &
		(SCOPE::SCOPE_EXIT = SMG$K_TRM_F8) OR &
		SCOPE::MACROFLAG = 3%
	THEN
		GOSUB KillTempPrint

		USER_COMMANDS$ = ""
		CALL LIB$GET_SYMBOL("CMC$COMMAND" BY DESC, &
			USER_COMMANDS$ BY DESC,,) &
			IF SCOPE::MACROFLAG = 3%

		SYSTEMP% = 0%
		SCOPE::MACROFLAG = 2%

		SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(UTL_REPORTX::WINDOW, &
			SCOPE::SMG_PBID)

		GOTO MainMenu
	END IF

	!
	! Set detach flag
	!
	NODETACH% = UTL_REPORTX::DETACH

	!
	! Write the data out to the ascii file
	!
	CALL OUTP_3WRITESTRUCTURE(UTL_REPORTX, PRNT.CH%, PRINTX)

730	!***************************************************************
	! Chain to print program
	!***************************************************************

	CLOSE PRNT.CH%

740	!
	! Test to see if detaching is allowed
	!
	GOTO 750 IF NODETACH%

	!
	! Should it detach?
	!
	INP$ = UTL_REPORTX::BACKGROUND

	IF INP$ <> "Y"
	THEN
		INP$ = EDIT$(ENTR_3STRING(SCOPE, SMG_SCREEN_DATA%, &
			"", "Run Background or Normally (B/N*)", "N", &
			4%, "", ""), -1%)

		IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F10) OR &
			(SCOPE::SCOPE_EXIT = SMG$K_TRM_CTRLC) OR &
			(SCOPE::SCOPE_EXIT = SMG$K_TRM_CTRLZ) OR &
			(SCOPE::SCOPE_EXIT = SMG$K_TRM_F8)
		THEN
			CALL ENTR_3MESSAGE(SCOPE, "Aborting", 0%)

			GOSUB KillTempPrint

			SYSTEMP% = 0%

			USER_COMMANDS$ = ""
			GOTO MainMenu
		END IF

		GOTO 750 IF EDIT$(INP$, -1%) <> 'B'
	ELSE
		GOTO 750 IF EDIT$(INP$, -1%) <> "Y"
	END IF

		!
		! Attempt to detach
		!
		CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)

		DLOOP% = -1%

745		!
		! Create the command file
		!
		DLOOP% = DLOOP% + 1%
		DET_TEMPFILE$ = "DET_" + JOBNUM$ + "_" + NUM1$(DLOOP%) + ".TMP"

		WHEN ERROR IN
			OPEN DET_TEMPFILE$ FOR INPUT AS FILE DET.CH%
			CLOSE #DET.CH%
		USE
			IF ERR = 5%
			THEN
				CONTINUE 747
			ELSE
				CONTINUE 745
			END IF
		END WHEN
		GOTO 745

747		OPEN DET_TEMPFILE$ AS FILE DET.CH%

748		PRINT #DET.CH%, "$ SET DEFAULT "; READ_DEFAULT
		PRINT #DET.CH%, "$ DEF SYS$LOGIN "; READ_SYSLOG("SYS$LOGIN")
		PRINT #DET.CH%, "$ CMC$REPORT :== " + TEMPFILE$
		PRINT #DET.CH%, "$ RUN " + PRGNAM$

		CLOSE DET.CH%

		!
		! Append after time is specified
		!
		IF (UTL_REPORTX::AFTERTIME > "000000")
		THEN
			DET_TEMPFILE$ = DET_TEMPFILE$ + "/A-- " + &
				PRNT_TIME(UTL_REPORTX::AFTERTIME, 0%) + ".0"
		END IF

		CALL SUBR_SUBMIT(DET_TEMPFILE$, V%)

		!
		! Exit program
		!
		SYSTEMP% = 0%

		CALL ENTR_3MESSAGE(SCOPE, "", 1%)

		USER_COMMANDS$ = ""
		GOTO MainMenu

750	!
	! Not detaching section
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_SCREEN_DATA%)
	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(UTL_REPORTX::WINDOW)

	GOSUB SetReturn

760	SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, SMG$M_CURSOR_ON)
	CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + PRGNAM$, TEMPFILE$)

	%PAGE

800	!******************************************************************
	! Prepare to chain or crash
	!******************************************************************

840	GOSUB SetReturn

850	!
	! Re-establish cursor
	!
	CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)

	SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, SMG$M_CURSOR_ON)

	SMG_STATUS% = SMG$DELETE_PASTEBOARD(SCOPE::SMG_PBID, 0%)


	IF LEFT(IMAGENAME$, 1%) = "@"
	THEN
		SMG_STATUS% = LIB$DO_COMMAND(IMAGENAME$)
	ELSE
		WHEN ERROR IN
			CHAIN IMAGENAME$
		USE
			CONTINUE
		END WHEN
	END IF

860	!
	! Crash and burn (gracefully)
	!

	!******************************************************************
	! Keyboard open routine
	!******************************************************************

	CALL READ_INITIALIZE

	!
	! Create the data display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(20%, 80%, SMG_SCREEN_DATA%,,,)
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	!
	! Paste the data display
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 1%, 1%)
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	CALL HELP_34MESSAGE(SCOPE, &
		TRM$(IMAGENAME$) + " program is missing", "E", &
		"UTL_MENU", "", "MISPROG")

	!++
	! Error:MISPROG
	!	^*Program is Missing\*
	!	.p
	!	^*Explanation:\*
	!	.p
	!	Executable version of desired program is missing.
	!	.p
	!	^*User Action:\*
	!	.P
	!	Check the program directory.
	!	.p
	!	Reinstall the CMC software.
	!	.p
	!	Write down the name of the missing program and call CMC
	!	(the author of this software package)
	!
	! Index:
	!
	!--

	GOTO 100

	%PAGE

 ExitProgram:
10000	!********************************************************************
	! END PROGRAM
	!********************************************************************

	!
	! Re-establish cursor
	!
	SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, SMG$M_CURSOR_ON)

	SMG_STATUS% = SMG$DELETE_PASTEBOARD(SCOPE::SMG_PBID)

	SYS_STATUS% = LIB$SET_SYMBOL("CMC$MENUPATH", "")

	GOTO 32767

	%PAGE

18100	!******************************************************************
	! Set help values in case they press help
	!******************************************************************
 SetHelpItem:

	IF MENU(DO_ITEM%)::OPTV = OPTV_INCLUDE
	THEN
		GOSUB LoadInclude
	END IF

	SELECT MENU(DO_ITEM%)::OPTV

	CASE OPTV_MENU
		TTYPE$ = "H"

	CASE ELSE
		TTYPE$ = "P"
	END SELECT

	MENU.SYSTEM$ = TRM$(MENU_LEVEL(1%)::SYSTEM)
	MENU.SYSTEM$ = MENU(DO_ITEM%)::OPT IF CURLEVEL% = 0%

	CALL HELP_MENUHELPKEY( &
		TRM$(MENU(DO_ITEM%)::SVALUE),	! Command &
		TTYPE$,			! Type H - help, P - prog &
		MENU.SYSTEM$,		! System name &
		SCOPE::PRG_IDENT,	! Identification &
		SCOPE::PRG_PROGRAM,	! Program name &
		SCOPE::PRG_ITEM,	! Item &
		PRG_DEVICE$,		! Device for program &
		0%)			! Report Device assignment &

18200	RETURN

	%PAGE

 LoadInclude:
18300	!*******************************************************************
	! Load in an include menu pointed to by DO_ITEM
	!*******************************************************************

	WHEN ERROR IN
		OPEN MENU(DO_ITEM%)::SVALUE FOR INPUT AS FILE MENU.CH%, &
			ORGANIZATION SEQUENTIAL, &
			ACCESS READ, &
			ALLOW READ
	USE
		CONTINUE 18395 IF ERR = 5%
		FILENAME$ = "MENU"
		CONTINUE HelpError
	END WHEN

	TEMP% = UT_SUBR_MENULOAD(SCOPE, &
		MENU.CH%, MENUPOINTER%, "", 0%, MENU())

	CLOSE MENU.CH%

	MENU(DO_ITEM%)::OPTV = MENU(TEMP%)::OPTV
	MENU(DO_ITEM%)::SVALUE = MENU(TEMP%)::SVALUE
	MENU(DO_ITEM%)::OVALUE = MENU(TEMP%)::OVALUE

18390	RETURN

18395	CALL HELP_34MESSAGE(SCOPE, "Unable to load menu", &
		"W", "UTL_MENU", "", "UNALOA")
	!++
	! Warning:UNALOA
	!	^*Unable to Load Menu\*
	!	.b
	!	.lm +5
	!	^*Explanation:\*
	!	.b
	!	Your menu is attempting to reference a menu option that
	!	does not exist.
	!	.b
	!	^*User Action:\*
	!	.b
	!	Re-install the option you are trying to access.
	!	.lm -5
	!
	! Index:
	!	.x Licence
	!
	!--
	GOTO 18390


	%PAGE

 SetReturn:
	!*******************************************************************
	! Remember how to get back to where we were.
	!*******************************************************************

	CPATH$ = ""
	CPATH$ = CPATH$ + TRM$(MENU_LEVEL(I%)::SYSTEM) + " " &
		FOR I% = 1% TO CURLEVEL%
	CPATH$ = CPATH$ + ">" + TRM$(MENU(DO_ITEM%)::OPT)

	SYS_STATUS% = LIB$SET_SYMBOL("CMC$MENUPATH", CPATH$) &
		IF EDIT$(CPATH$, -1%) <> ">"

	RETURN

	%PAGE

18800	!******************************************************************
	!-LOG OFF IF JOB IS DETACHED
	!******************************************************************

	SYS_STATUS% = LIB$DO_COMMAND("$LOGOUT")
	GOTO 32767

	%PAGE

 ReadMacros:
	!******************************************************************
	! Read user define macros (if any)
	!******************************************************************
18950	WHEN ERROR IN
		RESET #UTL_MACROS.CH%
	USE
		CONTINUE 18960 IF ERR = 11%
		FILENAME$ = "UTL_MACROS"
		CONTINUE HelpError
	END WHEN

	MACRO% = CMCMACRO%
	TEST_MACRO$ = ""

	WHILE 1%
		WHEN ERROR IN
			GET #UTL_MACROS.CH%, REGARDLESS
		USE
			CONTINUE 18960 IF ERR = 11%
			FILENAME$ = "UTL_MACROS"
			CONTINUE HelpError
		END WHEN
		SLASH% = INSTR(1%, TRM$(UTL_MACROS::COMMAND), "/")

		IF SLASH%
		THEN
			FIRST_MACRO$ = LEFT(TRM$(UTL_MACROS::COMMAND), &
				SLASH% - 1%)
		ELSE
			FIRST_MACRO$ = TRM$(UTL_MACROS::COMMAND)
		END IF

		IF TEST_MACRO$ <> FIRST_MACRO$
		THEN
			J% = 0%
			MACRO% = MACRO% + 1%
			MCL(MACRO%)::COMMAND = FIRST_MACRO$

			IF SLASH%
			THEN
				MCL(MACRO%)::CMD(J%) = ""
				J% = J% + 1%
				MCL(MACRO%)::QUALIF(J%)	= &
					RIGHT(TRM$(UTL_MACROS::COMMAND), &
					SLASH% + 1%)
				MCL(MACRO%)::CMD(J%) = TRM$(UTL_MACROS::CMD)
				MCL(MACRO%)::PRG(J%) = FIRST_MACRO$ + "/" + &
					RIGHT(TRM$(UTL_MACROS::COMMAND), &
					SLASH% + 1%)
				MCL(MACRO%)::HLP(J%) = "USERMACRO"
				MCL(MACRO%)::QUALIF(J% + 1%) = ""
			ELSE
				MCL(MACRO%)::CMD(J%)	= TRM$(UTL_MACROS::CMD)
				MCL(MACRO%)::PRG(J%)	= FIRST_MACRO$
				MCL(MACRO%)::HLP(J%)	= "USERMACRO"
				MCL(MACRO%)::QUALIF(J% + 1%) = ""
			END IF
		ELSE
			J% = J% + 1%
			MCL(MACRO%)::QUALIF(J%)	= &
				RIGHT(TRM$(UTL_MACROS::COMMAND), SLASH% + 1%)
			MCL(MACRO%)::CMD(J%)	= TRM$(UTL_MACROS::CMD)
			MCL(MACRO%)::PRG(J%)	= FIRST_MACRO$
			MCL(MACRO%)::HLP(J%)	= "USERMACRO"
			MCL(MACRO%)::QUALIF(J% + 1%) = ""
		END IF
		TEST_MACRO$ = FIRST_MACRO$
	NEXT

18960	MCL(MACRO% + 1%)::COMMAND = ""
	RETURN

 AbMacro:
	CALL HELP_34MESSAGE(SCOPE, USER_COMMANDS$ + &
		" ambibuous macro", "W", "UTL_MENU", "", "ABMACRO")
	!++
	! Warning:ABMACRO
	!	^*Ambiguous Macro\*
	!	.b
	!	.lm +5
	!	^*Explanation:\*
	!	.b
	!	Defined macro is ambiguous or makes ambiguous existing
	!	user defined macro or a MCL command.
	!	.b
	!	^*User Action:\*
	!	.b
	!	Re-enter the macro with a different name.
	!	.b
	!	Typing MACRO/LIST will display all user defined macros.
	!	For information on MCL commands refer to the ^~ CMC
	!	Software User's Guide and Reference Manual\~.
	!	.lm -5
	!
	! Index:
	!
	!--

	USER_COMMANDS$ = ""
	GOTO MainMenu

 CommNoExist:
	CALL HELP_34MESSAGE(SCOPE, &
		"command " + USER_COMMANDS$ + " not defined", &
		"W", "UTL_MENU", "", "COMMNOTDEF")
	!++
	! Warning:COMMNOTDEF
	!	^*Command Not Defined\*
	!	.b
	!	.lm +5
	!	^*Explanation:\*
	!	.b
	!	Command is not defined in the user defined macro file.
	!	.b
	!	^*User Action:\*
	!	.b
	!	Re-enter the command with the correct macro name.
	!	.b
	!	Typing MACRO/LIST will display all user defined macros.
	!	.lm -5
	!
	! Index:
	!
	!--

	USER_COMMANDS$ = ""
	GOTO MainMenu

 MaxQualif:
	CALL HELP_34MESSAGE(SCOPE, "too many qualifiers", &
		"E", "UTL_MENU", "", "TOOMQUALIF")
	!++
	! Error:TOOMQUALIF
	!
	!	^*Too Many Qualifiers\*
	!	.p
	!	^*Explanation:\*
	!	.p
	!	Maximum number of qualifiers for one user defined
	!	macro command has been already reached.
	!	.p
	!	^*User Action:\*
	!	.p
	!	Change the command name or delete any existing macro name
	!	for specified command. Typing MACRO/LIST may display all
	!	user defined macros.
	!--

18970	WHEN ERROR IN
		GET #UTL_MACROS.CH%, &
			KEY #0% EQ USER_COMMANDS$ + &
			SPACE$(20% - LEN(USER_COMMANDS$))
		DELETE #UTL_MACROS.CH%
	USE
		FILENAME$ = "UTL_MACROS"
		CONTINUE HelpError
	END WHEN

	GOSUB ReadMacros

	USER_COMMANDS$ = ""
	GOTO MainMenu

	%PAGE

18990	!*******************************************************************
	! Delete temp file to prepare for exit
	!******************************************************************
 KillTempPrint:

	SMG_STATUS% = LIB$DELETE_FILE(TEMPFILE$ + ";*")

18995	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

19000	!***************************************************************
	! ERROR TRAPPING
	!***************************************************************

	XERROR% = ERR
	XERLINE% = ERL

	SELECT ERR
	CASE 15%
		S$ = "BYE"
		RESUME 500

	CASE 27%
		RESUME 18800
	END SELECT

	FILENAME$ = ""

	SELECT ERL
	CASE 400%
		SELECT ERR
		CASE 173%	! Invalid RFA field
			INVRFA% = -1%
			RESUME 390
		CASE ELSE
			FILENAME$ = "MENU"
		END SELECT

	CASE 600%
		RESUME ChainTo IF ERR = 52%
		FILENAME$ = "MENU"

	CASE 18950%
		RESUME MaxQualif IF ERR = 55%
		FILENAME$ = "UTL_MACROS"

	END SELECT

	!
	! Untrapped error
	!
	RESUME HelpError

32767	END
