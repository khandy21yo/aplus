1	%TITLE "Device File Maintenance Function"
	%SBTTL "UTL_MAIN_DEVICE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG UTL_MAIN_DEVICE(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1989 BY
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
	! ID:0138
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Device\* option maintains the
	!	device protections in the system menus.
	!	.b
	!	The program contains the following four indicators:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	R = Read      User can read the file.
	!	.le
	!	W = Write     User can read from or write to the file.
	!	.le
	!	E = Execute   User can execute the file.
	!	.le
	!	D = Delete    User can delete the file.
	!	.els
	!	.lm -5
	!
	! Index:
	!	.x Device>Protection
	!	.x Protection>Device
	!	.x Table>Device
	!	.x Device>Table
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_MAIN_DEVICE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN UTL_MAIN_DEVICE
	!	$ DELETE UTL_MAIN_DEVICE.OBJ;*
	!
	! Author:
	!
	!	07/25/89 - Aaron Redd
	!
	! Modification history:
	!
	!	03/13/92 - Kevin Handy
	!		Unrolled error trap (check)
	!
	!	05/13/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	02/10/94 - Kevin Handy
	!		Added access to Frank's "FILEDICT" file,
	!		since there is no maintenance program for it
	!		anywhere.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/30/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	09/12/98 - Kevin Handy
	!		Clean up source
	!		Use '%INCLUDE' instead of '%INCLUDE %FROM %CDD'
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/04/99 - Kevin Handy
	!		Change field 'DEVICE' to 'DEVICENAME'
	!
	!	06/28/99 - Kevin Handy
	!		Modified to not alter existing records on a
	!		'getmaster', so that new files can be added
	!		while old ones are left alone.
	!
	!	08/25/99 - Kevin Handy
	!		Allow entry of undefined file names because
	!		we have systems with bad dictionaries but I
	!		still need to define file names.
	!
	!	11/06/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:TK_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_DEVICE.HB"
	MAP (UTL_DEVICE)	UTL_DEVICE_CDD		UTL_DEVICE
	MAP (UTL_DEVICE2)	UTL_DEVICE_CDD		UTL_DEVICE_OLD, UTL_DEVICE2
	MAP (UTL_DEVICE_T)	UTL_DEVICE_CDD		UTL_DEVICE_TEMP

	%INCLUDE "SOURCE:[TK.OPEN]TK_FILEDICT.HB"
	MAP (TK_FILEDICT)	TK_FILEDICT_CDD		TK_FILEDICT

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (UTL_DEVICE.CH) &
		UTL_DEVICE.CH%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_WINDOW
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 29000

	SELECT MOPTION

	!******************************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!******************************************************************
	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Device File Maintenance"
		SMG_WINDOW::NHELP = "UTL_MAIN_DEVICE"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 130%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 4%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "File_name"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KNAME(1%) = "System"
			SMG_WINDOW::KFIELD(1%, 0%) = 2%
			SMG_WINDOW::KFIELD(1%, 1%) = 2%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%

		!
		! Load in defaults for device
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

		!
		! Close channel if it's already open in another program
		! (we want Read/Write access)
		!
		CLOSE #UTL_DEVICE.CH% IF (UTL_DEVICE.CH% > 0%)

		!
		! Open main file (whether existing or not) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_DEVICE.CRE"
		USE
			CONTINUE 32767
		END WHEN

790		SMG_WINDOW::CHAN  = UTL_DEVICE.CH%
		WHEN ERROR IN
			RESET #UTL_DEVICE.CH%
			GET #UTL_DEVICE.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	%PAGE

	!
	! Modify the menu
	!
	CASE OPT_OPTLIST
		MVALUE = MVALUE + " Getmaster fiLedict"

	!
	! Optional menu items
	!
	CASE OPT_MOREMENU

		SELECT SCOPE::PRG_ITEM

		!
		! Get a system from the Master Device file
		!
		CASE "Getmaster"
			GOSUB GetMaster

		CASE "fiLedict"
			V% = MAIN_WINDOW(TK_MAIN_FILEDICT.ID, "")

		END SELECT

	%PAGE

20100	!******************************************************************
	! Display the background
	!
	! This option is used to display the background information
	! on the screen.  It must first clear any junk on the screen,
	! and then write the background onto it.
	!******************************************************************
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	6,  5, "(01) File Name", &
			8,  5, "(02) System", &
			9,  5, "(03) Device Name", &
			10,  5, "(04) Protection Code", &
			0,  0, ""

		RESTORE

		READ XPOS%, YPOS%, XSTR$
		I% = 0%
		WHILE (XPOS% <> 0%)
			I% = I% + 1%
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				XSTR$, XPOS%, YPOS%) &
				IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%
			READ XPOS%, YPOS%, XSTR$
		NEXT

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	%PAGE

20200	!******************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display
	! data, set defaults, and return the data back according to
	! MFLAG.
	!******************************************************************
	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		SCOPE::SCOPE_EXIT = 0%

 Eloop:		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) File Name\*
	!	.b
	!	.lm +5
	!	The ^*File Name\* field enters the name of the file
	!	which will be used in this function.
	!	.lm -5
	!
	! Index:
	!	.x File Name>Device
	!	.x Device>File Name
	!
	!--

			UTL_DEVICE::FILENAM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"6;30", TEMP$, UTL_DEVICE::FILENAM, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(TK_MAIN_FILEDICT.ID, "V0  ") = 1%)
				THEN
					UTL_DEVICE::FILENAM = TK_FILEDICT::FILENAME
				END IF
				GOTO ELoop
			END IF

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) System\*
	!	.b
	!	.lm +5
	!	The ^*System\* field enters the name of the system which
	!	the file comes from.
	!	.lm -5
	!
	! Index:
	!	.x System>Device
	!
	!--
			UTL_DEVICE::CATAG = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"8;30", TEMP$, UTL_DEVICE::CATAG, &
				MFLAG, "'E", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Device Name\*
	!	.b
	!	.lm +5
	!	The ^*Device Name\* field enters the assigned name of
	!	the specified device.
	!	.lm -5
	!
	! Index:
	!	.x Device Name
	!	.x Name>Device
	!
	!--
			UTL_DEVICE::DEVICENAME = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"9;30", TEMP$, LEFT(UTL_DEVICE::DEVICENAME, 35%), &
				MFLAG, "'E", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Protection Code\*
	!	.b
	!	.lm +5
	!	The ^*Protection Code\* field enters the code which
	!	indicates the type of Protection.
	!	.lm -5
	!
	! Index:
	!	.x Protection Code
	!	.x Code>Protection
	!
	!--
			UTL_DEVICE::PROCOD = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"10;30", TEMP$, UTL_DEVICE::PROCOD, &
				MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	%PAGE

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		UTL_MAIN_DEVICE = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Is the input defined?
			!
			XUTL_MAIN_DEVICE = FUNC_TESTENTRY(SMG_WINDOW, &
				UTL_DEVICE::FILENAM, &
				TK_FILEDICT::DESCR, &
				"UTL", MLOOP, "PRG", &
				"File name", TK_MAIN_FILEDICT.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(TK_FILEDICT::DESCR, 43%), &
				7%, 32%, , SMG$M_BOLD)

		CASE 2%
			UTL_MAIN_DEVICE = 1% &
				IF (UTL_DEVICE::CATAG = "")

		END SELECT

	!
	! Display file description
	!
	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			UTL_DEVICE_TEMP = UTL_DEVICE

			IF MAIN_WINDOW(TK_MAIN_FILEDICT.ID, &
				"Q0" + UTL_DEVICE::FILENAM) <> 1%
			THEN
				TK_FILEDICT::DESCR = STRING$(43%, A"?"B)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(TK_FILEDICT::DESCR, 43%), &
				7%, 32%, , SMG$M_BOLD)

			UTL_DEVICE = UTL_DEVICE_TEMP
		END IF


	!
	! Set UTL_DEVICE_OLD value
	!
20500	CASE OPT_SETOLD
		UTL_DEVICE_OLD = UTL_DEVICE

	!
	! Restore UTL_DEVICE_OLD value
	!
	CASE OPT_RESETOLD
		UTL_DEVICE = UTL_DEVICE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		UTL_DEVICE2 = UTL_DEVICE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		UTL_DEVICE = UTL_DEVICE2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  FileName                          " + &
				"System   Device                       " + &
				"                 Protection"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "036,045,091"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = LEFT(UTL_DEVICE::FILENAM, 33%) + " " + &
				UTL_DEVICE::CATAG + " " + &
				LEFT(UTL_DEVICE::DEVICENAME, 45%) + " " + &
				LEFT(UTL_DEVICE::PROCOD, 39%)


		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #UTL_DEVICE.CH%, &
				KEY #0% GE UTL_DEVICE::FILENAM + "", REGARDLESS

		CASE 1%
			FIND #UTL_DEVICE.CH%, &
				KEY #1% GE (UTL_DEVICE::CATAG + &
				UTL_DEVICE::FILENAM), &
				REGARDLESS

		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

	%PAGE

 GetMaster:
	!******************************************************************
	! Get a list of files from the Master Device file
	!******************************************************************
	RECORDS%, ADDED%, UPDATED% = 0%
	SYSTEM$, DIRECT$ = ""

	!
	! Open the Master File list (save the present Device record)
	!
27000	WHEN ERROR IN
		%INCLUDE "SOURCE:[TK.OPEN]TK_FILEDICT.OPN"
	USE
		CONTINUE GetReturn
	END WHEN

	TEXT$ = "System to get from Master"

27010	SYSTEM$ = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, "8;30", TEXT$, &
		UTL_DEVICE::CATAG, 0%, "'E", "")

	GOTO GetReturn IF &
		(SCOPE::SCOPE_EXIT = SMG$K_TRM_CTRLZ) OR &
		(SCOPE::SCOPE_EXIT = SMG$K_TRM_F10)

	GOTO GetReturn IF (SYSTEM$ = "")

	TEXT$ = "Default device"

	DIRECT$ = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, "9;30", TEXT$, &
		LEFT(UTL_DEVICE::DEVICENAME, 45%), 0%, "'E", "")

	GOTO GetReturn IF &
		(SCOPE::SCOPE_EXIT = SMG$K_TRM_CTRLZ) OR &
		(SCOPE::SCOPE_EXIT = SMG$K_TRM_F10)

	!
	! Search for first File from that system
	!
27050	WHEN ERROR IN
		FIND #TK_FILEDICT.CH%, KEY #1% GE SYSTEM$, REGARDLESS
	USE
		CONTINUE 27080 IF (ERR = 155%)
		CONTINUE GetReturn
	END WHEN

	!
	! Get the (next) record from the Master File list
	!
27060	WHEN ERROR IN
		GET #TK_FILEDICT.CH%, REGARDLESS
	USE
		CONTINUE 27080 IF (ERR = 11%)
		CONTINUE GetReturn
	END WHEN

	!
	! Exit if we're done with the specified system
	!
	GOTO 27080 IF (TK_FILEDICT::SYSTEM <> SYSTEM$)

	!
	! Build a record to add to the User's Device file
	!
	UTL_DEVICE::FILENAM	= TK_FILEDICT::FILENAME
	UTL_DEVICE::CATAG	= TK_FILEDICT::SYSTEM
	UTL_DEVICE::DEVICENAME	= DIRECT$
	UTL_DEVICE::PROCOD	= ""

	RECORDS% = RECORDS% + 1%

	!
	! Put the record in the User's Device file
	!
27070	WHEN ERROR IN
		PUT #UTL_DEVICE.CH%
	USE
		CONTINUE 27071 IF (ERR = 134%)
		CONTINUE GetReturn
	END WHEN

	ADDED% = ADDED% + 1%
	GOTO 27060

	!
	! Just go on if there's nothing to update
	!
27071	!
	! Get the record with the duplicate key
	!
 !	GET #UTL_DEVICE.CH%, KEY #0% EQ UTL_DEVICE::FILENAM + ""
 !
 !	UTL_DEVICE::DEVICENAME = DIRECT$

	!
	! Update the record in the User's Device file
	!
 !	UPDATE #UTL_DEVICE.CH%
 !	UPDATED% = UPDATED% + 1%

	!
	! Go back to get the next Master record
	!
27075	GOTO 27060

	!
	! Print message if records were added
	!
27080	IF (ADDED% = 0%)
	THEN
		TEXT$ = FORMAT$(UPDATED%, "##") + &
			" record(s) updated from Master"
		TEXT$ = "No new records!" IF (UPDATED% = 0%)
		TEXT$ = "No records in Master to add!" IF (RECORDS% = 0%)
	ELSE
		TEXT$ = FORMAT$(ADDED%, "##") + " record(s) added"
		TEXT$ = TEXT$ + ", " + FORMAT$(UPDATED%, "##") + &
			" updated" IF (UPDATED% <> 0%)
		TEXT$ = TEXT$ + " from Master"
	END IF

	CALL ENTR_3MESSAGE(SCOPE, TEXT$, 0%)
	CLOSE TK_FILEDICT.CH%
	UTL_MAIN_DEVICE = 8%

 GetReturn:
	RETURN

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	!
	! Trap untrapped errors
	!
	ON ERROR GO BACK

32767	!******************************************************************
	! End of maintenance function UTL_MAIN_DEVICE
	!******************************************************************
	END FUNCTION
