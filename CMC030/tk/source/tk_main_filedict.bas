1	%TITLE "Maintain Record Structure File"
	%SBTTL "TK_MAIN_FILEDICT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG TK_MAIN_FILEDICT(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1987 BY
	!
	! Computer Management Center
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be used and
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies therof may not be provided or otherwise made
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
	!	This program is a maintenance program for tracking
	!	record structures.
	!
	! Index:
	!
	! Option:
	!
	! Author:
	!
	!	08/17/87 - B. Craig Larsen
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_MAIN_FILEDICT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP TK_MAIN_FILEDICT
	!	$ DELETE TK_MAIN_FILEDICT.OBJ;*
	!
	! Modification history:
	!
	!	07/25/89 - Aaron Redd
	!		Rewrote to accomodate changed file layout.
	!
	!	05/31/90 - Kevin Handy
	!		Modified to allow only upper case inputs into fields.
	!
	!	08/23/90 - Frank F. Starman
	!		Change Structure_definition to Structure_description
	!		and call HELP_34MESSAGE function.
	!
	!	06/11/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/14/96 - Kevin Handy
	!		Lost extra '&' before 'else'.
	!		Reformat source code.
	!
	!	08/16/96 - Kevin Handy
	!		Lost another extra '&' before 'else'.
	!
	!	06/04/97 - Kevin Handy
	!		Use integer for #key
	!
	!	07/25/98 - Kevin Handy
	!		Reformat source code.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$, SS$ routines
	!
	!	09/11/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Disable CDD calls (so I don't need to install it again)
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "$SSDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:TK_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[SMG.OPEN]SMG_DDL.HB"
	DECLARE  SMG_DDL_CDD DDL

	%INCLUDE "SOURCE:[TK.OPEN]TK_FILEDICT.HB"
	MAP (TK_FILEDICT) TK_FILEDICT_CDD TK_FILEDICT
	MAP (TK_FILEDICT2) TK_FILEDICT_CDD TK_FILEDICT_OLD, TK_FILEDICT2

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_TK_FILEDICT) &
		TK_FILEDICT.CH%, &
		TK_FILEDICT.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION LIBR_MAINT
	EXTERNAL LONG   FUNCTION LIBR_CONNECT
	EXTERNAL LONG   FUNCTION LIBR_DELETE
	EXTERNAL LONG   FUNCTION LIBR_3INSERT

	EXTERNAL LONG   FUNCTION MAIN_JOURNAL

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!
	!
	! Modify the menu
	!
	CASE OPT_OPTLIST
		MVALUE = MVALUE + &
			" Opens Layouts docUmentation Structure_description"

	!
	! Optional menu items
	!
	CASE OPT_MOREMENU

		!********************************
		! Temporary alteration  UT => UTL
		!********************************
		IF (TK_FILEDICT::SYSTEM = "UT")
		THEN
			SYSTEM$ = "UTL"
		ELSE
			SYSTEM$ = TK_FILEDICT::SYSTEM
		END IF

		SELECT LEFT(SCOPE::PRG_ITEM, 4%)
		!
		! Edit the opens code
		!
		CASE "Open"
 FileOpen:		FO_FLAG% = 0%
			CALL TK_SUBR_CREATEOPEN(SYSTEM$, &
				TK_FILEDICT::FILENAME, &
				SYSTEM$, "", &
				FO_FLAG%, FO_STATUS%)

			IF (FO_STATUS% AND SS$_EXBYTLM)
			THEN
				CALL HELP_3MESSAGE(SCOPE, &
					"TK_SUBR_CREATEOPEN " + &
					NUM1$(FO_STATUS%) + " " + &
					"Exceded byte count quota !", &
					"ERR", "TK_SUBR_CREATEOPEN", &
					"STATUS" + NUM1$(FO_STATUS%))

				EXIT FUNCTION
			END IF

			GOTO FileOpen IF (FO_FLAG% AND 2%)

		!
		! Edit the DDL's
		!
		CASE "Layo"
			CALL TK_MAIN_DDL(SYSTEM$, SYSTEM$, &
				TK_FILEDICT::FILENAME, &
				TK_FILEDICT::DESCR)

		!
		! Structure description
		!
		CASE "Stru"
			CALL HELP_34MESSAGE(SCOPE, &
				TK_FILEDICT::FILENAME, "H", &
				TRM$(TK_FILEDICT::FILENAME), &
				"", "FILE")

		!
		! Documentation
		!
		CASE "docU"
			LOC_FILEDOCU$ = "TEMP" + READ_SYSJOB + ".TMP"
			CALL TK_SUBR_FILEDOCU(TK_FILEDICT, &
				LOC_FILEDOCU$)

			!
			! Insert into files on ref:
			!
			SMG_STATUS% = LIBR_3INSERT("REF:FILES", &
				LOC_FILEDOCU$, &
				TRM$(TK_FILEDICT::FILENAME) + "_CDD")

			!
			! Kill temp file
			!
			SMG_STATUS% = LIB$DELETE_FILE( &
				LOC_FILEDOCU$ + ";*")

			IF (SMG_STATUS% AND 1%) = 0%
			THEN
				CALL ENTR_3MESSAGE(SCOPE, &
					"Error deleting OPENS - " &
					+ NUM1$(SMG_STATUS%), 0%)
			END IF

			!
			! Call to library to allow maintenance
			!
			SMG_STATUS% = LIBR_MAINT("REF:FILES", &
				TRM$(TK_FILEDICT::FILENAME) + "_CDD", &
				"View Only Documentation", 0%)

			!
			! Delete from library
			!
			SMG_STATUS% = LIBR_DELETE( &
				"REF:FILES", &
				TRM$(TK_FILEDICT::FILENAME) + "_CDD")

			IF (SMG_STATUS% AND 1%) = 0%
			THEN
				CALL ENTR_3MESSAGE(SCOPE, &
					"Error deleting library entry - " &
					+ NUM1$(SMG_STATUS%), 0%)
			END IF

		END SELECT

	CASE OPT_AFTEROPT

		!********************************
		! Temporary alteration  UT => UTL
		!********************************
		IF (TK_FILEDICT::SYSTEM = "UT")
		THEN
			SYSTEM$ = "UTL"
		ELSE
			SYSTEM$ = TK_FILEDICT::SYSTEM
		END IF

		SELECT SCOPE::PRG_ITEM
		CASE "Erase"
			!
			! Delete the opens
			!
			SMG_STATUS% = LIB$DELETE_FILE( &
				"SOURCE:[" + EDIT$(SYSTEM$, &
				2% + 4% + 32% + 256%) + ".OPEN]" + &
				EDIT$(TK_FILEDICT::FILENAME, &
				2% + 4% + 32% + 256%) + ".*;*")

			IF (SMG_STATUS% AND 1%) = 0%
			THEN
				CALL ENTR_3MESSAGE(SCOPE, &
					"Error deleting OPENS - " &
					+ NUM1$(SMG_STATUS%), 0%)
			ELSE
				CALL ENTR_3MESSAGE(SCOPE, &
					"Open ERASE completed!!!", 1%)
			END IF

			!
			! Delete the DDL's
			!
 !			SMG_STATUS% = LIB$SPAWN("DMU DELETE " + &
 !				EDIT$(SYSTEM$, &
 !				2% + 4% + 32% + 256%) + "." + &
 !				EDIT$(TK_FILEDICT::FILENAME, &
 !				2% + 4% + 32% + 256%))
 !
 !			IF (SMG_STATUS% AND 1%) = 0%
 !			THEN
 !				CALL ENTR_3MESSAGE(SCOPE, &
 !					"Error deleting DDL - " + &
 !					NUM1$(SMG_STATUS%), 0%)
 !			ELSE
 !				CALL ENTR_3MESSAGE(SCOPE, &
 !					"DDL ERASE completed", 1%)
 !			END IF

			!
			! Delete the library entrys
			!
			SMG_STATUS% = LIBR_DELETE( &
				"REF:HELP_" + &
				LEFT(TK_FILEDICT::FILENAME, 2%), &
				TRM$(TK_FILEDICT::FILENAME) + "_CDD")

			IF (SMG_STATUS% AND 1%) = 0%
			THEN
				CALL ENTR_3MESSAGE(SCOPE, &
					"Error deleting library entry - " &
					+ NUM1$(SMG_STATUS%), 0%)
			ELSE
				CALL ENTR_3MESSAGE(SCOPE, &
					"Library ERASE completed!", 1%)
			END IF

			!
			! Delete FOREIGN keys
			!
			TK_MAIN_FILEDICT = &
				MAIN_JOURNAL(TK_MAIN_FOREIGN.ID, "E")

		CASE "Change"

			IF (TK_FILEDICT::FILENAME <> &
				TK_FILEDICT_OLD::FILENAME)
			THEN
				!
				! Connect the new key to the old file
				!
				X1% = LIBR_CONNECT( &
					"REF:HELP_" + &
					LEFT(TK_FILEDICT::FILENAME, 2%), &
					TRM$(TK_FILEDICT::FILENAME) + &
					"_CDD", &
					TRM$(TK_FILEDICT_OLD::FILENAME) + "_CDD")

				CALL ENTR_3MESSAGE(SCOPE, &
					"Error connecting new key " + &
					"to old file - " &
					+ NUM1$(X1%), 0%) &
					IF (X1% AND 1%) = 0%

				!
				! Delete the old library key
				!
				X2% = LIBR_DELETE( &
					"REF:HELP_" + &
					LEFT(TK_FILEDICT::FILENAME, 2%), &
					TRM$(TK_FILEDICT_OLD::FILENAME) + "_CDD")

				CALL ENTR_3MESSAGE(SCOPE, &
					"Error deleting old key - " + &
					NUM1$(X2%), &
					0%) IF (X2% AND 1%) = 0%

				CALL ENTR_3MESSAGE(SCOPE, &
					"Library change completed!", 1%) &
					IF (X1% AND 1%) AND (X2% AND 1%)
			END IF

			IF TK_FILEDICT::FILENAME <> &
				TK_FILEDICT_OLD::FILENAME
			THEN
				CALL TK_SUBR_DDLEXTRACT(DDL, &
					TK_FILEDICT_OLD::SYSTEM, &
					TK_FILEDICT_OLD::FILENAME, &
					FDE_STATUS%)

				IF (FDE_STATUS% AND SS$_EXBYTLM)
				THEN
					CALL HELP_3MESSAGE(SCOPE, &
						"TK_SUBR_DDLEXTRACT " + &
						NUM1$(FDE_STATUS%) + &
						" " + &
						"Exceded byte count quota !", &
						"ERR", "TK_SUBR_DDLEXTRACT", &
						"STATUS" + NUM1$(FDE_STATUS%))

					EXIT FUNCTION
				END IF

				DDL::FIELD_NUM	= 1% IF DDL::FIELD_NUM < 0%
				DDL::DESCR	= TK_FILEDICT_OLD::DESCR

				CALL TK_SUBR_DDLCOMPILE(DDL, &
					TK_FILEDICT_OLD::SYSTEM, &
					TK_FILEDICT::FILENAME)

				!
				! Change the opens from old to new
				!
				CALL TK_SUBR_CREATEOPEN(TK_FILEDICT_OLD::SYSTEM, &
					TK_FILEDICT_OLD::FILENAME, &
					TK_FILEDICT_OLD::SYSTEM, &
					TK_FILEDICT::FILENAME, 1%, &
					FO_STATUS%)

				IF (FO_STATUS% AND SS$_EXBYTLM)
				THEN
					CALL HELP_3MESSAGE(SCOPE, &
						"TK_SUBR_CREATEOPEN " + &
						NUM1$(FO_STATUS%) + " " + &
						"Exceded byte count quota !", &
						"ERR", "TK_SUBR_CREATEOPEN", &
						"STATUS" + NUM1$(FO_STATUS%))

					EXIT FUNCTION
				END IF

				!
				! Delete the DDL's
				!
 !				CALL ENTR_3MESSAGE(SCOPE, &
 !					"Deleting DDL's ", 1%) &
 !
 !				SMG_STATUS% = LIB$SPAWN("DMU DELETE CDD$TOP." + &
 !					EDIT$(TK_FILEDICT_OLD::SYSTEM, &
 !					2% + 4% + 32% + 256%) + "." + &
 !					EDIT$(TK_FILEDICT_OLD::FILENAME, &
 !					2% + 4% + 32% + 256%))
 !
 !				CALL ENTR_3MESSAGE(SCOPE, &
 !					"Error deleting DDL - " + &
 !					NUM1$(SMG_STATUS%), 0%) &
 !					IF (SMG_STATUS% AND 1%) = 0%

				!
				! Delete the opens
				!
				CALL ENTR_3MESSAGE(SCOPE, &
					"Deleting OPEN source !!!", 1%)

				SMG_STATUS% = LIB$DELETE_FILE( &
					"SOURCE:[" + EDIT$(TK_FILEDICT_OLD::SYSTEM, &
					2% + 4% + 32% + 256%) + ".OPEN]" + &
					EDIT$(TK_FILEDICT_OLD::FILENAME, &
					2% + 4% + 32% + 256%) + ".*;*")

				CALL ENTR_3MESSAGE(SCOPE, &
					"Error deleting OPENS - " &
					+ NUM1$(SMG_STATUS%), 0%) &
					IF (SMG_STATUS% AND 1%) = 0%

				CALL ENTR_3MESSAGE(SCOPE, &
					"Change finished !!!", 1%) &

				SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)

			END IF

			!
			! Change FOREIGN keys
			!
			TK_MAIN_FILEDICT = &
				MAIN_JOURNAL(TK_MAIN_FOREIGN.ID, "C" + &
				TK_FILEDICT::FILENAME)

		END SELECT

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Record Structure Maintenance"
		SMG_WINDOW::NHELP = "TK_MAIN_FILEDICT"
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 16%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 3%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS			= 2%
		SMG_WINDOW::KNAME(0%)			= "File_name"
			SMG_WINDOW::KFIELD(0%, 0%)	= 1%
			SMG_WINDOW::KFIELD(0%, 1%)	= 1%
		SMG_WINDOW::KNAME(1%)			= "System"
			SMG_WINDOW::KFIELD(1%, 0%)	= 2%
			SMG_WINDOW::KFIELD(1%, 1%)	= 3%
			SMG_WINDOW::KFIELD(1%, 2%)	= 1%

		!
		! Load in defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

20050		!
		! Get info required for main file
		!
		IF TK_FILEDICT.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF TK_FILEDICT.READONLY%
			GOTO 20090
		END IF

		!
		! Open main file (existing) for modification
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[TK.OPEN]TK_FILEDICT.CRE"
		USE
			CONTINUE 20060 IF ERR = 10%
			TK_MAIN_FILEDICT = ERR
			CONTINUE 20070
		END WHEN

		TK_FILEDICT.READONLY% = 0%
		GOTO 20090

20060		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[TK.OPEN]TK_FILEDICT.OPN"
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
			TK_FILEDICT.READONLY% = -1%
		USE
			TK_MAIN_FILEDICT = ERR
			CONTINUE 20070
		END WHEN

		GOTO 20090

20070		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(TK_FILEDICT.CH%)

		EXIT FUNCTION

20090		SMG_WINDOW::CHAN  = TK_FILEDICT.CH%

		WHEN ERROR IN
			RESET #TK_FILEDICT.CH%
			GET #TK_FILEDICT.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display the background
	!
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	06, 03, "(01) File Name", &
			08, 03, "(02) Description", &
			10, 03, "(03) System", &
			0, 0, ""

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

	!
	! Enter/Display/Default
	!
20200	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
			TK_FILEDICT::FILENAME = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;23", TEMP$, &
				TK_FILEDICT::FILENAME, &
				MFLAG OR 16%, "'E", MVALUE)

		CASE 2%
			TK_FILEDICT::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "8;23", TEMP$, &
				TK_FILEDICT::DESCR, MFLAG, &
				"'E", MVALUE)

		CASE 3%
			TK_FILEDICT::SYSTEM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "10;23", TEMP$, &
				TK_FILEDICT::SYSTEM, &
				MFLAG OR 16%, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
	TK_MAIN_FILEDICT = 0%

	SELECT MLOOP

	CASE 2%
		IF TK_FILEDICT::FILENAME = ""
		THEN
			TK_MAIN_FILEDICT = 1%
		ELSE
			IF (MVALUE = "ADD")
			THEN
				WHEN ERROR IN
					GET #TK_FILEDICT.CH%, &
						KEY #0% EQ TK_FILEDICT::FILENAME + "", &
						REGARDLESS
				USE
					CONTINUE 32767 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				TK_MAIN_FILEDICT = 2%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Record Already Exists", 1%)
			END IF
		END IF

	END SELECT


	!
	! Set TK_FILEDICT_OLD value
	!
20500	CASE OPT_SETOLD
		TK_FILEDICT_OLD = TK_FILEDICT

	!
	! Restore TK_FILEDICT_OLD value
	!
	CASE OPT_RESETOLD
		TK_FILEDICT = TK_FILEDICT_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		TK_FILEDICT2 = TK_FILEDICT

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		TK_FILEDICT = TK_FILEDICT2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Filename                       Description"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "033"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = &
				LEFT(TK_FILEDICT::FILENAME, 30%) + " " + &
				LEFT(TK_FILEDICT::DESCR, 45%)

		END SELECT
	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #TK_FILEDICT.CH%, &
				KEY #0% GE TK_FILEDICT::FILENAME + "", &
				REGARDLESS
		CASE 1%
			FIND #TK_FILEDICT.CH%, &
				KEY #1% GE (TK_FILEDICT::SYSTEM + TK_FILEDICT::FILENAME), &
				REGARDLESS
		END SELECT
	END SELECT

 ExitFunction:
	EXIT FUNCTION

29000	!
	! Trap Errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
