1	%TITLE "Cash Disbursements Journal Maintenance"
	%SBTTL "AP_MAIN_CDJ_H"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AP_MAIN_CDJ_H(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	!	The ^*Cash Disbursements Journal Maintenance\* program maintains the
	!	Cash Disbursements Journal header file.
	!	.lm -5
	!
	! Index:
	!	.x Cash Disbursements Journal Maintenance
	!	.x Maintenance>Cash Disbursements Journal
	!
	! Option:
	!
	! COMPILE:
	!
	!	$ BAS AP_SOURCE:AP_MAIN_CDJ_H/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AP_MAIN_CDJ_H
	!	$ DELETE AP_MAIN_CDJ_H.OBJ;*
	!
	! Author:
	!
	!	10/12/87 - Kevin Handy
	!
	! Modification history:
	!
	!	05/19/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	04/14/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/21/92 - Kevin Handy
	!		Clean up (check)
	!
	!	12/10/92 - Kevin Handy
	!		Modified for 132 columns.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/04/96 - Kevin Handy
	!		Added batch number to AP_CDJ.
	!		Reformat source code.
	!
	!	05/20/97 - Kevin Handy
	!		Use integer for #key
	!
	!	06/24/97 - Kevin Handy
	!		Lose unecessary externals.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/09/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	10/27/2000 - Kevin Handy
	!		Use A'x'B
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

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AP.OPEN]AP_CDJ.HB"
	MAP (AP_CDJ)		AP_CDJ_CDD	AP_CDJ

	MAP (AP_CDJ_H)		AP_CDJ_CDD	AP_CDJ_H
	MAP (AP_CDJ_H_OLD)	AP_CDJ_CDD	AP_CDJ_H_OLD, AP_CDJ_H2

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR)		AP_VENDOR_CDD	AP_VENDOR

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_AP_CDJ) &
		AP_CDJ.CH%, &
		AP_CDJ.READONLY%, &
		CDJ_BATCH$ = 2%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_WINDOW
	EXTERNAL LONG    FUNCTION MAIN_JOURNAL
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY


	!
	! Set up error trapping
	!
	ON ERROR GOTO 29000

	%PAGE

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
		SMG_WINDOW::DESCR = "Cash Disbursements Maintenance"
		SMG_WINDOW::NHELP = "AP_MAIN_CDJ_H"
		SMG_WINDOW::HSIZE = 130%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 130%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 1%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Vendor-number"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

			!
			! Load in defaults
			!
			CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Get info required for main file
		!
		IF AP_CDJ.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AP_CDJ.READONLY%
			GOTO 790
		END IF

		!
		! Open the file
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_CDJ.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			AP_MAIN_CDJ_H = ERR
			CONTINUE 770
		END WHEN

		AP_CDJ.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_CDJ.OPN"
		USE
			AP_MAIN_CDJ_H = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		AP_CDJ.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(AP_CDJ.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = AP_CDJ.CH%
		WHEN ERROR IN
			RESET #AP_CDJ.CH%
			GET #AP_CDJ.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

		SMG_WINDOW::CHAN = AP_CDJ.CH%
		AP_CDJ_H = AP_CDJ

	!
	! Modify the menu
	!
	CASE OPT_OPTLIST
		MVALUE = MVALUE + " Lines"

	!
	! Optional menu items
	!
	CASE OPT_MOREMENU
		SELECT SCOPE::PRG_ITEM

		!
		! Line option
		!
		CASE "Lines"
	!++
	! Abstract:LINES
	!	^*Lines Items\*
	!	.b
	!	.lm +5
	!	The ^*Line Items\* option allows for maintenance
	!	of the records by line.
	!	.lm -5
	!
	! Index:
	!	.x Line Items
	!
	!--
			AP_MAIN_CDJ_H = MAIN_JOURNAL(AP_MAIN_CDJ_L.ID, "")

		END SELECT

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

		DATA	1,  1, "(01) Vendor #", &
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

 ELoop:		SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Vendor _#\*
	!	.b
	!	.lm +5
	!	The ^*Vendor _#\* field contains the reference number or key used to identify
	!	a particular vendor. When a valid ^*Vendor _#\* is entered, the
	!	name and address of that vendor will be displayed.
	!	.b
	!	If the ^*Vendor _#\* entered does not exist in the Vendor Master
	!	file, the message, ^*"Undefined Vendor number - ADD to VENDOR file
	!	(Y/N) ? <Yes/No>: No"\*, will appear at the bottom of the screen. The
	!	default response is ^*No\*. Pressing ^*Ent\* or ^*Ret\* will cause
	!	the system to permit a re-try to enter the correct vendor number. A
	!	^*Yes\* response to the message will cause the system to access the
	!	Vendor Master File where a record can be added for the new vendor.
	!	After completing the Add function for the new vendor, press the
	!	^*Exit\* key to return to the purchases Journal routine.
	!	.b
	!	Function keys which can be used are:
	!	.b
	!	.lm +5
	!	^*List Choices\* will list the vendor master file and allow
	!	the user to select a vendor.
	!	.b
	!	^*F17\* will allow editing of the vendor whose number
	!	is displayed on the screen.
	!	.lm -5
	!
	! Index:
	!	.x Vendor Number
	!
	!--
			AP_CDJ_H::VENNUM = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"1;15", TEMP$, AP_CDJ_H::VENNUM, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				AP_CDJ_H::VENNUM = AP_VENDOR::VENNUM &
					IF MAIN_WINDOW(AP_MAIN_VENDOR.ID, "VX  ") = 1%
				GOTO ELoop
			END IF


			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				ST% = MAIN_WINDOW(AP_MAIN_VENDOR.ID, "M0" + &
					AP_CDJ_H::VENNUM) = 1%
				GOTO ELoop
			END IF

			T.SE% = SCOPE::SCOPE_EXIT
			ST% = MAIN_WINDOW(AP_MAIN_VENDOR.ID, "Q0" + AP_CDJ_H::VENNUM)
			SCOPE::SCOPE_EXIT = T.SE%

			IF ST% <> 1%
			THEN
				AP_VENDOR::VENNAM	= STRING$(40%, A"?"B)
				AP_VENDOR::ADD1		= STRING$(25%, A"?"B)
				AP_VENDOR::ADD2		= STRING$(21%, A"?"B)
				AP_VENDOR::CITY		= STRING$(15%, A"?"B)
				AP_VENDOR::STATE	= STRING$(2%, A"?"B)
				AP_VENDOR::ZIP		= STRING$(10%, A"?"B)
				AP_VENDOR::COUNTRY	= STRING$(8%, A"?"B)
			END IF


			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AP_VENDOR::VENNAM, 1%, 27%,, SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AP_VENDOR::ADD1, 2%, 27%,, SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AP_VENDOR::ADD2, 3%, 27%,, SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS( &
				SMG_WINDOW::WNUMBER, &
				LEFT(TRM$(AP_VENDOR::CITY) + " " + &
				AP_VENDOR::STATE + "  " + &
				AP_VENDOR::ZIP + "  " + &
				AP_VENDOR::COUNTRY + SPACE$(40%), 40%), &
				4%, 27%,, SMG$M_BOLD)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		AP_MAIN_CDJ_H = 0%

		SELECT MLOOP

		CASE 1%
			AP_MAIN_CDJ_H = FUNC_TESTENTRY(SMG_WINDOW, &
				AP_CDJ_H::VENNUM, AP_VENDOR::VENNAM, &
				"AP", MLOOP, "VENNUM", &
				"Vendor number", AP_MAIN_VENDOR.ID)

		END SELECT


	!
	! Set AP_CDJ_OLD value
	!
20500	CASE OPT_SETOLD
		AP_CDJ_H_OLD = AP_CDJ_H

	!
	! Restore AP_CDJ_OLD value
	!
	CASE OPT_RESETOLD
		AP_CDJ_H = AP_CDJ_H_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		AP_CDJ_H2 = AP_CDJ_H

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		AP_CDJ_H = AP_CDJ_H2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Vendor#"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "012"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = AP_CDJ_H::VENNUM

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #AP_CDJ.CH%, &
				KEY #0% GE AP_CDJ_H::VENNUM + "", &
				REGARDLESS

		END SELECT

	!
	! Handle finishing various options specially
	!
	CASE OPT_AFTEROPT

		SELECT SCOPE::PRG_ITEM

		!
		! Add records
		!
		CASE "Add"
			!
			! Add line items also
			!
			AP_MAIN_CDJ_H = MAIN_JOURNAL(AP_MAIN_CDJ_L.ID, "A")

		!
		! Change records
		!
		CASE "Change"
			!
			! Change line items to match new header
			! if the key was changed.
			!
			! The original record must be the one in the
			! MAP for this to be able to work.  The new
			! key is passed through the QUERY$ variable.
			!
			IF AP_CDJ_H_OLD::VENNUM <> AP_CDJ_H::VENNUM
			THEN
				TEMP$ = AP_CDJ_H::VENNUM + ""
				AP_CDJ_H = AP_CDJ_H_OLD
				AP_MAIN_CDJ_H = MAIN_JOURNAL(AP_MAIN_CDJ_L.ID, "C" + TEMP$)
				AP_CDJ_H::VENNUM = TEMP$
			END IF

		!
		! Erase record
		!
		CASE "Erase"
			!
			! Erase any line items under the header
			!
			AP_MAIN_CDJ_H = MAIN_JOURNAL(AP_MAIN_CDJ_L.ID, "E")

		END SELECT

	CASE OPT_ARRAY

		SELECT MLOOP

		!
		! Reset to first record
		!
		CASE 10%
			RESET #AP_CDJ.CH%, KEY #0%
			GET #AP_CDJ.CH%
			AP_CDJ_H = AP_CDJ

		!
		! Search for next record
		!
20700		CASE 11%
			WHEN ERROR IN
				GET #AP_CDJ.CH%, &
					KEY #0% GT AP_CDJ_H::VENNUM + "", &
					REGARDLESS
			USE
				AP_MAIN_CDJ_H = ERR
				CONTINUE 32767
			END WHEN
			AP_CDJ_H = AP_CDJ
		END SELECT
	END SELECT

	EXIT FUNCTION

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
