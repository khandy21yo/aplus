1	%TITLE "Document Destination Maintenance"
	%SBTTL "UTL_MAIN_DOC_DEST"
	%IDENT "V3.6a Calico"

	FUNCTION LONG UTL_MAIN_DOC_DEST(CDD_WINDOW_CDD SMG_WINDOW, &
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
	! ID:0150
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Document Destination\* screen is available to specify the
	!	device which is assigned to print a report.
	!
	! Index:
	!	.x Utility>Document Destination
	!	.x Document>Destination
	!	.x Document Destination>Maintenance
	!	.x Maintain>Document Destination
	!
	! Option:
	!
	! COMPILE:
	!
	!	$ BAS UTL_SOURCE:UTL_MAIN_DOC_DEST/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN UTL_MAIN_DOC_DEST
	!	$ DELETE UTL_MAIN_DOC_DEST.OBJ;*
	!
	! Author:
	!
	!	08/25/87 - Kevin Handy
	!
	! Modification history:
	!
	!	05/29/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/30/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/05/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND problem
	!
	!	12/12/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_DOC_DEST.HB"
	MAP (UTL_DOC_DEST)		UTL_DOC_DEST_CDD	UTL_DOC_DEST
	MAP (UTL_DOC_DEST_OLD) UTL_DOC_DEST_CDD UTL_DOC_DEST_OLD, UTL_DOC_DEST2

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_UTL_DOC_DEST) &
		UTL_DOC_DEST.CH%, &
		UTL_DOC_DEST.READONLY%

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Document  Destination  Maint."
		SMG_WINDOW::NHELP = "UTL_MAIN_DOC_DEST"
		SMG_WINDOW::HSIZE = 30%
		SMG_WINDOW::VSIZE = 5%
		SMG_WINDOW::HPOS  = 25%
		SMG_WINDOW::VPOS  = 6%
		SMG_WINDOW::HVIEW = 44%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::VHPOS = 12%
		SMG_WINDOW::VVPOS = 2%
		SMG_WINDOW::NITEMS= 3%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Name"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%


		IF INSTR(1%, " QV", MVALUE) <= 1%
		THEN
			!
			! Load in defaults
			!
			CALL READ_DEFAULTS(SMG_WINDOW)
		END IF

700		!
		! Declare channels
		!
		IF UTL_DOC_DEST.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF UTL_DOC_DEST.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_DOC_DEST.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			UTL_MAIN_DOC_DEST = ERR
			CONTINUE 770
		END WHEN

		UTL_DOC_DEST.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_DOC_DEST.OPN"
		USE
			UTL_MAIN_DOC_DEST = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		UTL_DOC_DEST.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(UTL_DOC_DEST.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = UTL_DOC_DEST.CH%
		WHEN ERROR IN
			RESET #UTL_DOC_DEST.CH%
			GET #UTL_DOC_DEST.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display the background
	!
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	1, 1, "(01) Name", &
			3, 1, "(02) Destination", &
			5, 1, "(03) Printer type", &
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

	!
	! Enter/Display/Default
	!
20200	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Name\*
	!	.b
	!	.lm +5
	!	The ^*Name\* field enters a reference name
	!	for a particular device.
	!	.b
	!	The field may contain up to ten (10) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!
	!--

			UTL_DOC_DEST::PNAME = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "1;19", TEMP$, &
				UTL_DOC_DEST::PNAME, MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Destination\*
	!	.b
	!	.lm +5
	!	This field is used to assign the device which will print
	!	the report.
	!	.b
	!	Possible values are:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	DISPLAY      - Display the report on the screen.
	!	.le
	!	DP           - Mnemonic for Display.
	!	.le
	!	PRINTER PORT - Send output to printer port.
	!	.le
	!	PP           - Mnemonic for printer port.
	!	.le
	!	SPOOLER      - Send output to spooler.
	!	.le
	!	SP           - Mnemonic for Spooler.
	!	.le
	!	Filename     - Send output to a file.
	!	.le
	!	Device:      - Send output to a device (Printer, etc.).
	!	.le
	!	Word Processing - Send output to a word processing file.
	!	.le
	!	WP            - Mnemonic for word processing.
	!	.els
	!	.lm -10
	!
	! Index:
	!
	!--

			UTL_DOC_DEST::DEST = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "3;19", TEMP$, &
				UTL_DOC_DEST::DEST, MFLAG, &
				"'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Printer Type\*
	!	.b
	!	.lm +5
	!	The ^*Printer Type\* field enters the type
	!	of printer to which the document is being sent.
	!	.lm -5
	!
	! Index:
	!
	!--

			UTL_DOC_DEST::PTYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "5;19", TEMP$, &
				UTL_DOC_DEST::PTYPE, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		UTL_MAIN_DOC_DEST = 0%

		SELECT MLOOP

		CASE 1%
			IF UTL_DOC_DEST::PNAME = ""
			THEN
				UTL_MAIN_DOC_DEST = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #UTL_DOC_DEST.CH%, &
							KEY #0% EQ UTL_DOC_DEST::PNAME + "", &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					UTL_MAIN_DOC_DEST = 2%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Record Already Exists", 1%)
				END IF
			END IF
		END SELECT

	!
	! Set UTL_DOC_DEST_OLD value
	!
20500	CASE OPT_SETOLD
		UTL_DOC_DEST_OLD = UTL_DOC_DEST

	!
	! Restore UTL_DOC_DEST_OLD value
	!
	CASE OPT_RESETOLD
		UTL_DOC_DEST = UTL_DOC_DEST_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		UTL_DOC_DEST2 = UTL_DOC_DEST

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		UTL_DOC_DEST = UTL_DOC_DEST2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Name       Destination          Type"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "013,034"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = UTL_DOC_DEST::PNAME + " " + &
				UTL_DOC_DEST::DEST + " " + &
				UTL_DOC_DEST::PTYPE

		END SELECT
	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #UTL_DOC_DEST.CH%, &
				KEY #0% GE UTL_DOC_DEST::PNAME + "", &
				REGARDLESS
		END SELECT

	END SELECT

	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
