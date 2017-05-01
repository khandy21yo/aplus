1	%TITLE "Element Maintenance Routine"
	%SBTTL "UTL_MAIN_ELEMENT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG UTL_MAIN_ELEMENT(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1987, 1988 BY
	!
	! Computer Management Center
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
	! Computer Management Center.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	! ID:0155
	!
	! Abstract:HELP
	!	.p
	!	The ^*Electronic Data Interchange (EDI)\* file enters
	!	the codes and definitions of all terms related to electronic business
	!	interchange data.
	!
	! Index:
	!	.x EDI>Elements
	!	.x EDI>Codes
	!	.x Electronic Data Interchange>Elements
	!	.x Electronic Data Interchange>Codes
	!
	! Option:
	!
	!
	! Input:
	!
	!
	! Output:
	!
	!
	! Example:
	!
	!
	! Author:
	!
	!	03/10/88 - Robert Peterson
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_MAIN_ELEMENT/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN UTL_MAIN_ELEMENT
	!	$ DELETE UTL_MAIN_ELEMENT.OBJ;*
	!
	! Modification history:
	!
	!	05/30/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/16/96 - Kevin Handy
	!		Reformat source code.
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
	!	11/29/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_EDI_DATAELEMENT.HB"
	MAP (UTL_EDI_DATAELEMENT)	UTL_EDI_DATAELEMENT_CDD	UTL_EDI_DATAELEMENT
	MAP (UTL_EDI_DATAELEMENT_OLD)	UTL_EDI_DATAELEMENT_CDD	UTL_EDI_DATAELEMENT_OLD, UTL_EDI_DATAELEMENT2

	MAP (TT_UTL_ELEMENT) &
		TYPTITLE$ = 20%, &
		TYP$(15%) = 20%
	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_UTL_EDI_DATAELEMENT) &
		UTL_EDI_DATAELEMENT.CH%, &
		UTL_EDI_DATAELEMENT.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_JOURNAL

	%PAGE

	!
	! Declare variables
	!
	DECLARE RFA TEMP_RFA

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "EDI Element Maintenance"
		SMG_WINDOW::NHELP = "UTL_MAIN_ELEMENT"
		SMG_WINDOW::CHAN  = UTL_EDI_DATAELEMENT.CH%
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 5%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "Reference"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		SMG_WINDOW::KNAME(1%) = "Title"
			SMG_WINDOW::KFIELD(1%, 0%) = 1%
			SMG_WINDOW::KFIELD(1%, 1%) = 2%


		IF INSTR(1%, " QV", MVALUE) <= 1%
		THEN
			!
			! Load in defaults
			!
			CALL READ_DEFAULTS(SMG_WINDOW)
		END IF


		!
		! Type table
		!
		TYPTITLE$ = "Code Description"
		TYP$(0%) = "15"
		TYP$(1%) = "N0   Numeric 0"
		TYP$(2%) = "N1   Numeric 1"
		TYP$(3%) = "N2   Numeric 2"
		TYP$(4%) = "N3   Numeric 3"
		TYP$(5%) = "N4   Numeric 4"
		TYP$(6%) = "N5   Numeric 5"
		TYP$(7%) = "N6   Numeric 6"
		TYP$(8%) = "N7   Numeric 7"
		TYP$(9%) = "R    Decimal"
		TYP$(10%)= "ID   Identifier"
		TYP$(11%)= "CL   Code List"
		TYP$(12%)= "MF   Master File"
		TYP$(13%)= "AN   String"
		TYP$(14%)= "DT   Date"
		TYP$(15%)= "TM   Time"

700		!
		! Declare channels
		!
		IF UTL_EDI_DATAELEMENT.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF UTL_EDI_DATAELEMENT.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_EDI_DATAELEMENT.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			UTL_MAIN_ELEMENT = ERR
			CONTINUE 770
		END WHEN

		UTL_EDI_DATAELEMENT.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_EDI_DATAELEMENT.OPN"
		USE
			UTL_MAIN_ELEMENT = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		UTL_EDI_DATAELEMENT.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(UTL_EDI_DATAELEMENT.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = UTL_EDI_DATAELEMENT.CH%
		WHEN ERROR IN
			RESET #UTL_EDI_DATAELEMENT.CH%
			GET #UTL_EDI_DATAELEMENT.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN


	!
	! Modify the menu
	!
	CASE OPT_OPTLIST
		MVALUE = MVALUE + " code_List"

	!
	! Optional menu items
	!
5000	CASE OPT_MOREMENU
		SELECT SCOPE::PRG_ITEM

		!
		! Line option
		!
		CASE "code_List"
			IF UTL_EDI_DATAELEMENT::TTYP = "CL"
			THEN
				!
				! Make sure there is a header
				!
				WHEN ERROR IN
					TEMP_RFA = GETRFA(UTL_EDI_DATAELEMENT.CH%)
				USE
					CONTINUE 29900
				END WHEN

				UTL_MAIN_ELEMENT = MAIN_JOURNAL(UTL_MAIN_CODELIST.ID, "")
			ELSE
				CALL ENTR_3MESSAGE(SCOPE, &
					"Element type must be 'CL'", 0%)
			END IF
		END SELECT

	!
	! Display the background
	!
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)
		DATA	02, 05, "(01) Reference", &
			03, 05, "(02) Title", &
			04, 05, "(03) Minimum", &
			05, 05, "(04) Maximum", &
			06, 05, "(05) Type", &
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

 Reenter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Reference _#\*
	!
	! Index:
	!
	!--

			UTL_EDI_DATAELEMENT::REFERENCE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "02;25", TEMP$, &
				UTL_EDI_DATAELEMENT::REFERENCE, MFLAG, "'E", &
				MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Title\*
	!	.b
	!	.lm +5
	!	The ^*Title\* field enters the title or definition of
	!	the code in field (01).
	!	.b
	!	The field will contain up to sixty (60) characters of alphanumeric
	!	data.
	!	.lm -5
	!
	! Index:
	!
	!--

			UTL_EDI_DATAELEMENT::TITLE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "3;25", TEMP$, &
				UTL_EDI_DATAELEMENT::TITLE, MFLAG, "'E", &
				MVALUE)
		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Minimum\*
	!
	! Index:
	!
	!--

			UTL_EDI_DATAELEMENT::MMIN = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "4;25", TEMP$, &
				UTL_EDI_DATAELEMENT::MMIN * 1.0, MFLAG, &
				"##", MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Maximum\*
	!
	! Index:
	!
	!--

			UTL_EDI_DATAELEMENT::MMAX = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "5;25", TEMP$, &
				UTL_EDI_DATAELEMENT::MMAX * 1.0, MFLAG, &
				"##", MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Type\*
	!	.b
	!	.lm +5
	!	Pressing ^*<List Choices>\* will cause a list of valid types to be
	!	displayed.
	!	.lm -5
	!
	! Index:
	!
	!--

			UTL_EDI_DATAELEMENT::TTYP = &
				EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;25", TEMP$, &
				UTL_EDI_DATAELEMENT::TTYP, MFLAG, "'E", &
				MVALUE, TYP$(), TYPTITLE$, "005"), -1%)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		UTL_MAIN_ELEMENT = 0%

		SELECT MLOOP

		CASE 1%
			IF UTL_EDI_DATAELEMENT::REFERENCE = ""
			THEN
				UTL_MAIN_ELEMENT = 1%
			ELSE
				SELECT MVALUE
				CASE "ADD"

					WHEN ERROR IN
						GET #UTL_EDI_DATAELEMENT.CH%, &
							KEY #0% EQ UTL_EDI_DATAELEMENT::REFERENCE + "", &
							REGARDLESS
					USE
						CONTINUE ExitProgram IF ERR = 155%
						EXIT HANDLER
					END WHEN

					UTL_MAIN_ELEMENT = 2%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Record Already Exists", 1%)

				END SELECT
			END IF

		END SELECT

	!
	! Test option
	!
	CASE OPT_TESTOPT
		UTL_MAIN_ELEMENT = 0%

	CASE OPT_DISPLAY

	!
	! Set UTL_ELEMENT_OLD value
	!
20500	CASE OPT_SETOLD
		UTL_EDI_DATAELEMENT_OLD = UTL_EDI_DATAELEMENT

	!
	! Restore UTL_ELEMENT_OLD value
	!
	CASE OPT_RESETOLD
		UTL_EDI_DATAELEMENT = UTL_EDI_DATAELEMENT_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		UTL_EDI_DATAELEMENT2 = UTL_EDI_DATAELEMENT

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		UTL_EDI_DATAELEMENT = UTL_EDI_DATAELEMENT2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "   Element  Title                       "

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "012"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = "  " + &
				UTL_EDI_DATAELEMENT::REFERENCE + "  " + &
				UTL_EDI_DATAELEMENT::TITLE

		END SELECT
	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #UTL_EDI_DATAELEMENT.CH%, &
				KEY #0% GE UTL_EDI_DATAELEMENT::REFERENCE + "", &
				REGARDLESS

		CASE 1%
			FIND #UTL_EDI_DATAELEMENT.CH%, &
				KEY #1% GE UTL_EDI_DATAELEMENT::TITLE + "", &
				REGARDLESS

		END SELECT

	!
	! Handle finishing various options specially
	!
	CASE OPT_AFTEROPT

		SELECT SCOPE::PRG_ITEM
		!
		! Add code list
		!
		CASE "Add"
			UTL_MAIN_ELEMENT = MAIN_JOURNAL(UTL_MAIN_CODELIST.ID, "A")


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
			IF (UTL_EDI_DATAELEMENT_OLD::REFERENCE <> UTL_EDI_DATAELEMENT::REFERENCE)
			THEN
				TEMP$ = UTL_EDI_DATAELEMENT::REFERENCE
				UTL_EDI_DATAELEMENT = UTL_EDI_DATAELEMENT_OLD
				UTL_MAIN_ELEMENT = &
					MAIN_JOURNAL(UTL_MAIN_CODELIST.ID, "C" + TEMP$)
			END IF

		!
		! Erase record
		!
		CASE "Erase"
			!
			! Erase any line items under the header
			!
			UTL_MAIN_ELEMENT = MAIN_JOURNAL(UTL_MAIN_CODELIST.ID, "E")

		END SELECT

	END SELECT

 ExitProgram:
	EXIT FUNCTION

29000	!
	! Trap Errors
	!
	ON ERROR GO BACK

29900	!
	! Handle no header for Line function here
	!
	CALL ENTR_3MESSAGE(SCOPE, "Sorry, but there is no current header item", 0%)

32767	END FUNCTION
