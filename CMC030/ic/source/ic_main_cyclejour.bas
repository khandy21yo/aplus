1	%TITLE "Cycle Count Journal"
	%SBTTL "IC_MAIN_CYCLEJOUR"
	%IDENT "V3.6a Calico"

	FUNCTION LONG IC_MAIN_CYCLEJOUR(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	The
	!	^*Cycle Count Journal\* maintains a journal where
	!	cycle count entries may be made.
	!	.lm -5
	!
	! Index:
	!	.x Add>Cycle Counting Journal Entry
	!	.x Erase>Cycle Counting Journal Entry
	!	.x Change>Cycle Counting Journal Entry
	!	.x Maintain>Cycle Counting Journal Entry
	!	.x Cycle Counting Journal Entry>Maintenance
	!	.x Cycle Counting Journal Entry>Add
	!	.x Cycle Counting Journal Entry>Erase
	!	.x Cycle Counting Journal Entry>Change
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_MAIN_CYCLEJOUR/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP IC_MAIN_CYCLEJOUR
	!	$ DELETE IC_MAIN_CYCLEJOUR.OBJ;*
	!
	! Author:
	!
	!	07/31/88 - Frank Starman
	!
	! Modification history:
	!
	!	04/06/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/23/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/31/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/12/94 - Kevin Handy
	!		Try to fix program so that erasing the header
	!		will cause the line items to delete.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/27/97 - Kevin Handy
	!		Use integer for #key
	!
	!	12/24/97 - Kevin Handy
	!		Added option to fill out the counts with
	!		remaining products ('FILL'). For KBJ
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/16/98 - Kevin Handy
	!		Allow more chatracters to be entered into the
	!		fillout category.
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	11/19/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:IC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[IC.OPEN]IC_CYCLEJOUR.HB"
	MAP (IC_CYCLEJOUR)	IC_CYCLEJOUR_CDD	IC_CYCLEJOUR
	MAP (IC_CYCLEJOUR2)	IC_CYCLEJOUR_CDD	IC_CYCLEJOUR_OLD, IC_CYCLEJOUR2

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSTYPE.HB"
	MAP (UTL_TRANSTYPE)	UTL_TRANSTYPE_CDD	UTL_TRANSTYPE

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (BATCH_IC_CYCLEJOUR) &
		BATCH_NO$ = 2%

	COM (CH_IC_CYCLEJOUR) &
		IC_CYCLEJOUR.CH%, &
		IC_CYCLEJOUR.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG		FUNCTION IC_FUNC_JOURSET
	EXTERNAL LONG		FUNCTION IC_FUNC_JOURRESET
	EXTERNAL LONG		FUNCTION IC_FUNC_COPYBATCH
	EXTERNAL LONG		FUNCTION IC_FUNC_COPYCAT
	EXTERNAL LONG		FUNCTION IC_FUNC_FILLOUT
	EXTERNAL LONG		FUNCTION FUNC_TESTENTRY

	!
	! Declare data types
	!
	DECLARE LONG XPOS, YPOS

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!
	CASE OPT_INIT

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Cycle Count Journal " + BATCH_NO$
		SMG_WINDOW::NHELP = "IC_MAIN_CYCLEJOUR"
		SMG_WINDOW::CHAN  = IC_CYCLEJOUR.CH%
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 7%

		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Location"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF IC_CYCLEJOUR.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF IC_CYCLEJOUR.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_CYCLEJOUR.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			IC_MAIN_CYCLEJOUR = ERR
			CONTINUE 770
		END WHEN

		IC_CYCLEJOUR.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_CYCLEJOUR.OPN"
		USE
			IC_MAIN_CYCLEJOUR = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		IC_CYCLEJOUR.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		!IF FLAGOPEN%
		!THEN
		!	IC_CYCLEJOUR.CH% = 0%
		!ELSE
		!	IC_CYCLEJOUR.CH% = -IC_CYCLEJOUR.CH%
		!END IF

		EXIT FUNCTION

790		SMG_WINDOW::CHAN = IC_CYCLEJOUR.CH%
		WHEN ERROR IN
			RESET #IC_CYCLEJOUR.CH%
			GET #IC_CYCLEJOUR.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display the background
	!
	! This option is used to display the background information on the
	! screen.  It must first clear any junk on the screen, and then
	! write the background onto it.
	!
20100	CASE OPT_BACKGROUND

		!
		! Main screen
		!
		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	02,05, "(01) Location", &
			03,05, "(02) Count Date", &
			04,05, "(03) Trans Type", &
			06,05, "(04) Prim Ref", &
			07,05, "(06) Cross Ref", &
			08,05, "(07) Subaccount", &
			09,05, "(08) Operator", &
			0, 0, ""

		RESTORE

		READ XPOS, YPOS, XSTR$
		I% = 0%
		WHILE (XPOS <> 0%)
			I% = I% + 1%
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				XSTR$, XPOS, YPOS) &
				IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%
			READ XPOS, YPOS, XSTR$
		NEXT

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! More options
	!
	CASE OPT_OPTLIST
		MVALUE = MVALUE + " counT filL-out adjuSt copY catcoPy"

	!
	! Handle additional options
	!
	CASE OPT_MOREMENU
		SELECT EDIT$(MVALUE, -1%)

		!
		! Units transaction entry
		!
		CASE "COUNT"
	!++
	! Abstract:COUNT
	!	^*Count\*
	!	.b
	!	.lm +5
	!	The ^*Count\* option
	!	maintains the Product _#, Quantity per Pack, and Quantity
	!	per Unit.
	!	.lm -5
	!
	! Index:
	!	.x counT>Function
	!	.x Maintain>Quanity per Unit
	!
	!--
			IF IC_FUNC_JOURRESET = 0%
			THEN
				IC_MAIN_CYCLEJOUR = &
					MAIN_JOURNAL(IC_MAIN_JOURCOUNT.ID, "")
			END IF

		!
		! Units transaction entry
		!
		CASE "FILL-OUT"
	!++
	! Abstract:FILL-OUT
	!	^*Fill\*
	!	.b
	!	.lm +5
	!	Fills out the count journal with any remaining products
	!	that are missing in the Count.
	!	.lm -5
	!
	! Index:
	!
	!--
			IF IC_FUNC_JOURRESET = 0%
			THEN
 FillCategory:
				CATEGORY$ = ENTR_3STRING(SCOPE, &
					SMG_WINDOW::WNUMBER, "", &
					"Fillout Wildcard Category", &
					"*          ", 0%, "'E", MVALUE)

				SELECT SCOPE::SCOPE_EXIT
				!
				! Control/C
				!
				CASE 3%, SMG$K_TRM_UP
					GOTO FillCategory	! (Ignored)

				!
				! Downarrow
				!
				CASE SMG$K_TRM_DOWN
					GOTO FillCategory

				!
				! Exit key typed
				!
				CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

				!
				! Good Keys
				!
				CASE 0%, 10%, 12%, &
					SMG$K_TRM_DO, SMG$K_TRM_CR, &
					SMG$K_TRM_SELECT

					IC_MAIN_CYCLEJOUR = &
						IC_FUNC_FILLOUT(IC_CYCLEJOUR::LOCATION, &
						CATEGORY$)

				!
				! Bad Keys
				!
				CASE ELSE
					CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
					GOTO FillCategory

				END SELECT

			END IF

		!
		! Units posting file
		!
		CASE "ADJUST"
	!++
	! Abstract:ADJUST
	!	^*Adjust\*
	!	.b
	!	.lm +5
	!	The ^*Adjust\* option makes
	!	adjustments according to the product _#, quantity per Unit, and Account _#.
	!	.lm -5
	!
	! Index:
	!	.x adjuSt>Function
	!
	!--
			IF IC_FUNC_JOURSET = 0%
			THEN
				IC_MAIN_CYCLEJOUR = &
					MAIN_JOURNAL(IC_MAIN_JOURADJUST.ID, "")
			END IF

		!
		! Copy batch template
		!
		CASE "COPY"
	!++
	! Abstract:COPY
	!	^*Copy\*
	!	.b
	!	.lm +5
	!	The ^*Copy\* option copies
	!	a selected week from the cycle count map into the cycle count journal.
	!	.b
	!	The ^*Copy\* option requires a valid cycle count journal header and uses
	!	the location in the header to create the count portion of the cycle
	!	count journal.
	!	.b
	!	This option allows the user to duplicate a worksheet used in the physical
	!	inventory count and then use the ^*initialize\* option to input  the counted
	!	quantities.
	!	.lm -5
	!
	! Index:
	!	.x Copy>Function
	!
	!--
			IC_MAIN_CYCLEJOUR = IC_FUNC_COPYBATCH

		!
		! Copy batch template
		!
		CASE "CATCOPY"
	!++
	! Abstract:CATCOPY
	!	^*Catcopy\*
	!	.b
	!	.lm +5
	!	The ^*Catcopy\* option copies
	!	a selected week from the cycle count map into the cycle count journal.
	!	.b
	!	The ^*Copy\* option requires a valid cycle count journal header and uses
	!	the location in the header to create the count portion of the cycle
	!	count journal.
	!	.b
	!	This option allows the user to duplicate a worksheet used in the physical
	!	inventory count and then use the ^*initialize\* option to input  the counted
	!	quantities.
	!	.lm -5
	!
	! Index:
	!	.x Copy>Function
	!
	!--
			IC_MAIN_CYCLEJOUR = IC_FUNC_COPYCAT

		END SELECT

	!
	! Handle finishing various options specially
	!
	CASE OPT_AFTEROPT

		SELECT SCOPE::PRG_ITEM

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
			IF IC_CYCLEJOUR_OLD::LOCATION <> IC_CYCLEJOUR::LOCATION
			THEN
				TEMP$ = IC_CYCLEJOUR::LOCATION
				IC_CYCLEJOUR = IC_CYCLEJOUR_OLD

				CALL READ_DEVICE("IC_JOURCOUNT", &
					IC_JOURCOUNT.DEV$, STAT%)
				IC_MAIN_CYCLEJOUR = &
					MAIN_JOURNAL(IC_MAIN_JOURCOUNT.ID, &
					"C" + TEMP$) &
					IF FIND_FILEEXISTS(IC_JOURCOUNT.DEV$ + &
					"IC_JOURCOUNT_" + BATCH_NO$ + ".JRL", &
					0%)

				CALL READ_DEVICE("IC_JOURPOST", &
					IC_JOURPOST.DEV$, STAT%)
				IC_MAIN_CYCLEJOUR = &
					MAIN_JOURNAL(IC_MAIN_JOURADJUST.ID, "C" + TEMP$) &
					IF FIND_FILEEXISTS(IC_JOURPOST.DEV$ + &
					"IC_JOURPOST_" + BATCH_NO$ + ".JRL", 0%)
			END IF

		!
		! Erase record
		!
		CASE "Erase"
			!
			! Erase any line items under the header
			!
			IC_MAIN_CYCLEJOUR = &
				MAIN_JOURNAL(IC_MAIN_JOURCOUNT.ID, "E")

			CALL READ_DEVICE("IC_JOURPOST", &
				IC_JOURPOST.DEV$, STAT%)
			IC_MAIN_CYCLEJOUR = &
				MAIN_JOURNAL(IC_MAIN_JOURADJUST.ID, "E")


		END SELECT


	!
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
	!
20200	CASE OPT_ENTRY

		TEMP$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View "

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		SCOPE::SCOPE_EXIT = 0%

 Reenter:	SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Location\*
	!	.b
	!	.lm +5
	!	The ^*Location\* field enters a location
	!	code which is established in the Company Profile file located
	!	in the Utility system.
	!	.b
	!	This field will accommodate up to four (4) alphanumeric characters.
	!	.b
	!	Valid Location codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Location Code
	!
	!--

			IC_CYCLEJOUR::LOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "2;30", TEMP$, &
				IC_CYCLEJOUR::LOCATION, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "V0") = 1%
				THEN
					IC_CYCLEJOUR::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO Reenter
			END IF

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Count Date\*
	!	.b
	!	.lm +5
	!	The ^*Count Date\* field enters the actual
	!	date of the count.
	!	.b
	!	The format for entry is MMDDYYYY.
	!	.lm -5
	!
	! Index:
	!	.x Count Date>Cycle Journal
	!	.x Cycle Journal>Count Date
	!
	!--

			IC_CYCLEJOUR::COUNTDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "3;30", TEMP$, &
				IC_CYCLEJOUR::COUNTDATE, MFLAG, &
				"'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Transaction Type\*
	!	.b
	!	.lm +5
	!	The ^*Transaction Type\* field enters the
	!	code for a particular transaction.
	!	.b
	!	Valid Transaction Type codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Transaction Type>Cycle Count Journal
	!	.x Cycle Count Journal>Transaction Type
	!	.x Type>Transaction
	!
	!--

			IC_CYCLEJOUR::TRANSTYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "4;30", TEMP$, &
				IC_CYCLEJOUR::TRANSTYPE, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_TRANSTYPE.ID, "V0") = 1%
				THEN
					IC_CYCLEJOUR::TRANSTYPE = &
						UTL_TRANSTYPE::CODE
				END IF
				GOTO Reenter
			END IF


		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Primary Reference\*
	!	.b
	!	.lm +5
	!	The ^*Primary Reference\* field contains the number which posting in the
	!	General Ledger will be completed under.  An example of a
	!	Primay Reference number is the invoice number.
	!	.lm -5
	!
	! Index:
	!	.x Primary Reference
	!
	!--


			IC_CYCLEJOUR::PRIMREF = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "06;30", TEMP$, &
				IC_CYCLEJOUR::PRIMREF, MFLAG, "'E", &
				MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(06) Cross Reference\*
	!	.b
	!	.lm +5
	!	The ^*Cross Reference\* field refers to the individual who originated the
	!	transaction.  Examples of Cross Reference are a customer or vendor number.
	!	This entry connects the transaction to the person
	!	responsible.
	!	.lm -5
	!
	! Index:
	!	.x Cross Reference
	!
	!--

			IC_CYCLEJOUR::CROSSREF = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "07;30", TEMP$, &
				IC_CYCLEJOUR::CROSSREF, MFLAG, "'E", &
				MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(07) Subaccount\*
	!	.b
	!	.lm +5
	!	The ^*Subaccount\* field creates a cross matrix for the
	!	subledger.  Each entry refers to the specific job identification. Examples of
	!	a ^*Subaccount\* are job _# and work order _#.
	!	.lm -5
	!
	! Index:
	!	.x Subaccount
	!
	!--

			IC_CYCLEJOUR::SUBACCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "08;30", TEMP$, &
				IC_CYCLEJOUR::SUBACCT, MFLAG, "'E", &
				MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	^*(08) Operator\*
	!	.b
	!	.lm +5
	!	The ^*Operator\* field enters the code which
	!	identifies the person responsible for a particular cycle
	!	count entry.
	!	.b
	!	The field will accommodate an entry of ten (10) alphanumeric
	!	characters.
	!	.lm -5
	!
	! Index:
	!	.x Operator>Cycle Count Journal
	!	.x Cycle Count Journal>Operator
	!
	!--

			IC_CYCLEJOUR::STATIONMAN = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "09;30", TEMP$, &
				IC_CYCLEJOUR::STATIONMAN, MFLAG, "'E", &
				MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

20300	CASE OPT_TESTENTRY

		IC_MAIN_CYCLEJOUR = 0%

		SELECT MLOOP

		CASE 1%
			IF IC_CYCLEJOUR::LOCATION = ""
			THEN
				IC_MAIN_CYCLEJOUR = 1%
			ELSE
				IC_MAIN_CYCLEJOUR = FUNC_TESTENTRY(SMG_WINDOW, &
					IC_CYCLEJOUR::LOCATION, &
					UTL_LOCATION::LOCNAME, &
					"IC", MLOOP, "PROD", &
					"Location", UTL_MAIN_LOCATION.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, 2%, 41%, , SMG$M_BOLD)

			IF (MVALUE = "ADD")
			THEN
				WHEN ERROR IN
					GET #IC_CYCLEJOUR.CH%, &
						KEY #0% EQ IC_CYCLEJOUR::LOCATION + "", &
						REGARDLESS
				USE
					CONTINUE 32767 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				IC_MAIN_CYCLEJOUR = 2%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Record Already Exists", 1%)
			END IF

		CASE 3%
			IF IC_CYCLEJOUR::TRANSTYPE = ""
			THEN
				IC_MAIN_CYCLEJOUR = 1%
			ELSE
				IC_MAIN_CYCLEJOUR = FUNC_TESTENTRY(SMG_WINDOW, &
					IC_CYCLEJOUR::TRANSTYPE, &
					UTL_TRANSTYPE::DESCRIPTION, &
					"IC", MLOOP, "PROD", &
					"Transaction Type", &
					UTL_MAIN_TRANSTYPE.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				STORENAME$, 4%, 41%, , SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			DISPLAYLOC$ = STRING$(40%, 63%)
			DISPLAYLOC$ = UTL_LOCATION::LOCNAME &
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, &
				"Q0" + IC_CYCLEJOUR::LOCATION) = 1%
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				DISPLAYLOC$, 2%, 41%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(3%) AND 2%) = 0%
		THEN
			TRANSDESC$  = STRING$(20%, 63%)
			TRANSDESC$  = UTL_TRANSTYPE::DESCRIPTION &
				IF MAIN_WINDOW(UTL_MAIN_TRANSTYPE.ID, &
				"Q0" + IC_CYCLEJOUR::TRANSTYPE) = 1%
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				TRANSDESC$, 4%, 41%, , SMG$M_BOLD)
		END IF

20500	CASE OPT_SETOLD
		IC_CYCLEJOUR_OLD = IC_CYCLEJOUR

	CASE OPT_RESETOLD
		IC_CYCLEJOUR = IC_CYCLEJOUR_OLD

	CASE OPT_SETDEFAULT
		IC_CYCLEJOUR2 = IC_CYCLEJOUR

	CASE OPT_RESETDEFAULT
		IC_CYCLEJOUR = IC_CYCLEJOUR2

	CASE OPT_VIEW
		SELECT MLOOP

		CASE 1%

			MVALUE = "  Loc  CountDate  TransType PrimRef"

		CASE 2%

			MVALUE = "007,018,028"

		CASE 3%

			MVALUE = IC_CYCLEJOUR::LOCATION + " " + &
				PRNT_DATE(IC_CYCLEJOUR::COUNTDATE, 8%) + " " + &
				IC_CYCLEJOUR::TRANSTYPE + "        " + &
				IC_CYCLEJOUR::PRIMREF
		END SELECT

	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #IC_CYCLEJOUR.CH%, &
				KEY #0% GE IC_CYCLEJOUR::LOCATION + "", &
				REGARDLESS

		END SELECT

	END SELECT

28000	EXIT FUNCTION

29000	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
