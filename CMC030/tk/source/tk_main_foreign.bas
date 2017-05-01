1	%TITLE "Maintain the Foreign Key"
	%SBTTL "TK_MAIN_FOREIGN"
	%IDENT "V3.6a Calico"

	FUNCTION LONG TK_MAIN_FOREIGN(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1987,1988 BY
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
	! Computer Management Center.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	! Abstract:HELP
	!	.p
	!	Maintains the foreign keys.
	!
	! Index:
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_MAIN_FOREIGN/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP TK_MAIN_FOREIGN
	!	$ DELETE TK_MAIN_FOREIGN.OBJ;*
	!
	! Author:
	!
	!	01/25/89 - Robert Peterson
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/29/96 - Kevin Handy
	!		Reformat source code
	!
	!	06/04/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/28/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/08/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	06/09/99 - Kevin Handy
	!		Remove lines 760, 770 (Dead Code)
	!
	!	11/22/2000 - Kevin Handy
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
	%INCLUDE "FUNC_INCLUDE:TK_WINDOW.INC"

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[TK.OPEN]TK_FOREIGN.HB"
	MAP (TK_FOREIGN)	TK_FOREIGN_CDD	TK_FOREIGN
	MAP (TK_FOREIGN_OLD)	TK_FOREIGN_CDD	TK_FOREIGN_OLD, TK_FOREIGN2

	%INCLUDE "SOURCE:[TK.OPEN]TK_FILEDICT.HB"
	MAP	(TK_FILEDICT)	TK_FILEDICT_CDD	TK_FILEDICT

	!
	! Create array to contain pointers
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
	END RECORD

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (TT_TK_FOREIGN) RARRAY_RECORD RARRAY(3000%)	! Allocate for 3000

	COM (CH_TK_FOREIGN) &
		TK_FOREIGN.CH%

	COM (FIELD_NAME) &
		STRING SUBSTRUCT = 50%, &
		STRING FIELD_NAME(300%) = 64%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION FUNC_TESTINTEGRITY
	EXTERNAL LONG   FUNCTION MAIN_WINDOW

	ON ERROR GOTO 29000

	%PAGE

	!
	! List of types
	!
	ASTITLE$ = "  Code  Description"
	AS$(0%) = "2"
	AS$(1%) = "1    One"
	AS$(2%) = "M    Many"

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
		SMG_WINDOW::DESCR = "Foreign Keys"
		SMG_WINDOW::NHELP = "TK_MAIN_FOREIGN"
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 9%
		SMG_WINDOW::HPOS  = 4%
		SMG_WINDOW::VPOS  = 10%
		SMG_WINDOW::NITEMS= 4%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 3%
		SMG_WINDOW::BOTLIN = 8%

		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Declare channels
		!
		IF TK_FOREIGN.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF TK_FOREIGN.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[TK.OPEN]TK_FOREIGN.CRE"
		USE
			CONTINUE 32767
		END WHEN

		TK_FOREIGN.READONLY% = 0%
		GOTO 790

 !760
		!
		! If unable to open for modify, try to open
		! with read access only.
		!
 !		%INCLUDE "SOURCE:[TK.OPEN]TK_FOREIGN.OPN"
 !		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
 !		TK_FOREIGN.READONLY% = -1%
 !
 !		GOTO 790
 !
 !770
		!
		! File not able to open, so reset channel
		!
 !		CALL ASSG_FREECHANNEL(TK_FOREIGN.CH%)
 !
 !		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = TK_FOREIGN.CH%

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

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  (01)                 (02)             " + &
			"              (03)  (04)  " + SPACE$(10%), &
			1%, 1%, , SMG$M_REVERSE)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  Field Name           Record Structure " + &
			"              Assoc iation" + SPACE$(10%), &
			2%, 1%, , SMG$M_REVERSE)

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Extra display stuff
	!
	CASE OPT_DISPLAY

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		!
		! Display totals
		!
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"Number of lines" + &
			FORMAT$(SMG_WINDOW::TOTREC, "#####") + &
			SPACE$(56%), &
			SMG_WINDOW::VSIZE, 1%, , SMG$M_REVERSE)


		! Paint lines on screen
		!
		FOR I% = 1% TO 4%
			A% = VAL%(MID("023,054,060,067", I% * 4% - 3%, 3%))

			SMG_STATUS% = SMG$DRAW_LINE(SMG_WINDOW::WNUMBER, &
				1%, A%, SMG_WINDOW::BOTLIN, A%)

		NEXT I%

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

		XLINE$ = NUM1$(SMG_WINDOW::CURLIN)

		TEMP1% = SCOPE::SCOPE_EXIT

 ELoop:		SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
			TK_FOREIGN::FLDNAMES = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";3", TEMP$, &
				TK_FOREIGN::FLDNAMES, MFLAG, &
				"'LLLLLLLLLLLLLLLLLLL", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				X% = ENTR_3CHOICE(SCOPE, "", "", &
					FIELD_NAME(), "", &
					2% + 8% + 64%, "  Field Name", &
					"022", 0%)

				GOTO Eloop IF X% <= 0%

				LOC_COMMA$ = ""
				LOC_COMMA$ = ", " IF TK_FOREIGN::FLDNAMES <> ""
				TK_FOREIGN::FLDNAMES = &
					TRM$(TK_FOREIGN::FLDNAMES) + &
					LOC_COMMA$ + &
					TRM$(FIELD_NAME(X%))
				GOTO Eloop
			END IF

		CASE 2%
			TK_FOREIGN::STRUCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";24", TEMP$, &
				TK_FOREIGN::STRUCT, MFLAG, &
				"'LLLLLLLLLLLLLLLLLLLLLLLLLLLL", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(TK_MAIN_FILEDICT.ID, "VX") = 1%)
				THEN
					TK_FOREIGN::STRUCT = &
						TK_FILEDICT::FILENAME
				END IF
				GOTO ELoop
			END IF

		CASE 3%
			TK_FOREIGN::SUBASSOCIATE = ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";55", TEMP$, &
				TK_FOREIGN::SUBASSOCIATE, MFLAG, &
				"'E", MVALUE, AS$(), ASTITLE$, "007")

		CASE 4%
			TK_FOREIGN::ASSOCIATE = ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";61", TEMP$, &
				TK_FOREIGN::ASSOCIATE, MFLAG, &
				"'E", MVALUE, AS$(), ASTITLE$, "007")

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		TK_MAIN_FOREIGN = 0%

		SELECT MLOOP

		CASE 1%
			TK_MAIN_FOREIGN = 1% IF TK_FOREIGN::FLDNAMES = ""

			LOC_FLDNAMES$ = TK_FOREIGN::FLDNAMES

			WHILE LOC_FLDNAMES$ <> ""
				LOC_COMMA% = 0%
				LOC_COMMA% = INSTR(1%, LOC_FLDNAMES$, ",")
				LOC_COMMA% = LEN(LOC_FLDNAMES$) + 1% &
					IF LOC_COMMA% = 0%
				LOC_TEST$ = EDIT$(LEFT(LOC_FLDNAMES$, &
					LOC_COMMA% - 1%), -1%)
				LOC_FLDNAMES$ = EDIT$(RIGHT(LOC_FLDNAMES$, &
					LOC_COMMA% + 1%), -1%)
				LOC_TEST% = 0%
				LOC_TEST% = -1% &
					IF LOC_TEST$ = TRM$(FIELD_NAME(LOOP%)) &
					FOR LOOP% = 1% TO VAL%(FIELD_NAME(0%))
				IF LOC_TEST% = 0%
				THEN
					CALL ENTR_3MESSAGE(SCOPE, &
						'Undefined field:  "' + &
						LOC_TEST$ + '"  - Please'+ &
						" enter again", 1%)
					TK_MAIN_FOREIGN = 1%
				END IF
			NEXT

		!
		! Is the file structure defined?
		!
		CASE 2%
			IF TK_FOREIGN::STRUCT = ""
			THEN
				TK_MAIN_FOREIGN = 1%
			ELSE
				TK_MAIN_FOREIGN = FUNC_TESTINTEGRITY(SMG_WINDOW, &
					TK_FOREIGN::STRUCT, &
					"TK", &
					SCOPE::PRG_PROGRAM, &
					"TK_FILEDICT", &
					"Structure Dictionary", &
					TK_MAIN_FILEDICT.ID, &
					1%, &
					1%)
			END IF

		END SELECT

	!
	! Set TK_FOREIGN_OLD value
	!
20500	CASE OPT_SETOLD
		TK_FOREIGN_OLD = TK_FOREIGN

	!
	! Restore TK_FOREIGN_OLD value
	!
	CASE OPT_RESETOLD
		TK_FOREIGN = TK_FOREIGN_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		TK_FOREIGN2 = TK_FOREIGN

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		TK_FOREIGN = TK_FOREIGN2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		TK_FOREIGN::SUBSTRUCT = SUBSTRUCT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #TK_FOREIGN.CH%, &
				KEY #1% GE TK_FOREIGN::SUBSTRUCT + "", &
				REGARDLESS
		END SELECT

	!
	! Handle array of records
	!
27000	CASE OPT_ARRAY

		!
		! Select sub-option of array
		!
		SELECT MLOOP

		!
		! Load array with line items
		!
		CASE 1%

			!
			! Empty array
			!
			SMG_WINDOW::TOTREC = 0%

27110			!
			! Search for first record
			!
			WHEN ERROR IN
				FIND #SMG_WINDOW::CHAN, &
					KEY #1% GE SUBSTRUCT + "", &
					REGARDLESS
			USE
				CONTINUE ExitFunction
			END WHEN

27120			!
			! Get a record
			!
			WHEN ERROR IN
				GET #SMG_WINDOW::CHAN
			USE
				CONTINUE ExitFunction IF ERR = 11%
				EXIT HANDLER
			END WHEN

			IF TK_FOREIGN::SUBSTRUCT = SUBSTRUCT
			THEN
				!
				! Add information to array
				!
				SMG_WINDOW::TOTREC = SMG_WINDOW::TOTREC + 1%
				RARRAY(SMG_WINDOW::TOTREC)::LINRFA = &
					GETRFA(SMG_WINDOW::CHAN)
				GOTO 27120
			END IF

		!
		! Remove one element of the array
		!
		CASE 2%
			!
			! Remove item pointed to by Mflag
			!
			FOR I% = MFLAG TO SMG_WINDOW::TOTREC - 1%
				RARRAY(I%) = RARRAY(I% + 1%)
			NEXT I%

		!
		! Set array item to current record
		!
		CASE 3%
			RARRAY(MFLAG)::LINRFA = GETRFA(SMG_WINDOW::CHAN)

		!
		! Load in current record, locked
		!
		CASE 4%
27200			GET #SMG_WINDOW::CHAN, RFA RARRAY(MFLAG)::LINRFA

		!
		! Load in current record, unlocked
		!
		CASE 5%
			GET #SMG_WINDOW::CHAN, RFA RARRAY(MFLAG)::LINRFA, &
				REGARDLESS

		!
		! Change the current record's key to match header.  The
		! new key is probably passes through MVALUE, unless some
		! other means is devised.
		!
		CASE 6%
			TK_FOREIGN::SUBSTRUCT = MID(MVALUE, 2%, 50%)

		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
