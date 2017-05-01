1	%TITLE "Maintain List of EDI Codes"
	%SBTTL "UTL_MAIN_CODELIST"
	%IDENT "V3.6a Calico"

	FUNCTION LONG UTL_MAIN_CODELIST(CDD_WINDOW_CDD SMG_WINDOW, &
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
	! ID:0120
	!
	! Abstract:HELP
	!	.p
	!	This program maintains the EDI code list definition file.
	!
	! Index:
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_MAIN_CODELIST/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN UTL_MAIN_CODELIST
	!	$ DELETE UTL_MAIN_CODELIST.OBJ;*
	!
	! Author:
	!
	!	03/15/88 - Robert Peterson
	!
	! Modification history:
	!
	!	05/23/88 - Lance Williams
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
	!	10/20/98 - Kevin Handy
	!		Lose unused SLINE field in array
	!
	!	03/08/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	06/09/2000 - Kevin Handy
	!		Lose confusion with UTL_ACCOUNT/UTL_EDI_CODELIST
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

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_EDI_CODELIST.HB"
	MAP (UTL_EDI_CODELIST)		UTL_EDI_CODELIST_CDD	UTL_EDI_CODELIST
	MAP (UTL_EDI_CODELIST_OLD) UTL_EDI_CODELIST_CDD UTL_EDI_CODELIST_OLD, &
		UTL_EDI_CODELIST2

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_EDI_DATAELEMENT.HB"
	MAP (UTL_EDI_DATAELEMENT) UTL_EDI_DATAELEMENT_CDD UTL_EDI_DATAELEMENT

	COM (CH_UTL_EDI_CODELIST) &
		UTL_EDI_CODELIST.CH%, &
		UTL_EDI_CODELIST.READONLY%

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
	END RECORD

	MAP (TT_UTL_EDI_CODELIST) RARRAY_RECORD RARRAY(300%)

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Code List"
		SMG_WINDOW::NHELP = "UTL_MAIN_CODELIST"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 11%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 9%
		SMG_WINDOW::NITEMS= 2%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 3%
		SMG_WINDOW::BOTLIN = 10%

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
		IF UTL_EDI_CODELIST.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF UTL_EDI_CODELIST.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_EDI_CODELIST.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			UTL_MAIN_CODELIST = ERR
			CONTINUE 770
		END WHEN

		UTL_EDI_CODELIST.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_EDI_CODELIST.OPN"
		USE
			UTL_MAIN_CODELIST = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		UTL_EDI_CODELIST.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(UTL_EDI_CODELIST.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = UTL_EDI_CODELIST.CH%

		WHEN ERROR IN
			RESET #UTL_EDI_CODELIST.CH%
			GET #UTL_EDI_CODELIST.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!*
	!*  Optional menu items
	!*
5000	CASE OPT_MOREMENU

	!
	! Display the background
	!
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"   (01)       (02)     " + SPACE$(57%), &
			1%, 1%, , SMG$M_REVERSE)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"   Code       Definition" + SPACE$(57%), &
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
			FORMAT$(SMG_WINDOW::TOTREC, "###") + SPACE$(60%), &
			SMG_WINDOW::VSIZE, 1%, , SMG$M_REVERSE)

		!
		! Paint lines on screen
		!
		FOR I% = 1% TO 1%

			A% = VAL%(MID("012", I% * 4% - 3%, 3%))

			SMG_STATUS% = SMG$DRAW_LINE(SMG_WINDOW::WNUMBER, &
				1%, A%, SMG_WINDOW::BOTLIN, A%)

		NEXT I%

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Enter/Display/Default
	!
20200	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		XLINE$ = NUM1$(SMG_WINDOW::CURLIN)

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
			UTL_EDI_CODELIST::CODE = LEFT(ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";4", TEMP$, &
				LEFT(UTL_EDI_CODELIST::CODE, &
				UTL_EDI_DATAELEMENT::MMAX), &
				MFLAG, "'E", MVALUE), &
				UTL_EDI_DATAELEMENT::MMAX)

			IF LEN(UTL_EDI_CODELIST::CODE) < &
				UTL_EDI_DATAELEMENT::MMIN
			THEN
				CALL ENTR_3MESSAGE(SCOPE, "Code is to short", 0%)
				GOTO E0Loop
			END IF

		CASE 2%
			UTL_EDI_CODELIST::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, XLINE$ + ";15", TEMP$, &
				UTL_EDI_CODELIST::DESCR, MFLAG, &
				"'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		UTL_MAIN_CODELIST = 0%

		SELECT MLOOP

		CASE 1%
			IF UTL_EDI_CODELIST::CODE = ""
			THEN
				UTL_MAIN_CODELIST = 1%
			ELSE
				SELECT MVALUE
				CASE "ADD"

					WHEN ERROR IN
						GET #UTL_EDI_CODELIST.CH%, &
							KEY #0% EQ UTL_EDI_CODELIST::REFERENCE + &
							UTL_EDI_CODELIST::CODE, &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					UTL_MAIN_CODELIST = 2%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Record Already Exists", 1%)

				END SELECT
			END IF

		END SELECT

	!
	! Set UTL_EDI_CODELIST_OLD value
	!
20500	CASE OPT_SETOLD
		UTL_EDI_CODELIST_OLD = UTL_EDI_CODELIST

	!
	! Restore UTL_EDI_CODELIST_OLD value
	!
	CASE OPT_RESETOLD
		UTL_EDI_CODELIST = UTL_EDI_CODELIST_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		UTL_EDI_CODELIST2 = UTL_EDI_CODELIST

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		UTL_EDI_CODELIST = UTL_EDI_CODELIST2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		UTL_EDI_CODELIST::REFERENCE = UTL_EDI_DATAELEMENT::REFERENCE

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #UTL_EDI_CODELIST.CH%, &
				KEY #0% GE UTL_EDI_CODELIST::REFERENCE + "", &
				REGARDLESS
		END SELECT

	!*
	!*  Handle various finished options specially
	!*
	CASE OPT_AFTEROPT

		SELECT SCOPE::PRG_ITEM

		!*
		!*  Change records
		!*
		CASE "Change"

			!*
			!*  Change line items to match new header if the
			!*    key was changed.
			!*
			!*  The original record must be the one in the
			!*    MAP for this to be able to work.  The new
			!*    key is passed through the QUERY$ variable.
			!*
			IF (UTL_EDI_CODELIST_OLD::CODE <> &
				UTL_EDI_CODELIST::CODE)
			THEN
				TEMP$ = UTL_EDI_CODELIST::REFERENCE + &
					UTL_EDI_CODELIST::CODE
				UTL_EDI_CODELIST = UTL_EDI_CODELIST_OLD
			END IF

		!*
		!*  Erase record
		!*
		CASE "Erase"

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
					KEY #0% GE UTL_EDI_DATAELEMENT::REFERENCE + "", &
					REGARDLESS
			USE
				CONTINUE 28000
			END WHEN

27120			!
			! Get a record
			!
			WHEN ERROR IN
				GET #SMG_WINDOW::CHAN
			USE
				CONTINUE 28000 IF ERR = 11%
				EXIT HANDLER
			END WHEN

			!
			! Add information to array
			!

			IF (UTL_EDI_DATAELEMENT::REFERENCE = &
				UTL_EDI_CODELIST::REFERENCE)
			THEN
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
		! new key is probibly passes through MVALUE, unless some
		! other means is devised.
		!
		CASE 6%
			UTL_EDI_CODELIST::REFERENCE = MID(MVALUE, 2%, 4%)

		END SELECT
	END SELECT

28000	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
