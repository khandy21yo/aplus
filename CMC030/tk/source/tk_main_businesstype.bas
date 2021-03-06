1	%TITLE "Standard Industrial Codes"
	%SBTTL "TK_MAIN_BUSINESSTYPE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG TK_MAIN_BUSINESSTYPE(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	Maintains the Standard Industrial Codes.
	!	file
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_MAIN_BUSINESSTYPE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN TK_MAIN_BUSINESSTYPE
	!	$ DELETE TK_MAIN_BUSINESSTYPE.OBJ;*
	!
	! Author:
	!
	!	10/13/88 - J. Shad Rydalch
	!
	! Modification history:
	!
	!	06/11/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/29/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/04/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	11/17/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:TK_WINDOW.INC"

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[TK.OPEN]TK_SIC.HB"
	MAP	(TK_SIC)	TK_SIC_CDD	TK_SIC

	%INCLUDE "SOURCE:[TK.OPEN]TK_BUSINESSTYPE.HB"
	MAP (TK_BUSINESSTYPE)	TK_BUSINESSTYPE_CDD	TK_BUSINESSTYPE
	MAP (TK_BUSINESSTYPE_OLD) TK_BUSINESSTYPE_CDD TK_BUSINESSTYPE_OLD, &
		TK_BUSINESSTYPE2

	MAP (CH_TK_MAIN_BUSINESSTYPE) TK_BUSINESSTYPE.CH%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_WINDOW

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!
	! Initilization
	!
	CASE OPT_INIT

		!*************************************************************
		! Set up information
		!*************************************************************

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Standard Industrial Codes"
		SMG_WINDOW::NHELP = "TK_MAIN_BUSINESSTYPE"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 2%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "BusinessType"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%

		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%

		CALL READ_DEFAULTS(SMG_WINDOW)

700		IF (TK_BUSINESSTYPE.CH% <= 0%)
		THEN
			!
			! Open main file (existing) for modification
			!
			WHEN ERROR IN
				%INCLUDE "SOURCE:[TK.OPEN]TK_BUSINESSTYPE.CRE"
			USE
				CALL ENTR_3MESSAGE(SCOPE, &
					"Unable to open TK_BUSINESSTYPE file " + NUM1$(ERR), 0%)
				TK_MAIN_BUSINESSTYPE = 1%
				CONTINUE 27000
			END WHEN
		END IF

710		SMG_WINDOW::CHAN  = TK_BUSINESSTYPE.CH%

		WHEN ERROR IN
			RESET #TK_BUSINESSTYPE.CH%
			GET #TK_BUSINESSTYPE.CH%, REGARDLESS
		USE
			CONTINUE 27000 IF ERR = 11%
			EXIT HANDLER
		END WHEN


	!
	! Display window background
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)


		DATA	04,05, "(01) Business Type", &
			05,05, "(02) Description", &
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
 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

			TK_BUSINESSTYPE::BUSINESSTYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "04;37", TEMP$, &
				TK_BUSINESSTYPE::BUSINESSTYPE, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(TK_MAIN_BUSINESSTYPE.ID, &
					"V0  ") = 1%)
				THEN
					TK_SIC::BUSINESSTYPE = &
						TK_BUSINESSTYPE::BUSINESSTYPE

				END IF
				GOTO ReEnter

			END IF


		CASE 2%

			TK_BUSINESSTYPE::DESCRIPTION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"05;37", TEMP$, TK_BUSINESSTYPE::DESCRIPTION, &
				MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		SELECT MLOOP

		CASE 1%
			IF TK_BUSINESSTYPE::BUSINESSTYPE = ""
			THEN
				TK_MAIN_BUSINESSTYPE = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #SMG_WINDOW::CHAN, &
							KEY #0% EQ TK_BUSINESSTYPE::BUSINESSTYPE + "", &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					TK_MAIN_BUSINESSTYPE = 2%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Record Already Exists", 0%)
				END IF
			END IF

		END SELECT


	!
	! Set TK_BUSINESSTYPE_OLD value
	!
20500	CASE OPT_SETOLD
		TK_BUSINESSTYPE_OLD = TK_BUSINESSTYPE

	!
	! Restore TK_BUSINESSTYPE_OLD value
	!
	CASE OPT_RESETOLD
		TK_BUSINESSTYPE = TK_BUSINESSTYPE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		TK_BUSINESSTYPE2 = TK_BUSINESSTYPE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		TK_BUSINESSTYPE = TK_BUSINESSTYPE2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
		!	          1212 | 1234567890123456789012345678901234567890
			MVALUE = "  BT   Description                             "

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "006,"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = TK_BUSINESSTYPE::BUSINESSTYPE + "   " + &
				TK_BUSINESSTYPE::DESCRIPTION
		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE TK_BUSINESSTYPE::BUSINESSTYPE + "", &
				REGARDLESS
		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION

