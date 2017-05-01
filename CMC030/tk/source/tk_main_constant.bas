1	%TITLE "TK Constants"
	%SBTTL "TK_MAIN_CONSTANT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG TK_MAIN_CONSTANT(CDD_WINDOW_CDD SMG_WINDOW, &
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
	!	Maintains the Constants file.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_MAIN_CONSTANT/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN TK_MAIN_CONSTANT
	!	$ DELETE TK_MAIN_CONSTANT.OBJ;*
	!
	! Author:
	!
	!	01/20/89 - J. Shad Rydalch
	!
	! Modification history:
	!
	!	09/23/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/11/93 - Kevin Handy
	!		Clean up (Check)
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
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
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

	%INCLUDE "$OTSDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	%INCLUDE "FUNC_INCLUDE:TK_WINDOW.INC"

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[TK.OPEN]TK_CONSTANT.HB"
	MAP (TK_CONSTANT)	TK_CONSTANT_CDD		TK_CONSTANT
	MAP (TK_CONSTANT_OLD)	TK_CONSTANT_CDD		TK_CONSTANT_OLD, &
							TK_CONSTANT2

	%INCLUDE "SOURCE:[TK.OPEN]TK_CONSTCLASS.HB"
	MAP (TK_CONSTCLASS)	TK_CONSTCLASS_CDD	TK_CONSTCLASS

	MAP (CH_TK_MAIN_CONSTANT) TK_CONSTANT.CH%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION TK_MAIN_CONSTCLASS
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY
	EXTERNAL LONG	FUNCTION OTS$CVT_L_TB

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
		SMG_WINDOW::DESCR = "CMC Constants"
		SMG_WINDOW::NHELP = "TK_MAIN_CONSTANT"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 3%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "Name"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KNAME(1%) = "Class"
			SMG_WINDOW::KFIELD(1%, 0%) = 2%
			SMG_WINDOW::KFIELD(1%, 1%) = 2%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%

		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%

		CALL READ_DEFAULTS(SMG_WINDOW)

700		IF (TK_CONSTANT.CH% <= 0%)
		THEN
			!
			! Open main file (existing) for modification
			!
			WHEN ERROR IN
				%INCLUDE "SOURCE:[TK.OPEN]TK_CONSTANT.CRE"
			USE
				CALL ENTR_3MESSAGE(SCOPE, &
					"Unable to open TK_CONSTANT file " + NUM1$(ERR), 0%)
				TK_MAIN_CONSTANT = 1%
				CONTINUE 27000
			END WHEN
		END IF

710		SMG_WINDOW::CHAN  = TK_CONSTANT.CH%

		WHEN ERROR IN
			RESET #TK_CONSTANT.CH%
			GET #TK_CONSTANT.CH%, REGARDLESS
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


		DATA	05,05, "(01) Constant Name", &
			06,05, "(02) Class", &
			07,05, "(03) Value", &
			08,05, "     Binary Format", &
			10,38, "Warning...............000", &
			11,38, "Success...............001", &
			12,38, "Error.................010", &
			13,38, "Information...........011", &
			14,38, "Severe Error..........100", &
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

			TK_CONSTANT::CONSTNAME = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "05;32", TEMP$, &
				TK_CONSTANT::CONSTNAME, MFLAG, "'E", &
				MVALUE)

		CASE 2%

			TK_CONSTANT::CLASS = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"06;32", TEMP$, TK_CONSTANT::CLASS, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(TK_MAIN_CONSTCLASS.ID, "V0  ") = 1%)
				THEN
					TK_CONSTANT::CLASS = &
						TK_CONSTCLASS::CLASS
				END IF
				GOTO Reenter
			END IF

		CASE 3%

			TK_CONSTANT::CONST = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"07;32", TEMP$, TK_CONSTANT::CONST * 1.0, &
				MFLAG, "##########", MVALUE)



			BINAR$ = SPACE$(31%)

			V% = OTS$CVT_L_TB(TK_CONSTANT::CONST, BINAR$)

			BINAR$ = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"08;32", TEMP$, BINAR$, 1%, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		TK_MAIN_CONSTANT = 0%

		SELECT MLOOP

		CASE 1%
			IF TK_CONSTANT::CONSTNAME = ""
			THEN
				TK_MAIN_CONSTANT = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #SMG_WINDOW::CHAN, &
							KEY #0% EQ TK_CONSTANT::CONSTNAME + "", &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					TK_MAIN_CONSTANT = 2%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Record Already Exists", 0%)
				END IF
			END IF

		CASE 2%
			!
			! Is the input defined?
			!
			TK_MAIN_CONSTANT = FUNC_TESTENTRY(SMG_WINDOW, &
				TK_CONSTANT::CLASS, &
				TK_CONSTCLASS::DESCRIPTION, &
				"TK", MLOOP, "CONSTANT", &
				"Class", TK_MAIN_CONSTCLASS.ID)

		END SELECT

	!
	! Set TK_CONSTANT_OLD value
	!
20500	CASE OPT_SETOLD
		TK_CONSTANT_OLD = TK_CONSTANT

	!
	! Restore TK_CONSTANT_OLD value
	!
	CASE OPT_RESETOLD
		TK_CONSTANT = TK_CONSTANT_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		TK_CONSTANT2 = TK_CONSTANT

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		TK_CONSTANT = TK_CONSTANT2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%

		MVALUE = "  ConstantName                              Class  " + &
			"  Value"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "043,051"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = TK_CONSTANT::CONSTNAME + "   " + &
				TK_CONSTANT::CLASS + "   " + &
				FORMAT$(TK_CONSTANT::CONST, "##########")
		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE TK_CONSTANT::CONSTNAME + "", &
				REGARDLESS
		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE TK_CONSTANT::CLASS + &
				TK_CONSTANT::CONSTNAME, &
				REGARDLESS
		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
