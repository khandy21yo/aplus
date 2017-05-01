1	%TITLE "Module Relation Maintenance"
	%SBTTL "TK_MAIN_RELATION"
	%IDENT "V3.6a Calico"

	FUNCTION LONG TK_MAIN_RELATION(CDD_WINDOW_CDD SMG_WINDOW, &
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
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_MAIN_RELATION/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP TK_MAIN_RELATION
	!	$ DELETE TK_MAIN_RELATION.OBJ;*
	!
	! Author:
	!
	!	10/08/87 - Frank F. Starman
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
	!		Reformat source code.
	!
	!	06/05/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/21/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[TK.OPEN]TK_RELATION.HB"
	MAP (TK_RELATION)	TK_RELATION_CDD		TK_RELATION
	MAP (TK_RELATION_OLD)	TK_RELATION_CDD		TK_RELATION_OLD, &
		TK_RELATION2, TK_RELATION3

	%INCLUDE "SOURCE:[TK.OPEN]TK_MODULE.HB"
	MAP (TK_MODULE)		TK_MODULE_CDD		TK_MODULE

	MAP (CH_TK_MAIN_RELATION) &
		TK_RELATION.CH%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_WINDOW
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY

	!
	! Dimension
	!
	DIM DEFREF$(20%)

	%PAGE

	ON ERROR GOTO 29000

	!
	! List of types
	!
	DEFREFTITLE$ = "DefRef Description"
	DEFREF$(0%) = "11"
	DEFREF$(1%) = "0    Variable"
	DEFREF$(2%) = "1    Subroutine"
	DEFREF$(3%) = "2    Function"
	DEFREF$(4%) = "3    Record Structure"
	DEFREF$(5%) = "4    Record Map"
	DEFREF$(6%) = "5    Record Field"
	DEFREF$(7%) = "6    Parameter"
	DEFREF$(8%) = "7    Constant"
	DEFREF$(9%) = "8    Common Structure"
	DEFREF$(10%) = "9    Common Field"
	DEFREF$(11%) = "I    Include File"

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
		SMG_WINDOW::DESCR = "Module structure"
		SMG_WINDOW::NHELP = "TK_MAIN_RELATION"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 6%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "Module_name"
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
		SMG_WINDOW::KNAME(1%) = "cOmponent_name"
			SMG_WINDOW::KFIELD(1%, 0%) = 2%
			SMG_WINDOW::KFIELD(1%, 1%) = 2%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%

		SMG_WINDOW::HVIEW = 130%
		SMG_WINDOW::VVIEW = 18%

		CALL READ_DEFAULTS(SMG_WINDOW)

700		IF (TK_RELATION.CH% <= 0%)
		THEN
			!
			! Open main file (existing) for modification
			!
			WHEN ERROR IN
				%INCLUDE "SOURCE:[TK.OPEN]TK_RELATION.CRE"
			USE
				CALL ENTR_3MESSAGE(SCOPE, &
					"Unable to open TK_RELATION file " + NUM1$(ERR), 0%)
				TK_MAIN_RELATION = 1%
				CONTINUE 27000
			END WHEN

		END IF

710		SMG_WINDOW::CHAN  = TK_RELATION.CH%

		WHEN ERROR IN
			RESET #TK_RELATION.CH%
			GET #TK_RELATION.CH%, REGARDLESS
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

		DATA	05,10, "(01) Module name", &
			06,10, "(02) Component ", &
			07,10, "(03) Quantity", &
			08,10, "(04) Def Reference", &
			10,10, "(05) Date", &
			11,10, "(06) Time", &
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

			TK_RELATION::PARENT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "05;30", TEMP$, &
				TK_RELATION::PARENT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(TK_MAIN_MODULE.ID, "V0  ") = 1%)
				THEN
					TK_RELATION::PARENT = &
						TK_MODULE::MODNAME
				END IF
				GOTO Reenter
			END IF

		CASE 2%

			TK_RELATION::CHILD = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "06;30", TEMP$, &
				TK_RELATION::CHILD, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(TK_MAIN_MODULE.ID, "V0  ") = 1%)
				THEN
					TK_RELATION::CHILD = &
						TK_MODULE::MODNAME
				END IF
				GOTO Reenter
			END IF

		CASE 3%

			TK_RELATION::QUANTITY = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "07;30", TEMP$, &
				1.0*TK_RELATION::QUANTITY, MFLAG, "###,###", &
				MVALUE)

		CASE 4%

			TK_RELATION::DEFREF= ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, "08;30", TEMP$, &
				TK_RELATION::DEFREF, MFLAG, "'E", &
				MVALUE, DEFREF$(), DEFREFTITLE$, "007")

		CASE 5%

			TK_RELATION::CDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "10;30", TEMP$, &
				TK_RELATION::CDATE, MFLAG, "'E", MVALUE)


		CASE 6%

			TK_RELATION::CTIME = ENTR_3TIME(SCOPE, &
				SMG_WINDOW::WNUMBER, "11;30", TEMP$, &
				TK_RELATION::CTIME, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		TK_MAIN_RELATION = 0%

		SELECT MLOOP

		CASE 1%
			TK_MAIN_RELATION = FUNC_TESTENTRY( SMG_WINDOW, &
				TK_RELATION::PARENT, &
				TK_MODULE::DESCRIPTION, &
				"TK", MLOOP, "MODULE", &
				"Parent", TK_MAIN_MODULE.ID)

		CASE 2%
			TK_MAIN_RELATION = FUNC_TESTENTRY( SMG_WINDOW, &
				TK_RELATION::CHILD, &
				TK_MODULE::DESCRIPTION, &
				"TK", MLOOP, "SUBMOD", &
				"Child", TK_MAIN_MODULE.ID)

		END SELECT

	!
	! Set TK_RELATION_OLD value
	!
20500	CASE OPT_SETOLD
		TK_RELATION_OLD = TK_RELATION

	!
	! Restore TK_RELATION_OLD value
	!
	CASE OPT_RESETOLD
		TK_RELATION = TK_RELATION_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		TK_RELATION2 = TK_RELATION

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		TK_RELATION = TK_RELATION2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Module                                  " + &
		"Component                               Quantity Ref"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "042,082,091,095"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = TK_RELATION::PARENT + " " + &
				TK_RELATION::CHILD + " " + &
				FORMAT$(TK_RELATION::QUANTITY, &
					" ###,###") + " " + &
				TK_RELATION::DEFREF
		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE TK_RELATION::PARENT + &
				TK_RELATION::CHILD, &
				REGARDLESS
		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE TK_RELATION::CHILD + &
				TK_RELATION::PARENT, &
				REGARDLESS
		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
