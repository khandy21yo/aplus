1	%TITLE "Customer Type Description"
	%SBTTL "AR_MAIN_CUSTYPE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AR_MAIN_CUSTYPE(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	! ID:7826
	!
	! Abstract:HELP
	!	.B
	!	.LM +5
	!	Accessing the ^*Customer Type Description\* file
	!	sets up identifying codes and enters descriptions
	!	for Customer Types which are utilized in the Customer Master
	!	file within the Accounts Receivable System.
	!	.lm -5
	!
	! Index:
	!	.x Customer Type>Table
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_MAIN_CUSTYPE/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AR_MAIN_CUSTYPE
	!	$ DELETE AR_MAIN_CUSTYPE.OBJ;*
	!
	! Author:
	!
	!	03/14/90 - J. Shad Rydalch
	!
	! Modification history:
	!
	!	05/24/91 - Kevin Handy
	!		Removed "EXTERNAL...LIB$FREELUN" which was never
	!		called in the program.
	!
	!	05/11/92 - Frank F. Starman
	!		Allow enter blank customer type.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/20/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/21/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/09/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	05/31/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[AR.OPEN]AR_CUSTYPE.HB"
	MAP (AR_CUSTYPE)		AR_CUSTYPE_CDD	AR_CUSTYPE
	MAP (AR_CUSTYPE_OLD)	AR_CUSTYPE_CDD	AR_CUSTYPE_OLD, AR_CUSTYPE2

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_AR_CUSTYPE) &
		AR_CUSTYPE.CH%, &
		AR_CUSTYPE.READONLY%

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
		! Define SMG_WINDOW
		!
		SMG_WINDOW::DESCR = "Customer Type Description"
		SMG_WINDOW::NHELP = "AR_MAIN_CUSTYPE"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 2%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Custype"
		SMG_WINDOW::KFIELD(0%, 0%) = 1%
		SMG_WINDOW::KFIELD(0%, 1%) = 1%

		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%

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
		IF AR_CUSTYPE.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AR_CUSTYPE.READONLY%
			GOTO 790
		END IF

		IF (AR_CUSTYPE.CH% <= 0%)
		THEN
			!
			! Open main file (existing) for modification
			!
			%INCLUDE "SOURCE:[AR.OPEN]AR_CUSTYPE.CRE"
		END IF

750		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_CUSTYPE.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			AR_MAIN_CUSTYPE = ERR
			CONTINUE 770
		END WHEN

		AR_CUSTYPE.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_CUSTYPE.OPN"
		USE
			AR_MAIN_CUSTYPE = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		AR_CUSTYPE.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(AR_CUSTYPE.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = AR_CUSTYPE.CH%

		WHEN ERROR IN
			RESET #AR_CUSTYPE.CH%
			GET #AR_CUSTYPE.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display SMG_WINDOW background
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)


		DATA	8,  10, "(01) Customer Type", &
			10,  10, "(02) Description", &
			0,   0, ""

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

	!++
	! Abstract:FLD001
	!	.x Customer Type
	!	^*(01) Customer Type\*
	!	.b
	!	.lm +5
	!	The ^*Customer Type\* field
	!	contains codes which identify the type of customer in
	!	the Customer Master File.
	!	.b
	!	Examples:
	!	.table 3,25
	!	.te
	!	^*RC\* = Retail Customer
	!	.te
	!	^*WC\* = Wholesale Customer
	!	.end table
	!	The field will accept up to two characters.
	!	.lm -5
	!
	! Index:
	!
	!--

			AR_CUSTYPE::CUSTYPE = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"8;33", TEMP$, &
				AR_CUSTYPE::CUSTYPE, MFLAG, "'E", &
				MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	.x Customer Type Description
	!	^*(02) Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field
	!	enters a description of the
	!	Customer type entered in field (01).
	!	.b
	!	Example:
	!	.table 3,25
	!	.te
	!	RG = ^*Regular\*
	!	.te
	!	DE = ^*Dealer\*
	!	.te
	!	03 = ^*Distributor\*
	!	.end table
	!	.b
	!	The field will accommodate up to forty characters.
	!	.lm -5
	!
	! Index:
	!
	!--

			AR_CUSTYPE::DESCRIPTION = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;33", TEMP$, &
				AR_CUSTYPE::DESCRIPTION, MFLAG, "'E", &
				MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		AR_MAIN_CUSTYPE = 0%

	!
	! Set AR_CUSTYPE_OLD value
	!
20500	CASE OPT_SETOLD
		AR_CUSTYPE_OLD = AR_CUSTYPE

	!
	! Restore AR_CUSTYPE_OLD value
	!
	CASE OPT_RESETOLD
		AR_CUSTYPE = AR_CUSTYPE_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		AR_CUSTYPE2 = AR_CUSTYPE

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		AR_CUSTYPE = AR_CUSTYPE2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  CustomerType Description"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "015"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = AR_CUSTYPE::CUSTYPE + SPACE$(11%) + &
				AR_CUSTYPE::DESCRIPTION
		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE AR_CUSTYPE::CUSTYPE + "", &
				REGARDLESS
		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!***************************************************************
	! Trap errors
	!***************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
