1	%TITLE "PR Tax Package File Maintenance"
	%SBTTL "PR_MAIN_TAX_PKG"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_TAX_PKG(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	! Abstract:HELP
	!	.p
	!	The ^*Maintain Tax Package\* option
	!	accesses the file where tax package tables can be entered
	!	and maintained.
	!	.p
	!	The purpose of the Tax Package concept is to provide a great
	!	amount of flexibility in causing an individual employee's earnings
	!	to be subject to tax in a variety of tax jurisdictions.  For example,
	!	an employee's earnings may sometimes be subject to tax in one State,
	!	sometimes in some other State(s), or even in more than one State
	!	simultaneously; some earnings could be subject to local tax in one
	!	jurisdiction, sometimes in others.
	!	.p
	!	Tax packages are established to "package" every conceivable
	!	combination of tax jurisdictions.  Employees' earnings are classified
	!	as to which tax package the earnings are applicable.
	!
	! Index:
	!	.x Tax Package>Maintain
	!	.x Tax Package
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_TAX_PKG/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PR_MAIN_TAX_PKG
	!	$ DELETE PR_MAIN_TAX_PKG.OBJ;*
	!
	! Author:
	!
	!	12/03/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	06/01/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	04/03/89 - Kevin Handy
	!		Added code for SI tax package.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/22/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/29/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
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

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.HB"
	MAP (PR_TAX_PKG)	PR_TAX_PKG_CDD	PR_TAX_PKG
	MAP (PR_TAX_PKG2)	PR_TAX_PKG_CDD	PR_TAX_PKG_OLD, PR_TAX_PKG2

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	! This common area must be mapped in both the MAIN program and
	! in PR_MAST_TAX_PKG.
	!
	COM (CH_PR_TAX_PKG) &
		PR_TAX_PKG.CH%, &
		PR_TAX_PKG.READONLY%

	COM (TT_EMP_STATUS) &
		STTITLE$ = 20%, &
		ST$(10%) = 20% &

	%PAGE

	ON ERROR GOTO 29000

	!
	! Withholding types
	!
	STTITLE$ = "Type Description"

	ST$(0%) = "7"
	ST$(1%) = "SW   State"
	ST$(2%) = "SU   Unempl"
	ST$(3%) = "SX   OST"
	ST$(4%) = "CW   City"
	ST$(5%) = "DW   County"
	ST$(6%) = "EW   School"
	ST$(7%) = "SI   WC Liability"

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "PR Tax Package Maintenance"
		SMG_WINDOW::NHELP = "PR_MAIN_TAX_PKG"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 4%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Taxpack_type"
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%

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
		IF PR_TAX_PKG.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_TAX_PKG.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PR_MAIN_TAX_PKG = ERR
			CONTINUE 770
		END WHEN

		PR_TAX_PKG.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PKG.OPN"
		USE
			PR_MAIN_TAX_PKG = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_TAX_PKG.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PR_TAX_PKG.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PR_TAX_PKG.CH%
		WHEN ERROR IN
			RESET #PR_TAX_PKG.CH%
			GET #PR_TAX_PKG.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display the background
	!
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	05, 05, "(01) Tax Package Number", &
			06, 05, "(02) Type", &
			07, 05, "(03) Code", &
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

 Reenter1:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Tax Package Number\*
	!	.p
	!	The ^*Tax Package Number\* field
	!	enters a two (2) character user defined alphanumeric
	!	code key. Duplicates are allowed.
	!	.p
	!	A Tax Package Number must be established for every combination
	!	of tax jurisdictions applicable to the user. In its most simple
	!	application, there would be only one package.
	!	.p
	!	For example, if an employee's earnings were subject to Idaho
	!	withholding tax only, and since there are no local tax considerations,
	!	the only needed Tax Package Number could be:
	!	.b
	!	.lm +15
	!	01 = Idaho State
	!	.BREAK
	!	######or
	!	.BREAK
	!	ID = Idaho State
	!	.lm -15
	!	.p
	!	If employees were located in Kentucky, earnings would be subject
	!	not only to Kentucky State withholding tax, but perhaps to one or
	!	more municipal jurisdictions. Examples of Tax Package Numbers could
	!	be:
	!	.lm +7
	!	02 = Kentucky State
	!	03 = Kentucky State and Louisville City
	!	04 = Kentucky State and Lexington-Fayette Urban County
	!	05 = Kentucky State and Paducah City
	!
	! Index:
	!	.x Tax Package>Number
	!	.x Number>Tax Package
	!
	!--

			PR_TAX_PKG::TAX_PKG = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "05;30", TEMP$, &
				PR_TAX_PKG::TAX_PKG, MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Type\*
	!	.p
	!	The ^*Type\* field enters
	!	the type or kind of tax which is to be included in the package. Valid
	!	types are:
	!	.b
	!	.lm +5
	!	.list 0,"*"
	!	.le
	!	SW = State Withholding
	!	.le
	!	SX = Other State Taxes i.e. State Disability
	!	.le
	!	CW = City Withholding
	!	.le
	!	DW = County Withholding
	!	.le
	!	EW = School District Withholding
	!	.els
	!	.lm -5
	!
	! Index:
	!	.x Tax Package>Type
	!	.x Type>Tax Package
	!
	!--

			PR_TAX_PKG::STTYPE = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, "06;30", TEMP$, &
				PR_TAX_PKG::STTYPE, MFLAG, "'E", MVALUE, &
				ST$(), STTITLE$, "005"), -1%)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Code\*
	!	.p
	!	The ^*Code\* field
	!	enters a two (2) character code which designates a specific tax
	!	jurisdiction. In order to be valid, the code must exist in the Tax
	!	Table file.
	!
	! Index:
	!	.x Tax Package>Code
	!	.x Code>Tax Package
	!
	!--

			PR_TAX_PKG::CODE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "07;30", TEMP$, &
				PR_TAX_PKG::CODE, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		PR_MAIN_TAX_PKG = 0%

	CASE OPT_DISPLAY

	!
	! Set PR_TAX_PKG_OLD value
	!
20500	CASE OPT_SETOLD
		PR_TAX_PKG_OLD = PR_TAX_PKG

	!
	! Restore PR_TAX_PKG_OLD value
	!
	CASE OPT_RESETOLD
		PR_TAX_PKG = PR_TAX_PKG_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PR_TAX_PKG2 = PR_TAX_PKG

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PR_TAX_PKG = PR_TAX_PKG2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Tax Pack #     Type     Code"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "015,024"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = "   " + &
				PR_TAX_PKG::TAX_PKG + "           " + &
				PR_TAX_PKG::STTYPE  + "       " + &
				PR_TAX_PKG::CODE

		END SELECT
	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #PR_TAX_PKG.CH%, KEY #0% &
				GE PR_TAX_PKG::TAX_PKG + PR_TAX_PKG::STTYPE, &
				REGARDLESS
		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	RESUME ExitFunction

32767	END FUNCTION
