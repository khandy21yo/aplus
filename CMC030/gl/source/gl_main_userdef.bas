1	%TITLE "General User Defined Journal Definition File"
	%SBTTL "GL_MAIN_USERDEF"
	%IDENT "V3.6a Calico"

	FUNCTION LONG GL_MAIN_USERDEF(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1998 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! ID:1002
	!
	! Abstract:HELP
	!	.p
	!	.lm +5
	!	This file contains the definitions for the user defined journal
	!	routines.
	!	.lm -5
	!
	! Index:
	!	.x User Defined>Journal
	!	.x Journal>User Defined
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_MAIN_USERDEF/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP GL_MAIN_USERDEF
	!	$ DELETE GL_MAIN_USERDEF.OBJ;*
	!
	! Author:
	!
	!	11/17/1998 - Kevin Handy
	!
	! Modification history:
	!
	!	12/12/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	%PAGE

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD	GL_CHART

	%INCLUDE "SOURCE:[GL.OPEN]GL_USERDEF.HB"
	MAP	(GL_USERDEF)	GL_USERDEF_CDD	GL_USERDEF
	MAP	(GL_CHART_OLD)	GL_USERDEF_CDD	GL_USERDEF_OLD, GL_USERDEF2

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_GL_CHART) &
		GL_CHART.CH%, &
		GL_CHART.READONLY%

	COM (CH_GL_USERDEF) &
		GL_USERDEF.CH%, &
		GL_USERDEF.READONLY%

	COM (TT_GL_USERDEF) &
		ARPTITLE$ = 20%, &
		ARPTYPE$(5%) = 20%, &
		SGNTITLE$ = 20%, &
		SGNTYPE$(5%) = 20%

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 29000

	SELECT MOPTION

	!***********************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!***********************************************************
	CASE OPT_INIT

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Chart of Accounts Maintenance"
		SMG_WINDOW::NHELP = "GL_MAIN_USERDEF"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 130%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 9%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 1%
		SMG_WINDOW::KNAME(0%) = "Jcode"
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%

		IF INSTR(1%, " QV", MVALUE) <= 1%
		THEN
			CALL READ_DEFAULTS(SMG_WINDOW)
		END IF

		!
		! AR/AP Flag
		!
		ARPTITLE$ = "Code   Description"
		ARPTYPE$(0%) = "3"
		ARPTYPE$(1%) = "     Not Recorded"
		ARPTYPE$(2%) = "AR   Accounts Receivable"
		ARPTYPE$(3%) = "AP   Accounts Payable"

		!
		! Sign Flag
		!
		SGNTITLE$ = "Code   Description"
		SGNTYPE$(0%) = "2"
		SGNTYPE$(1%) = "+    Post as/is"
		SGNTYPE$(2%) = "-    Reverse Sign"

700		!
		! Declare channels
		!
		IF GL_USERDEF.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF GL_USERDEF.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_USERDEF.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			GL_MAIN_USERDEF = ERR
			CONTINUE 770
		END WHEN

		GL_USERDEF.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_USERDEF.OPN"
		USE
			GL_MAIN_USERDEF = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		GL_USERDEF.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(GL_USERDEF.CH%)

		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = GL_USERDEF.CH%
		WHEN ERROR IN
			RESET #GL_USERDEF.CH%
			GET #GL_USERDEF.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

800		!
		! Declare channels
		!

20100	!
	! Display the background
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	5, 10, "(01) Journal Code", &
			6, 10, "(02) Line", &
			7, 10, "(03) Description", &
			8, 10, "(04) Account", &
			9, 10, "(05) AR/AP Flag", &
			10, 10, "(06) Unit Flag", &
			11, 10, "(07) Xref", &
			12, 10, "(08) Duplicates", &
			13, 10, "(09) Sign (+/-)", &
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


20200	!
	! Enter/Display/Default
	!
	CASE OPT_ENTRY
		TEMP$ = TRM$(SCOPE::PRG_ITEM)

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	.x Account
	!	^*(01) Account\*
	!	.b
	!	.lm +5
	!	The complete descriptive name of the ^*Account\* field is General Ledger
	!	Chart of Account Number, though it is sometimes referred to as
	!	General Ledger Number, Account Number, or Account.  This field
	!	cannot be set to null. A value must be entered when a record
	!	is being added.
	!	.b
	!	^*Note:  After an account number has been added
	!	and transactions have been posted to the account, this field cannot
	!	and should not be changed.\*
	!	.lm -5
	!
	! Index:
	!
	!--

			GL_USERDEF::JCODE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"5;38", TEMP$, &
				GL_USERDEF::JCODE, MFLAG, "'E", MVALUE)


		CASE 2%

	!++
	! Abstract:FLD002
	!	.x Line
	!	^*(02) Line\*
	!	.b
	!	.lm +5
	!	The line number of the item being defined.
	!	This is the code entered in the journal to use this
	!	specific definition.
	!	.lm -5
	!
	! Index:
	!
	!--

			GL_USERDEF::JLINE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"6;38", TEMP$, &
				GL_USERDEF::JLINE, MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	.x Description
	!	^*(03) Description\*
	!	.b
	!	.lm +5
	!	Description of the item being defined.
	!	.b
	!	Enter a blank description if you want the user to
	!	be asked for the description.
	!	.lm -5
	!
	! Index:
	!
	!--

			GL_USERDEF::DESCRIPTION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"7;38", TEMP$, GL_USERDEF::DESCRIPTION, &
				MFLAG, "'E", MVALUE)


		CASE 4%

	!++
	! Abstract:FLD004
	!	.x Account
	!	^*(04) Account\*
	!	.b
	!	.lm +5
	!	The General Ledger account number to be used when
	!	posting this line.
	!	.b
	!	Leave this item blank if you want the user to be
	!	asked for the GL number.
	!	.lm -5
	!
	! Index:
	!
	!--

			GL_USERDEF::ACCOUNT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"8;38", TEMP$, GL_USERDEF::ACCOUNT, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(GL_MAIN_CHART.ID, "VX  ") = 1%)
				THEN
					GL_USERDEF::ACCOUNT = &
						GL_CHART::ACCT
				END IF
				GOTO ReEnter
			END IF

		CASE 5%

	!++
	! Abstract:FLD005
	!	.x AR/AP Flag
	!	^*(05) AR/AP Flag\*
	!	.b
	!	.lm +5
	!	This flag specifies if a users entry should be posted
	!	to Accounts Receivable, or Accounts Payable.
	!	If so, then the Xref field should be set to the customer
	!	or vendor number appropriate.
	!	.lm -5
	!
	! Index:
	!
	!--

			GL_USERDEF::ARPFLAG = ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"9;38", TEMP$, GL_USERDEF::ARPFLAG, &
				MFLAG, "'E", MVALUE, &
				ARPTYPE$(), ARPTITLE$, "005")

		CASE 6%

	!++
	! Abstract:FLD006
	!	.x Units Flag
	!	^*(05) Units Flag\*
	!	.b
	!	.lm +5
	!	This fiels specifies if the user should be asked
	!	for units for this item.
	!	.lm -5
	!
	! Index:
	!
	!--
			GL_USERDEF::UNITFLAG = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"10;38", TEMP$, &
				GL_USERDEF::UNITFLAG, MFLAG, "'", MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	.x Xref
	!	^*(05) Xref\*
	!	.b
	!	.lm +5
	!	This field is the cross reference into the
	!	customer or vendor master file (depending on how
	!	the ^*AP/AR flag\* is set.
	!	.b
	!	A blank entry here will cause the user to be asked
	!	for the customer/vendor number if the ^*AP/AR flag\*
	!	is set.
	!	.lm -5
	!
	! Index:
	!
	!--
			GL_USERDEF::XREF = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"11;38", TEMP$, &
				GL_USERDEF::XREF, MFLAG, "'E", MVALUE)

		CASE 8%

	!++
	! Abstract:FLD008
	!	.x Xref
	!	^*(05) Xref\*
	!	.b
	!	.lm +5
	!	This field is the cross reference into the
	!	customer or vendor master file (depending on how
	!	the ^*AP/AR flag\* is set.
	!	.b
	!	A blank entry here will cause the user to be asked
	!	for the customer/vendor number if the ^*AP/AR flag\*
	!	is set.
	!	.lm -5
	!
	! Index:
	!
	!--
			GL_USERDEF::DUPLCT = ENTR_3YESNO(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"12;38", TEMP$, &
				GL_USERDEF::DUPLCT, MFLAG, "'E", MVALUE)

		CASE 9%

	!++
	! Abstract:FLD009
	!	.x Sign
	!	^*(05) Sign\*
	!	.b
	!	.lm +5
	!	^*Sign (+/-)\*
	!	.lm -5
	!
	! Index:
	!
	!--
			GL_USERDEF::SIGNED = ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"13;38", TEMP$, &
				GL_USERDEF::SIGNED, MFLAG, "'E", MVALUE, &
				SGNTYPE$(), SGNTITLE$, "005")

		END SELECT

		SCOPE::PRG_ITEM = TEMP$


20300	!***********************************************************
	! Test values
	!***********************************************************
	CASE OPT_TESTENTRY
		GL_MAIN_USERDEF = 0%

		SELECT MLOOP

		CASE 1%
			IF GL_USERDEF::JCODE = ""
			THEN
				GL_MAIN_USERDEF = 1%
			END IF

		CASE 2%
			IF GL_USERDEF::JLINE = ""
			THEN
				GL_MAIN_USERDEF = 1%
			END IF

		END SELECT


	!***********************************************************
	! Test option
	!***********************************************************
	CASE OPT_TESTOPT
		GL_MAIN_USERDEF = 0%



20500	!***********************************************************
	! Set GL_USERDEF_OLD value
	!***********************************************************
	CASE OPT_SETOLD
		GL_USERDEF_OLD = GL_USERDEF


	!***********************************************************
	! Restore GL_USERDEF_OLD value
	!***********************************************************
	CASE OPT_RESETOLD
		GL_USERDEF = GL_USERDEF_OLD


	!***********************************************************
	! Set default value
	!***********************************************************
	CASE OPT_SETDEFAULT
		GL_USERDEF2 = GL_USERDEF



	!***********************************************************
	! Restore default value
	!***********************************************************
	CASE OPT_RESETDEFAULT
		GL_USERDEF = GL_USERDEF2


	!***********************************************************
	! View header
	!***********************************************************
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  JCode Line  Description                               " + &
				"Account             ARP Unit   Sign"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "007,013,055,076,080,086"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = GL_USERDEF::JCODE + "  " + &
				GL_USERDEF::JLINE + "  " + &
				GL_USERDEF::DESCRIPTION + "  " + &
				GL_USERDEF::ACCOUNT + "  " + &
				GL_USERDEF::ARPFLAG + "   " + &
				GL_USERDEF::UNITFLAG + "     " + &
				GL_USERDEF::SIGNED

		END SELECT


	!***********************************************************
	! Find
	!***********************************************************
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			!
			! FIND according to Primary Key
			!	(Account number)
			!
			FIND #GL_USERDEF.CH%, KEY #0% GE GL_USERDEF::JCODE + &
				GL_USERDEF::JLINE, &
				REGARDLESS

		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

	%PAGE

29000	!***************************************************************
	! Trap errors
	!***************************************************************

	ON ERROR GO BACK

32767	!***********************************************************
	! End of GL_MAIN_USERDEF function
	!***********************************************************
	END FUNCTION
