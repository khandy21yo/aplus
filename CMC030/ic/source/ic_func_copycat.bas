1	%TITLE "Copy Worksheet to Cycle Journal"
	%SBTTL "IC_FUNC_COPYCAT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG IC_FUNC_COPYCAT
	!
	! COPYRIGHT (C) 2002 BY
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
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	Copies a worksheet to the inventory cycle counting
	!	journal.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_FUNC_COPYCAT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP IC_FUNC_COPYCAT
	!	$ DELETE IC_FUNC_COPYCAT.OBJ;*
	!
	! Author:
	!
	!	03/29/2002 - Kevin Handy
	!		Based on IC_FUNC_COPYBATCH
	!
	! Modification history:
	!
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[IC.OPEN]IC_CYCLEJOUR.HB"
	MAP (IC_CYCLEJOUR)	IC_CYCLEJOUR_CDD	IC_CYCLEJOUR

	%INCLUDE "SOURCE:[IC.OPEN]IC_JOURCOUNT.HB"
	MAP (IC_JOURCOUNT)	IC_JOURCOUNT_CDD	IC_JOURCOUNT

	%INCLUDE "SOURCE:[IC.OPEN]IC_JOURADJUST.HB"
	MAP (IC_JOURADJUST)	IC_JOURADJUST_CDD	IC_JOURADJUST

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	!
	! Array for listing
	!
	DECLARE INTEGER CONSTANT MAX_ITEM = 4%

	!
	! Declare constants
	!
	DECLARE LONG SMG_COPY

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (BATCH_IC_CYCLEJOUR) &
		BATCH_NO$ = 2%

	COM (CH_PD_PRODUCT) &
		PD_PRODUCT.CH%, &
		PD_PRODUCT.READONLY%


	%PAGE

	ON ERROR GOTO 19000

	IC_FUNC_COPYCAT = 0%
	TEMP1$ =  TRM$(SCOPE::PRG_PROGRAM)

300	!
	! Open main file (existing) for modification
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_JOURADJUST.OPN"
	USE
		CONTINUE 310 IF ERR = 5%
		FILENAME$ = "IC_JOURADJUST"
		CONTINUE HelpError
	END WHEN

	CALL ENTR_3MESSAGE(SCOPE, &
		"Adjustment file exists. Can't create worksheet.", 0%)

	GOTO ExitFunction

310	!
	! Open main file (existing) for modification
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_JOURCOUNT.CRE"
	USE
		FILENAME$ = "IC_JOURCOUNT"
		CONTINUE HelpError
	END WHEN

320	IF PD_PRODUCT.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
			PD_PRODUCT.READONLY% = 1%
		USE
			FILENAME$ = "PD_PRODUCT"
			CONTINUE HelpError
		END WHEN
	END IF

	%PAGE

	!
	! Paint the background
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		6%, &
		40%, &
		SMG_COPY, &
		SMG$M_BORDER &
	)

	SMG_STATUS% = SMG$LABEL_BORDER &
	( &
		SMG_COPY, &
		"Creating Worksheet by Category", &
		SMG$K_TOP &
	)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_COPY, &
		SCOPE::SMG_PBID, &
		12%, &
		5% &
	)

400	!******************************************************************
	! Declare defaults for screen
	!******************************************************************

	TO_WEEK% = 0%
	FROM_ITEM$ = SPACE$(4%)
	TO_ITEM$ = SPACE$(4%)
	WILDCARD$ = "*" + SPACE$(19%)


500	!******************************************************************
	! Main option menu
	!******************************************************************

	GOSUB Repaint

510	!
	! Enter options
	!
	SCOPE::PRG_ITEM = ""
	OPTLIST$ = "Change Blank Go Help eXit"
	OPT$ = ENTR_3OPTION(SCOPE, "COMMAND", OPTLIST$, OPT%, 0%)

	SELECT SCOPE::SCOPE_EXIT

	!
	! Control c
	!
	CASE 3%
		GOTO 500

	!
	! Exit key
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitFunction

	END SELECT

	SELECT OPT$

	CASE "C"
 Changer:
		!*****************************************************
		! Change information on the screen
		!*****************************************************

		LOOP% = ENTR_3NUMBER(SCOPE, SCOPE::SMG_OPTION, "", &
			"Item to change", 0.0, 4%, "##", "")

		SELECT SCOPE::SCOPE_EXIT
		!
		! Control c
		!
		CASE 3%
			GOTO 500

		!
		! Exit key
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 510

		END SELECT

		GOTO 510 IF LOOP% = 0%
		GOTO Changer IF LOOP% < 1% OR LOOP% > MAX_ITEM

		LOOP1% = LOOP%

 Changer1:	FLAG% = 0%
		GOSUB DataEntry

		SELECT SCOPE::SCOPE_EXIT
		!
		! Control c
		!
		CASE 3%
			GOTO 500

		!
		! Uparrow
		!
		CASE SMG$K_TRM_UP
			LOOP% = LOOP% - 1% IF LOOP% > 1%
			GOTO Changer1

		!
		! SMG$K_TRM_DOWN
		!
		CASE SMG$K_TRM_DOWN
			LOOP% = LOOP% + 1% IF LOOP% < MAX_ITEM
			GOTO Changer1

		!
		! Exit key
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 510

		END SELECT

		GOTO Changer

	CASE "B"
 BlankR:	!*****************************************************
		! Blank information on the screen
		!*****************************************************

		LOOP% = ENTR_3NUMBER(SCOPE, SCOPE::SMG_OPTION, "", &
			"Item to Blank", 0.0, 4%, "##", "")

		SELECT SCOPE::SCOPE_EXIT
		!
		! Control c
		!
		CASE 3%
			GOTO 500

		!
		! Exit key
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 510

		END SELECT

		GOTO 510 IF LOOP% = 0%
		GOTO Blankr IF LOOP% < 1% OR LOOP% > MAX_ITEM

		SELECT LOOP%

		CASE 1%
			TO_WEEK% = 0%

		CASE 2%
			LSET FROM_ITEM$ = SPACE$(6%)

		CASE 3%
			LSET TO_ITEM$   = SPACE$(6%)

		CASE 4%
			LSET WILDCARD$ = "*" + SPACE$(19%)

		END SELECT

		FLAG% = 1%
		GOSUB DataEntry

		GOTO Blankr

	CASE "G"
		GOTO 1000

	!
	! Help
	!
	! This option calls out a help message describing the
	! program.
	!
	CASE "H"
		CALL HELP_3MESSAGE(SCOPE, "", "PROG", &
			SCOPE::PRG_PROGRAM, "HELP")
		GOTO 500

	CASE "X"
		GOTO ExitFunction

	END SELECT

	GOTO 510

	%PAGE

1000	!******************************************************************
	! Copy
	!******************************************************************
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	!
	! Display message
	!
	CALL ENTR_3MESSAGE(SCOPE, "Creating Worksheet ... ", 1% + 16%)

	FROM_ITEM$ = EDIT$(FROM_ITEM$, 132%)
	TO_ITEM$ = EDIT$(TO_ITEM$, 132%)
	WILDCARD$ = EDIT$(WILDCARD$, -1%)

1003	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PD_PRODUCT.CH%
		ELSE
			FIND #PD_PRODUCT.CH%, &
				KEY #2% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE 1500 IF ERR = 155%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

1010	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, REGARDLESS
	USE
		CONTINUE 1500 IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

 !	GOTO 1010 IF READ_BIT(8%, PD_PRODUCT::CYCLEMAP, TO_WEEK%) = 0% &
 !		AND TO_WEEK% <> 0%


1020	GOTO 1500 IF (PD_PRODUCT::CATEGORY > TO_ITEM$) AND TO_ITEM$ <> ""

	IF WILDCARD$ <> ""
	THEN
		GOTO 1010 &
			IF COMP_STRING(EDIT$(PD_PRODUCT::CATEGORY, -1%), &
			WILDCARD$) = 0% &
	END IF

	!
	! Add record
	!
	IC_JOURCOUNT::LOCATION	= IC_CYCLEJOUR::LOCATION
	IC_JOURCOUNT::PRODUCT	= PD_PRODUCT::PRODUCT_NUM
	IC_JOURCOUNT::QUANTITY	= 0.0
	IC_JOURCOUNT::CONTROL	= ""

	PUT #IC_JOURCOUNT.CH%

	GOTO 1010

1500	CALL ENTR_3MESSAGE(SCOPE, "Process completed ", 1%)

 ExitFunction:
	CLOSE IC_JOURADJUST.CH%
	CLOSE IC_JOURCOUNT.CH%

	CALL ASSG_FREECHANNEL(IC_JOURCOUNT.CH%)
	CALL ASSG_FREECHANNEL(IC_JOURADJUST.CH%)
	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_COPY)

	SCOPE::PRG_PROGRAM = TEMP1$

	EXIT FUNCTION

	%PAGE

 Repaint:
	!******************************************************************
	! Repaint the screen
	!******************************************************************

	SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_COPY)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_COPY)


	DATA	02, 05, "(01) Count Week", &
		03, 05, "(02) From Category", &
		04, 05, "(03) To Category", &
		05, 05, "(04) Wildcard", &
		0, 0, ""

	RESTORE

	READ XPOS%, YPOS%, XSTR$

	WHILE (XPOS% <> 0%)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY, XSTR$, XPOS%, YPOS%)
		READ XPOS%, YPOS%, XSTR$
	NEXT

	FLAG% = 1%
	GOSUB DataEntry FOR LOOP% = 1% TO MAX_ITEM

	SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_COPY)

	RETURN

 DataEntry:
	!******************************************************************
	! Enter/Diaplay items
	!******************************************************************

	TEMP$ = TRM$(SCOPE::PRG_ITEM)
	SCOPE::PRG_ITEM = "FLD" + FORMAT$(LOOP%, "<0>##")
 Reenter:

	SELECT LOOP%

	CASE 1%
		TO_WEEK% = ENTR_3NUMBER(SCOPE, SMG_COPY, &
			"02;25", "Count Week ", &
			TO_WEEK% * 1.0, FLAG%, "##", DEFLT$)


	CASE 2%
		FROM_ITEM$ = ENTR_3STRING(SCOPE, SMG_COPY, &
			"03;25", "From Bin ", &
			FROM_ITEM$, FLAG%, "'E", DEFLT$)

	CASE 3%
		TO_ITEM$ = ENTR_3STRING(SCOPE, SMG_COPY, &
			"04;25", "To Bin ", &
			TO_ITEM$, FLAG%, "'E", DEFLT$)

	CASE 4%
		WILDCARD$ = ENTR_3STRING(SCOPE, SMG_COPY, &
			"05;25", "Wildcard ", &
			WILDCARD$, FLAG%, "'E", DEFLT$)

	END SELECT

	SCOPE::PRG_ITEM = TEMP$

	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	IC_FUNC_COPYCAT = 1%
	GOTO ExitFunction

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END FUNCTION
