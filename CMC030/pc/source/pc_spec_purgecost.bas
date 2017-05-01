1	%TITLE "Purge Product Std Cost File"
	%SBTTL "PC_SPEC_PURGECOST"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986, 1988 BY
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
	!	This program purges the product costs so there is only
	!	one cost before a specified date.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PC_SOURCE:PC_SPEC_PURGECOST/LINE
	!	$ LINK/EXE=PC_EXE: PC_SPEC_PURGECOST, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PC_SPEC_PURGECOST.OBJ;*
	!
	! Author:
	!
	!	08/05/88 - Frank Starman
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/14/96 - Kevin Handy
	!		Reformat Source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/03/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map file
	!
	%INCLUDE "SOURCE:[PC.OPEN]PC_COST.HB"
	MAP	(PC_COST)	PC_COST_CDD	PC_COST

	!
	! Array for listing
	!
	DECLARE INTEGER CONSTANT MAX_ITEM = 3%

	!
	! Declare constants
	!
	DECLARE LONG XLONG, YLONG, SMG_PURGE

	DECLARE RFA ADDRESS
	DECLARE STRING TEST_PRODUCT

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

300	!
	! Open COST file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PC.OPEN]PC_COST.UPD"
	USE
		FILENAME$ = "PC_COST"
		CONTINUE HelpError
	END WHEN

500	!******************************************************************
	! Declare defaults for screen
	!******************************************************************

	PRODUCT_ITEM$ = "*" + SPACE$(19%)
	STORE_ITEM$ = "*" + SPACE$(19%)

	DATE_ITEM$ = DATE_TODAY

900	!
	! Create a display window
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		78%, &
		SMG_PURGE, &
		SMG$M_BORDER &
	)

	!
	! Label the display
	!
	SMG_STATUS% = SMG$LABEL_BORDER(SMG_PURGE, &
		"Purge Product Std Cost File")

	GOSUB Repaint

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_PURGE, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)

	%PAGE

1000	!******************************************************************
	! Main option menu
	!******************************************************************

	GOSUB Repaint

1100	!
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
		GOTO 1000

	!
	! Exit key
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	END SELECT

	SELECT OPT$

	CASE "C"
 Changer:
		!*****************************************************
		! Change information on the screen
		!*****************************************************

		LOOP% = ENTR_3NUMBER(SCOPE, SCOPE::SMG_OPTION, "", &
			"Item to change", &
			0.0, 4%, "##", "")

		SELECT SCOPE::SCOPE_EXIT
		!
		! Control c
		!
		CASE 3%
			GOTO 1000

		!
		! Exit key
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1100

		END SELECT

		GOTO 1100 IF LOOP% = 0%
		GOTO Changer IF LOOP% < 1% OR LOOP% > MAX_ITEM

 Changer1:	FLAG% = 0%
		GOSUB DataEntry

		SELECT SCOPE::SCOPE_EXIT
		!
		! Control c
		!
		CASE 3%
			GOTO 1000

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
			GOTO 1100

		END SELECT

		GOTO Changer

	CASE "B"
 BlankR:	!*****************************************************
		! Blank information on the screen
		!*****************************************************

		LOOP% = ENTR_3NUMBER(SCOPE, SCOPE::SMG_OPTION, "", &
			"Item to Blank", 0.0, &
			4%, "##", "")

		SELECT SCOPE::SCOPE_EXIT
		!
		! Control c
		!
		CASE 3%
			GOTO 1000

		!
		! Exit key
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1100

		END SELECT

		GOTO 1100 IF LOOP% = 0%
		GOTO Blankr IF LOOP% < 1% OR LOOP% > MAX_ITEM

		SELECT LOOP%

		CASE 1%
			LSET DATE_ITEM$ = DATE_TODAY
		CASE 2%
			LSET PRODUCT_ITEM$ = "*"
		CASE 3%
			LSET STORE_ITEM$ = "*"
		END SELECT

		FLAG% = 1%
		GOSUB DataEntry

		GOTO Blankr

	CASE "G"
		GOSUB Purge
		GOTO ExitProgram

	!
	! Help
	!
	! This option calls out a help message describing the
	! program.
	!
	CASE "H"
		CALL HELP_3MESSAGE(SCOPE, "", "PROG", &
			SCOPE::PRG_PROGRAM, "HELP")
		GOTO 1000

	CASE "X"
		GOTO ExitProgram

	END SELECT

	GOTO 1100

	%PAGE

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

 Repaint:
	!******************************************************************
	! Repaint the screen
	!******************************************************************

	DATA	6,20, "(01) To Date", &
		7,20, "(02) Product #", &
		8,20, "(03) Location #", &
		0, 0, ""

	RESTORE
	READ XLONG, YLONG, ATEXT$

	WHILE XLONG
		SMG_STATUS% = SMG$PUT_CHARS(SMG_PURGE, ATEXT$, XLONG, YLONG)
		READ XLONG, YLONG, ATEXT$
	NEXT

	FLAG% = 1%
	GOSUB DataEntry FOR LOOP% = 1% TO MAX_ITEM

	RETURN

	%PAGE

 DataEntry:
	!******************************************************************
	! Enter/Diaplay items
	!******************************************************************

	TEMP$ = TRM$(SCOPE::PRG_ITEM)

	SCOPE::PRG_ITEM = "FLD" + FORMAT$(LOOP%, "<0>##")

	SELECT LOOP%

	CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) To Date\*
	!	.p
	!	Specifies the date that the costs should purge up to.
	!	Only one cost will be left for each product before
	!	this date.
	!
	! Index:
	!
	!--
		DATE_ITEM$ = ENTR_3DATE(SCOPE, SMG_PURGE, &
			"06;45", "To Date", DATE_ITEM$, FLAG%, "'E", &
			DEFLT$)

	CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Product\*
	!	.p
	!	This field specifies a wildcard product number to
	!	be purged.
	!	To purge all products, enter a "*_*" here.
	!
	! Index:
	!
	!--
		PRODUCT_ITEM$ = ENTR_3STRING(SCOPE, SMG_PURGE, "7;45", &
			"Product #", &
			PRODUCT_ITEM$, FLAG%, "'E", DEFLT$)

	CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Location\*
	!	.p
	!	This field contains a wildcard location to be purged.
	!	To purge all locations, enter a "*_*" here.
	!
	! Index:
	!
	!--
		STORE_ITEM$ = ENTR_3STRING(SCOPE, SMG_PURGE, "8;45", &
			"Location #", &
			STORE_ITEM$, FLAG%, "'E", DEFLT$)

	END SELECT

	SCOPE::PRG_ITEM = TEMP$

	RETURN

 Purge:
	!*****************************************************
	! Purge COST file
	!*****************************************************

	SCOPE::PRG_ITEM = "CONFIRM"
	INP$ = ENTR_3YESNO(SCOPE, SMG_PURGE, &
		"", "Confirm purging - then press <Do> ", "N", 0%, "", "")

	IF INP$ <> "Y"
	THEN
		GOTO ExitProgram
	END IF

	SMG_STATUS% = SMG$PUT_CHARS(SMG_PURGE, "Examine Product # ", 12%, 5%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_PURGE, "Remove            ", 14%, 5%)

	CALL ENTR_3MESSAGE(SCOPE, "Purging...", 1% + 16%)

	DELETE_FLAG% = 0%

	!
	! Purge
	!
	RESET #PC_COST.CH%

2000	!
	! Main loop starts here
	!
 GetNextRec:

	WHEN ERROR IN
		GET #PC_COST.CH%
	USE
		CONTINUE 2100 IF ERR=11%
		FILENAME$ = "PC_COST"
		CONTINUE HelpError
	END WHEN

	!
	! Check if prior record will be removed
	!
	GOSUB 18000 IF DELETE_FLAG%

	SMG_STATUS% = SMG$PUT_CHARS(SMG_PURGE, PC_COST::PRODUCT, &
		12%, 22%)

	GOTO GetNextRec IF COMP_STRING(PC_COST::PRODUCT, PRODUCT_ITEM$) = 0%
	GOTO GetNextRec IF COMP_STRING(PC_COST::LOCATION, STORE_ITEM$) = 0%
	GOTO GetNextRec IF PC_COST::EFFDATE > DATE_ITEM$

	!
	! This can be deleted
	!
	ADDRESS = GETRFA(PC_COST.CH%)
	DELETE_FLAG% = -1%
	TEST_PRODUCT = PC_COST::PRODUCT + PC_COST::LOCATION
	GOTO GetNextRec

2100	RETURN

18000	DELETE_FLAG% = 0%
	IF  TEST_PRODUCT = PC_COST::PRODUCT + PC_COST::LOCATION AND &
		PC_COST::EFFDATE <= DATE_ITEM$
	THEN
		!
		! Remove record from the file
		!
		GET #PC_COST.CH%, RFA ADDRESS
		SMG_STATUS% = SMG$PUT_CHARS(SMG_PURGE, TEST_PRODUCT, &
			14%, 22%, , SMG$M_REVERSE)
		DELETE #PC_COST.CH%
		GET #PC_COST.CH%
	END IF

	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

32767	END
