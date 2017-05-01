1	%TITLE "Copy Cycle Count"
	%SBTTL "IC_SPEC_COPYCYCLE"
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
	!	.b
	!	.lm +5
	!	The ^*Copy Cycle Count\* option copies the
	!	inventory scale from one record to another. This option makes it
	!	possible for all records to be of the same format. It also
	!	makes changes throughout the entire system faster and easier.
	!	.lm -5
	!
	! Index:
	!	.x Cycle Count Map
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_SPEC_COPYCYCLE/LINE
	!	$ LINK/EXE=IC_EXE: IC_SPEC_COPYCYCLE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_SPEC_COPYCYCLE.OBJ;*
	!
	! Author:
	!
	!	11/09/87 - Frank F. Starman
	!
	! Modification history:
	!
	!	04/27/92 - Kevin Handy
	!		Clean up (check)
	!
	!	11/17/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/01/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/14/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	07/12/95 - Kevin Handy
	!		Fix so that it blanks SAFETY and MAXLEVEL if it
	!		is creating a new record, instead of keeping
	!		whatever happens to be in it. (kingb)
	!
	!	10/19/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/28/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/29/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	03/24/2003 - Kevin Handy
	!		Added wildcard category
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
	%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.HB"
	MAP (IC_BINMAP)		IC_BINMAP_CDD		IC_BINMAP

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION READ_BIT
	EXTERNAL STRING FUNCTION FUNC_BITSWITCH

	DECLARE LONG WEEK(52%)
	!
	! Array for listing
	!
	DECLARE INTEGER CONSTANT MAX_ITEM = 10%

	!
	! Declare constants
	!
	DECLARE LONG XLONG, YLONG

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

300	!
	! Open bin file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.CRE"
	USE
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

310	!
	! Open product file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

320	!
	! Open location file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

500	!******************************************************************
	! Declare defaults for screen
	!******************************************************************

	PRODUCT_FROM$ = "?" + SPACE$(13%)
	STORE_FROM$ = "?" + SPACE$(3%)
	PRODUCT_ITEM$ = "*" + SPACE$(19%)
	STORE_ITEM$ = "*" + SPACE$(19%)
	TYPE_ITEM$ = "*" + SPACE$(19%)
	ABC_ITEM$ = "*" + SPACE$(19%)
	BIN_FLAG$ = "N"
	ABC_FLAG$ = "N"
	MOVE_FLAG$ = "N"
	CATEGORY_ITEM$ = "*" + SPACE$(19%)

900	!
	! Create a display window
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		78%, &
		SMG_COPY%, &
		SMG$M_BORDER &
	)

	!
	! Label the display
	!
	SMG_STATUS% = SMG$LABEL_BORDER(SMG_COPY%, &
		"Copy Cycle Map")

	GOSUB Repaint

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_COPY%, &
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
			"Item to change", 0.0, 4%, "##", "")

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

		LOOP1% = LOOP%

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
			"Item to Blank", 0.0, 4%, "##", "")

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
			CALL ENTR_3MESSAGE(SCOPE, "Sorry, Unable to blank", 0%)

		CASE 2%
			CALL ENTR_3MESSAGE(SCOPE, "Sorry, Unable to blank", 0%)

		CASE 3%
			LSET BIN_FLAG$ = "N"

		CASE 4%
			LSET ABC_FLAG$ = "N"

		CASE 5%
			LSET PRODUCT_ITEM$ = "*"

		CASE 6%
			LSET STORE_ITEM$ = "*"

		CASE 7%
			LSET TYPE_ITEM$ = "*"

		CASE 8%
			LSET ABC_ITEM$ = "*"

		CASE 9%
			LSET MOVE_FLAG$ = "N"

		CASE 10%
			LSET CATEGORY_ITEM$ = "*"

		END SELECT

		FLAG% = 1%
		GOSUB DataEntry

		GOTO Blankr

	CASE "G"
		GOSUB Copy

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

	DATA	4,20, "(01) Sample Product #", &
		5,20, "(02) Sample Location #", &
		6,20, "(03) Copy BinLoc (Y/N)", &
		7,20, "(04) Copy ABCFlag (Y/N)", &
		9,20, "(05) To Product #", &
		10,20, "(06) Location #", &
		11,20, "(07) Product Type", &
		12,20, "(08) ABC Flag", &
		13,20, "(09) Move Map by 1(Y/N)", &
		14,20, "(10) Category", &
		0, 0, ""

	RESTORE
	READ XLONG, YLONG, ATEXT$

	WHILE XLONG
		SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY%, ATEXT$, XLONG, YLONG)
		READ XLONG, YLONG, ATEXT$
	NEXT

	FLAG% = 1%
	GOSUB DataEntry &
		FOR LOOP% = 1% TO MAX_ITEM

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
	!	^*(01) Sample Product _#\*
	!	.b
	!	.lm +5
	!	The ^*Sample Product Number\* field enters the product
	!	number which is to be used for the copying process.
	!	.lm -5
	!
	! Index:
	!	.x Sample Product Number
	!
	!--
		PRODUCT_FROM$ = ENTR_3STRING(SCOPE, &
			SMG_COPY%, "4;50", "Product#", &
			PRODUCT_FROM$, FLAG%, "'E", DEFLT$)

	CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Sample Location _#\*
	!	.b
	!	.lm +5
	!	The ^*Sample Location Number\* field enters the location
	!	number of where the product being copied from is located.
	!	.lm -5
	!
	! Index:
	!	.x Sample Location Number
	!
	!--

		STORE_FROM$ = ENTR_3STRING(SCOPE, &
			SMG_COPY%, "5;50", "Store#", &
			STORE_FROM$, FLAG%, "'E", DEFLT$)

	CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Copy Bin Location (Y/N)\*
	!	.b
	!	.lm +5
	!	The ^*Copy Location\* field enters the desired choice
	!	of whether to copy the bin location in addition to copying the other fields.
	!	A ^*Y\* entry would cause the bin locations to be copied while a ^*N\*
	!	answer would cause the locations not to be copied.
	!	.lm -5
	!
	! Index:
	!	.x Copy Bin Location
	!
	!--

		BIN_FLAG$ = ENTR_3STRING(SCOPE, SMG_COPY%, "6;50", "(Y/N)", &
			BIN_FLAG$, FLAG%, "'E", DEFLT$)

	CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Copy ABC Flag (Y/N)\*
	!	.b
	!	.lm +5
	!	The ^*Copy ABC Flag\* field enters the desired choice
	!	of whether to copy the ABC Flag in addition to copying the other fields.
	!	A ^*Y\* entry would cause the flags to be copied while a ^*N\*
	!	answer would cause the flags not to be copied.
	!	.lm -5
	!
	! Index:
	!	.x Copy ABC Flag
	!
	!--

		ABC_FLAG$ = ENTR_3STRING(SCOPE, SMG_COPY%, "7;50", "(Y/N)", &
			ABC_FLAG$, FLAG%, "'E", DEFLT$)

	CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) To Product _#\*
	!	.b
	!	.lm +5
	!	The ^*To Product Number\* field enters the product
	!	number which will be copied to. Any number may be entered and Wildcarding
	!	techniques may be used.
	!	.lm -5
	!
	! Index:
	!	.x To Product Number
	!
	!--

		PRODUCT_ITEM$ = ENTR_3STRING(SCOPE, &
			SMG_COPY%, "9;50", "Product#", &
			PRODUCT_ITEM$, FLAG%, "'E", DEFLT$)

	CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Location _#\*
	!	.b
	!	.lm +5
	!	The ^*Location Number\* field enters the location
	!	number which will be copied to. Any number may be entered and Wildcarding
	!	techniques may be used.
	!	.lm -5
	!
	! Index:
	!	.x To Location Number
	!
	!--

		STORE_ITEM$ = ENTR_3STRING(SCOPE, &
			SMG_COPY%, "10;50", "Store#", &
			STORE_ITEM$, FLAG%, "'E", DEFLT$)

	CASE 7%

	!++
	! Abstract:FLD007
	!	^*(07) Product Type\*
	!	.b
	!	.lm +5
	!	The ^*Product Type\* field enters a specific product
	!	type to copy to. By copying to a product type, the entire type will be the
	!	same. When using the ^*Product Type\* field, the ^*To Product Number\* and
	!	^*To Location Number\* need not be used.
	!	.lm -5
	!
	! Index:
	!	.x Product Type
	!
	!--

		TYPE_ITEM$ = ENTR_3STRING(SCOPE, &
			SMG_COPY%, "11;50", "Product type", &
			TYPE_ITEM$, FLAG%, "'E", DEFLT$)

	CASE 8%

	!++
	! Abstract:FLD008
	!	^*(08) ABC Flag\*
	!	.b
	!	.lm +5
	!	The ^*ABC Flag\* field enters a specific product
	!	ABC flag to copy to.
	!	.lm -5
	!
	! Index:
	!	.x ABC Flag
	!
	!--

		ABC_ITEM$ = ENTR_3STRING(SCOPE, &
			SMG_COPY%, "12;50", "ABC Flag", &
			ABC_ITEM$, FLAG%, "'E", DEFLT$)

	CASE 9%
	!++
	! Abstract:FLD009
	!	^*(09) Move Map by 1 (Y/N)\*
	!	.b
	!	.lm +5
	!	The ^*Move Map by 1\* field
	!	.lm -5
	!
	! Index:
	!	.x Move Map
	!
	!--

		MOVE_FLAG$ = ENTR_3STRING(SCOPE, &
			SMG_COPY%, "13;50", "Move Map by 1(Y/N)", &
			MOVE_FLAG$, FLAG%, "'E", DEFLT$)

	CASE 10%

	!++
	! Abstract:FLD010
	!	^*(10) Product Category\*
	!	.b
	!	.lm +5
	!	The ^*Product Category\* field enters a specific product
	!	category to copy to. By copying to a product type, the entire type will be the
	!	same. When using the ^*Product Type\* field, the ^*To Product Number\* and
	!	^*To Location Number\* need not be used.
	!	.lm -5
	!
	! Index:
	!	.x Product Type
	!
	!--

		CATEGORY_ITEM$ = ENTR_3STRING(SCOPE, &
			SMG_COPY%, "14;50", "Product category", &
			CATEGORY_ITEM$, FLAG%, "'E", DEFLT$)

	END SELECT

	SCOPE::PRG_ITEM = TEMP$

	RETURN

 Copy:
	!*****************************************************
	! Copy cycle map
	!*****************************************************

	SCOPE::PRG_ITEM = "CONFIRM"
	INP$ = ENTR_3YESNO(SCOPE, SMG_COPY%, &
		"", "Confirm copying - then press <Do> ", "N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	IF INP$ <> "Y"
	THEN
		GOTO ExitProgram
	END IF

	SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY%, "Examine Product # ", 16%, 5%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY%, "Examine Location #   ", 17%, 5%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY%, "Copy            ", 18%, 5%)

	CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)

2000	WHEN ERROR IN
		GET #IC_BINMAP.CH%, KEY #0% EQ PRODUCT_FROM$ + STORE_FROM$
	USE
		CONTINUE 2200 IF ERR=155%
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

	TEST_BIN$(I%) = IC_BINMAP::BIN(I%) &
		FOR I% = 0% TO 3%
	TEST_ABC$ = IC_BINMAP::ABC
	TEST_MAP$ = IC_BINMAP::CYCLEMAP

	IF MOVE_FLAG$ = "Y"
	THEN
		LP% = 0%
		FOR I% = 1% TO 52%
			IF READ_BIT(8%, IC_BINMAP::CYCLEMAP, I%)
			THEN
				LP% = LP% + 1%
				WEEK(LP%) = I%
			END IF
		NEXT I%
	END IF

	RESET #UTL_LOCATION.CH%

2010	!
	! Main loop starts here
	!
	!
 GetNextSto:

	WHEN ERROR IN
		GET #UTL_LOCATION.CH%, REGARDLESS
	USE
		CONTINUE 2100 IF ERR = 11%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

	SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY%, UTL_LOCATION::LOCATION, &
		17%, 22%)

	GOTO GetNextSto &
		IF COMP_STRING(UTL_LOCATION::LOCATION, STORE_ITEM$) = 0%

	WHEN ERROR IN
		RESET #PD_PRODUCT.CH%
	USE
		CONTINUE 2100 IF ERR = 11%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

2020
 GetNextRec:

	WHEN ERROR IN
		GET #PD_PRODUCT.CH%,REGARDLESS
	USE
		CONTINUE GetNextSto IF ERR=11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY%, PD_PRODUCT::PRODUCT_NUM, &
		16%, 22%)

	GOTO GetNextRec &
		IF PRODUCT_FROM$ = TRM$(PD_PRODUCT::PRODUCT_NUM)
	GOTO GetNextRec &
		IF COMP_STRING(PD_PRODUCT::PRODUCT_NUM, PRODUCT_ITEM$) = 0%
	GOTO GetNextRec &
		IF COMP_STRING(PD_PRODUCT::PROD_TYPE, TYPE_ITEM$) = 0%
	GOTO GetNextRec &
		IF COMP_STRING(PD_PRODUCT::CATEGORY, CATEGORY_ITEM$) = 0%

2030	WHEN ERROR IN
		GET #IC_BINMAP.CH%, &
			KEY #0% EQ PD_PRODUCT::PRODUCT_NUM + &
			UTL_LOCATION::LOCATION
	USE
		CONTINUE 2040 IF ERR=155%
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

	IF ABC_FLAG$ = "Y"
	THEN
		IC_BINMAP::ABC = TEST_ABC$
	ELSE
		GOTO GetNextRec &
			IF COMP_STRING(IC_BINMAP::ABC, ABC_ITEM$) = 0%
	END IF

	IF MOVE_FLAG$ = "N"
	THEN
		IC_BINMAP::CYCLEMAP = TEST_MAP$
	ELSE
		TEST_MAP$ = STRING$(8%, 0%)
		FOR I% = 1% TO LP%
			WEEK(I%) = WEEK(I%) - INT(WEEK(I%) / 52%) * 52% + 1%
			TEST_MAP$ = FUNC_BITSWITCH(8%, TEST_MAP$, WEEK(I%))
		NEXT I%
		IC_BINMAP::CYCLEMAP = TEST_MAP$
	END IF

	IF BIN_FLAG$ = "Y"
	THEN
		IC_BINMAP::BIN(I%) = TEST_BIN$(I%) &
			FOR I% = 0% TO 3%
	END IF

	WHEN ERROR IN
		UPDATE #IC_BINMAP.CH%
	USE
		CONTINUE 2040 IF ERR=155%
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

	SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY%, IC_BINMAP::PRODUCT, 18%, 22%)

	GOTO GetNextRec

2040	!
	! Add record
	!
	IC_BINMAP::PRODUCT	= PD_PRODUCT::PRODUCT_NUM
	IC_BINMAP::LOCATION	= UTL_LOCATION::LOCATION
	IC_BINMAP::SAFETY	= 0.0
	IC_BINMAP::MAXLEVEL	= 0.0
	IC_BINMAP::CYCLEMAP	= TEST_MAP$

	IF BIN_FLAG$ = "Y"
	THEN
		IC_BINMAP::BIN(I%) = TEST_BIN$(I%) &
			FOR I% = 0% TO 3%
	ELSE
		IC_BINMAP::BIN(I%) = "" &
			FOR I% = 0% TO 3%
	END IF

	IF ABC_FLAG$ = "Y"
	THEN
		IC_BINMAP::ABC = TEST_ABC$
	ELSE
		IC_BINMAP::ABC = " "
	END IF

	PUT #IC_BINMAP.CH%

	SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY%, IC_BINMAP::PRODUCT, 18%, 22%)

	GOTO GetNextRec

2100	CALL ENTR_3MESSAGE(SCOPE, "Copy complete . . . ", 1%)
	RETURN

2200	CALL ENTR_3MESSAGE(SCOPE, "Sample record not found . . . ", 0%)
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
