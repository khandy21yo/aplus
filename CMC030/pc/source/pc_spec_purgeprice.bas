1	%TITLE "Purge Price File"
	%SBTTL "PC_SPEC_PURGEPRICE"
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
	!	This program will purge prices so that there will be only
	!	one price before a given date.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PC_SOURCE:PC_SPEC_PURGEPRICE/LINE
	!	$ LINK/EXE=PC_EXE: PC_SPEC_PURGEPRICE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PC_SPEC_PURGEPRICE.OBJ;*
	!
	! Author:
	!
	!	09/25/87 - Frank F. Starman
	!
	! Modification history:
	!
	!	03/22/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	04/14/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!		Change SMG_PURGE to SMG_PURGE%.
	!
	!	05/01/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/03/96 - Kevin Handy
	!		Added capability of purging one category.
	!		Modified to open price file ".MOD" instead
	!		of ".UPD"
	!
	!	05/14/96 - Kevin Handy
	!		Rewrote purge routine so that it does not
	!		assume that the prices are sorted properly,
	!		because they aren't.
	!
	!	05/17/96 - Kevin Handy
	!		Catch price without a product to match.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/25/2000 - Kevin Handy
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
	%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.HB"
	MAP	(PC_PRICE)	PC_PRICE_CDD	PC_PRICE

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP	(PD_PRODUCT)	PD_PRODUCT_CDD	PD_PRODUCT

	!
	! Array for listing
	!
	DECLARE INTEGER CONSTANT MAX_ITEM = 6%

	DECLARE INTEGER CONSTANT MAX_ARRAY = 100%
	DIM RFA PRICE_RFA(MAX_ARRAY)
	DIM PC_PRICE_CDD PRICE_DATA(MAX_ARRAY)

	!
	! Declare constants
	!
	DECLARE LONG XLONG, YLONG

	DECLARE RFA ADDRESS
	DECLARE STRING TEST_PRODUCT

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

300	!
	! Open price file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.MOD"
	USE
		FILENAME$ = "PC_PRICE"
		CONTINUE HelpError
	END WHEN

310	!
	! Open product master file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

500	!******************************************************************
	! Declare defaults for screen
	!******************************************************************

	PRODUCT_ITEM$ = "*" + SPACE$(19%)
	STORE_ITEM$ = "*" + SPACE$(19%)
	TYPE_ITEM$ = "*" + SPACE$(19%)
	CAT_ITEM$ = "*" + SPACE$(19%)

	DATE_ITEM$ = DATE_TODAY
	TIME_ITEM$ = TIME_NOW

900	!
	! Create a display window
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		78%, &
		SMG_PURGE%, &
		SMG$M_BORDER &
	)

	!
	! Label the display
	!
	SMG_STATUS% = SMG$LABEL_BORDER(SMG_PURGE%, &
		"Purge Price & Cost File")

	GOSUB Repaint

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_PURGE%, &
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
			CALL ENTR_3MESSAGE(SCOPE, "Sorry, Unable to blank", 0%)

		CASE 2%
			LSET TIME_ITEM$ = "000000"

		CASE 3%
			LSET PRODUCT_ITEM$ = "*"

		CASE 4%
			LSET STORE_ITEM$ = "*"

		CASE 5%
			LSET TYPE_ITEM$ = "*"

		CASE 6%
			LSET CAT_ITEM$ = "*"

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

	DATA	6, 20, "(01) To Date", &
		7, 20, "(02) To Time", &
		8, 20, "(03) Product #", &
		9, 20, "(04) Location #", &
		10, 20, "(05) Price Type", &
		11, 20, "(06) Category", &
		0, 0, ""

	RESTORE
	READ XLONG, YLONG, ATEXT$

	WHILE XLONG
		SMG_STATUS% = SMG$PUT_CHARS(SMG_PURGE%, ATEXT$, XLONG, YLONG)
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
	!	^*(01) From Date\*
	!	.b
	!	This field specifies the date to purge the prices up to.
	!	It is used along with field "^*(02) To Time\*" to determine
	!	the cutoff for the purge.
	!
	! Index:
	!
	!--
		DATE_ITEM$ = ENTR_3DATE(SCOPE, SMG_PURGE%, &
			"06;45", "To Date", DATE_ITEM$, FLAG%, "'E", &
			DEFLT$)

	CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) To Time\*
	!	.p
	!	This field specifies the time to purge prices up to.
	!	It is used along with the "^*(01) To Date\*" field
	!	to specify where the purge cutoff will be.
	!
	! Index:
	!
	!--
		TIME_ITEM$ = ENTR_3TIME(SCOPE, SMG_PURGE%, &
			"07;45", "To Time", TIME_ITEM$, FLAG%, "'E", &
			DEFLT$)

	CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Product Number\*
	!	.p
	!	This field contains a wildcard used to specify which products
	!	should be purged.
	!	To purge all products that meet all other criteria,
	!	enter a "*_*" here.
	!
	! Index:
	!
	!--
		PRODUCT_ITEM$ = ENTR_3STRING(SCOPE, SMG_PURGE%, &
			"8;45", "Product #", &
			PRODUCT_ITEM$, FLAG%, "'E", DEFLT$)

	CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Location\*
	!	.p
	!	This field specifies a wildcard location to purge.
	!	To purge all locations, enter a "*_*" here.
	!
	! Index:
	!
	!--
		STORE_ITEM$ = ENTR_3STRING(SCOPE, SMG_PURGE%, &
			"9;45", "Location #", &
			STORE_ITEM$, FLAG%, "'E", DEFLT$)

	CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Price Type\*
	!	.p
	!	This field specifies a wildcard price type to be purged.
	!	To purge all price types, enter a "*_*" here.
	!
	! Index:
	!
	!--
		TYPE_ITEM$ = ENTR_3STRING(SCOPE, SMG_PURGE%, &
			"10;45", "Price Type", &
			TYPE_ITEM$, FLAG%, "'E", DEFLT$)

	CASE 6%
		CAT_ITEM$ = ENTR_3STRING(SCOPE, SMG_PURGE%, &
			"11;45", "Category", &
			CAT_ITEM$, FLAG%, "'E", DEFLT$)

	END SELECT

	SCOPE::PRG_ITEM = TEMP$

	RETURN

 Purge:
	!*****************************************************
	! Purge Price file
	!*****************************************************

	SCOPE::PRG_ITEM = "CONFIRM"
	INP$ = ENTR_3YESNO(SCOPE, SMG_PURGE%, &
		"", "Confirm purging - then press <Do> ", "N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	IF INP$ <> "Y"
	THEN
		GOTO ExitProgram
	END IF

	SMG_STATUS% = SMG$PUT_CHARS(SMG_PURGE%, "Examine Product # ", 12%, 5%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_PURGE%, "Remove            ", 14%, 5%)

	CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)

	!
	! Purge
	!
	RESET #PC_PRICE.CH%
	PRICE_DATA% = 0%
	LAST_PRODUCT$ = ""

2000	!*******************************************************************
	! Main loop starts here
	!
	! Collect prices for one proguct
	!*******************************************************************

 GetNextRec:

	WHEN ERROR IN
		GET #PC_PRICE.CH%
	USE
		CONTINUE 2090 IF ERR = 11%
		FILENAME$ = "PC_PRICE"
		CONTINUE HelpError
	END WHEN

	!
	! Ignore prices that don't match criteria
	!
	GOTO GetNextRec IF COMP_STRING(PC_PRICE::PRODUCT_NUM, PRODUCT_ITEM$) = 0%
	GOTO GetNextRec IF COMP_STRING(PC_PRICE::LOCATION, STORE_ITEM$) = 0%
	GOTO GetNextRec IF COMP_STRING(PC_PRICE::PCTYPE, TYPE_ITEM$) = 0%

	ADDRESS = GETRFA(PC_PRICE.CH%)

2010	IF TRM$(CAT_ITEM$) <> "*"
	THEN
		IF (PC_PRICE::PRODUCT_NUM <> PD_PRODUCT::PRODUCT_NUM)
		THEN
			WHEN ERROR IN
				GET #PD_PRODUCT.CH%, &
					KEY #0% EQ PC_PRICE::PRODUCT_NUM, &
					REGARDLESS
			USE
				CONTINUE 2000 IF ERR = 155%
				FILENAME$ = "PD_PRODUCT"
				CONTINUE HelpError
			END WHEN
		END IF

		GOTO GetNextRec &
			IF COMP_STRING(PD_PRODUCT::CATEGORY, CAT_ITEM$) = 0%
	END IF

	!
	! Did product number change?
	!
	IF PC_PRICE::PRODUCT_NUM <> LAST_PRODUCT$
	THEN
		GOSUB DoPurge
		LAST_PRODUCT$ = PC_PRICE::PRODUCT_NUM
		PRICE_DATA% = 0%
		GET #PC_PRICE.CH%, RFA ADDRESS
	END IF

2030	PRICE_DATA% = PRICE_DATA% + 1%
	PRICE_RFA(PRICE_DATA%) = GETRFA(PC_PRICE.CH%)
	PRICE_DATA(PRICE_DATA%) = PC_PRICE

	GOTO GetNextRec

2090	!
	! Clean up
	!
	GOSUB DoPurge
	RETURN

	!*******************************************************************
	! Do the actual purging
	!*******************************************************************
 DoPurge:
	RETURN IF PRICE_DATA% = 0%

	SMG_STATUS% = SMG$PUT_CHARS(SMG_PURGE%, PC_PRICE::PRODUCT_NUM, &
		12%, 22%)

	!
	! Sort the price arrays
	!
	FOR I% = 1% TO PRICE_DATA%

		FOR J% = 1% TO PRICE_DATA% - 1%

			IF PRICE_DATA(J%)::PRODUCT_NUM + &
				PRICE_DATA(J%)::LOCATION + &
				PRICE_DATA(J%)::PCTYPE + &
				PRICE_DATA(J%)::XDATE + &
				PRICE_DATA(J%)::XTIME > &
				PRICE_DATA(J% + 1%)::PRODUCT_NUM + &
				PRICE_DATA(J% + 1%)::LOCATION + &
				PRICE_DATA(J% + 1%)::PCTYPE + &
				PRICE_DATA(J% + 1%)::XDATE + &
				PRICE_DATA(J% + 1%)::XTIME
			THEN
				PRICE_DATA(0%) = PRICE_DATA(J%)
				PRICE_DATA(J%) = PRICE_DATA(J% + 1%)
				PRICE_DATA(J% + 1%) = PRICE_DATA(0%)

				PRICE_RFA(0%) = PRICE_RFA(J%)
				PRICE_RFA(J%) = PRICE_RFA(J% + 1%)
				PRICE_RFA(J% + 1%) = PRICE_RFA(0%)
			END IF
		NEXT J%
	NEXT I%

2100	!
	! Scan through these records, deciding which ones to purge
	! out
	!
	FOR I% = 1% TO PRICE_DATA%

		IF (PRICE_DATA(I%)::PRODUCT_NUM = &
				PRICE_DATA(I% + 1%)::PRODUCT_NUM) AND &
			(PRICE_DATA(I%)::LOCATION = &
				PRICE_DATA(I% + 1%)::LOCATION) AND &
			(PRICE_DATA(I%)::PCTYPE = &
				PRICE_DATA(I% + 1%)::PCTYPE) AND &
			(PRICE_DATA(I% + 1%)::XDATE + &
				PRICE_DATA(I% + 1%)::XTIME <= &
				DATE_ITEM$ + TIME_ITEM$)
		THEN
			WHEN ERROR IN
				GET #PC_PRICE.CH%, RFA PRICE_RFA(I%)
				DELETE #PC_PRICE.CH%
			USE
				CONTINUE 2190
			END WHEN
		END IF
	NEXT I%

2190	RETURN

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
