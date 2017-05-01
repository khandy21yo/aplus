1	%TITLE "Read FASFAX RAIS Standard File"
	%SBTTL "PD_CONV_CONVERT"
	%IDENT "V3.6a Calico"

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
	!	This program reads the FASFAX RAIS standard data file
	!	and
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PD_SOURCE:PD_CONV_CONVERT/LINE
	!	$ LINK/EXECUTABLE=PD_EXE: PD_CONV_CONVERT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PD_CONV_CONVERT.OBJ;*
	!
	! Author:
	!
	!	06/28/89 - Lance Williams
	!
	! Modification history:
	!
	!	04/09/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/20/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/28/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

10	ON ERROR GOTO 19000

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD	PD_PRODUCT

	%INCLUDE "SOURCE:[RM.OPEN]RM_JOURNAL.HB"
	MAP (RM_JOURNAL)	RM_JOURNAL_CDD	RM_JOURNAL

	%INCLUDE "SOURCE:[RM.OPEN]RM_CONTROL.HB"
	MAP (RM_CONTROL)	RM_CONTROL_CDD	RM_CONTROL

	%INCLUDE "SOURCE:[RM.OPEN]RM_JOURPROD.HB"
	MAP (RM_JOURPROD)		RM_JOURPROD_CDD	RM_JOURPROD

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.CRE"
	%INCLUDE "SOURCE:[RM.OPEN]RM_JOURNAL.CRE"
	%INCLUDE "SOURCE:[RM.OPEN]RM_CONTROL.CRE"
	%INCLUDE "SOURCE:[RM.OPEN]RM_JOURPROD.CRE"

	DECLARE STRING FUNCTION CHECK(STRING, WORD, WORD, BYTE)

	%PAGE

	!
	! Declare channels
	!
	READFILE.CH% = 10%
	CALL ASSG_CHANNEL(INSYS.CH%, STAT%)

	!*******************************************************************
	! Initilize Convert
	!*******************************************************************

	CALL READ_INITIALIZE

240	CALL READ_DEVICE("INSYS_ASC", INSYS_ASC.DEV$, STAT%)

	!
	! Ask for the file date to read
	!
	INPUT "Date"; DATES$
	INPUT "Location"; LOCATION$
	INPUT "Stationman"; STATIONMAN$
	INPUT "Expense Account Number"; EXPENSE$

	!*******************************************************************
	! Initilize Convert
	!*******************************************************************

	STRG$ = ""

	READFILE$ = DATES$ + LOCATION$ + ".SDF"

250	OPEN  READFILE$ FOR INPUT AS FILE READFILE.CH%

	INPUT LINE #READFILE.CH%, STRG1$

 Get_next_line:

550	INPUT LINE #READFILE.CH%, STRG$

	CUR_REC$ = MID(STRG$, 1%, 2%) + MID(STRG$, 8%, 1%)

	GOTO 550 IF CUR_REC$ <> "201"

	GET #RM_CONTROL.CH%, REGARDLESS

560	GOSUB 2000

	STRG$ = ""
	GOTO 550

2000	PRICES = VAL(CHECK(STRG$, 29%, 05%, 05%))
	DESCRIPT$ = MID(STRG$, 14%, 10%)
	NUM_SOLD_REG$ = CHECK(STRG$, 37%, 05%, 05%)
	NUM_SOLD_FREE$ = CHECK(STRG$, 55%, 05%, 05%)
	NUM_SOLD_FRE_EM$ = CHECK(STRG$, 60%, 05%, 05%)

	GET #PD_PRODUCT.CH%, KEY #3% EQ DESCRIPT$, REGARDLESS

2010	IF NUM_SOLD_REG$ <> "0"
	THEN
		TYPES$ = RM_CONTROL::TTSALES
		PMENU$ = RM_CONTROL::PRCMENU
		QUANTITY$ = NUM_SOLD_REG$
	END IF

	IF (NUM_SOLD_REG$ = "0") AND (NUM_SOLD_FREE$ <> "0")
		THEN
		TYPES$ = RM_CONTROL::TTPROM
		PMENU$ = ""
		QUANTITY$ = NUM_SOLD_FREE$
	END IF

	IF (NUM_SOLD_FREE$ = "0") AND (NUM_SOLD_FRE_EM$ <> "0")
		THEN
		TYPES$ = RM_CONTROL::TTEMP
		PMENU$ = RM_CONTROL::PRCEMP
		QUANTITY$ = NUM_SOLD_FRE_EM$
	END IF

	!
	! RM_JOURNAL RECORD
	!
	RM_JOURNAL::LOCATION	= LOCATION$
	RM_JOURNAL::STARTDATE	= DATES$
	RM_JOURNAL::TRANSTYPE	= TYPES$
	RM_JOURNAL::PRICETYPE	= PMENU$
	RM_JOURNAL::STATIONMAN	= STATIONMAN$
	RM_JOURNAL::EXPACCOUNT	= EXPENSE$

	PUT	#RM_JOURNAL.CH%

	!
	! RM_JOURPROD RECORD
	!
	RM_JOURPROD::PRODUCT	= PD_PRODUCT::PRODUCT_NUM
	RM_JOURPROD::LOCATION	= LOCATION$
	RM_JOURPROD::STARTDATE	= DATES$
	RM_JOURPROD::TRANSTYPE	= TYPES$
	RM_JOURPROD::SEQNUM	= "0000"
	RM_JOURPROD::PRICE	= PRICES
	RM_JOURPROD::QUANTITY(0) = VAL(QUANTITY$)

	PUT	#RM_JOURPROD.CH%

	IF QUANTITY$ = NUM_SOLD_REG$
	THEN
		NUM_SOLD_REG$ = "0"
	ELSE
		IF QUANTITY$ = NUM_SOLD_FREE$
		THEN
			NUM_SOLD_FREE$ = "0"

			IF QUANTITY$ = NUM_SOLD_FRE_EM$
			THEN
				RETURN
			ELSE
				GOTO 2010
			END IF

2200	CLOSE #PD_PRODUCT.CH%
	CLOSE #RM_JOURNAL.CH%
	CLOSE #RM_CONTROL.CH%
	CLOSE #RM_JOURPROD.CH%

	RETURN

 ExitProgram:
15000	!*******************************************************************
	! Exit program
	!*******************************************************************

	CLOSE READFILE.CH%

	GOTO 32767

	%PAGE

 HelpError:
	!*******************************************************************
	! Help Message for an Error
	!*******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

19000	!*******************************************************************
	! Error trapping
	!*******************************************************************
	SELECT ERR
	CASE 11%	! End of file
		FILENAME$ = ""
		SELECT ERL
		CASE 550%	! End of ascii file
			RESUME ExitProgram
		END SELECT

	CASE 154%	! Locked Block
		SLEEP 1%
		RESUME
	END SELECT

	RESUME HelpError

	!
	! FUNCTION TO CHECK IF THERE ARE xx'S
	!
30000	DEF CHECK (STRING T_LINE, WORD START, WORD LENGTH, BYTE IMAGE_TYPE)

		DIVIDE_V = 1.0
		EXT_SPACE% = 0%
		SELECT IMAGE_TYPE
		CASE 1%
			P_IMAGE_TYPE$ = "# "
		CASE 2%
			P_IMAGE_TYPE$ = "## "
		CASE 3%
			P_IMAGE_TYPE$ = "### "
		CASE 4%
			P_IMAGE_TYPE$ = "##.## "
			EXT_SPACE% = 1%
			DIVIDE_V = 100.0
		CASE 5%
			P_IMAGE_TYPE$ = "##### "
		CASE 6%
			P_IMAGE_TYPE$ = "###.## "
			DIVIDE_V = 100.0
		CASE 7%
			P_IMAGE_TYPE$ = "##,###,### "
			EXT_SPACE% = 2%
		CASE 8%
			P_IMAGE_TYPE$ = "###,###.## "
			EXT_SPACE% = 2%
			DIVIDE_V = 100.0
		CASE 9%
			P_IMAGE_TYPE$ = "######### "
		END SELECT

		TEMP$ = SPACE$(EXT_SPACE%) + MID(T_LINE, START, LENGTH) + " "
		TEMP$ = FORMAT$(VAL(TEMP$) / DIVIDE_V, P_IMAGE_TYPE$) &
			IF INSTR(1%, TEMP$, "x") = 0%
		CHECK = TEMP$

	END DEF

32767	END
