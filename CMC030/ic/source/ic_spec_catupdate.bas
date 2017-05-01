1	%TITLE "Product Category Update"
	%SBTTL "IC_SPEC_CATUPDATE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1993 BY
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
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Product Category Update\* program
	!	updates blank product category fields for Robison's.
	!	It uses the first two or three characters of the product
	!	number to detirmine into which category it should be placed.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_SPEC_CATUPDATE/LINE
	!	$ LINK/EXECUTABLE=IC_EXE: IC_SPEC_CATUPDATE,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_SPEC_CATUPDATE.OBJ;*
	!
	! Author:
	!
	!	04/02/93 - Dan Perkins
	!
	! Modification history:
	!
	!	04/05/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/28/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/29/2000 - Kevin Handy
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
	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[PD.OPEN]PD_CATEGORY.HB"
	MAP (PD_CATEGORY)	PD_CATEGORY_CDD		PD_CATEGORY

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.HB"
	MAP (UTL_PROFILE)	UTL_PROFILE_CDD		UTL_PROFILE

	MAP (DP_OUTP_XUNSOL)				RRR_FLAG%

	!
	! Declare variables
	!
	DECLARE LONG   SMG_CATUPDATE

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION OUTP_UNSOLICITED

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

300	!
	! Open Product File
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.CRE"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

310	!
	! Open CategoryFile
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_CATEGORY.OPN"
	USE
		FILENAME$ = "PD_CATEGORY"
		CONTINUE HelpError
	END WHEN

320	!
	! Open Profile to get Default Location
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.OPN"
		GET #UTL_PROFILE.CH%, RECORD 1%, REGARDLESS
		CALL ASSG_FREECHANNEL(UTL_PROFILE.CH%)
	USE
		CONTINUE MakeWindow IF ERR = 5% OR ERR = 155%
		FILENAME$ = "UTL_PROFILE"
		CONTINUE HelpError
	END WHEN

 MakeWindow:
	!
	! Create a display window
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		13%, &
		70%, &
		SMG_CATUPDATE, &
		SMG$M_BORDER &
	)

	!
	! Label the display
	!
	SMG_STATUS% = SMG$LABEL_BORDER(SMG_CATUPDATE, &
		"Product Category Update for " + TRM$(SCOPE::PRG_COMPANY))

	SMG_STATUS% = SMG$PUT_CHARS(SMG_CATUPDATE, &
		"Updated Records", 4%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_CATUPDATE, &
		"Remaining Blank Records", 6%, 2%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_CATUPDATE, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)

	%PAGE

	!GOTO SelectOption

1000	!******************************************************************
	! Main option menu
	!******************************************************************

	!
	! Enter options
	!
	SCOPE::PRG_IDENT   = "H"
	SCOPE::PRG_ITEM    = "HELP"
	SCOPE::PRG_PROGRAM = "IC_SPEC_CATUPDATE"

	OPTLIST$ = "Go Help eXit"
	OPT$ = ENTR_3OPTION(SCOPE, "COMMAND", OPTLIST$, OPT%, 0%)

	SELECT SCOPE::SCOPE_EXIT
	!
	! Control C
	!
	CASE 3%
		GOTO 1000

	!
	! Exit key
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	END SELECT

 SelectOption:
	SELECT OPT$

	!
	! Call the help message
	!
	CASE "H"
		CALL HELP_34MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, SCOPE::PRG_PROGRAM, &
			"", "HELP")

		GOTO 1000

	CASE "X"
		GOTO ExitProgram

	CASE "G"
		GOTO 2000

	END SELECT

	GOTO 1000

	%PAGE

2000	CALL ENTR_3MESSAGE(SCOPE, "Working", 1% + 16%)

	!
	! Check unsolicited input
	!
	IF RRR_FLAG%
	THEN
		IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
		THEN
			EXIT_STATUS = CMC$_UNTERROR
			GOTO ExitProcess
		END IF
	END IF

	BIGGIE$ = ""
	COMMA$  = ""

	!
	! Read the Category file and put all records into a big string
	!
	WHEN ERROR IN
		RESET #PD_CATEGORY.CH%
	USE
		FILENAME$ = "PD_CATEGORY"
		CONTINUE HelpError
	END WHEN

2020	WHEN ERROR IN
		GET #PD_CATEGORY.CH%, REGARDLESS
	USE
		CONTINUE 2100 IF ERR = 11%
		FILENAME$ = "PD_CATEGORY"
		CONTINUE HelpError
	END WHEN

	BIGGIE$ = BIGGIE$ + COMMA$ + TRM$(PD_CATEGORY::CODE)
	COMMA$ = ","

	GOTO 2020

2100	UPDATED_RECS%, BLANK_RECS% = 0%

	WHEN ERROR IN
		RESET #PD_PRODUCT.CH%, KEY #2%
	USE
		CONTINUE ExitProcess IF ERR = 155%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
2120	WHEN ERROR IN
		GET #PD_PRODUCT.CH%
	USE
		CONTINUE ExitProcess IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	GOTO ExitProcess IF PD_PRODUCT::CATEGORY <> ""

	!
	! Search the biggie string for the frist three characters of
	! the product number
	!
	L% = INSTR(1%, BIGGIE$, LEFT(PD_PRODUCT::PRODUCT_NUM, 3%))

	IF L% > 0%
	THEN
		CAT$ = MID(BIGGIE$, L%, 3%)
		GOTO UpdateRec
	END IF

	!
	! Search the biggie string for the frist two characters of
	! the product number
	!
	L% = INSTR(1%, BIGGIE$, LEFT(PD_PRODUCT::PRODUCT_NUM, 2%))

	IF L% > 0%
	THEN
		CAT$ = MID(BIGGIE$, L%, 2%)
		GOTO UpdateRec
	END IF

	BLANK_RECS% = BLANK_RECS% + 1%

	SMG_STATUS% = SMG$PUT_CHARS(SMG_CATUPDATE, &
		FORMAT$(BLANK_RECS%, "#####"), 6%, 27%)

	GOTO GetNextRec

 UpdateRec:
	PD_PRODUCT::CATEGORY = CAT$

2150	WHEN ERROR IN
		UPDATE #PD_PRODUCT.CH%
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	UPDATED_RECS% = UPDATED_RECS% + 1%

	SMG_STATUS% = SMG$PUT_CHARS(SMG_CATUPDATE, &
		FORMAT$(UPDATED_RECS%, "#####"), 4%, 27%)

	GOTO GetNextRec

 ExitProcess:
	CALL ENTR_3MESSAGE(SCOPE, "", 1%)
	GOTO 1000

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

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
