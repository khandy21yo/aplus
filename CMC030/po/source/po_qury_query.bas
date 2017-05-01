1	%TITLE "Query the Purchase Order"
	%SBTTL "PO_QURY_QUERY"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1992 BY
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
	! ID:POQURY
	!
	! Abstract:HELP
	!	.p
	!	The ^*Query Purchase Order File\* option
	!	makes on-line inquiries into selected purchase orders.
	!
	! Index:
	!	.x Query
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_QURY_QUERY/LINE
	!	$ LINK/EXECUTABLE=PO_EXE: PO_QURY_QUERY,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PO_QURY_QUERY.OBJ;*
	!
	! Author:
	!
	!	03/06/92 - Dan Perkins
	!		Copied from Frank's IC query program.
	!
	! Modification history:
	!
	!	03/12/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	12/17/92 - Frank F. Starman
	!		Added option Product.
	!
	!	04/12/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/04/94 - Kevin Handy
	!		Added parameter to PO_READ_REGLINE
	!
	!	03/23/95 - Kevin Handy
	!		(V3.6)
	!		Modified to display mode digits in numbers (KingB).
	!		Reformatted source closer to 80 columns.
	!
	!	04/12/95 - Kevin Handy
	!		Update to V3.6 coding standards.
	!		Change SMG_QUERY to SMG_QUERY%
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC_STARLET for LIB$ routines
	!
	!	11/02/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	%INCLUDE "FUNC_INCLUDE:PO_WINDOW.INC"

	!
	! Map file
	!
	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	MAP(PO_REG_LINE)	PO_REG_LINE_CDD		PO_REG_LINE
	DECLARE			PO_REG_LINE_CDD		PO_REG_LINE_READ

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.HB"
	DECLARE			PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE_READ

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	DECLARE			UTL_LOCATION_CDD	UTL_LOCATION_EXAM

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	DECLARE			AP_VENDOR_CDD		AP_VENDOR_EXAM

	!
	! External functions
	!
	EXTERNAL LONG		FUNCTION UTL_EXAM_LOCATION
	EXTERNAL LONG		FUNCTION AP_EXAM_VENDOR
	EXTERNAL LONG		FUNCTION PO_READ_REG_LINE

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
		%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.OPN"
	USE
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

	GOSUB Initialize

	!
	! Create a display window
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		13%, &
		70%, &
		SMG_QUERY%, &
		SMG$M_BORDER &
	)

	!
	! Label the display
	!
	SMG_STATUS% = SMG$LABEL_BORDER(SMG_QUERY%, &
		"Purchase Order Query for " + TRM$(SCOPE::PRG_COMPANY))

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, "Purchase Order#", 2%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, "Line#", 3%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, "PO Type", 4%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, "Location ", 5%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, "PO Date ", 6%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, "Vendor#", 7%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, "Product", 8%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, "Description", 9%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, "UOM", 10%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, "Open/Close", 11%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, "QtyOrd     QtyRec     QtyCan" + &
			"     QtyInv     Balance", 12%, 2%)

	GOSUB Repaint

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_QUERY%, &
		SCOPE::SMG_PBID, &
		2%, &
		6% &
	)

	%PAGE

	OPT$ = "F"
	GOTO SelectOption

1000	!******************************************************************
	! Main option menu
	!******************************************************************

	!
	! Enter options
	!
	SCOPE::PRG_ITEM    = "H"
	SCOPE::PRG_IDENT   = "HELP"
	SCOPE::PRG_PROGRAM = "PO_QURY_QUERY"

	OPTLIST$ = "Find Next Help Product eXit"
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

  SelectOption:
	SELECT OPT$

	!
	! Call the help message
	!
	CASE "H"
		CALL HELP_34MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, SCOPE::PRG_PROGRAM, &
			"", "HELP")
		GOTO 1000

	CASE "F"
1120		PO_REG_LINE::PO = ENTR_3STRING(SCOPE, SMG_QUERY%, "2;20", &
			"PO Number", PO_REG_LINE::PO, MFLAG% OR 2%, "~R 'E", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT

		!
		! List Choices
		!
		CASE SMG$K_TRM_F14
			SCOPE::PRG_ITEM = "FLD001"
			PONUM$ = PO_REG_LINE::PO
			IF MAIN_WINDOW(PO_MAIN_REGLINE.ID, "VX") <> 1%
			THEN
				PO_REG_LINE::PO = PONUM$
			END IF
			GOTO 1120

		CASE SMG$K_TRM_CTRLC
			GOTO 1120

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT

1150		WHEN ERROR IN
			FIND #PO_REG_LINE.CH%, KEY #0% GE PO_REG_LINE::PO, REGARDLESS
			GET #PO_REG_LINE.CH%, REGARDLESS
		USE
			IF ERR = 155%
			THEN
				CALL ENTR_3MESSAGE(SCOPE, "Purchase Order not found", 0%)
				CONTINUE 1120
			END IF

			FILENAME$ = "PO_REG_LINE"
			CONTINUE HelpError
		END WHEN

		GOSUB Repaint

	CASE "N"
3000		WHEN ERROR IN
			GET #PO_REG_LINE.CH%, REGARDLESS
		USE
			IF ERR = 11%
			THEN
				CALL ENTR_3MESSAGE(SCOPE, "End of file", 0%)
				CONTINUE 1000
			END IF

			FILENAME$ = "PO_REG_LINE"
			CONTINUE HelpError
		END WHEN

	CASE "P"
		!
		! Loop up the inventory query
		!
		V% = LIB$SET_SYMBOL("CMC$SYSTEM", "PO")
		V% = LIB$SET_SYMBOL("CMC$SPAWN", "LO")
		CALL SUBR_3SPAWN(SCOPE, "RUN CMC$ROOT:[IC]IC_QURY_QUERY")
		V% = LIB$SET_SYMBOL("CMC$SYSTEM", "")
		V% = LIB$SET_SYMBOL("CMC$SPAWN", "")

	CASE "X"
		GOTO ExitProgram

	END SELECT

	GOSUB Repaint

	SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(TEMP.DISPLAY%, SCOPE::SMG_PBID)
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(2%, 132%, TEMP.DISPLAY%)
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(TEMP.DISPLAY%, SCOPE::SMG_PBID, 25%, 1%)

	GOTO 1000

	%PAGE

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************

	SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(TEMP.DISPLAY%, SCOPE::SMG_PBID)

	CALL LIB$GET_SYMBOL("CMC$SPAWN" BY DESC, USER_COMMANDS$ BY DESC,,)
	CALL SUBR_3EXITPROGRAM(SCOPE, USER_COMMANDS$, "")

 Repaint:
	!**************************************************************
	! Repaint product information
	!**************************************************************

	!
	! Get some other info first
	!
	V% = UTL_EXAM_LOCATION(PO_REG_LINE::FROMLOCATION, UTL_LOCATION_EXAM)

	V% = AP_EXAM_VENDOR(PO_REG_LINE::VENDOR, AP_VENDOR_EXAM)

	V% = PO_READ_REG_LINE(PO_REG_LINE::PO, PO_REG_LINE::PO_LINE, "EQ", &
		PO_REG_LINE_READ, PO_REG_SUB_LINE_READ, QTY(), "")

	!
	! Put the jazz on the screen
	!

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		CONV_STRING(PO_REG_LINE::PO, CMC$_LEFT), &
		2%, 20%,, SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		PO_REG_LINE::PO_LINE, 3%, 20%,, SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		PO_REG_LINE::PO_TYPE, 4%, 20%,, SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		PO_REG_LINE::FROMLOCATION, 5%, 20%,, SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		UTL_LOCATION_EXAM::LOCNAME, 5%, 35%,, SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		PRNT_DATE(PO_REG_LINE::ORDDATE, 8%), &
		6%, 20%,, SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		PO_REG_LINE::VENDOR, 7%, 20%,, SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		AP_VENDOR_EXAM::VENNAM, 7%, 35%,, SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		PO_REG_LINE::PRODUCT, 8%, 20%,, SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		PO_REG_LINE::DESCRIPTION, 9%, 20%,, SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		PO_REG_LINE::UOM, 10%, 20%,, SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		PO_REG_LINE::OPEN_CLOSE, 11%, 20%,, SMG$M_BOLD)

	TEXT$ = FORMAT$(QTY(1%), "#######") + " "  + &
		FORMAT$(QTY(2%), "#########") + " "  + &
		FORMAT$(QTY(3%), "##########") + " "  + &
		FORMAT$(QTY(9%), "##########") + " " + &
		FORMAT$(QTY(0%), "###########")

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		TEXT$, 13%, 2%,, SMG$M_BOLD)

	RETURN

	%Page

 Initialize:
	!*******************************************************************
	! Set Initialize values
	!*******************************************************************
		PO_REG_LINE::PO = &
			STRING$(LEN(PO_REG_LINE::PO), A"?")

		PO_REG_LINE::PO_LINE = &
			STRING$(LEN(PO_REG_LINE::PO_LINE), A"?")

		PO_REG_LINE::PO_TYPE = &
			STRING$(LEN(PO_REG_LINE::PO_TYPE), A"?")

		PO_REG_LINE::FROMLOCATION = &
			STRING$(LEN(PO_REG_LINE::FROMLOCATION), A"?")

		PO_REG_LINE::ORDDATE = &
			STRING$(LEN(PO_REG_LINE::ORDDATE), A"?")

		PO_REG_LINE::VENDOR = &
			STRING$(LEN(PO_REG_LINE::VENDOR), A"?")

		PO_REG_LINE::PRODUCT = &
			STRING$(LEN(PO_REG_LINE::PRODUCT), A"?")

		PO_REG_LINE::DESCRIPTION = &
			STRING$(LEN(PO_REG_LINE::DESCRIPTION), A"?")

		PO_REG_LINE::UOM = &
			STRING$(LEN(PO_REG_LINE::UOM), A"?")

		PO_REG_LINE::OPEN_CLOSE = &
			STRING$(LEN(PO_REG_LINE::OPEN_CLOSE), A"?")

	RETURN

	%Page

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

	%Page

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	RESUME HelpError

19999	END

20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:PO_WINDOW.INC"

	EXTERNAL LONG FUNCTION PO_MAIN_REGLINE

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE PO_MAIN_REGLINE.ID

		MAINT_GROUP = PO_MAIN_REGLINE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
