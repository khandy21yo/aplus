1	%TITLE "Enter Purchases"
	%SBTTL "AP_MAST_PJ"
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
	!	.b
	!	.lm +5
	!	The ^*Enter Purchases\* option enters invoices
	!	and charges from vendors into a Purchases Journal.
	!	.b
	!	Vendor charges which are yet to be paid and/or charges for which hand checks
	!	have already been written are entered in the Purchases Journal.
	!	.lm -5
	!
	! Index:
	!	.x Purchases Journal>Enter Transactions
	!	.x Purchases Journal>Maintain Transactions
	!	.x Purchases Journal
	!
	! Option:
	!
	!	AP_MAIN_PJ_H$HELP
	!	AP_MAIN_PJ_L$HELP
	!
	! Author:
	!
	!	08/06/87 - B. Craig Larsen
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_MAST_PJ/LINE
	!	$ LINK/EXEC=AP_EXE:*.EXE AP_MAST_PJ,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_MAST_PJ.OBJ;*
	!
	! Modification history:
	!
	!	05/17/88 - Lance Williams
	!		Modified the header.
	!
	!	02/09/89 - Kevin Handy
	!		Modified for change in ENTR_ENTER
	!
	!	08/02/90 - Kevin Handy
	!		Modified to open PO_REG_LINE so we can use it in line
	!		item maintenance.
	!
	!	06/18/93 - Kevin Handy
	!		Added Contact maintenance in.
	!
	!	03/09/94 - Kevin Handy
	!		Modified to open chart of accounts here in a
	!		read only mode, so it will not lock out the
	!		G/L CLOSE or RESET.
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update source to V3.6 standards.
	!		Change last parameter on ENTR_3CHOICE from "" to 0%
	!
	!	05/12/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/07/98 - Kevin Handy
	!		Use PO_MAIN_REGLINE_01 instead of PO_MAIN_REGLINE
	!		for a different format of the view screen.
	!
	!	04/08/99 - Kevin Handy
	!		Include from BASIC$STARLET instead of trying to
	!		define the LIB$ routines manually
	!
	!	11/09/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	02/19/2002 - Kevin Handy
	!		Added code for form
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	!
	! Map of the various files
	!
	COM (CH_AP_PJH) &
		AP_PJH.CH%, &
		AP_PJH.READONLY%, &
		BATCH_NO$ = 2%

	COM (CH_AP_CONTROL) &
		AP_CONTROL.CH%

	COM (CH_AP_PJL) &
		AP_PJL.CH%, &
		AP_PJL.READONLY%

	COM (CH_AP_VENDOR) &
		AP_VENDOR.CH%

	COM (CH_AP_OPEN) &
		AP_OPEN.CH%

	COM (CH_AP_CLOSE) &
		AP_CLOSE.CH%

	COM (CH_PO_REG_LINE) &
		PO_REG_LINE.CH%, &
		PO_REG_LINE.READONLY%

	COM (CH_GL_CHART) &
		GL_CHART.CH%, &
		GL_CHART.READONLY%

	%INCLUDE "SOURCE:[AP.OPEN]AP_PJH.HB"
	MAP (AP_PJH)		AP_PJH_CDD	AP_PJH

	%INCLUDE "SOURCE:[AP.OPEN]AP_PJL.HB"
	MAP (AP_PJL)		AP_PJL_CDD	AP_PJL

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP (AP_OPEN)		AP_OPEN_CDD	AP_OPEN

	%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.HB"
	MAP (AP_CLOSE)		AP_CLOSE_CDD	AP_CLOSE

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.HB"
	MAP (AP_CONTROL)	AP_CONTROL_CDD	AP_CONTROL

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	MAP (PO_REG_LINE)	PO_REG_LINE_CDD	PO_REG_LINE

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD	GL_CHART

	DIM JRL_FILE$(50%)

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!
	! Look up device
	!
	CALL READ_DEVICE("AP_PJH", AP_PJH.DEV$, STAT%)

	!
	! Get info required for main file
	!
300	!
	! Kill any journals with null batch numbers.
	!
	SMG_STATUS% = LIB$DELETE_FILE(AP_PJH.DEV$ + "AP_PJH_.JRL;*")
	SMG_STATUS% = LIB$DELETE_FILE(AP_PJH.DEV$ + "AP_PJL_.JRL;*")

	!
	! Query user for year of file
	!
	CALL FIND_FILE(AP_PJH.DEV$ + "AP_PJH_*.JRL", JRL_FILE$(), &
		16%, "", "")

	JRL_FILE% = VAL%(JRL_FILE$(0%))

	IF JRL_FILE%
	THEN
		JRL_FILE$(LOOP%) = MID(JRL_FILE$(LOOP%), 8%, 2%) &
			FOR LOOP% = 1% TO JRL_FILE%

		X% = ENTR_3CHOICE(SCOPE, "", "", JRL_FILE$(), "", &
			0%, "PJ Batch Files", "", 0%)

		IF X% > 0%
		THEN
			BATCH_NO$ = EDIT$(JRL_FILE$(X%), -1%)
			GOTO 700
		END IF
	END IF

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	!
	! Ask for batch number
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 80%, SMG_SCREEN_DATA%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"Please enter a Batch number<01> ?  01", 6%, 20%)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 1%, 1%)

320	!
	! Get the journal name
	!
	BATCH_NO$ = "01"

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, &
		6%, 55%, BATCH_NO$, -1%, 16%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_F8, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO 320
	END SELECT

	BATCH_NO$ = "01" IF EDIT$(BATCH_NO$, -1%) = ""

	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

	IF LEN(EDIT$(BATCH_NO$, -1%)) <> 2%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter the batch # in (XX) format", 0%)
		GOTO 320
	END IF

700	!
	! Open main file (existing) for modification
	!
	%INCLUDE "SOURCE:[AP.OPEN]AP_PJH.CRE"
	AP_PJH.READONLY% = 0%

710	!
	! Open main file (existing) for modification
	!
	%INCLUDE "SOURCE:[AP.OPEN]AP_PJL.CRE"
	AP_PJL.READONLY% = 0%

720	!
	! Open control file
	!
	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.CRE"

730	!
	! Open OPEN file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.OPN"
	USE
		CONTINUE 740
	END WHEN

740	!
	! Open Closed file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.OPN"
	USE
		CONTINUE 750
	END WHEN

750	!
	! Open PO Register if available
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.OPN"
	USE
		CONTINUE 760
	END WHEN

760	!
	! Open chart of accounts read/only
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
	USE
		CONTINUE 800
	END WHEN

	GL_CHART.READONLY% = -1%

800	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(AP_MAIN_PJ_H.ID, "")

 ExitProgram:
	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

	%Page

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

19900	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PO_WINDOW.INC"

	EXTERNAL LONG FUNCTION AP_MAIN_PJ_H
	EXTERNAL LONG FUNCTION AP_MAIN_PJ_L
	EXTERNAL LONG FUNCTION AP_MAIN_VENDOR
	EXTERNAL LONG FUNCTION AP_MAIN_CONTACT
	EXTERNAL LONG FUNCTION GL_MAIN_CHART
	EXTERNAL LONG FUNCTION AP_MAIN_1099_TABLE
	EXTERNAL LONG FUNCTION PO_MAIN_REGLINE_01

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE GL_MAIN_CHART.ID

		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AP_MAIN_VENDOR.ID

		MAINT_GROUP = AP_MAIN_VENDOR(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AP_MAIN_CONTACT.ID

		MAINT_GROUP = AP_MAIN_CONTACT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AP_MAIN_PJ_H.ID

		MAINT_GROUP = AP_MAIN_PJ_H(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AP_MAIN_PJ_L.ID

		MAINT_GROUP = AP_MAIN_PJ_L(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AP_MAIN_1099_TABLE.ID

		MAINT_GROUP = AP_MAIN_1099_TABLE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PO_MAIN_REGLINE.ID

		MAINT_GROUP = PO_MAIN_REGLINE_01(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

	END FUNCTION

	!*******************************************************************
	! Loadvar routine
	!*******************************************************************

30000	SUB FORM_LOADVAR(P1$, P2, P3$)

	CALL AP_OUTP_PJ_LOADVAR(P1$, P2, P3$)

32767	END SUB
