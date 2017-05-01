1	%TITLE "Accounts Payable Control File Maintenance"
	%SBTTL "AP_CNTR_CONTROL"
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
	!	When the Accounts Payable system is initialized, the ^*Maintain
	!	Accounts Payable Control Record\* option accesses:
	!	.table 3,25
	!	.te
	!	Determine the beginning transaction number
	!	.te
	!	Enter the Accounts Payable default account number
	!	.te
	!	Enter the Discounts Lost default account number
	!	.te
	!	Enter the Cash Account default account number
	!	.te
	!	Determine the number of periods to retain Accounts
	!	Payable historical information
	!	.te
	!	Establish the current year reference
	!	.te
	!	Establish the last period closed reference
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Control Record
	!
	! Option:
	!	AP_MAIN_CONTROL$HELP
	!
	! Author:
	!
	!	08/10/87 - B. Craig Larsen
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_CNTR_CONTROL/LINE
	!	$ LINK/EXEC=AP_EXE:*.EXE AP_CNTR_CONTROL, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_CNTR_CONTROL.OBJ;*
	!
	! Modification history:
	!
	!	05/17/88 - Lance Williams
	!		Modified the header.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	11/02/95 - Kevin Handy
	!		Added code for AP_MAIN_CONTROL_ACCT
	!
	!	07/03/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"

	!
	! This common area must be mapped in both the main program and
	!
	EXTERNAL LONG		FUNCTION MAINT_GROUP
	EXTERNAL LONG		FUNCTION MAIN_WINDOW

	%PAGE

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

1000	!******************************************************************
	! Handle the main file
	!******************************************************************

	V% = MAIN_WINDOW(AP_MAIN_CONTROL.ID, "")

 ExitProgram:
	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END


20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! CDD and Maps
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	EXTERNAL LONG		FUNCTION AP_MAIN_CONTROL
	EXTERNAL LONG		FUNCTION AP_MAIN_CONTROL_ACCT
	EXTERNAL LONG		FUNCTION GL_MAIN_CHART

	%PAGE

	SELECT SMG_WINDOW::IDENT

	CASE GL_MAIN_CHART.ID

		MAINT_GROUP = GL_MAIN_CHART(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AP_MAIN_CONTROL.ID

		MAINT_GROUP = AP_MAIN_CONTROL(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE AP_MAIN_CONTROL_ACCT.ID

		MAINT_GROUP = AP_MAIN_CONTROL_ACCT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
