1	%TITLE "Maintain Operations Table"
	%SBTTL "PR_MAST_OPER"
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
	!	The ^*Maintain Operations Table\* defines operations.
	!
	! Index:
	!	.x Operation>Table
	!	.x Tables>Operation
	!
	! Option:
	!	PR_MAIN_OPER$HELP
	!
	! Author:
	!
	!	11/24/87 - B. Craig Larsen
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAST_OPER
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_MAST_OPER, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_MAST_OPER.OBJ;*
	!
	! Modification history:
	!
	!	12/11/90 - Craig Tanner
	!		Split in to two modules, PR_MAST_OPER and PR_MAIN_OPER
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/24/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW

	%PAGE

400	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(PR_MAIN_OPER.ID, "")

 ExitProgram:
	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION PR_MAIN_OPER

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	!
	! Process the Operations maintenance window
	!
	CASE PR_MAIN_OPER.ID
		MAINT_GROUP = PR_MAIN_OPER(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
	!+-+-+
	!++
	! Abstract:FLD003
	!	^*(03) Unit Rate\*
	!	.p
	!	The ^*Unit Rate\* field defines
	!	the industrial standard unit rate for the specific
	!	subject operation as of a specified effective date.
	!
	! Index:
	!	.x Operation>Pieces Per Hour
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD001
	!	^*(01) Operation\*
	!	.p
	!	The ^*Operation\* field
	!	enters an operation name or code up to eight (8) alphanumeric
	!	characters in length. The field is a key. Duplicates are allowed
	!	in order to have more than one record for the same operation, but with
	!	different effective dates and different industrial standards for
	!	pieces per hour and/or hourly rates.
	!
	! Index:
	!	.x Operation
	!	.x Operation>Key
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD002
	!	^*(02) Effective Date\*
	!	.p
	!	The ^*Effective Date\* field
	!	enters a date in MMDDYY format indicating when an
	!	industrial standard pieces per hour and/or hourly rate became or
	!	will become effective.
	!
	! Index:
	!	.x Operation>Effective Date
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD004
	!	^*(04) Hourly Rate\*
	!	.p
	!	The ^*Hourly Rate\* field
	!	enters the industrial standard hourly rate for the specific
	!	subject operation as of a specified effective date.
	!
	! Index:
	!	.x Operation>Hourly Rate
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD005
	!	^*(05) Unit/Hourly Rate\*
	!	.p
	!	This field contains the unit (03) divided by the hourly rate (04)
	!	to get an approximate unit rate. This field is used in the ^*Copy\*
	!	function to recompute the new Units based on the new hourly rate.
	!
	! Index:
	!	.x Oper>Unit/Hour
	!	.x Unit/Hour>Oper
	!
	!--
