1	%TITLE "Job Register Maintenance"
	%SBTTL "WP_MAST_REGISTER"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
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
	!	..lm +5
	!	.b
	!	The ^*Job Register Maintenance\* option
	!	maintains Job Register records.
	!	.NOTE
	!	^*This file should not routinely be edited.  Certain changes could have negative
	!	and/or unexpected consequences.\*
	!	.end note
	!	.lm -5
	!
	! Index:
	!	.x Job Register Maintenance
	!	.x Maintenance>Job Register
	!
	! Option:
	!	JC_MAIN_JOB$HELP
	!	WP_MAIN_REGLINE$HELP
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_MAST_REGISTER/LINE
	!	$ LINK/EXE=WP_EXE: WP_MAST_REGISTER,-
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_MAST_REGISTER.OBJ;*
	!
	! Author:
	!
	!	05/29/91 - Val James "kool hand" Allen
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	07/08/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/22/98 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:JC_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION MAIN_WINDOW

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

400	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(JC_MAIN_JOB.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19000	!******************************************************************
	! Error trapping
	!******************************************************************

19990	END


20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:JC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:SB_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:WP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP	(SB_SUBACCOUNT)	JC_JOB_CDD	JC_JOB
	MAP	(JC_JOB_OLD)	JC_JOB_CDD	JC_JOB_OLD
	MAP	(JC_JOB_ONE)	JC_JOB_CDD	JC_JOB_ONE

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP	(SB_SUBACCOUNT)		SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	EXTERNAL LONG FUNCTION MAIN_WINDOW

	EXTERNAL LONG FUNCTION JC_MAIN_TYPE
	EXTERNAL LONG FUNCTION JC_MAIN_CLASS
	EXTERNAL LONG FUNCTION JC_MAIN_JOB
	EXTERNAL LONG FUNCTION WP_MAIN_REGLINE
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION
	EXTERNAL LONG FUNCTION PD_MAIN_PRODUCT

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE JC_MAIN_JOB.ID

		MAINT_GROUP = JC_MAIN_JOB(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

		SELECT MOPTION

		CASE OPT_OPTLIST
			MVALUE = MVALUE + " Lines "

		CASE OPT_MOREMENU
			JC_JOB_ONE = JC_JOB

			SELECT EDIT$(MVALUE, -1%)
			!
			! Lines
			!
			CASE "LINES"
				MAINT_GROUP = MAIN_WINDOW( &
					WP_MAIN_REGLINE.ID, "")
			END SELECT

		CASE OPT_AFTEROPT

			SELECT MVALUE
			!
			! Need to remove under old key, and insert under
			! (possibly) new key
			!
			CASE "Change", "Blank", "Initialize"
				IF JC_JOB::SUBJECT+ &
					JC_JOB::JOB <> &
						JC_JOB_OLD::SUBJECT + &
						JC_JOB_OLD::JOB
				THEN
					JC_JOB_ONE = JC_JOB_OLD
					MAINT_GROUP = MAIN_WINDOW( &
						WP_MAIN_REGLINE.ID, "C")
				END IF

			!
			! Erase records in subwindow
			!
			CASE "Erase"
				JC_JOB_ONE = JC_JOB
				MAINT_GROUP = MAIN_WINDOW( &
					WP_MAIN_REGLINE.ID, "E")
			END SELECT
		END SELECT

	CASE WP_MAIN_REGLINE.ID

		SELECT MOPTION
		CASE OPT_RESETDEFAULT
			MVALUE = JC_JOB_ONE::JOB
		CASE OPT_SUBWIND
			SELECT MLOOP
			CASE 6%
				MVALUE = JC_JOB::JOB
			CASE ELSE
				MVALUE = JC_JOB_ONE::JOB
			END SELECT
		END SELECT

		MAINT_GROUP = WP_MAIN_REGLINE(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_LOCATION.ID
		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE PD_MAIN_PRODUCT.ID

		MAINT_GROUP = PD_MAIN_PRODUCT(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE JC_MAIN_TYPE.ID

		MAINT_GROUP = JC_MAIN_TYPE(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	CASE JC_MAIN_CLASS.ID

		MAINT_GROUP = JC_MAIN_CLASS(SMG_WINDOW, MOPTION, &
			MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
