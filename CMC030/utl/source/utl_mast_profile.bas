1	%TITLE "Company Profile Maintenance"
	%SBTTL "UTL_MAST_PROFILE"
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
	!	The ^*Company Profile Maintenance\* option
	!	maintains certain information relating to the company, such as
	!	screen banner display; reports title; location codes, descriptions and
	!	addresses; departmental organization; work centers; and accounting
	!	era (periods) information.
	!
	! Index:
	!	.x Maintain>Company Profile
	!	.x Profile>Company>Maintenance
	!	.x Company>Profile>Maintenance
	!	.x Banner Display
	!	.x Reports>Title
	!	.x Location>Codes
	!	.x Location>Descriptions
	!	.x Location>Addresses
	!	.x Table>Location
	!	.x Table>Departments
	!	.x Table>Work Centers
	!	.x Table>Era
	!	.x Era>Table
	!	.x Department>Table
	!	.x Work Center>Table
	!
	! Option:
	!
	!	UTL_MAIN_PROFILE$HELP
	!	UTL_MAIN_LOCATION$HELP
	!	UTL_MAIN_DEPARTMENT$HELP
	!	UTL_MAIN_WORKCENTER$HELP
	!	UTL_MAIN_ERA$HELP
	!	UTL_MAIN_PERIOD$HELP
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_MAST_PROFILE/LINE
	!	$ LINK/EXE=UTL_EXE: UTL_MAST_PROFILE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UTL_MAST_PROFILE.OBJ;*
	!
	! Author:
	!
	!	07/22/87 - Kevin Handy
	!
	! Modification history:
	!
	!	12/01/87 - Frank Starman
	!		Add UTL_MAIN_LOCATION
	!		Add UTL_MAIN_DEPARTMENT
	!
	!	12/08/87 - Frank Starman
	!		Add UTL_MAIN_ERA
	!		Add UTL_MAIN_PERIOD
	!
	!	12/16/87 - Frank Starman
	!		Add UTL_MAIN_WORKCENTER
	!
	!	08/12/88 - Kevin Handy
	!		Open UTL_SET in proper access mode in mast program.
	!
	!	02/03/89 - Frank Starman
	!		New layouts for profile files
	!		Used MAIN_WINDOW instead MAIN_JOURNAL
	!		Remove set file
	!
	!	05/21/90 - Frank F. Starman
	!		Added COMMAND help message.
	!
	!	06/16/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/30/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--

	!++
	! Abstract:COMMAND
	!	^*PROFILE\*
	!	.p
	!	^*Profile\* maintains certain information relating to the company,
	!	such as
	!	screen banner display, reports title, location codes, descriptions, and
	!	addresses, departmental organization, work centers, and accounting
	!	era (periods) information.
	!	.p
	!	^*Format: PROFILE\*
	!	.p
	!	^*Example:\*
	!	.literal
	!	Menu Command Level: /PROFILE
	!	.end literal
	!
	! Index:
	!	.x PROFILE
	!
	! Option:
	!	UTL_MAIN_PROFILE$HELP
	!	UTL_MAIN_LOCATION$HELP
	!	UTL_MAIN_DEPARTMENT$HELP
	!	UTL_MAIN_WORKCENTER$HELP
	!	UTL_MAIN_ERA$HELP
	!	UTL_MAIN_PERIOD$HELP
	!	UTL_RPRT_PERIOD$COMMAND
	!	UTL_RPRT_PROFILE$COMMAND
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION MAIN_WINDOW

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	!*******************************************************************
	! Handle main program
	!*******************************************************************

	V% = MAIN_WINDOW(UTL_MAIN_PROFILE.ID, "")

	!******************************************************************
	! End of the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

19990	END



20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD UTL_LOCATION
	MAP (UTL_LOCATION_OLD)	UTL_LOCATION_CDD UTL_LOCATION_OLD
	MAP (UTL_LOCATION_ONE)	UTL_LOCATION_CDD UTL_LOCATION_ONE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_DEPARTMENT.HB"
	MAP (UTL_DEPARTMENT)		UTL_DEPARTMENT_CDD UTL_DEPARTMENT
	MAP (UTL_DEPARTMENT_OLD)	UTL_DEPARTMENT_CDD UTL_DEPARTMENT_OLD
	MAP (UTL_DEPARTMENT_ONE)	UTL_DEPARTMENT_CDD UTL_DEPARTMENT_ONE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_ERA.HB"
	MAP (UTL_ERA)		UTL_ERA_CDD UTL_ERA
	MAP (UTL_ERA_OLD)	UTL_ERA_CDD UTL_ERA_OLD
	MAP (UTL_ERA_ONE)	UTL_ERA_CDD UTL_ERA_ONE

	EXTERNAL LONG FUNCTION MAIN_WINDOW
	EXTERNAL LONG FUNCTION MAIN_JOURNAL

	EXTERNAL LONG FUNCTION UTL_MAIN_REGION
	EXTERNAL LONG FUNCTION UTL_MAIN_PROFILE
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION
	EXTERNAL LONG FUNCTION UTL_MAIN_DEPARTMENT
	EXTERNAL LONG FUNCTION UTL_MAIN_WORKCENTER
	EXTERNAL LONG FUNCTION UTL_MAIN_ERA
	EXTERNAL LONG FUNCTION UTL_MAIN_PERIOD

	EXTERNAL LONG FUNCTION WRIT_PERIOD

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE UTL_MAIN_PROFILE.ID

		MAINT_GROUP = UTL_MAIN_PROFILE(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

		SELECT MOPTION
		CASE OPT_OPTLIST
			MVALUE = MVALUE + " Location Era"
		CASE OPT_MOREMENU
			SELECT EDIT$(MVALUE, -1%)
			CASE "LOCATION"
				MAINT_GROUP = MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "")
			CASE "ERA"
				MAINT_GROUP = MAIN_WINDOW(UTL_MAIN_ERA.ID, "")
			END SELECT
		END SELECT

	CASE UTL_MAIN_LOCATION.ID

		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

		SELECT MOPTION
		CASE OPT_OPTLIST
			MVALUE = MVALUE + " dePartment"
		CASE OPT_MOREMENU
			UTL_LOCATION_ONE = UTL_LOCATION
			MAINT_GROUP = MAIN_WINDOW(UTL_MAIN_DEPARTMENT.ID, "")
		CASE OPT_AFTEROPT
			SELECT MVALUE
			CASE "Change", "Blank", "Initialize"
				IF UTL_LOCATION::LOCATION <> &
					UTL_LOCATION_OLD::LOCATION
				THEN
					UTL_LOCATION_ONE = UTL_LOCATION_OLD
					MAINT_GROUP = MAIN_WINDOW( &
						UTL_MAIN_DEPARTMENT.ID, "C")
					MAINT_GROUP = MAIN_WINDOW( &
						UTL_MAIN_WORKCENTER.ID, "C")
				END IF
			!
			! Erase records in subwindow
			!
			CASE "Erase"
				UTL_LOCATION_ONE = UTL_LOCATION
				MAINT_GROUP = MAIN_WINDOW(UTL_MAIN_DEPARTMENT.ID, "E")
				MAINT_GROUP = MAIN_WINDOW(UTL_MAIN_WORKCENTER.ID, "E")
			END SELECT
		END SELECT

	CASE UTL_MAIN_DEPARTMENT.ID

		SELECT MOPTION
		CASE OPT_RESETDEFAULT
			MVALUE = UTL_LOCATION_ONE::LOCATION
		CASE OPT_SUBWIND
			SELECT MLOOP
			CASE 6%
				MVALUE = UTL_LOCATION::LOCATION
			CASE ELSE
				MVALUE = UTL_LOCATION_ONE::LOCATION
			END SELECT
		END SELECT

		MAINT_GROUP = UTL_MAIN_DEPARTMENT(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

		SELECT MOPTION
		CASE OPT_OPTLIST
			MVALUE = MVALUE + " Work_center "
		CASE OPT_MOREMENU
			UTL_DEPARTMENT_ONE = UTL_DEPARTMENT
			MAINT_GROUP = MAIN_WINDOW(UTL_MAIN_WORKCENTER.ID, "")
		CASE OPT_AFTEROPT
			SELECT MVALUE
			!
			! Need to remove under old key, and insert under
			! (possibly) new key
			!
			CASE "Change", "Blank", "Initialize"
				IF UTL_DEPARTMENT::LOCATION+ &
					UTL_DEPARTMENT::DEPT_NUM <> &
						UTL_DEPARTMENT_OLD::LOCATION+ &
						UTL_DEPARTMENT_OLD::DEPT_NUM
				THEN
					UTL_DEPARTMENT_ONE = UTL_DEPARTMENT_OLD
					MAINT_GROUP = MAIN_WINDOW( &
						UTL_MAIN_WORKCENTER.ID, "C")
				END IF
			!
			! Erase records in subwindow
			!
			CASE "Erase"
				UTL_DEPARTMENT_ONE = UTL_DEPARTMENT
				MAINT_GROUP = MAIN_WINDOW(UTL_MAIN_WORKCENTER.ID, "E")
			END SELECT
		END SELECT

	CASE UTL_MAIN_WORKCENTER.ID

		SELECT MOPTION
		CASE OPT_RESETDEFAULT
			MVALUE = UTL_DEPARTMENT::LOCATION+ &
				UTL_DEPARTMENT::DEPT_NUM
		CASE OPT_SUBWIND
			SELECT MLOOP
			CASE 6%
				MVALUE = UTL_DEPARTMENT::LOCATION+ &
					UTL_DEPARTMENT::DEPT_NUM
			CASE ELSE
				MVALUE = UTL_DEPARTMENT_ONE::LOCATION+ &
					UTL_DEPARTMENT_ONE::DEPT_NUM
			END SELECT
		END SELECT

		MAINT_GROUP = UTL_MAIN_WORKCENTER(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_ERA.ID

		MAINT_GROUP = UTL_MAIN_ERA(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

		SELECT MOPTION
		CASE OPT_OPTLIST
			MVALUE = MVALUE + " Period"
		CASE OPT_MOREMENU
			UTL_ERA_ONE = UTL_ERA
 TryAgain:
			MAINT_GROUP = MAIN_JOURNAL(UTL_MAIN_PERIOD.ID, "")
			GOTO TryAgain IF WRIT_PERIOD = 2%

		CASE OPT_AFTEROPT
			SELECT SCOPE::PRG_ITEM
			CASE "Change", "Blank", "Initialize"
				IF UTL_ERA::ERA <> &
						UTL_ERA_OLD::ERA
				THEN
					MAINT_GROUP = MAIN_WINDOW( &
						UTL_MAIN_PERIOD.ID, "C" + &
						UTL_ERA::ERA)
				END IF
			!
			! Erase records in subwindow
			!
			CASE "Erase"
				!UTL_ERA_ONE = UTL_ERA
				MAINT_GROUP = MAIN_WINDOW(UTL_MAIN_PERIOD.ID, "E")
			END SELECT
		END SELECT

	CASE UTL_MAIN_PERIOD.ID

		MAINT_GROUP = UTL_MAIN_PERIOD(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_REGION.ID
		SELECT MOPTION
		CASE OPT_RESETDEFAULT
			MVALUE = UTL_LOCATION_ONE::COUNTRY
		CASE OPT_SUBWIND
			SELECT MLOOP
			CASE 6%
				MVALUE = UTL_LOCATION::COUNTRY
			CASE ELSE
				MVALUE = UTL_LOCATION_ONE::COUNTRY
			END SELECT
		END SELECT

			MAINT_GROUP = UTL_MAIN_REGION(SMG_WINDOW, &
				MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
