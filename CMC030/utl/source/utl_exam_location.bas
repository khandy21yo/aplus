1	%TITLE "Examination Location Number"
	%SBTTL "UTL_EXAM_LOCATION"
	%IDENT "V3.6a Calico"

	FUNCTION LONG UTL_EXAM_LOCATION(STRING LOCATION, &
		UTL_LOCATION_CDD UTL_LOCATION_EXAM)

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	!	.p
	!	This function is used to check undefine location number
	!	in the location description file
	!
	! Inputs
	!
	!	XLOCATION$
	!
	! Outputs
	!	UTL_EXAM_LOCATION = CMC$_NORMAL
	!	UTL_EXAM_LOCATION = <anything else> Can't find
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_EXAM_LOCATION
	!	$ LIB FUNC_LIB:CMCFUN/REP UTL_EXAM_LOCATION
	!	$ DELETE UTL_EXAM_LOCATION.OBJ;*
	!
	! AUTHOR:
	!
	!	05/05/88 - Frank Starman
	!
	! MODIFICATION HISTORY:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	10/30/96 - Kevin Handy
	!		Reformst source code
	!
	!	06/05/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/11/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	11/06/2000 - Kevin Handy
	!		Use A"x"B
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION
	COM (UTL_LOCATION_INI)	UTL_LOCATION_CDD	UTL_LOCATION_INI

	COM (CH_UTL_LOCATION_READ) UTL_LOCATION.CH%

	DECLARE LONG EXIT_STATUS

	%PAGE

	!
	! Assume undefined
	!
	EXIT_STATUS = CMC$_UNDEFINED

100	IF UTL_LOCATION.CH% <= 0%
	THEN
		UTL_LOCATION_INI::LOCATION = &
			STRING$(LEN(UTL_LOCATION_INI::LOCATION), A"?"B)
		UTL_LOCATION_INI::LOCNAME = &
			STRING$(LEN(UTL_LOCATION_INI::LOCNAME), A"?"B)
		UTL_LOCATION_INI::REGION = &
			STRING$(LEN(UTL_LOCATION_INI::REGION), A"?"B)
		UTL_LOCATION_INI::LOCGROUP = &
			STRING$(LEN(UTL_LOCATION_INI::LOCGROUP), A"?"B)
		UTL_LOCATION_INI::ADDRESS1 = &
			STRING$(LEN(UTL_LOCATION_INI::ADDRESS1), A"?"B)
		UTL_LOCATION_INI::ADDRESS2 = &
			STRING$(LEN(UTL_LOCATION_INI::ADDRESS2), A"?"B)
		UTL_LOCATION_INI::CITY = &
			STRING$(LEN(UTL_LOCATION_INI::CITY), A"?"B)
		UTL_LOCATION_INI::STATE = &
			STRING$(LEN(UTL_LOCATION_INI::STATE), A"?"B)
		UTL_LOCATION_INI::ZIP = &
			STRING$(LEN(UTL_LOCATION_INI::ZIP), A"?"B)
		UTL_LOCATION_INI::COUNTY = &
			STRING$(LEN(UTL_LOCATION_INI::COUNTY), A"?"B)
		UTL_LOCATION_INI::COUNTRY = &
			STRING$(LEN(UTL_LOCATION_INI::COUNTRY), A"?"B)
		UTL_LOCATION_INI::PHONE = &
			STRING$(LEN(UTL_LOCATION_INI::PHONE), A"?"B)
		UTL_LOCATION_INI::SHPADDRESS1 = &
			STRING$(LEN(UTL_LOCATION_INI::SHPADDRESS1), A"?"B)
		UTL_LOCATION_INI::SHPADDRESS2 = &
			STRING$(LEN(UTL_LOCATION_INI::SHPADDRESS2), A"?"B)
		UTL_LOCATION_INI::SHPCITY = &
			STRING$(LEN(UTL_LOCATION_INI::SHPCITY), A"?"B)
		UTL_LOCATION_INI::SHPSTATE = &
			STRING$(LEN(UTL_LOCATION_INI::SHPSTATE), A"?"B)
		UTL_LOCATION_INI::SHPZIP = &
			STRING$(LEN(UTL_LOCATION_INI::SHPZIP), A"?"B)
		UTL_LOCATION_INI::SHPCOUNTY = &
			STRING$(LEN(UTL_LOCATION_INI::SHPCOUNTY), A"?"B)
		UTL_LOCATION_INI::SHPCOUNTRY = &
			STRING$(LEN(UTL_LOCATION_INI::SHPCOUNTRY), A"?"B)
		UTL_LOCATION_INI::SHPPHONE = &
			STRING$(LEN(UTL_LOCATION_INI::SHPPHONE), A"?"B)

		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "UTL_LOCATION"
			CONTINUE HelpError
		END WHEN
	END IF

200	WHEN ERROR IN
		GET #UTL_LOCATION.CH%, KEY #0% EQ LOCATION, REGARDLESS
	USE
		UTL_LOCATION_EXAM = UTL_LOCATION_INI

		CONTINUE ExitFunction IF (ERR = 155%) OR (ERR = 9%)
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

	UTL_LOCATION_EXAM = UTL_LOCATION
	EXIT_STATUS = CMC$_NORMAL

 ExitFunction:
	UTL_EXAM_LOCATION = EXIT_STATUS

	EXIT FUNCTION

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	EXIT_STATUS = CMC$_UNTERROR
	GOTO ExitFunction

32767	END FUNCTION
