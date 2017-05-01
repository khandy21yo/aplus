1	%TITLE "Examination of Asset Number"
	%SBTTL "AD_EXAM_ASSET"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AD_EXAM_ASSET( STRING ASSETNUMBER, &
		AD_35ASSET_CDD AD_35ASSET_EXAM)

	!
	! COPYRIGHT (C) 1989 BY
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
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This function, given an asset number, will
	!	return the record from the AD_35ASSET file associated with
	!	this asset number.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Inputs:
	!
	!	ASSETNUMBER	is the given asset number
	!
	! Outputs:
	!
	!	AD_35ASSET	is the record associated with the given
	!		asset number.
	!	Returned Value	is CMC$_NORMAL if the search for the
	!		record was successful, CMC$_UNDEFINED if the
	!		record wasn't found, or CMC$_UNTERROR if there
	!		was an error of some sort.
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_EXAM_ASSET
	!	$ LIB FUNC_LIB:CMCFUN/REP AD_EXAM_ASSET
	!	$ DELETE AD_EXAM_ASSET.OBJ;*
	!
	! Author:
	!
	!	06/16/89 - Aaron Redd
	!
	! Modification History:
	!
	!	04/04/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 level
	!
	!	08/28/96 - Kevin Handy
	!		Reformat source.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/23/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Lose AD_35ASSET_INI version of record.
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include special CMC codes
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	!
	! CDD inclusions
	!
	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.HB"
	MAP	(AD_35ASSET)	AD_35ASSET_CDD	AD_35ASSET

	!
	! Common memory areas
	!
	COM (AD_EXAM_ASSET.COM) AD_35ASSET.CH%

	DECLARE LONG EXIT_STATUS

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Set returned initial value (assume failure)
	!
	EXIT_STATUS = CMC$_UNDEFINED

	%PAGE

	!
	! Open the AD Asset file if not already open
	!
100	IF AD_35ASSET.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "AD_35ASSET"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Get the AD_35ASSET record and set it to be returned to the main program
	!
200	WHEN ERROR IN
		GET #AD_35ASSET.CH%, KEY #0% EQ ASSETNUMBER, REGARDLESS
	USE
		AD_35ASSET_EXAM::ASSET_NUM = &
			STRING$(LEN(AD_35ASSET_EXAM::ASSET_NUM), A"?"B)
		AD_35ASSET_EXAM::DESCRIPTION = &
			STRING$(LEN(AD_35ASSET_EXAM::DESCRIPTION), A"?"B)
		AD_35ASSET_EXAM::ASSET_TYPE = &
			STRING$(LEN(AD_35ASSET_EXAM::ASSET_TYPE), A"?"B)
		AD_35ASSET_EXAM::LOCATION = &
			STRING$(LEN(AD_35ASSET_EXAM::LOCATION), A"?"B)
		AD_35ASSET_EXAM::DEPT_NUM = &
			STRING$(LEN(AD_35ASSET_EXAM::DEPT_NUM), A"?"B)
		AD_35ASSET_EXAM::SERIAL_NUM = &
			STRING$(LEN(AD_35ASSET_EXAM::SERIAL_NUM), A"?"B)
		AD_35ASSET_EXAM::SERVDATE = &
			STRING$(LEN(AD_35ASSET_EXAM::SERVDATE), A"?"B)
		AD_35ASSET_EXAM::COST = 0.0
		AD_35ASSET_EXAM::SALVAGE = 0.0
		AD_35ASSET_EXAM::BONUS = 0.0
		AD_35ASSET_EXAM::ITC = 0.0
		AD_35ASSET_EXAM::ITCREDUCE = 0.0
		AD_35ASSET_EXAM::UNITS = 0.0

		CONTINUE ExitFunction IF (ERR = 155%) OR (ERR = 9%)
		FILENAME$ = "AD_35ASSET"
		CONTINUE HelpError
	END WHEN

	AD_35ASSET_EXAM = AD_35ASSET

	!
	! Reset returned value for success
	!
	EXIT_STATUS = CMC$_NORMAL

 ExitFunction:
	AD_EXAM_ASSET = EXIT_STATUS

	EXIT FUNCTION

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	EXIT_STATUS = CMC$_UNTERROR
	GOTO ExitFunction

	%PAGE

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	!
	! Trap untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

	%PAGE

32767	!******************************************************************
	! End of function AD_EXAM_ASSET
	!******************************************************************
	END FUNCTION
