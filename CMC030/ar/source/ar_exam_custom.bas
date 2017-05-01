1	%TITLE "Examination of Customer Number"
	%SBTTL "AR_EXAM_CUSTOM"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AR_EXAM_CUSTOM(STRING CUSTOMER, &
		AR_35CUSTOM_CDD AR_35CUSTOM_EXAM)

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
	!	This function, given a customer number, will
	!	return the record from the Accounts Receivable Customer file
	!	associated with this customer number.
	!	.LM -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Inputs:
	!
	!	CUSTOMER	is the given customer number
	!
	! Outputs:
	!
	!	AR_35CUSTOM	is the record associated with the given
	!		customer number.
	!	Returned Value	is CMC$_NORMAL if the search for the
	!		record was successful, CMC$_UNDEFINED if the
	!		record wasn't found, or CMC$_UNTERROR if there
	!		was an error of some sort.
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_EXAM_CUSTOM
	!	$ LIB FUNC_LIB:CMCFUN/REP AR_EXAM_CUSTOM
	!	$ DELETE AR_EXAM_CUSTOM.OBJ;*
	!
	! Author:
	!
	!	06/26/89 - Aaron Redd
	!
	! Modification History:
	!
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	06/25/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/18/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP	(AR_35CUSTOM)	AR_35CUSTOM_CDD	AR_35CUSTOM

	!
	! Common memory areas
	!
	COM	(CH_AR_35CUSTOM_READ)		AR_35CUSTOM.CH%

	DECLARE LONG EXIT_STATUS

	%PAGE

	!
	! Set returned initial value (assume failure)
	!
	EXIT_STATUS = CMC$_UNDEFINED

	!
	! Open the AR Customer file if not already open
	!
100	IF AR_35CUSTOM.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "AR_35CUSTOM"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Get the AR_35CUSTOM record and set it to be returned to the main program
	!
200	WHEN ERROR IN
		GET #AR_35CUSTOM.CH%, KEY #0% EQ CUSTOMER, REGARDLESS
	USE
		IF (ERR = 155%) OR (ERR = 9%)
		THEN
			!
			! Zap a couple of descriptive items because
			! too many reports get screwed up otherwise.
			!
			AR_35CUSTOM::CUSNUM = ""
			AR_35CUSTOM::CUSNAM = ""
			CONTINUE ExitFunction
		END IF
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	AR_35CUSTOM_EXAM = AR_35CUSTOM

	!
	! Reset returned value for success
	!
	EXIT_STATUS = CMC$_NORMAL

 ExitFunction:
	AR_EXAM_CUSTOM = EXIT_STATUS

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

32767	!******************************************************************
	! End of function AR_EXAM_CUSTOM
	!******************************************************************
	END FUNCTION
