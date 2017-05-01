1	%TITLE "Examination of Vendor Number"
	%SBTTL "AP_EXAM_VENDOR"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AP_EXAM_VENDOR(STRING VENDORNUM, &
		AP_VENDOR_CDD AP_VENDOR_EXAM)

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
	!	This function, given a vendor number, will
	!	return the record from the Accounts Payable Vendor file
	!	associated with this vendor number.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Inputs:
	!
	!	VENDORNUM	is the given vendor number
	!
	! Outputs:
	!
	!	AP_VENDOR	is the record associated with the given
	!		vendor number.
	!	Returned Value	is CMC$_NORMAL if the search for the
	!		record was successful, CMC$_UNDEFINED if the
	!		record wasn't found, or CMC$_UNTERROR if there
	!		was an error of some sort.
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_EXAM_VENDOR
	!	$ LIB FUNC_LIB:CMCFUN/REP AP_EXAM_VENDOR
	!	$ DELETE AP_EXAM_VENDOR.OBJ;*
	!
	! Author:
	!
	!	06/21/89 - Aaron Redd
	!
	! Modification History:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/12/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/18/2000 - Kevin Handy
	!		Set up _READ when can't open file
	!		Use WHEN ERROR IN
	!
	!	10/27/2000 - Kevin Handy
	!		Use A'x'B
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
	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP	(AP_VENDOR)	AP_VENDOR_CDD	AP_VENDOR
	COM	(AP_VENDOR_INI)	AP_VENDOR_CDD	AP_VENDOR_INI

	!
	! Common memory areas
	!
	COM (CH_AP_VENDOR_READ) AP_VENDOR.CH%

	DECLARE LONG EXIT_STATUS

	%PAGE

	!
	! Set returned initial value (assume failure)
	!
	EXIT_STATUS = CMC$_UNDEFINED

	!
	! Open the AP Vendor file if not already open
	!
100	IF AP_VENDOR.CH% <= 0%
	THEN
		AP_VENDOR_INI::VENNUM	= &
			STRING$(LEN(AP_VENDOR_INI::VENNUM), A"?"B)
		AP_VENDOR_INI::VENNAM	= &
			STRING$(LEN(AP_VENDOR_INI::VENNAM), A"?"B)
		AP_VENDOR_INI::ADD1	= &
			STRING$(LEN(AP_VENDOR_INI::ADD1), A"?"B)
		AP_VENDOR_INI::ADD2	= &
			STRING$(LEN(AP_VENDOR_INI::ADD2), A"?"B)
		AP_VENDOR_INI::CITY	= &
			STRING$(LEN(AP_VENDOR_INI::CITY), A"?"B)
		AP_VENDOR_INI::STATE	= &
			STRING$(LEN(AP_VENDOR_INI::STATE), A"?"B)
		AP_VENDOR_INI::ZIP	= &
			STRING$(LEN(AP_VENDOR_INI::ZIP), A"?"B)
		AP_VENDOR_INI::COUNTRY	= &
			STRING$(LEN(AP_VENDOR_INI::COUNTRY), A"?"B)
		AP_VENDOR_INI::PHONE	= &
			STRING$(LEN(AP_VENDOR_INI::PHONE), A"?"B)
		AP_VENDOR_INI::POADD1	= &
			STRING$(LEN(AP_VENDOR_INI::POADD1), A"?"B)
		AP_VENDOR_INI::POADD2	= &
			STRING$(LEN(AP_VENDOR_INI::POADD2), A"?"B)
		AP_VENDOR_INI::POCITY	= &
			STRING$(LEN(AP_VENDOR_INI::POCITY), A"?"B)
		AP_VENDOR_INI::POSTATE	= &
			STRING$(LEN(AP_VENDOR_INI::POSTATE), A"?"B)
		AP_VENDOR_INI::POZIP	= &
			STRING$(LEN(AP_VENDOR_INI::POZIP), A"?"B)
		AP_VENDOR_INI::POCOUNTRY	= &
			STRING$(LEN(AP_VENDOR_INI::POCOUNTRY), A"?"B)
		AP_VENDOR_INI::POPHONE	= &
			STRING$(LEN(AP_VENDOR_INI::POPHONE), A"?"B)
		AP_VENDOR_INI::PURGE	= &
			STRING$(LEN(AP_VENDOR_INI::PURGE), A"?"B)
		AP_VENDOR_INI::FEDID	= &
			STRING$(LEN(AP_VENDOR_INI::FEDID), A"?"B)
		AP_VENDOR_INI::FLG1099	= &
			STRING$(LEN(AP_VENDOR_INI::FLG1099), A"?"B)
		AP_VENDOR_INI::DUEDAYS	= 0%
		AP_VENDOR_INI::DUEDATE	= &
			STRING$(LEN(AP_VENDOR_INI::DUEDATE), A"?")
		AP_VENDOR_INI::DISDAYS	= 0%
		AP_VENDOR_INI::DISDATE	= &
			STRING$(LEN(AP_VENDOR_INI::DISDATE), A"?")
		AP_VENDOR_INI::DISCPER	= 0%
		AP_VENDOR_INI::ALPSRT	= &
			STRING$(LEN(AP_VENDOR_INI::ALPSRT), A"?")

		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.OPN"
		USE
			AP_VENDOR_EXAM = AP_VENDOR_INI

			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "AP_VENDOR"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Get the AP_VENDOR record and set it to be returned to the main program
	!
200	WHEN ERROR IN
		GET #AP_VENDOR.CH%, KEY #0% EQ VENDORNUM, REGARDLESS
	USE
		AP_VENDOR_EXAM = AP_VENDOR_INI

		CONTINUE ExitFunction IF (ERR = 155%) OR (ERR = 9%)
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

	AP_VENDOR_EXAM = AP_VENDOR

	!
	! Reset returned value for success
	!
	EXIT_STATUS = CMC$_NORMAL

 ExitFunction:
	AP_EXAM_VENDOR = EXIT_STATUS

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
	! End of function AP_EXAM_VENDOR
	!******************************************************************
	END FUNCTION
