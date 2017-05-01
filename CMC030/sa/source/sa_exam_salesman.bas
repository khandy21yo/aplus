1	%TITLE "Examination of Salesman and Subaccount Number"
	%SBTTL "SA_EXAM_SALESMAN"
	%IDENT "V3.6a Calico"

	FUNCTION LONG SA_EXAM_SALESMAN( STRING SALESMAN, &
		SA_SALESMAN_CDD  SA_SALESMAN_EXAM, &
		SB_SUBACCOUNT_CDD SB_SUBACCOUNT_EXAM)

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
	!
	! Abstract:HELP
	!	.p
	!	This function is used to check Salesman's number
	!	in Salesman's file and return SUBACCOUNT description file
	!	as well as the salesman's file
	!
	! Index:
	!
	! Option:
	!
	!
	! Inputs:
	!
	!	SALESMAN = Salesman's number
	!
	! Outputs:
	!
	!	SA_SALESMAN_EXAM = record for salesman
	!	SB_SUBACCOUNT_EXAM = record for subaccount
	!
	! Compile:
	!
	!	$ BAS SA_SOURCE:SA_EXAM_SALESMAN
	!	$ LIB FUNC_LIB:CMCFUN/REP SA_EXAM_SALESMAN
	!	$ DELETE SA_EXAM_SALESMAN.OBJ;*
	!
	! Author:
	!
	!	01/07/91 - Val James Allen
	!
	! Modification History:
	!
	!	05/28/91 - J. Shad Rydalch
	!		Made necessary changes to acomidate changes made in
	!		file layout and new overlay.
	!
	!	03/28/94 - Kevin Handy
	!		Reformatted.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/16/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/18/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	11/03/2000 - Kevin Handy
	!		Use A"x"B
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! CDD inclusions and memory MAPs
	!
	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP	(SB_SUBACCOUNT)		SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[SA.OPEN]SA_SALESMAN.HB"
	MAP	(SB_SUBACCOUNT)		SA_SALESMAN_CDD		SA_SALESMAN
	COM	(SA_SALESMAN_INI)	SA_SALESMAN_CDD		SA_SALESMAN_INI

	!
	! Common memory areas
	!
	COM (CH_SB_SUBACCOUNT_READ) SB_SUBACCOUNT.CH%

	!
	! Declare constants and/or variables
	!
	DECLARE	LONG	EXIT_STATUS

	%PAGE

	!
	! Assume failure
	!
	EXIT_STATUS = CMC$_UNDEFINED

100	IF SB_SUBACCOUNT.CH% <= 0%
	THEN
		SA_SALESMAN_INI::SUBJECT = &
			STRING$(LEN(SA_SALESMAN_INI::SUBJECT), A"?"B)
		SA_SALESMAN_INI::SALESMAN = &
			STRING$(LEN(SA_SALESMAN_INI::SALESMAN), A"?"B)
		SA_SALESMAN_INI::DESCR = &
			STRING$(LEN(SA_SALESMAN_INI::DESCR), A"?"B)
		SA_SALESMAN_INI::TTYPE = &
			STRING$(LEN(SA_SALESMAN_INI::TTYPE), A"?"B)
		SA_SALESMAN_INI::CLASS = &
			STRING$(LEN(SA_SALESMAN_INI::CLASS), A"?"B)
		SA_SALESMAN_INI::BDATE = &
			STRING$(LEN(SA_SALESMAN_INI::BDATE), A"?"B)
		SA_SALESMAN_INI::SSTATUS = &
			STRING$(LEN(SA_SALESMAN_INI::SSTATUS), A"?"B)
		SA_SALESMAN_INI::EDATE = &
			STRING$(LEN(SA_SALESMAN_INI::EDATE), A"?"B)
		SA_SALESMAN_INI::ADD1 = &
			STRING$(LEN(SA_SALESMAN_INI::ADD1), A"?"B)
		SA_SALESMAN_INI::ADD2 = &
			STRING$(LEN(SA_SALESMAN_INI::ADD2), A"?"B)
		SA_SALESMAN_INI::CITY = &
			STRING$(LEN(SA_SALESMAN_INI::CITY), A"?"B)
		SA_SALESMAN_INI::STATE = &
			STRING$(LEN(SA_SALESMAN_INI::STATE), A"?"B)
		SA_SALESMAN_INI::ZIP = &
			STRING$(LEN(SA_SALESMAN_INI::ZIP), A"?"B)
		SA_SALESMAN_INI::COUNTRY = &
			STRING$(LEN(SA_SALESMAN_INI::COUNTRY), A"?"B)
		SA_SALESMAN_INI::PHONE = &
			STRING$(LEN(SA_SALESMAN_INI::PHONE), A"?"B)
		SA_SALESMAN_INI::INITIALS = &
			STRING$(LEN(SA_SALESMAN_INI::INITIALS), A"?"B)
		SA_SALESMAN_INI::REGION = &
			STRING$(LEN(SA_SALESMAN_INI::REGION), A"?"B)
		SA_SALESMAN_INI::COMMPER = 0.0

		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
		USE
			SA_SALESMAN_EXAM = SA_SALESMAN_INI

			FILENAME$ = "SB_SUBACCOUNT"
			CONTINUE ExitFunction IF ERR = 5%
			CONTINUE HelpError
		END WHEN
	END IF

200	WHEN ERROR IN
		GET #SB_SUBACCOUNT.CH%, KEY #0% EQ ("S" + SALESMAN), REGARDLESS
	USE
		SA_SALESMAN_EXAM = SA_SALESMAN_INI

		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE ExitFunction IF (ERR = 155% OR ERR = 9%)
		CONTINUE HelpError
	END WHEN

	SA_SALESMAN_EXAM = SA_SALESMAN

	EXIT_STATUS = CMC$_NORMAL

 ExitFunction:
	SA_EXAM_SALESMAN = EXIT_STATUS
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

32767	END FUNCTION
