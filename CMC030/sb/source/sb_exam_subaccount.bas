1	%TITLE "Examination of Subaccount Number"
	%SBTTL "SB_EXAM_SUBACCOUNT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG SB_EXAM_SUBACCOUNT(STRING SUBJECT, &
		STRING SUBACCOUNT, &
		SB_SUBACCOUNT_CDD SB_SUBACCOUNT_EXAM)

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
	!
	! Abstract:HELP
	!	.p
	!	This function is used to check undefine SUBACCOUNT number
	!	in SUBACCOUNT description file
	!
	! Index:
	!
	! Option:
	!
	!
	! Inputs:
	!
	!	SUBJECT = Subject of the subaccount
	!	SUBACCOUNT = SUBACCOUNT number
	!
	! Outputs:
	!
	! Compile:
	!
	!	$ BAS SB_SOURCE:SB_EXAM_SUBACCOUNT
	!	$ LIB FUNC_LIB:CMCFUN/REP SB_EXAM_SUBACCOUNT
	!	$ DELETE SB_EXAM_SUBACCOUNT.OBJ;*
	!
	! Author:
	!
	!	03/20/89 - Frank Starman
	!
	! Modification History:
	!
	!	10/07/92 - Kevin Handy
	!		Modified to set channel negitive if doesn't
	!		actually open the file.  This allows a check
	!		for file exists, and disables constantly trying
	!		to open file when it doesnt exist.
	!
	!	04/20/93 - Frank F. Starman
	!		Check the length of SUBACCOUNT before trying
	!		to find a record.
	!
	!	05/28/93 - Kevin Handy
	!		Changed the map CH_SUBACCOUNT_READ back to
	!		CH_SB_SUBACCOUNT so that the many programs that
	!		expect it that way will once again work.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	11/10/95 - Kevin Handy
	!		Reformat closer to 80 columns.
	!
	!	11/10/95 - Kevin Handy
	!		Move default fill of _EXAM into error trap, to
	!		reduce CPU used by copying.
	!
	!	12/12/95 - Kevin Handy
	!		Loose goofy tabs in source code formatting.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excess %PAGE
	!
	!	07/18/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	11/05/2000 - Kevin Handy
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
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT
	COM (SB_SUBACCOUNT_INI) SB_SUBACCOUNT_CDD	SB_SUBACCOUNT_INI

	!
	! Common memory areas
	!
	COM (CH_SB_SUBACCOUNT) SB_SUBACCOUNT.CH%

	!
	! Declare constants and/or variables
	!
	DECLARE	LONG	EXIT_STATUS

	%PAGE

	!
	! Assume failure
	!
	EXIT_STATUS = CMC$_UNDEFINED
	SUBACCOUNT = LEFT(SUBACCOUNT, LEN(SB_SUBACCOUNT::SUBACCOUNT))

100	GOTO ExitFunction IF SB_SUBACCOUNT.CH% < 0%

	IF SB_SUBACCOUNT.CH% = 0%
	THEN
		SB_SUBACCOUNT_INI::SUBJECT = &
			STRING$(LEN(SB_SUBACCOUNT_INI::SUBJECT), A"?"B)
		SB_SUBACCOUNT_INI::SUBACCOUNT = &
			STRING$(LEN(SB_SUBACCOUNT_INI::SUBACCOUNT), A"?"B)
		SB_SUBACCOUNT_INI::DESCR = &
			STRING$(LEN(SB_SUBACCOUNT_INI::DESCR), A"?"B)
		SB_SUBACCOUNT_INI::TTYPE = &
			STRING$(LEN(SB_SUBACCOUNT_INI::TTYPE), A"?"B)
		SB_SUBACCOUNT_INI::CLASS = &
			STRING$(LEN(SB_SUBACCOUNT_INI::CLASS), A"?"B)
		SB_SUBACCOUNT_INI::BDATE = &
			STRING$(LEN(SB_SUBACCOUNT_INI::BDATE), A"?"B)
		SB_SUBACCOUNT_INI::SSTATUS = &
			STRING$(LEN(SB_SUBACCOUNT_INI::SSTATUS), A"?"B)
		SB_SUBACCOUNT_INI::EDATE = &
			STRING$(LEN(SB_SUBACCOUNT_INI::EDATE), A"?"B)

		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
		USE
			SB_SUBACCOUNT.CH% = -SB_SUBACCOUNT.CH%
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "SB_SUBACCOUNT"
			CONTINUE HelpError
		END WHEN
	END IF

200	WHEN ERROR IN
		GET #SB_SUBACCOUNT.CH%, &
			KEY #0% EQ (SUBJECT + SUBACCOUNT), &
			REGARDLESS
	USE
		IF (ERR = 155% OR ERR = 9%)
		THEN
			SB_SUBACCOUNT_EXAM = SB_SUBACCOUNT_INI
			CONTINUE ExitFunction
		END IF
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

	SB_SUBACCOUNT_EXAM = SB_SUBACCOUNT
	EXIT_STATUS = CMC$_NORMAL

 ExitFunction:
	SB_EXAM_SUBACCOUNT = EXIT_STATUS
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
