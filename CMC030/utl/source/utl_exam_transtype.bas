1	%TITLE "Examination Transaction Type"
	%SBTTL "UTL_EXAM_TRANSTYPE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG UTL_EXAM_TRANSTYPE(STRING TRANSTYPE, &
		UTL_TRANSTYPE_CDD UTL_TRANSTYPE_EXAM)

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
	!	This function is used to check undefined transaction type
	!	in the transaction description file
	!
	! Inputs
	!
	!	XTRANSTYPE$ - Transaction type
	!
	! Outputs
	!
	!	UTL_EXAM_TRANSTYPE = 0%
	!	UTL_EXAM_TRANSTYPE = 1%
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_EXAM_TRANSTYPE
	!	$ LIB FUNC_LIB:CMCFUN/REP UTL_EXAM_TRANSTYPE
	!	$ DELETE UTL_EXAM_TRANSTYPE.OBJ;*
	!
	! AUTHOR:
	!
	!	05/05/88 - Frank Starman
	!
	! MODIFICATION HISTORY:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/30/96 - Kevin Handy
	!		Reformat source code
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
	!
	!	01/09/2000 - Kevin Handy
	!		Lose need for UTL_TRANSTYPE_INI by just setting
	!		the proper fields in UTL_TRANSTYPE_EXAM when
	!		the search fails.
	!
	!	01/09/2000 - Kevin Handy
	!		Copy over the code from IC_READ_35BALANCE to get
	!		the transaction type records, so that much less disk
	!		access should result (with some speed increase)
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSTYPE.HB"
	MAP (UTL_TRANSTYPE)	UTL_TRANSTYPE_CDD	UTL_TRANSTYPE

	COM (CH_UTL_TRANSTYPE_READ) UTL_TRANSTYPE.CH%

	MAP (XXX_UTL_TRANSTYPE) &
		LONG UTL_TRANSTYPE_COUNT, &
		UTL_TRANSTYPE_CDD UTL_TRANSTYPE_ARRAY(20%)

	DECLARE LONG EXIT_STATUS

	%PAGE

	!
	! Assume undefined
	!
	EXIT_STATUS = CMC$_UNDEFINED

100	IF UTL_TRANSTYPE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSTYPE.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "UTL_TRANSTYPE"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! LOAD IN THE TRABSTYPE_ARRAY (once only)
	!
	IF UTL_TRANSTYPE_COUNT = 0%
	THEN
		RESET #UTL_TRANSTYPE.CH%

		WHILE (UTL_TRANSTYPE_COUNT < 20%)
			WHEN ERROR IN
				GET #UTL_TRANSTYPE.CH%
				UTL_TRANSTYPE_ARRAY(UTL_TRANSTYPE_COUNT) = &
					UTL_TRANSTYPE
				UTL_TRANSTYPE_COUNT = UTL_TRANSTYPE_COUNT + 1%
			USE
				CONTINUE 200
			END WHEN
		NEXT
	END IF

200	!
	! Try this code to lookup the table to see if it runs faster
	!
	!	(There are only 18 items in the file, so a simple
	!	loop should be about as fast a a more complex binary
	!	lookup).
	!
	FOR I% = 0% TO UTL_TRANSTYPE_COUNT - 1%

		IF UTL_TRANSTYPE_ARRAY(I%)::CODE = TRANSTYPE
		THEN
			UTL_TRANSTYPE_EXAM = UTL_TRANSTYPE_ARRAY(I%)
			EXIT_STATUS = CMC$_NORMAL
			GOTO ExitFunction
		END IF

		IF UTL_TRANSTYPE_ARRAY(I%)::CODE > TRANSTYPE
		THEN
			! EXIT_STATUS should already be set to error
			GOTO NotFound
		END IF
	NEXT I%

 NotFound:
	UTL_TRANSTYPE_EXAM::CODE = &
		STRING$(LEN(UTL_TRANSTYPE_EXAM::CODE), A"?"B)
	UTL_TRANSTYPE_EXAM::DESCRIPTION = &
		STRING$(LEN(UTL_TRANSTYPE_EXAM::DESCRIPTION), A"?"B)
	UTL_TRANSTYPE_EXAM::CLASS = &
		STRING$(LEN(UTL_TRANSTYPE_EXAM::CLASS), A"?"B)
	UTL_TRANSTYPE_EXAM::TRANSSIGN = &
		STRING$(LEN(UTL_TRANSTYPE_EXAM::TRANSSIGN), A"?"B)

 ExitFunction:
	UTL_EXAM_TRANSTYPE = EXIT_STATUS

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
