1	%TITLE "Create Include File to Declare Constant"
	%SBTTL "TK_WRIT_CONSTANT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG TK_WRIT_CONSTANT
	!
	!		COPYRIGHT (C) 1986 BY
	!		Computer Management Center, Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be used and
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies therof may not be provided or otherwise made
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
	!	This function creates include file to declare constant
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_WRIT_CONSTANT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP TK_WRIT_CONSTANT
	!	$ DELETE TK_WRIT_CONSTANT.OBJ;*
	!
	! Author:
	!
	!	01/21/89 - Frank Starman
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/19/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/06/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[TK.OPEN]TK_CONSTANT.HB"
	MAP (TK_CONSTANT)	TK_CONSTANT_CDD	TK_CONSTANT

	DECLARE LONG EXIT_STATUS

	%PAGE

	ON ERROR GOTO 19000

	!
	! Assume success
	!
	EXIT_STATUS = 1%

300	!
	! Open constant file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[TK.OPEN]TK_CONSTANT.OPN"
	USE
		CONTINUE ExitFunction IF ERR = 5%
		FILENAME$ = "TK_CONSTANT"
		CONTINUE HelpError
	END WHEN

310	CALL ASSG_CHANNEL(TEMP_CONSTANT.CH%, STAT%)
	WHEN ERROR IN
		OPEN "FUNC_INCLUDE:CONSTANTS.INC" FOR OUTPUT AS FILE &
			TEMP_CONSTANT.CH%, &
			RECORDSIZE 132%
	USE
		FILENAME$ = "CONSTANTS.INC"
		CONTINUE HelpError
	END WHEN

1000	CALL ENTR_3MESSAGE(SCOPE, "Creating include file... ", 1% + 16%)

	RESET #TK_CONSTANT.CH%, KEY #1%
	PRINT #TEMP_CONSTANT.CH%, "9"C + "%NOLIST"
	PRINT #TEMP_CONSTANT.CH%, "9"C + "%NOCROSS"
	PRINT #TEMP_CONSTANT.CH%,

1010	WHEN ERROR IN
		GET #TK_CONSTANT.CH%, REGARDLESS
	USE
		CONTINUE 1020 IF ERR = 11%
		FILENAME$ = "TK_WRIT_CONSTANT"
		CONTINUE HelpError
	END WHEN

	IF TK_CONSTANT::CLASS <> LAST.CLASS$
	THEN
		PRINT #TEMP_CONSTANT.CH%, "9"C + "!" + STRING$(75%, A"="B)
	END IF

	PRINT #TEMP_CONSTANT.CH%, "9"C + "DECLARE LONG CONSTANT " + &
		LEFT(TK_CONSTANT::CONSTNAME + SPACE$(39%), 39%) + " = " + &
		FORMAT$(TK_CONSTANT::CONST, "##########")

	LAST.CLASS$ = TK_CONSTANT::CLASS
	GOTO 1010

1020	PRINT #TEMP_CONSTANT.CH%,
	PRINT #TEMP_CONSTANT.CH%, "9"C + "%LIST"
	PRINT #TEMP_CONSTANT.CH%, "9"C + "%CROSS"

	CLOSE #TK_CONSTANT.CH%
	CLOSE #TEMP_CONSTANT.CH%

1030	WHEN ERROR IN
		KILL "FUNC_INCLUDE:CONSTANTS.INC;-1" FOR I% = 1% TO 10%
	USE
		CONTINUE ExitFunction
	END WHEN

 ExitFunction:

	TK_WRIT_CONSTANT = EXIT_STATUS

	EXIT FUNCTION

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	EXIT_STATUS = 4%
	GOTO ExitFunction

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	GOTO HelpError

32767	END FUNCTION
