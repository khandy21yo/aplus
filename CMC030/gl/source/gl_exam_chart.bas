1	%TITLE "Examine Account Number"
	%SBTTL "GL_EXAM_CHART"
	%IDENT "V3.6a Calico"

	FUNCTION LONG GL_EXAM_CHART(STRING ACCOUNT, &
		GL_CHART_CDD GL_CHART_EXAM)

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
	!	.b
	!	.lm +5
	!	This function is used to check for undefined account numbers
	!	in the chart of accounts file.
	!
	! Index:
	!
	! Option:
	!
	!
	! Inputs:
	!
	!	ACCOUNT = Account number
	!
	! Outputs:
	!
	!	CMC$_NORMAL,CMC$_WARNING
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_EXAM_CHART
	!	$ LIB FUNC_LIB:CMCFUN/REP GL_EXAM_CHART
	!	$ DELETE GL_EXAM_CHART.OBJ;*
	!
	! Author:
	!
	!	01/23/89 - Frank Starman
	!
	! Modification History:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/17/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/01/97 - Kevin Handy
	!		Recormat source code.
	!		Use integer for #key
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/18/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD	GL_CHART
	COM	(GL_CHART_INI)	GL_CHART_CDD	GL_CHART_INI

	COM (CH_GL_CHART_READ) GL_CHART.CH%

	DECLARE LONG EXIT_STATUS

	%PAGE

	!
	! Assume undefined
	!
	EXIT_STATUS = CMC$_UNDEFINED

100	IF GL_CHART.CH% <= 0%
	THEN
		GL_CHART_INI::ACCT	= &
			STRING$(LEN(GL_CHART_INI::ACCT), A"?"B)
		GL_CHART_INI::DESCR	= &
			STRING$(LEN(GL_CHART_INI::DESCR), A"?"B)
		GL_CHART_INI::ACCTYPE	= &
			STRING$(LEN(GL_CHART_INI::ACCTYPE), A"?"B)
		GL_CHART_INI::FLOW	= &
			STRING$(LEN(GL_CHART_INI::FLOW), A"?"B)
		GL_CHART_INI::WORK	= &
			STRING$(LEN(GL_CHART_INI::WORK), A"?"B)
		GL_CHART_INI::FINTYPE	= &
			STRING$(LEN(GL_CHART_INI::FINTYPE), A"?"B)
		GL_CHART_INI::SUMMARY	= &
			STRING$(LEN(GL_CHART_INI::SUMMARY), A"?"B)
		FOR I% = 0% TO 20%
			GL_CHART_INI::DOLLAR(I%)	= 0.0
			GL_CHART_INI::UNIT(I%)		= 0.0
			GL_CHART_INI::HOUR(I%)		= 0.0
		NEXT I%
		GL_CHART_INI::CPERIOD	= 0%
		GL_CHART_INI::RUNDOL	= 0.0
		GL_CHART_INI::RUNUNIT	= 0.0
		GL_CHART_INI::RUNHOUR	= 0.0
		GL_CHART_INI::CURDOL	= 0.0
		GL_CHART_INI::CURUNIT	= 0.0
		GL_CHART_INI::CURHOUR	= 0.0
		GL_CHART_INI::BATCH	= &
			STRING$(LEN(GL_CHART_INI::BATCH), A"?"B)

		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
		USE
			GL_CHART_EXAM = GL_CHART_INI

			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "GL_CHART"
			CONTINUE HelpError
		END WHEN
	END IF

200	WHEN ERROR IN
		GET #GL_CHART.CH%, KEY #0% EQ ACCOUNT,REGARDLESS
	USE
		GL_CHART_EXAM = GL_CHART_INI

		CONTINUE ExitFunction IF ERR = 155%
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	GL_CHART_EXAM = GL_CHART
	EXIT_STATUS = CMC$_NORMAL

 ExitFunction:
	GL_EXAM_CHART = EXIT_STATUS

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
