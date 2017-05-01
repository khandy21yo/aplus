1	%TITLE "Force Period into Chart"

	!
	!++
	!
	! Author:
	!	11/05/91 - Kevin Handy
	!
	! Modification History:
	!
	!	12/01/92 - Kevin Handy
	!		Modified so it doesn't require sharabe library.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART) GL_CHART_CDD GL_CHART

50	INPUT "What period to force"; NPERIOD%

100 !	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.MOD"
	!======================================================================
	! GL_CHART file (open read/write)
	!======================================================================

	GL_CHART.CH% = 10%
	GL_CHART.DEV$ = ""

	GL_CHART.NAME$ = GL_CHART.DEV$ + "GL_CHART.MAS"

	OPEN GL_CHART.NAME$ FOR INPUT AS FILE GL_CHART.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP GL_CHART, &
		PRIMARY KEY &
			GL_CHART::ACCT, &
		ALTERNATE KEY &
			GL_CHART::FLOW &
			DUPLICATES CHANGES, &
		ALTERNATE KEY &
			GL_CHART::WORK &
			DUPLICATES CHANGES, &
		ALTERNATE KEY &
			GL_CHART::FINTYPE &
			DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY


110	RESET #GL_CHART.CH%

200	GET #GL_CHART.CH%

	IF (GL_CHART::CPERIOD <> NPERIOD%)
	THEN
		PRINT GL_CHART::ACCT
		GL_CHART::CPERIOD = NPERIOD%
		UPDATE #GL_CHART.CH%
	END IF

	GOTO 200

32767	END
