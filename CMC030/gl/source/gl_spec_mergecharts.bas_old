1	!
	! Merge information from two chart files.
	! The most important being .mas_preconvert,
	! comverting into .mas.
	!
	option size = (integer long, real gfloat)

	%include "source:[gl.open]gl_chart,hb"

	map (gl_chart) gl_chart_cdd gl_chart
	map (gl_chart_pre) gl_chart_cdd gl_chart_pre

	ON ERROR GOTO 19000

100	!======================================================================
	! GL_CHART file (open read/write)
	!======================================================================

	GL_CHART.CH% = 10%
	GL_CHART.DEV$ = ""

	GL_CHART.NAME$ = GL_CHART.DEV$+"GL_CHART.MAS"

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


200	!======================================================================
	! GL_CHART file (open read/write)
	!======================================================================

	GL_CHART_pre.CH% = 12%

	GL_CHART_PRE.NAME$ = GL_CHART.DEV$+"GL_CHART.MAS_PRECONVERT"

	OPEN GL_CHART_PRE.NAME$ FOR INPUT AS FILE GL_CHART_PRE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP GL_CHART_PRE, &
		PRIMARY KEY &
			GL_CHART_PRE::ACCT, &
		ALTERNATE KEY &
			GL_CHART_PRE::FLOW &
			DUPLICATES CHANGES, &
		ALTERNATE KEY &
			GL_CHART_PRE::WORK &
			DUPLICATES CHANGES, &
		ALTERNATE KEY &
			GL_CHART_PRE::FINTYPE &
			DUPLICATES CHANGES, &
		ACCESS READ, ALLOW MODIFY


1000	RESET #GL_CHART_PRE.CH%

1100	GET #GL_CHART_PRE.CH%, REGARDLESS

1200	GET #GL_CHART.CH%, KEY #0% EQ GL_CHART_PRE::ACCT

1300	GL_CHART::DESCR		= GL_CHART_PRE::DESCR
	GL_CHART::ACCTYPE	= GL_CHART_PRE::ACCTYPE
	GL_CHART::FLOW		= GL_CHART_PRE::FLOW
	GL_CHART::WORK		= GL_CHART_PRE::WORK
	GL_CHART::FINTYPE	= GL_CHART_PRE::FINTYPE

	UPDATE #GL_CHART.CH%

	PRINT GL_CHART::ACCT; " Updated"

	GOTO 1100

1400	GL_CHART = GL_CHART_PRE

	FOR LOOP% = 0% TO 20%
		GL_CHART::DOLLAR(I%) = 0.0
		GL_CHART::UNIT(I%) = 0.0
		GL_CHART::HOUR(I%) = 0.0
	NEXT LOOP%

	GL_CHART::CPERIOD = 0%
	GL_CHART::RUNDOL = 0.0
	GL_CHART::RUNUNIT = 0.0
	GL_CHART::RUNHOUR = 0.0
	GL_CHART::CURDOL = 0.0
	GL_CHART::CURUNIT = 0.0
	GL_CHART::CURHOUR = 0.0

	PUT #GL_CHART.CH%

	PRINT GL_CHART::ACCT; " Added"

	GOTO 1100

2000	CLOSE GL_CHART.CH%, GL_CHART_PRE.CH%

	GOTO 32767

19000	SELECT ERL

		CASE 1100%
			RESUME 2000 IF ERR = 11%

		CASE 1200%
			RESUME 1400 IF ERR = 155%

	END SELECT

	ON ERROR GOTO 0

32767	END
