	!======================================================================
	! GL_CHART file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(GL_CHART.CH%, STAT%)
	CALL READ_DEVICE('GL_CHART',GL_CHART.DEV$, STAT%)

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

