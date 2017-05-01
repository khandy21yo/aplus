	!======================================================================
	! GL_TRIJOUR file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(GL_TRIJOUR.CH%, STAT%)
	CALL READ_DEVICE('GL_TRIJOUR',GL_TRIJOUR.DEV$, STAT%)

	GL_TRIJOUR.NAME$ = GL_TRIJOUR.DEV$+"GL_TRIJOUR_"+BATCH_NO$+".JRL"

	OPEN GL_TRIJOUR.NAME$ FOR INPUT AS FILE GL_TRIJOUR.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP GL_TRIJOUR, &
		PRIMARY KEY &
		( &
			GL_TRIJOUR::LOCATION, &
			GL_TRIJOUR::TRANDATE &
		)	, &
		ALTERNATE KEY &
		( &
			GL_TRIJOUR::TRANDATE, &
			GL_TRIJOUR::LOCATION &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

