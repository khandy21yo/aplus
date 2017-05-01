	!======================================================================
	! GL_USERJOUR file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(GL_USERJOUR.CH%, STAT%)
	CALL READ_DEVICE('GL_USERJOUR',GL_USERJOUR.DEV$, STAT%)

	GL_USERJOUR.NAME$ = GL_USERJOUR.DEV$+"GL_USERJOUR_"+BATCH_NO$+".JRL"

	OPEN GL_USERJOUR.NAME$ FOR INPUT AS FILE GL_USERJOUR.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP GL_USERJOUR, &
		PRIMARY KEY &
		( &
			GL_USERJOUR::JCODE, &
			GL_USERJOUR::JLINE &
		)	DUPLICATES , &
		ACCESS MODIFY, ALLOW MODIFY

