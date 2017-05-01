	!======================================================================
	! GL_USERHEAD file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(GL_USERHEAD.CH%, STAT%)
	CALL READ_DEVICE('GL_USERHEAD',GL_USERHEAD.DEV$, STAT%)

	GL_USERHEAD.NAME$ = GL_USERHEAD.DEV$+"GL_USERHEAD_"+BATCH_NO$+".JRL"

	OPEN GL_USERHEAD.NAME$ FOR INPUT AS FILE GL_USERHEAD.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP GL_USERHEAD, &
		PRIMARY KEY &
			GL_USERHEAD::JCODE, &
		ACCESS MODIFY, ALLOW MODIFY

