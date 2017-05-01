	!======================================================================
	! GL_MASK file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(GL_MASK.CH%, STAT%)
	CALL READ_DEVICE('GL_MASK',GL_MASK.DEV$, STAT%)

	GL_MASK.NAME$ = GL_MASK.DEV$+"GL_MASK.TBL"

	OPEN GL_MASK.NAME$ FOR INPUT AS FILE GL_MASK.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP GL_MASK, &
		PRIMARY KEY &
			GL_MASK::ACCT_MASK, &
		ACCESS MODIFY, ALLOW MODIFY

