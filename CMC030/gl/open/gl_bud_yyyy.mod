	!======================================================================
	! GL_BUD_YYYY file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(GL_BUD_YYYY.CH%, STAT%)
	CALL READ_DEVICE('GL_BUD_YYYY',GL_BUD_YYYY.DEV$, STAT%)

	GL_BUD_YYYY.NAME$ = GL_BUD_YYYY.DEV$+"GL_BUD_"+GL_BUDGET.YEAR$+".MAS"

	OPEN GL_BUD_YYYY.NAME$ FOR INPUT AS FILE GL_BUD_YYYY.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP GL_BUD_YYYY, &
		PRIMARY KEY &
			GL_BUD_YYYY::ACCT, &
		ACCESS MODIFY, ALLOW MODIFY

