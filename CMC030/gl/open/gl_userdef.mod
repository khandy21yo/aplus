	!======================================================================
	! GL_USERDEF file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(GL_USERDEF.CH%, STAT%)
	CALL READ_DEVICE('GL_USERDEF',GL_USERDEF.DEV$, STAT%)

	GL_USERDEF.NAME$ = GL_USERDEF.DEV$+"GL_USERDEF.MAS"

	OPEN GL_USERDEF.NAME$ FOR INPUT AS FILE GL_USERDEF.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP GL_USERDEF, &
		PRIMARY KEY &
		( &
			GL_USERDEF::JCODE, &
			GL_USERDEF::JLINE &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

