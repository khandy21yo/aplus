	!======================================================================
	! GL_35HISTORY file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(GL_35HISTORY.CH%, STAT%)
	CALL READ_DEVICE('GL_35HISTORY',GL_35HISTORY.DEV$, STAT%)

	GL_35HISTORY.NAME$ = GL_35HISTORY.DEV$+"GL_35HISTORY.LED"

	OPEN GL_35HISTORY.NAME$ FOR INPUT AS FILE GL_35HISTORY.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP GL_35HISTORY, &
		PRIMARY KEY &
		( &
			GL_35HISTORY::ACCOUNT, &
			GL_35HISTORY::PERIOD &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

