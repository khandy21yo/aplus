	!======================================================================
	! GL_TRICONTROL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(GL_TRICONTROL.CH%, STAT%)
	CALL READ_DEVICE('GL_TRICONTROL',GL_TRICONTROL.DEV$, STAT%)

	GL_TRICONTROL.NAME$ = GL_TRICONTROL.DEV$+"GL_TRICONTROL.MAS"

	OPEN GL_TRICONTROL.NAME$ FOR INPUT AS FILE GL_TRICONTROL.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP GL_TRICONTROL, &
		PRIMARY KEY &
			GL_TRICONTROL::LOCATION, &
		ACCESS MODIFY, ALLOW MODIFY

