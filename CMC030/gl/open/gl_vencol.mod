	!======================================================================
	! GL_VENCOL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(GL_VENCOL.CH%, STAT%)
	CALL READ_DEVICE('GL_VENCOL',GL_VENCOL.DEV$, STAT%)

	GL_VENCOL.NAME$ = GL_VENCOL.DEV$+"GL_VENCOL.TBL"

	OPEN GL_VENCOL.NAME$ FOR INPUT AS FILE GL_VENCOL.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP GL_VENCOL, &
		PRIMARY KEY &
			GL_VENCOL::RECKEY, &
		ACCESS MODIFY, ALLOW MODIFY

