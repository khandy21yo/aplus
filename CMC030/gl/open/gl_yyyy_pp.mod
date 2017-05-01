	!======================================================================
	! GL_YYYY_PP file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(GL_YYYY_PP.CH%, STAT%)
	CALL READ_DEVICE('GL_YYYY_PP',GL_YYYY_PP.DEV$, STAT%)

	GL_YYYY_PP.NAME$ = GL_YYYY_PP.DEV$+"GL_"+YYYY_PP$+".LED"

	OPEN GL_YYYY_PP.NAME$ FOR INPUT AS FILE GL_YYYY_PP.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP GL_YYYY_PP, &
		PRIMARY KEY &
		( &
			GL_YYYY_PP::ACCT, &
			GL_YYYY_PP::TRANDAT &
		)	DUPLICATES , &
		ALTERNATE KEY &
		( &
			GL_YYYY_PP::SUBACC, &
			GL_YYYY_PP::OPERATION, &
			GL_YYYY_PP::ACCT &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
		( &
			GL_YYYY_PP::XREFNO, &
			GL_YYYY_PP::ACCT &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
		( &
			GL_YYYY_PP::CKNO, &
			GL_YYYY_PP::ACCT &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
			GL_YYYY_PP::BTHNUM &
			DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

