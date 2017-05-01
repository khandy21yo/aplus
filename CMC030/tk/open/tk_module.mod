	!======================================================================
	! TK_MODULE file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(TK_MODULE.CH%, STAT%)
	CALL READ_DEVICE('TK_MODULE',TK_MODULE.DEV$, STAT%)

	TK_MODULE.NAME$ = "REF:TK_MODULE.MAS"

	OPEN TK_MODULE.NAME$ FOR INPUT AS FILE TK_MODULE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP TK_MODULE, &
		PRIMARY KEY &
			TK_MODULE::MODNAME, &
		ALTERNATE KEY &
		( &
			TK_MODULE::CATEGORY, &
			TK_MODULE::MODNAME &
		)	CHANGES, &
		ALTERNATE KEY &
		( &
			TK_MODULE::MODTYPE, &
			TK_MODULE::MODNAME &
		)	CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

