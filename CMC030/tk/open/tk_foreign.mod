	!======================================================================
	! TK_FOREIGN file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(TK_FOREIGN.CH%, STAT%)
	CALL READ_DEVICE('TK_FOREIGN',TK_FOREIGN.DEV$, STAT%)

	TK_FOREIGN.NAME$ = TK_FOREIGN.DEV$+"TK_FOREIGN.MAS"

	OPEN TK_FOREIGN.NAME$ FOR INPUT AS FILE TK_FOREIGN.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP TK_FOREIGN, &
		PRIMARY KEY &
		( &
			TK_FOREIGN::STRUCT, &
			TK_FOREIGN::SUBSTRUCT, &
			TK_FOREIGN::FLDNAMES &
		)	, &
		ALTERNATE KEY &
		( &
			TK_FOREIGN::SUBSTRUCT, &
			TK_FOREIGN::STRUCT &
		)	CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

