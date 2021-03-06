	!======================================================================
	! TK_RELATION file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(TK_RELATION.CH%, STAT%)
	CALL READ_DEVICE('TK_RELATION',TK_RELATION.DEV$, STAT%)

	TK_RELATION.NAME$ = "REF:TK_RELATION.MAS"

	OPEN TK_RELATION.NAME$ FOR INPUT AS FILE TK_RELATION.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP TK_RELATION, &
		PRIMARY KEY &
		( &
			TK_RELATION::PARENT, &
			TK_RELATION::CHILD &
		)	, &
		ALTERNATE KEY &
		( &
			TK_RELATION::CHILD, &
			TK_RELATION::PARENT &
		)	CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

