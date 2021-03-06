	!======================================================================
	! BM_RELATION file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(BM_RELATION.CH%, STAT%)
	CALL READ_DEVICE('BM_RELATION',BM_RELATION.DEV$, STAT%)

	BM_RELATION.NAME$ = BM_RELATION.DEV$+"BM_RELATION.MAS"

	OPEN BM_RELATION.NAME$ FOR INPUT AS FILE BM_RELATION.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP BM_RELATION, &
		PRIMARY KEY &
		( &
			BM_RELATION::PRODUCT, &
			BM_RELATION::ITEMNUM &
		)	, &
		ALTERNATE KEY &
		( &
			BM_RELATION::COMPONENT, &
			BM_RELATION::PRODUCT &
		)	DUPLICATES , &
		ACCESS MODIFY, ALLOW MODIFY

