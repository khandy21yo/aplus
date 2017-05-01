	!======================================================================
	! RI_RELATION file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(RI_RELATION.CH%, STAT%)
	CALL READ_DEVICE('RI_RELATION',RI_RELATION.DEV$, STAT%)

	RI_RELATION.NAME$ = RI_RELATION.DEV$+"RI_RELATION.MAS"

	OPEN RI_RELATION.NAME$ FOR INPUT AS FILE RI_RELATION.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP RI_RELATION, &
		PRIMARY KEY &
		( &
			RI_RELATION::PRODUCT, &
			RI_RELATION::ITEMNUM &
		)	, &
		ALTERNATE KEY &
		( &
			RI_RELATION::INGREDIENT, &
			RI_RELATION::PRODUCT &
		)	DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

