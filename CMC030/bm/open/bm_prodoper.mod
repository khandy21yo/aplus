	!======================================================================
	! BM_PRODOPER file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(BM_PRODOPER.CH%, STAT%)
	CALL READ_DEVICE('BM_PRODOPER',BM_PRODOPER.DEV$, STAT%)

	BM_PRODOPER.NAME$ = BM_PRODOPER.DEV$+"BM_PRODOPER.MAS"

	OPEN BM_PRODOPER.NAME$ FOR INPUT AS FILE BM_PRODOPER.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP BM_PRODOPER, &
		PRIMARY KEY &
		( &
			BM_PRODOPER::PRODUCT, &
			BM_PRODOPER::ITEMNUM &
		)	, &
		ALTERNATE KEY &
		( &
			BM_PRODOPER::PRODUCT, &
			BM_PRODOPER::OPERATION, &
			BM_PRODOPER::EFFDATE &
		)	CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

