	!======================================================================
	! OS_BOM_LINE file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(OS_BOM_LINE.CH%, STAT%)
	CALL READ_DEVICE('OS_BOM_LINE',OS_BOM_LINE.DEV$, STAT%)

	OS_BOM_LINE.NAME$ = OS_BOM_LINE.DEV$+"OS_BOM_LINE.MAS"

	OPEN OS_BOM_LINE.NAME$ FOR INPUT AS FILE OS_BOM_LINE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP OS_BOM_LINE, &
		PRIMARY KEY &
		( &
			OS_BOM_LINE::PRODUCT, &
			OS_BOM_LINE::CATEGORY &
		)	DUPLICATES , &
		ACCESS MODIFY, ALLOW MODIFY

