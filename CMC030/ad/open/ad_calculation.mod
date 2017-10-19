	!======================================================================
	! AD_CALCULATION file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AD_CALCULATION.CH%, STAT%)
	CALL READ_DEVICE('AD_CALCULATION',AD_CALCULATION.DEV$, STAT%)

	AD_CALCULATION.NAME$ = AD_CALCULATION.DEV$+"AD_CALCULATION.LED"

	OPEN AD_CALCULATION.NAME$ FOR INPUT AS FILE AD_CALCULATION.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AD_CALCULATION, &
		PRIMARY KEY &
		( &
			AD_CALCULATION::ASSET_NUM, &
			AD_CALCULATION::DEP_OBJECT &
		)	, &
		ALTERNATE KEY &
		( &
			AD_CALCULATION::DEP_OBJECT, &
			AD_CALCULATION::ASSET_NUM &
		)	CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY
