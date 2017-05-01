	!======================================================================
	! AD_VERYOLD file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AD_VERYOLD.CH%, STAT%)
	CALL READ_DEVICE('AD_VERYOLD',AD_VERYOLD.DEV$, STAT%)

	AD_VERYOLD.NAME$ = AD_VERYOLD.DEV$+"AD_VERYOLD_"+BATCH_NO$+".JRL"

	OPEN AD_VERYOLD.NAME$ FOR INPUT AS FILE AD_VERYOLD.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AD_VERYOLD, &
		PRIMARY KEY &
			AD_VERYOLD::ASSET, &
		ACCESS MODIFY, ALLOW MODIFY

