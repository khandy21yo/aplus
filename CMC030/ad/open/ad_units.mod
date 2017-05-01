	!======================================================================
	! AD_UNITS file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AD_UNITS.CH%, STAT%)
	CALL READ_DEVICE('AD_UNITS',AD_UNITS.DEV$, STAT%)

	AD_UNITS.NAME$ = AD_UNITS.DEV$+"AD_UNITS_"+BATCH_NO$+".JRL"

	OPEN AD_UNITS.NAME$ FOR INPUT AS FILE AD_UNITS.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AD_UNITS, &
		PRIMARY KEY &
		( &
			AD_UNITS::DEP_OBJECT, &
			AD_UNITS::ACTION_DATE &
		)	DUPLICATES , &
		ACCESS MODIFY, ALLOW MODIFY

