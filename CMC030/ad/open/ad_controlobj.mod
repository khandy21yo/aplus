	!======================================================================
	! AD_CONTROLOBJ file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AD_CONTROLOBJ.CH%, STAT%)
	CALL READ_DEVICE('AD_CONTROLOBJ',AD_CONTROLOBJ.DEV$, STAT%)

	AD_CONTROLOBJ.NAME$ = AD_CONTROLOBJ.DEV$+"AD_CONTROLOBJ.CTR"

	OPEN AD_CONTROLOBJ.NAME$ FOR INPUT AS FILE AD_CONTROLOBJ.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AD_CONTROLOBJ, &
		PRIMARY KEY &
			AD_CONTROLOBJ::DEP_OBJECT, &
		ACCESS MODIFY, ALLOW MODIFY

