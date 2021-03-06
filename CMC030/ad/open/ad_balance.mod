	!======================================================================
	! AD_BALANCE file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AD_BALANCE.CH%, STAT%)
	CALL READ_DEVICE('AD_BALANCE',AD_BALANCE.DEV$, STAT%)

	AD_BALANCE.NAME$ = AD_BALANCE.DEV$+"AD_BALANCE.MAS"

	OPEN AD_BALANCE.NAME$ FOR INPUT AS FILE AD_BALANCE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AD_BALANCE, &
		EXTENDSIZE 27%, &
		PRIMARY KEY &
		( &
			AD_BALANCE::ASSET_NUM, &
			AD_BALANCE::DEP_OBJECT &
		)	, &
		ALTERNATE KEY &
		( &
			AD_BALANCE::DEP_OBJECT, &
			AD_BALANCE::ASSET_NUM &
		)	CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

