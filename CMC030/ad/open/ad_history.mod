	!======================================================================
	! AD_HISTORY file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AD_HISTORY.CH%, STAT%)
	CALL READ_DEVICE('AD_HISTORY',AD_HISTORY.DEV$, STAT%)

	AD_HISTORY.NAME$ = AD_HISTORY.DEV$+"AD_HISTORY.HIS"

	OPEN AD_HISTORY.NAME$ FOR INPUT AS FILE AD_HISTORY.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AD_HISTORY, &
		PRIMARY KEY &
		( &
			AD_HISTORY::ASSET_NUM, &
			AD_HISTORY::DEP_OBJECT, &
			AD_HISTORY::PERIOD &
		)	, &
		ALTERNATE KEY &
		( &
			AD_HISTORY::PERIOD, &
			AD_HISTORY::DEP_OBJECT, &
			AD_HISTORY::ASSET_NUM &
		)	CHANGES, &
		ALTERNATE KEY &
		( &
			AD_HISTORY::DEP_OBJECT, &
			AD_HISTORY::PERIOD, &
			AD_HISTORY::ASSET_NUM &
		)	CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

