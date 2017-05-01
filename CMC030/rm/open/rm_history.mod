	!======================================================================
	! RM_HISTORY file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(RM_HISTORY.CH%, STAT%)
	CALL READ_DEVICE('RM_HISTORY',RM_HISTORY.DEV$, STAT%)

	RM_HISTORY.NAME$ = RM_HISTORY.DEV$+"RM_HISTORY.HIS"

	OPEN RM_HISTORY.NAME$ FOR INPUT AS FILE RM_HISTORY.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP RM_HISTORY, &
		PRIMARY KEY &
		( &
			RM_HISTORY::CATEGORY, &
			RM_HISTORY::LOCATION, &
			RM_HISTORY::ACTION_DATE &
		)	, &
		ALTERNATE KEY &
		( &
			RM_HISTORY::LOCATION, &
			RM_HISTORY::ACTION_DATE &
		)	DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

