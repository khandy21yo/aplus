	!======================================================================
	! IC_JOURADJUST file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(IC_JOURADJUST.CH%, STAT%)
	CALL READ_DEVICE('IC_JOURADJUST',IC_JOURADJUST.DEV$, STAT%)

	IC_JOURADJUST.NAME$ = IC_JOURADJUST.DEV$+"IC_JOURADJUST_"+BATCH_NO$+".JRL"

	OPEN IC_JOURADJUST.NAME$ FOR INPUT AS FILE IC_JOURADJUST.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP IC_JOURADJUST, &
		PRIMARY KEY &
		( &
			IC_JOURADJUST::LOCATION, &
			IC_JOURADJUST::PRODUCT &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

