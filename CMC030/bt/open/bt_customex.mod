	!======================================================================
	! BT_CUSTOMEX file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(BT_CUSTOMEX.CH%, STAT%)
	CALL READ_DEVICE('BT_CUSTOMEX',BT_CUSTOMEX.DEV$, STAT%)

	BT_CUSTOMEX.NAME$ = BT_CUSTOMEX.DEV$+"BT_CUSTOMEX.MAS"

	OPEN BT_CUSTOMEX.NAME$ FOR INPUT AS FILE BT_CUSTOMEX.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP BT_CUSTOMEX, &
		PRIMARY KEY &
			BT_CUSTOMEX::CUSNUM, &
		ACCESS MODIFY, ALLOW MODIFY

