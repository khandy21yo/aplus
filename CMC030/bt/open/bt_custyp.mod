	!======================================================================
	! BT_CUSTYP file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(BT_CUSTYP.CH%, STAT%)
	CALL READ_DEVICE('BT_CUSTYP',BT_CUSTYP.DEV$, STAT%)

	BT_CUSTYP.NAME$ = BT_CUSTYP.DEV$+"BT_CUSTYP.TBL"

	OPEN BT_CUSTYP.NAME$ FOR INPUT AS FILE BT_CUSTYP.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP BT_CUSTYP, &
		PRIMARY KEY &
			BT_CUSTYP::CUSTYP, &
		ACCESS MODIFY, ALLOW MODIFY

