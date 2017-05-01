	!======================================================================
	! MO_INVJOUR file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(MO_INVJOUR.CH%, STAT%)
	CALL READ_DEVICE('MO_INVJOUR',MO_INVJOUR.DEV$, STAT%)

	MO_INVJOUR.NAME$ = MO_INVJOUR.DEV$+"MO_INVJOUR_"+BATCH_NO$+".JRL"

	OPEN MO_INVJOUR.NAME$ FOR INPUT AS FILE MO_INVJOUR.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP MO_INVJOUR, &
		PRIMARY KEY &
			MO_INVJOUR::ORDNUM, &
		ACCESS MODIFY, ALLOW MODIFY

