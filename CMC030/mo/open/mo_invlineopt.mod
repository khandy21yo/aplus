	!======================================================================
	! MO_INVLINEOPT file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(MO_INVLINEOPT.CH%, STAT%)
	CALL READ_DEVICE('MO_INVLINEOPT',MO_INVLINEOPT.DEV$, STAT%)

	MO_INVLINEOPT.NAME$ = MO_INVLINEOPT.DEV$+"MO_INVLINEOPT_"+BATCH_NO$+".JRL"

	OPEN MO_INVLINEOPT.NAME$ FOR INPUT AS FILE MO_INVLINEOPT.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP MO_INVLINEOPT, &
		PRIMARY KEY &
		( &
			MO_INVLINEOPT::ORDNUM, &
			MO_INVLINEOPT::OLINE, &
			MO_INVLINEOPT::OPTLINE &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

