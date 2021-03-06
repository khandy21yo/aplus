	!======================================================================
	! RM_JOURPOST file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(RM_JOURPOST.CH%, STAT%)
	CALL READ_DEVICE('RM_JOURPOST',RM_JOURPOST.DEV$, STAT%)

	RM_JOURPOST.NAME$ = RM_JOURPOST.DEV$+"RM_JOURPOST_"+BATCH_NO$+".JRL"

	OPEN RM_JOURPOST.NAME$ FOR INPUT AS FILE RM_JOURPOST.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP RM_JOURPOST, &
		PRIMARY KEY &
		( &
			RM_JOURPOST::LOCATION, &
			RM_JOURPOST::STARTDATE, &
			RM_JOURPOST::TRANSTYPE, &
			RM_JOURPOST::PRODUCT, &
			RM_JOURPOST::ACTDATE &
		)	DUPLICATES , &
		ALTERNATE KEY &
		( &
			RM_JOURPOST::LOCATION, &
			RM_JOURPOST::ACTDATE, &
			RM_JOURPOST::TTYPE &
		)	DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

