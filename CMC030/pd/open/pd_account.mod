	!======================================================================
	! PD_ACCOUNT file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PD_ACCOUNT.CH%, STAT%)
	CALL READ_DEVICE('PD_ACCOUNT',PD_ACCOUNT.DEV$, STAT%)

	PD_ACCOUNT.NAME$ = PD_ACCOUNT.DEV$+"PD_ACCOUNT.TBL"

	OPEN PD_ACCOUNT.NAME$ FOR INPUT AS FILE PD_ACCOUNT.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PD_ACCOUNT, &
		PRIMARY KEY &
		( &
			PD_ACCOUNT::PRODTYPE, &
			PD_ACCOUNT::LOCATION &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

