	!======================================================================
	! PD_PRODACCT file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PD_PRODACCT.CH%, STAT%)
	CALL READ_DEVICE('PD_PRODACCT',PD_PRODACCT.DEV$, STAT%)

	PD_PRODACCT.NAME$ = PD_PRODACCT.DEV$+"PD_PRODACCT.TBL"

	OPEN PD_PRODACCT.NAME$ FOR INPUT AS FILE PD_PRODACCT.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PD_PRODACCT, &
		PRIMARY KEY &
		( &
			PD_PRODACCT::LOCATION, &
			PD_PRODACCT::PRODTYPE &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

