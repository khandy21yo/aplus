	!======================================================================
	! PD_PRODTYPE file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PD_PRODTYPE.CH%, STAT%)
	CALL READ_DEVICE('PD_PRODTYPE',PD_PRODTYPE.DEV$, STAT%)

	PD_PRODTYPE.NAME$ = PD_PRODTYPE.DEV$+"PD_PRODTYPE.TBL"

	OPEN PD_PRODTYPE.NAME$ FOR INPUT AS FILE PD_PRODTYPE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PD_PRODTYPE, &
		PRIMARY KEY &
			PD_PRODTYPE::CODE, &
		ACCESS MODIFY, ALLOW MODIFY

