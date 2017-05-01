	!======================================================================
	! PD_LABEL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PD_LABEL.CH%, STAT%)
	CALL READ_DEVICE('PD_LABEL',PD_LABEL.DEV$, STAT%)

	PD_LABEL.NAME$ = PD_LABEL.DEV$+"PD_LABEL.TBL"

	OPEN PD_LABEL.NAME$ FOR INPUT AS FILE PD_LABEL.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PD_LABEL, &
		PRIMARY KEY &
			PD_LABEL::CODE, &
		ACCESS MODIFY, ALLOW MODIFY

