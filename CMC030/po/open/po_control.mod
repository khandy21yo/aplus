	!======================================================================
	! PO_CONTROL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PO_CONTROL.CH%, STAT%)
	CALL READ_DEVICE('PO_CONTROL',PO_CONTROL.DEV$, STAT%)

	PO_CONTROL.NAME$ = PO_CONTROL.DEV$+"PO_CONTROL.CTR"

	OPEN PO_CONTROL.NAME$ FOR INPUT AS FILE PO_CONTROL.CH%, &
		ORGANIZATION RELATIVE FIXED, &
		MAP PO_CONTROL, &
		ACCESS MODIFY, ALLOW MODIFY

