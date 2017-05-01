	!======================================================================
	! PO_REGHEADER file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PO_REGHEADER.CH%, STAT%)
	CALL READ_DEVICE('PO_REGHEADER',PO_REGHEADER.DEV$, STAT%)

	PO_REGHEADER.NAME$ = PO_REGHEADER.DEV$+"PO_REGHEADER.HIS"

	OPEN PO_REGHEADER.NAME$ FOR INPUT AS FILE PO_REGHEADER.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PO_REGHEADER, &
		PRIMARY KEY &
			PO_REGHEADER::PO_NUM, &
		ALTERNATE KEY &
		( &
			PO_REGHEADER::VEN_NUM, &
			PO_REGHEADER::PO_NUM &
		)	CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

