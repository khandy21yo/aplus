	!======================================================================
	! OE_REGHEADER file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(OE_REGHEADER.CH%, STAT%)
	CALL READ_DEVICE('OE_REGHEADER',OE_REGHEADER.DEV$, STAT%)

	OE_REGHEADER.NAME$ = OE_REGHEADER.DEV$+"OE_REGHEADER.HIS"

	OPEN OE_REGHEADER.NAME$ FOR INPUT AS FILE OE_REGHEADER.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP OE_REGHEADER, &
		PRIMARY KEY &
			OE_REGHEADER::ORDNUM, &
		ALTERNATE KEY &
		( &
			OE_REGHEADER::ORDTYPE, &
			OE_REGHEADER::ORDNUM &
		)	CHANGES, &
		ALTERNATE KEY &
		( &
			OE_REGHEADER::ORDCAT, &
			OE_REGHEADER::ORDNUM &
		)	CHANGES, &
		ALTERNATE KEY &
		( &
			OE_REGHEADER::CUSNUM, &
			OE_REGHEADER::ORDNUM &
		)	CHANGES, &
		ALTERNATE KEY &
		( &
			OE_REGHEADER::BATCH, &
			OE_REGHEADER::ORDNUM &
		)	CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

