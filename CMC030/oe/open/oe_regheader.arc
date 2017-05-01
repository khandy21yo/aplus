	!======================================================================
	! OE_REGHEADER file (create, open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(OE_REGHEADER.CH_ARC%, STAT%)
	CALL READ_DEVICE('OE_REGHEADER_ARC',OE_REGHEADER.DEV$, STAT%)
	CALL READ_PROTECTION('OE_REGHEADER_ARC',OE_REGHEADER.PRO$,STAT%)
	CALL READ_CURPROTECTION(OLD_PROT$,STAT%)
	CALL WRIT_CURPROTECTION(OE_REGHEADER.PRO$, STAT%)

	OE_REGHEADER.NAME_ARC$ = OE_REGHEADER.DEV$+"OE_REGHEADER.HIS_ARC"

	OPEN OE_REGHEADER.NAME_ARC$ AS FILE OE_REGHEADER.CH_ARC%, &
		ORGANIZATION INDEXED FIXED, &
		MAP OE_REGHEADER, &
		BUFFER 32%, &
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
		ACCESS MODIFY, ALLOW READ

	CALL WRIT_CURPROTECTION(OLD_PROT$, STAT%)

