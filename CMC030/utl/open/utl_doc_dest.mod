	!======================================================================
	! UTL_DOC_DEST file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(UTL_DOC_DEST.CH%, STAT%)
	CALL READ_DEVICE('UTL_DOC_DEST',UTL_DOC_DEST.DEV$, STAT%)

	UTL_DOC_DEST.NAME$ = "CMC:UTL_DOC_DEST.TBL"

	OPEN UTL_DOC_DEST.NAME$ FOR INPUT AS FILE UTL_DOC_DEST.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP UTL_DOC_DEST, &
		PRIMARY KEY &
			UTL_DOC_DEST::PNAME, &
		ACCESS MODIFY, ALLOW MODIFY

