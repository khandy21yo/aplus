	!======================================================================
	! TV_BILTYP file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(TV_BILTYP.CH%, STAT%)
	CALL READ_DEVICE('TV_BILTYP',TV_BILTYP.DEV$, STAT%)

	TV_BILTYP.NAME$ = TV_BILTYP.DEV$+"TV_BILTYP.TBL"

	OPEN TV_BILTYP.NAME$ FOR INPUT AS FILE TV_BILTYP.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP TV_BILTYP, &
		PRIMARY KEY &
			TV_BILTYP::BTYPE, &
		ACCESS MODIFY, ALLOW MODIFY

