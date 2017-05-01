	!======================================================================
	! TV_ARTOGL file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(TV_ARTOGL.CH%, STAT%)
	CALL READ_DEVICE('TV_ARTOGL',TV_ARTOGL.DEV$, STAT%)

	TV_ARTOGL.NAME$ = TV_ARTOGL.DEV$+"TV_ARTOGL.TBL"

	OPEN TV_ARTOGL.NAME$ FOR INPUT AS FILE TV_ARTOGL.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP TV_ARTOGL, &
		PRIMARY KEY &
			TV_ARTOGL::CUSTYP, &
		ACCESS MODIFY, ALLOW MODIFY

