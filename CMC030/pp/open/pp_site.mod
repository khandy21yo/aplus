	!======================================================================
	! PP_SITE file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PP_SITE.CH%, STAT%)
	CALL READ_DEVICE('PP_SITE',PP_SITE.DEV$, STAT%)

	PP_SITE.NAME$ = PP_SITE.DEV$+"PP_SITE.MAS"

	OPEN PP_SITE.NAME$ FOR INPUT AS FILE PP_SITE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PP_SITE, &
		PRIMARY KEY &
		( &
			PP_SITE::HOST, &
			PP_SITE::SITE, &
			PP_SITE::STYPE &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

