	!======================================================================
	! PP_DISCOUNT file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PP_DISCOUNT.CH%, STAT%)
	CALL READ_DEVICE('PP_DISCOUNT',PP_DISCOUNT.DEV$, STAT%)

	PP_DISCOUNT.NAME$ = PP_DISCOUNT.DEV$+"PP_DISCOUNT.MAS"

	OPEN PP_DISCOUNT.NAME$ FOR INPUT AS FILE PP_DISCOUNT.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PP_DISCOUNT, &
		PRIMARY KEY &
			PP_DISCOUNT::CODE, &
		ACCESS MODIFY, ALLOW MODIFY

