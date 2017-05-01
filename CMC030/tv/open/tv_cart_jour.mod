	!======================================================================
	! TV_CART_JOUR file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(TV_CART_JOUR.CH%, STAT%)
	CALL READ_DEVICE('TV_CART_JOUR',TV_CART_JOUR.DEV$, STAT%)

	TV_CART_JOUR.NAME$ = TV_CART_JOUR.DEV$+"TV_CART_JOUR.TBL"

	OPEN TV_CART_JOUR.NAME$ FOR INPUT AS FILE TV_CART_JOUR.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP TV_CART_JOUR, &
		PRIMARY KEY &
		( &
			TV_CART_JOUR::CARTNUM, &
			TV_CART_JOUR::CUTNUM &
		)	, &
		ACCESS MODIFY, ALLOW MODIFY

