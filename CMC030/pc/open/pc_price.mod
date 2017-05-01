	!======================================================================
	! PC_PRICE file (open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(PC_PRICE.CH%, STAT%)
	CALL READ_DEVICE('PC_PRICE',PC_PRICE.DEV$, STAT%)

	PC_PRICE.NAME$ = PC_PRICE.DEV$+"PC_PRICE.MAS"

	OPEN PC_PRICE.NAME$ FOR INPUT AS FILE PC_PRICE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PC_PRICE, &
		PRIMARY KEY &
		( &
			PC_PRICE::PRODUCT_NUM, &
			PC_PRICE::LOCATION &
		)	DUPLICATES , &
		ALTERNATE KEY &
		( &
			PC_PRICE::PCTYPE, &
			PC_PRICE::PRODUCT_NUM, &
			PC_PRICE::LOCATION, &
			PC_PRICE::XDATE &
		)	DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY

