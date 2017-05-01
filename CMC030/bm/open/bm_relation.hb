	!
	! File Layout for: BM.BM_RELATION on 21-May-01
	!
	! Product Structure File
	!

	RECORD BM_RELATION_CDD
		! Element = PRODUCT_NUM
		!   Description = Product number
		STRING PRODUCT = 14
		! Element =
		!   Description = Item number
		STRING ITEMNUM = 4
		! Element = PRODUCT_NUM
		!   Description = Product component
		STRING COMPONENT = 14
		! Element =
		!   Description = Quantity
		GFLOAT QUANTITY
		! Element =
		!   Description = Operation
		STRING OPERATION = 8
		! Element =
		!   Description = Scrap percentage
		WORD SCRAP
	END RECORD
