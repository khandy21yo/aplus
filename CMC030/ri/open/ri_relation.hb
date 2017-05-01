	!
	! File Layout for: RI.RI_RELATION on 21-May-01
	!
	! Product Relation
	!

	RECORD RI_RELATION_CDD
		! Element = PRODUCT_NUM
		!   Description = Product number
		STRING PRODUCT = 14
		! Element =
		!   Description = Item number
		STRING ITEMNUM = 4
		! Element = PRODUCT_NUM
		!   Description = Product number
		STRING INGREDIENT = 14
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
