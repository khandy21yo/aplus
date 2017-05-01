	!
	! File Layout for: PD.PD_PRODUCT on 21-May-01
	!
	! Product Description
	!

	RECORD PD_PRODUCT_CDD
		! Element = PRODUCT_NUM
		!   Description = Product number
		STRING PRODUCT_NUM = 14
		! Element =
		!   Description = Description
		STRING DESCRIPTION = 40
		! Element =
		!   Description = Product type
		STRING PROD_TYPE = 2
		! Element =
		!   Description = Category
		STRING CATEGORY = 4
		! Element =
		!   Description = Unit of measure
		STRING UOM = 2
		! Element =
		!   Description = Unused...
		STRING PACK = 4
		! Element =
		!   Description = Label
		STRING LABEL = 4
		! Element = COSTMETHOD
		!   Description = Costing method
		STRING METHOD = 4
		! Element =
		!   Description = On set Date
		STRING BDATE = 8
		! Element =
		!   Description = Activity Status
		STRING SSTATUS = 1
		! Element =
		!   Description = End Date
		STRING EDATE = 8
		! Element =
		!   Description = Secondary Code
		STRING SECONDARY_CODE = 10
		! Element =
		!   Description = Weight of one unit
		GFLOAT WEIGHT
		! Element =
		!   Description = Pack unit of measure
		STRING BOMUOM = 2
		! Element =
		!   Description = Product units in one pack
		GFLOAT PRODUCT_FACTOR
	END RECORD
