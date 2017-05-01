	!
	! File Layout for: OS.OS_BOM_LINE on 21-May-01
	!
	! Bill of Material of Signs
	!

	RECORD OS_BOM_LINE_CDD
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element = CATEGORY
		!   Description = Category to fill out
		STRING CATEGORY = 4
		! Element = QUANTITY
		!   Description = Default Quantity
		GFLOAT QUANTITY
		! Element = YESNO
		!   Description = Multiple entries allowed
		STRING MULTIPLE = 1
		! Element = YESNO
		!   Description = At least one required?
		STRING REQUIRED = 1
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCOUNT = 18
	END RECORD
