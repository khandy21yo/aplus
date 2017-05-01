	!
	! File Layout for: PD.PD_PRODACCT on 21-May-01
	!
	! Product GL Account Table
	!

	RECORD PD_PRODACCT_CDD
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element = PRODTYPE
		!   Description = Inventory Product Type
		STRING PRODTYPE = 2
		! Element = ACCOUNT
		!   Description = Inventory GL Account Number
		STRING INVACC = 18
		! Element = ACCOUNT
		!   Description = Cost of Sale GL Account Number
		STRING COSACCT = 18
		! Element = ACCOUNT
		!   Description = Product Discount GL Account Number
		STRING DISCACCT = 18
	END RECORD
