	!
	! File Layout for: PD.PD_ACCOUNT on 21-May-01
	!
	! Product Type Account Table
	!

	RECORD PD_ACCOUNT_CDD
		! Element = PRODTYPE
		!   Description = Inventory Product Type
		STRING PRODTYPE = 2
		! Element = LOCATION
		!   Description = Location
		STRING LOCATION = 4
		! Element = ACCOUNT
		!   Description = Inventory General Ledger Account Number
		STRING INVACCT = 18
		! Element = ACCOUNT
		!   Description = Work in Process Account
		STRING WIPACCT = 18
		! Element = ACCOUNT
		!   Description = COS General Ledger Account Number
		STRING COSACCT = 18
		! Element = ACCOUNT
		!   Description = Inv Disc General Ledger Account Number
		STRING DISCACCT = 18
		! Element = ACCOUNT
		!   Description = Miscellaneous Charges Account Num
		STRING MISCHACCT = 18
		! Element = ACCOUNT
		!   Description = Product Price Variance Account
		STRING PRICEVARACCT = 18
		! Element = ACCOUNT
		!   Description = Miscellaneous (2) Changes Account
		STRING MISCH2ACCT = 18
	END RECORD
