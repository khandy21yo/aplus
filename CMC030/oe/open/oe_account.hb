	!
	! File Layout for: OE.OE_ACCOUNT on 21-May-01
	!
	! Sales Order Account Table
	!

	RECORD OE_ACCOUNT_CDD
		! Element = CUSTTYPE
		!   Description = Customer type
		STRING CUSTTYPE = 2
		! Element =
		!   Description = Sales Order Type
		STRING ORDTYPE = 2
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element = ACCOUNT
		!   Description = AR GL Account Number
		STRING ACCOUNT = 18
		! Element = ACCOUNT
		!   Description = Order Discount GL Account Number
		STRING DISACCT = 18
		! Element = ACCOUNT
		!   Description = Freight General Ledger Account Number
		STRING FRACCT = 18
		! Element = ACCOUNT
		!   Description = Handling GL Account Number
		STRING HANDLING = 18
		! Element = ACCOUNT
		!   Description = Seles Account Number
		STRING SALES = 18
		! Element = ACCOUNT
		!   Description = COS Account Number
		STRING COSACCT = 18
	END RECORD
