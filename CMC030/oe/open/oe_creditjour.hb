	!
	! File Layout for: OE.OE_CREDITJOUR on 21-May-01
	!
	! Credit Memo Journal Header
	!

	RECORD OE_CREDITJOUR_CDD
		! Element =
		!   Description = Memo Number
		STRING MEMONUM = 8
		! Element =
		!   Description = Memo Date
		STRING MEMODATE = 8
		! Element =
		!   Description = Order Date
		STRING ORDDATE = 8
		! Element =
		!   Description = Handling
		GFLOAT HANDLING
		! Element =
		!   Description = Order Discount Amount
		GFLOAT DISC
		! Element =
		!   Description = Miscellaneous Charges
		GFLOAT MISC
		! Element =
		!   Description = Miscellaneous Charges GL Account
		STRING MISCACCT = 18
		! Element =
		!   Description = Freight Amount
		GFLOAT FREIGHT
		! Element =
		!   Description = Sales Tax Amount
		GFLOAT SALESTAX
		! Element =
		!   Description = Operator
		STRING OPERATOR = 10
		! Element =
		!   Description = Notes
		STRING NOTES(1) = 40
		! Element =
		!   Description = Reason Code
		STRING REASON = 2
		! Element = CUSTOMER
		!   Description = Customer Number
		STRING CUSNUM = 10
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element = ORDTYPE
		!   Description = Order type
		STRING ORDTYPE = 2
		! Element = SALESMAN
		!   Description = Salesperson number
		STRING SALESMAN = 10
		! Element =
		!   Description = Shipping Name
		STRING SHIPNAM = 50
		! Element =
		!   Description = Address, line 1
		STRING ADD1 = 25
		! Element =
		!   Description = Address, line 2
		STRING ADD2 = 25
		! Element =
		!   Description = Address, line 3
		STRING ADD3 = 25
		! Element =
		!   Description = City
		STRING CITY = 15
		! Element =
		!   Description = State
		STRING STATE = 2
		! Element =
		!   Description = Zip Code
		STRING ZIP = 10
		! Element =
		!   Description = Country
		STRING COUNTRY = 2
	END RECORD
