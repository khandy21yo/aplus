	!
	! File Layout for: MO.MO_INVJOUR on 21-May-01
	!
	! Manufacturing Order Invoice Header Journal File
	!

	RECORD MO_INVJOUR_CDD
		! Element =
		!   Description = Order Number
		STRING ORDNUM = 10
		! Element =
		!   Description = Invoice Date
		STRING INVDATE = 8
		! Element =
		!   Description = Date
		STRING SHIPDATE = 8
		! Element =
		!   Description = Terms
		STRING TERMS = 2
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
		STRING NOTES(3) = 40
		! Element = INVOICE
		!   Description = Invoice number
		STRING INVOICE = 8
		! Element =
		!   Description = Packing List Release Number
		STRING SHIPNO = 2
		! Element = CARRIER
		!   Description = Carrier Code (Ship Via)
		STRING SHIPVIA = 2
	END RECORD
