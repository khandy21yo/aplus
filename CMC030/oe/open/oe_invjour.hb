	!
	! File Layout for: OE.OE_INVJOUR on 21-May-01
	!
	! Order Invoice Journal Header File
	!

	RECORD OE_INVJOUR_CDD
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
		!   Description = Reason Code
		STRING CREASON = 2
		! Element =
		!   Description = Number of Payments
		WORD PAYMNT
		! Element =
		!   Description = unused field
		STRING UNUSED = 14
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
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING MISCACCT = 18
	END RECORD
