	!
	! File Layout for: PW.PW_JL on 21-May-01
	!
	! PW Journal Line
	!

	RECORD PW_JL_CDD
		! Element = ORDNUM
		!   Description = Order Number
		STRING ORDNUM = 10
		! Element =
		!   Description = Window (1,2)
		STRING WINDOW = 1
		! Element = LINE
		!   Description = Line
		STRING JLINE = 4
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRONUM = 14
		! Element = LOCATION
		!   Description = Store number
		STRING STONUM = 4
		! Element = LOT
		!   Description = Lot Number
		STRING LOTNUM = 10
		! Element = DESCRIPTION
		!   Description = Description
		STRING DESCRIPTION = 40
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCNUM = 18
		! Element = QUANTITY
		!   Description = Quantity
		GFLOAT QTY
		! Element =
		!   Description = Price
		GFLOAT PRICE
		! Element =
		!   Description = Weight
		GFLOAT POUNDS
		! Element =
		!   Description = Extension
		GFLOAT EXT
		! Element = PCTYPE
		!   Description = Price Flag
		STRING PRTYPE = 2
		! Element = VENDOR
		!   Description = Vendor Number
		STRING VENNUM = 10
	END RECORD
