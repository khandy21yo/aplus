	!
	! File Layout for: IC.IC_TRANSACTION on 21-May-01
	!
	! Inventory Transaction
	!

	RECORD IC_TRANSACTION_CDD
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element = DATE
		!   Description = Date (MMDDYYYY)
		STRING TRANS_DATE = 8
		! Element = REFNO
		!   Description = Reference number
		STRING PRIMARY_REF = 16
		! Element = XREF
		!   Description = Cross reference
		STRING CROSS_REF = 10
		! Element =
		!   Description = Subaccount
		STRING SUBACCOUNT = 10
		! Element =
		!   Description = Lot number
		STRING LOT = 10
		! Element =
		!   Description = Salesman,Operator etc
		STRING STATIONMAN = 10
		! Element =
		!   Description = Transaction type
		STRING TYPE_A = 2
		! Element = UNIT
		!   Description = Unit amount
		GFLOAT QUANTITY_A
		! Element =
		!   Description = Transaction type
		STRING TYPE_B = 2
		! Element = UNIT
		!   Description = Unit amount
		GFLOAT QUANTITY_B
		! Element =
		!   Description = Inventory cost
		GFLOAT COST
		! Element =
		!   Description = Price Amount
		GFLOAT PRICE
		! Element = ACCOUNT
		!   Description = Transaction General Ledger Account Numbe
		STRING TRANSACCT = 18
		! Element = DATE
		!   Description = Date (MMDDYYYY)
		STRING POSTDATE = 8
		! Element = TIME
		!   Description = Time (HHMMSS)
		STRING POSTTIME = 6
		! Element = BATCH
		!   Description = Batch number used for posting
		STRING BATCH = 6
	END RECORD
