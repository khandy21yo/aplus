	!
	! File Layout for: AR.AR_SALTAXLED on 21-May-01
	!
	! Sales Tax Monthly Report File
	!

	RECORD AR_SALTAXLED_CDD
		! Element =
		!   Description = Tax Type
		STRING TAXTYP = 1
		! Element = CUSTOMER
		!   Description = Customer Number
		STRING CUSNUM = 10
		! Element = INVOICE
		!   Description = Invoice number
		STRING INVNUM = 8
		! Element =
		!   Description = Sales Tax Amount
		GFLOAT AMOUNT
		! Element = BATCH
		!   Description = Batch number used for posting
		STRING BATCH = 6
		! Element =
		!   Description = Transaction date
		STRING TRADAT = 8
	END RECORD
