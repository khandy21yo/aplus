	!
	! File Layout for: RM.RM_JOURPOST on 21-May-01
	!
	! Restaurant Journal Posting File
	!

	RECORD RM_JOURPOST_CDD
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element = DATE
		!   Description = Date
		STRING STARTDATE = 8
		! Element = TRANSTYPE
		!   Description = Transaction type code from entry
		STRING TRANSTYPE = 2
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element = DATE
		!   Description = Actual date (YYYYMMDD)
		STRING ACTDATE = 8
		! Element = TRANSTYPE
		!   Description = Transaction type code
		STRING TTYPE = 2
		! Element =
		!   Description = Quantity
		GFLOAT QUANTITY
		! Element =
		!   Description = Price
		GFLOAT PRICE
		! Element =
		!   Description = Sequential number
		STRING SEQNUM = 4
	END RECORD
