	!
	! File Layout for: RM.RM_JOURPROD on 21-May-01
	!
	! Restaurant Journal Product File
	!

	RECORD RM_JOURPROD_CDD
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element = DATE
		!   Description = Start date
		STRING STARTDATE = 8
		! Element = TRANSTYPE
		!   Description = Transaction type code
		STRING TRANSTYPE = 2
		! Element =
		!   Description = Sequential number
		STRING SEQNUM = 4
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element =
		!   Description = Product price
		GFLOAT PRICE
		! Element =
		!   Description = Daily quantity
		GFLOAT QUANTITY(6)
	END RECORD
