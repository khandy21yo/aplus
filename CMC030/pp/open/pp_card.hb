	!
	! File Layout for: PP.PP_CARD on 21-May-01
	!
	! Pacific Pride Card Number File
	!

	RECORD PP_CARD_CDD
		! Element = CUSTOMER
		!   Description = Customer Number
		STRING CUSNUM = 10
		! Element =
		!   Description = Card Number
		STRING CARD = 8
		! Element =
		!   Description = Card Type (1=drv,2=2nd drv,3=veh)
		STRING CTYPE = 1
		! Element = DESCRIPTION
		!   Description = Description
		STRING DESCRIPTION = 40
		! Element =
		!   Description = Beginning Odometer Reading
		GFLOAT ODOMETER
		! Element =
		!   Description = Pacific Pride Customer Number
		STRING SYSCUS = 08
		! Element =
		!   Description = Discount Code
		STRING DISCOUNT = 4
	END RECORD
