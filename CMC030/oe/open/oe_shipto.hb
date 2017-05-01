	!
	! File Layout for: OE.OE_SHIPTO on 21-May-01
	!
	! Ship to Address
	!

	RECORD OE_SHIPTO_CDD
		! Element =
		!   Description = Customer Number
		STRING CUSNUM = 10
		! Element = LINE
		!   Description = Line
		STRING LINES = 4
		! Element =
		!   Description = Shipping Name
		STRING SHIPNAM = 50
		! Element =
		!   Description = Address 1
		STRING ADD1 = 25
		! Element =
		!   Description = Address 2
		STRING ADD2 = 25
		! Element =
		!   Description = Address 3
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
		! Element =
		!   Description = Ship Via
		STRING SHIPVIA = 2
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element =
		!   Description = Salesmen
		STRING SALESMAN = 10
		! Element = TAXCODE
		!   Description = Tax code
		STRING TAXCODE = 2
		! Element = TAXEXEMPT
		!   Description = Tax Exampt Permit Number
		STRING TAXEXEMP = 15
		! Element =
		!   Description = Ship to adrress notes
		STRING NOTES(2) = 40
		! Element = PHONE
		!   Description = Phone number
		STRING PHONE = 10
	END RECORD
