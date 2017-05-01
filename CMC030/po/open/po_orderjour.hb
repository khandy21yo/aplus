	!
	! File Layout for: PO.PO_ORDERJOUR on 21-May-01
	!
	! Purchase Order Header
	!

	RECORD PO_ORDERJOUR_CDD
		! Element = PO
		!   Description = Purchase order number
		STRING PO = 10
		! Element =
		!   Description = Purchase Order Type
		STRING POTYPE = 2
		! Element = DATE
		!   Description = Release Date (YYYYMMDD)
		STRING PODATE = 8
		! Element = VENDOR
		!   Description = Vendor Number (Seller)
		STRING VENDOR = 10
		! Element =
		!   Description = Buyer
		STRING BUYER = 10
		! Element =
		!   Description = Salesman
		STRING SALESMAN = 10
		! Element = TERM
		!   Description = Terms
		STRING TERMS = 2
		! Element =
		!   Description = Carrier
		STRING CARRIER = 2
		! Element =
		!   Description = FOB Code
		STRING FOB = 2
		! Element =
		!   Description = Acknowledgement code
		STRING ACKNOW = 2
		! Element =
		!   Description = Collect / Pre-paid
		STRING COL_PPD = 1
		! Element =
		!   Description = Notes
		STRING NOTE(3) = 40
		! Element =
		!   Description = Person who typed in data
		STRING OPERATOR = 10
		! Element =
		!   Description = Print form (Yes/No)
		STRING PRINTFORM = 1
		! Element = LOCATION
		!   Description = Location number
		STRING FROMLOCATION = 4
		! Element = NAME
		!   Description = Name
		STRING TONAME = 30
		! Element =
		!   Description = Address 1
		STRING TOADD1 = 25
		! Element =
		!   Description = Address 2
		STRING TOADD2 = 25
		! Element = CITY
		!   Description = City
		STRING TOCITY = 15
		! Element = STATE
		!   Description = State
		STRING TOSTATE = 2
		! Element = ZIP
		!   Description = Zip code
		STRING TOZIP = 10
		! Element = COUNTRY
		!   Description = Country
		STRING TOCOUNTRY = 2
		! Element =
		!   Description = Batch Number
		STRING BATCH = 2
		! Element = FLAG
		!   Description = Po Flag if this is a new PO
		STRING NEW = 1
	END RECORD
