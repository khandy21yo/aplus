	!
	! File Layout for: MO.MO_ORDERJOUR on 21-May-01
	!
	! Manufacturing Order Journal
	!

	RECORD MO_ORDERJOUR_CDD
		! Element =
		!   Description = Order Number
		STRING ORDNUM = 10
		! Element =
		!   Description = Order Date
		STRING ORDDATE = 8
		! Element =
		!   Description = Order Type
		STRING ORDTYPE = 2
		! Element =
		!   Description = Order Category
		STRING ORDCAT = 4
		! Element =
		!   Description = Customer Number
		STRING CUSNUM = 10
		! Element =
		!   Description = Order Discount
		GFLOAT DISC
		! Element =
		!   Description = Miscellaneous Charges
		GFLOAT MISC
		! Element =
		!   Description = Ship Name
		STRING SHIPNAM = 50
		! Element =
		!   Description = Ship To Address 1
		STRING ADD1 = 25
		! Element =
		!   Description = Ship To Address 2
		STRING ADD2 = 25
		! Element =
		!   Description = Ship To Address 3
		STRING ADD3 = 25
		! Element =
		!   Description = Ship To City
		STRING CITY = 15
		! Element =
		!   Description = Ship To State
		STRING STATE = 2
		! Element =
		!   Description = Ship To Zip Code
		STRING ZIP = 10
		! Element =
		!   Description = Ship To Country
		STRING COUNTRY = 2
		! Element =
		!   Description = Customer PO.
		STRING CUSTPO = 10
		! Element =
		!   Description = Date
		STRING SHIPDATE = 8
		! Element =
		!   Description = Ship Via
		STRING SHIPVIA = 2
		! Element =
		!   Description = Terms
		STRING TERMS = 2
		! Element =
		!   Description = Taxes
		GFLOAT SALESTAX
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element =
		!   Description = Operator
		STRING OPERATOR = 10
		! Element =
		!   Description = Commission amount
		GFLOAT COMMAMT
		! Element =
		!   Description = Commission percentage
		GFLOAT COMMPERC
		! Element =
		!   Description = Salesmen
		STRING SALESMAN(1) = 10
		! Element =
		!   Description = Commission for salesmen
		GFLOAT SALCOMM(1)
		! Element =
		!   Description = Paid Amount
		GFLOAT AMTPAID
		! Element = CHECK
		!   Description = Check number
		STRING CHECK = 6
		! Element =
		!   Description = Notes
		STRING NOTES(3) = 40
		! Element =
		!   Description = Freight Number
		GFLOAT FREIGHT
		! Element = TAXCODE
		!   Description = Tax code
		STRING TAXCODE = 2
		! Element = TAXFLAG
		!   Description = Tax Flag
		STRING TAXFLAG = 1
		! Element =
		!   Description = Line Number of Ship To Code
		STRING SHIPLIN = 4
	END RECORD
