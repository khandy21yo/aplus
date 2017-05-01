	!
	! File Layout for: MO.MO_REGHEADER on 21-May-01
	!
	! MO Register Header
	!

	RECORD MO_REGHEADER_CDD
		! Element = ORDNUM
		!   Description = Order Number
		STRING ORDNUM = 10
		! Element = ORDTYPE
		!   Description = Sales Order Type
		STRING ORDTYPE = 2
		! Element =
		!   Description = Order Category
		STRING ORDCAT = 4
		! Element =
		!   Description = Order Date
		STRING ORDDATE = 8
		! Element = ASTATUS
		!   Description = Activity status
		STRING ASTATUS = 1
		! Element =
		!   Description = Date
		STRING SDATE = 8
		! Element =
		!   Description = Customer Number
		STRING CUSNUM = 10
		! Element =
		!   Description = Shipping Name
		STRING SHIPNAM = 50
		! Element =
		!   Description = Address, line 1
		STRING ADD1 = 25
		! Element =
		!   Description = Address, line 2
		STRING ADD2 = 25
		! Element =
		!   Description = Address, line 3
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
		!   Description = Customer PO.
		STRING CUSTPO = 10
		! Element =
		!   Description = Ship Via
		STRING SHIPVIA = 2
		! Element =
		!   Description = Terms
		STRING TERMS = 2
		! Element =
		!   Description = Discount Percentage
		GFLOAT DISC
		! Element = TAXCODE
		!   Description = Tax code
		STRING TAXCODE = 2
		! Element = TAXFLAG
		!   Description = Tax Flag
		STRING TAXFLAG = 1
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element =
		!   Description = Commission amount
		GFLOAT COMMAMT
		! Element =
		!   Description = Salesmen
		STRING SALESMAN(1) = 10
		! Element =
		!   Description = Commission for salesmen
		GFLOAT SALCOMM(1)
		! Element =
		!   Description = Packing List Release Number
		STRING SHIPNO = 2
		! Element = BATCH
		!   Description = Batch number used for process (post,clos
		STRING BATCH = 6
	END RECORD
