	!
	! File Layout for: OE.OE_REGHEADER on 21-May-01
	!
	! Sales Order Register Header File
	!

	RECORD OE_REGHEADER_CDD
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
		STRING SHIPNAM = 46
		! Element = SHIPLIN
		!   Description = Shipping location number
		STRING SHIPLIN = 4
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
		! Element = DEPOSIT
		!   Description = Deposit number
		STRING DEPOSIT = 6
		! Element =
		!   Description = Customer PO. (Obsolete remainder)
		STRING OLDCUSTPO = 4
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
		STRING SALESMAN = 10
		! Element = OPERATOR
		!   Description = Operator
		STRING OPERATOR = 10
		! Element =
		!   Description = Commission for salesmen
		GFLOAT SALCOMM
		! Element =
		!   Description = Amount Paid
		GFLOAT AMTPAID
		! Element =
		!   Description = Packing List Release Number
		STRING SHIPNO = 2
		! Element = BATCH
		!   Description = Batch number used for process (post,clos
		STRING BATCH = 6
		! Element = NOTES
		!   Description = Notes
		STRING NOTES(2) = 40
		! Element =
		!   Description = Purchase order number
		STRING CUSTPO = 20
	END RECORD
