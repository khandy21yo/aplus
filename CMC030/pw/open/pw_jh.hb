	!
	! File Layout for: PW.PW_JH on 21-May-01
	!
	! PW Journal Header
	!

	RECORD PW_JH_CDD
		! Element = ORDNUM
		!   Description = Order Number
		STRING ORDNUM = 10
		! Element = CUSTOMER
		!   Description = Customer Sold To
		STRING SOLDTO = 10
		! Element = CUSTOMER
		!   Description = Customer Shipped To
		STRING SHIPTO = 10
		! Element = INVOICE
		!   Description = Invoice number
		STRING INVNUM = 8
		! Element = DATE
		!   Description = Invoice Date (YYYYMMDD)
		STRING INVDAT = 8
		! Element = DATE
		!   Description = Ship Date (YYYYMMDD)
		STRING SHPDAT = 8
		! Element = PO
		!   Description = Purchase order number
		STRING CUSPO = 10
		! Element =
		!   Description = Sold By
		STRING SOLDBY = 10
		! Element = TERMS
		!   Description = Terms
		STRING TERMS = 2
		! Element = CARRIER
		!   Description = Carrier Code (Ship Via)
		STRING CARNAM = 2
		! Element = FOB
		!   Description = F.O.B.
		STRING FOBFLG = 1
		! Element =
		!   Description = Line Count
		LONG LINE1
		! Element =
		!   Description = Line Count
		LONG LINE2
	END RECORD
