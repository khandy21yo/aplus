	!
	! File Layout for: OS.OS_JOURNAL on 21-May-01
	!
	! Main Journal Header
	!

	RECORD OS_JOURNAL_CDD
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
		! Element = DEPOSIT
		!   Description = Deposit number
		STRING DEPOSIT = 6
		! Element =
		!   Description = Customer PO. (Obsolete remainder)
		STRING OLDCUSTPO = 4
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
		STRING SALESMAN = 10
		! Element =
		!   Description = Misc Charges Reason Code
		STRING CREASON = 2
		! Element =
		!   Description = Commission for salesmen
		GFLOAT SALCOMM
		! Element =
		!   Description = Handling Amount
		GFLOAT HANDLING
		! Element =
		!   Description = Paid Amount
		GFLOAT AMTPAID
		! Element = CHECK
		!   Description = Check number
		STRING CHECK = 6
		! Element =
		!   Description = Notes
		STRING NOTES(2) = 40
		! Element =
		!   Description = Miscellaneous Account
		STRING MISCACCT = 18
		! Element = DATE
		!   Description = Transaction Date (YYYYMMDD)
		STRING TRANDATE = 8
		! Element = TIME
		!   Description = Transaction Time (HHMMSS)
		STRING TRANTIME = 6
		! Element =
		!   Description = Invoice Number
		STRING INVNUM = 8
		! Element =
		!   Description = Freight Number
		GFLOAT FREIGHT
		! Element = TAXCODE
		!   Description = Tax code
		STRING TAXCODE = 2
		! Element = TAXFLAG
		!   Description = Tax Flag
		STRING TAXFLAG = 1
		! Element = LINE
		!   Description = Shipping Line Address Code
		STRING SHIPLIN = 4
		! Element =
		!   Description = Number of payments
		WORD PAYMNT
		! Element = FLAG
		!   Description = Register Flag - Order Exists in Register
		STRING REG_FLAG = 1
		! Element =
		!   Description = Purchase order number
		STRING CUSTPO = 20
	END RECORD
