	!
	! File Layout for: AR.AR_35CUSTOM on 21-May-01
	!
	! Customer Address File
	!

	RECORD AR_35CUSTOM_CDD
		! Element = CUSNUM
		!   Description = Customer Number
		STRING CUSNUM = 10
		! Element = COMPNAME
		!   Description = Person or Company Name
		STRING CUSNAM = 50
		! Element = CUSTYPE
		!   Description = Customer type
		STRING TTYPE = 2
		! Element = CATEGORY
		!   Description = Category
		STRING CATEGORY = 4
		! Element = DATE
		!   Description = Onset Date (YYYYMMDD)
		STRING BDATE = 8
		! Element = ASTATUS
		!   Description = Activity status
		STRING SSTATUS = 1
		! Element = DATE
		!   Description = Ending Date (YYYYMMDD)
		STRING EDATE = 8
		! Element = ADDRESS
		!   Description = Address line 1
		STRING ADD1 = 25
		! Element = ADDRESS
		!   Description = Address line 2
		STRING ADD2 = 25
		! Element = ADDRESS
		!   Description = Address line 3
		STRING ADD3 = 25
		! Element = CITY
		!   Description = City
		STRING CITY = 15
		! Element = STATE
		!   Description = State
		STRING STATE = 2
		! Element = ZIP
		!   Description = Zip code
		STRING ZIP = 10
		! Element = COUNTRY
		!   Description = Country
		STRING COUNTRY = 2
		! Element = COUNTY
		!   Description = County
		STRING COUNTY = 2
		! Element = PHONE
		!   Description = Phone number
		STRING PHONE = 10
		! Element =
		!   Description = O-open item, B-balance forward
		STRING METHOD = 1
		! Element =
		!   Description = Statement (Y/N)
		STRING STMTFLG = 1
		! Element = ALFASORT
		!   Description = Alpha Sort Key
		STRING ALPSRT = 15
		! Element = YESNO
		!   Description = Service Charge (Yes or No Flag)
		STRING SERCHRG = 1
		! Element = TAXCODE
		!   Description = Tax code
		STRING TAXCODE = 2
		! Element = TAXEXEMPT
		!   Description = Tax Exampt Permit Number
		STRING TAXEXEMP = 15
		! Element = LOCATION
		!   Description = Primary Location
		STRING LOCATION = 4
		! Element = TERMS
		!   Description = Terms
		STRING TERMS = 2
		! Element = CARRIER
		!   Description = Carrier Code (Ship Via)
		STRING CARRIER = 2
		! Element = SALESMAN
		!   Description = Salesperson number
		STRING SALESMAN = 10
		! Element =
		!   Description = Credit Limit
		GFLOAT CREDITLIM
		! Element = DISCOUNT
		!   Description = Discount percentage
		GFLOAT DISCOUNT
		! Element = YESNO
		!   Description = Accept Backorders Flag (Y/N)
		STRING BACKORDER = 1
		! Element = TAXFLAG
		!   Description = Tax Flag
		STRING TAXFLAG = 1
	END RECORD
