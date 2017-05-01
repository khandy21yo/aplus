	!
	! File Layout for: AR.AR_CUSTOM on 21-May-01
	!
	! Accounts Receivable Customer File
	!

	RECORD AR_CUSTOM_CDD
		! Element = CUSNUM
		!   Description = Customer Number
		STRING CUSNUM = 10
		! Element =
		!   Description = Customer name
		STRING CUSNAM = 50
		! Element =
		!   Description = Customer address 1
		STRING ADD1 = 25
		! Element =
		!   Description = Customer address 2
		STRING ADD2 = 21
		! Element =
		!   Description = Customer city
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
		! Element =
		!   Description = Unused
		STRING FILLER1 = 4
		! Element = PHONE
		!   Description = Phone number
		STRING PHONE = 10
		! Element =
		!   Description = O-open item, B-balance forward
		STRING METHOD = 1
		! Element =
		!   Description = Statement (y/n)
		STRING STMTFLG = 1
		! Element =
		!   Description = Alpha sort field
		STRING ALPSRT = 15
		! Element =
		!   Description = Service charge (y/n)
		STRING SERCHRG = 1
		! Element =
		!   Description = Purge flag (Y or N)
		STRING PURGE = 1
		! Element =
		!   Description = UNUSED SPACE
		STRING FILLER = 9
	END RECORD
