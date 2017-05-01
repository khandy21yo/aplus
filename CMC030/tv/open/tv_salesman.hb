	!
	! File Layout for: TV.TV_SALESMAN on 21-May-01
	!
	! TV Salesman Table
	!

	RECORD TV_SALESMAN_CDD
		! Element = TV_SALNUM
		!   Description = Salesperson number
		STRING SALNUM = 10
		! Element = NAME
		!   Description = Name
		STRING SNAME = 30
		! Element = ADD1
		!   Description = Address line 1
		STRING ADD1 = 25
		! Element = ADD2
		!   Description = Address line 2
		STRING ADD2 = 21
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
		STRING COUNTRY = 8
		! Element = PHONE
		!   Description = Phone number
		STRING PHONE = 10
		! Element = ALPSRT
		!   Description = Alpha sort key
		STRING ALPSRT = 15
		! Element =
		!   Description = Rate
		GFLOAT COMM
	END RECORD
