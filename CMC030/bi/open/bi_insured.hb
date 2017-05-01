	!
	! File Layout for: BI.BI_INSURED on 21-May-01
	!
	! Insured File
	!

	RECORD BI_INSURED_CDD
		! Element = CUSNUM
		!   Description = Insured Number
		STRING INSURED = 10
		! Element =
		!   Description = Insured name
		STRING INSNAME = 50
		! Element =
		!   Description = Insured address, line 1
		STRING ADD1 = 25
		! Element =
		!   Description = Insured address, line 2
		STRING ADD2 = 21
		! Element =
		!   Description = Insured city
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
		!   Description = Alpha sort field
		STRING ALPSRT = 15
		! Element = REFNO
		!   Description = Reference number
		STRING REFNO = 16
		! Element = DATE
		!   Description = Birthdate (YYYYMMDD)
		STRING BIRTHDATE = 8
		! Element = SEX
		!   Description = Sex
		STRING SEX = 1
		! Element = DATE
		!   Description = Onset date (YYYYMMDD)
		STRING ONSETDATE = 8
	END RECORD
