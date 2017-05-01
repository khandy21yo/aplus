	!
	! File Layout for: UTL.UTL_LOCATION on 21-May-01
	!
	! Location Profile
	!

	RECORD UTL_LOCATION_CDD
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element =
		!   Description = Location name
		STRING LOCNAME = 40
		! Element = REGION
		!   Description = Region number
		STRING REGION = 2
		! Element = LOCGROUP
		!   Description = Location group number
		STRING LOCGROUP = 2
		! Element = STREET
		!   Description = Address (street)
		STRING ADDRESS1 = 25
		! Element = POBOX
		!   Description = Address (P.O.Box)
		STRING ADDRESS2 = 21
		! Element = CITY
		!   Description = City
		STRING CITY = 15
		! Element = STATE
		!   Description = State
		STRING STATE = 2
		! Element = ZIP
		!   Description = Zip code
		STRING ZIP = 10
		! Element = COUNTY
		!   Description = County
		STRING COUNTY = 2
		! Element = COUNTRY
		!   Description = Country
		STRING COUNTRY = 2
		! Element = PHONE
		!   Description = Phone number
		STRING PHONE = 10
		! Element = ADDRESS1
		!   Description = Ship to address, line 1
		STRING SHPADDRESS1 = 25
		! Element = ADDRESS2
		!   Description = Ship to address, line 2
		STRING SHPADDRESS2 = 21
		! Element = CITY
		!   Description = Ship to City
		STRING SHPCITY = 15
		! Element = STATE
		!   Description = Ship to State
		STRING SHPSTATE = 2
		! Element = ZIP
		!   Description = Ship to Zip Code
		STRING SHPZIP = 10
		! Element = COUNTY
		!   Description = Ship to County
		STRING SHPCOUNTY = 2
		! Element = COUNTRY
		!   Description = Ship to Country
		STRING SHPCOUNTRY = 2
		! Element = PHONE
		!   Description = Ship to Phone number
		STRING SHPPHONE = 10
	END RECORD
