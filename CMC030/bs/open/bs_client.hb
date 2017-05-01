	!
	! File Layout for: BS.BS_CLIENT on 21-May-01
	!
	! Client File
	!

	RECORD BS_CLIENT_CDD
		! Element = CLIENT
		!   Description = Client Number
		STRING CLIENT = 10
		! Element =
		!   Description = Client name
		STRING CLIENTNAME = 50
		! Element =
		!   Description = Alpha sort
		STRING ALPSRT = 15
		! Element =
		!   Description = Client address 1
		STRING ADD1 = 25
		! Element =
		!   Description = Client address 2
		STRING ADD2 = 21
		! Element =
		!   Description = Client city
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
		!   Description =
		STRING REFNO = 16
		! Element =
		!   Description =
		STRING BIRTHDATE = 8
		! Element =
		!   Description =
		STRING SEX = 1
		! Element =
		!   Description =
		STRING ONSETDATE = 8
		! Element =
		!   Description =
		STRING SSTATUS = 1
		! Element =
		!   Description =
		STRING TERMDATE = 8
	END RECORD
