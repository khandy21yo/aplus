	!
	! File Layout for: SA.SA_SALESMAN on 21-May-01
	!
	! Salesman Address File
	!

	RECORD SA_SALESMAN_CDD
		! Element = SUBJECT
		!   Description = Subject type for salesman "S"
		STRING SUBJECT = 1
		! Element =
		!   Description = Salesperson number
		STRING SALESMAN = 10
		! Element =
		!   Description = Salesman or Broker Name
		STRING DESCR = 40
		! Element =
		!   Description = Job Type
		STRING TTYPE = 2
		! Element =
		!   Description = Job Class
		STRING CLASS = 4
		! Element =
		!   Description = Onset Date
		STRING BDATE = 8
		! Element =
		!   Description = Current Job Status
		STRING SSTATUS = 1
		! Element =
		!   Description = Termination Date
		STRING EDATE = 8
		! Element = ADDRESS
		!   Description = Address Line
		STRING ADD1 = 25
		! Element = ADDRESS
		!   Description = Address Line
		STRING ADD2 = 25
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
		! Element = PHONE
		!   Description = Phone number
		STRING PHONE = 10
		! Element = INITIALS
		!   Description = Inititals
		STRING INITIALS = 3
		! Element = REGION
		!   Description = Region number
		STRING REGION = 2
		! Element =
		!   Description = Commission Percentage
		GFLOAT COMMPER
	END RECORD
