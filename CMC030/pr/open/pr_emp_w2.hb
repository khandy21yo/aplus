	!
	! File Layout for: PR.PR_EMP_W2 on 21-May-01
	!
	! W2 EMPLOYEE GENERATION
	!

	RECORD PR_EMP_W2_CDD
		! Element = EMPLOYEE
		!   Description = Employee number
		STRING EMPNUM = 10
		! Element = NAME
		!   Description = Name of a Person
		STRING EMPNAME = 30
		! Element = ADDRESS
		!   Description = Address Line
		STRING ADD1 = 20
		! Element = ADDRESS
		!   Description = Address Line
		STRING ADD2 = 20
		! Element = CITY
		!   Description = City
		STRING CITY = 16
		! Element = STATE
		!   Description = State
		STRING STATE = 2
		! Element = ZIP
		!   Description = Zip code
		STRING ZIP = 10
		! Element = COUNTRY
		!   Description = Country
		STRING COUNTRY = 2
		! Element = SSN
		!   Description = Social Security Number
		STRING SSN = 12
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element =
		!   Description = Array of earnings
		GFLOAT EARNINGS(10)
		! Element =
		!   Description = Array of earnings codes
		STRING EARNINGS_CODE(10) = 2
		! Element =
		!   Description = Array of Taxes witheld
		GFLOAT TAXES(10)
		! Element =
		!   Description = Array of taxables
		GFLOAT TAXABLE(10)
		! Element =
		!   Description = Array of tax codes
		STRING TAXES_CODE(10) = 2
		! Element =
		!   Description = Array of state codes for taxes
		STRING TAXES_STATE(10) = 2
		! Element =
		!   Description = Array of tax id numbers
		STRING TAXES_ID(10) = 20
		! Element =
		!   Description = Array of deductions
		GFLOAT DEDUCTIONS(10)
		! Element =
		!   Description = Array of deduction codes
		STRING DEDUCTIONS_CODE(10) = 2
	END RECORD
