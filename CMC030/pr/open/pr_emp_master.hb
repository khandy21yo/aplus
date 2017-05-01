	!
	! File Layout for: PR.PR_EMP_MASTER on 21-May-01
	!
	! Employee Master File
	!

	RECORD PR_EMP_MASTER_CDD
		! Element =
		!   Description = Employee number
		STRING EMPNUM = 10
		! Element =
		!   Description = Employee name
		STRING EMPNAME = 30
		! Element =
		!   Description = Street address
		STRING ADD1 = 20
		! Element =
		!   Description = Post office box
		STRING ADD2 = 20
		! Element =
		!   Description = City
		STRING CITY = 16
		! Element =
		!   Description = FIPS postal abbreviation
		STRING STATE = 2
		! Element =
		!   Description = Zip code
		STRING ZIP = 10
		! Element =
		!   Description = Country
		STRING COUNTRY = 6
		! Element =
		!   Description = Telephone number
		STRING PHONE = 10
		! Element =
		!   Description = Social security number
		STRING SSN = 12
		! Element =
		!   Description = Alpha sort key
		STRING SORT = 14
		! Element =
		!   Description = Default sub account number for costing
		STRING SUBACC = 10
		! Element =
		!   Description = Default account number
		STRING ACCT = 18
		! Element =
		!   Description = Default trade
		STRING TRADE = 6
		! Element =
		!   Description = Operation
		STRING OPER = 8
		! Element =
		!   Description = Default Union Code
		STRING UNION = 2
		! Element = LOCATION
		!   Description = Location
		STRING LOCATION = 4
		! Element =
		!   Description = Default department
		STRING DEPT = 6
		! Element = WORK_CENTER
		!   Description = Work Center
		STRING WORK_CENTER = 4
		! Element =
		!   Description = Employee Skill
		STRING EMP_SKILL = 6
		! Element =
		!   Description = Employee grade
		STRING EMP_GRADE = 2
		! Element =
		!   Description = Disabled (Y/N)
		STRING DISABLED = 1
		! Element =
		!   Description = Number of pay periods in a year
		WORD PAYFREQ
		! Element =
		!   Description = Default rate type
		STRING RATE_TYPE = 1
		! Element =
		!   Description = Default rate code
		STRING RATE_CDE = 2
		! Element =
		!   Description = State unemployment code
		STRING SUI_SW = 2
		! Element =
		!   Description = Tax Package
		STRING TAX_PKG = 2
		! Element = CODE
		!   Description = Workmen comp code
		STRING WC = 6
		! Element =
		!   Description = W2 = N, 1099 = Y
		STRING W2_1099 = 1
		! Element =
		!   Description = Employee birthday
		STRING BIRTH = 8
		! Element =
		!   Description = Employee hire date
		STRING HIREDAY = 8
		! Element =
		!   Description = Employee termination date
		STRING TERMDAY = 8
		! Element = FLAG
		!   Description = Rehire this person flag (Y/N)
		STRING REHIRE_FLAG = 1
		! Element =
		!   Description = M = Male, F = Female
		STRING SEX = 1
		! Element =
		!   Description = B=Black,H=Hisp,O=Orien,I=Indian,W=White
		STRING RACE = 1
		! Element =
		!   Description = US Citizen (Y/N)
		STRING USCIT = 1
		! Element =
		!   Description = Work permit number
		STRING WRKPERMIT = 15
		! Element =
		!   Description = Home country
		STRING HOMCNTRY = 6
		! Element =
		!   Description = Active (Y/N)
		STRING ACTIVE_FLAG = 1
	END RECORD
