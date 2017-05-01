	!
	! File Layout for: PR.PR_SALES on 21-May-01
	!
	! Sales File
	!

	RECORD PR_SALES_CDD
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element = DEPARTMENT
		!   Description = Department number
		STRING DEPARTMENT = 6
		! Element = DATE
		!   Description = Date of sale (YYYYMMDD)
		STRING SALEDATE = 8
		! Element =
		!   Description = Sales amount
		GFLOAT AMOUNT
	END RECORD
