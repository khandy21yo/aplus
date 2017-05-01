	!
	! File Layout for: WP.WP_JOB on 21-May-01
	!
	! Manufacturing Work In Process Journal Header File
	!

	RECORD WP_JOB_CDD
		! Element = SUBACCT
		!   Description = Job number
		STRING JOB = 10
		! Element = DESCRIPTION
		!   Description = Job Description
		STRING DESCR = 40
		! Element =
		!   Description = Job Type
		STRING TTYPE = 2
		! Element = CLASS
		!   Description = Job Class
		STRING CLASS = 4
		! Element = DATE
		!   Description = Creation Date (YYYYMMDD)
		STRING BDATE = 8
		! Element = LOCATION
		!   Description = Job Location
		STRING LOCATION = 4
		! Element = OPERATOR
		!   Description = Operator
		STRING OPERATOR = 10
		! Element = REFNO
		!   Description = Reference number
		STRING REFNO = 16
		! Element =
		!   Description = Notes
		STRING NOTES(1) = 40
	END RECORD
