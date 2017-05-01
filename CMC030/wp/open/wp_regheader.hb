	!
	! File Layout for: WP.WP_REGHEADER on 21-May-01
	!
	! WP Register Header
	!

	RECORD WP_REGHEADER_CDD
		! Element =
		!   Description = Subject type for Job "J"
		STRING SUBJECT = 1
		! Element = SUBACCT
		!   Description = Job number
		STRING JOB = 10
		! Element = DESCRIPTION6
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
		! Element =
		!   Description = Job Status
		STRING SSTATUS = 1
		! Element = DATE
		!   Description = Closed Date (YYYYMMDD)
		STRING EDATE = 8
		! Element = LOCATION
		!   Description = Job Location
		STRING LOCATION = 4
		! Element = OPERATOR
		!   Description = Operator
		STRING OPERATOR = 10
		! Element = REFNO
		!   Description = Reference number
		STRING REFNO = 16
		! Element = BATCH
		!   Description = Batch No
		STRING BATCH = 6
		! Element = TIME
		!   Description = Post Time
		STRING POST_TIME = 6
		! Element = DATE
		!   Description = Post Date
		STRING POST_DATE = 8
	END RECORD
