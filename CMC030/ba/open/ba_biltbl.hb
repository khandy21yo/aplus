	!
	! File Layout for: BA.BA_BILTBL on 21-May-01
	!
	! Employee Billing
	!

	RECORD BA_BILTBL_CDD
		! Element = EMPLOYEE
		!   Description = Employee number
		STRING EMPNUM = 10
		! Element = CUSTOMER
		!   Description = Customer Number
		STRING CUSNUM = 10
		! Element =
		!   Description = Contract number
		STRING CONTRACT = 20
		! Element = DATE
		!   Description = From Date (YYYYMMDD)
		STRING FROMDATE = 8
		! Element = DATE
		!   Description = To Date (YYYYMMDD)
		STRING TODATE = 8
		! Element =
		!   Description = Method
		STRING METHOD = 1
		! Element =
		!   Description = Rate
		GFLOAT RATE
		! Element =
		!   Description = Amount Billable
		GFLOAT BILLABLE
		! Element =
		!   Description = Amount Billed To date
		GFLOAT BILTODAT
		! Element = BATCH
		!   Description = Batch number used for process (post,clos
		STRING BATCH = 6
	END RECORD
