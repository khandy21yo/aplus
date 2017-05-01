	!
	! File Layout for: SA.SA_SALCUST on 21-May-01
	!
	! Customer Sales File
	!

	RECORD SA_SALCUST_CDD
		! Element = CUSTOMER
		!   Description = Customer Number
		STRING CUSNUM = 10
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCOUNT = 18
		! Element = PERIOD
		!   Description = Fiscal year (YYYY) and Cycle (PP)
		STRING PERIOD = 6
		! Element =
		!   Description = Amount
		GFLOAT AMOUNT
		! Element = DATE
		!   Description = PostDate (YYYYMMDD)
		STRING POSTDATE = 8
		! Element = TIME
		!   Description = Post Time (HHMMSS)
		STRING POSTTIME = 6
		! Element = BATCH
		!   Description = Batch number used for process (post,clos
		STRING BATCH = 6
	END RECORD
