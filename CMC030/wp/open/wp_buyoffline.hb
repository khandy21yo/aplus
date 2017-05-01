	!
	! File Layout for: WP.WP_BUYOFFLINE on 21-May-01
	!
	! Manufacturing WIP Buyoff line file
	!

	RECORD WP_BUYOFFLINE_CDD
		! Element =
		!   Description = WIP Job Number
		STRING JOB = 10
		! Element =
		!   Description = Line Number
		STRING LLINE = 4
		! Element =
		!   Description = Completed Quantity
		GFLOAT COMPQTY
		! Element =
		!   Description = Cancelled Quantity
		GFLOAT CANCELQTY
		! Element =
		!   Description = Unit Cost
		GFLOAT COST
		! Element = DATE
		!   Description = Date of buyoff(YYYYMMDD)
		STRING BDATE = 8
		! Element = ACCOUNT
		!   Description = Buyoff General Ledger Account Number
		STRING ACCT = 18
		! Element =
		!   Description = Finished Goods Id No.
		STRING FG_ID_NO = 10
	END RECORD
