	!
	! File Layout for: WP.WP_ISSLINE on 21-May-01
	!
	! Material Issue Line File
	!

	RECORD WP_ISSLINE_CDD
		! Element = REQNUM
		!   Description = Requisition Number
		STRING REQNUM = 10
		! Element = JOB
		!   Description = Job number
		STRING JOB = 10
		! Element =
		!   Description = Job Line Number
		STRING LLINE = 4
		! Element =
		!   Description = Requisition Line Number
		STRING REQLINE = 4
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element =
		!   Description = Product Cost
		GFLOAT COST
		! Element =
		!   Description = Quantity Issued
		GFLOAT QTYISSUE
		! Element =
		!   Description = Quantity Canceled
		GFLOAT QTYCANCEL
		! Element = DATE
		!   Description = Issue Date (YYYYMMDD)
		STRING ISSDATE = 8
		! Element =
		!   Description = Running Quantity
		GFLOAT QTYRUN
		! Element =
		!   Description = Product Flag
		STRING PROD_FLAG = 1
	END RECORD
