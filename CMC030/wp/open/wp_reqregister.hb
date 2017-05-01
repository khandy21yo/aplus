	!
	! File Layout for: WP.WP_REQREGISTER on 21-May-01
	!
	! Material Requisition Register File
	!

	RECORD WP_REQREGISTER_CDD
		! Element = JOB
		!   Description = Job number
		STRING JOB = 10
		! Element = LINE
		!   Description = Job Line
		STRING LLINE = 4
		! Element = REQNUM
		!   Description = Requisition Number
		STRING REQNUM = 10
		! Element = LINE
		!   Description = Requisition Line
		STRING REQLIN = 4
		! Element =
		!   Description = Record Type
		STRING RECTYP = 2
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element = QUANTITY
		!   Description = Quantity
		GFLOAT QTY
		! Element =
		!   Description = Dollar Amount
		GFLOAT AMT
		! Element = DATE
		!   Description = Transactin Date (YYYYMMDD)
		STRING TRANDATE = 8
		! Element = OPERATOR
		!   Description = Operator
		STRING OPERATOR = 10
		! Element = PERIOD
		!   Description = Fiscal year (YYYY) and Cycle (PP)
		STRING PERIOD = 6
		! Element = DATE
		!   Description = Posting Date (YYYYMMDD)
		STRING POSTDATE = 8
		! Element = TIME
		!   Description = Post Time (HHMMSS)
		STRING POSTTIME = 6
		! Element = BATCH
		!   Description = Batch number used for process (post,clos
		STRING BATCH = 6
	END RECORD
