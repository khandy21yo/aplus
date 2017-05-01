	!
	! File Layout for: WP.WP_REGLINE on 21-May-01
	!
	! Manufacturing Work In Process Register Line File
	!

	RECORD WP_REGLINE_CDD
		! Element = JOB
		!   Description = Job number
		STRING JOB = 10
		! Element = LINE
		!   Description = Line Number
		STRING LLINE = 4
		! Element =
		!   Description = Record Type(01-order,02-comp,03-cancel)
		STRING REC_TYPE = 2
		! Element =
		!   Description = Trans Type(M=material,L=labor)
		STRING TTYPE = 1
		! Element =
		!   Description = Product Number or Operation Code
		STRING ITEMCODE = 14
		! Element =
		!   Description = Cost per Unit Of measure
		GFLOAT COST
		! Element = DESCRIPTION
		!   Description = Description of product or additional
		STRING DESCR = 40
		! Element =
		!   Description = Original Qty
		GFLOAT QTY
		! Element = DATE
		!   Description = Date expected to start production
		STRING START_DATE = 8
		! Element = DATE
		!   Description = Date Production expected to be complete
		STRING COMP_DATE = 8
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
