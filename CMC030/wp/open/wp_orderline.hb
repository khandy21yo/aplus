	!
	! File Layout for: WP.WP_ORDERLINE on 21-May-01
	!
	! Manufacturing Work In Process Production Line File
	!

	RECORD WP_ORDERLINE_CDD
		! Element = JOB
		!   Description = Job number
		STRING JOB = 10
		! Element =
		!   Description = Type of Line(M=material,L=labor)
		STRING TTYPE = 1
		! Element = PRODUCT
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
		! Element =
		!   Description = Line Number
		STRING LLINE = 4
	END RECORD
