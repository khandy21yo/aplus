	!
	! File Layout for: WP.WP_REQLINE on 21-May-01
	!
	! Material Requisition Journal Line
	!

	RECORD WP_REQLINE_CDD
		! Element = JOB
		!   Description = Job number
		STRING JOB = 10
		! Element = LINE
		!   Description = Job Line
		STRING LLINE = 4
		! Element = REQNUM
		!   Description = Requisition Number
		STRING REQNUM = 10
		! Element = OPERATION
		!   Description = Operation
		STRING OPERATION = 8
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element =
		!   Description = Quantity Required
		GFLOAT QTY
		! Element = REQLINE
		!   Description = Requistition Line
		STRING REQLINE = 4
	END RECORD
