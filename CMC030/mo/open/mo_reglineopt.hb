	!
	! File Layout for: MO.MO_REGLINEOPT on 21-May-01
	!
	! Manufacturing Order Register Line Option File
	!

	RECORD MO_REGLINEOPT_CDD
		! Element =
		!   Description = Order Number
		STRING ORDNUM = 10
		! Element =
		!   Description = Model record line number
		STRING LIN = 4
		! Element =
		!   Description = Option Record line number
		STRING OPTLIN = 4
		! Element =
		!   Description = Option Group
		STRING OPTGROUP = 2
		! Element =
		!   Description = Option Code
		STRING OPTN = 4
		! Element =
		!   Description = Order Quantity
		GFLOAT QTY
		! Element =
		!   Description = Cost Per Unit
		GFLOAT COST
		! Element =
		!   Description = Price Per Unit
		GFLOAT PRICE
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element =
		!   Description = Transaction Code
		STRING TRANTYPE = 2
		! Element =
		!   Description = Batch Number
		STRING BATCH = 6
		! Element =
		!   Description = Ship No
		STRING SHIPNO = 2
		! Element = DATE
		!   Description = Date (YYYYMMDD)
		STRING POSTDATE = 8
		! Element = TIME
		!   Description = Time (HHMMSS)
		STRING POSTTIME = 6
		! Element =
		!   Description = Option Description
		STRING OPTDESCR = 40
		! Element = DATE
		!   Description = Transaction Date (YYYYMMDD)
		STRING TDATE = 8
		! Element = PERIOD
		!   Description = Fiscal year (YYYY) and Cycle (PP)
		STRING PERIOD = 6
	END RECORD
