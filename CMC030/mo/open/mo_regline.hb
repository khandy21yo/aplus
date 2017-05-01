	!
	! File Layout for: MO.MO_REGLINE on 21-May-01
	!
	! Manufacturing Order Register Line File
	!

	RECORD MO_REGLINE_CDD
		! Element = ORDNUM
		!   Description = Order Number
		STRING ORDNUM = 10
		! Element =
		!   Description = Line Number
		STRING LIN = 4
		! Element =
		!   Description = Transaction Type
		STRING TRANTYPE = 2
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element =
		!   Description = Quantity Recorded
		GFLOAT QTY
		! Element = DATE
		!   Description = Transaction Date (YYYYMMDD)
		STRING TDATE = 8
		! Element =
		!   Description = Unit Price
		GFLOAT PRICE
		! Element =
		!   Description = Unit Cost
		GFLOAT COST
		! Element = DATE
		!   Description = Posting Date (YYYYMMDD)
		STRING POSTDATE = 8
		! Element = TIME
		!   Description = Posting Time (HHMMSS)
		STRING POSTTIME = 6
		! Element = BATCH
		!   Description = Batch number used for process (post,clos
		STRING BATCH = 6
		! Element =
		!   Description = Packing List Release Number
		STRING SHIPNO = 2
		! Element =
		!   Description = Make of Dealer's Model
		STRING MAKE = 10
		! Element =
		!   Description = Year of Make
		STRING YEAR = 4
		! Element =
		!   Description = Type of Make
		STRING MTYPE = 2
		! Element =
		!   Description = Size of Make
		STRING MSIZE = 4
		! Element =
		!   Description = Model Code
		STRING MODELCODE = 4
		! Element = REFNUM
		!   Description = Reference number
		STRING REFNUM = 8
		! Element = NOTES
		!   Description = Line notes
		STRING NOTES(1) = 40
		! Element =
		!   Description = Discount
		GFLOAT DISCOUNT
		! Element = PERIOD
		!   Description = Fiscal year (YYYY) and Cycle (PP)
		STRING PERIOD = 6
		! Element =
		!   Description = Serial Num
		STRING IDNUM = 10
	END RECORD
