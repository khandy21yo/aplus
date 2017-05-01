	!
	! File Layout for: OE.OE_REGLINE on 21-May-01
	!
	! Sales Order Register Line File
	!

	RECORD OE_REGLINE_CDD
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
		!   Description = Discount Percentage
		GFLOAT DISCOUNT
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
		!   Description = Promo Amount
		GFLOAT PROMO
		! Element = REFNUM
		!   Description = Reference Number,Invoice Number
		STRING REFNUM = 8
		! Element =
		!   Description = Miscellaneous Charges
		GFLOAT MISCH
		! Element = PERIOD
		!   Description = Fiscal year (YYYY) and Cycle (PP)
		STRING PERIOD = 6
		! Element = NOTES
		!   Description = Notes
		STRING NOTES1 = 40
		! Element = NOTES
		!   Description = Notes
		STRING NOTES2 = 30
		! Element = SUBACCT
		!   Description = Sub account (job number)/Serail Number
		STRING SUBACCT = 10
		! Element =
		!   Description = Miscellaneous Charges (2)
		GFLOAT MISCH2
	END RECORD
