	!
	! File Layout for: PO.PO_REG_SUB_LINE on 21-May-01
	!
	! Purcahse Order Segister Sub-Line
	!

	RECORD PO_REG_SUB_LINE_CDD
		! Element = PO
		!   Description = Purchase order number
		STRING PO = 10
		! Element = LINE
		!   Description = Line
		STRING PO_LINE = 4
		! Element =
		!   Description = Action
		STRING PO_ACTION = 2
		! Element = DATE
		!   Description = Date (YYYYMMDD)
		STRING ACTION_DATE = 8
		! Element =
		!   Description = Quantity
		GFLOAT QTY
		! Element =
		!   Description = Price
		GFLOAT PRICE
		! Element = SUBACCT
		!   Description = Sub account (job number)
		STRING SUBACCT = 10
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCOUNT = 18
		! Element = BATCH
		!   Description = Batch number used for process (post,clos
		STRING BATCH = 6
		! Element = POSTDATE
		!   Description = Date of posting (YYYYMMDD)
		STRING POSTDATE = 8
		! Element = POSTTIME
		!   Description = Time of posting
		STRING POSTTIME = 6
	END RECORD
