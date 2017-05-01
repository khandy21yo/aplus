	!
	! File Layout for: PO.PO_ARCHIVE_SUB_LINE on 21-May-01
	!
	! Purchase Order Archive Sub Line
	!

	RECORD PO_ARCHIVE_SUB_LINE_CDD
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
		!   Description = Our Quanity
		GFLOAT OUR_QTY
		! Element =
		!   Description = Vendor Quanity
		GFLOAT VEN_QTY
		! Element =
		!   Description = Expected Price
		GFLOAT VEN_RATE
		! Element = DATE
		!   Description = Expected Date (YYYYMMDD)
		STRING RECEIVEDATE = 8
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
