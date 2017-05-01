	!
	! File Layout for: PO.PO_REG_LINE on 21-May-01
	!
	! Purchase Order Register Lines
	!

	RECORD PO_REG_LINE_CDD
		! Element = PO
		!   Description = Purchase order number
		STRING PO = 10
		! Element = LINE
		!   Description = Line
		STRING PO_LINE = 4
		! Element = VENDOR
		!   Description = Vendor Number
		STRING VENDOR = 10
		! Element = LOCATION
		!   Description = Location number
		STRING FROMLOCATION = 4
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element = UOM
		!   Description = Unit of measurement
		STRING UOM = 2
		! Element = DESCRIPTION
		!   Description = Description
		STRING DESCRIPTION = 40
		! Element = PO_TYPE
		!   Description = Purchase Order Type
		STRING PO_TYPE = 2
		! Element =
		!   Description = Open/Closed Flag
		STRING OPEN_CLOSE = 1
		! Element = DATE
		!   Description = Order Date (YYYYMMDD)
		STRING ORDDATE = 8
		! Element = BATCH
		!   Description = Batch number used for process (post,clos
		STRING BATCH = 6
		! Element = PERIOD
		!   Description = Fiscal year (YYYY) and Cycle (PP)
		STRING PERIOD = 6
	END RECORD
