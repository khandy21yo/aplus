	!
	! File Layout for: PO.PO_ARCHIVE_LINE on 21-May-01
	!
	! Purchase Order Archive Line
	!

	RECORD PO_ARCHIVE_LINE_CDD
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
		STRING OUR_PRODUCT = 14
		! Element = UOM
		!   Description = Unit of measurement
		STRING OUR_UOM = 2
		! Element =
		!   Description = Our Conversion Factor
		GFLOAT OUR_FACTOR
		! Element = PRODUCT
		!   Description = Product Number
		STRING VEN_PRODUCT = 14
		! Element = DESCRIPTION
		!   Description = Description
		STRING VEN_DESCRIPTION = 40
		! Element = UOM
		!   Description = Unit of measurement
		STRING VEN_UOM = 2
		! Element =
		!   Description = Vendors conversion factor
		GFLOAT VEN_FACTOR
		! Element =
		!   Description = Discount percentage
		GFLOAT VEN_DISCOUNT
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING GL_ACCOUNT = 18
		! Element = SUBACCT
		!   Description = Sub account (job number)
		STRING SUBACCT = 10
		! Element = PO_TYPE
		!   Description = Purchase Order Type
		STRING PO_TYPE = 2
		! Element =
		!   Description = Open/Closed Flag
		STRING OPEN_CLOSE = 1
	END RECORD
