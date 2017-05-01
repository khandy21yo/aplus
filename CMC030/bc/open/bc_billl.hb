	!
	! File Layout for: BC.BC_BILLL on 21-May-01
	!
	! Billing to Customer Billing Journal Line
	!

	RECORD BC_BILLL_CDD
		! Element =
		!   Description = Order Number
		STRING ORDER = 8
		! Element =
		!   Description = Line Number
		STRING LINENO = 4
		! Element =
		!   Description = Amount Ordered
		GFLOAT ORDAMT
		! Element =
		!   Description = Amount Shipped
		GFLOAT SHPAMT
		! Element =
		!   Description = Amount Back-Ordered
		GFLOAT BOAMT
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element =
		!   Description = Description
		STRING DESCR = 40
		! Element = UOM
		!   Description = Units of measure code
		STRING UNITME = 2
		! Element =
		!   Description = Unit Price
		GFLOAT UNIPRI
		! Element =
		!   Description = Total Amount
		GFLOAT AMOUNT
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCOUNT = 18
		! Element =
		!   Description = Line Type
		STRING LTYPE = 1
		! Element =
		!   Description = Tax Type
		STRING TAXTYP = 1
	END RECORD
