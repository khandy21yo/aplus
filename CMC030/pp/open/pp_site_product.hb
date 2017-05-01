	!
	! File Layout for: PP.PP_SITE_PRODUCT on 21-May-01
	!
	! Products Available at this Site
	!

	RECORD PP_SITE_PRODUCT_CDD
		! Element =
		!   Description = Host Number
		STRING HOST = 4
		! Element =
		!   Description = site code
		STRING SITE = 4
		! Element =
		!   Description = Site Type
		STRING STYPE = 1
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element =
		!   Description = Federal INTP
		STRING FED_INTP = 1
		! Element =
		!   Description = Federal Rate
		GFLOAT FED_RATE
		! Element = ACCOUNT
		!   Description = Federal General Ledger Account Number
		STRING FED_ACCOUNT = 18
		! Element =
		!   Description = State INTP
		STRING STA_INTP = 1
		! Element =
		!   Description = State Rate
		GFLOAT STA_RATE
		! Element = ACCOUNT
		!   Description = State General Ledger Account Number
		STRING STA_ACCOUNT = 18
		! Element =
		!   Description = County INTP
		STRING COU_INTP = 1
		! Element =
		!   Description = County Rate
		GFLOAT COU_RATE
		! Element = ACCOUNT
		!   Description = County General Ledger Account Number
		STRING COU_ACCOUNT = 18
		! Element =
		!   Description = City INTP
		STRING CTY_INTP = 1
		! Element =
		!   Description = City Rate
		GFLOAT CTY_RATE
		! Element = ACCOUNT
		!   Description = City General Ledger Account Number
		STRING CTY_ACCOUNT = 18
		! Element =
		!   Description = Sales Tax INTP
		STRING STX_INTP = 1
		! Element =
		!   Description = Sales Tax Rate
		GFLOAT STX_RATE
		! Element = ACCOUNT
		!   Description = Sales Tax General Ledger Account Number
		STRING STX_ACCOUNT = 18
		! Element = DATE
		!   Description = Effective Date (YYYYMMDD)
		STRING EFFDATE = 8
	END RECORD
