	!
	! File Layout for: TV.TV_COMMER on 21-May-01
	!
	! Commercial Order Header
	!

	RECORD TV_COMMER_CDD
		! Element = TV_FRMNUM
		!   Description = Form Number
		STRING FRMNUM = 8
		! Element = TV_CUSNUM
		!   Description = Customer Number
		STRING CUSNUM = 10
		! Element =
		!   Description = Commercial CLASS (PO-political, etc.)
		STRING CLASS = 4
		! Element =
		!   Description = Source (LV-live, etc.)
		STRING SOURCE = 4
		! Element =
		!   Description = Priority (1-5)
		WORD PRIORITY
		! Element =
		!   Description = Start date
		STRING START_DATE = 8
		! Element =
		!   Description = End date
		STRING END_DATE = 8
		! Element =
		!   Description = Cancelled (Y,N)
		STRING CANCELLED = 1
		! Element = TV_CUSNUM
		!   Description = Agency Number
		STRING AGENCY_NUM = 10
		! Element =
		!   Description = Agency Commission Percentage
		WORD AGENCY_PCT
		! Element =
		!   Description = Date billed
		STRING DATE_BILLED = 8
		! Element =
		!   Description = Match code
		STRING MATCH = 6
		! Element = TV_CUSNUM
		!   Description = Rep number
		STRING REP_NUM = 10
		! Element =
		!   Description = Rep commission rate
		WORD REP_PCT
		! Element = TV_SALNUM
		!   Description = Salesperson number
		STRING SALES_NUM = 10
		! Element =
		!   Description = Salesman Commission
		WORD SALES_PCT
		! Element = NAME
		!   Description = Name
		STRING CONTACT = 25
		! Element =
		!   Description = Billing type
		STRING BILL_TYPE = 2
		! Element =
		!   Description = Spot seperation
		STRING SPOT_SEP = 6
		! Element =
		!   Description = Product Seperation
		STRING PROD_SEP = 6
		! Element =
		!   Description = PO Number
		STRING PO = 10
		! Element =
		!   Description = Bill flag. 1-Normal. 2-Flat rate.
		STRING BILL_FLAG = 1
		! Element =
		!   Description = Confirmation printed (Y/N)
		STRING CONFIRM = 1
		! Element =
		!   Description = Confirmation number
		WORD MOD_NUM
		! Element =
		!   Description = Single Conflict code for this order
		STRING CONFLICT = 8
		! Element =
		!   Description = Description
		STRING DESCR = 30
	END RECORD
