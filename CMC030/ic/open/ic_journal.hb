	!
	! File Layout for: IC.IC_JOURNAL on 21-May-01
	!
	! Inventory Journal
	!

	RECORD IC_JOURNAL_CDD
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element = DATE
		!   Description = Date (MMDDYYYY)
		STRING TRANS_DATE = 8
		! Element = REF
		!   Description = Primary Reference
		STRING PRIMARY_REF = 8
		! Element = REF
		!   Description = Secondary Reference.
		STRING SECONDARY_REF = 8
		! Element = XREF
		!   Description = Cross reference
		STRING CROSS_REF = 10
		! Element =
		!   Description = Subaccount
		STRING SUBACCOUNT = 10
		! Element =
		!   Description = Salesman,Operator etc
		STRING STATIONMAN = 10
		! Element =
		!   Description = Transaction type
		STRING TYPE_A = 2
		! Element = UNIT
		!   Description = Unit amount
		GFLOAT QUANTITY_A
		! Element =
		!   Description = Transaction type
		STRING TYPE_B = 2
		! Element = UNIT
		!   Description = Unit amount
		GFLOAT QUANTITY_B
		! Element =
		!   Description = Unit Cost
		GFLOAT COST
		! Element = ACCOUNT
		!   Description = Trans General ledger account number
		STRING EXPACCT = 18
		! Element = LOCATION
		!   Description = Location number
		STRING TOLOCATION = 4
	END RECORD
