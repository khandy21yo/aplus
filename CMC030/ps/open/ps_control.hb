	!
	! File Layout for: PS.PS_CONTROL on 21-May-01
	!
	! Point Of Sale Controlling File
	!

	RECORD PS_CONTROL_CDD
		! Element = ORDNUM
		!   Description = Last Ticket Number
		STRING LAST_TICKET = 10
		! Element = DATE
		!   Description = Last Purge Date (YYYYMMDD)
		STRING PURGDATE = 8
		! Element =
		!   Description = Activity Status
		STRING STATUS_FLAG = 1
		! Element = PRICETYPE
		!   Description = Price type for miscellaneous charges
		STRING MISCTYPE = 2
		! Element = FLAG
		!   Description = Display Price
		STRING DSPLPRICE = 1
		! Element = FLAG
		!   Description = Display Balance
		STRING DSPLQTY = 1
		! Element = PRICETYPE
		!   Description = List Price type
		STRING LISTCODE = 2
		! Element =
		!   Description = Wildcard for cust type without balances
		STRING CUSBAL = 20
		! Element = YESNO
		!   Description = Is this item taxable?
		STRING MISCTAXABLE = 1
		! Element =
		!   Description = What customer tax types are exempt
		STRING MISCEXEMPT = 6
		! Element = PRICETYPE
		!   Description = Misc. (2) Price type
		STRING MISC2TYPE = 2
		! Element = YESNO
		!   Description = Is this item taxable?
		STRING MISC2TAXABLE = 1
		! Element =
		!   Description = What tax types are exempt
		STRING MISC2EXEMPT = 6
	END RECORD
