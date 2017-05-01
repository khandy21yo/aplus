	!
	! File Layout for: OE.OE_CONTROL on 21-May-01
	!
	! Sales Order Controlling File
	!

	RECORD OE_CONTROL_CDD
		! Element = ORDNUM
		!   Description = Control Order Number
		STRING ORDNUM = 10
		! Element = DATE
		!   Description = Last Purge Date (YYYYMMDD)
		STRING PURGDATE = 8
		! Element =
		!   Description = Activity status
		STRING STATUS_FLAG = 1
		! Element =
		!   Description = For the memo form to communicate with
		STRING LAST_MEMO = 8
		! Element =
		!   Description = For the invoice form to communicate with
		STRING LAST_INV = 8
		! Element =
		!   Description = Aging Days in the Period
		WORD AGEPER(4)
		! Element =
		!   Description = Name of Backorder Period
		STRING AGENAM(4) = 16
		! Element = PRICETYPE
		!   Description = Price type for Miscellaneous Charges
		STRING MISCTYPE = 2
		! Element = FLAG
		!   Description = Misc Charges Flag
		STRING MISCFLAG = 1
		! Element = FLAG
		!   Description = Display Price (Y/N)
		STRING DSPLPRICE = 1
		! Element = FLAG
		!   Description = Display Balances (Y/N)
		STRING DSPLQTY = 1
		! Element = PRICETYPE
		!   Description = List Price Type Code
		STRING LISTCODE = 2
	END RECORD
