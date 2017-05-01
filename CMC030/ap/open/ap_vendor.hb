	!
	! File Layout for: AP.AP_VENDOR on 21-May-01
	!
	! Vendor Description
	!

	RECORD AP_VENDOR_CDD
		! Element =
		!   Description = Vendor number
		STRING VENNUM = 10
		! Element =
		!   Description = Vendor name
		STRING VENNAM = 40
		! Element =
		!   Description = Vendor remittance address line 1
		STRING ADD1 = 25
		! Element =
		!   Description = Vendor remittance address line 2
		STRING ADD2 = 21
		! Element =
		!   Description = Vendor remittance city
		STRING CITY = 15
		! Element =
		!   Description = Vendor remittance state
		STRING STATE = 2
		! Element =
		!   Description = Vendor remittance zip
		STRING ZIP = 10
		! Element =
		!   Description = Vendor remittance country
		STRING COUNTRY = 8
		! Element =
		!   Description = Vendor phone number
		STRING PHONE = 10
		! Element =
		!   Description = Vendor address to send PO, line 1
		STRING POADD1 = 25
		! Element =
		!   Description = Vendor address to send PO, line 2
		STRING POADD2 = 21
		! Element =
		!   Description = Vendor city to send PO
		STRING POCITY = 15
		! Element =
		!   Description = Vendor state to send PO
		STRING POSTATE = 2
		! Element =
		!   Description = Vendor Zip Code to send PO
		STRING POZIP = 10
		! Element =
		!   Description = Vendor Country to send PO
		STRING POCOUNTRY = 8
		! Element =
		!   Description = Vendor Phone number to send PO
		STRING POPHONE = 10
		! Element =
		!   Description = Purge (Y/N)
		STRING PURGE = 1
		! Element =
		!   Description =
		STRING FEDID = 13
		! Element =
		!   Description = 1099 Flag (Y/N)
		STRING FLG1099 = 1
		! Element =
		!   Description = Number of days until payment due
		WORD DUEDAYS
		! Element =
		!   Description = Date payment is due
		STRING DUEDATE = 2
		! Element =
		!   Description = Number of days until discount is lost
		WORD DISDAYS
		! Element =
		!   Description = Date discount is lost
		STRING DISDATE = 2
		! Element =
		!   Description = Discount percentage
		WORD DISCPER
		! Element =
		!   Description = Sort Key
		STRING ALPSRT = 15
	END RECORD
