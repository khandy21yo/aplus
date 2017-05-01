	!
	! File Layout for: RM.RM_CONTROL on 21-May-01
	!
	! Restaurant Control File
	!

	RECORD RM_CONTROL_CDD
		! Element = TRANSTYPE
		!   Description = Transaction type code for issue
		STRING TTISSUE = 2
		! Element = TRANSTYPE
		!   Description = Transaction type code fot receiver
		STRING TTREC = 2
		! Element = TRANSTYPE
		!   Description = Transaction type code for promotionals
		STRING TTPROM = 2
		! Element = TRANSTYPE
		!   Description = Transaction type code for employee meal
		STRING TTEMP = 2
		! Element = TRANSTYPE
		!   Description = Transaction type code for sales units
		STRING TTSALES = 2
		! Element = TRANSTYPE
		!   Description = Transaction type code for waste
		STRING TTWASTE = 2
		! Element = PCTYPE
		!   Description = Price type code for menu price
		STRING PRCMENU = 2
		! Element = PCTYPE
		!   Description = Price type code for indented menu price
		STRING INDMENU = 2
		! Element = PCTYPE
		!   Description = Price type code for employee menu price
		STRING PRCEMP = 2
		! Element = CONTROLFLAG
		!   Description = Status flag in the control files
		STRING CONTROLFLAG = 1
	END RECORD
