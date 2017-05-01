	!
	! File Layout for: SMG.SMG_OPN on 11-Mar-99
	!
	! PASS OPEN FILE DATA BETWEEN PROGRAMS
	!

	RECORD SMG_OPN_CDD
		! Element =
		!   Description = File organization
		STRING ORGNIZATION = 30
		! Element =
		!   Description = File structure
		STRING STRCTURE = 30
		! Element =
		!   Description = File open name
		STRING FILE_NAME = 60
		! Element =
		!   Description = File extension
		STRING EXTENSION = 10
		! Element =
		!   Description = Keys
		STRING KEYS(31) = 255
		! Element =
		!   Description = Number of keys
		WORD KEYS_NUM
	END RECORD
