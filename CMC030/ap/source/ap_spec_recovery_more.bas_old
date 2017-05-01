1	!
	! Special program to recover data from one of Coastals
	! trashed files.
	!
	option size = (integer long, real gfloat)

	%include "source:[ap.open]ap_vendor.hb"
	map (ap_vendor) ap_vendor_cdd ap_vendor

	%include "source:[ap.open]ap_open.hb"
	map (ap_open) ap_open_cdd ap_open

	map (ap_temp) &
		rfa ap_temp_rfa
	map (ap_temp) &
		string ap_temp_rfastr=6%

100	!
	! Open vendor file
	!
	AP_VENDOR.CH% = 10%
	AP_VENDOR.DEV$ = ""

	AP_VENDOR.NAME$ = AP_VENDOR.DEV$ + "AP_VENDOR.MAS"

	OPEN AP_VENDOR.NAME$ FOR INPUT AS FILE AP_VENDOR.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AP_VENDOR, &
		PRIMARY KEY &
			AP_VENDOR::VENNUM, &
		ALTERNATE KEY &
			AP_VENDOR::VENNAM &
			DUPLICATES CHANGES, &
		ALTERNATE KEY &
			AP_VENDOR::ALPSRT &
			DUPLICATES CHANGES, &
		ALTERNATE KEY &
			AP_VENDOR::STATE &
			DUPLICATES CHANGES, &
		ALTERNATE KEY &
			AP_VENDOR::ZIP &
			DUPLICATES CHANGES, &
		ACCESS READ, ALLOW MODIFY

110	!
	! Open old AP_OPEN file
	!
	AP_OPEN.CH% = 11%
	AP_OPEN.DEV$ = ""

	AP_OPEN.NAME$ = AP_OPEN.DEV$+"AP_OPEN.LED_BAD"

	OPEN AP_OPEN.NAME$ FOR INPUT AS FILE AP_OPEN.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AP_OPEN, &
		PRIMARY KEY &
		( &
			AP_OPEN::VENNUM, &
			AP_OPEN::TRANKEY &
		)	DUPLICATES, &
		ALTERNATE KEY &
		( &
			AP_OPEN::VENNUM, &
			AP_OPEN::INVNUM &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
			AP_OPEN::BATCH &
			DUPLICATES CHANGES, &
		ACCESS READ, ALLOW MODIFY


120	!
	! Open new AP_OPEN file
	!
	!======================================================================
	! AP_OPEN file (create, open read/write)
	!======================================================================

	AP_OPEN.CH_new% = 12%
	AP_OPEN.DEV$ = ""

	AP_OPEN.NAME$ = AP_OPEN.DEV$+"AP_OPEN.LED"

	OPEN AP_OPEN.NAME$ AS FILE AP_OPEN.CH_new%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AP_OPEN, &
		BUFFER 32%, &
		PRIMARY KEY &
		( &
			AP_OPEN::VENNUM, &
			AP_OPEN::TRANKEY &
		)	DUPLICATES, &
		ALTERNATE KEY &
		( &
			AP_OPEN::VENNUM, &
			AP_OPEN::INVNUM &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
			AP_OPEN::BATCH &
			DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY


	on error goto 19000

1000	!*******************************************************************
	! Main loop through file
	!*******************************************************************

	!
	! Scan through open file, trying to recover more information
	! than what we currently have
	!
	START_NUMBER% = 517317%
	END_NUMBER% = 999999%


1010	GOTO 1100 IF START_NUMBER%+10% > END_NUMBER%

	MID_NUMBER% = (START_NUMBER% + END_NUMBER%) / 2%

	PRINT START_NUMBER%, MID_NUMBER%, END_NUMBER%

	GET #AP_OPEN.CH%, KEY#0% GE "1019      " + NUM1$(MID_NUMBER%)

	END_NUMBER% = MID_NUMBER%

	GOTO 1010

1100	GET #AP_OPEN.CH%, KEY#0% GE "1019      " + NUM1$(END_NUMBER%)

1110	WHILE AP_OPEN::VENNUM = "1019      "

		PRINT "!";

		PUT #AP_OPEN.CH_NEW%

		GET #AP_OPEN.CH%

	NEXT

1190	CLOSE AP_OPEN.CH_NEW%

	GOTO 32767

19000	!*******************************************************************
	! Trap errors
	!*******************************************************************
	SELECT ERL
		CASE 1010%
			START_NUMBER% = MID_NUMBER%
			RESUME 1010

	END SELECT

	ON ERROR GOTO 0

32767	END
