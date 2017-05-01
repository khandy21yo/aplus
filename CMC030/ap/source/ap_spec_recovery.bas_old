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


130	ap_temp.ch% = 13%

	open "ap_temp.tmp" as file ap_temp.ch%, &
		organization indexed fixed, &
		map ap_temp, &
		primary key ap_temp_rfastr, &
		access modify, allow none, &
		temporary


	on error goto 19000

1000	!*******************************************************************
	! Main loop through file
	!*******************************************************************

	print "Starting key 0"
	reset #ap_open.ch%

1010	get #ap_open.ch%, regardless

1015	this_vendor$ = ap_open::vennum + ""
	ap_temp_rfa = getrfa(ap_open.ch%)
	put #ap_open.ch_new%

1020	put #ap_temp.ch%

	goto 1010

1100	!
	! Try to get past error
	!
	get #ap_open.ch%, key #0% gt this_vendor$, regardless
	goto 1015

1110	get #ap_vendor.ch%, key #0% gt this_vendor$, regardless
	this_vendor$ = ap_vendor::vennum

	goto 1100

1200	!*******************************************************************
	! Now try alternate keys
	!*******************************************************************

1210	for i% = 1% to 2%

		print "Starting key"; i%

		reset #ap_open.ch%, key #i%

1220		get #ap_open.ch%

1225		select i%
			case 1%
				this_vendor$ = ap_open::vennum+ap_open::invnum
			case 2%
				this_vendor$ = ap_open::batch
		end select

1230		ap_temp_rfa = getrfa(ap_open.ch%)
		get #ap_temp.ch%, key#0% eq ap_temp_rfastr

		goto 1220

1240		put #ap_open.ch_new%

1250		put #ap_temp.ch%

		print "!One more revocered"

		GOTO 1220

1290	goto 1390

1300	!
	! Try to get past error
	!
	get #ap_open.ch%, key #0% gt this_vendor$, regardless
	goto 1225

1390	next i%

	goto 32767


19000	!*******************************************************************
	! Trap errors
	!*******************************************************************
	SELECT ERL
	CASE 1010%
		PRINT ERL;"Error"; ERR; "at #"; THIS_VENDOR$
		RESUME 1200 IF ERR = 11%
		RESUME 1100

	CASE 1100%
		PRINT ERL;"Error"; ERR; "at #"; THIS_VENDOR$
		RESUME 1110

	CASE 1110%
		PRINT ERL;"Error"; ERR; "at #"; THIS_VENDOR$
		RESUME 1200

	CASE 1220%
		PRINT ERL;"Error"; ERR; "at #"; THIS_VENDOR$
		RESUME 1300

	CASE 1230%
		RESUME 1240

	CASE 1300%
		RESUME 1290

	END SELECT

	ON ERROR GOTO 0

32767	END
