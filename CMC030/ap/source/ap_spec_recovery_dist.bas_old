1	!
	! Special program to recover data from one of Coastals
	! trashed files.
	!
	option size = (integer long, real gfloat)

	%include "source:[ap.open]AP_OPEN_DIST.hb"
	map (AP_OPEN_DIST) AP_OPEN_DIST_cdd AP_OPEN_DIST

110	!
	! Open old AP_OPEN_DIST file
	!
	AP_OPEN_DIST.CH% = 11%
	AP_OPEN_DIST.DEV$ = ""

	AP_OPEN_DIST.NAME$ = AP_OPEN_DIST.DEV$+"AP_OPEN_DIST.LED_BAD"

	OPEN AP_OPEN_DIST.NAME$ FOR INPUT AS FILE AP_OPEN_DIST.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AP_OPEN_DIST, &
		PRIMARY KEY &
		( &
			AP_OPEN_DIST::TRANKEY, &
			AP_OPEN_DIST::SLINE &
		), &
		ALTERNATE KEY &
			AP_OPEN_DIST::BTHNUM &
			DUPLICATES CHANGES, &
		ACCESS READ, ALLOW MODIFY

120	!
	! Open new AP_OPEN_DIST file
	!

	AP_OPEN_DIST.CH_NEW% = 12%
	AP_OPEN_DIST.DEV_NEW$ = ""

	AP_OPEN_DIST.NAME$ = AP_OPEN_DIST.DEV$+"AP_OPEN_DIST.LED"

	OPEN AP_OPEN_DIST.NAME$ AS FILE AP_OPEN_DIST.CH_NEW%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AP_OPEN_DIST, &
		BUFFER 32%, &
		PRIMARY KEY &
		( &
			AP_OPEN_DIST::TRANKEY, &
			AP_OPEN_DIST::SLINE &
		), &
		ALTERNATE KEY &
			AP_OPEN_DIST::BTHNUM &
			DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW MODIFY


	on error goto 19000

1000	!*******************************************************************
	! Main loop through file
	!*******************************************************************

	print "Starting"
	reset #AP_OPEN_DIST.ch%

1010	get #AP_OPEN_DIST.ch%, regardless

1015	this_vendor$ = AP_OPEN_DIST::trankey + ""
	put #AP_OPEN_DIST.ch_new%

1020	goto 1010

1100	!
	! Try to get past error
	!
	this_vendor$ = num1$(val(this_vendor$) + 1%)
	print ".";
	get #AP_OPEN_DIST.ch%, key #0% gt this_vendor$, regardless
	print this_vendor$
	goto 1015

1200	close ap_open_dist.ch%
	close ap_open_dist.ch_new%

	goto 32767

19000	!*******************************************************************
	! Trap errors
	!*******************************************************************
	SELECT ERL
		CASE 1010%
			print erl;"Error"; err; "at #"; this_vendor$
			resume 1200 if err = 11%
			RESUME 1100

		case 1100%
			resume 1100

	END SELECT

	ON ERROR GOTO 0

32767	END
