1	option size = (integer long, real gfloat)

	%include "source:[ic.open]ic_transaction.hb"
	map (ic_transaction) ic_transaction_cdd ic_transaction

	%include "source:[sb.open]sb_subaccount.hb"
	map (sb_subaccount) sb_subaccount_cdd sb_subaccount

	%include "source:[jc.open]jc_job.hb"
	map (sb_subaccount) jc_job_cdd jc_job

	%include "source:[wp.open]wp_regline.hb"
	map (wp_regline) wp_regline_cdd wp_regline

	on error goto 19000

100	batch$ = "55NVGG"

110	yyyypp$ = "199403"
	%include "source:[ic.open]ic_transaction.MOD"

120	%include "source:[sb.open]sb_subaccount.opn"

130	%include "source:[wp.open]wp_regline.opn"

1000	reset #wp_regline.ch%

1100	!
	! Search for a line with this batch number on it
	!
	get #wp_regline.ch%, regardless

	goto 1100 if wp_regline::batch <> batch$

1200	get #sb_subaccount.ch%, &
		key #0% eq "J" + wp_regline::job, &
		regardless

1300	ic_transaction::product		= wp_regline::itemcode
	ic_transaction::location	= jc_job::location
	ic_transaction::trans_date	= wp_regline::start_date
	ic_transaction::primary_ref	= ""
	ic_transaction::cross_ref	= ""
	ic_transaction::subaccount	= wp_regline::job
	ic_transaction::lot		= ""
	ic_transaction::stationman	= jc_job::operator
	ic_transaction::type_a		= "MA"
	ic_transaction::quantity_a	= wp_regline::qty
	ic_transaction::type_b		= "WO"
	ic_transaction::quantity_b	= 0.0
	ic_transaction::cost		= wp_regline::cost
	ic_transaction::price		= 0.0
	ic_transaction::transacct	= ""
	ic_transaction::postdate	= wp_regline::post_date
	ic_transaction::posttime	= wp_regline::post_time
	ic_transaction::batch		= batch$

	print ic_transaction::product; " "; ic_transaction::subaccount

	put #ic_transaction.ch%

	goto 1100

9000	goto 32767

19000	select erl
	case 1100%
		resume 9000
	case 1200%
		resume 1100
	end select

	on error goto 0

32767	end
