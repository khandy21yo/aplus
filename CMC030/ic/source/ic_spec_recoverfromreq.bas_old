1	option size = (integer long, real gfloat)

	%include "source:[ic.open]ic_transaction.hb"
	map (ic_transaction) ic_transaction_cdd ic_transaction

	%include "source:[wp.open]wp_reqregister.hb"
	map (wp_reqregister) wp_reqregister_cdd wp_reqregister

	on error goto 19000


100	batch$ = "55NVGG"

110	yyyypp$ = "199403"
	%include "source:[ic.open]ic_transaction.MOD"

130	%include "source:[wp.open]wp_reqregister.opn"

1000	reset #wp_reqregister.ch%

1100	!
	! Search for a line with this batch number on it
	!
	get #wp_reqregister.ch%, regardless

	goto 1100 if wp_reqregister::batch <> batch$

1300	ic_transaction::product		= wp_reqregister::product
	ic_transaction::location	= wp_reqregister::location
	ic_transaction::trans_date	= wp_reqregister::trandate
	ic_transaction::primary_ref	= "01          " + wp_reqregister::lline
	ic_transaction::cross_ref	= ""
	ic_transaction::subaccount	= wp_reqregister::job
	ic_transaction::lot		= ""
	ic_transaction::stationman	= wp_reqregister::operator
	ic_transaction::type_a		= "IS"
	ic_transaction::quantity_a	= -wp_reqregister::qty
	ic_transaction::type_b		= ""
	ic_transaction::quantity_b	= 0.0
	ic_transaction::cost		= -wp_reqregister::amt
	ic_transaction::price		= 0.0
	ic_transaction::transacct	= ""
	ic_transaction::postdate	= wp_reqregister::postdate
	ic_transaction::posttime	= wp_reqregister::posttime
	ic_transaction::batch		= batch$

	print ic_transaction::product; " "; ic_transaction::subaccount

	put #ic_transaction.ch%

	goto 1100

9000	goto 32767

19000	select erl
	case 1100%
		resume 9000
 !	case 1200%
 !		resume 1100
	end select

	on error goto 0

32767	end
