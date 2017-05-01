1	!++
	! Author:
	!	10/22/91 - Kevin Handy
	!
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--

	option size = (integer long, real gfloat)

	%include "source:[pd.open]pd_product.hb"
	map (pd_product) pd_product_cdd pd_product

	%include "source:[ic.open]ic_binmap.hb"
	map (ic_binmap) ic_binmap_cdd ic_binmap


100	%include "source:[pd.open]pd_product.opn"

110	%include "source:[ic.open]ic_binmap.cre"

200	input "Create location"; location$

	ic_binmap::location = location$
	ic_binmap::bin(i%) = "" for i%=0% to 2%
	ic_binmap::safety = 0.0
	ic_binmap::maxlevel = 0.0
	ic_binmap::abc = ""
	ic_binmap::cyclemap = string$(8%, 0%)

210	reset #pd_product.ch%

	on error goto 19000

300	get #pd_product.ch%

310	ic_binmap::product = pd_product::product_num

	put #ic_binmap.ch%

	print ic_binmap::product

	goto 300

900	close ic_binmap.ch%

	close pd_proguct.ch%

	goto 32767

19000	select erl

	case 300%

		resume 900

	case 310%

		resume 300

	end select

	on error goto 0

32767	end
