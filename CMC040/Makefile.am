
AM_CPPFLAGS = -I ~/btran/lib
AM_LDFLAGS = -L ~/btran/lib -L .

lib_LIBRARIES = \
	libcmcfun.a

bin_PROGRAMS = \
	testbatch \
	tk_spec_checkline

# cmcfun
libcmcfun_a_SOURCES = \
	cmcfun/assg_makebatch.cc \
	cmcfun/assg_unmakebatch.cc \
       	cmcfun/date_daycode.cc \
       	cmcfun/date_invdcode.cc \
       	cmcfun/date_today.cc \
       	cmcfun/time_code.cc \
       	cmcfun/time_invcode.cc \
       	cmcfun/time_now.cc

# utl
testbatch_SOURCES = utl/testbatch.cc
testbatch_LDADD = -lcmcfun -lbtran -lvms

# tk programs
tk_spec_checkline_SOURCES = tk/tk_spec_checkline.cc
tk_spec_checkline_LDADD = -lbtran -lvms

