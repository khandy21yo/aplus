
AM_CPPFLAGS = -I ~/btran/lib
AM_LDFLAGS = -L ~/btran/lib -L .

lib_LIBRARIES = \
	libcmcfun.a \
	libsmg.a

bin_PROGRAMS = \
	dbtest \
	testbatch \
	tk_spec_inlibbat \
	tk_spec_checkline \
	ut_spec_amortization

# cmcfun
libcmcfun_a_SOURCES = \
	cmcfun/assg_makebatch.cc \
	cmcfun/assg_unmakebatch.cc \
	cmcfun/comp_string.cc \
	cmcfun/database.cc \
       	cmcfun/date_daycode.cc \
       	cmcfun/date_invdcode.cc \
       	cmcfun/date_today.cc \
	cmcfun/dspl_scroll.cc \
	cmcfun/entr_3badkey.cc \
	cmcfun/entr_3enter.cc \
	cmcfun/entr_3entrystring.cc \
	cmcfun/entr_3message.cc \
	cmcfun/entr_3option.cc \
	cmcfun/entr_4entry.cc \
	cmcfun/entr_4specialkeys.cc \
	cmcfun/entr_macro.cc \
	cmcfun/find_3printgroupitem.cc \
       	cmcfun/find_file.cc \
	cmcfun/find_fileexists.cc \
       	cmcfun/func_4scoseq.cc \
       	cmcfun/func_round.cc \
	cmcfun/help_34message.cc \
       	cmcfun/libr_3insert.cc \
	cmcfun/libr_digsr.cc \
	cmcfun/menu_3interrupt.cc \
	cmcfun/outp_3writestructure.cc \
	cmcfun/outp_createstr.cc \
	cmcfun/outp_initform.cc \
	cmcfun/outp_initfromfile.cc \
	cmcfun/outp_initstructure.cc \
	cmcfun/outp_line.cc \
	cmcfun/read_35set.cc \
	cmcfun/read_3broadcast.cc \
       	cmcfun/read_initialize.cc \
       	cmcfun/read_sysjob.cc \
       	cmcfun/read_syspn.cc \
	cmcfun/subr_3spawn.cc \
       	cmcfun/time_code.cc \
       	cmcfun/time_invcode.cc \
       	cmcfun/time_now.cc

libsmg_a_SOURCES = \
	smg/lib_set_symbol.cc \
	smg/libr_get_record.cc \
	smg/lbr_lookup_key.cc \
	smg/lbr_open.cc \
	smg/smg_begin_display_update.cc \
	smg/smg_change_pbd_characteristics.cc \
	smg/smg_create_pasteboard.cc \
	smg/smg_create_virtual_display.cc \
	smg/smg_create_virtual_keyboard.cc \
	smg/smg_cursor_column.c \
	smg/smg_cursor_row.c \
	smg/smg_delete_virtual_display.cc \
	smg/smg_draw_line.cc \
	smg/smg_end_display_update.cc \
	smg/smg_erase_display.cc \
	smg/smg_erase_line.cc \
	smg/smg_flush_buffer.cc \
	smg/smg_get_broadcast_message.cc \
	smg/smg_get_display_attr.cc \
	smg/smg_label_botder.cc \
	smg/smg_paste_virtual_display.cc \
	smg/smg_pop_virtual_display.cc \
	smg/smg_put_chars.cc \
	smg/smg_put_virtual_display_encoded.cc \
	smg/smg_read_keystroke.cc \
	smg/smg_read_string.cc \
	smg/smg_repaint_screen.cc \
	smg/smg_ring_bell.cc \
	smg/smg_set_broadcast_trapping \
	smg/smg_sewt_cursor_abs.cc \
	smg/smg_set_cursor_mode.cc \
	smg/smg_set_term_characteristics.cc

# dbtest
dbtest_SOURCES = dbtest.cc
dbtest_LDADD = -lpq

# utl
testbatch_SOURCES = utl/testbatch.cc
testbatch_LDADD = libcmcfun.a -lbtran -lvms

# tk programs
tk_spec_checkline_SOURCES = tk/tk_spec_checkline.cc
tk_spec_checkline_LDADD = -lbtran -lvms

tk_spec_inlibbat_SOURCES = tk/tk_spec_inlibbat.cc
tk_spec_inlibbat_LDADD = libcmcfun.a -lbtran -lvms -lpq

# utl programs
ut_spec_amortization_SOURCES = utl/ut_spec_amortization.cc
ut_spec_amortization_LDADD = libcmcfun.a libsmg.a -lbtran -lvms -lpq

