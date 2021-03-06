	%NOLIST
	%NOCROSS

	!
	! Let's try to use the (new) include files that
	! are now supplied with VAX/BASIC instead of rolling
	! our own. (04/08/99)
	!
	%IF 1
	%THEN

	%INCLUDE "SMG$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "$SMGDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "$SMGMSG" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "$SMGTRMPTR" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	%ELSE

	!
	! Declare External SMG$ functions
	!
	!	(Covered by SMG$ROUTINES)
	!
	EXTERNAL LONG FUNCTION				&
		SMG$ADD_KEY_DEF,			&
		SMG$ALLOW_ESCAPE,			&
		SMG$BEGIN_DISPLAY_UPDATE,		&
		SMG$BEGIN_PASTEBOARD_UPDATE,		&
		SMG$CANCEL_INPUT,			&
		SMG$CHANGE_PBD_CHARACTERISTICS,		&
		SMG$CHANGE_RENDITION,			&
		SMG$CHANGE_VIRTUAL_DISPLAY,		&
		SMG$CHECK_FOR_OCCLUSION,		&
		SMG$CONTROL_MODE,			&
		SMG$COPY_VIRTUAL_DISPLAY,		&
		SMG$CREATE_KEY_TABLE,			&
		SMG$CREATE_PASTEBOARD,			&
		SMG$CREATE_VIRTUAL_DISPLAY,		&
		SMG$CREATE_VIRTUAL_KEYBOARD,		&
		SMG$CURSOR_COLUMN,			&
		SMG$CURSOR_ROW,				&
		SMG$DEFINE_KEY,				&
		SMG$DELETE_KEY,				&
		SMG$DELETE_CHARS,			&
		SMG$DELETE_KEY_DEF,			&
		SMG$DELETE_LINE,			&
		SMG$DELETE_PASTEBOARD,			&
		SMG$DELETE_VIRTUAL_DISPLAY,		&
		SMG$DELETE_VIRTUAL_KEYBOARD,		&
		SMG$DISABLE_BROADCAST_TRAPPING,		&
		SMG$DISABLE_UNSOLICITED_INPUT,		&
		SMG$DRAW_LINE,				&
		SMG$DRAW_RECTANGLE,			&
		SMG$ENABLE_UNSOLICITED_INPUT,		&
		SMG$END_DISPLAY_UPDATE,			&
		SMG$END_PASTEBOARD_UPDATE,		&
		SMG$ERASE_CHARS,			&
		SMG$ERASE_DISPLAY,			&
		SMG$ERASE_LINE,				&
		SMG$ERASE_PASTEBOARD,			&
		SMG$FIND_CURSOR_DISPLAY,		&
		SMG$FLUSH_BUFFER,			&
		SMG$GET_BROADCAST_MESSAGE,		&
		SMG$GET_CHAR_AT_PHYSICAL_CURSOR,	&
		SMG$GET_DISPLAY_ATTR,			&
		SMG$GET_KEY_DEF,			&
		SMG$GET_KEYBOARD_ATTRIBUTES,		&
		SMG$GET_NUMERIC_DATA,			&
		SMG$GET_PASTEBOARD_ATTRIBUTES,		&
		SMG$GET_PASTING_INFO,			&
		SMG$HOME_CURSOR,			&
		SMG$INSERT_CHARS,			&
		SMG$INSERT_LINE,			&
		SMG$INVALIDATE_DISPLAY,			&
		SMG$LABEL_BORDER,			&
		SMG$LIST_KEY_DEFS,			&
		SMG$LOAD_KEY_DEFS,			&
		SMG$MOVE_VIRTUAL_DISPLAY,		&
		SMG$PASTE_VIRTUAL_DISPLAY,		&
		SMG$POP_VIRTUAL_DISPLAY,		&
		SMG$PUT_CHARS,				&
		SMG$PUT_CHARS_HIGHWIDE,			&
		SMG$PUT_CHARS_WIDE,			&
		SMG$PUT_LINE,				&
		SMG$PUT_LINE_HIGHWIDE,			&
		SMG$PUT_LINE_WIDE,			&
		SMG$PUT_PASTEBOARD,			&
		SMG$PUT_VIRTUAL_DISPLAY_ENCODED,	&
		SMG$PUT_WITH_SCROLL,			&
		SMG$READ_COMPOSED_LINE,			&
		SMG$READ_FROM_DISPLAY,			&
		SMG$READ_KEYSTROKE,			&
		SMG$READ_STRING,			&
		SMG$READ_VERIFY,			&
		SMG$REPAINT_LINE,			&
		SMG$REPAINT_SCREEN,			&
		SMG$REPASTE_VIRTUAL_DISPLAY,		&
		SMG$REPLACE_INPUT_LINE,			&
		SMG$RESTORE_PHYSICAL_SCREEN,		&
		SMG$RETURN_CURSOR_POS,			&
		SMG$RETURN_INPUT_LINE,			&
		SMG$RING_BELL,				&
		SMG$SAVE_PHYSICAL_SCREEN,		&
		SMG$SCROLL_DISPLAY_AREA,		&
		SMG$SET_BROADCAST_TRAPPING,		&
		SMG$SET_CURSOR_ABS,			&
		SMG$SET_CURSOR_MODE,			&
		SMG$SET_CURSOR_REL,			&
		SMG$SET_DEFAULT_STATE,			&
		SMG$SET_DISPLAY_SCROLL_REGION,		&
		SMG$SET_KEYPAD_MODE,			&
		SMG$SET_OUT_OF_BAND_ASTS,		&
		SMG$SET_PHYSICAL_CURSOR,		&
		SMG$SET_TERM_CHARACTERISTICS,		&
		SMG$SNAPSHOT,				&
		SMG$UNPASTE_VIRTUAL_DISPLAY

	!
	! SMG Comstants
	!
	!	Covered by $SMGDEF and $SMGTRMPTR
	!
	DECLARE LONG CONSTANT &
		SMG$K_ADVANCED_VIDEO		= 1, &
		SMG$K_ANSI_CRT			= 2, &
		SMG$K_AUTO_MARGIN		= 3, &
		SMG$K_BACKSPACE			= 4, &
		SMG$K_BLOCK_MODE		= 5, &
		SMG$K_DEC_CRT			= 6, &
		SMG$K_EDIT			= 7, &
		SMG$K_EIGHT_BIT			= 8, &
		SMG$K_FULLDUP			= 9, &
		SMG$K_IGNORE_NEWLINE		= 10, &
		SMG$K_INSERT_MODE_NULLS		= 11, &
		SMG$K_LOWERCASE			= 12, &
		SMG$K_NO_ERASE			= 13, &
		SMG$K_NO_SCROLL			= 14, &
		SMG$K_OVERSTRIKE		= 15, &
		SMG$K_PRINTER_PORT		= 16, &
		SMG$K_REGIS			= 17, &
		SMG$K_SCOPE			= 18, &
		SMG$K_SIXEL_GRAPHICS		= 19, &
		SMG$K_SOFT_CHARACTERS		= 20, &
		SMG$K_PHYSICAL_TABS		= 21, &
		SMG$K_PHYSICAL_FF		= 22, &
		SMG$K_UNDERLINE			= 23, &
		SMG$K_CURSOR_REPORT_ANSI	= 24, &
		SMG$K_DEC_CRT_2			= 25, &
		SMG$K_DEC_CRT_3			= 26, &
		SMG$K_SET_CURSOR_COL_ROW	= 27, &
		SMG$K_PRIVATE_BOO_1		= 211, &
		SMG$K_PRIVATE_BOO_2		= 212, &
		SMG$K_PRIVATE_BOO_3		= 213, &
		SMG$K_PRIVATE_BOO_4		= 214, &
		SMG$K_PRIVATE_BOO_5		= 215, &
		SMG$K_PRIVATE_BOO_6		= 216, &
		SMG$K_PRIVATE_BOO_7		= 217, &
		SMG$K_PRIVATE_BOO_8		= 218, &
		SMG$K_PRIVATE_BOO_9		= 219, &
		SMG$K_PRIVATE_BOO_10		= 220, &
		SMG$K_MAX_BOOLEAN_CODE		= 220, &
		SMG$K_COLUMNS			= 221, &
		SMG$K_CR_FILL			= 222, &
		SMG$K_FRAME			= 223, &
		SMG$K_LF_FILL			= 224, &
		SMG$K_NUMBER_FN_KEYS		= 225, &
		SMG$K_ROWS			= 226, &
		SMG$K_VMS_TERMINAL_NUMBER	= 227, &
		SMG$K_WIDE_SCREEN_COLUMNS	= 228, &
		SMG$K_PRIVATE_NUM_1		= 431, &
		SMG$K_PRIVATE_NUM_2		= 432, &
		SMG$K_PRIVATE_NUM_3		= 433, &
		SMG$K_PRIVATE_NUM_4		= 434, &
		SMG$K_PRIVATE_NUM_5		= 435, &
		SMG$K_PRIVATE_NUM_6		= 436, &
		SMG$K_PRIVATE_NUM_7		= 437, &
		SMG$K_PRIVATE_NUM_8		= 438, &
		SMG$K_PRIVATE_NUM_9		= 439, &
		SMG$K_PRIVATE_NUM_10		= 440, &
		SMG$K_MAX_NUMERIC_CODE		= 440, &
		SMG$K_BEGIN_ALTERNATE_CHAR	= 441, &
		SMG$K_BEGIN_BLINK		= 442, &
		SMG$K_BEGIN_BOLD		= 443, &
		SMG$K_BEGIN_DELETE_MODE		= 444, &
		SMG$K_BEGIN_INSERT_MODE		= 445, &
		SMG$K_BEGIN_LINE_DRAWING_CHAR	= 446, &
		SMG$K_BEGIN_REVERSE		= 447, &
		SMG$K_BEGIN_UNDERSCORE		= 448, &
		SMG$K_BOTTOM_T_CHAR		= 449, &
		SMG$K_CLEAR_TAB			= 450, &
		SMG$K_CROSS_CHAR		= 451, &
		SMG$K_CURSOR_DOWN		= 452, &
		SMG$K_CURSOR_LEFT		= 453, &
		SMG$K_CURSOR_RIGHT		= 454, &
		SMG$K_CURSOR_UP			= 455, &
		SMG$K_DARK_SCREEN		= 456, &
		SMG$K_DELETE_CHAR		= 457, &
		SMG$K_DELETE_LINE		= 458, &
		SMG$K_DEVICE_ATTRIBUTES		= 459, &
		SMG$K_DOUBLE_HIGH_BOTTOM	= 460, &
		SMG$K_DOUBLE_HIGH_TOP		= 461, &
		SMG$K_DOUBLE_WIDE		= 462, &
		SMG$K_DUPLICATE			= 463, &
		SMG$K_END_ALTERNATE_CHAR	= 464, &
		SMG$K_END_BLINK			= 465, &
		SMG$K_END_BOLD			= 466, &
		SMG$K_END_DELETE_MODE		= 467, &
		SMG$K_END_INSERT_MODE		= 468, &
		SMG$K_END_LINE_DRAWING_CHAR	= 469, &
		SMG$K_END_REVERSE		= 470, &
		SMG$K_END_UNDERSCORE		= 471, &
		SMG$K_ERASE_TO_END_DISPLAY	= 472, &
		SMG$K_ERASE_TO_END_LINE		= 473, &
		SMG$K_ERASE_WHOLE_DISPLAY	= 474, &
		SMG$K_ERASE_WHOLE_LINE		= 475, &
		SMG$K_HOME			= 476, &
		SMG$K_HORIZONTAL_BAR		= 477, &
		SMG$K_INIT_STRING		= 478, &
		SMG$K_INSERT_CHAR		= 479, &
		SMG$K_INSERT_LINE		= 480, &
		SMG$K_INSERT_PAD		= 481, &
		SMG$K_KEY_0			= 482, &
		SMG$K_KEY_1			= 483, &
		SMG$K_KEY_2			= 484, &
		SMG$K_KEY_3			= 485, &
		SMG$K_KEY_4			= 486, &
		SMG$K_KEY_5			= 487, &
		SMG$K_KEY_6			= 488, &
		SMG$K_KEY_7			= 489, &
		SMG$K_KEY_8			= 490, &
		SMG$K_KEY_9			= 491, &
		SMG$K_KEY_BACKSPACE		= 492, &
		SMG$K_KEY_COMMA			= 493, &
		SMG$K_KEY_DOWN_ARROW		= 494, &
		SMG$K_KEY_E1			= 495, &
		SMG$K_KEY_E2			= 496, &
		SMG$K_KEY_E3			= 497, &
		SMG$K_KEY_E4			= 498, &
		SMG$K_KEY_E5			= 499, &
		SMG$K_KEY_E6			= 500, &
		SMG$K_KEY_ENTER			= 501, &
		SMG$K_KEY_F1			= 502, &
		SMG$K_KEY_F2			= 503, &
		SMG$K_KEY_F3			= 504, &
		SMG$K_KEY_F4			= 505, &
		SMG$K_KEY_F5			= 506, &
		SMG$K_KEY_F6			= 507, &
		SMG$K_KEY_F7			= 508, &
		SMG$K_KEY_F8			= 509, &
		SMG$K_KEY_F9			= 510, &
		SMG$K_KEY_F10			= 511, &
		SMG$K_KEY_F11			= 512, &
		SMG$K_KEY_F12			= 513, &
		SMG$K_KEY_F13			= 514, &
		SMG$K_KEY_F14			= 515, &
		SMG$K_KEY_F15			= 516, &
		SMG$K_KEY_F16			= 517, &
		SMG$K_KEY_F17			= 518, &
		SMG$K_KEY_F18			= 519, &
		SMG$K_KEY_F19			= 520, &
		SMG$K_KEY_F20			= 521, &
		SMG$K_KEY_LEFT_ARROW		= 522, &
		SMG$K_KEY_MINUS			= 523, &
		SMG$K_KEY_PERIOD		= 524, &
		SMG$K_KEY_PF1			= 525, &
		SMG$K_KEY_PF2			= 526, &
		SMG$K_KEY_PF3			= 527, &
		SMG$K_KEY_PF4			= 528, &
		SMG$K_KEY_RIGHT_ARROW		= 529, &
		SMG$K_KEY_UP_ARROW		= 530, &
		SMG$K_LABEL_F1			= 531, &
		SMG$K_LABEL_F2			= 532, &
		SMG$K_LABEL_F3			= 533, &
		SMG$K_LABEL_F4			= 534, &
		SMG$K_LABEL_F5			= 535, &
		SMG$K_LABEL_F6			= 536, &
		SMG$K_LABEL_F7			= 537, &
		SMG$K_LABEL_F8			= 538, &
		SMG$K_LABEL_F9			= 539, &
		SMG$K_LABEL_F10			= 540, &
		SMG$K_LABEL_F11			= 541, &
		SMG$K_LABEL_F12			= 542, &
		SMG$K_LABEL_F13			= 543, &
		SMG$K_LABEL_F14			= 544, &
		SMG$K_LABEL_F15			= 545, &
		SMG$K_LABEL_F16			= 546, &
		SMG$K_LABEL_F17			= 547, &
		SMG$K_LABEL_F18			= 548, &
		SMG$K_LABEL_F19			= 549, &
		SMG$K_LABEL_F20			= 550, &
		SMG$K_LEFT_T_CHAR		= 551, &
		SMG$K_LIGHT_SCREEN		= 552, &
		SMG$K_LOWER_LEFT_CORNER		= 553, &
		SMG$K_LOWER_RIGHT_CORNER	= 554, &
		SMG$K_NAME			= 555, &
		SMG$K_NEWLINE_CHAR		= 556, &
		SMG$K_PAD_CHAR			= 557, &
		SMG$K_RESTORE_CURSOR		= 558, &
		SMG$K_RIGHT_T_CHAR		= 559, &
		SMG$K_SAVE_CURSOR		= 560, &
		SMG$K_SCROLL_FORWARD		= 561, &
		SMG$K_SCROLL_REVERSE		= 562, &
		SMG$K_SEL_ERASE_TO_END_DISPLAY	= 563, &
		SMG$K_SEL_ERASE_TO_END_LINE	= 564, &
		SMG$K_SEL_ERASE_WHOLE_DISPLAY	= 565, &
		SMG$K_SEL_ERASE_WHOLE_LINE	= 566, &
		SMG$K_SET_APPLICATION_KEYPAD	= 567, &
		SMG$K_SET_CHAR_NOT_SEL_ERASE	= 568, &
		SMG$K_SET_CHAR_SEL_ERASE	= 569, &
		SMG$K_SET_CURSOR_ABS		= 570, &
		SMG$K_SET_NUMERIC_KEYPAD	= 571, &
		SMG$K_SET_SCROLL_REGION		= 572, &
		SMG$K_SET_TAB			= 573, &
		SMG$K_SINGLE_HIGH		= 574, &
		SMG$K_TAB_CHAR			= 575, &
		SMG$K_TOP_T_CHAR		= 576, &
		SMG$K_UNDERLINE_CHAR		= 577, &
		SMG$K_UPPER_LEFT_CORNER		= 578, &
		SMG$K_UPPER_RIGHT_CORNER	= 579, &
		SMG$K_VERTICAL_BAR		= 580, &
		SMG$K_WIDTH_NARROW		= 581, &
		SMG$K_WIDTH_WIDE		= 582, &
		SMG$K_CURSOR_POSITION_REPORT	= 583, &
		SMG$K_REQUEST_CURSOR_POSITION	= 584, &
		SMG$K_CR_GRAPHIC		= 585, &
		SMG$K_FF_GRAPHIC		= 586, &
		SMG$K_LF_GRAPHIC		= 587, &
		SMG$K_HT_GRAPHIC		= 588, &
		SMG$K_VT_GRAPHIC		= 589, &
		SMG$K_TRUNCATION_ICON		= 590, &
		SMG$K_CURSOR_NEXT_LINE		= 591, &
		SMG$K_CURSOR_PRECEDING_LINE	= 592, &
		SMG$K_INDEX			= 593, &
		SMG$K_REVERSE_INDEX		= 594, &
		SMG$K_BEGIN_NORMAL_RENDITION	= 595, &
		SMG$K_BEGIN_AUTOWRAP_MODE	= 596, &
		SMG$K_END_AUTOWRAP_MODE		= 597, &
		SMG$K_BEGIN_AUTOREPEAT_MODE	= 598, &
		SMG$K_END_AUTOREPEAT_MODE	= 599, &
		SMG$K_SET_ORIGIN_RELATIVE	= 600, &
		SMG$K_SET_ORIGIN_ABSOLUTE	= 601, &
		SMG$K_ERASE_LINE_TO_CURSOR	= 602, &
		SMG$K_NEXT_LINE			= 603, &
		SMG$K_BEGIN_AUTOPRINT_MODE	= 604, &
		SMG$K_END_AUTOPRINT_MODE	= 605, &
		SMG$K_PRINT_SCREEN		= 606, &
		SMG$K_SET_CURSOR_ON		= 607, &
		SMG$K_SET_CURSOR_OFF		= 608, &
		SMG$K_SET_PRINTER_OUTPUT	= 609, &
		SMG$K_SET_SCREEN_OUTPUT		= 610, &
		SMG$K_ERASE_DISPLAY_TO_CURSOR	= 611, &
		SMG$K_PRIVATE_STR_1		= 651, &
		SMG$K_PRIVATE_STR_2		= 652, &
		SMG$K_PRIVATE_STR_3		= 653, &
		SMG$K_PRIVATE_STR_4		= 654, &
		SMG$K_PRIVATE_STR_5		= 655, &
		SMG$K_PRIVATE_STR_6		= 656, &
		SMG$K_PRIVATE_STR_7		= 657, &
		SMG$K_PRIVATE_STR_8		= 658, &
		SMG$K_PRIVATE_STR_9		= 659, &
		SMG$K_PRIVATE_STR_10		= 660

	DECLARE LONG CONSTANT &
		SMG$K_TRM_CTRLA			= 1, &
		SMG$K_TRM_CTRLB			= 2, &
		SMG$K_TRM_CTRLC			= 3, &
		SMG$K_TRM_CTRLD			= 4, &
		SMG$K_TRM_CTRLE			= 5, &
		SMG$K_TRM_CTRLF			= 6, &
		SMG$K_TRM_CTRLG			= 7, &
		SMG$K_TRM_CTRLH			= 8, &
		SMG$K_TRM_CTRLI			= 9, &
		SMG$K_TRM_CTRLJ			= 10, &
		SMG$K_TRM_CTRLK			= 11, &
		SMG$K_TRM_CTRLL			= 12, &
		SMG$K_TRM_CTRLM			= 13, &
		SMG$K_TRM_CTRLN			= 14, &
		SMG$K_TRM_CTRLO			= 15, &
		SMG$K_TRM_CTRLP			= 16, &
		SMG$K_TRM_CTRLQ			= 17, &
		SMG$K_TRM_CTRLR			= 18, &
		SMG$K_TRM_CTRLS			= 19, &
		SMG$K_TRM_CTRLT			= 20, &
		SMG$K_TRM_CTRLU			= 21, &
		SMG$K_TRM_CTRLV			= 22, &
		SMG$K_TRM_CTRLW			= 23, &
		SMG$K_TRM_CTRLX			= 24, &
		SMG$K_TRM_CTRLY			= 25, &
		SMG$K_TRM_CTRLZ			= 26, &
		SMG$K_TRM_ESCAPE		= 27, &
		SMG$K_TRM_SPACE			= 32, &
		SMG$K_TRM_DELETE		= 127, &
		SMG$K_TRM_BS			= 8, &
		SMG$K_TRM_HT			= 9, &
		SMG$K_TRM_LF			= 10, &
		SMG$K_TRM_CR			= 13, &
		SMG$K_TRM_PF1			= 256, &
		SMG$K_TRM_PF2			= 257, &
		SMG$K_TRM_PF3			= 258, &
		SMG$K_TRM_PF4			= 259, &
		SMG$K_TRM_KP0			= 260, &
		SMG$K_TRM_KP1			= 261, &
		SMG$K_TRM_KP2			= 262, &
		SMG$K_TRM_KP3			= 263, &
		SMG$K_TRM_KP4			= 264, &
		SMG$K_TRM_KP5			= 265, &
		SMG$K_TRM_KP6			= 266, &
		SMG$K_TRM_KP7			= 267, &
		SMG$K_TRM_KP8			= 268, &
		SMG$K_TRM_KP9			= 269, &
		SMG$K_TRM_ENTER			= 270, &
		SMG$K_TRM_MINUS			= 271, &
		SMG$K_TRM_COMMA			= 272, &
		SMG$K_TRM_PERIOD		= 273, &
		SMG$K_TRM_UP			= 274, &
		SMG$K_TRM_DOWN			= 275, &
		SMG$K_TRM_LEFT			= 276, &
		SMG$K_TRM_RIGHT			= 277, &
		SMG$K_TRM_F1			= 281, &
		SMG$K_TRM_F2			= 282, &
		SMG$K_TRM_F3			= 283, &
		SMG$K_TRM_F4			= 284, &
		SMG$K_TRM_F5			= 285, &
		SMG$K_TRM_F6			= 286, &
		SMG$K_TRM_F7			= 287, &
		SMG$K_TRM_F8			= 288, &
		SMG$K_TRM_F9			= 289, &
		SMG$K_TRM_F10			= 290, &
		SMG$K_TRM_F11			= 291, &
		SMG$K_TRM_F12			= 292, &
		SMG$K_TRM_F13			= 293, &
		SMG$K_TRM_F14			= 294, &
		SMG$K_TRM_HELP			= 295, &
		SMG$K_TRM_DO			= 296, &
		SMG$K_TRM_F17			= 297, &
		SMG$K_TRM_F18			= 298, &
		SMG$K_TRM_F19			= 299, &
		SMG$K_TRM_F20			= 300, &
		SMG$K_TRM_F15			= 295, &
		SMG$K_TRM_F16			= 296, &
		SMG$K_TRM_FIND			= 311, &
		SMG$K_TRM_INSERT_HERE		= 312, &
		SMG$K_TRM_REMOVE		= 313, &
		SMG$K_TRM_SELECT		= 314, &
		SMG$K_TRM_PREV_SCREEN		= 315, &
		SMG$K_TRM_NEXT_SCREEN		= 316, &
		SMG$K_TRM_E1			= 311, &
		SMG$K_TRM_E2			= 312, &
		SMG$K_TRM_E3			= 313, &
		SMG$K_TRM_E4			= 314, &
		SMG$K_TRM_E5			= 315, &
		SMG$K_TRM_E6			= 316, &
		SMG$K_TRM_CANCELLED		= 508, &
		SMG$K_TRM_TIMEOUT		= 509, &
		SMG$K_TRM_BUFFER_FULL		= 510, &
		SMG$K_TRM_UNKNOWN		= 511, &
		SMG$M_KEY_NOECHO		= 1, &
		SMG$M_KEY_TERMINATE		= 2, &
		SMG$M_KEY_LOCK			= 4, &
		SMG$M_KEY_PROTECTED		= 8, &
		SMG$M_KEY_SETSTATE		= 16, &
		SMG$S_KEY_DEF_ATTR		= 4, &
		SMG$C_CHANGE_RENDITION		= 10, &
		SMG$C_DELETE_CHARS		= 11, &
		SMG$C_ERASE_DISPLAY		= 12, &
		SMG$C_ERASE_LINE		= 13, &
		SMG$C_HOME_CURSOR		= 14, &
		SMG$C_INSERT_CHARS		= 15, &
		SMG$C_INSERT_LINE		= 16, &
		SMG$C_PUT_CHARS			= 17, &
		SMG$C_PUT_LINE			= 18, &
		SMG$C_PUT_DISPLAY_ENCODED	= 19, &
		SMG$C_RETURN_CURSOR_POS		= 20, &
		SMG$C_PUT_WITH_SCROLL		= 21, &
		SMG$C_SET_CURSOR_ABS		= 22, &
		SMG$C_SET_CURSOR_REL		= 23, &
		SMG$C_DELETE_LINE		= 24, &
		SMG$C_ERASE_CHARS		= 25, &
		SMG$C_SCROLL_DISPLAY_AREA	= 26, &
		SMG$C_CHANGE_VIRTUAL_DISPLAY	= 27, &
		SMG$C_LABEL_BORDER		= 28, &
		SMG$C_END_DISPLAY_UPDATE	= 29, &
		SMG$C_UNITED_KINGDOM		= 0, &
		SMG$C_ASCII			= 1, &
		SMG$C_SPEC_GRAPHICS		= 2, &
		SMG$C_ALT_CHAR			= 3, &
		SMG$C_ALT_GRAPHICS		= 4, &
		SMG$C_UPPER_LEFT		= 0, &
		SMG$C_LOWER_LEFT		= 1, &
		SMG$C_UPPER_RIGHT		= 2, &
		SMG$C_LOWER_RIGHT		= 3, &
		SMG$K_TOP			= 0, &
		SMG$K_BOTTOM			= 1, &
		SMG$K_LEFT			= 2, &
		SMG$K_RIGHT			= 3, &
		SMG$M_BOLD			= 1, &
		SMG$M_REVERSE			= 2, &
		SMG$M_BLINK			= 4, &
		SMG$M_UNDERLINE			= 8, &
		SMG$M_NORMAL			= 0, &
		SMG$M_BUF_ENABLED		= 1, &
		SMG$M_MINUPD			= 2, &
		SMG$M_CLEAR_SCREEN		= 4, &
		SMG$M_NOTABS			= 8, &
		SMG$K_BUF_ENABLED		= 0, &
		SMG$K_MINUPD			= 1, &
		SMG$K_CLEAR_SCREEN		= 2, &
		SMG$K_NOTABS			= 3, &
		SMG$C_COLOR_UNKNOWN		= 0, &
		SMG$C_COLOR_WHITE		= 1, &
		SMG$C_COLOR_BLACK		= 2, &
		SMG$K_UNKNOWN			= 0, &
		SMG$K_VT05			= 1, &
		SMG$K_VT52			= 2, &
		SMG$K_VT100			= 3, &
		SMG$K_VTFOREIGN			= 4, &
		SMG$K_HARDCOPY			= 5, &
		SMG$K_VTTERMTABLE		= 6, &
		SMG$M_BORDER			= 1, &
		SMG$M_TRUNC_ICON		= 2, &
		SMG$M_DISPLAY_CONTROLS		= 4, &
		SMG$M_USER_DISPLAY		= 8, &
		SMG$M_UP			= 1, &
		SMG$M_DOWN			= 2, &
		SMG$M_RIGHT			= 4, &
		SMG$M_LEFT			= 8, &
		SMG$K_FIRST_PRIV_TYPE		= 191, &
		SMG$S_SMGDEF			= 32, &
		SMG$R_SMGDEF_BITS		= 0, &
		SMG$R_SMGMODE_BITS		= 0, &
		SMG$S_DISPLAY_ATTRIBUTES	= 1, &
		SMG$R_DISPLAY_ATTRIBUTES	= 0, &
		SMG$S_BAND_INFORMATION_TABLE	= 12, &
		SMG$R_BAND_INFORMATION_TABLE	= 0, &
		SMG$L_PASTEBOARD_ID		= 0, &
		SMG$L_ARG			= 4, &
		SMG$R_CHARACTER_OVERLAY		= 8, &
		SMG$B_CHARACTER			= 8, &
		SMG$L_CHARACTER			= 8, &
		SMG$S_PASTEBOARD_INFO_BLOCK	= 32, &
		SMG$R_PASTEBOARD_INFO_BLOCK	= 0, &
		SMG$L_DEVCHAR			= 0, &
		SMG$L_DEVDEPEND			= 4, &
		SMG$L_DEVDEPEND2		= 8, &
		SMG$B_DEVCLASS			= 12, &
		SMG$B_SMG_DEVTYPE		= 13, &
		SMG$B_PHY_DEVTYPE		= 14, &
		SMG$B_ROWS			= 15, &
		SMG$W_WIDTH			= 16, &
		SMG$B_COLOR			= 18, &
		SMG$B_PARITY			= 19, &
		SMG$W_SPEED			= 20, &
		SMG$W_FILL			= 22, &
		SMG$W_CURSOR_ROW		= 24, &
		SMG$W_CURSOR_COL		= 26, &
		SMG$L_CURSOR_DID		= 28, &
		SMG$S_KEYBOARD_INFO_BLOCK	= 20, &
		SMG$R_KEYBOARD_INFO_BLOCK	= 0, &
		SMG$B_RECALL_SIZE		= 13, &
		SMG$B_TYPEAHEAD_CHAR		= 15, &
		SMG$W_TYPEAHEAD_COUNT		= 18, &
		SMG$S_SCROLL_DIRS		= 1, &
		SMG$R_SCROLL_DIRS		= 0

	!
	! Covered by $SMGMSG
	!
	EXTERNAL LONG CONSTANT &
		SMG$_BATSTIPRO, &
		SMG$_BATWASOFF, &
		SMG$_BATWAS_ON, &
		SMG$_DELEXIGBL, &
		SMG$_DIALINNOT, &
		SMG$_DISREQ, &
		SMG$_EOF, &
		SMG$_ERRAT_LIN, &
		SMG$_ERRLIN, &
		SMG$_EXPTOOCOM, &
		SMG$_FACILITY, &
		SMG$_FAIOPEFIL, &
		SMG$_FATERRLIB, &
		SMG$_FILTOOLON, &
		SMG$_GBLNOTCRE, &
		SMG$_GBLSECMAP, &
		SMG$_GETDATSTR, &
		SMG$_ILLBATFNC, &
		SMG$_ILLVAL, &
		SMG$_INVARG, &
		SMG$_INVCOL, &
		SMG$_INVCOLARG, &
		SMG$_INVDEFATT, &
		SMG$_INVDIR, &
		SMG$_INVDIS_ID, &
		SMG$_INVEXP, &
		SMG$_INVKBD_ID, &
		SMG$_INVKEYNAM, &
		SMG$_INVKTB_ID, &
		SMG$_INVMAXLEN, &
		SMG$_INVPAGARG, &
		SMG$_INVPAS_ID, &
		SMG$_INVREQCOD, &
		SMG$_INVROW, &
		SMG$_INVSTANAM, &
		SMG$_INVTERTAB, &
		SMG$_INVWIDARG, &
		SMG$_KEYDEFPRO, &
		SMG$_KEYNOTDEF, &
		SMG$_LENMUSONE, &
		SMG$_LENNOTEQL, &
		SMG$_LINNOTFND, &
		SMG$_MISENDSTA, &
		SMG$_MISFILSPE, &
		SMG$_MISNAMREQ, &
		SMG$_MISTERNAM, &
		SMG$_NOMOREKEYS, &
		SMG$_NORMAL, &
		SMG$_NOTBOOCAP, &
		SMG$_NOTIMP, &
		SMG$_NOTNUMCAP, &
		SMG$_NOTPASTED, &
		SMG$_NOTRMSOUT, &
		SMG$_NOTSTRCAP, &
		SMG$_NOT_A_TRM, &
		SMG$_NO_ARGS, &
		SMG$_NO_CHADIS, &
		SMG$_NO_MORMSG, &
		SMG$_PASALREXI, &
		SMG$_PBDIN_USE, &
		SMG$_PREDEFREP, &
		SMG$_PRISECMAP, &
		SMG$_PROTOOLON, &
		SMG$_STRTERESC, &
		SMG$_SYNERR, &
		SMG$_TABID_MIS, &
		SMG$_TOOMANDIS, &
		SMG$_TOOMANPAS, &
		SMG$_TRMNOTANS, &
		SMG$_UNDTERNAM, &
		SMG$_UNDTERNOP, &
		SMG$_UNDTERNOS, &
		SMG$_UNDTERTYP, &
		SMG$_UNRECSTA, &
		SMG$_WILUSERMS, &
		SMG$_WRONUMARG
	%END %IF

	%LIST
	%CROSS

