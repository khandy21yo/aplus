DEFINE RECORD CDD$TOP.PP.PP_TRANTYPE

        DESCRIPTION IS /*Pacific Pride Transaction Type*/.

        PP_TRANTYPE_CDD STRUCTURE.

        /* Element =
        Description = Transaction Type */
        TRANTYPE                DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Transaction Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 30.

        END PP_TRANTYPE_CDD STRUCTURE.

END PP_TRANTYPE.
