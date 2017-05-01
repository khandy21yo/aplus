DEFINE RECORD CDD$TOP.MO.MO_OPTION

        DESCRIPTION IS /*Option Definition Master File*/.

        MO_OPTION_CDD STRUCTURE.

        /* Element =
        Description = Option Group */
        OPTGROUP                DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Option Code */
        OPTN                    DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Option Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        END MO_OPTION_CDD STRUCTURE.

END MO_OPTION.
