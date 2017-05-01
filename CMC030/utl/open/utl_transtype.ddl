DEFINE RECORD CDD$TOP.UTL.UTL_TRANSTYPE

        DESCRIPTION IS /*Transaction Type Description*/.

        UTL_TRANSTYPE_CDD STRUCTURE.

        /* Element = TRANSTYPE
        Description = Transaction type code */
        CODE                    DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Transaction type description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = Classification */
        CLASS                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Transaction sign */
        TRANSSIGN               DATATYPE IS TEXT SIZE IS 1.

        END UTL_TRANSTYPE_CDD STRUCTURE.

END UTL_TRANSTYPE.
