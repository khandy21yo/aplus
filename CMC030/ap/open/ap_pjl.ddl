DEFINE RECORD CDD$TOP.AP.AP_PJL

        DESCRIPTION IS /*Purchase Journal Line File*/.

        AP_PJL_CDD STRUCTURE.

        /* Element =
        Description = */
        TRANKEY                 DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Must be reset */
        SLINE                   DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = */
        PONUM                   DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = */
        PO_LINE                 DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = */
        ACCT                    DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = */
        SUBACC                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = */
        OPERATION               DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = */
        UNITS                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        AMOUNT                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        DISCAMT                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Flag it either (y/N) */
        USE_TAX_FLAG            DATATYPE IS TEXT SIZE IS 1.

        END AP_PJL_CDD STRUCTURE.

END AP_PJL.
