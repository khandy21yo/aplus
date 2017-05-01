DEFINE RECORD CDD$TOP.AP.AP_CLOSE_DIST

        DESCRIPTION IS /*Closed Distribution Information*/.

        AP_CLOSE_DIST_CDD STRUCTURE.

        /* Element =
        Description = Transaction key */
        TRANKEY                 DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Must be reset */
        SLINE                   DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = PO number */
        PONUM                   DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Must be reset */
        PO_LINE                 DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Account number */
        ACCT                    DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Subaccount */
        SUBACC                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Operation */
        OPERATION               DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Units */
        UNITS                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = Amount */
        AMOUNT                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Discount amount */
        DISCAMT                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Flag is either (y/N) */
        USE_TAX_FLAG            DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Batch update number */
        BTHNUM                  DATATYPE IS TEXT SIZE IS 6.

        END AP_CLOSE_DIST_CDD STRUCTURE.

END AP_CLOSE_DIST.
