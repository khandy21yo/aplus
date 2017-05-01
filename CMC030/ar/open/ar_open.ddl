DEFINE RECORD CDD$TOP.AR.AR_OPEN

        DESCRIPTION IS /*Open Item File*/.

        AR_OPEN_CDD STRUCTURE.

        /* Element = CUSNUM
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = INVNUM
        Description = Invoice number */
        INVNUM                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Transaction Type */
        TRATYP                  DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Transaction date */
        TRADAT                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Sale amount */
        SALAMT                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Discount amount */
        DISAMT                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Other charges (Sales tax) */
        OTHCHG                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Receipt number */
        RECNUM                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Check number */
        CHKNUM                  DATATYPE IS TEXT SIZE IS 6.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ARACCT                  DATATYPE IS TEXT SIZE IS 18.

        /* Element = SUBACC
        Description = Sub account (job number) */
        SUBACC                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 25.

        /* Element =
        Description = Salesperson number */
        SALNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = BATCH
        Description = Batch number used for posting */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = (PPYYYY) */
        UPDATED                 DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = (PPYYYY) */
        CLOSEDATE               DATATYPE IS TEXT SIZE IS 6.

        /* Element = DATE
        Description = Due Date (YYYYMMDD) */
        DUEDATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element = DATE
        Description = Discount Date (YYYYMMDD) */
        DISCOUNTDATE            DATATYPE IS TEXT SIZE IS 8.

        END AR_OPEN_CDD STRUCTURE.

END AR_OPEN.
