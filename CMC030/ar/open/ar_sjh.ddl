DEFINE RECORD CDD$TOP.AR.AR_SJH

        DESCRIPTION IS /*Sales Journal Header*/.

        AR_SJH_CDD STRUCTURE.

        /* Element = INVNUM
        Description = Invoice number */
        INVNUM                  DATATYPE IS TEXT SIZE IS 8.

        /* Element = CUSNUM
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Transaction type 01 - Invoice 02 - Cash */
        TRATYP                  DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Transaction date */
        TRADAT                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Transaction amount */
        AMOUNT                  DATATYPE IS G_FLOATING.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ARACCT                  DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Receipt number */
        RECNUM                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Check number */
        CHECK                   DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Deposit number */
        DEPOSIT                 DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 26.

        /* Element = DATE
        Description = Due Date (YYYYMMDD) */
        DUEDATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element = DATE
        Description = Discount Date (YYYYMMDD) */
        DISCOUNTDATE            DATATYPE IS TEXT SIZE IS 8.

        /* Element = SUBACCT
        Description = Sub account (job number) */
        SUBACCT                 DATATYPE IS TEXT SIZE IS 10.

        END AR_SJH_CDD STRUCTURE.

END AR_SJH.
