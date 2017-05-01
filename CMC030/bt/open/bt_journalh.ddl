DEFINE RECORD CDD$TOP.BT.BT_JOURNALH

        DESCRIPTION IS /*Billing Tuition Journal Header*/.

        BT_JOURNALH_CDD STRUCTURE.

        /* Element = CUSTOMER
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = DATE
        Description = Transaction Date (YYYYMMDD) */
        TRADAT                  DATATYPE IS TEXT SIZE IS 8.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        AR_ACCT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element = INVOICE
        Description = Invoice number */
        INVNUM                  DATATYPE IS TEXT SIZE IS 8.

        END BT_JOURNALH_CDD STRUCTURE.

END BT_JOURNALH.
