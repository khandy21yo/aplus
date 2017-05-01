DEFINE RECORD CDD$TOP.PS.PS_CASHINOUT

        DESCRIPTION IS /*Cash in and out of the cash Register*/.

        PS_CASHINOUT_CDD STRUCTURE.

        /* Element = DATE
        Description = Date (YYYYMMDD) */
        CASHDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element = TIME
        Description = Time (HHMMSS) */
        CASHTIME                DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Cash Amount */
        AMOUNT                  DATATYPE IS G_FLOATING.

        /* Element = NOTES
        Description = Notes */
        NOTES                   DATATYPE IS TEXT SIZE IS 40.

        /* Element = OPERATOR
        Description = Written By */
        OPERATOR                DATATYPE IS TEXT SIZE IS 10.

        /* Element = DEPOSIT
        Description = Deposit number */
        DEPOSIT                 DATATYPE IS TEXT SIZE IS 6.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCOUNT                 DATATYPE IS TEXT SIZE IS 18.

        END PS_CASHINOUT_CDD STRUCTURE.

END PS_CASHINOUT.
