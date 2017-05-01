DEFINE RECORD CDD$TOP.OE.OE_REASONACCT

        DESCRIPTION IS /*Reason Account Table*/.

        OE_REASONACCT_CDD STRUCTURE.

        /* Element =
        Description = Reason Code */
        CREASON                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCOUNT                 DATATYPE IS TEXT SIZE IS 18.

        END OE_REASONACCT_CDD STRUCTURE.

END OE_REASONACCT.
