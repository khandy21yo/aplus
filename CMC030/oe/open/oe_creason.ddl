DEFINE RECORD CDD$TOP.OE.OE_CREASON

        DESCRIPTION IS /*Reason Code Description*/.

        OE_CREASON_CDD STRUCTURE.

        /* Element =
        Description = */
        CREASON                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = DESCRIPTION */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        /* Element = TAXABLE
        Description = Taxable Flag */
        TAXABLE                 DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Unused space */
        CR_ACCT                 DATATYPE IS TEXT SIZE IS 17.

        END OE_CREASON_CDD STRUCTURE.

END OE_CREASON.
