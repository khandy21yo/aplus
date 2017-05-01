DEFINE RECORD CDD$TOP.BC.BC_CUSTYP

        DESCRIPTION IS /*Billing to Customer Type Definitions*/.

        BC_CUSTYP_CDD STRUCTURE.

        /* Element =
        Description = Customer type */
        CUSTYP                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        AR_ACCT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 30.

        END BC_CUSTYP_CDD STRUCTURE.

END BC_CUSTYP.
