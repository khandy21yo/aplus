DEFINE RECORD CDD$TOP.CK.CK_CONTROLACC

        DESCRIPTION IS /*Check Account Control File*/.

        CK_CONTROLACC_CDD STRUCTURE.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCOUNT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Bank Account */
        BANK_ACCT               DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Start Check Number */
        STARTCK                 DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = End Check Number */
        ENDCK                   DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Bank Number */
        BANK_NUM                DATATYPE IS TEXT SIZE IS 20.

        END CK_CONTROLACC_CDD STRUCTURE.

END CK_CONTROLACC.
