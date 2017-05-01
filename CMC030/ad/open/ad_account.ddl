DEFINE RECORD CDD$TOP.AD.AD_ACCOUNT

        DESCRIPTION IS /*Asset and Depreciation Account Table*/.

        AD_ACCOUNT_CDD STRUCTURE.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Asset type */
        ASSET_TYPE              DATATYPE IS TEXT SIZE IS 2.

        /* Element = ACCOUNT
        Description = Asset Account Number */
        ASS_ACCT                DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = Depreciation Account Number */
        DEP_ACCT                DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = Expense Account Number */
        EXP_ACCT                DATATYPE IS TEXT SIZE IS 18.

        END AD_ACCOUNT_CDD STRUCTURE.

END AD_ACCOUNT.
