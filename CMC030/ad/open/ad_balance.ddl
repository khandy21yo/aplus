DEFINE RECORD CDD$TOP.AD.AD_BALANCE

        DESCRIPTION IS /*Asset Depreciation Balances*/.

        AD_BALANCE_CDD STRUCTURE.

        /* Element = ASSET_NUM
        Description = Asset number */
        ASSET_NUM               DATATYPE IS TEXT SIZE IS 10.

        /* Element = DEP_OBJECT
        Description = Depreciation object */
        DEP_OBJECT              DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Depreciation status (active,retired..) */
        DEP_STATUS              DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Total depreciated dollars */
        AMOUNT_CTD              DATATYPE IS G_FLOATING.

        /* Element =
        Description = Total units */
        UNIT_CTD                DATATYPE IS G_FLOATING.

        /* Element = PERIOD
        Description = Last period updated */
        LASTPER                 DATATYPE IS TEXT SIZE IS 6.

        END AD_BALANCE_CDD STRUCTURE.

END AD_BALANCE.
