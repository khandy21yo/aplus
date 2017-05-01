DEFINE RECORD CDD$TOP.AD.AD_CALCULATION

        DESCRIPTION IS /*Asset Depreciation Ledger*/.

        AD_CALCULATION_CDD STRUCTURE.

        /* Element = ASSET_NUM
        Description = Asset number */
        ASSET_NUM               DATATYPE IS TEXT SIZE IS 10.

        /* Element = DEP_OBJECT
        Description = Depreciation object */
        DEP_OBJECT              DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Depreciation status (active,retired) */
        DEP_STATUS              DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Current depreciated dollars */
        AMOUNT_CUR              DATATYPE IS G_FLOATING.

        /* Element =
        Description = Current units */
        UNIT_CUR                DATATYPE IS G_FLOATING.

        END AD_CALCULATION_CDD STRUCTURE.

END AD_CALCULATION.
