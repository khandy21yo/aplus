DEFINE RECORD CDD$TOP.AD.AD_HISTORY

        DESCRIPTION IS /*Asset Depreciation History*/.

        AD_HISTORY_CDD STRUCTURE.

        /* Element = ASSET_NUM
        Description = Asset number */
        ASSET_NUM               DATATYPE IS TEXT SIZE IS 10.

        /* Element = DEP_OBJECT
        Description = Depreciation object */
        DEP_OBJECT              DATATYPE IS TEXT SIZE IS 1.

        /* Element = DEP_STATUS
        Description = Prior the asset activity status */
        DEP_STATUS              DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Depreciation amount */
        AMOUNT_HIS              DATATYPE IS G_FLOATING.

        /* Element =
        Description = Depreciation units */
        UNIT_HIS                DATATYPE IS G_FLOATING.

        /* Element = PERIOD
        Description = Fiscal year (YYYY) and Cycle (PP) */
        PERIOD                  DATATYPE IS TEXT SIZE IS 6.

        END AD_HISTORY_CDD STRUCTURE.

END AD_HISTORY.
