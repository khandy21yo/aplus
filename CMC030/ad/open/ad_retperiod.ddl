DEFINE RECORD CDD$TOP.AD.AD_RETPERIOD

        DESCRIPTION IS /*Retired Asset Last Period*/.

        AD_RETPERIOD_CDD STRUCTURE.

        /* Element = ASSET_NUM
        Description = Asset number */
        ASSET_NUM               DATATYPE IS TEXT SIZE IS 10.

        /* Element = DEP_OBJECT
        Description = Depreciation object */
        DEP_OBJECT              DATATYPE IS TEXT SIZE IS 1.

        /* Element = PERIOD
        Description = Period (YYYYPP) */
        PERIOD                  DATATYPE IS TEXT SIZE IS 6.

        END AD_RETPERIOD_CDD STRUCTURE.

END AD_RETPERIOD.
