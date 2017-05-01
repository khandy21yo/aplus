DEFINE RECORD CDD$TOP.AD.AD_DEPRECIATION

        DESCRIPTION IS /*Asset Depreciation File*/.

        AD_DEPRECIATION_CDD STRUCTURE.

        /* Element = ASSET_NUM
        Description = Asset number */
        ASSET_NUM               DATATYPE IS TEXT SIZE IS 10.

        /* Element = DEP_OBJECT
        Description = Depreciation object */
        DEP_OBJECT              DATATYPE IS TEXT SIZE IS 1.

        /* Element = DEPCLASS
        Description = Depreciation class code */
        DEPCLASS                DATATYPE IS TEXT SIZE IS 4.

        END AD_DEPRECIATION_CDD STRUCTURE.

END AD_DEPRECIATION.
