DEFINE RECORD CDD$TOP.AD.AD_UNITS

        DESCRIPTION IS /*Units of Production Journal*/.

        AD_UNITS_CDD STRUCTURE.

        /* Element = DEP_OBJECT
        Description = Depreciation object */
        DEP_OBJECT              DATATYPE IS TEXT SIZE IS 1.

        /* Element = DATE
        Description = Date */
        ACTION_DATE             DATATYPE IS TEXT SIZE IS 8.

        /* Element = ASSET_NUM
        Description = Asset number */
        ASSET_NUM               DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Units */
        QUANTITY                DATATYPE IS SIGNED LONGWORD.

        END AD_UNITS_CDD STRUCTURE.

END AD_UNITS.
