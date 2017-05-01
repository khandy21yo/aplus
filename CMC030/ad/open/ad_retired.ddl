DEFINE RECORD CDD$TOP.AD.AD_RETIRED

        DESCRIPTION IS /*Retired Asset*/.

        AD_RETIRED_CDD STRUCTURE.

        /* Element = ASSET_NUM
        Description = Asset number */
        ASSET_NUM               DATATYPE IS TEXT SIZE IS 10.

        /* Element = DATE
        Description = Date when asset has been retired */
        RET_DATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Amount of disposition */
        PROCEEDS                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Notes */
        NOTES                   DATATYPE IS TEXT SIZE IS 40.

        END AD_RETIRED_CDD STRUCTURE.

END AD_RETIRED.
