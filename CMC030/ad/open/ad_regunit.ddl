DEFINE RECORD CDD$TOP.AD.AD_REGUNIT

        DESCRIPTION IS /*Depreciation Unit Register*/.

        AD_REGUNIT_CDD STRUCTURE.

        /* Element = ASSET_NUM
        Description = Asset number */
        ASSET_NUM               DATATYPE IS TEXT SIZE IS 10.

        /* Element = DEP_OBJECT
        Description = Depreciation object */
        DEP_OBJECT              DATATYPE IS TEXT SIZE IS 1.

        /* Element = PERIOD
        Description = Period (YYYYPP) */
        PERIOD                  DATATYPE IS TEXT SIZE IS 6.

        /* Element = DATE
        Description = Date */
        ACTION_DATE             DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Station man */
        STATIONMAN              DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Quantity, units */
        QUANTITY                DATATYPE IS G_FLOATING.

        /* Element = DATE
        Description = Post date */
        POST_DATE               DATATYPE IS TEXT SIZE IS 8.

        /* Element = TIME
        Description = Time (HHMMSS) */
        POST_TIME               DATATYPE IS TEXT SIZE IS 6.

        /* Element = BATCH
        Description = Batch number used for posting */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        END AD_REGUNIT_CDD STRUCTURE.

END AD_REGUNIT.
