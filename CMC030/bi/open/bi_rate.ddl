DEFINE RECORD CDD$TOP.BI.BI_RATE

        DESCRIPTION IS /*Medical Service Rate*/.

        BI_RATE_CDD STRUCTURE.

        /* Element = CPT
        Description = Current Procedural Terminology Code */
        CPT                     DATATYPE IS TEXT SIZE IS 5.

        /* Element = DATE
        Description = Effective Date (YYYYMMDD) */
        EFFDATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Flat Rate */
        RATE                    DATATYPE IS G_FLOATING.

        /* Element = RATETABLE
        Description = CPT Time Rate Table */
        RATETABLE               DATATYPE IS TEXT SIZE IS 6.

        END BI_RATE_CDD STRUCTURE.

END BI_RATE.
