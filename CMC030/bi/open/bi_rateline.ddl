DEFINE RECORD CDD$TOP.BI.BI_RATELINE

        DESCRIPTION IS /*CPT Time Rate Line File*/.

        BI_RATELINE_CDD STRUCTURE.

        /* Element = RATETABLE
        Description = CPT Time Rate Table */
        RATETABLE               DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Time Interval */
        INTERVAL                DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Rate */
        RATE                    DATATYPE IS G_FLOATING.

        END BI_RATELINE_CDD STRUCTURE.

END BI_RATELINE.
