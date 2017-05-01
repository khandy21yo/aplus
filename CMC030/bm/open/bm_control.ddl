DEFINE RECORD CDD$TOP.BM.BM_CONTROL

        DESCRIPTION IS /*BOM Control File*/.

        BM_CONTROL_CDD STRUCTURE.

        /* Element =
        Description = STD Burden Hourly Rate */
        BURDENRATE              DATATYPE IS G_FLOATING.

        /* Element =
        Description = Component Types */
        PRODTYPE                DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = STD Labor Hourly Rate */
        LABORRATE               DATATYPE IS G_FLOATING.

        /* Element =
        Description = Raw Material Type */
        RMAT                    DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = Burden percentage of Labor */
        BURDENPERC              DATATYPE IS G_FLOATING.

        END BM_CONTROL_CDD STRUCTURE.

END BM_CONTROL.
