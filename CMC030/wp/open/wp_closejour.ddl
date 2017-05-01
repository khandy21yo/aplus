DEFINE RECORD CDD$TOP.WP.WP_CLOSEJOUR

        DESCRIPTION IS /*Job Close Journal Header*/.

        WP_CLOSEJOUR_CDD STRUCTURE.

        /* Element = JOB
        Description = Job number */
        JOB                     DATATYPE IS TEXT SIZE IS 10.

        /* Element = DATE
        Description = Closing Date (YYYYMMDD) */
        CLOSEDATE               DATATYPE IS TEXT SIZE IS 8.

        /* Element = OPERATOR
        Description = Operator */
        OPERATOR                DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = STD Inventory Parts */
        STDPARTS                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Actual Inventory Parts */
        ACTPARTS                DATATYPE IS G_FLOATING.

        /* Element =
        Description = STD Raw Material */
        STDRAWMAT               DATATYPE IS G_FLOATING.

        /* Element =
        Description = Actual raw Material */
        ACTRAWMAT               DATATYPE IS G_FLOATING.

        /* Element =
        Description = STD Labor */
        STDLABOR                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Actual labor */
        ACTLABOR                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Standard Burden */
        STDBURDEN               DATATYPE IS G_FLOATING.

        /* Element =
        Description = Actual Burden */
        ACTBURDEN               DATATYPE IS G_FLOATING.

        /* Element =
        Description = Variance Flag */
        VARFLAG                 DATATYPE IS TEXT SIZE IS 1.

        END WP_CLOSEJOUR_CDD STRUCTURE.

END WP_CLOSEJOUR.
