DEFINE RECORD CDD$TOP.TV.TV_CONTROL

        DESCRIPTION IS /*Control File for TV Traffic*/.

        TV_CONTROL_CDD STRUCTURE.

        /* Element =
        Description = Program number */
        PROGNUM                 DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Weight for priority */
        WEIGHT_PRIORITY         DATATYPE IS G_FLOATING.

        /* Element =
        Description = Weight for spot length */
        WEIGHT_LENGTH           DATATYPE IS G_FLOATING.

        /* Element =
        Description = Weight for spot cost */
        WEIGHT_COST             DATATYPE IS G_FLOATING.

        /* Element =
        Description = Weight for spot randomness */
        WEIGHT_RANDOM           DATATYPE IS G_FLOATING.

        /* Element =
        Description = Badness for each priority point */
        BADNESS_PRIORITY        DATATYPE IS G_FLOATING.

        /* Element =
        Description = Badness for each spot in break already */
        BADNESS_PER_SPOT        DATATYPE IS G_FLOATING.

        /* Element =
        Description = Badness for each confl. spot in break */
        BADNESS_CONFLICT        DATATYPE IS G_FLOATING.

        /* Element =
        Description = Badness if break match code matches */
        BADNESS_MATCH           DATATYPE IS G_FLOATING.

        /* Element =
        Description = Random badness amount */
        BADNESS_RANDOM          DATATYPE IS G_FLOATING.

        /* Element =
        Description = Worst badness in break that can sched. */
        BADNESS_WORST           DATATYPE IS G_FLOATING.

        /* Element =
        Description = Default spot seperation */
        DEFAULT_SPOT_SEPER      DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Default product seperation */
        DEFAULT_PROD_SEPER      DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Maximum spot/product seperation */
        MAXIMUM_SEPER           DATATYPE IS TEXT SIZE IS 6.

        END TV_CONTROL_CDD STRUCTURE.

END TV_CONTROL.
