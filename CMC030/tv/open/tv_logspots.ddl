DEFINE RECORD CDD$TOP.TV.TV_LOGSPOTS

        DESCRIPTION IS /*Scheduled Spots*/.

        TV_LOGSPOTS_CDD STRUCTURE.

        /* Element =
        Description = Customer number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Form number */
        FRMNUM                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Schedule number */
        SKEDNUM                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Date */
        SCH_DATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Time */
        SCH_TIME                DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Rate */
        RATE                    DATATYPE IS G_FLOATING.

        /* Element =
        Description = Type (0-commercial, 1-fill) */
        SCH_TYPE                DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Spots flag (N-not run, R-run, S-sch) */
        SPOTS_FLAG              DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Spots code (MG-make good, etc.) */
        SPOTS_CODE              DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Length */
        LENGTH                  DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Agency number */
        AGENCY_NUM              DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = Cart number */
        CARTNUM                 DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Cut number */
        CUTNUM                  DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = From time slot */
        FROM_TIME_SLOT          DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = To time slot */
        TO_TIME_SLOT            DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Invoice number */
        INVNUM                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Invoice date */
        INVDAT                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Post date */
        POSTDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Actual run time */
        RUN_TIME                DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Log class */
        CLASS                   DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Conflict code */
        CONFLICT                DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Description/comment */
        DESCR                   DATATYPE IS TEXT SIZE IS 30.

        /* Element =
        Description = Sequence number */
        SEQNUM                  DATATYPE IS TEXT SIZE IS 2.

        END TV_LOGSPOTS_CDD STRUCTURE.

END TV_LOGSPOTS.
