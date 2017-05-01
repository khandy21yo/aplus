DEFINE RECORD CDD$TOP.TV.TV_COMMER_SKED_INSTR

        DESCRIPTION IS /*Commercial Schedule Instructions*/.

        TV_COMMER_SKED_INSTR_CDD STRUCTURE.

        /* Element = TV_FRMNUM
        Description = Form Number */
        FRMNUM                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Schedule number */
        SKED_NUM                DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Start date */
        START_DATE              DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = End date */
        END_DATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Start time slot */
        START_TIME              DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = End time slot */
        END_TIME                DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Number of weeks in */
        IN_WEEKS                DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Number of weeks out */
        OUT_WEEKS               DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Length */
        LENGTH                  DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Rate per spot */
        RATE_PER_SPOT           DATATYPE IS G_FLOATING.

        /* Element =
        Description = Spots per day (mon thur sun) */
        SPOTS_PER_DAY           ARRAY 0:6 DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Used for rotating spots */
        TOTAL_SPOTS             DATATYPE IS SIGNED WORD.

        END TV_COMMER_SKED_INSTR_CDD STRUCTURE.

END TV_COMMER_SKED_INSTR.
