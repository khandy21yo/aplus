DEFINE RECORD CDD$TOP.TV.TV_LOG_BREAK

        DESCRIPTION IS /*TV Break Log*/.

        TV_LOG_BREAK_CDD STRUCTURE.

        /* Element =
        Description = Date of break */
        DATE                    DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Scheduled Time of break */
        SCH_TIME                DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Actual run time */
        RUN_TIME                DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Break Number */
        PRGNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Break Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 30.

        /* Element =
        Description = Type of break */
        BRKTYPE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Length of break */
        LENGTH                  DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Comment */
        COMMENT                 DATATYPE IS TEXT SIZE IS 30.

        /* Element =
        Description = Maximum number of commercials */
        MAXCOM                  DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Break priority */
        PRIORITY                DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Match code */
        MATCH                   DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Fill (usable for anything) */
        FILLER                  DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Run (0-yes, 1-no, 2-cancelled) */
        RUN                     DATATYPE IS TEXT SIZE IS 1.

        END TV_LOG_BREAK_CDD STRUCTURE.

END TV_LOG_BREAK.
