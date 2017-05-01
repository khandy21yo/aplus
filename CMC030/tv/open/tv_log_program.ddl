DEFINE RECORD CDD$TOP.TV.TV_LOG_PROGRAM

        DESCRIPTION IS /*TV Program Log*/.

        TV_LOG_PROGRAM_CDD STRUCTURE.

        /* Element =
        Description = Run date */
        DATE                    DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Run time */
        START_TIME              DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Actual run time */
        RUN_TIME                DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Program number */
        PRGNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Title */
        TITLE                   DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Source */
        SOURCE                  DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Type */
        PTYPE                   DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Length */
        LENGTH                  DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Run (0-yes, 1-no, 2-cancelled) */
        RUN                     DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Comment */
        COMMENT                 DATATYPE IS TEXT SIZE IS 50.

        /* Element =
        Description = Cutaway flag (Y/N) */
        CUTAWAY                 DATATYPE IS TEXT SIZE IS 10.

        END TV_LOG_PROGRAM_CDD STRUCTURE.

END TV_LOG_PROGRAM.
