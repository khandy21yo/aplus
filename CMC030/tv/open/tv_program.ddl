DEFINE RECORD CDD$TOP.TV.TV_PROGRAM

        DESCRIPTION IS /*TV Program Master File*/.

        TV_PROGRAM_CDD STRUCTURE.

        /* Element =
        Description = Program number */
        PRGNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Run time */
        START_TIME              ARRAY 0:6 DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = From Date */
        FROM_DATE               DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = To date */
        TO_DATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Program title */
        TITLE                   DATATYPE IS TEXT SIZE IS 50.

        /* Element =
        Description = Program Source */
        SOURCE                  DATATYPE IS TEXT SIZE IS 04.

        /* Element =
        Description = Program Type */
        PTYPE                   DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Program Length */
        LENGTH                  DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Comment */
        COMMENT                 DATATYPE IS TEXT SIZE IS 50.

        /* Element =
        Description = Cutaway flag */
        CUTAWAY                 DATATYPE IS TEXT SIZE IS 10.

        END TV_PROGRAM_CDD STRUCTURE.

END TV_PROGRAM.
