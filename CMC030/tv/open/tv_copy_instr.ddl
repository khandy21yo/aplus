DEFINE RECORD CDD$TOP.TV.TV_COPY_INSTR

        DESCRIPTION IS /*TV Copy Instructions*/.

        TV_COPY_INSTR_CDD STRUCTURE.

        /* Element =
        Description = Form number */
        FRMNUM                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Sequence number */
        SEQNUM                  DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = From date */
        FROM_DATE               DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = To date */
        TO_DATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Length */
        LENGTH                  DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = From time */
        FROM_TIME               DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = To time */
        TO_TIME                 DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Spot rotations */
        SPOT_ROTATION           DATATYPE IS TEXT SIZE IS 30.

        /* Element =
        Description = Current Rotation */
        CURRENT_ROTATION        DATATYPE IS SIGNED WORD.

        END TV_COPY_INSTR_CDD STRUCTURE.

END TV_COPY_INSTR.
