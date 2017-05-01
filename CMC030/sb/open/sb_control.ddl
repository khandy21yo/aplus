DEFINE RECORD CDD$TOP.SB.SB_CONTROL

        DESCRIPTION IS /*Subaccount Control File*/.

        SB_CONTROL_CDD STRUCTURE.

        /* Element = SYSTEM
        Description = Software System code */
        SYSTEM                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = PERIOD
        Description = Fiscal year (YYYY) and Cycle (PP) closed */
        PERIOD                  DATATYPE IS TEXT SIZE IS 6.

        /* Element = CONTROLFLAG
        Description = Status flag in the control files */
        CONTROLFLAG             DATATYPE IS TEXT SIZE IS 1.

        /* Element = DATE
        Description = Date (YYYYMMDD) */
        CDATE                   DATATYPE IS TEXT SIZE IS 8.

        /* Element = TIME
        Description = Time (HHMMSS) */
        CTIME                   DATATYPE IS TEXT SIZE IS 6.

        /* Element = BATCH
        Description = Batch number used for process (post,clos */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        /* Element = SUBJECT
        Description = Subaccount subject */
        SUBJECT                 DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Default number */
        DEFNUMBER               DATATYPE IS TEXT SIZE IS 10.

        END SB_CONTROL_CDD STRUCTURE.

END SB_CONTROL.
