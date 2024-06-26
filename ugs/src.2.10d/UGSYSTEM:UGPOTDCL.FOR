C  DECLARATION OF THE PICTURE OPTIONS TABLE STRUCTURE.
      INTEGER*4     EXST(13)
C  INTENSITY LEVEL.
      INTEGER*4     EXIL
C  COLOR VALUE.
      INTEGER*4     EXCR
C  BLINKING FLAG.
      INTEGER*4     EXBL
C  PICK IDENTIFICATION.
      INTEGER*4     EXPI
C  MARKER VALUE.
      INTEGER*4     EXMK
C  LINE STRUCTURE.
      INTEGER*4     EXLS
C  CHARACTER SIZE (IN WINDOW).
      REAL*4        EXSZ
C  CHARACTER SIZE (IN DEFAULT DRAWING SPACE).
      REAL*4        EXDZ
C  CHARACTER ANGLE.
      REAL*4        EXAG
C  JUSTIFICATION FLAG.
      INTEGER*4     EXJF
C  CHARACTER PRIORITY.
      INTEGER*4     EXCP
C  FIXED SIZE FLAG.
      INTEGER*4     EXFX
C  CONSTANT COORDINATE FLAG.
      INTEGER*4     EXCC
C  EQUIVALENCES FOR OPTIONS TABLE.
      EQUIVALENCE   (EXIL,EXST( 1)),      (EXCR,EXST( 2)),
     X              (EXBL,EXST( 3)),      (EXPI,EXST( 4)),
     X              (EXMK,EXST( 5)),      (EXLS,EXST( 6)),
     X              (EXSZ,EXST( 7)),      (EXDZ,EXST( 8)),
     X              (EXAG,EXST( 9)),      (EXJF,EXST(10)),
     X              (EXCP,EXST(11)),      (EXFX,EXST(12)),
     X              (EXCC,EXST(13))
