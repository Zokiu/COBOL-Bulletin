      ****************************************************************** 
      *    
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. report.
       AUTHOR. AlexEnCode&Terry.

      ****************************************************************** 
      *    
      ****************************************************************** 
       ENVIRONMENT DIVISION.
      ****************************************************************** 
      *    
      ******************************************************************        
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       SOURCE-COMPUTER. DELL WITH DEBUGGING MODE.

      ****************************************************************** 
      *    
      ****************************************************************** 
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-INPUT
               ASSIGN TO 'input.dat'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS F-INPUT-STATUS.

           SELECT F-OUTPUT
               ASSIGN TO 'output.dat'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS F-OUTPUT-STATUS.    

      ****************************************************************** 
      *    
      ****************************************************************** 
       DATA DIVISION.

      ****************************************************************** 
      *    
      ****************************************************************** 
       FILE SECTION.

       FD  F-INPUT
           RECORD CONTAINS 2 TO 1000 CHARACTERS 
           RECORDING MODE IS V.

       01  REC-F-INPUT-2         PIC 9(02).
       01  REC-F-INPUT-10        PIC X(10).
       01  REC-F-INPUT-100       PIC X(100).
       01  REC-F-INPUT-1000      PIC X(1000).

       01  REC-STUDENT.
           03 R-S-KEY            PIC 9(02).       
           03 R-LASTNAME         PIC X(07).       
           03 R-FIRSTNAME        PIC X(06).       
           03 R-AGE              PIC 9(02).       

       01  REC-COURSE.
           03 R-C-KEY            PIC 9(02).       
           03 R-LABEL            PIC X(21).       
           03 R-COEF             PIC 9,9.       
           03 R-GRADE            PIC 99,99.       

       FD  F-OUTPUT
           RECORD CONTAINS 250 CHARACTERS
           RECORDING MODE IS F.

       01  REC-F-OUTPUT          PIC X(250).

      ****************************************************************** 
      *    
      ****************************************************************** 
       WORKING-STORAGE SECTION.

      *Flag gérant la lecture du fichier importé.
       01  F-INPUT-STATUS        PIC X(02) VALUE SPACE.
           88 F-INPUT-STATUS-OK    VALUE '00'.        
           88 F-INPUT-STATUS-EOF   VALUE '10'.

      *Flag gérant l'écriture du fichier exporté.
       01  F-OUTPUT-STATUS       PIC X(02) VALUE SPACE.
           88 F-OUTPUT-STATUS-OK    VALUE '00'.        
           88 F-OUTPUT-STATUS-EOF   VALUE '10'.

      *Groupe de variable comprenant un tableau pour les étudiants.
       01  DATA-STUDENT.
      *Variable gérant la taille du tableau.
           05 STUDENT-LGHT       PIC 9(03).
      *Tableau pour stocker les informations de chaque étudiant.
           05 STUDENT OCCURS 1 TO 999 TIMES 
                                           DEPENDING ON STUDENT-LGHT.
               10 S-ID           PIC 9(03).
               10 S-FIRSTNAME    PIC X(06).      
               10 S-LASTNAME     PIC X(07).
               10 S-AGE          PIC 9(02).
               10 S-AVERAGE      PIC 9(02)v9(02).
      
      *Variable servant d'index pour naviguer dans le tableau STUDENT.
       77  WS-STUDENT-IDX        PIC 9(03).

      *Groupe de variable comprenant un tableau pour les cours.
       01  DATA-COURSE.
      *Variable gérant la taille du tableau.
           05 COURSE-LGHT        PIC 9(03).
      *Tableau pour stocker les informations de chaque cours.
           05 COURSE OCCURS 1 TO 999 TIMES 
                                           DEPENDING ON COURSE-LGHT.
               10 C-ID           PIC 9(03).
               10 C-LABEL        PIC X(21).
               10 C-COEF         PIC 9(01)v9(01).
               10 C-GRADE        PIC 9(02)v9(02).

      *Variable servant d'index pour naviguer dans le tableau COURSE.
       77  WS-COURSE-IDX         PIC 9(03).


      *Variable d'affichage d'étoile.
       01  AFF-ETOILE            PIC X(80) VALUE ALL "*".

      ****************************************************************** 
      *    
      ****************************************************************** 
       PROCEDURE DIVISION.

      *Appel d'un paragraphe pour la lecture du fichier importé.
           PERFORM 0100-READ-START
           THRU    0100-READ-END.

      *Appel d'un paragraphe pour l'affichage console.
           PERFORM 0200-DISPLAY-START
           THRU    0200-DISPLAY-END.

      *Appel d'un paragraphe pour calculer les moyennes.
           PERFORM 0300-AVERAGE-START
           THRU    0300-AVERAGE-END.

           STOP RUN.

      ******************************************************************
      *
      ******************************************************************

      *Paragraphe gérant la lecture du fichier importé
      *Et le stockage de ses informations dans la WS.
       0100-READ-START.
           
      *Ouverture du fichier importé.
           OPEN INPUT F-INPUT.

      *On boucle jusqu'à l'état voulu.
           PERFORM UNTIL F-INPUT-STATUS-EOF
      *On lit le fichier.
               READ F-INPUT
                NOT AT END
      *On crée une boucle conditionnelle pour gérer les KEY
      *Et savoir où positionner les informations.
                   EVALUATE REC-F-INPUT-2 
      *Dans le cas 1: On gère les étudiants.
      *On augmente la capacité du tableau à chaque itération.
                     WHEN = "01"
                       ADD 1             TO STUDENT-LGHT
                       MOVE R-LASTNAME   TO S-LASTNAME(STUDENT-LGHT)
                       MOVE R-FIRSTNAME  TO S-FIRSTNAME(STUDENT-LGHT)
                       MOVE R-AGE        TO S-AGE(STUDENT-LGHT)
                       MOVE STUDENT-LGHT TO S-ID(STUDENT-LGHT)
      *Dans le cas 2: On gère les cours.
      *On augmente la capacité du tableau à chaque itération.
      *On assigne l'ID de l'étudiant à tout les cours qui lui correspondent.
                     WHEN = "02"
                       ADD 1                   TO COURSE-LGHT
                       MOVE S-ID(STUDENT-LGHT) TO C-ID(COURSE-LGHT)
                       MOVE R-LABEL            TO C-LABEL(COURSE-LGHT)
                       MOVE R-COEF             TO C-COEF(COURSE-LGHT)
                       MOVE R-GRADE            TO C-GRADE(COURSE-LGHT)
                     WHEN OTHER 
                       DISPLAY "Valeur non trouvé"
                   END-EVALUATE

               END-READ

           END-PERFORM.

      *On ferme le fichier importé.
           CLOSE F-INPUT.

           EXIT.
       0100-READ-END.
      
      *Paragraphe gérant l'affichage console des données stockées.
       0200-DISPLAY-START.

      *On boucle sur le tableau STUDENT
           PERFORM VARYING WS-STUDENT-IDX FROM 1 BY 1
                                   UNTIL WS-STUDENT-IDX > STUDENT-LGHT
      *On affiche les données de chaque étudiant un à un.
                DISPLAY AFF-ETOILE
                DISPLAY STUDENT(WS-STUDENT-IDX)
                DISPLAY AFF-ETOILE
      *On boucle sur le tableau COURSE
                PERFORM VARYING WS-COURSE-IDX FROM 1 BY 1
                                   UNTIL WS-COURSE-IDX  > COURSE-LGHT
      *Sous condition que les ID correspondent,
      *On affiche les données de chaque cours.
                    IF C-ID(WS-COURSE-IDX) = S-ID(WS-STUDENT-IDX)
                    DISPLAY COURSE(WS-COURSE-IDX)
                    END-IF
                END-PERFORM
           END-PERFORM.
                                   
           EXIT.
       0200-DISPLAY-END.

       0300-AVERAGE-START.

           PERFORM VARYING WS-STUDENT-IDX FROM 1 BY 1
                                   UNTIL WS-STUDENT-IDX > STUDENT-LGHT
              PERFORM VARYING WS-COURSE-IDX FROM 1 BY 1
                             UNTIL WS-COURSE-IDX >  COURSE-LGHT
                

           EXIT.
       0300-AVERAGE-END.
       

