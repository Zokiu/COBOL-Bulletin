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

      *SOURCE-COMPUTER. DELL WITH DEBUGGING MODE.

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

       01  REC-F-OUTPUT        PIC X(250).

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
                                           DEPENDING ON STUDENT-LGHT
                                           INDEXED BY WS-STUDENT-IDX.
               10 S-ID           PIC 9(03).
               10 S-FIRSTNAME    PIC X(06).      
               10 S-LASTNAME     PIC X(07).
               10 S-INITIALS     PIC X(01).
               10 S-AGE          PIC 9(02).
               10 S-AVERAGE      PIC 9(02)v9(02).

      *Groupe de variable comprenant un tableau pour les cours.
       01  DATA-COURSE.
      *Variable gérant la taille du tableau.
           05 COURSE-LGHT        PIC 9(03).
      *Variable pour compter le nombre de cours différents
           05 COURSE-NBR         PIC 9(03).
      *Tableau pour stocker les informations de chaque cours.
           05 COURSE OCCURS 1 TO 999 TIMES 
                                           DEPENDING ON COURSE-LGHT
                                           INDEXED BY WS-COURSE-IDX.
               10 C-ID           PIC 9(03).
               10 C-LABEL        PIC X(21).
               10 C-COEF         PIC 9(01)v9(01).
               10 C-GRADE        PIC 9(02)v9(02).

      *Variable Tampon pour paramétrer COURSE-NBR
       01  WS-TEMP-COURSE-NBR    PIC 9(03).

      *Groupe de variable pour stocker les moyennes de la classe.
       01  WS-AVERAGE.
           05  WS-CLASS-AVERAGE  PIC 9(02)v9(02).
           05  WS-TAB-C-AVERAGE OCCURS 1 TO 50 TIMES 
                                              DEPENDING ON COURSE-NBR.
                 10 WS-C-AVERAGE    PIC 9(02)v9(02).

      *Variables indépendantes pour les calculs de moyenne.
       01  WS-CALCUL.
           05 WS-CALCUL-COEF     PIC 9(04)v9(01).
           05 WS-CALCUL-SUM      PIC 9(04)v9(02).
           05 WS-CALCUL-TEMP     PIC 9(04)v9(02).

      *Variable servant d'index pour la fonction SEARCH dans 0400.
       01  WS-SEARCH-IDX         PIC 9(04).

      *Variable d'affichage d'étoile.
       01  AFF-ETOILE            PIC X(80) VALUE ALL "*".

      *Index indépendant.
       77  WS-IDX                PIC 9(03).

      *Groupe de variable pour préparation sortie affichage.
           
           01 WS-HEADER.
              05 WS-TITLE.
                10 FILLER        PIC X(20) VALUE SPACE.
                10 WS-TITLE-TEXT PIC X(17) VALUE "BULLETIN DE NOTES".
                10 FILLER        PIC X(20) VALUE SPACE.
              05 WS-SUB-HEADER.
                10 FILLER        PIC X(10) VALUE "Nom".
                10 FILLER        PIC X(11) VALUE "Moyenne".
                10 WS-SUB-HDR-COURSE OCCURS 1 TO 50 TIMES
                                              DEPENDING ON COURSE-NBR.
                     15 FILLER   PIC X(01) VALUE "C".
                     15 WS-C-NBR PIC 9(01).
                     15 FILLER   PIC X(04) VALUE SPACE.
           
           01 WS-BODY.
                10 WS-NAME         PIC X(10).
                10 WS-S-AVERAGE-ED PIC 9(02),9(02).
                10 FILLER          PIC X(05) VALUE SPACE.
                10 WS-TAB-GRADE OCCURS 1 TO 50 TIMES 
                                              DEPENDING ON COURSE-NBR.
                     15 WS-GRADE      PIC 9(02),9(02).
                     15 FILLER        PIC X(01) VALUE SPACE.
           01 WS-TOTAL.
                05 FILLER              PIC X(10) VALUE "Classe: ".
                05 WS-CLASS-AVERAGE-ED PIC 9(02),9(02).
                05 FILLER              PIC X(05) VALUE SPACE.
                05 WS-TAB-AVERAGE OCCURS 1 TO 50 TIMES
                                              DEPENDING ON COURSE-NBR.
                    10 WS-C-AVERAGE-ED PIC 9(02),9(02).
                    10 FILLER          PIC X(01) VALUE SPACE.
           
           01 WS-COURSE-INFO.
                05 WS-COURSE-INFO-TAB OCCURS 1 TO 50 TIMES
                                              DEPENDING ON COURSE-NBR.
                     10 FILLER     PIC X(01) VALUE "C".
                     10 WS-C-NBR-2 PIC 9(01).
                     10 FILLER     PIC X(01) VALUE SPACE.
                     10 FILLER     PIC X(08) VALUE "=> COEF:".
                     10 FILLER     PIC X(01) VALUE SPACE.
                     10 WS-C-COEF  PIC 9(01),9(01).
                     10 FILLER     PIC X(01) VALUE SPACE.
                     10 FILLER     PIC X(06) VALUE "LABEL:".
                     10 FILLER     PIC X(01) VALUE SPACE.
                     10 WS-C-LABEL PIC X(21).

           01 WS-STATS.
             03 WS-STUDENT-NBR-ED.
                05 FILLER     PIC X(18) VALUE "NOMBRE D'ELEVES =>".
                05 FILLER     PIC X(01) VALUE SPACE.
                05 WS-STUDENT-NBR       PIC Z(03).
             03 WS-COURSE-NBR-ED.
                05 FILLER     PIC X(18) VALUE "NOMBRE DE COURS =>".
                05 FILLER     PIC X(01) VALUE SPACE.
                05 WS-COURSE-NBR        PIC Z(03).
             03 WS-GRADE-NBR-ED.
                05 FILLER     PIC X(18) VALUE "NOMBRE DE NOTES =>".
                05 FILLER     PIC X(01) VALUE SPACE.
                05 WS-GRADE-NBR         PIC Z(03).
                
           01 WS-FOOTER.
               10 FILLER        PIC X(21) VALUE SPACE.
               10 WS-FOOTER-TXT PIC X(14) VALUE "Fin de rapport".
               10 FILLER        PIC X(20) VALUE SPACE.
       
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
        
      *Appel d'un paragraphe pour gérer les doublons de S-LASTNAME.
           PERFORM 0400-LASTNAME-DUPLICATES-START
           THRU    0400-LASTNAME-DUPLICATES-END.

      *Appel d'un paragraphe pour gérer le tri alphabétique
      *                            par LASTNAME dans le tableau STUDENT.
           PERFORM 0500-SORT-NAME-START
           THRU    0500-SORT-NAME-END.

      *Appel d'un paragraphe pour gérer l'écriture du fichier.
           PERFORM 0600-WRITE-START
           THRU    0600-WRITE-END.

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
                       MOVE 0            TO WS-TEMP-COURSE-NBR
      *Dans le cas 2: On gère les cours.
      *On augmente la capacité du tableau à chaque itération.
      *On assigne l'ID de l'étudiant à tout les cours qui lui correspondent.
                     WHEN = "02"
                       ADD 1                   TO COURSE-LGHT
                       MOVE S-ID(STUDENT-LGHT) TO C-ID(COURSE-LGHT)
                       MOVE R-LABEL            TO C-LABEL(COURSE-LGHT)
                       MOVE R-COEF             TO C-COEF(COURSE-LGHT)
                       MOVE R-GRADE            TO C-GRADE(COURSE-LGHT)
                       ADD 1                   TO WS-TEMP-COURSE-NBR
                       IF WS-TEMP-COURSE-NBR > COURSE-NBR
                       MOVE WS-TEMP-COURSE-NBR TO COURSE-NBR
                       END-IF
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

      *Paragraphe gérant le calcul de toutes les moyennes.
       0300-AVERAGE-START.
      *Appel d'un paragraphe pour la moyenne par élève.
           PERFORM 0310-S-AVERAGE-START
           THRU    0310-S-AVERAGE-END.
      *Appel d'un paragraphe pour la moyenne par matière.
           PERFORM 0320-C-AVERAGE-START
           THRU    0320-C-AVERAGE-END.
      *Appel d'un paragraphe pour la moyenne générale de la classe.
           PERFORM 0330-CLASS-AVERAGE-START
           THRU    0330-CLASS-AVERAGE-END.
           
           EXIT.
       0300-AVERAGE-END.

      *Paragraphe gérant le calcul des moyennes par élève.
       0310-S-AVERAGE-START.
      *On boucle dans le tableau STUDENT
           PERFORM VARYING WS-STUDENT-IDX FROM 1 BY 1
                                   UNTIL WS-STUDENT-IDX > STUDENT-LGHT
      *On réinitialise les variables de calcul entre chaque étudiant.
              MOVE 0 TO WS-CALCUL-COEF
              MOVE 0 TO WS-CALCUL-SUM
              MOVE 0 TO WS-CALCUL-TEMP
      *On boucle dans le tableau COURSE
              PERFORM VARYING WS-COURSE-IDX FROM 1 BY 1
                             UNTIL WS-COURSE-IDX >  COURSE-LGHT
      *Si les ID correspondent on commence les calculs
                IF C-ID(WS-COURSE-IDX) = S-ID(WS-STUDENT-IDX)
      *On ajoute le coefficient de chaque cours dans une variable stock.
                ADD C-COEF(WS-COURSE-IDX) TO WS-CALCUL-COEF
      *On multiplie la note avec son coefficient correspondant.
      *On stocke le résultat dans une variable temporaire.
                MULTIPLY C-GRADE(WS-COURSE-IDX) BY
                         C-COEF (WS-COURSE-IDX) GIVING
                         WS-CALCUL-TEMP ROUNDED
      *On ajoute la somme des notes pondérées
      *                    de chaque cours dans une variable stock.
                ADD WS-CALCUL-TEMP TO WS-CALCUL-SUM
                END-IF
                
              END-PERFORM
      *On divise finalement la somme des notes pondérées
      *                 par la somme des coefficients et on arrondis.
              DIVIDE WS-CALCUL-SUM BY WS-CALCUL-COEF GIVING
                                    S-AVERAGE(WS-STUDENT-IDX) ROUNDED
      *Affichage pour debug.
      D       DISPLAY S-FIRSTNAME(WS-STUDENT-IDX) SPACE WITH
      D                                                   NO ADVANCING
      D       DISPLAY S-LASTNAME(WS-STUDENT-IDX)
      D       DISPLAY "Moyenne: " S-AVERAGE(WS-STUDENT-IDX)

           END-PERFORM.

           EXIT.
       0310-S-AVERAGE-END.

      *Paragraphe gérant le calcul de la moyenne par matière.
       0320-C-AVERAGE-START.
      *On boucle autant de fois qu'il y a de cours différents.
           PERFORM VARYING WS-IDX FROM 1 BY 1
                               UNTIL WS-IDX > COURSE-NBR
      *On réinitialise la variable tampon.
                MOVE 0 TO WS-CALCUL-TEMP
      *On boucle sur le tableau COURSE.
                PERFORM VARYING WS-COURSE-IDX FROM 1 BY 1
                                    UNTIL WS-COURSE-IDX > COURSE-LGHT
      *Si Le Label du cours correspond à celui recherché: 
      *On ajoute la note correspondante à une variable tampon.
                  IF C-LABEL(WS-COURSE-IDX) = C-LABEL(WS-IDX)
                     ADD C-GRADE(WS-COURSE-IDX) TO WS-CALCUL-TEMP
                  END-IF
                END-PERFORM
      *On divise le total par le nombre d'étudiant (donc le nombre de note).
                DIVIDE WS-CALCUL-TEMP BY STUDENT-LGHT 
                                   GIVING WS-C-AVERAGE(WS-IDX) ROUNDED
      *Affichage pour debug
      D         DISPLAY WS-C-AVERAGE(WS-IDX)
           END-PERFORM.
           
           EXIT.
       0320-C-AVERAGE-END.

      *Paragraphe gérant le calcul de la moyenne générale de la classe.
       0330-CLASS-AVERAGE-START.
      *On réinitialise les variables de calcul.
           MOVE 0 TO WS-CALCUL-COEF.
           MOVE 0 TO WS-CALCUL-SUM.
      *On boucle sur le tableau COURSE.
           PERFORM VARYING WS-COURSE-IDX FROM 1 BY 1
                             UNTIL WS-COURSE-IDX >  COURSE-LGHT
      *On ajoute tout les coefficients dans une variable tampon.
                     ADD C-COEF(WS-COURSE-IDX) TO WS-CALCUL-COEF
      *On multiplie les notes avec leur coefficients correspondants
      *Et on stocke le résultat dans une variable tampon.
                     MULTIPLY C-GRADE(WS-COURSE-IDX) BY
                         C-COEF (WS-COURSE-IDX) GIVING
                         WS-CALCUL-TEMP ROUNDED
      *On ajoute chaque résultat dans une autre variable de calcul.
                     ADD WS-CALCUL-TEMP TO WS-CALCUL-SUM
           END-PERFORM.
      *On divise la somme pondérée par la somme des coefficients
      * et on arrondis le résultat.
           DIVIDE WS-CALCUL-SUM BY WS-CALCUL-COEF GIVING
                                    WS-CLASS-AVERAGE ROUNDED.
      *Affichage pour debug.
      D    DISPLAY WS-CLASS-AVERAGE.

           EXIT.
       0330-CLASS-AVERAGE-END.

       0400-LASTNAME-DUPLICATES-START.

      *On itère un index pour parcourir le tableau pour chaque étudiant.
           PERFORM VARYING WS-SEARCH-IDX FROM 1 BY 1
                            UNTIL WS-SEARCH-IDX > STUDENT-LGHT
      *On assigne la valeur 1 à l'index 
      *                        dont on se sert dans la fonction SEARCH.
                SET WS-STUDENT-IDX TO 1
      *On cherche dans le tableau STUDENT si un NOM apparait deux fois.
                SEARCH STUDENT
                WHEN S-LASTNAME(WS-STUDENT-IDX) EQUAL
                     S-LASTNAME(WS-SEARCH-IDX)  AND 
                     WS-STUDENT-IDX NOT EQUAL WS-SEARCH-IDX
      *Si c'est le cas on stocke la premier caractère du prénom 
      *                dans une variable spécialement déclarée pour ça.
                     MOVE S-FIRSTNAME(WS-SEARCH-IDX) TO 
                          S-INITIALS (WS-SEARCH-IDX)
                END-SEARCH
      *Affichage pour debug.
      D    DISPLAY S-LASTNAME(WS-SEARCH-IDX) SPACE 
      D            S-INITIALS(WS-SEARCH-IDX)
           END-PERFORM.

           EXIT.
       0400-LASTNAME-DUPLICATES-END.

      *Paragraphe pour gérer le tri du tableau STUDENT 
      *                                            par la key LASTNAME.
       0500-SORT-NAME-START.

      *On boucle le tableau STUDENT.
           PERFORM VARYING WS-STUDENT-IDX FROM 1 BY 1
                                   UNTIL WS-STUDENT-IDX > STUDENT-LGHT
      *On trie le tableau STUDENT alphabétiquement par le NOM.
                SORT STUDENT ON ASCENDING KEY S-LASTNAME
      *Affichage pour debug.
      D    DISPLAY STUDENT(WS-STUDENT-IDX)

           END-PERFORM.
      
           EXIT.
       0500-SORT-NAME-END.

      *Paragraphe gérant le remplissage ds structures d'éditions
      *ainsi que l'écriture du fichier de sortie dans le format voulu.
       0600-WRITE-START.

           OPEN OUTPUT F-OUTPUT.

           MOVE AFF-ETOILE TO REC-F-OUTPUT
           WRITE REC-F-OUTPUT

           MOVE WS-TITLE TO REC-F-OUTPUT
           WRITE REC-F-OUTPUT

           MOVE AFF-ETOILE TO REC-F-OUTPUT
           WRITE REC-F-OUTPUT

           PERFORM VARYING WS-IDX FROM 1 BY 1
                                    UNTIL WS-IDX > COURSE-NBR
           MOVE WS-IDX TO WS-C-NBR(WS-IDX)
      D    DISPLAY WS-IDX
      D    DISPLAY  WS-SUB-HDR-COURSE
           END-PERFORM.
           MOVE WS-SUB-HEADER TO REC-F-OUTPUT
           WRITE REC-F-OUTPUT
           

           PERFORM VARYING WS-STUDENT-IDX FROM 1 BY 1
                                   UNTIL WS-STUDENT-IDX > STUDENT-LGHT
             MOVE SPACE TO WS-NAME

             STRING S-LASTNAME(WS-STUDENT-IDX) SPACE
                      S-INITIALS(WS-STUDENT-IDX) SPACE
             INTO  WS-NAME
             MOVE S-AVERAGE (WS-STUDENT-IDX) TO WS-S-AVERAGE-ED
             MOVE 0 TO WS-IDX
                
             PERFORM VARYING WS-COURSE-IDX FROM 1 BY 1
                                   UNTIL WS-COURSE-IDX > COURSE-LGHT
               
               IF C-ID(WS-COURSE-IDX) = S-ID(WS-STUDENT-IDX)
                 ADD 1 TO WS-IDX
                 MOVE C-GRADE(WS-COURSE-IDX) TO WS-GRADE(WS-IDX)
      D          DISPLAY  C-ID(WS-COURSE-IDX) SPACE
      D                   S-ID(WS-STUDENT-IDX)
      
               END-IF
             END-PERFORM
               
             MOVE WS-BODY TO REC-F-OUTPUT
             WRITE REC-F-OUTPUT

           END-PERFORM.

           MOVE SPACE TO REC-F-OUTPUT
           WRITE REC-F-OUTPUT

           MOVE 0 TO WS-IDX.
           MOVE WS-CLASS-AVERAGE TO WS-CLASS-AVERAGE-ED
           PERFORM VARYING WS-IDX FROM 1 BY 1
                                    UNTIL WS-IDX > COURSE-NBR
                    MOVE WS-C-AVERAGE(WS-IDX) TO WS-C-AVERAGE-ED(WS-IDX)
           END-PERFORM.
           MOVE WS-TOTAL TO REC-F-OUTPUT
           WRITE REC-F-OUTPUT

           MOVE AFF-ETOILE TO REC-F-OUTPUT
           WRITE REC-F-OUTPUT

           PERFORM VARYING WS-IDX FROM 1 BY 1
                                    UNTIL WS-IDX > COURSE-NBR
               MOVE WS-C-NBR(WS-IDX) TO WS-C-NBR-2(WS-IDX)
               MOVE C-COEF(WS-IDX)   TO WS-C-COEF(WS-IDX)
               MOVE C-LABEL(WS-IDX)  TO WS-C-LABEL(WS-IDX)
               MOVE WS-COURSE-INFO-TAB(WS-IDX) TO REC-F-OUTPUT
               WRITE REC-F-OUTPUT
           END-PERFORM.
           
           MOVE AFF-ETOILE TO REC-F-OUTPUT
           WRITE REC-F-OUTPUT

           MOVE STUDENT-LGHT          TO WS-STUDENT-NBR
           MOVE WS-STUDENT-NBR-ED     TO REC-F-OUTPUT
           WRITE REC-F-OUTPUT

           MOVE COURSE-NBR            TO WS-COURSE-NBR
           MOVE WS-COURSE-NBR-ED      TO REC-F-OUTPUT
           WRITE REC-F-OUTPUT

           MOVE COURSE-LGHT           TO WS-GRADE-NBR
           MOVE WS-GRADE-NBR-ED       TO REC-F-OUTPUT
           WRITE REC-F-OUTPUT

           MOVE AFF-ETOILE TO REC-F-OUTPUT
           WRITE REC-F-OUTPUT
           
           MOVE WS-FOOTER TO REC-F-OUTPUT
           WRITE REC-F-OUTPUT

           MOVE AFF-ETOILE TO REC-F-OUTPUT
           WRITE REC-F-OUTPUT

           CLOSE F-OUTPUT.

           EXIT.
       0600-WRITE-END.
     
       
       